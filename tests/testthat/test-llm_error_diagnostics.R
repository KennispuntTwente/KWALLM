library(testthat)

source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)
source(here::here("R", "utils_handle_detailed_error.R"), local = TRUE)

test_that("real HTTP failures work through streaming and non-streaming providers", {
  # Use an actual HTTP server: mocking req_perform() cannot exercise streaming
  # connection setup or how httr2 attaches the response to its condition.
  ready <- tempfile()
  withr::defer(unlink(ready))
  port <- httpuv::randomPort()
  server <- callr::r_bg(function(port, ready) {
    server <- httpuv::startServer("127.0.0.1", port, list(call = function(req) {
      list(status = 400L, headers = list(
        "Content-Type" = "application/json", "X-Request-ID" = "req-real-http",
        "Retry-After" = "15"
      ), body = paste0('{"error":{"message":"Unsupported parameter"},',
                       '"debug":"PRIVATE_HTTP_BODY"}'))
    }))
    on.exit(httpuv::stopServer(server))
    writeLines("ready", ready)
    repeat httpuv::service(100)
  }, args = list(port = port, ready = ready), libpath = .libPaths())
  withr::defer(server$kill())
  deadline <- Sys.time() + 15
  while (!file.exists(ready) && server$is_alive() && Sys.time() < deadline) Sys.sleep(0.05)
  expect_true(file.exists(ready), info = paste(server$read_error_lines(), collapse = "\n"))
  if (!file.exists(ready)) return(invisible(NULL))

  for (stream in c(FALSE, TRUE)) {
    for (provider_name in c("openai", "ollama")) {
      args <- list(parameters = list(model = "test-model", stream = stream),
                   verbose = FALSE, url = paste0("http://127.0.0.1:", port, "/error"))
      if (provider_name == "openai") args$api_key <- "test-only"
      provider <- do.call(getExportedValue("tidyprompt", paste0("llm_provider_", provider_name)), args)
      error <- tryCatch(send_prompt_with_retries("test", provider, max_tries = 1), error = identity)
      expect_s3_class(error, "kwallm_llm_error")
      expect_identical(error$status_code, 400L)
      expect_identical(error$request_id, "req-real-http")
      expect_identical(error$diagnostics$retry_after, "15")
      expect_match(conditionMessage(error), "Unsupported parameter", fixed = TRUE)
      expect_true("httr2_http_400" %in% error$diagnostics$causes[[2]]$error_class)
      expect_false(grepl("PRIVATE_HTTP_BODY", paste(capture.output(str(error)), collapse = "\n")))
    }
  }
})

test_that("real tidyprompt HTTP errors reach logs, provenance and diagnostic reports", {
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)
  withr::local_options(send_prompt_with_retries__log_prompts_to_file = FALSE,
                      send_prompt_with_retries__log_prompts = FALSE)
  providers <- list(
    tidyprompt::llm_provider_openai(
      list(model = "test-model", stream = FALSE), verbose = FALSE,
      url = "https://example.invalid/chat", api_key = "private-api-key"
    ),
    tidyprompt::llm_provider_ollama(
      list(model = "test-model", stream = FALSE), verbose = FALSE,
      url = "https://example.invalid/chat"
    )
  )
  for (provider in providers) {
    logged <- character()
    log_warn <- function(message, ...) logged <<- c(logged, message)
    .kwallm__prompt_execution_reset()
    response <- httr2::response(status_code = 429L, headers = list(
      "content-type" = "application/json", "x-request-id" = "req-integration-429",
      "retry-after" = "30", "authorization" = "private-header"
    ), body = charToRaw(paste0(
      '{"error":{"message":"Quota exhausted; Bearer private-api-key"},',
      '"debug":"private-response-body"}'
    )))
    error <- httr2::with_mocked_responses(list(response, response), tryCatch(
      send_prompt_with_retries("private-prompt", provider,
        max_tries = 2, retry_delay_seconds = 0), error = identity
    ))
    expect_s3_class(error, "kwallm_llm_error")
    expect_identical(error$status_code, 429L)
    expect_identical(error$request_id, "req-integration-429")
    expect_identical(error$diagnostics$retry_after, "30")
    expect_identical(error$diagnostics$attempt, 2)
    expect_true("tidyprompt_request_error" %in% error$diagnostics$causes[[1]]$error_class)
    expect_true("httr2_http_429" %in% error$diagnostics$causes[[2]]$error_class)
    expect_identical(error$diagnostics$tidyprompt_sha,
      utils::packageDescription("tidyprompt")$RemoteSha)
    report <- paste(kwallm_error_diagnostics(error), collapse = "\n")
    expect_match(report, "Provider request ID: req-integration-429", fixed = TRUE)
    expect_match(report, "tidyprompt commit:", fixed = TRUE)
    expect_match(report, "Quota exhausted", fixed = TRUE)
    expect_match(report, "including retries and waits", fixed = TRUE)
    expect_match(logged[[2]], "Retry-After=30", fixed = TRUE)
    records <- .kwallm__prompt_execution_get()
    expect_equal(records$try_count, 2)
    expect_match(records$final_error_message, "HTTP status=429", fixed = TRUE)
    output <- paste(report, paste(logged, collapse = "\n"),
      records$final_error_message, paste(capture.output(str(error)), collapse = "\n"))
    for (secret in c("private-api-key", "private-header", "private-response-body", "private-prompt")) {
      expect_false(grepl(secret, output, fixed = TRUE), info = secret)
    }
    expect_null(error$parent)
  }
})

test_that("transport causes stay visible without inventing HTTP metadata", {
  for (stream in c(FALSE, TRUE)) {
    local_mocked_bindings(
      req_perform = function(...) rlang::abort("Could not resolve host", class = "httr2_failure"),
      req_perform_connection = function(...) rlang::abort("Could not resolve host", class = "httr2_failure"),
      .package = "httr2"
    )
    provider <- tidyprompt::llm_provider_openai(
      list(model = "test-model", stream = stream), verbose = FALSE, api_key = "fake"
    )
    error <- tryCatch(send_prompt_with_retries("test", provider, max_tries = 1), error = identity)
    expect_s3_class(error, "kwallm_llm_error")
    expect_null(error$status_code)
    expect_null(error$request_id)
    expect_null(error$diagnostics$retry_after)
    expect_match(conditionMessage(error), "Could not resolve host", fixed = TRUE)
    expect_true("httr2_failure" %in% error$diagnostics$causes[[2]]$error_class)
  }
})

test_that("cause snapshots redact messages, omit calls and bound nested diagnostics", {
  parent <- simpleError("private-prompt api_key=private-key", call = quote(request("private-call")))
  for (i in seq_len(12)) {
    parent <- rlang::error_cnd("wrapped_error", message = paste("wrapper", i), parent = parent)
  }
  bounded <- kwallm_llm_error_diagnostics(parent, 10)
  expect_length(bounded$causes, 9)
  parent <- rlang::error_cnd("wrapped_error", message = "outer", parent = simpleError(
    paste("private-prompt api_key=private-key", strrep("x", 5000)),
    call = quote(request("private-call"))
  ))
  details <- kwallm_llm_error_diagnostics(parent, 10, sensitive_text = "private-prompt")
  output <- paste(capture.output(str(details)), collapse = "\n")
  expect_false(grepl("private-prompt|private-key|private-call", output))
  expect_match(details$causes[[2]]$message, "[truncated]", fixed = TRUE)
  expect_lte(nchar(details$message), 4000)
  expect_null(kwallm_llm_error_diagnostics(simpleError("HTTP 401"), 0)$status_code)
})

test_that("HTTP fallbacks accept httr responses and reject malformed metadata", {
  error <- structure(list(message = "provider failed", call = NULL,
    status_code = c(401, 500), request_id = "bad\nID",
    response = structure(list(status_code = 503L, headers = list(
      "X-MS-Request-ID" = "req-fallback", "Retry-After" = "Wed, 21 Oct 2026 07:28:00 GMT"
    )), class = "response")), class = c("error", "condition"))
  details <- kwallm_llm_error_diagnostics(error, 0)
  expect_identical(details$status_code, 503L)
  expect_identical(details$request_id, "req-fallback")
  expect_identical(details$retry_after, "Wed, 21 Oct 2026 07:28:00 GMT")
  error$response$headers[["Retry-After"]] <- "30\nprivate-header"
  expect_null(kwallm_llm_error_diagnostics(error, 0)$retry_after)
})

test_that("tidyprompt diagnostics survive a real worker and appear in the report", {
  kwallm_test_start_mirai_daemons(n = 1L)
  worker <- mirai::mirai({
    source(file.path(app_root, "R", "utils_handle_detailed_error.R"))
    source(file.path(app_root, "R", "utils_send_prompt_with_retries.R"))
    kwallm_capture_worker_error({
      provider <- tidyprompt::llm_provider_openai(
        list(model = "test-model", stream = FALSE), verbose = FALSE, api_key = "fake"
      )
      httr2::with_mocked_responses(list(httr2::response(
        status_code = 401L, headers = list("x-request-id" = "req-worker-401")
      )), send_prompt_with_retries("test", provider, max_tries = 1))
    })
  }, app_root = here::here())
  result <- worker[]
  expect_s3_class(result, "kwallm_worker_failure")
  expect_identical(result$error$status_code, 401L)
  expect_identical(result$error$request_id, "req-worker-401")
  report <- paste(kwallm_error_diagnostics(result$error), collapse = "\n")
  expect_match(report, "tidyprompt_request_error", fixed = TRUE)
  expect_match(report, "httr2_http_401", fixed = TRUE)
  expect_match(report, "tidyprompt commit:", fixed = TRUE)
  expect_match(report, "req-worker-401", fixed = TRUE)
})
