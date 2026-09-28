library(testthat)
library(shiny)
library(htmltools)

# Stub UI side effects.
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)
showNotification <- function(...) invisible(NULL)

source(here::here("R", "utils_handle_detailed_error.R"), local = TRUE)
source(here::here("R", "utils_logger.R"), local = TRUE)
source(here::here("R", "utils_app_error.R"), local = TRUE)

make_translator <- function(lang_code = "nl") {
  tr <- shiny.i18n::Translator$new(
    translation_json_path = here::here("language", "language.json")
  )
  tr$set_translation_language(lang_code)
  tr
}

make_fake_session <- function() {
  closed <- FALSE
  list(
    token = "deadbeefcafebabe",
    close = function() {
      closed <<- TRUE
    },
    is_closed = function() closed
  )
}

test_that("LLM diagnostics reach the downloadable report and sharing link", {
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)
  source(here::here("R", "utils_app_error.R"), local = TRUE)
  log_warn <- function(...) invisible(NULL)
  logged <- NULL
  log_error <- function(message, ...) logged <<- message
  ui <- NULL
  showModal <- function(value, ...) ui <<- value
  showNotification <- function(value, ...) ui <<- value
  withr::local_options(
    send_prompt_with_retries__log_prompts = FALSE,
    send_prompt_with_retries__log_prompts_to_file = FALSE
  )
  provider <- tidyprompt::llm_provider_openai(
    list(model = "test-model", stream = FALSE),
    verbose = FALSE,
    api_key = "private-key"
  )
  error <- httr2::with_mocked_responses(
    list(httr2::response(
      status_code = 401L,
      headers = list(
        "content-type" = "application/json",
        "x-request-id" = "req-ui-401"
      ),
      body = charToRaw(
        '{"error":{"message":"Rejected private-key"},"debug":"PRIVATE_UI_BODY"}'
      )
    )),
    tryCatch(
      send_prompt_with_retries("test", provider, max_tries = 1),
      error = identity
    )
  )
  # Also exercise the snapshot used to transfer conditions from a worker.
  remote <- kwallm_capture_worker_error(stop(error))$error
  for (condition in list(error, remote)) {
    for (fatal in c(FALSE, TRUE)) {
      capture.output(app_error(
        condition,
        fatal = fatal,
        shiny_session = make_fake_session(),
        lang = make_translator("en")
      ))
      html <- xml2::read_html(htmltools::renderTags(ui)$html)
      report <- xml2::xml_text(xml2::xml_find_first(html, ".//textarea"))
      for (expected in c(
        "HTTP status: 401",
        "Provider request ID: req-ui-401",
        "tidyprompt commit:",
        "tidyprompt_request_error",
        "httr2_http_401"
      )) {
        expect_match(report, expected, fixed = TRUE)
        expect_match(logged, expected, fixed = TRUE)
      }
      expect_false(grepl("private-key|PRIVATE_UI_BODY", report))
      expect_match(report, "[redacted]", fixed = TRUE)
      if (fatal) {
        href <- xml2::xml_attr(
          xml2::xml_find_first(html, ".//a[@href]"),
          "href"
        )
        expect_match(
          href,
          "github.com/KennispuntTwente/KWALLM/issues/new",
          fixed = TRUE
        )
        expect_lt(nchar(href, type = "bytes"), 1800)
        id <- xml2::xml_attr(
          xml2::xml_find_first(html, ".//*[@data-error-id]"),
          "data-error-id"
        )
        expect_match(utils::URLdecode(href), id, fixed = TRUE)
      }
    }
  }
})

test_that("every error includes deployment and version in console and file", {
  log_dir <- withr::local_tempdir()
  withr::local_options(
    kwallm__app_version = "1.2.3-test",
    kwallm__logger_state = list(
      initialized = TRUE,
      use_logger_pkg = FALSE,
      level = "DEBUG",
      log_dir = log_dir,
      log_dir_abs = log_dir,
      app_mode = "electron"
    )
  )
  for (fatal in c(FALSE, TRUE)) {
    output <- capture.output(app_error(
      simpleError("environment test"),
      fatal = fatal,
      shiny_session = make_fake_session(),
      lang = make_translator()
    ))
    for (expected in c(
      "App version: 1.2.3-test",
      "Deployment: electron",
      "R:",
      "Platform:"
    )) {
      expect_match(paste(output, collapse = "\n"), expected, fixed = TRUE)
    }
  }
  lines <- readLines(file.path(log_dir, paste0(Sys.Date(), ".log")))
  expect_true(all(grepl("App version: 1.2.3-test", lines, fixed = TRUE)))
  expect_true(all(grepl("Deployment: electron", lines, fixed = TRUE)))

  options(kwallm__app_version = NULL)
  expect_output(
    app_error(
      "missing version",
      shiny_session = make_fake_session(),
      lang = make_translator()
    ),
    "App version: unknown",
    fixed = TRUE
  )
})

test_that("large reports have bounded sharing links and a matching error ID", {
  modal <- NULL
  logged <- NULL
  showModal <- function(ui, ...) modal <<- ui
  log_error <- function(message, ...) logged <<- message
  app_error <- app_error
  environment(app_error) <- environment()
  long_error <- paste0(strrep("\u00e9 & # ? = ", 3000), "REPORT_END_SENTINEL")
  ids <- character()
  for (email in c(FALSE, TRUE)) {
    sess <- make_fake_session()
    capture.output(app_error(
      long_error,
      fatal = TRUE,
      when = "report export",
      shiny_session = sess,
      lang = make_translator("en"),
      admin_name = if (email) "Support" else NULL,
      admin_email = if (email) "support@example.com" else NULL
    ))
    html <- xml2::read_html(htmltools::renderTags(modal)$html)
    report <- xml2::xml_text(xml2::xml_find_first(html, ".//textarea"))
    id <- xml2::xml_attr(
      xml2::xml_find_first(html, ".//*[@data-error-id]"),
      "data-error-id"
    )
    ids <- c(ids, id)
    href <- xml2::xml_attr(xml2::xml_find_first(html, ".//a[@href]"), "href")
    expect_lt(nchar(href, type = "bytes"), 1800)
    expect_match(utils::URLdecode(href), id, fixed = TRUE)
    expect_match(report, id, fixed = TRUE)
    expect_match(logged, id, fixed = TRUE)
    expect_match(report, long_error, fixed = TRUE)
    expect_false(grepl("REPORT_END_SENTINEL", href, fixed = TRUE))
    expect_match(href, "%26", fixed = TRUE)
    expect_match(href, "%23", fixed = TRUE)
    expect_true(sess$is_closed())
    expect_match(
      htmltools::renderTags(modal)$html,
      "Copy diagnostic report",
      fixed = TRUE
    )
  }
  expect_false(identical(ids[1], ids[2]))
})

test_that("nonfatal errors also offer complete diagnostic reports", {
  notification <- NULL
  showNotification <- function(ui, ...) notification <<- ui
  log_error <- function(...) invisible(NULL)
  app_error <- app_error
  environment(app_error) <- environment()
  sess <- make_fake_session()
  capture.output(app_error(
    "retryable failure",
    shiny_session = sess,
    lang = make_translator()
  ))
  html <- xml2::read_html(htmltools::renderTags(notification)$html)
  expect_match(
    xml2::xml_text(xml2::xml_find_first(html, ".//textarea")),
    "retryable failure"
  )
  expect_length(
    xml2::xml_find_all(html, ".//button[@data-error-report-action]"),
    2L
  )
  expect_false(sess$is_closed())
})


test_that("app_error: nonfatal logs to nonfatal folder and does not close session", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()

  expect_output(
    app_error(
      simpleError("boom"),
      when = "unit",
      fatal = FALSE,
      shiny_session = sess,
      lang = make_translator("nl")
    ),
    regexp = "Error:"
  )
  expect_false(sess$is_closed())
})


test_that("app_error: fatal logs to fatal folder and closes session", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()

  expect_output(
    app_error(
      simpleError("boom"),
      when = "unit",
      fatal = TRUE,
      shiny_session = sess,
      lang = make_translator("nl")
    ),
    regexp = "Error:"
  )
  expect_true(sess$is_closed())
})


test_that("app_error: with NULL session stops after logging", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  expect_error(
    app_error(
      simpleError("boom"),
      when = "unit",
      fatal = FALSE,
      shiny_session = NULL,
      lang = make_translator("nl")
    ),
    "boom",
    fixed = TRUE
  )
})


test_that("app_error shows condition messages without call-object wrappers", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()
  provider_error <- paste0(
    "Invalid parameter: 'response_format' of type 'json_schema' ",
    "is not supported with this model."
  )
  wrapped_error <- structure(
    list(
      message = provider_error,
      call = quote(onFulfilled(...))
    ),
    class = c("simpleError", "error", "condition")
  )

  output <- capture.output(app_error(
    wrapped_error,
    when = "main processing",
    fatal = FALSE,
    shiny_session = sess,
    lang = make_translator("nl")
  ))

  expect_match(paste(output, collapse = "\n"), provider_error, fixed = TRUE)
  expect_false(any(grepl("<simpleError", output, fixed = TRUE)))
  expect_false(any(grepl("onFulfilled", output, fixed = TRUE)))
})


test_that("app_error writes complete marking failure context to the log file", {
  log_dir <- withr::local_tempdir(pattern = "kwallm-app-error-logs-")
  withr::local_options(
    kwallm__logger_state = list(
      initialized = TRUE,
      use_logger_pkg = FALSE,
      level = "DEBUG",
      log_dir = log_dir,
      log_dir_abs = log_dir,
      retention = NULL,
      app_mode = "test"
    ),
    kwallm__log_session_id = "deadbeef"
  )

  provider_error <- paste0(
    "Invalid parameter: 'response_format' of type 'json_schema' ",
    "is not supported with this model."
  )
  marking_error <- paste0(
    "Marking failed for analysis_unit_id=17, chunk_id=4, ",
    "chunk_index=2, code='Housing'.\nProvider error: ",
    provider_error
  )
  wrapped_error <- structure(
    list(
      message = marking_error,
      call = quote(onFulfilled(...))
    ),
    class = c("simpleError", "error", "condition")
  )
  sess <- make_fake_session()

  suppressMessages(capture.output(app_error(
    wrapped_error,
    when = "main processing of marking",
    fatal = TRUE,
    shiny_session = sess,
    lang = make_translator("nl")
  )))
  expect_true(sess$is_closed())

  log_file <- file.path(
    log_dir,
    paste0(format(Sys.Date(), "%Y-%m-%d"), ".log")
  )
  expect_true(file.exists(log_file))

  log_lines <- readLines(log_file, warn = FALSE)
  error_lines <- log_lines[grepl(
    "[ERROR] [error]",
    log_lines,
    fixed = TRUE
  )]

  expect_length(error_lines, 1L)
  expect_match(
    error_lines,
    "^\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}[+-]\\d{4}\\]"
  )
  expect_match(error_lines, "[deadbeef] [sync] [ERROR] [error]", fixed = TRUE)
  expect_match(error_lines, "[FATAL] Error occurred:", fixed = TRUE)
  expect_match(error_lines, "analysis_unit_id=17", fixed = TRUE)
  expect_match(error_lines, "chunk_id=4", fixed = TRUE)
  expect_match(error_lines, "chunk_index=2", fixed = TRUE)
  expect_match(error_lines, "code='Housing'", fixed = TRUE)
  expect_match(error_lines, provider_error, fixed = TRUE)
  expect_match(error_lines, "When: main processing of marking", fixed = TRUE)
  expect_match(error_lines, "Session ID: deadbeef", fixed = TRUE)
  expect_false(grepl("<simpleError", error_lines, fixed = TRUE))
  expect_false(grepl("onFulfilled", error_lines, fixed = TRUE))
})


test_that("app error messages retain the deepest purrr cause without a backtrace", {
  indexed_error <- tryCatch(
    purrr::imap(
      list(batch_one = "text"),
      function(value, name) {
        force(value)
        force(name)
        stop("PROVIDER_ERROR_SENTINEL", call. = FALSE)
      }
    ),
    error = identity
  )

  message <- kwallm_error_message(indexed_error)

  expect_match(message, "PROVIDER_ERROR_SENTINEL", fixed = TRUE)
  expect_match(message, "batch_one", fixed = TRUE)
  expect_false(grepl("Backtrace:", message, fixed = TRUE))
})


test_that("app_error: downgrades legacy interrupt transport error from fatal to nonfatal", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()

  ipc_err <- simpleError("Cannot pop from destroyed TextFileSource")
  expect_output(
    app_error(
      ipc_err,
      when = "unit",
      fatal = TRUE,
      shiny_session = sess,
      lang = make_translator("nl")
    ),
    regexp = "Error:"
  )
  expect_false(sess$is_closed())
})


test_that("app_error: downgrades local async interrupt error from fatal to nonfatal", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()

  interrupt_err <- structure(
    list(message = "user cancelled"),
    class = c("kwallm_async_interrupt", "error", "condition")
  )

  expect_output(
    app_error(
      interrupt_err,
      when = "unit",
      fatal = TRUE,
      shiny_session = sess,
      lang = make_translator("nl")
    ),
    regexp = "Error:"
  )
  expect_false(sess$is_closed())
})
