library(testthat)

source(here::here("R", "utils_handle_detailed_error.R"), local = TRUE)


test_that("kwallm_error_message omits condition calls but preserves causes", {
  provider_error <- structure(
    list(
      message = "PROVIDER_ERROR_SENTINEL",
      call = quote(onFulfilled(...))
    ),
    class = c("simpleError", "error", "condition")
  )

  expect_identical(
    kwallm_error_message(provider_error),
    "PROVIDER_ERROR_SENTINEL"
  )
  expect_identical(kwallm_error_message("plain error"), "plain error")
})


test_that("handle_detailed_error: wraps message with context", {
  h <- handle_detailed_error("Topic reduction")

  expect_true(is.function(h))

  expect_error(
    h(simpleError("nope")),
    "Topic reduction failed:\nMessage: nope",
    fixed = TRUE
  )
})

test_that("calling handlers retain original condition fields and failure frames", {
  original <- structure(
    list(
      message = "provider refused",
      call = quote(provider_call()),
      status_code = 429L,
      request_id = "request-123"
    ),
    class = c("test_provider_error", "error", "condition")
  )
  provider_call <- function() stop(original)
  wrapped <- tryCatch(
    withCallingHandlers(
      provider_call(),
      error = handle_detailed_error("Topic reduction")
    ),
    error = identity
  )
  expect_identical(wrapped$parent, original)
  expect_identical(conditionCall(wrapped), quote(provider_call()))
  expect_true("provider_call(...)" %in% wrapped$worker_calls)
})

test_that("structured errors and original worker frames survive real mirai transport", {
  kwallm_test_start_mirai_daemons(n = 1L)
  result <- NULL
  completed <- FALSE
  promise <- kwallm_mirai_submit(
    {
      source(helper, local = TRUE)
      provider_call <- function() {
        stop(structure(
          list(
            message = "provider refused",
            call = quote(provider_call()),
            status_code = 429L,
            request_id = "request-123"
          ),
          class = c("test_provider_error", "error", "condition")
        ))
      }
      withCallingHandlers(
        provider_call(),
        error = handle_detailed_error("Analysis")
      )
    },
    .args = list(helper = here::here("R", "utils_handle_detailed_error.R"))
  )
  promises::then(
    promise,
    onFulfilled = function(value) {
      result <<- value
      completed <<- TRUE
    },
    onRejected = function(error) {
      result <<- error
      completed <<- TRUE
    }
  )
  deadline <- Sys.time() + 15
  while (!completed && Sys.time() < deadline) {
    later::run_now(0.05)
  }
  expect_true(completed)
  expect_s3_class(result, "kwallm_remote_error")
  expect_match(conditionMessage(result), "Analysis failed", fixed = TRUE)
  expect_identical(
    result$parent$original_class,
    c("test_provider_error", "error", "condition")
  )
  expect_identical(result$parent$status_code, 429L)
  expect_identical(result$parent$request_id, "request-123")
  expect_true("provider_call(...)" %in% result$worker_calls)
  expect_match(
    paste(kwallm_error_diagnostics(result), collapse = "\n"),
    "provider_call",
    fixed = TRUE
  )
})

test_that("worker capture preserves values and interruption classification", {
  expect_identical(
    kwallm_capture_worker_error(list(answer = 42L)),
    list(answer = 42L)
  )
  failure <- kwallm_capture_worker_error(stop(structure(
    list(message = "cancelled", call = NULL),
    class = c("kwallm_async_interrupt", "error", "condition")
  )))
  expect_s3_class(failure$error, "kwallm_async_interrupt")
})
