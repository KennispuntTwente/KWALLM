# Utility: format detailed errors for tryCatch/future handlers
# Returns a closure suitable for use in `error = ...` handlers.

kwallm_error_message <- function(error) {
  message <- if (inherits(error, "condition")) {
    tryCatch(conditionMessage(error), error = function(e) NULL)
  } else if (is.character(error)) {
    error
  } else {
    tryCatch(conditionMessage(error), error = function(e) NULL)
  }

  if (is.null(message) || !length(message)) {
    message <- tryCatch(as.character(error), error = function(e) NULL)
  }
  if (is.null(message) || !length(message)) {
    message <- tryCatch(
      capture.output(print(error)),
      error = function(e) "Unknown error"
    )
  }

  paste(as.character(message), collapse = "\n")
}


handle_detailed_error <- function(context = "An operation") {
  force(context)
  function(e) {
    error_message <- paste0(
      context,
      " failed:\n",
      "Message: ",
      kwallm_error_message(e)
    )
    # Keep the original condition available instead of replacing it with text.
    # Use this as a calling handler so the originating frames still exist.
    calls <- vapply(utils::tail(sys.calls(), 25), function(call) {
      head <- call[[1L]]
      if (is.symbol(head)) paste0(as.character(head), "(...)") else "<anonymous>(...)"
    }, character(1))
    stop(structure(
      list(message = error_message, call = conditionCall(e), parent = e,
           worker_calls = calls),
      class = c("kwallm_context_error",
                if (inherits(e, "kwallm_async_interrupt")) "kwallm_async_interrupt",
                "error", "condition")
    ))
  }
}


# Self-contained because this runs before worker bootstrap, including when
# bootstrap itself fails. Return errors as data so mirai does not flatten them.
kwallm_capture_worker_error <- function(expr) {
  calls <- character()
  snapshot <- function(e, depth = 0L) {
    parent <- e$parent
    fields <- list(
      message = conditionMessage(e), call = conditionCall(e),
      original_class = class(e),
      worker_calls = if (length(e$worker_calls)) e$worker_calls else calls
    )
    # Only explicitly selected diagnostic fields cross the process boundary;
    # request/response objects may contain environments, tokens or user text.
    for (name in c("status_code", "request_id", "elapsed_ms", "diagnostics")) {
      if (!is.null(e[[name]])) fields[[name]] <- e[[name]]
    }
    if (inherits(parent, "condition") && depth < 8L) {
      fields$parent <- snapshot(parent, depth + 1L)
    }
    structure(fields, class = c(
      "kwallm_remote_error",
      if (inherits(e, "kwallm_async_interrupt")) "kwallm_async_interrupt",
      "error", "condition"
    ))
  }
  tryCatch(
    withCallingHandlers(force(expr), error = function(e) {
      calls <<- vapply(utils::tail(sys.calls(), 25), function(call) {
        head <- call[[1L]]
        if (is.symbol(head)) paste0(as.character(head), "(...)") else "<anonymous>(...)"
      }, character(1))
    }),
    error = function(e) structure(
      list(error = snapshot(e)), class = "kwallm_worker_failure"
    )
  )
}


kwallm_error_diagnostics <- function(error) {
  if (!inherits(error, c("kwallm_remote_error", "kwallm_context_error", "kwallm_llm_error"))) {
    return(character())
  }
  details <- character()
  for (depth in seq_len(9L)) {
    classes <- if (length(error$original_class)) error$original_class else class(error)
    details <- c(details, paste("Cause class:", paste(classes, collapse = ", ")))
    if ("kwallm_llm_error" %in% classes && is.list(error$diagnostics)) {
      diagnostic <- error$diagnostics
      labels <- c(
        status_code = "HTTP status", request_id = "Provider request ID",
        retry_after = "Retry-After (provider header)",
        elapsed_ms = "Total elapsed ms (including retries and waits)",
        prompt_id = "Prompt ID", model = "Model", attempt = "Final attempt",
        tidyprompt_version = "tidyprompt version", tidyprompt_sha = "tidyprompt commit",
        httr2_version = "httr2 version"
      )
      for (name in names(labels)) {
        value <- diagnostic[[name]]
        if (is.atomic(value) && length(value) == 1L && !is.na(value)) {
          details <- c(details, paste0(labels[[name]], ": ", value))
        }
      }
      for (cause in diagnostic$causes) {
        details <- c(details, paste0("Provider cause [",
          paste(cause$error_class, collapse = ", "), "]: ", cause$message))
      }
    }
    if (!is.null(conditionCall(error))) {
      details <- c(details, paste("Originating call:",
        paste(deparse(conditionCall(error)), collapse = " ")))
    }
    if (length(error$worker_calls)) {
      details <- c(details, "Worker calls at failure:", error$worker_calls)
    }
    if (!inherits(error$parent, "condition")) break
    error <- error$parent
  }
  details
}
