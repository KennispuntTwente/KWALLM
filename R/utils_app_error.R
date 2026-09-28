# Function handle errors in the Shiny app
# Logs the error message with a timestamp
# Shows details about the error message in the modal, and stops the app if it's considered a
#   fatal error
# Also shows contact details to report the error

# 1 Functions --------------------------------------------------------

# Limit encoded bytes, not characters: Unicode and reserved characters can
# expand considerably inside mailto/GitHub URLs. Full details stay in the UI.
kwallm_error_urlencode <- function(text, max_bytes) {
  encoded <- utils::URLencode(enc2utf8(text), reserved = TRUE)
  while (nchar(encoded, type = "bytes") > max_bytes) {
    text <- substr(text, 1, floor(nchar(text) * 0.8))
    encoded <- utils::URLencode(enc2utf8(text), reserved = TRUE)
  }
  encoded
}

kwallm_error_report_controls <- function(report, error_id, lang) {
  htmltools::attachDependencies(
    htmltools::tags$div(
      class = "kwallm-error-report",
      `data-error-id` = error_id,
      `data-copy-success` = lang$t("Diagnostisch rapport gekopieerd."),
      `data-copy-failure` = lang$t("Kopieer de geselecteerde tekst handmatig."),
      htmltools::tags$button(
        type = "button", class = "btn btn-secondary btn-sm",
        `data-error-report-action` = "copy",
        lang$t("Kopieer diagnostisch rapport")
      ),
      " ",
      htmltools::tags$button(
        type = "button", class = "btn btn-secondary btn-sm",
        `data-error-report-action` = "download",
        lang$t("Download diagnostisch rapport")
      ),
      htmltools::tags$p(role = "status", `aria-live` = "polite",
                        class = "kwallm-error-report-status"),
      htmltools::tags$details(
        htmltools::tags$summary(lang$t("Technische details")),
        htmltools::tags$textarea(
          readonly = "readonly", rows = 12, class = "form-control",
          `aria-label` = lang$t("Diagnostisch rapport"), report
        )
      )
    ),
    htmltools::htmlDependency(
      name = "kwallm-error-report", version = "1.0.0",
      src = c(file = here::here("www")), script = "error-report.js",
      all_files = FALSE
    )
  )
}

app_error <- function(
  error,
  when = "unknown",
  fatal = FALSE,
  shiny_session = shiny::getDefaultReactiveDomain(),
  admin_name = getOption("app_admin_name", NULL),
  admin_email = getOption("app_admin_email", NULL),
  github_repo = "https://github.com/KennispuntTwente/KWALLM",
  lang = shiny.i18n::Translator$new(
    translation_json_path = "language/language.json"
  )
) {
  # Downgrade known async interruption errors to nonfatal.
  if (
    isTRUE(grepl(
      "Cannot pop from destroyed TextFileSource",
      try(conditionMessage(error), silent = TRUE),
      fixed = TRUE
    )) ||
      inherits(error, "kwallm_async_interrupt")
  ) {
    fatal <- FALSE
  }

  # Use the condition message rather than printing the full condition object.
  # Printing promise/rlang errors exposes wrapper calls such as
  # `<simpleError in onFulfilled(...)>` and can bury the provider's message.
  error_diagnostics <- kwallm_error_diagnostics(error)
  error <- kwallm_error_message(error)
  error_preview <- stringr::str_trunc(gsub("[\r\n]+", " ", error), 160)
  if (length(error_diagnostics)) {
    error <- paste(error, paste(error_diagnostics, collapse = "\n"), sep = "\n\n")
  }

  session_id <- "system"
  if (
    !is.null(shiny_session) &&
      !is.null(shiny_session$token) &&
      nzchar(shiny_session$token)
  ) {
    session_id <- substr(shiny_session$token, 1, 8)
  } else if (exists("get_session_id", mode = "function")) {
    session_id <- tryCatch(get_session_id(), error = function(e) "system")
  }

  current_time <- Sys.time()
  error_id <- uuid::UUIDgenerate()
  formatted_time <- format(current_time, "%Y-%m-%d %H:%M:%S%z")
  app_version <- getOption("kwallm__app_version", "unknown")
  if (!is.character(app_version) || length(app_version) != 1L ||
      is.na(app_version) || !nzchar(app_version)) {
    app_version <- "unknown"
  }
  deployment <- tryCatch(get_app_mode(), error = function(e) "unknown")
  environment_details <- paste0(
    "App version: ", app_version,
    "\nDeployment: ", deployment,
    "\nR: ", R.version.string,
    "\nPlatform: ", R.version$platform
  )
  log_message <- paste0(
    "Error: ",
    error,
    "\n",
    "When: ",
    when,
    "\n",
    "Session ID: ",
    session_id,
    "\n",
    "Time: ",
    formatted_time,
    "\nError ID: ", error_id,
    "\n",
    environment_details,
    "\n"
  )

  cat(log_message)

  # Log error using the centralized logger
  # Keep this as one structured log record: condition messages can contain
  # newlines, which would otherwise leave continuation lines without the
  # logger's timestamp/session/component prefix.
  error_for_log <- gsub("[\r\n]+", " | ", error)
  when_for_log <- gsub("[\r\n]+", " | ", when)
  tryCatch(
    log_error(
      sprintf(
        "Error occurred: %s | When: %s | Session ID: %s | Error ID: %s | %s",
        error_for_log,
        when_for_log,
        session_id,
        error_id,
        gsub("[\r\n]+", " | ", environment_details)
      ),
      component = "error",
      fatal = fatal
    ),
    error = function(e) invisible(NULL)
  )

  if (is.null(shiny_session)) {
    stop(error)
  }

  report_controls <- kwallm_error_report_controls(log_message, error_id, lang)
  summary <- paste0(
    "Error ID: ", error_id,
    "\nSession ID: ", session_id,
    "\nTime: ", formatted_time,
    "\nApp version: ", substr(app_version, 1, 60),
    "\nDeployment: ", deployment,
    "\nWhen: ", substr(when, 1, 80),
    "\nError: ", error_preview
  )
  if (fatal) {
    removeModal()

    body_encoded <- kwallm_error_urlencode(paste0(
      summary, "\n\n",
      lang$t("Voeg het diagnostisch rapport toe aan uw melding.")
    ), max_bytes = 1400)

    # Fallback if admin contact info is missing
    contact_info <- if (!is.null(admin_name) && !is.null(admin_email)) {
      email_subject <- kwallm_error_urlencode(paste0(
        lang$t("Tekstanalyse-app-foutmelding: "),
        stringr::str_trunc(error, 50, ellipsis = "...")
      ), max_bytes = 250)
      mailto_link <- paste0(
        "mailto:",
        admin_email,
        "?subject=",
        email_subject,
        "&body=",
        body_encoded
      )
      tagList(
        p(paste(
          lang$t("Neem contact op met"),
          admin_name,
          lang$t("als je deze foutmelding blijft zien.")
        )),
        p(tags$a(
          href = mailto_link,
          lang$t("Klik hier om een e-mail te sturen met de foutmelding."),
          target = "_blank"
        ))
      )
    } else {
      github_issue_link <- paste0(
        github_repo,
        "/issues/new?labels=bug&title=",
        kwallm_error_urlencode(paste0(
          lang$t("Foutmelding: "),
          stringr::str_trunc(error, 50)
        ), max_bytes = 250),
        "&body=",
        body_encoded
      )
      tagList(
        p(tags$a(
          href = github_issue_link,
          lang$t(
            "Klik hier deze foutmelding te rapporteren als GitHub issue. Bedankt!"
          ),
          target = "_blank"
        ))
      )
    }

    showModal(modalDialog(
      title = lang$t("Fout"),
      tagList(
        tags$div(
          style = "display:none;",
          `data-kwallm-modal-id` = "app_error_modal",
          `data-kwallm-modal-details` = sprintf("fatal=%s", fatal)
        ),
        p(lang$t(
          "Er gebeurde iets onverwachts, waardoor de app is gestopt. Sorry!"
        )),
        hr(),
        pre(summary),
        report_controls,
        p(lang$t("Voeg het diagnostisch rapport toe aan uw melding.")),
        hr(),
        contact_info
      ),
      easyClose = FALSE,
      footer = NULL,
      size = "l"
    ))

    shiny_session$close()
  } else {
    showNotification(
      tagList(pre(summary), report_controls),
      type = "error",
      duration = NULL
    )
  }
}


# 2 Example/development usage --------------------------------------
if (FALSE) {
  library(shiny)
  library(shinyjs)

  ui <- bslib::page(
    useShinyjs(),
    actionButton("trigger_error", "Trigger Error"),
    actionButton("trigger_fatal_error", "Trigger Fatal Error"),
    actionButton(
      "trigger_fatal_error_unexpected",
      "Trigger Unexpected Fatal Error"
    )
  )

  server <- function(input, output, session) {
    observeEvent(input$trigger_error, {
      app_error("This is a non-fatal error.")
    })

    observeEvent(input$trigger_fatal_error, {
      app_error("This is a fatal error.", fatal = TRUE)
    })

    # Create unexpected error
    observeEvent(input$trigger_fatal_error_unexpected, {
      stop("This is an unexpected error.")
    })
  }

  app <- shinyApp(ui, server)
  shiny::runApp(app)
}
