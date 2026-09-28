library(testthat)

source(here::here("R", "utils_app_error.R"), local = TRUE)

test_that("report copy and download work in a browser without a Shiny session", {
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome is unavailable")
  directory <- withr::local_tempdir()
  withr::local_dir(directory)
  translator <- shiny.i18n::Translator$new(
    translation_json_path = here::here("language", "language.json")
  )
  translator$set_translation_language("en")
  report <- paste0("Error ID: browser-test\n", strrep("Full details \u00e9\n", 2000),
    "</textarea><script>window.injected = true</script>\nEND")
  htmltools::save_html(
    kwallm_error_report_controls(report, "browser-test", translator),
    file = "report.html"
  )
  browser <- chromote::ChromoteSession$new()
  withr::defer(browser$close())
  browser$Page$navigate(paste0("file:///", normalizePath("report.html", winslash = "/")))
  evaluate <- function(js) {
    result <- browser$Runtime$evaluate(js, awaitPromise = TRUE, returnByValue = TRUE)
    if (!is.null(result$exceptionDetails)) stop("Browser JavaScript failed")
    result$result$value
  }
  evaluate("new Promise((resolve, reject) => {
    let attempts = 0;
    const wait = () => {
      if (window.kwallmErrorReportControls) return resolve(true);
      if (++attempts > 100) return reject(new Error('Report controls did not load'));
      setTimeout(wait, 25);
    }; wait();
  })")
  expect_false(evaluate("typeof Shiny !== 'undefined'"))
  expect_false(evaluate("window.injected === true"))
  # Browsers normalize textarea line endings; all other content must survive.
  expect_identical(evaluate("document.querySelector('textarea').value"), report)

  evaluate("Object.defineProperty(navigator, 'clipboard', { configurable: true,
    value: {writeText: async text => { window.copied = text; }} });
    document.querySelector('[data-error-report-action=copy]').click();")
  expect_identical(evaluate("window.copied"), report)
  expect_identical(evaluate("document.querySelector('[role=status]').textContent"),
    "Diagnostic report copied.")

  # Clipboard APIs may be absent on HTTP or denied by the browser.
  evaluate("navigator.clipboard.writeText = async () => { throw new Error('denied'); };
    document.execCommand = () => false;
    document.querySelector('[data-error-report-action=copy]').click();")
  expect_true(evaluate("document.querySelector('details').open"))
  expect_identical(evaluate("document.querySelector('[role=status]').textContent"),
    "Copy the selected text manually.")

  evaluate("Object.defineProperty(navigator, 'clipboard', { value: undefined });
    document.execCommand = () => true;
    document.querySelector('[data-error-report-action=copy]').click();")
  expect_identical(evaluate("document.querySelector('[role=status]').textContent"),
    "Diagnostic report copied.")

  evaluate("URL.createObjectURL = blob => {window.downloadBlob = blob; return 'blob:test';};
    HTMLAnchorElement.prototype.click = function() {window.downloadName = this.download;};
    document.querySelector('[data-error-report-action=download]').click();")
  expect_identical(evaluate("window.downloadName"), "kwallm-error-browser-test.txt")
  expect_identical(evaluate("window.downloadBlob.text()"), report)
})
