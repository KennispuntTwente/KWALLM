library(testthat)

source(here::here("R", "result_model.R"), local = TRUE)
source(here::here("R", "result_builders.R"), local = TRUE)
source(here::here("R", "result_serializers.R"), local = TRUE)
source(here::here("R", "utils_processing_helpers.R"), local = TRUE)

test_that("Pandoc conversion captures native stderr on a real IO failure", {
  skip_if_not_installed("callr")
  skip_if_not(rmarkdown::pandoc_available())
  directory <- withr::local_tempdir(pattern = "pandoc diagnostics ")
  missing_input <- file.path(directory, "missing-report-input.md")
  error <- tryCatch(
    kwallm_pandoc_convert(
      input = missing_input,
      to = "html",
      output = file.path(directory, "report.html")
    ),
    error = identity
  )

  expect_s3_class(error, "error")
  details <- conditionMessage(error)
  expect_match(details, "pandoc document conversion failed with error 1", fixed = TRUE)
  expect_match(details, "--- Pandoc stdout / command ---", fixed = TRUE)
  stderr <- strsplit(details, "--- Pandoc stderr ---", fixed = TRUE)[[1]][2]
  expect_match(stderr, "missing-report-input.md", fixed = TRUE)
  expect_match(stderr, "does not exist|No such file|cannot find")
})

test_that("captured conversion preserves successful output and caller state", {
  skip_if_not_installed("callr")
  skip_if_not(rmarkdown::pandoc_available())
  directory <- withr::local_tempdir(pattern = "pandoc diagnostics ")
  input <- file.path(directory, "input with spaces.md")
  output <- file.path(directory, "output with spaces.html")
  writeLines("# Conversion succeeded", input)
  original_wd <- getwd()
  original_sinks <- c(sink.number(), sink.number(type = "message"))

  expect_invisible(kwallm_pandoc_convert(input, to = "html", output = output))
  expect_match(paste(readLines(output), collapse = "\n"), "Conversion succeeded")
  expect_identical(getwd(), original_wd)
  expect_identical(c(sink.number(), sink.number(type = "message")), original_sinks)
})

test_that("Pandoc stderr survives conversion inside a real async worker", {
  skip_if_not_installed("callr")
  skip_if_not(rmarkdown::pandoc_available())
  kwallm_test_start_mirai_daemons(n = 1L)
  worker <- mirai::mirai({
    source(helper_file, local = TRUE)
    tryCatch(
      kwallm_pandoc_convert(
        file.path(tempdir(), "missing-worker-input.md"),
        to = "html", output = file.path(tempdir(), "report.html")
      ),
      error = conditionMessage
    )
  }, helper_file = here::here("R", "utils_processing_helpers.R"), .timeout = 60000)

  details <- worker[]
  expect_false(mirai::is_error_value(details))
  expect_match(details, "Pandoc stderr", fixed = TRUE)
  expect_match(details, "missing-worker-input.md", fixed = TRUE)
  expect_match(details, "pandoc document conversion failed with error 1", fixed = TRUE)
})

test_that("bundle errors include native Pandoc details and render context", {
  skip_if_not_installed("callr")
  skip_if_not(rmarkdown::pandoc_available())
  directory <- withr::local_tempdir()
  # Keep this regression focused on the error path: use a real Pandoc failure
  # at the render boundary, without depending on a broken report template.
  local_mocked_bindings(
    render = function(input, output_format, ...) {
      warning("render diagnostic warning")
      output_format$pandoc$convert_fun(
        file.path(directory, "missing-bundle-input.md"),
        to = "html", output = file.path(directory, "report.html")
      )
    },
    .package = "rmarkdown"
  )
  analysis_result <- build_analysis_result(
    texts_df = data.frame(
      source_document_id = 1L, document_id = 1L,
      source_document_text = "Text", document_text = "Text",
      preprocessed = "Text", analysis_unit_id = 1L
    ),
    results_table = data.frame(text = "Text", result = 10),
    uuid = "diagnostic-test", mode = "Scoren", language = "en",
    research_background = "", style_prompt = NULL, irr_result = NULL,
    by_column_name = NULL, by_column_lookup = NULL,
    models = list(main = list(parameters = list(model = "test-model"))),
    scoring_characteristic = "helpfulness", write_paragraphs = FALSE
  )

  error <- suppressWarnings(tryCatch(
    create_analysis_result_download_bundle(analysis_result, temp_dir = directory),
    error = identity
  ))
  expect_s3_class(error, "error")
  details <- conditionMessage(error)
  for (expected in c(
    "Rmarkdown file generation error", "missing-bundle-input.md",
    "Pandoc stderr", "report_Scoren_en.Rmd", "Language: en",
    "Pandoc version:", "Pandoc directory:", "Intermediates writable: TRUE",
    "rmarkdown:", "render diagnostic warning", "R calls at failure"
  )) {
    expect_match(details, expected, fixed = TRUE)
  }
  expect_false(grepl("No traceback available", details, fixed = TRUE))
  expect_length(list.files(directory, pattern = "\\.zip$"), 0L)
})
