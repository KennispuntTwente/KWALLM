library(shinytest2)

test_that("{shinytest2} recording: standard process - categorization", {
  app <- AppDriver$new(
    name = "standard process - categorization",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
  )

  app$set_inputs(`team-accordion` = character(0))
  app$set_inputs(`language-toggle` = "en")

  app$upload_file(`text_upload-text_file` = "test_texts.txt")

  app$set_inputs(`research_background-research_background` = "no clue!")

  app$set_inputs(`categories-category1` = "a")
  app$set_inputs(`categories-category2` = "b")
  app$click("categories-toggleEdit")

  app$set_inputs(
    `llm_provider-select_openai` = 0.123,
    allow_no_input_binding_ = TRUE
  )
  Sys.sleep(3)
  app$click("llm_provider-get_models")
  Sys.sleep(10)
  app$set_inputs(`model-main_model` = "gpt-4.1-nano-2025-04-14")

  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")

  app$click("processing-process")
  Sys.sleep(15)

  app$expect_values(
    export = c(
      # Text upload & processing works
      "text_management-anonymization_mode",
      "text_management-texts__raw",
      "text_management-texts__preprocessed",
      "text_management-texts__df",

      # Categories works
      "categories-n_fields",
      "categories-txt_in_fields",
      "categories-isEditing",

      # Processing was successful
      "processing-processing",
      "processing-success"
    )
  )
})

# test_that("{shinytest2} recording: basic controls", {
#   app <- AppDriver$new(
#     name = "basic controls",
#     height = 1400,
#     width = 2400,
#     load_timeout = 90000
#   )
#
#   app$set_inputs(`language-toggle` = "nl")
#
#   app$upload_file(
#     `text_upload-text_file` = here::here(
#       "tests/testthat/test_texts.txt"
#     )
#   )
#
#   app$set_inputs(`categories-category1` = "test category 1")
#   app$set_inputs(`categories-category2` = "test category 2")
#   app$set_inputs(`categories-category3` = "test category 3")
#   app$click("categories-toggleEdit")
#
#   app$set_inputs(
#     `research_background-research_background` = "Test research background"
#   )
#
#   # Click to change mode
#   app$set_inputs(`mode-mode` = "Scoring")
#   app$set_inputs(
#     `scoring-scoring_characteristic` = "Test scoring characteristic"
#   )
#
#   app$set_inputs(`mode-mode` = "Topic extraction")
#
#   app$expect_values(
#     export = c(
#       # Text upload works
#       "text_management-anonymization_mode",
#       "text_management-texts__raw",
#       "text_management-texts__preprocessed",
#       "text_management-texts__df",
#
#       # Categories works
#       "categories-n_fields",
#       "categories-txt_in_fields",
#       "categories-isEditing",
#
#       # Scoring characteristic works
#       "scoring-scoring_characteristic",
#
#       # Research background works
#       "research_background-research_background",
#
#       # Mode selection works
#       "mode-mode"
#     )
#   )
# })
