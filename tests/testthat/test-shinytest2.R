library(shinytest2)

test_that("{shinytest2} recording: standard process - topic modelling", {
  app <- AppDriver$new(
    name = "standard process - topic modelling",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
  )

  app$set_inputs(`team-accordion` = character(0))
  app$set_inputs(`language-toggle` = "en")

  app$upload_file(`text_upload-text_file` = "test_texts.txt")
  Sys.sleep(3)

  app$set_inputs(
    `research_background-research_background` = "My research background"
  )

  app$set_inputs(`mode-mode` = "Topic extraction")

  app$set_inputs(
    `llm_provider-select_openai` = 0.123,
    allow_no_input_binding_ = TRUE
  )
  Sys.sleep(3)
  app$click("llm_provider-get_models")
  Sys.sleep(10)
  app$set_inputs(`model-main_model` = "gpt-4.1-nano-2025-04-14")
  app$set_inputs(`model-large_model` = "gpt-4.1-nano-2025-04-14")

  app$set_inputs(`assign_multiple_categories_toggle-toggle` = "Yes")
  app$set_inputs(`human_in_the_loop_toggle-toggle` = "Yes")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")
  app$set_inputs(`interrater_toggle-interrater_reliability` = "No")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "Yes")

  app$click("processing-process")
  Sys.sleep(10)
  app$click("processing-edit_topics-confirm_topics")
  Sys.sleep(20)

  app$expect_values(
    export = c(
      # Text upload & processing works
      "text_management-anonymization_mode",
      "text_management-texts__raw",
      "text_management-texts__preprocessed",
      "text_management-texts__df",

      # Processing was successful
      "processing-processing",
      "processing-success"
    )
  )

  app$stop()
})

test_that("{shinytest2} recording: standard process - scoring", {
  app <- AppDriver$new(
    name = "standard process - scoring",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
  )

  app$set_inputs(`team-accordion` = character(0))
  app$set_inputs(`language-toggle` = "en")

  app$upload_file(`text_upload-text_file` = "test_texts.txt")
  Sys.sleep(3)

  app$set_inputs(
    `text_management-select_none` = 0.123,
    allow_no_input_binding_ = TRUE
  )
  Sys.sleep(3)

  app$set_inputs(`mode-mode` = "Scoring")
  Sys.sleep(3)
  app$set_inputs(`scoring-scoring_characteristic` = "Positive sentiment")
  Sys.sleep(3)

  app$set_inputs(
    `llm_provider-select_openai` = 0.123,
    allow_no_input_binding_ = TRUE
  )
  Sys.sleep(3)
  app$click("llm_provider-get_models")
  Sys.sleep(10)
  app$set_inputs(`model-main_model` = "gpt-4.1-nano-2025-04-14")
  Sys.sleep(3)

  app$click("processing-process")
  Sys.sleep(20)

  app$expect_values(
    export = c(
      # Text upload & processing works
      "text_management-anonymization_mode",
      "text_management-texts__raw",
      "text_management-texts__preprocessed",
      "text_management-texts__df",

      # Processing was successful
      "processing-processing",
      "processing-success"
    )
  )

  app$stop()
})

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
  Sys.sleep(3)

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

  app$stop()
})
