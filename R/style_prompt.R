# Module for entering style prompt for summary generation
# Style prompt allows users to customize the writing style of LLM-generated summaries
#   which will be used to provide style guidance to the LLM in the prompts
# NOTE: This module is now integrated into write_paragraphs_toggle.R
#   This file is kept for backward compatibility

#### 1 UI ####

style_prompt_ui <- function(id) {
  ns <- NS(id)
  # Return empty div - functionality moved to write_paragraphs_toggle
  div()
}


#### 2 Server ####

style_prompt_server <- function(
  id,
  processing,
  mode = reactiveVal("Categorisatie"),
  write_paragraphs = reactiveVal(TRUE),
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  moduleServer(
    id,
    function(input, output, session) {
      # Return empty reactive - functionality moved to write_paragraphs_toggle
      return(reactiveVal(""))
    }
  )
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)

  ui <- bslib::page(
    useShinyjs(),
    css_js_head(),
    style_prompt_ui("style_prompt_module")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)
    mode <- reactiveVal("Categorisatie")
    write_paragraphs <- reactiveVal(TRUE)

    style_prompt <- style_prompt_server(
      "style_prompt_module",
      processing,
      mode,
      write_paragraphs
    )

    observe({
      print(paste("Style prompt:", style_prompt()))
    })
  }

  shinyApp(ui, server)
}