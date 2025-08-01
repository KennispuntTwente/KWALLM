# Module for entering style prompt for summary generation
# Style prompt allows users to customize the writing style of LLM-generated summaries
#   which will be used to provide style guidance to the LLM in the prompts

#### 1 UI ####

style_prompt_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("card"))
  )
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
      ns <- session$ns

      style_prompt <- reactiveVal("")
      shiny::exportTestValues(
        style_prompt = style_prompt()
      )

      output$card <- renderUI({
        # Only show when mode supports paragraph writing and write_paragraphs is enabled
        req(mode() %in% c("Onderwerpextractie", "Categorisatie", "Markeren"))
        req(write_paragraphs())
        
        bslib::card(
          class = "card",
          card_header(
            lang()$t("Stijlprompt voor samenvattingen"),
            tooltip(
              bs_icon("info-circle"),
              paste0(
                lang()$t(
                  "Hier kan je aangeven hoe het LLM de samenvattingen moet schrijven."
                ),
                lang()$t(
                  " Deze instructies worden meegegeven wanneer het LLM samenvattingen schrijft over categorieën of onderwerpen."
                ),
                lang()$t(
                  " Je kan bijvoorbeeld aangeven welke toon, stijl of focus je wilt (bijv. 'schrijf in academische stijl' of 'focus op emotionele aspecten')."
                )
              )
            )
          ),
          card_body(
            p(paste0(
              lang()$t(
                "Geef aan hoe de samenvattingen geschreven moeten worden."
              ),
              lang()$t(" Welke stijl of focus wil je?")
            )),
            textAreaInput(
              ns("style_prompt"),
              NULL,
              value = "",
              rows = 3,
              width = "100%",
              placeholder = lang()$t("Bijvoorbeeld: 'Schrijf in een formele, academische stijl' of 'Focus op emotionele aspecten van de teksten'")
            )
          )
        )
      })

      # Observe input
      observeEvent(input$style_prompt, {
        style_prompt(input$style_prompt)
      })

      # Disable when processing
      observeEvent(
        processing(),
        {
          shinyjs::toggleState(
            id = ns("style_prompt"),
            condition = !processing()
          )
        },
        ignoreInit = TRUE
      )

      return(style_prompt)
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