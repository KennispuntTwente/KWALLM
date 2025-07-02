# Module for selecting mode; categorization/scoring/topic modelling

#### 1 UI ####

mode_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("card"))
  )
}


#### 2 Server ####

mode_server <- function(
  id,
  processing,
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

      mode <- reactiveVal("Categorisatie")
      shiny::exportTestValues(mode = mode())

      output$card <- renderUI({
        bslib::card(
          class = "card",
          card_header(
            lang()$t("Modus"),
            tooltip(
              bs_icon("info-circle"),
              lang()$t(
                "Kies de gewenste analysemethode: categoriseren, scoren of onderwerpen extraheren."
              )
            )
          ),
          card_body(
            div(
              class = "d-flex justify-content-center",
              shinyWidgets::radioGroupButtons(
                ns("mode"),
                NULL,
                choices = c(
                  lang()$t("Categorisatie"),
                  lang()$t("Scoren"),
                  lang()$t("Onderwerpextractie")
                ),
                selected = lang()$t("Categorisatie"),
                size = "sm"
              )
            ),
            uiOutput(ns("mode_description_ui"))
          )
        )
      })

      # Reactive value which holds text message about the splitting progress
      #   (set from async process via 'ipc' package, queue object)
      output$mode_description_ui <- renderUI({
        req(mode())

        msg <- switch(
          mode(),
          "Categorisatie" = lang()$t(
            "Teksten worden door het model ingedeeld op basis van categorieën die jij opgeeft. Per categorie kan het model een samenvatting met quotes schrijven."
          ),
          "Scoren" = lang()$t(
            "Teksten worden door het model beoordeeld op een score (van 0 t/m 100) voor in hoeverre ze overeenkomen met een door jou opgegeven kenmerk."
          ),
          "Onderwerpextractie" = lang()$t(
            "Het model zal verschillende perspectieven extraheren uit de teksten, en de teksten hiernaar categoriseren. Per categorie kan het model een samenvatting met quotes schrijven."
          )
        )

        div(
          class = "llm-narrow-container",
          style = "
            margin: 10px auto 15px auto;
            padding: 15px 20px;
            background-color: #f8f9fa;
            border: 1px solid #dee2e6;
            border-radius: 5px;
            font-size: 0.9em;
            color: #495057;
            text-align: center;
            word-break: normal;
            overflow-wrap: normal;
          ",
          HTML(msg)
        )
      })

      # When selecting input, update reactiveVal
      observeEvent(input$mode, {
        new_mode <- NULL

        if (input$mode == lang()$t("Categorisatie")) {
          new_mode <- "Categorisatie"
        } else if (input$mode == lang()$t("Scoren")) {
          new_mode <- "Scoren"
        } else if (input$mode == lang()$t("Onderwerpextractie")) {
          new_mode <- "Onderwerpextractie"
        }

        mode(new_mode)
      })

      # When processing, disable the mode selection
      observeEvent(processing(), {
        if (processing()) {
          shinyjs::disable("mode")
        } else {
          shinyjs::enable("mode")
        }
      })

      return(mode)
    }
  )
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(bslib)
  library(bsicons)

  ui <- bslib::page(
    useShinyjs(),
    css_js_head(),
    mode_ui("mode_module")
  )

  server <- function(input, output, session) {
    mode_server("mode_module", reactiveVal(FALSE))
  }

  shinyApp(ui, server)
}
