# Module for toggling inter-rater reliability

#### 1 UI ####

interrater_toggle_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("card"))
  )
}


#### 2 Server ####

interrater_toggle_server <- function(
  id,
  processing,
  mode,
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
      interrater_reliability_toggle <- reactiveVal(FALSE)

      output$card <- renderUI({
        req(isTRUE(
          mode() %in%
            c(
              "Categorisatie",
              "Scoren",
              "Onderwerpextractie"
            )
        ))

        bslib::card(
          class = "card",
          card_header(
            lang()$t("Inter-rater reliability"),
            tooltip(
              bs_icon("info-circle"),
              paste0(
                lang()$t(
                  "Wil je een steekproef trekken van de teksten om interrater-reliability te berekenen?"
                ),
                lang()$t(
                  " Nadat het model de teksten heeft geanalyseerd, zal een venster openen waarin je zelf teksten kunt beoordelen."
                ),
                lang()$t(
                  " Je beoordelingen worden vergeleken met die van het taalmodel (bij categorisatie/onderwerpextractie wordt Cohen's Kappa berekend; bij scoren wordt een paired t-test uitgevoerd)."
                )
              )
            )
          ),
          card_body(
            # Toggle for inter-rater reliability
            p(
              lang()$t("Zelf steekproef beoordelen?"),
              class = "mb-2 text-center"
            ),
            div(
              class = "d-flex justify-content-center",
              shinyWidgets::radioGroupButtons(
                ns("interrater_reliability"),
                NULL,
                choices = c(
                  lang()$t("Nee"),
                  lang()$t("Ja")
                ),
                selected = lang()$t("Nee"),
                size = "sm"
              )
            )
          )
        )
      })

      observeEvent(input$interrater_reliability, {
        interrater_reliability_toggle(
          input$interrater_reliability == lang()$t("Ja")
        )
      })

      # Disable when processing
      observeEvent(
        processing(),
        {
          shinyjs::toggleState(
            "interrater_reliability",
            condition = !processing()
          )
        },
        ignoreInit = TRUE
      )

      return(interrater_reliability_toggle)
    }
  )
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)

  ui <- bslib::page(
    css_js_head(),
    interrater_toggle_ui("interrater_toggle")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)

    interrater_toggle_server("interrater_toggle", processing)
  }

  shinyApp(ui, server)
}
