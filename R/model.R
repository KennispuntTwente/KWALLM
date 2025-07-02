# User interface for selecting a large language model (LLM) and displaying its URL

#### 1 UI ####

model_ui <- function(
  id
) {
  ns <- NS(id) # Define namespace function for the UI
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("card"))
  )
}


#### 2 Server ####

model_server <- function(
  id,
  processing = reactiveVal(FALSE),
  mode = reactiveVal("Onderwerpextractie"),
  llm_provider_rv = reactiveValues(
    llm_provider = tidyprompt::llm_provider_openai(),
    provider_mode = "preconfigured",
    available_models_main = c("gpt-4o-mini", "gpt-4o"),
    available_models_large = c("gpt-4o", "o3")
  ),
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

      models <- reactiveValues(
        main = NULL,
        large = NULL
      )

      output$card <- renderUI({
        bslib::card(
          class = "card",
          card_header(
            "Model",
            bslib::tooltip(
              bsicons::bs_icon("info-circle"),
              paste0(
                lang()$t(
                  "Kies hier het LLM (large language model) dat je wilt gebruiken."
                ),
                lang()$t(
                  " Verschillende modellen hebben verschillende kwaliteiten; het ene model is sneller/goedkoper,",
                ),
                lang()$t(
                  " terwijl het andere model bijvoorbeeld duurder/langzamer is maar wel betere resultaten oplevert."
                ),
                lang()$t(
                  " Hoe goed het model moet zijn is afhankelijk van de complexiteit van de analysevraag die je hebt."
                )
              )
            )
          ),
          card_body(
            div(
              class = "d-flex flex-wrap justify-content-center gap-3",
              uiOutput(ns("main_model_selector_ui")),
              uiOutput(ns("large_model_selector_ui"))
            ),
            div(
              class = "d-flex flex-wrap justify-content-center gap-3",
              textOutput(ns("url"))
            )
          )
        )
      })

      observe({
        req(llm_provider_rv$llm_provider)

        # Set initial models only once
        if (is.null(models$main)) {
          initial_main_model <- llm_provider_rv$llm_provider$parameters$model
          if (!initial_main_model %in% llm_provider_rv$available_models_main) {
            initial_main_model <- llm_provider_rv$available_models_main[1]
          }
          models$main <- initial_main_model
        }

        if (is.null(models$large)) {
          models$large <- llm_provider_rv$available_models_large[1]
        }
      })

      # Select main model
      output$main_model_selector_ui <- renderUI({
        div(
          class = "selector-container text-center",
          selectInput(
            # Use namespaced ID
            inputId = ns("main_model"),
            label = HTML(paste0("Model")),
            choices = llm_provider_rv$available_models_main,
            # Use the reactive value or the initial value
            selected = models$main # Use the reactive value
          )
        )
      })

      # Select large model (voor onderwerpen reduceren bij onderwerpextractie)
      output$large_model_selector_ui <- renderUI({
        req(mode() == "Onderwerpextractie")
        div(
          class = "selector-container text-center",
          selectInput(
            inputId = ns("large_model"),
            label = span(
              HTML(paste0(lang()$t("Model voor onderwerpreductie"))),
              tooltip(
                bs_icon("info-circle"),
                paste0(
                  lang()$t(
                    "Bij onderwerpextractie kan je een apart model kiezen dat gebruikt zal worden voor het terugbrengen van mogelijke onderwerpen tot een finale lijst van onderwerpen."
                  ),
                  lang()$t(
                    " Omdat deze stap slechts één keer gebeurt maar wel grote invloed heeft op de resultaten, wordt aangeraden om een groter model te kiezen (mogelijk een 'thinking'-model zoals OpenAI's o3 of Deepseek's R1)."
                  )
                )
              )
            ),
            choices = llm_provider_rv$available_models_large,
            # Use the reactive value or the initial value
            selected = models$large # Use the reactive value
          )
        )
      })

      # Show URL of LLM provider
      output$url <- renderText({
        llm_provider_rv$llm_provider$url
      })

      # Update model reactiveValues when selected
      observeEvent(input$main_model, {
        req(input$main_model)
        models$main <- input$main_model
      })
      observeEvent(input$large_model, {
        req(input$large_model)
        models$large <- input$large_model
      })

      # Disable/enable inputs when processing status changes
      observe({
        req(mode())
        if (processing()) {
          shinyjs::disable("main_model")
          if (mode() == "Onderwerpextractie") {
            shinyjs::disable("large_model")
          }
        } else {
          shinyjs::enable("main_model")
          if (mode() == "Onderwerpextractie") {
            shinyjs::enable("large_model")
          }
        }
      })

      observe({
        req(llm_provider_rv$llm_provider)

        isolate({
          # Check and update main model if no longer valid
          if (
            !is.null(models$main) &&
              !(models$main %in% llm_provider_rv$available_models_main)
          ) {
            models$main <- NULL
          }

          # Check and update large model if no longer valid
          if (
            !is.null(models$large) &&
              !(models$large %in% llm_provider_rv$available_models_large)
          ) {
            models$large <- NULL
          }

          # If still NULL after reset, set to first available option
          if (
            is.null(models$main) &&
              length(llm_provider_rv$available_models_main) > 0
          ) {
            models$main <- llm_provider_rv$available_models_main[1]
          }

          if (
            is.null(models$large) &&
              length(llm_provider_rv$available_models_large) > 0
          ) {
            models$large <- llm_provider_rv$available_models_large[1]
          }
        })
      })

      return(models)
    }
  )
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(bsicons)

  ui <- bslib::page_fluid(
    css_js_head(),
    shinyjs::useShinyjs(),
    language_ui("language"),
    llm_provider_ui("llm_provider"),
    model_ui("model"),
    tagList(
      textOutput("main_model"),
      textOutput("large_model")
    )
  )

  server <- function(input, output, session) {
    lang <- language_server("language", processing = reactiveVal(FALSE))

    llm_provider_rv <- llm_provider_server(
      "llm_provider",
      processing = reactiveVal(FALSE),
      preconfigured_llm_provider = tidyprompt::llm_provider_openai(),
      preconfigured_main_models = c("gpt-4o-mini", "gpt-3.5-turbo"),
      preconfigured_large_models = c("gpt-4o", "o3"),
      lang = lang
    )

    model <- model_server(
      "model",
      llm_provider_rv = llm_provider_rv,
      lang = lang
    )

    output$main_model <- renderText({
      paste("Primair model:", model$main)
    })

    output$large_model <- renderText({
      paste("Groot model:", model$large)
    })
  }

  shinyApp(ui = ui, server = server)
}
