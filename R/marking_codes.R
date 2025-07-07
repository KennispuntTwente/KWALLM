marking_codes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("codes"))
  )
}

marking_codes_server <- function(
  id,
  mode,
  processing,
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    ## State/reactive values ---------------------------------------------
    n_fields <- reactiveVal(1)
    txt_in_fields <- reactiveVal(rep("", 3))
    isEditing <- reactiveVal(TRUE)

    shiny::exportTestValues(
      n_fields = n_fields(),
      txt_in_fields = txt_in_fields(),
      isEditing = isEditing()
    )

    ## UI ---------------------------------------------------------------
    output$code_fields <- renderUI({
      tagList(lapply(seq_len(n_fields()), function(i) {
        value <- txt_in_fields()[i] %||% ""

        fluidRow(
          column(
            width = 12,
            textAreaInput(
              ns(paste0("code", i)),
              label = paste(lang()$t("Code"), i),
              value = value,
              rows = 1,
              width = "100%"
            )
          )
        )
      }))
    })

    output$editButtonUI <- renderUI({
      button_label <- if (isEditing()) icon("save") else icon("pencil")
      actionButton(
        ns("toggleEdit"),
        label = tagList(button_label, ""),
        class = "btn btn-primary",
        style = "min-width: 75px;"
      )
    })

    output$codes <- renderUI({
      if (mode() == "Markeren") {
        bslib::card(
          class = "card",
          card_header(
            lang()$t("Codes"),
            tooltip(
              bs_icon("info-circle"),
              paste0(
                lang()$t(
                  "Bewerk hier de codes waarnaar het taalmodel relevante delen van de teksten zal markeren."
                ),
                lang()$t(
                  " Gebruik de '+'- en '-'-knoppen om codes toe te voegen of te verwijderen."
                ),
                lang()$t(
                  " Gebruik tenslotte de save/edit-knop om de codes op te slaan (of weer te kunnen bewerken)."
                )
              )
            )
          ),
          card_body(
            p(
              paste0(
                lang()$t(
                  "Voer de codes in waarvoor het taalmodel relevante delen van de teksten zal markeren."
                ),
                lang()$t(
                  "Geef beknopte, duidelijke omschrijvingen (met optioneel voorbeelden van teksten die bij de code zouden moeten horen)."
                )
              ),
              class = "mb-2 text-center"
            ),
            div(
              class = "category-button-container",
              actionButton(
                ns("addCode"),
                label = icon("plus"),
                class = "btn btn-success category-button",
                style = "min-width: 75px;"
              ),
              actionButton(
                ns("removeCode"),
                label = icon("minus"),
                class = "btn btn-danger category-button",
                style = "min-width: 75px;"
              ),
              uiOutput(ns("editButtonUI"))
            ),
            uiOutput(ns("code_fields"))
          )
        )
      }
    })

    ## Remember check-box changes while editing ------------------------
    observe({
      req(isEditing())
      txt_in_fields(sapply(
        seq_len(n_fields()),
        function(i) isolate(input[[paste0("code", i)]]) %||% txt_in_fields()[i],
        simplify = TRUE,
        USE.NAMES = FALSE
      ))
    })

    ## Add/remove categories -----------------------------------------
    observeEvent(input$addCode, {
      req(isEditing())
      txt_in_fields(c(txt_in_fields(), ""))
      n_fields(n_fields() + 1)
    })

    observeEvent(input$removeCode, {
      req(isEditing(), n_fields() > 1)
      txt_in_fields(utils::head(txt_in_fields(), -1))
      n_fields(n_fields() - 1)
    })

    ##  Toggle edit/save ----------------------------------------------
    observeEvent(input$toggleEdit, {
      if (isEditing()) {
        # ----> SAVE
        txt_in_fields(sapply(
          seq_len(n_fields()),
          function(i) input[[paste0("code", i)]] %||% txt_in_fields()[i]
        ))
        isEditing(FALSE)
        # Disable + & - buttons when not editing
        shinyjs::disable("addCode")
        shinyjs::disable("removeCode")
      } else {
        # ----> EDIT
        isEditing(TRUE)
        # Enable + & - buttons when editing
        shinyjs::enable("addCode")
        shinyjs::enable("removeCode")
      }
    })

    ##  Unified input-state updater (always in sync) -----------------
    update_input_state <- function() {
      lapply(seq_len(n_fields()), function(i) {
        txt_id <- paste0("code", i)
        if (!isEditing() || processing()) {
          shinyjs::disable(txt_id)
        } else {
          shinyjs::enable(txt_id)
        }
      })
      if (!isEditing()) {
        shinyjs::disable(c("addCode", "removeCode"))
      } else if (!processing()) {
        shinyjs::enable(c("addCode", "removeCode"))
      }
    }

    ## Trigger updater whenever any relevant reactive changes -----------
    observe({
      isEditing()
      processing()
      lang()
      n_fields()
      shinyjs::delay(50, update_input_state())
    })

    ##  Reactives returned to caller ------------------------------------
    nonEmptyTexts <- reactive({
      vals <- txt_in_fields()
      unique(trimws(vals))[nzchar(trimws(vals))]
    })

    nonEmptyUniqueCount <- reactive({
      sum(nzchar(nonEmptyTexts()))
    })

    ## Disable inputs when processing
    observe({
      if (isTRUE(processing())) {
        shinyjs::disable("addCode")
        shinyjs::disable("removeCode")
        shinyjs::disable("toggleEdit")
        lapply(seq_len(n_fields()), function(i) {
          shinyjs::disable(paste0("code", i))
        })
      } else {
        shinyjs::enable("addCode")
        shinyjs::enable("removeCode")
        shinyjs::enable("toggleEdit")
        lapply(seq_len(n_fields()), function(i) {
          shinyjs::enable(paste0("code", i))
        })
      }
    })

    return(list(
      texts = nonEmptyTexts,
      editing = isEditing,
      unique_non_empty_count = nonEmptyUniqueCount
    ))
  })
}


#### 2 Example/development usage ####

if (TRUE) {
  library(shiny)
  library(shinyjs)
  library(bslib)

  ui <- bslib::page(
    useShinyjs(),
    css_js_head(),
    marking_codes_ui("marking_codes")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)
    mode <- reactiveVal("Markeren")

    codes <- marking_codes_server("marking_codes", mode, processing)

    observe({
      req(codes$texts())
      print(codes$texts())
    })
  }

  shinyApp(ui, server)
}
