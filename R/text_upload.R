# Module to upload files with the texts
# Can handle .txt, .csv, .xlsx, and .sav files.
# Can select a sheet for Excel files, and a specific column for files with multiple columns
# Can filter rows based on column values through a modal dialog
# Note: pre-processing of texts is handled in the text_management module,
#   this module only uploads the raw data

#### 1 UI ####

text_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "card-container",
      uiOutput(ns("card"))
    )
  )
}


#### 2 Server ####

text_upload_server <- function(
  id,
  processing,
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$card <- renderUI({
      card(
        # ---- Card header -----------------------------------------------------
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center w-100",
            span(
              lang()$t("Upload teksten"),
              tooltip(
                bs_icon("info-circle"),
                paste0(
                  lang()$t(
                    "Upload de teksten die je wilt analyseren. Je kunt een platte tekstbestand (.txt), een CSV-bestand (.csv), een Excel-bestand (.xlsx) of een SPSS-bestand (.sav) uploaden."
                  ),
                  lang()$t(
                    " Let op dat .txt-bestanden worden gesplitst in een tekst per nieuwe regel. Voor de andere bestanden kun je een specifieke kolom met teksten selecteren. Voor Excel-bestanden kun je ook een specifiek werkblad selecteren."
                  ),
                  lang()$t(
                    " Via het filter-icoon kun je de data filteren, voor wanneer je alleen een subset van de data wilt analyseren."
                  )
                )
              )
            ),
            # Dynamic filter icon (updated from server for colour change)
            uiOutput(ns("filter_icon"))
          )
        ),
        # ---- Card body -------------------------------------------------------
        card_body(
          div(
            class = "d-flex justify-content-center",
            style = "width: 100%;",
            div(
              class = "d-flex flex-wrap justify-content-center gap-3",
              style = "max-width: 800px;",

              # ---------- File input + checkbox -----------------------------------------
              div(
                class = "selector-container d-flex flex-column align-items-center",
                style = "max-width: 300px;",
                fileInput(
                  inputId = ns("text_file"),
                  label   = lang()$t("Upload (.txt, .csv, .xlsx, of .sav)"),
                  accept  = c(".txt", ".csv", ".xlsx", ".sav")
                )
              ),

              # ---------- Sheet selector (Excel only) ------------------------
              div(
                class = "selector-container",
                style = "max-width: 300px; min-height: 100px;", # reserved height
                uiOutput(ns("sheet_selector"))
              ),

              # ---------- Column selector -----------------------------------
              div(
                id = ns("column_container"), # ◄ NEW: easy hide/show
                class = "selector-container",
                style = "max-width: 300px; min-height: 100px;", # reserved height
                uiOutput(ns("column_selector"))
              ),
              # ---- Text mode selector (for .txt files) -------------------
              uiOutput(ns("txt_mode_ui"))
            )
          )
        )
      )
    })

    # ---- Helpers ------------------------------------------------------------
    discard_empty <- function(x) {
      x <- x[!is.na(x)]
      x <- x[stringr::str_trim(x) != ""]
      unique(x)
    }

    # ---- Reactive values ----------------------------------------------------
    raw_texts <- reactiveVal(NULL) # vector of texts returned by module
    uploaded_data <- reactiveVal(NULL) # raw data (data.frame) read from file
    sheet_names <- reactiveVal(NULL) # character vector of Excel sheet names
    filter_spec <- reactiveVal(NULL) # list(col = <chr>, vals = <chr>) | NULL
    file_type <- reactiveVal(NULL) # ◄ NEW: current file extension

    # Logical reactive: is a filter currently active?
    filter_active <- reactive({
      spec <- filter_spec()
      df <- uploaded_data()
      if (is.null(spec) || is.null(df)) return(FALSE)
      col <- spec$col %||% if (file_type() == "txt") "text" else NULL
      if (is.null(col) || !col %in% names(df)) return(FALSE)
      col_vals <- df[[col]]
      filtered <- col_vals %in% spec$vals
      any(filtered) && sum(filtered) < nrow(df)
    })

    # Data after optional filtering -----------------------------------------
    filtered_data <- reactive({
      df <- uploaded_data()
      spec <- filter_spec()
      if (is.null(df) || is.null(spec)) return(df)

      # Default to "text" column for txt files
      col <- spec$col %||% if (file_type() == "txt") "text" else NULL
      if (is.null(col) || !col %in% names(df)) return(df)

      df[df[[col]] %in% spec$vals, , drop = FALSE]
    })

    # ---- File upload --------------------------------------------------------
    observe({
      req(input$text_file)

      # Reset all state -------------------------------------------------------
      raw_texts(NULL)
      uploaded_data(NULL)
      sheet_names(NULL)
      filter_spec(NULL)

      file_ext <- tools::file_ext(input$text_file$name)
      file_type(file_ext) # ◄ track for UI logic
      file_path <- input$text_file$datapath
      file_name <- input$text_file$name
      file_size <- file.size(file_path)

      # Show loading notification
      notification_id <- showNotification(
        paste(lang()$t("Bestand wordt geladen:"), file_name),
        type = "message",
        duration = NULL
      )

      # Validate file before processing
      validation_result <- tryCatch({
        # Check if file exists and is readable
        if (!file.exists(file_path)) {
          return(list(valid = FALSE, message = lang()$t("Bestand niet gevonden")))
        }
        
        # Check file size (warn if very large)
        if (file_size > 50 * 1024^2) { # 50 MB
          showNotification(
            paste(lang()$t("Groot bestand wordt geladen ("), 
                  round(file_size / 1024^2, 1), "MB). Dit kan even duren..."),
            type = "warning",
            duration = 5
          )
        }
        
        # Check if file is empty
        if (file_size == 0) {
          return(list(valid = FALSE, message = lang()$t("Bestand is leeg")))
        }
        
        # Additional validation for Excel files
        if (file_ext == "xlsx") {
          # Try to open the file to check if it's a valid Excel file
          test_conn <- tryCatch({
            conn <- file(file_path, "rb")
            # Read first few bytes to check Excel signature
            header <- readBin(conn, "raw", 8)
            close(conn)
            # Excel files should start with PK (ZIP signature) 
            if (length(header) >= 2 && header[1] == as.raw(0x50) && header[2] == as.raw(0x4B)) {
              return(TRUE)
            } else {
              return(FALSE)
            }
          }, error = function(e) FALSE)
          
          if (!test_conn) {
            return(list(valid = FALSE, message = lang()$t("Bestand lijkt geen geldig Excel-bestand te zijn")))
          }
        }
        
        return(list(valid = TRUE))
      }, error = function(e) {
        return(list(valid = FALSE, message = paste(lang()$t("Fout bij validatie van bestand:"), e$message)))
      })

      # Remove loading notification
      removeNotification(notification_id)

      # Stop if validation failed
      if (!validation_result$valid) {
        showNotification(
          validation_result$message,
          type = "error",
          duration = 10
        )
        return()
      }

      # Show processing notification
      notification_id <- showNotification(
        paste(lang()$t("Bestand wordt verwerkt:"), file_name),
        type = "message",
        duration = NULL
      )

      if (file_ext == "txt") {
        tryCatch(
          {
            # Try different encodings if UTF-8 fails
            txt_lines <- tryCatch({
              readLines(file_path, encoding = "UTF-8", warn = FALSE)
            }, error = function(e) {
              # Try other common encodings
              for (enc in c("latin1", "windows-1252", "UTF-8-BOM")) {
                result <- tryCatch({
                  readLines(file_path, encoding = enc, warn = FALSE)
                }, error = function(e2) NULL)
                if (!is.null(result)) return(result)
              }
              # If all encodings fail, throw the original error
              stop(e)
            })

            split_lines <- isTRUE(input$txt_split_lines == lang()$t("Ja"))

            txt <- if (split_lines) {
              discard_empty(txt_lines)
            } else {
              paste(txt_lines, collapse = "\n") # combine to single text
            }

            if (length(txt) == 0) {
              stop(lang()$t("Geen tekst gevonden in bestand"))
            }

            df <- data.frame(text = txt, stringsAsFactors = FALSE)
            uploaded_data(df)
            raw_texts(df$text)
            
            removeNotification(notification_id)
            showNotification(
              paste(lang()$t("Tekstbestand succesvol geladen:"), length(txt), lang()$t("teksten")),
              type = "success",
              duration = 3
            )
          },
          error = function(e) {
            removeNotification(notification_id)
            error_msg <- if (grepl("cannot open.*connection", e$message, ignore.case = TRUE)) {
              lang()$t("Kan bestand niet openen. Controleer of het bestand niet in gebruik is door een ander programma.")
            } else if (grepl("invalid.*UTF-8", e$message, ignore.case = TRUE)) {
              lang()$t("Tekstcodering van het bestand wordt niet ondersteund. Probeer het bestand op te slaan als UTF-8.")
            } else {
              paste(lang()$t("Fout bij lezen van tekstbestand:"), e$message)
            }
            showNotification(error_msg, type = "error", duration = 10)
          }
        )
      } else if (file_ext %in% c("csv", "tsv")) {
        tryCatch(
          {
            df <- vroom::vroom(file_path, show_col_types = FALSE)
            
            if (nrow(df) == 0) {
              stop(lang()$t("Geen data gevonden in bestand"))
            }
            
            uploaded_data(df)
            
            removeNotification(notification_id)
            showNotification(
              paste(lang()$t("CSV/TSV bestand succesvol geladen:"), nrow(df), lang()$t("rijen")),
              type = "success",
              duration = 3
            )
          },
          error = function(e) {
            removeNotification(notification_id)
            error_msg <- if (grepl("could not find function", e$message)) {
              lang()$t("Ontbrekende afhankelijkheid voor CSV-bestanden. Neem contact op met de beheerder.")
            } else if (grepl("lexical error", e$message) || grepl("parse", e$message)) {
              lang()$t("CSV-bestand heeft een ongeldig formaat. Controleer komma's, aanhalingstekens en rij-eindes.")
            } else {
              paste(lang()$t("Fout bij lezen van CSV/TSV bestand:"), e$message)
            }
            showNotification(error_msg, type = "error", duration = 10)
          }
        )
      } else if (file_ext == "xlsx") {
        tryCatch(
          {
            # First try to get sheet names
            sheets <- tryCatch({
              readxl::excel_sheets(file_path)
            }, error = function(e) {
              if (grepl("zip file.*corrupt", e$message, ignore.case = TRUE) || 
                  grepl("not in zip format", e$message, ignore.case = TRUE)) {
                stop(lang()$t("Excel-bestand is beschadigd of heeft een ongeldig formaat"))
              } else if (grepl("cannot open", e$message, ignore.case = TRUE)) {
                stop(lang()$t("Kan Excel-bestand niet openen. Controleer of het bestand niet in gebruik is."))
              } else {
                stop(e)
              }
            })
            
            if (length(sheets) == 0) {
              stop(lang()$t("Geen werkbladen gevonden in Excel-bestand"))
            }
            
            sheet_names(sheets)
            
            removeNotification(notification_id)
            showNotification(
              paste(lang()$t("Excel-bestand geladen met"), length(sheets), lang()$t("werkblad(en). Selecteer een werkblad.")),
              type = "success",
              duration = 5
            )
          },
          error = function(e) {
            removeNotification(notification_id)
            showNotification(
              paste(lang()$t("Fout bij lezen van Excel-bestand:"), e$message),
              type = "error",
              duration = 10
            )
          }
        )
      } else if (file_ext == "sav") {
        tryCatch(
          {
            df <- haven::read_sav(file_path)
            
            if (nrow(df) == 0) {
              stop(lang()$t("Geen data gevonden in bestand"))
            }
            
            uploaded_data(df)
            
            removeNotification(notification_id)
            showNotification(
              paste(lang()$t("SPSS-bestand succesvol geladen:"), nrow(df), lang()$t("rijen")),
              type = "success",
              duration = 3
            )
          },
          error = function(e) {
            removeNotification(notification_id)
            error_msg <- if (grepl("could not find function", e$message)) {
              lang()$t("Ontbrekende afhankelijkheid voor SPSS-bestanden. Neem contact op met de beheerder.")
            } else {
              paste(lang()$t("Fout bij lezen van SPSS-bestand:"), e$message)
            }
            showNotification(error_msg, type = "error", duration = 10)
          }
        )
      } else {
        removeNotification(notification_id)
        showNotification(
          paste(lang()$t("Niet ondersteund bestandstype:"), file_ext, ". ", 
                lang()$t("Ondersteunde formaten: .txt, .csv, .xlsx, .sav")),
          type = "error",
          duration = 10
        )
      }
    })

    # ---- Show / hide column selector depending on file type ---------------
    observe({
      if (is.null(file_type())) return()
      if (file_type() == "txt") {
        shinyjs::hide(ns("column_container"))
      } else {
        shinyjs::show(ns("column_container"))
      }
    })

    # ---- Text mode selector (for .txt files) --------------------------------
    output$txt_mode_ui <- renderUI({
      req(file_type() == "txt")
      div(
        # Toggle for inter-rater reliability
        p(
          lang()$t("Splits tekst op nieuwe regels?"),
          class = "mb-2 text-center"
        ),
        div(
          class = "d-flex justify-content-center w-100",   # add w-100
          shinyWidgets::radioGroupButtons(
            ns("txt_split_lines"), NULL,
            choices = c(lang()$t("Nee"), lang()$t("Ja")),
            selected = lang()$t("Ja"),
            size = "sm"
          )
        )
      )
    })

    # ---- Sheet selector (Excel only) ---------------------------------------
    output$sheet_selector <- renderUI({
      req(sheet_names())
      selectInput(
        ns("sheet"),
        lang()$t("Selecteer sheet"),
        choices = sheet_names(),
        selected = sheet_names()[1]
      )
    })

    observeEvent(input$sheet, {
      req(input$text_file, input$sheet)
      file_path <- input$text_file$datapath
      sheet_name <- input$sheet
      
      # Show loading notification
      notification_id <- showNotification(
        paste(lang()$t("Werkblad wordt geladen:"), sheet_name),
        type = "message",
        duration = NULL
      )
      
      tryCatch(
        {
          df <- readxl::read_excel(file_path, sheet = sheet_name)
          
          # Validate the loaded data
          if (nrow(df) == 0) {
            warning(lang()$t("Werkblad is leeg"))
          } else if (ncol(df) == 0) {
            stop(lang()$t("Werkblad heeft geen kolommen"))
          }
          
          # Check for completely empty columns (all NA or empty strings)
          non_empty_cols <- sapply(df, function(col) {
            if (is.character(col)) {
              any(!is.na(col) & nzchar(stringr::str_trim(col)))
            } else {
              any(!is.na(col))
            }
          })
          
          if (!any(non_empty_cols)) {
            warning(lang()$t("Werkblad bevat geen data"))
          }
          
          uploaded_data(df)
          
          removeNotification(notification_id)
          showNotification(
            paste(lang()$t("Werkblad succesvol geladen:"), nrow(df), lang()$t("rijen,"), ncol(df), lang()$t("kolommen")),
            type = "success",
            duration = 3
          )
        },
        error = function(e) {
          removeNotification(notification_id)
          error_msg <- if (grepl("Evaluation error.*subscript", e$message)) {
            lang()$t("Werkblad niet gevonden. Mogelijk is de naam gewijzigd.")
          } else if (grepl("zip file.*corrupt", e$message, ignore.case = TRUE)) {
            lang()$t("Excel-bestand is beschadigd")
          } else if (grepl("cannot open", e$message, ignore.case = TRUE)) {
            lang()$t("Kan werkblad niet openen. Controleer of het Excel-bestand niet in gebruik is.")
          } else {
            paste(lang()$t("Fout bij lezen van werkblad:"), e$message)
          }
          showNotification(error_msg, type = "error", duration = 10)
        },
        warning = function(w) {
          removeNotification(notification_id)
          showNotification(
            paste(lang()$t("Waarschuwing:"), w$message),
            type = "warning",
            duration = 8
          )
        }
      )
    })

    # ---- Column selector ----------------------------------------------------
    output$column_selector <- renderUI({
      req(filtered_data())
      if (file_type() == "txt") return(NULL)
      cols <- names(filtered_data())
      # if (length(cols) <= 1) return(NULL)
      selectInput(
        ns("column"),
        lang()$t("Selecteer kolom met teksten"),
        choices = cols,
        selected = NULL
      )
    })

    observeEvent(input$column, {
      req(filtered_data())
      col <- input$column
      if (!is.null(col) && nzchar(col)) {
        tryCatch({
          if (!col %in% names(filtered_data())) {
            stop(paste(lang()$t("Kolom niet gevonden:"), col))
          }
          
          txt <- filtered_data()[[col]]
          clean_txt <- discard_empty(txt)
          
          if (length(clean_txt) == 0) {
            showNotification(
              paste(lang()$t("Geen tekst gevonden in kolom:"), col),
              type = "warning",
              duration = 5
            )
            raw_texts(character(0))
          } else {
            raw_texts(clean_txt)
            showNotification(
              paste(lang()$t("Kolom geselecteerd:"), length(clean_txt), lang()$t("teksten uit"), col),
              type = "success",
              duration = 3
            )
          }
        }, error = function(e) {
          showNotification(
            paste(lang()$t("Fout bij selecteren van kolom:"), e$message),
            type = "error",
            duration = 8
          )
        })
      }
    })

    # ---- Filter icon (dynamic colour) --------------------------------------
    output$filter_icon <- renderUI({
      style <- if (filter_active()) "color:#0d6efd;" else "color:#6c757d;"
      actionLink(
        ns("filter_btn"),
        icon("filter", lib = "font-awesome"),
        style = paste0(style, "font-size:1.25rem;")
      ) |>
        bslib::tooltip(lang()$t("Filter data"))
    })

    # ---- Filter modal -------------------------------------------------------
    observeEvent(input$filter_btn, {
      req(uploaded_data())
      req(!isTRUE(processing()))

      showModal(modalDialog(
        title = lang()$t("Filter data"),
        size = "l",
        easyClose = TRUE,
        footer = NULL,

        bslib::page(
          p(lang()$t(
            "Je kunt hier de data filteren op basis van waarden in een kolom. Selecteer een kolom en kies waarden. Rijen zonder de gekozen waarden worden uitgesloten."
          )),
          hr(),

          # Inputs centered in modal
          div(
            class = "d-flex flex-column align-items-center text-center",
            style = "width: 100%; gap: 1rem;",
            uiOutput(ns("filter_col_selector")),
            uiOutput(ns("filter_values_ui"))
          ),
          hr(),

          ## ------------------------------------------------------------------
          ##  L E F T                |                R I G H T
          ## ------------------------------------------------------------------
          div(
            class = "d-flex justify-content-between align-items-stretch gap-2",

            # Left: Sluiten
            div(
              class = "d-flex align-items-stretch",
              div(class = "h-100", modalButton(lang()$t("Sluiten")))
            ),

            # Center: Filter wissen
            div(
              class = "d-flex justify-content-center flex-grow-1 align-items-stretch",
              actionButton(
                ns("clear_filter"),
                label = tagList(icon("rotate-left"), lang()$t("Filter wissen")),
                class = "btn btn-warning h-100 w-100"
              )
            ),

            # Right: Toepassen
            div(
              class = "d-flex align-items-stretch",
              actionButton(
                ns("apply_filter"),
                label = tagList(icon("filter"), lang()$t("Toepassen")),
                class = "btn btn-primary h-100"
              )
            )
          )
        )
      ))
    })

    # Dynamic values selector -------------------------------------------------
    output$filter_col_selector <- renderUI({
      req(uploaded_data())
      if (file_type() == "txt") return(NULL) # Hide if plain text file

      shinyWidgets::pickerInput(
        ns("filter_col"),
        label = lang()$t("Kies kolom voor filter"),
        choices = names(uploaded_data()),
        selected = filter_spec()$col %||% input$column %||% NULL,
        options = shinyWidgets::pickerOptions(container = "body")
      )
    })

    output$filter_values_ui <- renderUI({
      req(uploaded_data())
      if (!is.null(input$filter_col)) {
        df_col <- uploaded_data()[[input$filter_col]]
      } else {
        df_col <- uploaded_data()[[1]]
      }

      counts <- table(na.omit(df_col))
      vals <- names(counts)
      labels <- paste0(vals, " (", counts, ")")
      choices <- setNames(vals, labels)

      tagList(
        tags$style(
          HTML(
            "
            /* -------------------- overall menu -------------------- */
            .bootstrap-select .dropdown-menu.show{
              max-width: 75vw !important;   /* you said 75 % earlier */
            }

            /* -------------------- each row ------------------------ */
            .bootstrap-select .dropdown-item{
              /* turn each <a> into a flex-row so we can
                 allocate space separately for label + extras      */
              display: flex;
              align-items: center;
              gap: .4rem;                   /* little breathing room */
            }

            /* main label: truncate after 80 % of the row ----------- */
            .bootstrap-select .dropdown-item .text{ /* span that holds label */
              flex: 0 1 80%;               /* ≤ 80 % of the row */
              white-space: nowrap;
              overflow: hidden;
              text-overflow: ellipsis;
            }

            /* optional sub-text (generated by data-subtext) -------- */
            .bootstrap-select .dropdown-item small{
              flex: 0 1 auto;              /* take only what it needs */
              white-space: nowrap;
            }

            /* the built-in check-mark is absolutely positioned by
               Bootstrap-select, so no extra work is needed here.    */
            "
          )
        ),

        shinyWidgets::pickerInput(
          ns("filter_vals"),
          lang()$t("Kies waarden om te behouden"),
          choices = choices,
          selected = filter_spec()$vals %||% vals,
          multiple = TRUE,
          width = "100%",
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            deselectAllText = lang()$t("Deselecteer alles"),
            selectAllText = lang()$t("Selecteer alles"),
            noneSelectedText = lang()$t("Niks geselecteerd")
          )
        )
      )
    })

    # Apply / clear filter ----------------------------------------------------
    observeEvent(input$apply_filter, {
      if (!length(input$filter_vals))
        return(showNotification(
          lang()$t("Selecteer minstens één waarde om te behouden."),
          type = "error"
        ))

      filter_spec(list(
        col = if (file_type() == "txt") "text" else input$filter_col,
        vals = input$filter_vals
      ))
      removeModal()
    })

    observeEvent(input$clear_filter, {
      filter_spec(NULL)
      removeModal()
    })

    # Refresh raw_texts when filter or column changes ------------------------
    observeEvent(filtered_data(), {
      df <- filtered_data()
      req(df)

      if (file_type() == "txt") {
        # single-column data.frame called “text”
        raw_texts(discard_empty(df[["text"]]))
      } else if (!is.null(input$column) && nzchar(input$column)) {
        raw_texts(discard_empty(df[[input$column]]))
      }
    })

    # ---- Disable inputs while processing -----------------------------------
    observe({
      if (processing()) {
        shinyjs::disable("text_file")
        shinyjs::disable("sheet")
        shinyjs::disable("column")
        shinyjs::disable("filter_btn")
      }
    })

    # ---- Reset fileInput on new session ------------------------------------
    shinyjs::reset("text_file")

    # ---- Return raw texts (character vector) -------------------------------
    return(raw_texts)
  })
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)

  ui <- bslib::page(
    useShinyjs(),
    css_js_head(),
    text_upload_ui("text_upload_module")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE) # Simulate processing state

    raw_texts <- text_upload_server("text_upload_module", processing)

    observe({
      req(raw_texts())
      print(raw_texts()) # For debugging: print uploaded texts
    })
  }

  shinyApp(ui, server)
}
