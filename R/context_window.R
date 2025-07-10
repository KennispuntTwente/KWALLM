# Module for context window and chunking parameters
# Ensures that the texts fit within the context window of the LLM

#### 1 UI ####

context_window_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    div(
      class = "card-container",
      uiOutput(ns("card"))
    )
  )
}


#### 2 Server ####

context_window_server <- function(
  id,
  mode = reactiveVal("Categorisatie"),
  models = reactiveValues(
    main = "gpt-4o-mini",
    large = "gpt-4o-mini"
  ),
  categories = list(
    texts = reactiveVal(c(
      "positive review",
      "negative review",
      "neutral review"
    )),
    editing = reactiveVal(FALSE),
    unique_non_empty_count = reactiveVal(3)
  ),
  scoring_characteristic = reactiveVal("positive sentiment"),
  codes = list(
    texts = reactiveVal(c(
      "positive",
      "negative",
      "neutral"
    )),
    editing = reactiveVal(FALSE),
    unique_non_empty_count = reactiveVal(3)
  ),
  research_background = reactiveVal(
    "We have collected consumer reviews of our product."
  ),
  assign_multiple_categories = reactiveVal(FALSE),
  texts = reactiveValues(
    preprocessed = c(
      "This is a positive review.",
      "This is a negative review.",
      "This is a neutral review. Very longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg textttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt"
    ),
    raw = c(
      "Dit is een positieve review.",
      "Dit is een negatieve review.",
      "Dit is een neutrale review."
    )
  ),
  processing = reactiveVal(FALSE),
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  ),
  chunk_size_default = getOption(
    "topic_modelling__chunk_size_default",
    25
  ),
  chunk_size_limit = getOption(
    "topic_modelling__chunk_size_limit",
    50
  ),
  number_of_chunks_limit = getOption(
    "topic_modelling__number_of_chunks_limit",
    100
  ),
  draws_default = getOption(
    "topic_modelling__draws_default",
    1
  ),
  draws_limit = getOption(
    "topic_modelling__draws_limit",
    5
  )
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$card <- renderUI({
        bslib::card(
          class = "card",
          card_header(
            lang()$t("Context-window"),
            tooltip(
              bs_icon("info-circle"),
              paste0(
                lang()$t(
                  "Het context-window is de hoeveelheid tekst die het taalmodel kan verwerken in één keer."
                ),
                lang()$t(
                  " Er moet voor worden gezorgd dat de onderzoeksachtergrond met de (langste) tekst die je invoert binnen het context-window van het model past."
                ),
                lang()$t(
                  " Daarnaast worden bij de eerste stap van onderwerpextractie de teksten in chunks verdeeld; deze chunks moeten ook binnen het context-window passen."
                ),
                lang()$t(
                  " Met parameters kan je de grootte van de chunks en het aantal trekkingen per tekst instellen."
                )
              )
            )
          ),
          card_body(
            div(
              class = "d-flex flex-column align-items-center",
              uiOutput(ns("context_window_ui")),
              uiOutput(ns("fit_context_window_warning")),
              uiOutput(ns("too_many_chunks_warning")),
              uiOutput(ns("n_chunks_display")),
            )
          )
        )
      })

      #### Reactive values ####
      rv <- reactiveValues(
        # State
        text_chunks = NULL,
        n_chunks = NULL,
        context_window_known = NULL,

        # Validity
        too_many_chunks = NULL,
        any_fit_problem = NULL,
        fit_context_window_chunks = NULL,
        fit_context_window_assigning = NULL,

        # Chunking parameters
        chunk_size = chunk_size_default,
        draws = draws_default,
        n_tokens_context_window = 2048,
        base_prompt_text = NULL
      )

      #### Sync user input to internal state ####
      observe({
        is_valid_number <- function(input) {
          if (is.null(input)) {
            return(FALSE)
          }

          if (!is.numeric(input)) {
            return(FALSE)
          }

          if (input != round(input)) {
            return(FALSE)
          }

          if (input <= 0) {
            return(FALSE)
          }

          return(TRUE)
        }

        if (
          is_valid_number(input$chunk_size) &&
            input$chunk_size <= chunk_size_limit
        ) {
          rv$chunk_size <- input$chunk_size
        }

        if (
          is_valid_number(input$draws) &&
            input$draws <= draws_limit
        ) {
          rv$draws <- input$draws
        }

        if (is_valid_number(input$context_window)) {
          rv$n_tokens_context_window <- input$context_window
        }
      })

      # Enforce limit on chunk_size
      observe({
        req(input$chunk_size)
        if (input$chunk_size > chunk_size_limit) {
          updateNumericInput(session, "chunk_size", value = chunk_size_limit)
        } else if (input$chunk_size < 1) {
          updateNumericInput(session, "chunk_size", value = 1)
        }
      })

      # Enforce limit on draws
      observe({
        req(input$draws)
        if (input$draws > draws_limit) {
          updateNumericInput(session, "draws", value = draws_limit)
        } else if (input$draws < 1) {
          updateNumericInput(session, "draws", value = 1)
        }
      })

      # Enforce limit on context window size
      observe({
        req(input$context_window)
        if (input$context_window < 0) {
          updateNumericInput(session, "context_window", value = 0)
        }
      })

      #### Obtain context window size based on model ####
      observe({
        req(models$main)
        size <- get_context_window_size_in_tokens(models$main)
        context_window_known <- is.null(size)

        size <- ifelse(
          is.null(size),
          2048,
          size
        )

        rv$n_tokens_context_window <- size
        rv$context_window_known <- context_window_known
      })

      #### Enable/disable input based on if context window is known ####
      # observe({
      #   req(models$main)
      #
      #   shinyjs::delay(
      #     250,
      #     shinyjs::toggleState(
      #       "context_window",
      #       condition = isTRUE(rv$context_window_known)
      #     )
      #   )
      # })

      #### Obtain number of characters in the base prompt, based on parameters ####
      # This is for categorization & scoring
      # (not candidate topic generation or writing paragraphs)
      observe({
        req(mode())
        req(!is.null(research_background()))
        rv$base_prompt_text <- NULL

        prompt <- switch(
          mode(),
          "Categorisatie" = {
            req(categories$texts())
            if (assign_multiple_categories()) {
              prompt_multi_category(
                text = "",
                research_background = research_background(),
                categories = categories$texts(),
                exclusive_categories = categories$texts()[
                  1:length(categories$texts()) %% 2 == 0
                ]
              )
            } else {
              prompt_category(
                text = "",
                research_background = research_background(),
                categories = categories$texts()
              )
            }
          },
          "Onderwerpextractie" = {
            # Approximate categories (as they are not known yet; assume a long list of 50)
            prompt_multi_category(
              text = "",
              research_background = research_background(),
              categories = paste0("Category ", seq(1, 50)),
              exclusive_categories = paste0("Category ", seq(2, 50, by = 2))
            )
          },
          "Scoren" = {
            req(scoring_characteristic())
            prompt_score(
              text = "",
              research_background = research_background(),
              scoring_characteristic = scoring_characteristic()
            )
          },
          "Markeren" = {
            req(codes$texts())
            longest_code <- codes$texts()[
              which.max(count_tokens(codes$texts()))
            ]

            mark_text_prompt(
              text = "",
              code = longest_code,
              research_background = research_background()
            )
          },
          NULL
        )

        if (!is.null(prompt)) {
          rv$base_prompt_text <- prompt |> tidyprompt::construct_prompt_text()
        } else {
          rv$base_prompt_text <- NULL
        }
      })

      #### Check if longest text + base prompt fit ####
      observe({
        req(
          mode() %in%
            c("Categorisatie", "Scoren", "Onderwerpextractie", "Markeren")
        )
        req(texts$preprocessed)
        req(rv$base_prompt_text)

        texts <- texts$preprocessed
        base_prompt_text <- rv$base_prompt_text

        # Check if the longest text + base prompt fits in the context window
        # Ensure only one longest text is selected
        longest_text <- texts[which.max(count_tokens(texts))]
        total_length <- count_tokens(longest_text) +
          count_tokens(base_prompt_text)

        mode <- mode()

        if (total_length > (rv$n_tokens_context_window)) {
          rv$fit_context_window_assigning <- FALSE
        } else {
          rv$fit_context_window_assigning <- TRUE
        }
      })

      #### Make chunks & check if they fit ####
      observe({
        req(mode() == "Onderwerpextractie")
        req(texts$preprocessed)
        req(rv$n_tokens_context_window)
        req(rv$chunk_size)
        req(rv$draws)

        texts <- texts$preprocessed

        # Based on prompt for candidate topic generation; 600 characters + background
        base_prompt_text <- prompt_candidate_topics(
          text_chunk = c(""),
          research_background = research_background(),
          language = lang()$get_translation_language()
        ) |>
          tidyprompt::construct_prompt_text()

        rv$text_chunks <- create_text_chunks(
          texts = texts,
          chunk_size = rv$chunk_size,
          draws = rv$draws,
          n_tokens_context_window = rv$n_tokens_context_window,
          base_prompt_text = base_prompt_text
        )

        if (is.null(rv$text_chunks)) {
          rv$fit_context_window_chunks <- FALSE
        } else {
          rv$fit_context_window_chunks <- TRUE
        }

        if (length(rv$text_chunks) > number_of_chunks_limit) {
          rv$too_many_chunks <- TRUE
        } else {
          rv$too_many_chunks <- FALSE
        }

        rv$n_chunks <- length(rv$text_chunks)
      })

      #### Check for presence of any fit problem ####
      observe({
        if (isTRUE(mode() == "Onderwerpextractie")) {
          if (
            isFALSE(rv$fit_context_window_chunks) ||
              isFALSE(rv$fit_context_window_assigning)
          ) {
            rv$any_fit_problem <- TRUE
          } else {
            rv$any_fit_problem <- FALSE
          }
        }

        if (isTRUE(mode() %in% c("Categorisatie", "Scoren", "Markeren"))) {
          if (isFALSE(rv$fit_context_window_assigning)) {
            rv$any_fit_problem <- TRUE
          } else {
            rv$any_fit_problem <- FALSE
          }
        }
      })

      #### Show inputs (context window, chunking parameters), based on mode ####
      output$context_window_ui <- renderUI({
        req(
          mode() %in%
            c("Categorisatie", "Scoren", "Onderwerpextractie", "Markeren")
        )
        return(div(
          class = "d-flex flex-column align-items-center",
          numericInput(
            ns("context_window"),
            lang()$t("Context-window grootte (tokens)"),
            value = rv$n_tokens_context_window,
            min = 0
          ),
          if (mode() == "Onderwerpextractie") {
            list(
              numericInput(
                ns("chunk_size"),
                lang()$t("Maximaal aantal teksten per chunk"),
                value = rv$chunk_size,
                min = 1,
                max = chunk_size_limit
              ),
              numericInput(
                ns("draws"),
                lang()$t("Aantal trekkingen per tekst"),
                value = rv$draws,
                min = 1,
                max = 5
              )
            )
          }
        ))
      })

      #### Show number of chunks and warnings, based on mode ####

      # Show number of chunks
      output$n_chunks_display <- renderUI({
        req(mode() == "Onderwerpextractie")
        req(rv$n_chunks)
        return(div(
          class = "alert alert-info d-flex align-items-center mt-2",
          bs_icon("blockquote-left"),
          span(
            class = "ms-2 fw",
            paste(lang()$t("Aantal chunks:"), rv$n_chunks)
          )
        ))
      })

      # Show warning if too many chunks
      output$too_many_chunks_warning <- renderUI({
        req(isTRUE(mode() == "Onderwerpextractie"))
        req(isTRUE(rv$too_many_chunks))
        return(div(
          class = "alert alert-danger d-flex align-items-center mt-2",
          bs_icon("exclamation-triangle-fill"),
          span(
            class = "ms-2",
            paste0(
              lang()$t("Te veel chunks"),
              " (> ",
              number_of_chunks_limit,
              ")"
            )
          )
        ))
      })

      # Show warning if context window is too small for the texts
      output$fit_context_window_warning <- renderUI({
        req(length(texts$preprocessed) > 0)
        req(
          (!is.null(rv$base_prompt_text) |
            isTRUE(mode() == "Onderwerpextractie"))
        )

        if (isTRUE(rv$any_fit_problem)) {
          return(div(
            class = "alert alert-danger d-flex align-items-center mt-2",
            bs_icon("exclamation-triangle-fill"),
            span(
              class = "ms-2",
              lang()$t("Sommige teksten zijn te lang voor het context-window")
            )
          ))
        }

        if (isFALSE(rv$any_fit_problem)) {
          return(div(
            class = "alert alert-success d-flex align-items-center mt-2",
            bs_icon("check-circle-fill"),
            span(
              class = "ms-2",
              lang()$t("Alle teksten passen binnen het context-window")
            )
          ))
        }
      })

      # Disable when processing
      observeEvent(
        processing(),
        {
          shinyjs::toggleState(
            "context_window",
            condition = !processing()
          )
          shinyjs::toggleState(
            "chunk_size",
            condition = !processing()
          )
          shinyjs::toggleState(
            "draws",
            condition = !processing()
          )
        },
        ignoreInit = TRUE
      )

      return(rv)
    }
  )
}

#' Create text chunks
#'
#' @param texts A vector of texts to be chunked.
#' @param chunk_size Maximum number of texts in a chunk
#' @param draws Number of times each text can be drawn into a chunk
#' @param n_tokens_context_window Number of tokens in the context window of the LLM
#' @param base_prompt_text Text of the base prompt to be used for candidate topic generation
#'
#' @return A list of text chunks, where each chunk is a vector of texts.
#' @export
create_text_chunks <- function(
  texts,
  chunk_size = 50,
  draws = 1, # new parameter: maximum number of times each text can be used,
  n_tokens_context_window = 2056,
  base_prompt_text = ""
) {
  stopifnot(
    is.character(texts),
    length(texts) > 0,
    is.numeric(chunk_size),
    chunk_size > 0,
    is.numeric(draws),
    draws > 0,
    is.numeric(n_tokens_context_window),
    n_tokens_context_window > 0,
    is.character(base_prompt_text),
    length(base_prompt_text) == 1
  )

  n_tokens_base_prompt <- count_tokens(base_prompt_text)
  allowed_tokens <- n_tokens_context_window - n_tokens_base_prompt

  # First check that each individual text does not exceed allowed_tokens
  if (any(count_tokens(texts) > allowed_tokens)) {
    # warning("One or more texts exceed the maximum allowed characters")
    return(NULL)
  }

  # If draws > 1, replicate each text accordingly so it can be redrawn.
  texts <- rep(texts, times = draws)

  # Randomize the order
  texts <- sample(texts)

  chunks <- list()
  current_chunk <- character(0)
  # current_total stores the effective token count for the current chunk
  current_total <- 0

  for (txt in texts) {
    txt_tokens <- count_tokens(txt)
    new_total <- current_total + txt_tokens

    # If adding the new text does not exceed allowed_tokens and chunk size, append it.
    if ((new_total <= allowed_tokens) && (length(current_chunk) < chunk_size)) {
      current_chunk <- c(current_chunk, txt)
      current_total <- new_total
    } else {
      # Otherwise, flush the current chunk and start a new one with the new text.
      if (length(current_chunk) > 0) {
        chunks <- c(chunks, list(current_chunk))
      }
      current_chunk <- c(txt)
      current_total <- txt_tokens
    }
  }

  # Flush any remaining texts in the current chunk
  if (length(current_chunk) > 0) {
    chunks <- c(chunks, list(current_chunk))
  }

  return(chunks)
}


#### 3 Helper functions ####

# Helper function with some hardcoded context window sizes for common models
# Will default to 2048 if the model is not recognized
# Better approach may be to retrieve via API or configuration file
get_context_window_size_in_tokens <- function(model) {
  if (
    model %in%
      c(
        "gpt-4.1-mini-2025-04-14",
        "gpt-4.1-2025-04-14",
        "gpt-4.1",
        "gpt-4.1-mini"
      )
  ) {
    return(1047576)
  }

  if (
    model %in%
      c(
        "o4-mini-2025-04-16",
        "o3-2025-04-16",
        "o3-mini-2025-01-31",
        "o1-2024-12-17",
        "o1-pro-2025-03-19",
        "o4-mini",
        "o3",
        "o3-mini",
        "o1",
        "o1-pro"
      )
  ) {
    return(200000)
  }

  if (
    model %in%
      c(
        "gpt-4o-2024-08-06",
        "chatgpt-4o-latest",
        "gpt-4o-mini-2024-07-18",
        "gpt-4o-mini",
        "gpt-4o"
      )
  ) {
    return(128000)
  }

  if (
    model %in%
      c(
        "gpt-3.5-turbo-0125"
      )
  ) {
    return(4096)
  }

  return(NULL)
}

#### 4 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(tidyverse)
  library(bsicons)

  ui <- bslib::page(
    shinyjs::useShinyjs(),
    if (exists("css_js_head")) css_js_head(),
    div(
      class = "card-container",
      mode_ui("mode"),
      model_ui("models"),
      context_window_ui("context_window")
    )
  )

  server <- function(input, output, session) {
    mode <- mode_server("mode", processing = reactiveVal(FALSE))
    models <- model_server(
      "models",
      processing = reactiveVal(FALSE),
      mode = mode,
      llm_provider = tidyprompt::llm_provider_openai(),
      available_main_models = c(
        "gpt-4o-mini-2024-07-18",
        "gpt-4.1-mini-2025-04-14",
        "some model"
      ),
      available_large_models = c(
        "gpt-4o-mini-2024-07-18",
        "gpt-4.1-mini-2025-04-14",
        "some model"
      )
    )
    context_window_server(
      "context_window",
      mode = mode,
      models = models
    )
  }

  shinyApp(ui, server)
}
