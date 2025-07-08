mark_texts <- function(
  texts = c(
    "hi my super boi, its raining!",
    "hello my mister man, the sky is blue",
    "hello to the world, its a hot day today",
    "for sure! i like cake"
  ),
  codes = c("greeting", "weather"),
  text_size_tokens = 128,
  overlap_size_tokens = 64,
  research_background = "",
  llm_provider = tidyprompt::llm_provider_openai(),
  progress_primary = NULL,
  progress_secondary = NULL,
  interrupter = NULL,
  lang = shiny.i18n::Translator$new(
    translation_json_path = "language/language.json"
  ),
  write_paragraphs = TRUE
) {
  stopifnot(
    is.character(texts),
    is.vector(texts),
    length(texts) > 0,
    is.character(codes),
    is.vector(codes),
    length(codes) > 0
  )
  total_steps <- 3
  if (write_paragraphs) {
    total_steps <- total_steps + 1
  }

  # Set initial progress
  try(progress_primary$set_with_total(
    1,
    total_steps,
    lang$t("Teksten splitsen...")
  ))

  # Load chunker
  chunker_name <- paste0("semchunker_", text_size_tokens)
  if (
    !exists(
      chunker_name
    )
  ) {
    semchunker <- semchunk_load_chunker(chunk_size = text_size_tokens)
    assign(chunker_name, semchunker, envir = .GlobalEnv)
  } else {
    semchunker <- get(chunker_name, envir = .GlobalEnv)
  }

  # Create dataframe with original texts
  df <- tibble::tibble(text = texts)
  # Split texts into semantic chunks, creating additional rows where needed
  df <- df |>
    dplyr::mutate(
      sub_text = purrr::map(text, function(x) {
        # Split text into semantic chunks
        semchunker(x, overlap = overlap_size_tokens)
      })
    ) |>
    tidyr::unnest(sub_text)

  # Verify that longest text does not exceed token limit
  model <- llm_provider$parameters$model
  n_tokens_context_window <- get_context_window_size_in_tokens(model)
  if (is.null(n_tokens_context_window)) n_tokens_context_window <- 2048
  longest_prompt_tokens <- mark_text_prompt(
    text = df$sub_text[which.max(count_tokens(df$sub_text))],
    code = codes[which.max(count_tokens(codes))]
  ) |>
    tidyprompt::construct_prompt_text() |>
    count_tokens()
  if (longest_prompt_tokens > n_tokens_context_window) {
    stop(paste0(
      "The longest prompt (with longest text, longest code) exceeds the context window token limit of ",
      n_tokens_context_window,
      " tokens (the longest prompt has ",
      longest_prompt_tokens,
      " tokens)"
    ))
  }

  # For each code & sub_text, ask LLM to mark relevant sections in the text
  # Create own row for each text, sub_text, code, and marked_text
  total_combinations <- nrow(df) * length(codes)
  current_count <- 0
  try({
    progress_primary$set_with_total(
      2,
      total_steps,
      lang$t(
        "Teksten markeren..."
      )
    )
    progress_secondary$show()
    progress_secondary$set_with_total(
      current_count,
      total_combinations,
      "..."
    )
  })
  df_result <- df |>
    tidyr::crossing(code = codes) |>
    dplyr::mutate(
      marked_text = purrr::map2(sub_text, code, function(txt, cd) {
        current_count <<- current_count + 1
        try({
          progress_secondary$set_with_total(
            current_count,
            total_combinations,
            paste0(
              lang$t("Tekst markeren voor code '"),
              cd,
              "'..."
            )
          )
        })

        if (!is.null(interrupter)) {
          interrupter$execInterrupts()
        }

        prompt <- mark_text_prompt(txt, cd)
        result <- send_prompt_with_retries(prompt, llm_provider)

        # Ensure result is always a character vector for unnesting
        if (length(result) == 0 || is.null(result)) {
          return(NA_character_)
        }

        return(result)
      })
    ) |>
    tidyr::unnest(marked_text) # This creates one row per marked section
  try({
    progress_secondary$hide()
    progress_primary$set_with_total(
      2.5,
      total_steps,
      lang$t(
        "Resultaten opschonen..."
      )
    )
  })

  # Clean up the result: remove NA marked_text, normalize, and check for substrings
  # (substring is when overlapped text parts are present in other marked sections;
  # i.e., same text, same code)
  df_result_clean <- df_result |>
    dplyr::filter(!is.na(marked_text)) |>
    dplyr::group_by(text, code) |>
    dplyr::mutate(
      # Normalize for comparison: lowercase, remove punctuation and spaces
      norm_text = stringr::str_remove_all(
        stringr::str_to_lower(marked_text),
        "[[:punct:]\\s]+"
      ),
      is_substring = purrr::map_lgl(norm_text, function(txt) {
        others <- setdiff(norm_text, txt)
        any(stringr::str_detect(others, stringr::fixed(txt)))
      })
    ) |>
    dplyr::filter(!is_substring) |>
    dplyr::select(-norm_text, -is_substring) |>
    dplyr::ungroup() |>
    dplyr::distinct()

  # Write paragraphs if requested
  if (write_paragraphs) {
    try({
      progress_primary$set_with_total(
        3,
        total_steps,
        lang$t("Rapport schrijven...")
      )
    })

    # Create list; keys as codes, values as vectors of all sub_texts for that code
    # Highlight in sub_texts the marked sections (with '**' around them)
    text_list <- df_result_clean |>
      dplyr::group_by(code) |>
      dplyr::summarise(
        paragraphs = list(
          purrr::map2_chr(
            text,
            marked_text,
            ~ if (is.na(.y) || .y == "") {
              .x # keep original text
            } else {
              stringr::str_replace_all(
                .x,
                stringr::fixed(.y),
                paste0("**", .y, "**")
              )
            }
          )
        ),
        .groups = "drop"
      ) |>
      tibble::deframe()

    # Create paragraphs for each code
    try({
      progress_secondary$set_with_total(
        0,
        length(text_list),
        "..."
      )
      progress_secondary$show()
    })
    paragraphs <- purrr::imap(
      text_list,
      function(texts, code) {
        if (!is.null(interrupter)) {
          interrupter$execInterrupts()
        }

        try({
          progress_secondary$set_with_total(
            progress_secondary$get_current() + 1,
            length(text_list),
            paste0(
              lang$t("Schrijven over '"),
              code,
              "'..."
            )
          )
        })

        paragraph <- write_paragraph(
          texts = texts,
          topic = code,
          research_background = research_background,
          llm_provider = llm_provider,
          language = lang$get_translation_language(),
          focus_on_highlighted_text = TRUE
        )

        return(paragraph)
      }
    )
    try({
      progress_secondary$hide()
      progress_primary$set_with_total(
        3.5,
        total_steps,
        lang$t("Afronden...")
      )
    })

    # Set paragraphs as an attribute of the result
    attr(df_result_clean, "paragraphs") <- paragraphs
  }

  return(df_result_clean)
}

# Helper: prompt to mark text
mark_text_prompt <- function(
  text,
  code,
  research_background = ""
) {
  prompt <- "You are given a qualitative 'code' and a 'text'.\nYour task is to mark the relevant parts in the text that correspond to the code."
  if (!is.null(research_background) && research_background != "") {
    prompt <- prompt |>
      tidyprompt::add_text(
        glue::glue_safe(
          "The text was obtained during a research project. Read 'research background'.\n\n<research background>\n{research_background}\n</research background>"
        ),
        sep = "\n"
      )
  }

  prompt <- prompt |>
    tidyprompt::add_text(
      glue::glue_safe(
        "<code>\n{code}\n</code>\n\n<text>\n{text}\n</text>\n\nYou need to return literal parts of the text that are relevant to the code.",
        " If there are no relevant parts, return an empty array under key 'text_parts'."
      )
    ) |>
    tidyprompt::answer_as_json(
      schema = list(
        type = "object",
        properties = list(
          text_parts = list(
            type = "array",
            items = list(
              type = "string"
            )
          )
        ),
        required = list("text_parts")
      ),
      type = "auto"
    )

  # Add validation function
  prompt <- prompt |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(x) {
        if (!is.list(x) || length(x) == 0 || !("text_parts" %in% names(x))) {
          return(tidyprompt::llm_feedback(paste0(
            "Invalid response format. Please return a JSON object with a 'text_parts' key containing an array of relevant text parts.",
            " Return an empty array if there are no relevant parts."
          )))
        }

        text_parts <- x$text_parts

        # If of length NULL, return an empty character vector
        if (length(text_parts) == 0) {
          return(character(0))
        }
        # If there is one entry and it's "", return an empty character vector
        if (length(text_parts) == 1 && text_parts[1] == "") {
          return(character(0))
        }

        # Check if all text parts are present in the original text
        not_found <- vapply(
          text_parts,
          function(part) !grepl(part, text, fixed = TRUE),
          logical(1)
        )
        if (any(not_found)) {
          missing_parts <- text_parts[not_found]
          return(tidyprompt::llm_feedback(paste0(
            "The following text parts could not be found in the original text: ",
            paste(shQuote(missing_parts), collapse = ", "),
            ". Please ensure all sections are present exactly as they appear in the text."
          )))
        }

        # If all parts are found, it's valid
        return(text_parts)
      }
    )

  return(prompt)
}
