mark_texts <- function(
  texts = c(
    "hi my super boi, its raining!",
    "hello my mister man, the sky is blue",
    "hello to the world, its a hot day today",
    "for sure! i like cake"
  ),
  codes = c("greeting", "weather"),
  text_size_tokens = 256,
  overlap_size_tokens = 64,
  research_background = "",
  llm_provider = tidyprompt::llm_provider_openai(),
  progress_primary = NULL,
  progress_secondary = NULL,
  interrupter = NULL
) {
  stopifnot(
    is.character(texts),
    is.vector(texts),
    length(texts) > 0,
    is.character(codes),
    is.vector(codes),
    length(codes) > 0
  )
  # Set initial progress
  try(progress_primary$set_with_total(
    1,
    3,
    "Splitting texts..."
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
      split_text = purrr::map(text, function(x) {
        # Split text into semantic chunks
        semchunker(x, overlap = overlap_size_tokens)
      })
    ) |>
    tidyr::unnest(split_text)

  # Verify that longest text does not exceed token limit
  model <- llm_provider$parameters$model
  n_tokens_context_window <- get_context_window_size_in_tokens(model)
  if (is.null(n_tokens_context_window)) n_tokens_context_window <- 2048
  longest_prompt_tokens <- mark_text_prompt(
    text = df$split_text[which.max(count_tokens(df$split_text))],
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

  # For each code & split_text, ask LLM to mark relevant sections in the text
  # Create own row for each text, split_text, code, and marked_text
  total_combinations <- nrow(df) * length(codes)
  current_count <- 0
  try({
    progress_primary$set_with_total(
      2,
      3,
      "Marking texts..."
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
      marked_text = purrr::map2(split_text, code, function(txt, cd) {
        current_count <<- current_count + 1
        try({
          progress_secondary$set_with_total(
            current_count,
            total_combinations,
            paste0("Marking text for code '", cd, "'...")
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
      3,
      "Formatting results..."
    )
  })

  # Clean up the result: remove NA marked_text, normalize, and check for substrings
  # (substring is when overlapped text parts are present in other marked sections;
  # i.e., same text, same code)
  df_result_clean <- df_result |>
    filter(!is.na(marked_text)) |>
    group_by(text, code) |>
    mutate(
      # Normalize for comparison: lowercase, remove punctuation and spaces
      norm_text = str_remove_all(str_to_lower(marked_text), "[[:punct:]\\s]+"),
      is_substring = map_lgl(norm_text, function(txt) {
        others <- setdiff(norm_text, txt)
        any(str_detect(others, fixed(txt)))
      })
    ) |>
    filter(!is_substring) |>
    # Clean final text_parts: remove punctuation, keep spaces
    mutate(marked_text = str_remove_all(marked_text, "[[:punct:]]")) |>
    select(-norm_text, -is_substring, -split_text) |>
    ungroup() |>
    dplyr::distinct()

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
