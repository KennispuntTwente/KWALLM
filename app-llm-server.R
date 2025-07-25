#### 1 Load dependencies ####

# This project uses renv to manage package dependencies;
#   see https://rstudio.github.io/renv/articles/renv.html
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# Install packages with renv
renv::restore()

# Setup Python with reticulate & uv
try({
  reticulate:::uv_exec("sync")
  reticulate::use_virtualenv("./.venv")
})

# Load core packages
library(tidyverse)
library(tidyprompt)
library(shiny)
library(shinyjs)
library(bslib)
library(bsicons)
library(htmltools)
library(future)
library(promises)
library(DT)

# Load components in R/-folder
r_files <- list.files(
  path = "R",
  pattern = "\\.R$",
  full.names = TRUE
)
for (file in r_files) {
  if (!grepl("llmQuali-package\\.R|rstudio_addin\\.R|zzz\\.R", file)) {
    source(file)
  }
}


#### 2 Settings ####

# Set asynchronous processing
# - Asynchronous processing is recommended when deploying the app to a server,
#     where multiple users can use the app simultaneously
# - To enable asynchronous processing, you need to use `future::plan()`, e.g.,
#     `future::plan(multisession)`
# - When you asynchronous processing is not needed, you can use
#     `future::plan("sequential")`; note that the progress bar may lag behind
#     in that case, as this is built around asynchronous processing
# - See the documentation for `future::plan()` for more details
future::plan(multisession, .skip = TRUE)

# Wait for Ollama  to be up
start_time <- Sys.time()
timeout <- 300  # 5 minutes in seconds
ollama_available <- FALSE

repeat {
  try({
    ollama_available <- rollama::ping_ollama()

    if (ollama_available) {
      message("Ollama is available!")
      break
    }
  }, silent = FALSE)

  if (as.numeric(Sys.time() - start_time, units = "secs") > timeout) {

    app_error(
      error = "Ollama server is not available",
      when = "checking ollama availability",
      fatal = TRUE,
      in_shiny = FALSE
    )
  }

  Sys.sleep(5)
}

# Set LLM provider, load available models
llm_provider <- tidyprompt::llm_provider_ollama(num_ctx = 2048)
available_models <- tryCatch(
  {
    rollama::list_models() |>
      dplyr::pull(name)
  },

  error = function(e) {
    print(e)
    NULL
  }
)

if (length(available_models) == 0) {
  app_error(
    error = "No available models on Ollama server",
    when = "checking ollama models",
    fatal = TRUE,
    in_shiny = FALSE
  )
}

llm_provider$parameters$model <- available_models[[1]]
if ("gemma3:27b" %in% available_models) {
  llm_provider$parameters$model <- "gemma3:27b"
  available_models <- c("gemma3:27b", available_models) |> unique()
}
llm_provider$parameters$stream <- FALSE

# Optionally set other options
options(
  # - How the Shiny app is served;
  shiny.port = 8100,
  shiny.host = "0.0.0.0",

  # - Retry behaviour upon LLM API errors;
  #   max tries defines the maximum number of retries
  #   in connecting to the LLM API, while max interactions
  #   defines the maximum number of messages sent to the LLM API
  #   to evaluate the prompt once connected
  #     see: R/send_prompt_with_retries.R
  send_prompt_with_retries__max_tries = 5,
  send_prompt_with_retries__retry_delay_seconds = 3,
  send_prompt_with_retries__max_interactions = 10,

  # - Prompt logging;
  #   if prompts & LLM replies should be written to folder 'prompt_logs',
  #   used primarily for debugging purposes;
  #     see: R/send_prompt_with_retries.R
  send_prompt_with_retries__log_prompts = FALSE,

  # - Maximum number of texts to process at once;
  #     see: R/processing.R
  processing___max_texts = 3000,

  # - Configuration of LLM provider by user;
  #   these enable the user to set their own OpenAI-compatible or Ollama APIs,
  #   as alternative to the preconfigured LLM provider;
  #     see: R/llm_provider.R
  llm_provider__can_configure_oai = FALSE,
  llm_provider__default_oai_url = "https://api.openai.com/v1",
  llm_provider__default_oai_url_chat_suffix = "/chat/completions",
  llm_provider__can_configure_ollama = TRUE,
  llm_provider__default_ollama_url = "http://localhost:11434/api",
  llm_provider__default_ollama_url_chat_suffix = "/chat",

  # - Language for app interface & results (Dutch (nl) or English (en));
  #   see R/language.R
  language = "nl", # Default language
  language__can_toggle = TRUE, # If user can switch language in the app

  # - Default setting for anonymization of texts, and if user
  #   can toggle this setting;
  #     see R/text_management.R
  anonymization__default = "regex", # Default anonymization method ("none', "regex", or "gliner")
  anonymization__none = TRUE, # If the "none" anonymization method is available
  anonymization__regex = TRUE, # If the "regex" anonymization method is available
  anonymization__gliner_model = TRUE, # If the "gliner" anonymization method is available
  anonymization__gliner_test = FALSE, # If gliner model should be tested before launching the app

  # - If a topic 'unknown/not applicable' should always be added
  #   to to the list of candiate topics during topic modelling;
  #   this may be useful to avoid LLM failure in the topic assignment process;
  #     see R/topic_modelling.R
  topic_modelling__always_add_not_applicable = TRUE,
  # - Parameters for text chunking;
  #     see R/context_window.R
  topic_modelling__chunk_size_default = 25,
  topic_modelling__chunk_size_limit = 100,
  topic_modelling__number_of_chunks_limit = 50,
  topic_modelling__draws_default = 1,
  topic_modelling__draws_limit = 5
)

if (getOption("anonymization__gliner_test", FALSE)) {
  invisible(gliner_load_model(test_model = TRUE))
}

if (!getOption("shiny.testmode", FALSE)) {
  try(tiktoken_load_tokenizer())
}


#### 3 Run app ####

# Make images in www folder available to the app
shiny::addResourcePath("www", "www")

shiny::shinyApp(
  ui = main_ui(),
  server = main_server(
    preconfigured_llm_provider = llm_provider,
    preconfigured_main_models = available_models,
    preconfigured_large_models = available_models
  )
)
