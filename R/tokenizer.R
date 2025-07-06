#### 1  Load/attach a tiktoken tokenizer  ####################################

tiktoken_load_tokenizer <- function(
  venv_name = "py-venv",
  python_version = "3.12.10",
  encoding = "gpt-4o", # model name *or* tiktoken encoding name
  use_system_python = FALSE,
  docker_env = NULL, # autodetect if NULL
  test_tokenizer = FALSE,
  reload_if_exists = FALSE, # if TRUE, reload existing tokenizer if already in global env
  queue = NULL # 'ipc' queue for reactive logs
) {
  ## ── quick console/queue logger ────────────────────────────────────────────
  print_message <- function(msg, type = c("info", "success")) {
    type <- match.arg(type)
    if (type == "success") {
      cli::cli_alert_success(msg)
      msg <- paste0(cli::col_green("✔ "), msg)
    } else {
      cli::cli_alert_info(msg)
      msg <- paste0(cli::col_blue("ℹ "), msg)
    }
    if (!is.null(queue)) {
      try(queue$producer$fireAssignReactive("tiktoken_message", msg))
    }
  }

  ## ── check existence ────────────────────────────────────────────
  if (
    !reload_if_exists &&
      exists("tiktoken_tokenizer", envir = .GlobalEnv, inherits = FALSE)
  ) {
    print_message("tiktoken tokenizer already loaded, skipping creation")
    return(invisible(get("tiktoken_tokenizer", envir = .GlobalEnv)))
  }

  ## ── detect Docker + pick Python binary ────────────────────────────────────
  if (is.null(docker_env)) {
    docker_env <- identical(tolower(Sys.getenv("IS_DOCKER")), "true")
    print_message(paste0("Docker environment auto-detected: ", docker_env))
  }

  ## ── create / activate virtual-env ─────────────────────────────────────────
  print_message(paste0(
    "Loading/creating virtual environment (",
    venv_name,
    ") …"
  ))

  if (docker_env) {
    print_message("Inside Docker: assuming Python + tiktoken already installed")
    reticulate::use_virtualenv("/opt/py-venv", required = TRUE)
  } else {
    Sys.setenv(RETICULATE_VIRTUALENV_ROOT = getwd())

    if (!reticulate::virtualenv_exists(venv_name)) {
      py_exec <- if (use_system_python) {
        print_message("Using system Python at /usr/bin/python3")
        "/usr/bin/python3"
      } else {
        print_message("Installing Python via pyenv …")
        reticulate::install_python(python_version)
        python_version
      }
      reticulate::virtualenv_create(envname = venv_name, python = py_exec)
    }

    reticulate::use_virtualenv(venv_name, required = TRUE)

    ## install tiktoken if needed
    pkgs <- reticulate::py_list_packages(envname = venv_name)$package
    if (!"tiktoken" %in% pkgs) {
      print_message("Installing Python package: tiktoken …")
      reticulate::py_install(envname = venv_name, packages = "tiktoken")
    }
  }

  ## ── import tiktoken & build tokenizer ─────────────────────────────────────
  tk <- reticulate::import("tiktoken")

  tok <- tryCatch(
    {
      if (grepl("^gpt[-0-9]", encoding, ignore.case = TRUE))
        tk$encoding_for_model(encoding) else tk$get_encoding(encoding)
    },
    error = function(e) {
      stop("Failed to create tokenizer: ", e$message, call. = FALSE)
    }
  )

  ## ── smoke-test? ───────────────────────────────────────────────────────────
  if (test_tokenizer) {
    print_message("Testing tokenizer on sample text …")
    sample <- "The quick brown fox jumps over the lazy dog."
    print_message(paste0("Token count: ", length(tok$encode(sample))))
    print_message(paste(tok$encode(sample), collapse = ", "))
  }

  ## ── stash in global env & return invisibly ────────────────────────────────
  assign("tiktoken_tokenizer", tok, envir = .GlobalEnv)
  print_message("tiktoken tokenizer ready!", type = "success")
  invisible(tok)
}


#### 2  Simple token-counter  #################################################

count_tokens <- function(
  text,
  tokenizer = NULL,
  fallback_chars_per_token = 4
) {
  if (is.null(tokenizer)) {
    if (exists("tiktoken_tokenizer", envir = .GlobalEnv, inherits = FALSE))
      tokenizer <- get("tiktoken_tokenizer", envir = .GlobalEnv)
  }

  if (!is.null(tokenizer) && inherits(tokenizer, "python.builtin.object")) {
    # vectorise over character vectors
    vapply(
      text,
      function(x) length(tokenizer$encode(as.character(x))),
      integer(1)
    )
  } else {
    # crude fallback: 1 token ≈ 4 characters
    ceiling(nchar(text) / fallback_chars_per_token)
  }
}
