#### 1 Load semantic chunker ####

# Allows you to interrupt Python without an R crash
Sys.setenv(FOR_DISABLE_CONSOLE_CTRL_HANDLER = "1")

semchunk_load_chunker <- function(
  venv_name = "py-venv",
  python_version = "3.12.10",
  tokenizer = "gpt-4", # name, HF repo, tiktoken encoding, or a custom Python object
  chunk_size = 64, # tokens per chunk (remember to subtract special tokens)
  use_system_python = FALSE,
  docker_env = NULL, # autodetected if NULL
  test_chunker = FALSE, # run a quick round-trip on sample text?
  queue = NULL # 'ipc' queue for reactive logs
) {
  #### ───────── Argument checks ──────────────────────────────────────────────
  stopifnot(
    is.character(venv_name) && length(venv_name) == 1,
    is.character(python_version) && length(python_version) == 1,
    (is.character(tokenizer) || inherits(tokenizer, "python.builtin.object")),
    is.numeric(chunk_size) && length(chunk_size) == 1 && chunk_size > 0,
    is.logical(use_system_python) && length(use_system_python) == 1,
    is.null(docker_env) || (is.logical(docker_env) && length(docker_env) == 1),
    is.logical(test_chunker) && length(test_chunker) == 1
  )

  #### ───────── Helper: log to console (+ shiny queue) ───────────────────────
  # Helper to print messages to the console + queue if needed
  print_message <- function(
    message,
    type = c("info", "success")
  ) {
    type <- match.arg(type)
    if (type == "success") {
      cli::cli_alert_success(message)
      message <- paste0(
        cli::col_green("✔"),
        " ",
        message
      )
    } else {
      message <- paste0(
        cli::col_blue("ℹ"),
        " ",
        message
      )
      cli::cli_alert_info(message)
    }

    if (!is.null(queue)) {
      try(queue$producer$fireAssignReactive(
        "semchunk_message",
        message
      ))
    }
  }

  #### ───────── Detect Docker & pick Python binary ───────────────────────────
  if (is.null(docker_env)) {
    docker_env <- identical(tolower(Sys.getenv("IS_DOCKER")), "true")
    print_message(paste0("Docker environment auto-detected: ", docker_env))
  }

  #### ───────── Create / activate virtual-env ────────────────────────────────
  print_message(paste0(
    "Loading/creating virtual environment (",
    venv_name,
    ") …"
  ))

  if (docker_env) {
    print_message("Inside Docker: assuming Python + semchunk already installed")
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

    #### ───── Install semchunk (+ helpers) if missing ──────────────────
    needed <- c()
    pkgs <- reticulate::py_list_packages(envname = venv_name)$package
    if (!"semchunk" %in% pkgs) needed <- c(needed, "semchunk")

    # Rough heuristic: install helpers only if the user *might* need them
    if (is.character(tokenizer)) {
      if (
        grepl("^cl\\d+k_|^gpt-", tokenizer, ignore.case = TRUE) &&
          !"tiktoken" %in% pkgs
      )
        needed <- c(needed, "tiktoken")
      if (
        grepl("/", tokenizer) && # looks like HF repo name
          !"transformers" %in% pkgs
      )
        needed <- c(needed, "transformers")
    }

    if (length(setdiff(needed, pkgs)) > 0) {
      print_message(paste0(
        "Installing Python packages: ",
        paste(setdiff(needed, pkgs), collapse = ", "),
        " ..."
      ))
      reticulate::py_install(
        envname = venv_name,
        packages = setdiff(needed, pkgs)
      )
    }
  }

  #### ───────── Import semchunk & friends ────────────────────────────────────
  semchunk <- reticulate::import("semchunk")
  # AutoTokenizer or tiktoken only imported lazily if needed
  if (inherits(tokenizer, "python.builtin.object")) {
    tokenizer_obj <- tokenizer
  } else {
    tokenizer_obj <- tokenizer # let semchunk decide what to do with the string
  }

  #### ───────── Build the chunker ────────────────────────────────────────────
  print_message("Constructing chunker …")
  chunker <- semchunk$chunkerify(tokenizer_obj, as.integer(chunk_size))

  #### ───────── (Optional) smoke-test ────────────────────────────────────────
  if (test_chunker) {
    print_message("Testing chunker on sample sentence …")
    sample_text <- "The quick brown fox jumps over the lazy dog."
    # Returns a Python list, automatically converted
    chunks <- chunker(sample_text)
    print_message(paste0("Chunks: ", paste(chunks, collapse = " | ")))
  }

  print_message("semchunk chunker ready!", type = "success")
  invisible(chunker)
}

#### 2 Example/development usage ####

if (FALSE) {
  chunker <- semchunk_load_chunker(chunk_size = 32)

  chunker(
    "Interviewer: So, can you tell me a little about your background and how you got into this field?

    Interviewee: Yeah, absolutely. Um, I started out studying environmental science at college, and—uh—what really drew me in was, like, the intersection between data and sustainability.

    Interviewer: Interesting. Could you elaborate on that?

    Interviewee: Sure. During my final year, I worked on this project where we used satellite imagery to monitor deforestation patterns. That kind of real-world application just clicked for me, you know?

    Interviewer: Mhm. And what happened after graduation?

    Interviewee: I joined a non-profit based in Colorado. We were doing field research, but I found myself gravitating toward the data side of things—cleaning it, analyzing trends, that sort of stuff.

    Interviewer: So more behind the scenes?

    Interviewee: Yeah, exactly. Although, I still did some fieldwork. I guess I like having that balance.

    Interviewer: Got it. Uh, let’s shift gears a bit. Can you talk about a challenge you faced and how you handled it?

    Interviewee: Sure thing. At one point, our dataset had, like, hundreds of inconsistencies—misspelled place names, duplicated entries, the whole mess. I ended up writing an R script to clean and standardize the entries. Took a while, but it was worth it.

    Interviewer: Sounds like you’re pretty comfortable coding?

    Interviewee: Yeah, I'd say so. R is kind of my go-to.

    Interviewer: Last question—where do you see yourself in five years?

    Interviewee: Hmm, hopefully leading a data science team that’s tackling real environmental problems. Either that or running a goat farm. laughs"
  )

  # Fake interviews
  interview_texts <- c(
    "Interviewer: Let's start with your background.\nInterviewee: Sure, I studied marine biology and moved into conservation work after that.\nInterviewer: What drew you to that?\nInterviewee: I've always loved the ocean, and data lets us protect it better.",

    "Interviewer: What’s your current role?\nInterviewee: I'm a data analyst at a healthcare startup.\nInterviewer: And your path there?\nInterviewee: Started with epidemiology, then moved into R programming and dashboards.",

    "Interviewer: Tell me about a challenge you faced.\nInterviewee: Sure, we had a noisy dataset with missing entries. I wrote a pipeline in R to clean it, visualize gaps, and patch using external sources.\nInterviewer: Sounds intense.\nInterviewee: It was, but satisfying.",

    "Interviewer: Where do you see yourself in five years?\nInterviewee: Hopefully leading a research team or consulting in climate tech.\nInterviewer: Interesting.\nInterviewee: Yeah, maybe also teaching part-time if time allows.",

    "Interviewer: What’s one project you’re proud of?\nInterviewee: A survey tool I built in Shiny to collect real-time feedback from field teams. It synced to a database and auto-generated reports. Made life easier for everyone."
  )

  interview_df <- tibble(
    id = paste0("resp_", seq_along(interview_texts)),
    text = interview_texts
  )

  # Apply chunking to each text row
  chunked_df <- interview_df %>%
    mutate(
      chunks = map(text, function(x) {
        chunker(x, overlap = TRUE)
      })
    ) %>%
    select(id, chunks) %>%
    unnest_longer(chunks, values_to = "chunk_text")

  # View the chunked result
  print(chunked_df)
}
