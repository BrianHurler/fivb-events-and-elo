required_packages <- function() {
  c("arrow", "dplyr", "httr2", "purrr", "readr", "stringr", "tibble", "xml2", "yaml")
}

assert_project_root <- function() {
  required <- c("R", "scripts", "config")
  missing <- required[!file.exists(required)]
  if (length(missing) > 0L) {
    stop(
      "Run this pipeline from the repository root. Missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

check_packages <- function() {
  missing <- required_packages()[
    !vapply(required_packages(), requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0L) {
    stop(
      "Install required packages first: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

initialize_directories <- function() {
  dirs <- c(
    "data-raw",
    "data-raw/matches",
    "data-raw/matches/by-tournament",
    "data-processed"
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

source_project_functions <- function() {
  files <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE))
  invisible(lapply(files, source))
}

force_refresh <- function() {
  tolower(Sys.getenv("FIVB_FORCE_REFRESH", unset = "0")) %in%
    c("1", "true", "yes", "y")
}

request_pause_seconds <- function() {
  as.numeric(Sys.getenv("FIVB_REQUEST_PAUSE_SECONDS", unset = "0.10"))
}

read_elo_config <- function() {
  yaml::read_yaml("config/elo.yml")
}

read_type_map <- function() {
  readr::read_csv(
    "config/tournament-types.csv",
    show_col_types = FALSE,
    col_types = readr::cols(
      type_value = readr::col_integer(),
      type_name = readr::col_character(),
      event_class = readr::col_character(),
      senior_international = readr::col_logical()
    )
  )
}

write_parquet <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(x, path)
  message("Wrote ", nrow(x), " rows to ", path)
  invisible(path)
}

read_parquet <- function(path) {
  if (!file.exists(path)) stop("Missing required input: ", path, call. = FALSE)
  arrow::read_parquet(path) |> tibble::as_tibble()
}
