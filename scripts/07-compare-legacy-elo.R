#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
source_project_functions()

if (!requireNamespace("aws.s3", quietly = TRUE)) {
  stop(
    "Stage 07 requires the optional package 'aws.s3'. Install it with install.packages('aws.s3').",
    call. = FALSE
  )
}

if (Sys.getenv("AWS_PROFILE") == "") {
  Sys.setenv(AWS_PROFILE = "brian-hurler")
}
if (Sys.getenv("AWS_DEFAULT_REGION") == "") {
  Sys.setenv(AWS_DEFAULT_REGION = "us-west-1")
}

legacy_bucket <- "usavbeach"
legacy_key <- "elo/long_matches_k_factor_30.rda"
legacy_cache <- "data-raw/long_matches_k_factor_30.rda"

dir.create("data-raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data-processed", recursive = TRUE, showWarnings = FALSE)

if (!file.exists(legacy_cache) || force_refresh()) {
  message("Downloading legacy Elo artifact from S3...")
  raw_object <- aws.s3::get_object(
    object = legacy_key,
    bucket = legacy_bucket
  )
  writeBin(raw_object, legacy_cache)
} else {
  message("Using cached legacy Elo artifact: ", legacy_cache)
}

legacy_env <- new.env(parent = emptyenv())
loaded_objects <- load(legacy_cache, envir = legacy_env)

if (!"long_matches" %in% loaded_objects) {
  stop(
    "Legacy RDA did not contain expected object 'long_matches'. Found: ",
    paste(loaded_objects, collapse = ", "),
    call. = FALSE
  )
}

legacy <- tibble::as_tibble(legacy_env$long_matches)
new_history <- read_parquet("data-processed/athlete_elo_history.parquet")

required_legacy <- c(
  "athlete", "date", "gender",
  "athlete_elo_before", "athlete_elo_after"
)
missing_legacy <- setdiff(required_legacy, names(legacy))
if (length(missing_legacy) > 0L) {
  stop(
    "Legacy long_matches is missing expected fields: ",
    paste(missing_legacy, collapse = ", "),
    call. = FALSE
  )
}

legacy_schema <- tibble::tibble(
  column = names(legacy),
  class = vapply(
    legacy,
    function(x) paste(class(x), collapse = "/"),
    character(1)
  )
)
readr::write_csv(
  legacy_schema,
  "data-processed/legacy_elo_schema.csv"
)

legacy <- legacy |>
  dplyr::mutate(
    athlete = as.character(athlete),
    date = as.Date(date),
    legacy_row = dplyr::row_number()
  )

new_history <- new_history |>
  dplyr::mutate(
    athlete_id = as.character(athlete_id),
    date = as.Date(date)
  )

as_of_date <- min(
  Sys.Date(),
  max(new_history$date, na.rm = TRUE),
  max(legacy$date, na.rm = TRUE)
)
active_cutoff <- as_of_date - 365

legacy_current <- legacy |>
  dplyr::filter(date <= as_of_date) |>
  dplyr::group_by(athlete) |>
  dplyr::filter(max(date, na.rm = TRUE) >= active_cutoff) |>
  dplyr::arrange(date, legacy_row, .by_group = TRUE) |>
  dplyr::mutate(legacy_career_matches = dplyr::row_number()) |>
  dplyr::slice_tail(n = 1L) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    athlete_id = athlete,
    legacy_gender = as.character(gender),
    legacy_federation = if ("federation" %in% names(legacy)) {
      as.character(federation)
    } else {
      NA_character_
    },
    legacy_last_match = date,
    legacy_elo = as.numeric(athlete_elo_after),
    legacy_career_matches
  )

new_current <- new_history |>
  dplyr::filter(date <= as_of_date) |>
  dplyr::group_by(athlete_id) |>
  dplyr::filter(max(date, na.rm = TRUE) >= active_cutoff) |>
  dplyr::arrange(
    date,
    dplyr::coalesce(as.character(local_time), "00:00:00"),
    match_no,
    .by_group = TRUE
  ) |>
  dplyr::slice_tail(n = 1L) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    athlete_id,
    display_name,
    new_gender = as.character(gender),
    new_federation = as.character(federation),
    new_last_match = date,
    new_elo = as.numeric(athlete_elo_after),
    new_career_matches = as.integer(career_match_number)
  )

current_comparison <- new_current |>
  dplyr::inner_join(legacy_current, by = "athlete_id") |>
  dplyr::mutate(
    elo_delta_new_minus_legacy = new_elo - legacy_elo,
    abs_elo_delta = abs(elo_delta_new_minus_legacy),
    career_match_delta = new_career_matches - legacy_career_matches,
    last_match_delta_days = as.integer(new_last_match - legacy_last_match),
    gender_agrees = new_gender == legacy_gender
  ) |>
  dplyr::arrange(new_gender, dplyr::desc(abs_elo_delta))

readr::write_csv(
  current_comparison,
  "data-processed/legacy_current_elo_comparison.csv"
)

current_summary <- current_comparison |>
  dplyr::group_by(gender = new_gender) |>
  dplyr::summarise(
    shared_active_athletes = dplyr::n(),
    mean_new_elo = mean(new_elo, na.rm = TRUE),
    mean_legacy_elo = mean(legacy_elo, na.rm = TRUE),
    mean_delta = mean(elo_delta_new_minus_legacy, na.rm = TRUE),
    median_delta = stats::median(elo_delta_new_minus_legacy, na.rm = TRUE),
    mean_abs_delta = mean(abs_elo_delta, na.rm = TRUE),
    median_abs_delta = stats::median(abs_elo_delta, na.rm = TRUE),
    correlation = stats::cor(new_elo, legacy_elo, use = "complete.obs"),
    mean_match_count_delta = mean(career_match_delta, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(
  current_summary,
  "data-processed/legacy_current_elo_summary.csv"
)

legacy_by_year <- legacy |>
  dplyr::filter(!is.na(date)) |>
  dplyr::mutate(year = as.integer(format(date, "%Y"))) |>
  dplyr::count(gender, year, name = "athlete_match_rows") |>
  dplyr::mutate(approx_matches = athlete_match_rows / 4)

new_by_year <- new_history |>
  dplyr::filter(!is.na(date)) |>
  dplyr::mutate(year = as.integer(format(date, "%Y"))) |>
  dplyr::count(gender, year, name = "athlete_match_rows") |>
  dplyr::mutate(matches = athlete_match_rows / 4)

coverage_by_year <- dplyr::full_join(
  new_by_year |>
    dplyr::rename(
      new_athlete_match_rows = athlete_match_rows,
      new_matches = matches
    ),
  legacy_by_year |>
    dplyr::rename(
      legacy_athlete_match_rows = athlete_match_rows,
      legacy_approx_matches = approx_matches
    ),
  by = c("gender", "year")
) |>
  dplyr::mutate(
    athlete_match_row_delta =
      dplyr::coalesce(new_athlete_match_rows, 0L) -
      dplyr::coalesce(legacy_athlete_match_rows, 0L)
  ) |>
  dplyr::arrange(gender, year)

readr::write_csv(
  coverage_by_year,
  "data-processed/legacy_coverage_by_year.csv"
)

overview <- tibble::tibble(
  metric = c(
    "as_of_date",
    "legacy_rows",
    "legacy_unique_athletes",
    "legacy_first_date",
    "legacy_last_date",
    "new_rows",
    "new_unique_athletes",
    "new_first_date",
    "new_last_date",
    "legacy_active_athletes",
    "new_active_athletes",
    "shared_active_athletes"
  ),
  value = c(
    as.character(as_of_date),
    as.character(nrow(legacy)),
    as.character(dplyr::n_distinct(legacy$athlete)),
    as.character(min(legacy$date, na.rm = TRUE)),
    as.character(max(legacy$date, na.rm = TRUE)),
    as.character(nrow(new_history)),
    as.character(dplyr::n_distinct(new_history$athlete_id)),
    as.character(min(new_history$date, na.rm = TRUE)),
    as.character(max(new_history$date, na.rm = TRUE)),
    as.character(nrow(legacy_current)),
    as.character(nrow(new_current)),
    as.character(nrow(current_comparison))
  )
)

readr::write_csv(
  overview,
  "data-processed/legacy_elo_comparison_overview.csv"
)

message("\nLegacy/new Elo comparison overview:")
print(overview, n = Inf)

message("\nCurrent Elo comparison for athletes active in both systems:")
print(current_summary, n = Inf)

message("\nLargest current Elo differences:")
current_comparison |>
  dplyr::select(
    display_name,
    athlete_id,
    new_gender,
    new_elo,
    legacy_elo,
    elo_delta_new_minus_legacy,
    new_career_matches,
    legacy_career_matches,
    career_match_delta,
    new_last_match,
    legacy_last_match
  ) |>
  dplyr::slice_head(n = 20L) |>
  print(n = 20, width = Inf)

if ("tourn_cat" %in% names(legacy)) {
  message("\nLegacy tournament categories:")
  legacy |>
    dplyr::count(gender, tourn_cat, sort = TRUE) |>
    print(n = Inf)
}

message("\nStage 07 complete. Comparison files written to data-processed/.")
