#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
source_project_functions()

config <- read_elo_config()
tournaments <- read_parquet(
  "data-processed/beach_tournaments_classified.parquet"
)
matches <- read_parquet("data-processed/beach_matches.parquet")
elo_matches <- read_parquet("data-processed/elo_matches.parquet")
history <- read_parquet("data-processed/athlete_elo_history.parquet")
current <- read_parquet("data-processed/athlete_elo_current.parquet")

first_rows <- history |>
  dplyr::arrange(
    date,
    dplyr::coalesce(as.character(local_time), "00:00:00"),
    match_no
  ) |>
  dplyr::group_by(athlete_id) |>
  dplyr::slice_head(n = 1L) |>
  dplyr::ungroup()

checks <- tibble::tibble(
  check = c(
    "tournament_no_unique",
    "match_no_unique",
    "elo_match_no_unique",
    "four_elo_rows_per_match",
    "current_one_row_per_athlete",
    "first_rating_equals_initial",
    "all_elo_values_finite"
  ),
  passed = c(
    dplyr::n_distinct(tournaments$no) == nrow(tournaments),
    dplyr::n_distinct(matches$no) == nrow(matches),
    dplyr::n_distinct(elo_matches$no) == nrow(elo_matches),
    nrow(history) == 4L * nrow(elo_matches),
    dplyr::n_distinct(current$athlete_id) == nrow(current),
    all(abs(first_rows$athlete_elo_before - config$elo$initial_rating) < 1e-9),
    all(
      is.finite(history$athlete_elo_before) &
        is.finite(history$athlete_elo_after) &
        is.finite(history$athlete_elo_change) &
        is.finite(history$expected_score)
    )
  )
)

readr::write_csv(checks, "data-processed/pipeline_validation.csv")

if (any(!checks$passed)) {
  failed <- checks$check[!checks$passed]
  stop(
    "Validation failed: ",
    paste(failed, collapse = ", "),
    call. = FALSE
  )
}

message("All pipeline validation checks passed.")
