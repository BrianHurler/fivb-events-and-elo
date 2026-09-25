#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
source_project_functions()

config <- read_elo_config()
tournaments <- read_parquet(
  "data-processed/beach_tournaments_classified.parquet"
)

audit <- build_elo_tournament_audit(tournaments, config)

selected <- audit |>
  dplyr::filter(elo_selection_status == "include")

review <- audit |>
  dplyr::filter(elo_selection_status == "review")

continental <- audit |>
  dplyr::filter(continental_candidate)

readr::write_csv(
  selected,
  "data-processed/elo_tournaments_proposed.csv",
  na = ""
)

readr::write_csv(
  audit,
  "data-processed/elo_tournament_selection_audit.csv",
  na = ""
)

readr::write_csv(
  review,
  "data-processed/elo_tournaments_needing_review.csv",
  na = ""
)

readr::write_csv(
  continental,
  "data-processed/elo_continental_tournament_audit.csv",
  na = ""
)

summary <- audit |>
  dplyr::count(
    elo_selection_status,
    event_class,
    sort = TRUE,
    name = "tournaments"
  )

readr::write_csv(
  summary,
  "data-processed/elo_tournament_selection_summary.csv",
  na = ""
)

message(
  "Elo tournament review files written for profile '",
  config$profile_name,
  "': ",
  nrow(selected), " proposed includes; ",
  nrow(review), " tournaments require review; ",
  sum(continental$elo_selection_status == "include"), " continental includes."
)
