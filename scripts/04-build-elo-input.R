#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
source_project_functions()

config <- read_elo_config()
matches <- read_parquet("data-processed/beach_matches.parquet")
tournaments <- read_parquet(
  "data-processed/beach_tournaments_classified.parquet"
)

elo_matches <- prepare_elo_matches(matches, tournaments, config)

write_parquet(elo_matches, "data-processed/elo_matches.parquet")

summary <- elo_matches |>
  dplyr::count(gender, event_class, name = "matches") |>
  dplyr::arrange(gender, dplyr::desc(matches))

readr::write_csv(summary, "data-processed/elo_match_selection_summary.csv")

message(
  "Elo input stage complete for profile '",
  config$profile_name,
  "': ",
  nrow(elo_matches),
  " matches."
)
