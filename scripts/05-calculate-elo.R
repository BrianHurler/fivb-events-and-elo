#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
source_project_functions()

config <- read_elo_config()
matches <- read_parquet("data-processed/elo_matches.parquet")

history <- calculate_athlete_elo(
  matches,
  initial_rating = config$elo$initial_rating,
  k_factor = config$elo$k_factor,
  scale = config$elo$scale
)

current <- build_current_elo(history)

write_parquet(history, "data-processed/athlete_elo_history.parquet")
write_parquet(current, "data-processed/athlete_elo_current.parquet")

message(
  "Elo calculation complete: ",
  nrow(history), " athlete-match rows for ",
  dplyr::n_distinct(history$athlete_id), " athletes."
)
