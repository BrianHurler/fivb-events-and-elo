#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
initialize_directories()
source_project_functions()

player_cache <- "data-raw/vis_beach_players.rds"

if (!force_refresh() && file.exists(player_cache)) {
  players <- readRDS(player_cache)
  message("Using cached VIS beach players: ", player_cache)
} else {
  message("Requesting VIS beach player directory...")
  players <- vis_get_beach_players()
  saveRDS(players, player_cache)
}

write_parquet(
  players,
  "data-processed/vis_players.parquet"
)

message(
  "Player directory stage complete: ",
  nrow(players),
  " beach players."
)
