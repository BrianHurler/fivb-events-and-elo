#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
source_project_functions()

player_path <- "data-processed/vis_players.parquet"
history_path <- "data-processed/athlete_elo_history.parquet"
current_path <- "data-processed/athlete_elo_current.parquet"

if (!file.exists(player_path)) {
  stop(
    "Missing ", player_path,
    ". Run source(\"scripts/01b-pull-players.R\") first.",
    call. = FALSE
  )
}

players <- read_parquet(player_path)
history <- read_parquet(history_path)
current <- read_parquet(current_path)

history <- enrich_elo_with_players(history, players)
current <- enrich_elo_with_players(current, players)

write_parquet(history, history_path)
write_parquet(current, current_path)

coverage <- tibble::tibble(
  artifact = c("history", "current"),
  rows = c(nrow(history), nrow(current)),
  named_rows = c(
    sum(!is.na(history$display_name) & history$display_name != ""),
    sum(!is.na(current$display_name) & current$display_name != "")
  ),
  unique_athletes = c(
    dplyr::n_distinct(history$athlete_id),
    dplyr::n_distinct(current$athlete_id)
  ),
  named_unique_athletes = c(
    dplyr::n_distinct(history$athlete_id[!is.na(history$display_name) & history$display_name != ""]),
    dplyr::n_distinct(current$athlete_id[!is.na(current$display_name) & current$display_name != ""])
  )
)

readr::write_csv(
  coverage,
  "data-processed/player_key_coverage.csv"
)

message("Elo player-key enrichment complete.")
print(coverage, n = Inf)
