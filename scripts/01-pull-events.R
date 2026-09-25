#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
initialize_directories()
source_project_functions()

event_cache <- "data-raw/vis_events.rds"
tournament_cache <- "data-raw/beach_tournaments.rds"

if (!force_refresh() && file.exists(event_cache)) {
  events <- readRDS(event_cache)
  message("Using cached VIS events: ", event_cache)
} else {
  message("Requesting full VIS event list...")
  events <- vis_get_events()
  saveRDS(events, event_cache)
}

if (!force_refresh() && file.exists(tournament_cache)) {
  tournaments <- readRDS(tournament_cache)
  message("Using cached beach tournaments: ", tournament_cache)
} else {
  message("Requesting full VIS beach tournament list...")
  tournaments <- vis_get_beach_tournaments()
  saveRDS(tournaments, tournament_cache)
}

write_parquet(events, "data-processed/vis_events.parquet")
write_parquet(tournaments, "data-processed/beach_tournaments.parquet")

summary <- tibble::tibble(
  table = c("vis_events", "beach_tournaments"),
  rows = c(nrow(events), nrow(tournaments)),
  min_date = c(
    if ("start_date" %in% names(events)) {
      as.character(min(as.Date(events$start_date), na.rm = TRUE))
    } else NA_character_,
    if ("start_date_main_draw" %in% names(tournaments)) {
      as.character(min(as.Date(tournaments$start_date_main_draw), na.rm = TRUE))
    } else NA_character_
  ),
  max_date = c(
    if ("end_date" %in% names(events)) {
      as.character(max(as.Date(events$end_date), na.rm = TRUE))
    } else NA_character_,
    if ("end_date_main_draw" %in% names(tournaments)) {
      as.character(max(as.Date(tournaments$end_date_main_draw), na.rm = TRUE))
    } else NA_character_
  )
)

readr::write_csv(summary, "data-processed/event_pull_summary.csv")
message("Event archive stage complete.")
