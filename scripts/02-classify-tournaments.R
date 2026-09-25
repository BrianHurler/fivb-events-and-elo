#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
source_project_functions()

tournaments <- read_parquet("data-processed/beach_tournaments.parquet")
type_map <- read_type_map()

classified <- classify_beach_tournaments(tournaments, type_map)

write_parquet(
  classified,
  "data-processed/beach_tournaments_classified.parquet"
)

classification_summary <- classified |>
  dplyr::count(
    gender,
    event_class,
    classification_source,
    sort = TRUE,
    name = "tournaments"
  )

readr::write_csv(
  classification_summary,
  "data-processed/tournament_classification_summary.csv"
)

unclassified <- classified |>
  dplyr::filter(is.na(event_class) | classification_source == "unclassified")

readr::write_csv(
  unclassified,
  "data-processed/tournaments_needing_classification_review.csv"
)

message(
  "Tournament classification complete. ",
  nrow(unclassified),
  " tournaments need classification review."
)
