#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
source_project_functions()

tournaments <- read_parquet("data-processed/beach_tournaments.parquet")
type_map <- read_type_map()

classified <- classify_beach_tournaments(tournaments, type_map)

override_path <- "config/tournament-overrides.csv"
if (file.exists(override_path)) {
  overrides <- readr::read_csv(
    override_path,
    show_col_types = FALSE,
    col_types = readr::cols(
      tournament_no = readr::col_double(),
      event_class = readr::col_character(),
      notes = readr::col_character()
    )
  )

  overrides <- overrides |>
    dplyr::filter(!is.na(tournament_no))

  if (anyDuplicated(overrides$tournament_no)) {
    stop("Duplicate tournament_no values in ", override_path, call. = FALSE)
  }

  if (nrow(overrides) > 0L) {
    classified <- classified |>
      dplyr::left_join(
        overrides,
        by = c("no" = "tournament_no"),
        suffix = c("", "_override")
      ) |>
      dplyr::mutate(
        event_class = dplyr::coalesce(event_class_override, event_class),
        classification_source = dplyr::if_else(
          !is.na(event_class_override),
          "manual_override",
          classification_source
        ),
        classification_notes = notes
      ) |>
      dplyr::select(-event_class_override, -notes)
  } else {
    classified$classification_notes <- NA_character_
  }
} else {
  classified$classification_notes <- NA_character_
}

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
