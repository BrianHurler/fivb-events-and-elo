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

tournament_audit <- build_elo_tournament_audit(tournaments, config) |>
  dplyr::select(
    tournament_no,
    event_class,
    gender,
    start_date,
    elo_selection_status,
    elo_selection_reason
  )

matches_audit <- matches |>
  dplyr::mutate(
    local_date = as.Date(local_date),
    match_year = suppressWarnings(as.integer(format(local_date, "%Y"))),
    missing_player_id =
      is.na(no_player_a1) | no_player_a1 <= 0 |
      is.na(no_player_a2) | no_player_a2 <= 0 |
      is.na(no_player_b1) | no_player_b1 <= 0 |
      is.na(no_player_b2) | no_player_b2 <= 0,
    missing_team_id =
      is.na(no_team_a) | no_team_a <= 0 |
      is.na(no_team_b) | no_team_b <= 0,
    decisive_match_points =
      !is.na(match_points_a) &
      !is.na(match_points_b) &
      match_points_a != match_points_b
  ) |>
  dplyr::left_join(
    tournament_audit,
    by = c("no_tournament" = "tournament_no"),
    suffix = c("", "_classified")
  )

overview <- tibble::tibble(
  metric = c(
    "unique_matches",
    "first_match_date",
    "last_match_date",
    "matches_before_2008",
    "matches_2008_onward",
    "matches_in_selected_elo_tournaments",
    "matches_in_review_tournaments",
    "missing_player_ids",
    "missing_team_ids",
    "non_decisive_match_points"
  ),
  value = c(
    as.character(dplyr::n_distinct(matches_audit$no)),
    as.character(min(matches_audit$local_date, na.rm = TRUE)),
    as.character(max(matches_audit$local_date, na.rm = TRUE)),
    as.character(sum(matches_audit$local_date < as.Date("2008-01-01"), na.rm = TRUE)),
    as.character(sum(matches_audit$local_date >= as.Date("2008-01-01"), na.rm = TRUE)),
    as.character(sum(matches_audit$elo_selection_status == "include", na.rm = TRUE)),
    as.character(sum(matches_audit$elo_selection_status == "review", na.rm = TRUE)),
    as.character(sum(matches_audit$missing_player_id, na.rm = TRUE)),
    as.character(sum(matches_audit$missing_team_id, na.rm = TRUE)),
    as.character(sum(!matches_audit$decisive_match_points, na.rm = TRUE))
  )
)

by_year <- matches_audit |>
  dplyr::count(match_year, name = "matches") |>
  dplyr::arrange(match_year)

by_result_type <- matches_audit |>
  dplyr::mutate(
    result_type = dplyr::coalesce(as.character(result_type), "NA"),
    status = dplyr::coalesce(as.character(status), "NA")
  ) |>
  dplyr::count(result_type, status, decisive_match_points, sort = TRUE)

by_elo_class <- matches_audit |>
  dplyr::filter(elo_selection_status == "include") |>
  dplyr::count(gender, event_class, sort = TRUE, name = "matches")

readr::write_csv(
  overview,
  "data-processed/match_archive_audit_overview.csv",
  na = ""
)
readr::write_csv(
  by_year,
  "data-processed/match_archive_by_year.csv",
  na = ""
)
readr::write_csv(
  by_result_type,
  "data-processed/match_archive_result_types.csv",
  na = ""
)
readr::write_csv(
  by_elo_class,
  "data-processed/match_archive_selected_by_class.csv",
  na = ""
)

message("Match archive audit complete.")
print(overview, n = Inf)
