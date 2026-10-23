#!/usr/bin/env Rscript

# Stage 03c: pull rally-by-rally scores with GetBeachLive for matches in the
# beach match archive. Not part of run-all.R because a full pull is one request
# per match. Run it explicitly:
#
#   Sys.setenv(FIVB_RALLY_MODE = "probe")   # default: 2 matches per tournament
#   source("scripts/03c-pull-rallies.R")
#   Sys.setenv(FIVB_RALLY_MODE = "full")    # every match in covered tournaments
#   source("scripts/03c-pull-rallies.R")
#
# Settings (environment variables):
#   FIVB_RALLY_MODE        "probe" or "full" (default "probe")
#   FIVB_RALLY_START_DATE  earliest tournament main-draw date (default 2021-01-01;
#                          VIS release notes first mention GetBeachLive rally
#                          statistics in mid-2021)
#   FIVB_RALLY_OPTIONS     GetBeachLive export options (default 576; 605 adds
#                          serving team and time offsets)
#   FIVB_REQUEST_PAUSE_SECONDS  pause between requests (shared with stage 03)
#   FIVB_FORCE_REFRESH     re-request matches already cached
#
# Per-match results are cached per tournament under data-raw/rallies so the
# pull can restart, and probe results are reused by the full pull.

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
initialize_directories()
source_project_functions()

mode <- tolower(Sys.getenv("FIVB_RALLY_MODE", unset = "probe"))
if (!mode %in% c("probe", "full")) {
  stop("FIVB_RALLY_MODE must be 'probe' or 'full'.", call. = FALSE)
}
start_date <- as.Date(Sys.getenv("FIVB_RALLY_START_DATE", unset = "2021-01-01"))
live_options <- as.integer(Sys.getenv("FIVB_RALLY_OPTIONS", unset = VIS_BEACH_LIVE_OPTIONS))
probe_matches_per_tournament <- 2L

pause <- request_pause_seconds()
refresh <- force_refresh()
cache_dir <- "data-raw/rallies/by-tournament"
xml_dir <- "data-raw/rallies/xml"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(xml_dir, recursive = TRUE, showWarnings = FALSE)

matches <- read_parquet("data-processed/beach_matches.parquet")
tournaments <- read_parquet("data-processed/beach_tournaments_classified.parquet")

# Match universe: normally completed matches with a real first set, in
# tournaments starting on or after start_date. Forfeits and injuries have no
# point sequence worth requesting.
candidate_tournaments <- tournaments |>
  dplyr::mutate(start_date_main_draw = as.Date(start_date_main_draw)) |>
  dplyr::filter(!is.na(start_date_main_draw), start_date_main_draw >= start_date) |>
  dplyr::arrange(start_date_main_draw, no) |>
  dplyr::select(tournament_no = no, start_date_main_draw)

candidate_matches <- matches |>
  dplyr::filter(
    dplyr::coalesce(result_type, 0L) == 0L,
    pmax(
      dplyr::coalesce(points_team_aset1, 0L),
      dplyr::coalesce(points_team_bset1, 0L)
    ) >= 15L
  ) |>
  dplyr::select(
    match_no = no, tournament_no = no_tournament, no_in_tournament,
    local_date, match_points_a, match_points_b
  ) |>
  dplyr::inner_join(candidate_tournaments, by = "tournament_no") |>
  dplyr::arrange(start_date_main_draw, tournament_no, no_in_tournament)

# In full mode, restrict to tournaments where the probe found rallies, when a
# probe coverage file exists. Without one, every candidate tournament is pulled.
coverage_path <- "data-processed/rally_coverage.csv"
if (mode == "full" && file.exists(coverage_path)) {
  covered <- readr::read_csv(coverage_path, show_col_types = FALSE) |>
    dplyr::filter(matches_with_rallies > 0L) |>
    dplyr::pull(tournament_no)
  candidate_matches <- candidate_matches |>
    dplyr::filter(tournament_no %in% covered)
  message("Full mode: ", length(covered), " tournaments with probe coverage.")
}

# In probe mode, request the first and last match of each tournament so both
# qualification/early rounds and finals are checked.
if (mode == "probe") {
  candidate_matches <- candidate_matches |>
    dplyr::group_by(tournament_no) |>
    dplyr::arrange(no_in_tournament, .by_group = TRUE) |>
    dplyr::filter(
      dplyr::row_number() <= ceiling(probe_matches_per_tournament / 2) |
        dplyr::row_number() > dplyr::n() - floor(probe_matches_per_tournament / 2)
    ) |>
    dplyr::ungroup()
}

message(
  "Rally pull (", mode, "): ", nrow(candidate_matches), " matches in ",
  dplyr::n_distinct(candidate_matches$tournament_no), " tournaments from ",
  start_date, "."
)

# One cache file per tournament holding a per-match manifest and the rallies
# pulled so far. Matches already in the manifest are skipped unless refreshed.
empty_rallies <- vis_parse_beach_live_rallies("<NoData/>", 0L)

tournament_nos <- unique(candidate_matches$tournament_no)
for (i in seq_along(tournament_nos)) {
  this_tournament <- as.integer(tournament_nos[[i]])
  cache_path <- file.path(cache_dir, sprintf("%08d.rds", this_tournament))
  tournament_xml_dir <- file.path(xml_dir, sprintf("%08d", this_tournament))

  cache <- if (!refresh && file.exists(cache_path)) {
    readRDS(cache_path)
  } else {
    list(manifest = tibble::tibble(), rallies = empty_rallies)
  }

  todo <- candidate_matches |>
    dplyr::filter(tournament_no == this_tournament)
  if (nrow(cache$manifest) > 0L) {
    todo <- todo |> dplyr::filter(!match_no %in% cache$manifest$match_no)
  }
  if (nrow(todo) == 0L) next

  dir.create(tournament_xml_dir, recursive = TRUE, showWarnings = FALSE)

  for (j in seq_len(nrow(todo))) {
    this_match <- as.integer(todo$match_no[[j]])

    pulled <- tryCatch(
      {
        xml_text <- vis_get_beach_live_xml(this_match, options = live_options)
        writeLines(xml_text, file.path(tournament_xml_dir, paste0(this_match, ".xml")))
        rallies <- vis_parse_beach_live_rallies(xml_text, this_match)
        list(rallies = rallies, status = attr(rallies, "live_status"), error = NA_character_)
      },
      error = function(e) {
        list(rallies = empty_rallies, status = "error", error = conditionMessage(e))
      }
    )

    cache$manifest <- dplyr::bind_rows(
      cache$manifest,
      tibble::tibble(
        tournament_no = this_tournament,
        match_no = this_match,
        pulled_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        live_options = live_options,
        live_status = pulled$status,
        n_rallies = nrow(pulled$rallies),
        error = pulled$error
      )
    )
    cache$rallies <- dplyr::bind_rows(cache$rallies, pulled$rallies)

    if (pause > 0) Sys.sleep(pause)
  }

  saveRDS(cache, cache_path)

  if (i %% 25L == 0L || i == length(tournament_nos)) {
    message("Processed ", i, " / ", length(tournament_nos), " tournaments.")
  }
}

# Combine every cache (probe and full runs together) into the processed layer.
cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
if (length(cache_files) == 0L) {
  stop("No rally caches were created. Check the candidate match universe.", call. = FALSE)
}

caches <- purrr::map(cache_files, readRDS)
manifest <- purrr::map(caches, "manifest") |> dplyr::bind_rows()
rallies <- purrr::map(caches, "rallies") |>
  dplyr::bind_rows() |>
  dplyr::distinct(match_no, set_no, rally_no, .keep_all = TRUE) |>
  dplyr::arrange(match_no, set_no, rally_no)

readr::write_csv(manifest, "data-processed/rally_pull_manifest.csv")
write_parquet(rallies, "data-processed/beach_rallies.parquet")

# Tournament-level coverage: how many requested matches came back with rallies.
coverage <- manifest |>
  dplyr::group_by(tournament_no) |>
  dplyr::summarise(
    matches_requested = dplyr::n(),
    matches_with_rallies = sum(live_status == "rallies"),
    matches_no_data = sum(live_status %in% c("NoData", "no_rallies")),
    matches_access_denied = sum(live_status == "AccessDenied"),
    matches_error = sum(live_status == "error"),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    tournaments |>
      dplyr::select(tournament_no = no, code, name, gender, event_class, start_date_main_draw),
    by = "tournament_no"
  ) |>
  dplyr::arrange(start_date_main_draw, tournament_no)

readr::write_csv(coverage, coverage_path)

message(
  "Rally stage complete: ", nrow(rallies), " rallies from ",
  sum(manifest$live_status == "rallies"), " of ", nrow(manifest),
  " requested matches; ", sum(coverage$matches_with_rallies > 0L), " of ",
  nrow(coverage), " tournaments have live data."
)
print(
  coverage |>
    dplyr::group_by(event_class) |>
    dplyr::summarise(
      tournaments = dplyr::n(),
      with_rallies = sum(matches_with_rallies > 0L),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(tournaments)),
  n = 50
)
