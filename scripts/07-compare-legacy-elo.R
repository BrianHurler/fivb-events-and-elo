#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
source_project_functions()

is_ec2_environment <- function() {
  files <- c(
    "/sys/hypervisor/uuid",
    "/sys/devices/virtual/dmi/id/product_uuid"
  )
  values <- unlist(
    lapply(
      files[file.exists(files)],
      function(path) {
        tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) "")
      }
    ),
    use.names = FALSE
  )
  any(grepl("^ec2", values, ignore.case = TRUE))
}

configure_aws_environment <- function(config) {
  Sys.setenv(
    AWS_DEFAULT_REGION = config$region,
    AWS_S3_SIGNATURE_VERSION = "s3v4"
  )
  if (is_ec2_environment()) {
    Sys.unsetenv("AWS_PROFILE")
  } else {
    Sys.setenv(AWS_PROFILE = config$local_aws_profile)
  }
  invisible(TRUE)
}

download_s3_file <- function(bucket, object, region, destination) {
  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    stop(
      "Package `aws.s3` is required to download the canonical Elo history.",
      call. = FALSE
    )
  }

  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  tryCatch(
    aws.s3::save_object(
      object = object,
      bucket = bucket,
      file = destination,
      region = region
    ),
    error = function(error) {
      stop(
        "Unable to download s3://", bucket, "/", object,
        " using AWS profile `",
        Sys.getenv("AWS_PROFILE", unset = "<EC2 IAM role>"),
        "` in region `", region, "`: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
  downloaded <- file.exists(destination) &&
    !is.na(file.info(destination)$size) &&
    file.info(destination)$size > 0
  if (!downloaded) {
    stop(
      "Unable to download s3://", bucket, "/", object,
      " using AWS profile `",
      Sys.getenv("AWS_PROFILE", unset = "<EC2 IAM role>"),
      "` in region `", region, "`.",
      call. = FALSE
    )
  }
  destination
}

legacy_config <- list(
  bucket = "usavbeach",
  object = "elo/long_matches_k_factor_30.rda",
  object_name = "long_matches",
  region = "us-west-1",
  local_aws_profile = "brian-hurler"
)

legacy_bucket <- legacy_config$bucket
legacy_key <- legacy_config$object
legacy_cache <- "data-raw/long_matches_k_factor_30.rda"

dir.create("data-raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data-processed", recursive = TRUE, showWarnings = FALSE)

configure_aws_environment(legacy_config)

if (!file.exists(legacy_cache) || force_refresh()) {
  message(
    "Downloading legacy Elo artifact from S3 using AWS profile '",
    Sys.getenv("AWS_PROFILE", unset = "<EC2 IAM role>"),
    "'..."
  )
  download_s3_file(
    legacy_config$bucket,
    legacy_config$object,
    legacy_config$region,
    legacy_cache
  )
} else {
  message("Using cached legacy Elo artifact: ", legacy_cache)
}

legacy_env <- new.env(parent = emptyenv())
loaded_objects <- load(legacy_cache, envir = legacy_env)

if (!"long_matches" %in% loaded_objects) {
  stop(
    "Legacy RDA did not contain expected object 'long_matches'. Found: ",
    paste(loaded_objects, collapse = ", "),
    call. = FALSE
  )
}

legacy <- tibble::as_tibble(legacy_env$long_matches)
new_history <- read_parquet("data-processed/athlete_elo_history.parquet")

required_legacy <- c(
  "athlete", "date", "gender",
  "athlete_elo_before", "athlete_elo_after"
)
missing_legacy <- setdiff(required_legacy, names(legacy))
if (length(missing_legacy) > 0L) {
  stop(
    "Legacy long_matches is missing expected fields: ",
    paste(missing_legacy, collapse = ", "),
    call. = FALSE
  )
}

legacy_schema <- tibble::tibble(
  column = names(legacy),
  class = vapply(
    legacy,
    function(x) paste(class(x), collapse = "/"),
    character(1)
  )
)
readr::write_csv(
  legacy_schema,
  "data-processed/legacy_elo_schema.csv"
)

legacy <- legacy |>
  dplyr::mutate(
    athlete = as.character(athlete),
    date = as.Date(date),
    legacy_row = dplyr::row_number()
  )

new_history <- new_history |>
  dplyr::mutate(
    athlete_id = as.character(athlete_id),
    date = as.Date(date)
  )

as_of_date <- min(
  Sys.Date(),
  max(new_history$date, na.rm = TRUE),
  max(legacy$date, na.rm = TRUE)
)
active_cutoff <- as_of_date - 365

legacy_current <- legacy |>
  dplyr::filter(date <= as_of_date) |>
  dplyr::group_by(athlete) |>
  dplyr::filter(max(date, na.rm = TRUE) >= active_cutoff) |>
  dplyr::arrange(date, legacy_row, .by_group = TRUE) |>
  dplyr::mutate(legacy_career_matches = dplyr::row_number()) |>
  dplyr::slice_tail(n = 1L) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    athlete_id = athlete,
    legacy_gender = as.character(gender),
    legacy_federation = if ("federation" %in% names(legacy)) {
      as.character(federation)
    } else {
      NA_character_
    },
    legacy_last_match = date,
    legacy_elo = as.numeric(athlete_elo_after),
    legacy_career_matches
  )

new_current <- new_history |>
  dplyr::filter(date <= as_of_date) |>
  dplyr::group_by(athlete_id) |>
  dplyr::filter(max(date, na.rm = TRUE) >= active_cutoff) |>
  dplyr::arrange(
    date,
    dplyr::coalesce(as.character(local_time), "00:00:00"),
    match_no,
    .by_group = TRUE
  ) |>
  dplyr::slice_tail(n = 1L) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    athlete_id,
    display_name,
    new_gender = as.character(gender),
    new_federation = as.character(federation),
    new_last_match = date,
    new_elo = as.numeric(athlete_elo_after),
    new_career_matches = as.integer(career_match_number)
  )

current_comparison <- new_current |>
  dplyr::inner_join(legacy_current, by = "athlete_id") |>
  dplyr::mutate(
    elo_delta_new_minus_legacy = new_elo - legacy_elo,
    abs_elo_delta = abs(elo_delta_new_minus_legacy),
    career_match_delta = new_career_matches - legacy_career_matches,
    last_match_delta_days = as.integer(new_last_match - legacy_last_match),
    gender_agrees = new_gender == legacy_gender
  ) |>
  dplyr::arrange(new_gender, dplyr::desc(abs_elo_delta))

readr::write_csv(
  current_comparison,
  "data-processed/legacy_current_elo_comparison.csv"
)

current_summary <- current_comparison |>
  dplyr::group_by(gender = new_gender) |>
  dplyr::summarise(
    shared_active_athletes = dplyr::n(),
    mean_new_elo = mean(new_elo, na.rm = TRUE),
    mean_legacy_elo = mean(legacy_elo, na.rm = TRUE),
    mean_delta = mean(elo_delta_new_minus_legacy, na.rm = TRUE),
    median_delta = stats::median(elo_delta_new_minus_legacy, na.rm = TRUE),
    mean_abs_delta = mean(abs_elo_delta, na.rm = TRUE),
    median_abs_delta = stats::median(abs_elo_delta, na.rm = TRUE),
    correlation = stats::cor(new_elo, legacy_elo, use = "complete.obs"),
    mean_match_count_delta = mean(career_match_delta, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(
  current_summary,
  "data-processed/legacy_current_elo_summary.csv"
)

legacy_by_year <- legacy |>
  dplyr::filter(!is.na(date)) |>
  dplyr::mutate(year = as.integer(format(date, "%Y"))) |>
  dplyr::count(gender, year, name = "athlete_match_rows") |>
  dplyr::mutate(approx_matches = athlete_match_rows / 4)

new_by_year <- new_history |>
  dplyr::filter(!is.na(date)) |>
  dplyr::mutate(year = as.integer(format(date, "%Y"))) |>
  dplyr::count(gender, year, name = "athlete_match_rows") |>
  dplyr::mutate(matches = athlete_match_rows / 4)

coverage_by_year <- dplyr::full_join(
  new_by_year |>
    dplyr::rename(
      new_athlete_match_rows = athlete_match_rows,
      new_matches = matches
    ),
  legacy_by_year |>
    dplyr::rename(
      legacy_athlete_match_rows = athlete_match_rows,
      legacy_approx_matches = approx_matches
    ),
  by = c("gender", "year")
) |>
  dplyr::mutate(
    athlete_match_row_delta =
      dplyr::coalesce(new_athlete_match_rows, 0L) -
      dplyr::coalesce(legacy_athlete_match_rows, 0L)
  ) |>
  dplyr::arrange(gender, year)

readr::write_csv(
  coverage_by_year,
  "data-processed/legacy_coverage_by_year.csv"
)

overview <- tibble::tibble(
  metric = c(
    "as_of_date",
    "legacy_rows",
    "legacy_unique_athletes",
    "legacy_first_date",
    "legacy_last_date",
    "new_rows",
    "new_unique_athletes",
    "new_first_date",
    "new_last_date",
    "legacy_active_athletes",
    "new_active_athletes",
    "shared_active_athletes"
  ),
  value = c(
    as.character(as_of_date),
    as.character(nrow(legacy)),
    as.character(dplyr::n_distinct(legacy$athlete)),
    as.character(min(legacy$date, na.rm = TRUE)),
    as.character(max(legacy$date, na.rm = TRUE)),
    as.character(nrow(new_history)),
    as.character(dplyr::n_distinct(new_history$athlete_id)),
    as.character(min(new_history$date, na.rm = TRUE)),
    as.character(max(new_history$date, na.rm = TRUE)),
    as.character(nrow(legacy_current)),
    as.character(nrow(new_current)),
    as.character(nrow(current_comparison))
  )
)

readr::write_csv(
  overview,
  "data-processed/legacy_elo_comparison_overview.csv"
)

message("\nLegacy/new Elo comparison overview:")
print(overview, n = Inf)

message("\nCurrent Elo comparison for athletes active in both systems:")
print(current_summary, n = Inf)

message("\nLargest current Elo differences:")
current_comparison |>
  dplyr::select(
    display_name,
    athlete_id,
    new_gender,
    new_elo,
    legacy_elo,
    elo_delta_new_minus_legacy,
    new_career_matches,
    legacy_career_matches,
    career_match_delta,
    new_last_match,
    legacy_last_match
  ) |>
  dplyr::slice_head(n = 20L) |>
  print(n = 20, width = Inf)

# -------------------------------------------------------------------------
# Cross-source match overlap using date + gender + four VIS player IDs.
#
# Legacy match_id and current VIS BeachMatch No are different identifier
# namespaces, so they must never be joined directly.
# -------------------------------------------------------------------------

raw_matches <- read_parquet("data-processed/beach_matches.parquet")
new_elo_matches <- read_parquet("data-processed/elo_matches.parquet")
classified_tournaments <- read_parquet(
  "data-processed/beach_tournaments_classified.parquet"
)

make_roster_key <- function(ids) {
  ids <- sort(unique(as.character(ids[!is.na(ids) & ids != ""])))
  paste(ids, collapse = "|")
}

legacy_match_rosters <- legacy |>
  dplyr::filter(!is.na(date), date <= as_of_date) |>
  dplyr::group_by(legacy_match_id = as.character(match_id)) |>
  dplyr::summarise(
    match_date = dplyr::first(date),
    gender = dplyr::first(as.character(gender)),
    athlete_count = dplyr::n_distinct(as.character(athlete)),
    roster_key = make_roster_key(athlete),
    legacy_tournament = if ("tourn" %in% names(legacy)) {
      dplyr::first(as.character(tourn))
    } else {
      NA_character_
    },
    legacy_tourn_cat = if ("tourn_cat" %in% names(legacy)) {
      dplyr::first(as.character(tourn_cat))
    } else {
      NA_character_
    },
    .groups = "drop"
  ) |>
  dplyr::mutate(
    fingerprint = paste(match_date, gender, roster_key, sep = "||")
  )

new_match_rosters <- new_history |>
  dplyr::filter(date <= as_of_date) |>
  dplyr::group_by(new_match_no = as.character(match_no)) |>
  dplyr::summarise(
    match_date = dplyr::first(date),
    gender = dplyr::first(as.character(gender)),
    athlete_count = dplyr::n_distinct(athlete_id),
    roster_key = make_roster_key(athlete_id),
    new_event_class = dplyr::first(as.character(event_class)),
    new_tournament = dplyr::first(as.character(tournament_name)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    fingerprint = paste(match_date, gender, roster_key, sep = "||")
  )

raw_tournament_gender <- classified_tournaments |>
  dplyr::transmute(
    no_tournament = as.integer(no),
    gender = as.character(gender)
  ) |>
  dplyr::distinct(no_tournament, .keep_all = TRUE)

raw_match_rosters <- raw_matches |>
  dplyr::left_join(raw_tournament_gender, by = "no_tournament") |>
  dplyr::filter(!is.na(local_date), as.Date(local_date) <= as_of_date) |>
  dplyr::transmute(
    raw_match_no = as.character(no),
    match_date = as.Date(local_date),
    gender,
    no_player_a1 = as.character(no_player_a1),
    no_player_a2 = as.character(no_player_a2),
    no_player_b1 = as.character(no_player_b1),
    no_player_b2 = as.character(no_player_b2)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    athlete_count = dplyr::n_distinct(
      c(no_player_a1, no_player_a2, no_player_b1, no_player_b2),
      na.rm = TRUE
    ),
    roster_key = make_roster_key(
      c(no_player_a1, no_player_a2, no_player_b1, no_player_b2)
    ),
    fingerprint = paste(match_date, gender, roster_key, sep = "||")
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    raw_match_no, match_date, gender, athlete_count,
    roster_key, fingerprint
  )

fingerprint_counts <- dplyr::bind_rows(
  legacy_match_rosters |>
    dplyr::count(fingerprint, name = "legacy_n"),
  new_match_rosters |>
    dplyr::count(fingerprint, name = "new_n"),
  raw_match_rosters |>
    dplyr::count(fingerprint, name = "raw_n")
) |>
  dplyr::group_by(fingerprint) |>
  dplyr::summarise(
    legacy_n = sum(dplyr::coalesce(legacy_n, 0L)),
    new_n = sum(dplyr::coalesce(new_n, 0L)),
    raw_n = sum(dplyr::coalesce(raw_n, 0L)),
    .groups = "drop"
  )

ambiguous_fingerprints <- fingerprint_counts |>
  dplyr::filter(legacy_n > 1L | new_n > 1L | raw_n > 1L)

readr::write_csv(
  ambiguous_fingerprints,
  "data-processed/legacy_match_fingerprint_ambiguities.csv"
)

legacy_valid <- legacy_match_rosters |>
  dplyr::filter(athlete_count == 4L) |>
  dplyr::left_join(fingerprint_counts, by = "fingerprint") |>
  dplyr::mutate(
    fingerprint_unambiguous =
      legacy_n == 1L &
      dplyr::coalesce(new_n, 0L) <= 1L &
      dplyr::coalesce(raw_n, 0L) <= 1L
  )

raw_fingerprints <- raw_match_rosters |>
  dplyr::filter(athlete_count == 4L) |>
  dplyr::semi_join(
    fingerprint_counts |>
      dplyr::filter(raw_n == 1L),
    by = "fingerprint"
  ) |>
  dplyr::select(fingerprint, raw_match_no) |>
  dplyr::mutate(in_raw_archive = TRUE)

new_fingerprints <- new_match_rosters |>
  dplyr::filter(athlete_count == 4L) |>
  dplyr::semi_join(
    fingerprint_counts |>
      dplyr::filter(new_n == 1L),
    by = "fingerprint"
  ) |>
  dplyr::select(fingerprint, new_match_no) |>
  dplyr::mutate(in_new_elo = TRUE)

legacy_overlap <- legacy_valid |>
  dplyr::filter(fingerprint_unambiguous) |>
  dplyr::left_join(raw_fingerprints, by = "fingerprint") |>
  dplyr::left_join(new_fingerprints, by = "fingerprint") |>
  dplyr::mutate(
    in_raw_archive = dplyr::coalesce(in_raw_archive, FALSE),
    in_new_elo = dplyr::coalesce(in_new_elo, FALSE),
    common_period = match_date >= as.Date("2008-01-01"),
    disposition = dplyr::case_when(
      in_new_elo ~ "shared_new_elo",
      in_raw_archive ~ "raw_archive_but_not_new_elo",
      TRUE ~ "missing_from_raw_archive"
    )
  )

readr::write_csv(
  legacy_overlap,
  "data-processed/legacy_match_disposition.csv"
)

legacy_overlap_summary <- dplyr::bind_rows(
  tibble::tibble(
    period = "all_legacy_through_as_of",
    metric = c(
      "legacy_unique_matches",
      "legacy_matches_with_four_athletes",
      "unambiguous_fingerprint_matches",
      "ambiguous_fingerprint_matches",
      "found_in_raw_archive",
      "included_in_new_elo",
      "raw_archive_but_not_new_elo",
      "missing_from_raw_archive"
    ),
    value = c(
      nrow(legacy_match_rosters),
      sum(legacy_match_rosters$athlete_count == 4L),
      nrow(legacy_overlap),
      sum(
        legacy_valid$athlete_count == 4L &
        !legacy_valid$fingerprint_unambiguous
      ),
      sum(legacy_overlap$in_raw_archive),
      sum(legacy_overlap$in_new_elo),
      sum(
        legacy_overlap$in_raw_archive & !legacy_overlap$in_new_elo
      ),
      sum(!legacy_overlap$in_raw_archive)
    )
  ),
  {
    x_all <- legacy_overlap |>
      dplyr::filter(common_period)
    legacy_all_common <- legacy_match_rosters |>
      dplyr::filter(match_date >= as.Date("2008-01-01"))
    legacy_valid_common <- legacy_valid |>
      dplyr::filter(match_date >= as.Date("2008-01-01"))

    tibble::tibble(
      period = "common_period_2008_onward",
      metric = c(
        "legacy_unique_matches",
        "legacy_matches_with_four_athletes",
        "unambiguous_fingerprint_matches",
        "ambiguous_fingerprint_matches",
        "found_in_raw_archive",
        "included_in_new_elo",
        "raw_archive_but_not_new_elo",
        "missing_from_raw_archive"
      ),
      value = c(
        nrow(legacy_all_common),
        sum(legacy_all_common$athlete_count == 4L),
        nrow(x_all),
        sum(
          legacy_valid_common$athlete_count == 4L &
          !legacy_valid_common$fingerprint_unambiguous
        ),
        sum(x_all$in_raw_archive),
        sum(x_all$in_new_elo),
        sum(x_all$in_raw_archive & !x_all$in_new_elo),
        sum(!x_all$in_raw_archive)
      )
    )
  }
)

readr::write_csv(
  legacy_overlap_summary,
  "data-processed/legacy_match_overlap_summary.csv"
)

shared_match_map <- legacy_overlap |>
  dplyr::filter(in_new_elo) |>
  dplyr::select(
    fingerprint,
    legacy_match_id,
    new_match_no,
    match_date,
    gender
  )

match_overlap_by_year <- dplyr::full_join(
  legacy_match_rosters |>
    dplyr::mutate(year = as.integer(format(match_date, "%Y"))) |>
    dplyr::count(gender, year, name = "legacy_matches"),
  new_match_rosters |>
    dplyr::mutate(year = as.integer(format(match_date, "%Y"))) |>
    dplyr::count(gender, year, name = "new_matches"),
  by = c("gender", "year")
) |>
  dplyr::left_join(
    shared_match_map |>
      dplyr::mutate(year = as.integer(format(match_date, "%Y"))) |>
      dplyr::count(gender, year, name = "shared_matches"),
    by = c("gender", "year")
  ) |>
  dplyr::mutate(
    legacy_matches = dplyr::coalesce(legacy_matches, 0L),
    new_matches = dplyr::coalesce(new_matches, 0L),
    shared_matches = dplyr::coalesce(shared_matches, 0L),
    legacy_only = legacy_matches - shared_matches,
    new_only = new_matches - shared_matches
  ) |>
  dplyr::arrange(gender, year)

readr::write_csv(
  match_overlap_by_year,
  "data-processed/legacy_match_overlap_by_year.csv"
)

legacy_shared_rows <- legacy |>
  dplyr::filter(!is.na(date), date <= as_of_date) |>
  dplyr::transmute(
    legacy_match_id = as.character(match_id),
    athlete_id = as.character(athlete),
    legacy_date = as.Date(date),
    legacy_result = as.numeric(result),
    legacy_elo_before = as.numeric(athlete_elo_before),
    legacy_elo_after = as.numeric(athlete_elo_after)
  ) |>
  dplyr::inner_join(
    shared_match_map |>
      dplyr::select(legacy_match_id, new_match_no),
    by = "legacy_match_id"
  )

new_shared_rows <- new_history |>
  dplyr::filter(date <= as_of_date) |>
  dplyr::transmute(
    new_match_no = as.character(match_no),
    athlete_id,
    display_name,
    new_date = date,
    new_result = as.numeric(actual_score),
    new_elo_before = as.numeric(athlete_elo_before),
    new_elo_after = as.numeric(athlete_elo_after),
    new_event_class = event_class,
    new_tournament = tournament_name
  )

shared_athlete_matches <- legacy_shared_rows |>
  dplyr::inner_join(
    new_shared_rows,
    by = c("new_match_no", "athlete_id")
  ) |>
  dplyr::mutate(
    date_agrees = legacy_date == new_date,
    result_agrees = legacy_result == new_result,
    elo_before_delta = new_elo_before - legacy_elo_before,
    elo_after_delta = new_elo_after - legacy_elo_after,
    abs_elo_after_delta = abs(elo_after_delta)
  )

readr::write_csv(
  shared_athlete_matches,
  "data-processed/legacy_shared_athlete_match_comparison.csv"
)

shared_row_summary <- tibble::tibble(
  metric = c(
    "shared_athlete_match_rows",
    "shared_unique_matches",
    "shared_unique_athletes",
    "date_agreement_rate",
    "result_agreement_rate",
    "mean_abs_elo_after_delta",
    "median_abs_elo_after_delta",
    "elo_after_correlation"
  ),
  value = c(
    nrow(shared_athlete_matches),
    dplyr::n_distinct(shared_athlete_matches$new_match_no),
    dplyr::n_distinct(shared_athlete_matches$athlete_id),
    mean(shared_athlete_matches$date_agrees, na.rm = TRUE),
    mean(shared_athlete_matches$result_agrees, na.rm = TRUE),
    mean(shared_athlete_matches$abs_elo_after_delta, na.rm = TRUE),
    stats::median(shared_athlete_matches$abs_elo_after_delta, na.rm = TRUE),
    stats::cor(
      shared_athlete_matches$new_elo_after,
      shared_athlete_matches$legacy_elo_after,
      use = "complete.obs"
    )
  )
)

readr::write_csv(
  shared_row_summary,
  "data-processed/legacy_shared_athlete_match_summary.csv"
)

message("\nCross-source match overlap (date + gender + four VIS athlete IDs):")
print(legacy_overlap_summary, n = Inf)

message("\nShared athlete-match row comparison:")
print(shared_row_summary, n = Inf)

message("\nMatch overlap by year/gender:")
print(match_overlap_by_year, n = Inf)

if ("tourn_cat" %in% names(legacy)) {
  message("\nLegacy tournament categories:")
  legacy |>
    dplyr::count(gender, tourn_cat, sort = TRUE) |>
    print(n = Inf)
}

message("\nStage 07 complete. Comparison files written to data-processed/.")
