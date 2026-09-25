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

# VIS omits optional attributes when they are not populated. Mirror the main
# Elo preparation stage so this QA script is robust to those schema variants.
raw_matches <- add_missing_columns(
  raw_matches,
  list(
    deleted_dt = NA_character_,
    result_type = NA_integer_,
    round_name = NA_character_,
    round_code = NA_character_,
    match_points_a = NA_real_,
    match_points_b = NA_real_,
    no_team_a = NA_real_,
    no_team_b = NA_real_
  )
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

message("Building cross-source four-athlete match fingerprints...")

# The raw archive is much larger than the legacy/new Elo histories. Build its
# four-player fingerprint with a vectorized four-value sorting network rather
# than rowwise() so this QA stage stays fast.
raw_match_rosters <- raw_matches |>
  dplyr::left_join(raw_tournament_gender, by = "no_tournament") |>
  dplyr::filter(!is.na(local_date), as.Date(local_date) <= as_of_date) |>
  dplyr::transmute(
    raw_match_no = as.character(no),
    match_date = as.Date(local_date),
    gender,
    a = suppressWarnings(as.numeric(no_player_a1)),
    b = suppressWarnings(as.numeric(no_player_a2)),
    c = suppressWarnings(as.numeric(no_player_b1)),
    d = suppressWarnings(as.numeric(no_player_b2))
  ) |>
  dplyr::filter(
    !is.na(gender),
    !is.na(a), !is.na(b), !is.na(c), !is.na(d),
    a > 0, b > 0, c > 0, d > 0
  ) |>
  dplyr::mutate(
    p1 = pmin(a, b),
    p2 = pmax(a, b),
    p3 = pmin(c, d),
    p4 = pmax(c, d),
    s1 = pmin(p1, p3),
    hi13 = pmax(p1, p3),
    lo24 = pmin(p2, p4),
    s4 = pmax(p2, p4),
    s2 = pmin(hi13, lo24),
    s3 = pmax(hi13, lo24),
    athlete_count = dplyr::if_else(
      s1 < s2 & s2 < s3 & s3 < s4,
      4L,
      3L
    ),
    roster_key = paste(
      format(s1, scientific = FALSE, trim = TRUE),
      format(s2, scientific = FALSE, trim = TRUE),
      format(s3, scientific = FALSE, trim = TRUE),
      format(s4, scientific = FALSE, trim = TRUE),
      sep = "|"
    ),
    fingerprint = paste(match_date, gender, roster_key, sep = "||")
  ) |>
  dplyr::filter(athlete_count == 4L) |>
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

# -------------------------------------------------------------------------
# Why are legacy matches excluded from the rebuilt Elo universe?
# -------------------------------------------------------------------------

config <- read_elo_config()
tournament_audit <- build_elo_tournament_audit(
  classified_tournaments,
  config
)

include_match_nos <- as.character(unlist(config$selection$include_match_nos))
exclude_match_nos <- as.character(unlist(config$selection$exclude_match_nos))
exclude_tournament_nos <- as.integer(
  unlist(config$selection$exclude_tournament_nos)
)
include_result_types <- as.integer(
  unlist(config$selection$include_result_types)
)

legacy_only_raw <- legacy_overlap |>
  dplyr::filter(
    common_period,
    in_raw_archive,
    !in_new_elo
  ) |>
  dplyr::left_join(
    raw_matches |>
      dplyr::transmute(
        raw_match_no = as.character(no),
        no_tournament = as.integer(no_tournament),
        local_date = as.Date(local_date),
        result_type_code = suppressWarnings(as.integer(result_type)),
        match_points_a = suppressWarnings(as.numeric(match_points_a)),
        match_points_b = suppressWarnings(as.numeric(match_points_b)),
        no_team_a = suppressWarnings(as.numeric(no_team_a)),
        no_team_b = suppressWarnings(as.numeric(no_team_b)),
        deleted_dt = as.character(deleted_dt),
        round_name = as.character(round_name),
        round_code = as.character(round_code)
      ),
    by = "raw_match_no"
  ) |>
  dplyr::left_join(
    tournament_audit |>
      dplyr::transmute(
        no_tournament = as.integer(tournament_no),
        event_class,
        tournament_name_current = tournament_name,
        elo_tournament_status = elo_selection_status,
        elo_tournament_reason = elo_selection_reason
      ),
    by = "no_tournament"
  ) |>
  dplyr::mutate(
    is_qualification = stringr::str_detect(
      stringr::str_to_lower(
        paste(
          dplyr::coalesce(round_name, ""),
          dplyr::coalesce(round_code, "")
        )
      ),
      "qual"
    ),
    exclusion_reason = dplyr::case_when(
      raw_match_no %in% exclude_match_nos ~
        "manual match exclusion",
      no_tournament %in% exclude_tournament_nos ~
        "manual tournament exclusion",
      !(elo_tournament_status == "include") &
        !(raw_match_no %in% include_match_nos) ~
        paste0(
          "tournament not selected: ",
          dplyr::coalesce(elo_tournament_reason, "unknown")
        ),
      length(include_result_types) > 0L &
        !result_type_code %in% include_result_types ~
        paste0(
          "result type excluded: ",
          dplyr::coalesce(as.character(result_type_code), "NA")
        ),
      is.na(match_points_a) | is.na(match_points_b) |
        match_points_a == match_points_b ~
        "non-decisive or missing match points",
      is.na(no_team_a) | is.na(no_team_b) |
        no_team_a <= 0 | no_team_b <= 0 ~
        "invalid team IDs",
      !is.na(deleted_dt) & deleted_dt != "" ~
        "deleted VIS match",
      !isTRUE(config$selection$include_qualification) &
        is_qualification ~
        "qualification excluded",
      TRUE ~
        "other selector difference"
    )
  )

readr::write_csv(
  legacy_only_raw,
  "data-processed/legacy_matches_excluded_from_new_elo.csv"
)

legacy_exclusion_summary <- legacy_only_raw |>
  dplyr::count(exclusion_reason, sort = TRUE, name = "matches")

readr::write_csv(
  legacy_exclusion_summary,
  "data-processed/legacy_match_exclusion_reasons.csv"
)

legacy_missing_raw <- legacy_overlap |>
  dplyr::filter(
    common_period,
    !in_raw_archive
  ) |>
  dplyr::mutate(
    year = as.integer(format(match_date, "%Y"))
  ) |>
  dplyr::arrange(match_date, gender, legacy_tournament)

readr::write_csv(
  legacy_missing_raw,
  "data-processed/legacy_matches_missing_from_raw_archive.csv"
)

legacy_missing_raw_summary <- legacy_missing_raw |>
  dplyr::count(
    gender,
    year,
    legacy_tournament,
    sort = TRUE,
    name = "matches"
  )

readr::write_csv(
  legacy_missing_raw_summary,
  "data-processed/legacy_missing_raw_summary.csv"
)

message("\nWhy legacy 2008+ matches are excluded from the new Elo universe:")
print(legacy_exclusion_summary, n = Inf)

olympic_qualification_exclusions <- legacy_only_raw |>
  dplyr::filter(event_class == "Olympic Qualification") |>
  dplyr::mutate(year = as.integer(format(match_date, "%Y"))) |>
  dplyr::count(
    gender,
    year,
    no_tournament,
    tournament_name_current,
    sort = TRUE,
    name = "matches"
  )

readr::write_csv(
  olympic_qualification_exclusions,
  "data-processed/legacy_olympic_qualification_exclusions.csv"
)

if (nrow(olympic_qualification_exclusions) > 0L) {
  message("\nLegacy Olympic Qualification matches excluded by current profile:")
  print(olympic_qualification_exclusions, n = Inf, width = Inf)
}

message("\nLargest legacy 2008+ raw-archive gaps:")
print(
  legacy_missing_raw_summary |>
    dplyr::slice_head(n = 25L),
  n = 25,
  width = Inf
)

# -------------------------------------------------------------------------
# Legacy Elo formula parity.
#
# This test is path-independent: use the legacy pre-match ratings themselves,
# reconstruct opponent strength and the K=30 update, then compare the
# reconstructed post-match rating with the stored legacy post-match rating.
# -------------------------------------------------------------------------

if (!"partner" %in% names(legacy)) {
  stop(
    "Legacy formula parity requires the legacy 'partner' column.",
    call. = FALSE
  )
}

legacy_formula_base <- legacy |>
  dplyr::filter(!is.na(match_id), !is.na(athlete), !is.na(partner)) |>
  dplyr::transmute(
    legacy_match_id = as.character(match_id),
    athlete_id = as.character(athlete),
    partner_id = as.character(partner),
    match_date = as.Date(date),
    gender = as.character(gender),
    tournament = if ("tourn" %in% names(legacy)) {
      as.character(tourn)
    } else {
      NA_character_
    },
    tournament_category = if ("tourn_cat" %in% names(legacy)) {
      as.character(tourn_cat)
    } else {
      NA_character_
    },
    actual_score = suppressWarnings(as.numeric(result)),
    elo_before = as.numeric(athlete_elo_before),
    elo_after = as.numeric(athlete_elo_after),
    k_factor = if ("k_factor" %in% names(legacy)) {
      as.numeric(k_factor)
    } else {
      30
    }
  )

legacy_opponent_rows <- legacy_formula_base |>
  dplyr::select(
    legacy_match_id,
    opponent_id = athlete_id,
    opponent_elo_before = elo_before
  )

legacy_formula_check <- legacy_formula_base |>
  dplyr::inner_join(
    legacy_opponent_rows,
    by = "legacy_match_id",
    relationship = "many-to-many"
  ) |>
  dplyr::filter(
    opponent_id != athlete_id,
    opponent_id != partner_id
  ) |>
  dplyr::group_by(
    legacy_match_id,
    athlete_id,
    partner_id,
    match_date,
    gender,
    tournament,
    tournament_category,
    actual_score,
    elo_before,
    elo_after,
    k_factor
  ) |>
  dplyr::summarise(
    opponent_count = dplyr::n_distinct(opponent_id),
    opponent_team_mean_elo = mean(opponent_elo_before),
    .groups = "drop"
  ) |>
  dplyr::filter(
    opponent_count == 2L,
    actual_score %in% c(0, 1),
    !is.na(elo_before),
    !is.na(elo_after)
  ) |>
  dplyr::mutate(
    reconstructed_expected = 1 / (
      1 + 10 ^ ((opponent_team_mean_elo - elo_before) / 400)
    ),
    reconstructed_change =
      k_factor * (actual_score - reconstructed_expected),
    reconstructed_after = elo_before + reconstructed_change,
    formula_error = elo_after - reconstructed_after,
    abs_formula_error = abs(formula_error)
  )

readr::write_csv(
  legacy_formula_check,
  "data-processed/legacy_elo_formula_parity.csv"
)

legacy_formula_outliers <- legacy_formula_check |>
  dplyr::filter(abs_formula_error > 1e-6) |>
  dplyr::arrange(dplyr::desc(abs_formula_error))

readr::write_csv(
  legacy_formula_outliers,
  "data-processed/legacy_elo_formula_outliers.csv"
)

legacy_formula_outlier_summary <- legacy_formula_outliers |>
  dplyr::mutate(
    year = as.integer(format(match_date, "%Y"))
  ) |>
  dplyr::count(
    gender,
    year,
    tournament_category,
    tournament,
    sort = TRUE,
    name = "athlete_match_rows"
  )

readr::write_csv(
  legacy_formula_outlier_summary,
  "data-processed/legacy_elo_formula_outlier_summary.csv"
)

legacy_formula_summary <- tibble::tibble(
  metric = c(
    "eligible_athlete_match_rows",
    "formula_outlier_rows_gt_1e_6",
    "formula_outlier_matches_gt_1e_6",
    "mean_abs_formula_error",
    "median_abs_formula_error",
    "max_abs_formula_error",
    "share_within_1e_8",
    "share_within_1e_6",
    "share_within_1e_4"
  ),
  value = c(
    nrow(legacy_formula_check),
    nrow(legacy_formula_outliers),
    dplyr::n_distinct(legacy_formula_outliers$legacy_match_id),
    mean(legacy_formula_check$abs_formula_error, na.rm = TRUE),
    stats::median(
      legacy_formula_check$abs_formula_error,
      na.rm = TRUE
    ),
    max(legacy_formula_check$abs_formula_error, na.rm = TRUE),
    mean(legacy_formula_check$abs_formula_error <= 1e-8),
    mean(legacy_formula_check$abs_formula_error <= 1e-6),
    mean(legacy_formula_check$abs_formula_error <= 1e-4)
  )
)

readr::write_csv(
  legacy_formula_summary,
  "data-processed/legacy_elo_formula_parity_summary.csv"
)

message("\nLegacy K=30 Elo formula parity:")
print(legacy_formula_summary, n = Inf)

message("\nLargest legacy formula-parity outliers:")
print(
  legacy_formula_outliers |>
    dplyr::select(
      legacy_match_id,
      match_date,
      gender,
      tournament_category,
      tournament,
      athlete_id,
      actual_score,
      elo_before,
      opponent_team_mean_elo,
      k_factor,
      elo_after,
      reconstructed_after,
      formula_error,
      abs_formula_error
    ) |>
    dplyr::slice_head(n = 25L),
  n = 25,
  width = Inf
)

if ("tourn_cat" %in% names(legacy)) {
  message("\nLegacy tournament categories:")
  legacy |>
    dplyr::count(gender, tourn_cat, sort = TRUE) |>
    print(n = Inf)
}

message("\nStage 07 complete. Comparison files written to data-processed/.")
