#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
initialize_directories()
source_project_functions()

tournaments <- read_parquet(
  "data-processed/beach_tournaments_classified.parquet"
)

if (!"start_date_qualification" %in% names(tournaments)) {
  tournaments$start_date_qualification <- NA_character_
}
if (!"start_date_main_draw" %in% names(tournaments)) {
  tournaments$start_date_main_draw <- NA_character_
}

tournaments <- tournaments |>
  dplyr::filter(!is.na(no), no > 0) |>
  dplyr::mutate(
    archive_start_date = dplyr::coalesce(
      as.Date(start_date_qualification),
      as.Date(start_date_main_draw)
    )
  ) |>
  dplyr::arrange(archive_start_date, no)

cache_dir <- "data-raw/matches/by-tournament"
pause <- request_pause_seconds()
refresh <- force_refresh()

manifest <- vector("list", nrow(tournaments))

for (i in seq_len(nrow(tournaments))) {
  tournament_no <- as.integer(tournaments$no[[i]])
  cache_path <- file.path(cache_dir, sprintf("%08d.rds", tournament_no))

  if (!refresh && file.exists(cache_path)) {
    x <- readRDS(cache_path)
    manifest[[i]] <- tibble::tibble(
      tournament_no = tournament_no,
      status = "cached",
      rows = nrow(x),
      error = NA_character_
    )
  } else {
    pulled <- tryCatch(
      {
        x <- vis_get_beach_matches(tournament_no)
        saveRDS(x, cache_path)
        list(data = x, error = NULL)
      },
      error = function(e) list(data = NULL, error = conditionMessage(e))
    )

    if (is.null(pulled$error)) {
      manifest[[i]] <- tibble::tibble(
        tournament_no = tournament_no,
        status = "downloaded",
        rows = nrow(pulled$data),
        error = NA_character_
      )
    } else {
      manifest[[i]] <- tibble::tibble(
        tournament_no = tournament_no,
        status = "error",
        rows = NA_integer_,
        error = pulled$error
      )
    }

    if (pause > 0) Sys.sleep(pause)
  }

  if (i %% 100L == 0L || i == nrow(tournaments)) {
    message("Processed ", i, " / ", nrow(tournaments), " tournaments.")
  }
}

manifest <- dplyr::bind_rows(manifest)
readr::write_csv(manifest, "data-processed/match_pull_manifest.csv")

cache_files <- list.files(
  cache_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

if (length(cache_files) == 0L) {
  stop(
    "No tournament match caches were created. Check data-processed/match_pull_manifest.csv.",
    call. = FALSE
  )
}

matches <- purrr::map_dfr(cache_files, readRDS)

if (!"no" %in% names(matches)) {
  stop("Cached VIS match data do not contain the canonical match field 'No'.", call. = FALSE)
}

matches <- matches |>
  dplyr::distinct(no, .keep_all = TRUE)

write_parquet(matches, "data-processed/beach_matches.parquet")

errors <- manifest |> dplyr::filter(status == "error")
message(
  "Match archive stage complete: ",
  nrow(matches), " unique matches; ",
  nrow(errors), " tournament requests currently failed."
)
