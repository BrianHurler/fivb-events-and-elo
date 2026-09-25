as_date_or_null <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || identical(x, "")) return(NULL)
  as.Date(x)
}

add_missing_columns <- function(data, defaults) {
  for (nm in names(defaults)) {
    if (!nm %in% names(data)) {
      data[[nm]] <- rep(defaults[[nm]], nrow(data))
    }
  }
  data
}

decode_beach_match_result_type <- function(x) {
  x <- suppressWarnings(as.integer(x))

  dplyr::case_when(
    x == 0L ~ "Normal",
    x == 1L ~ "ForfeitA",
    x == 2L ~ "ForfeitB",
    x == 3L ~ "ForfeitBoth",
    x == 4L ~ "InjuryA",
    x == 5L ~ "InjuryB",
    x == 6L ~ "InjuryBoth",
    x == 7L ~ "OutA",
    x == 8L ~ "OutB",
    x == 9L ~ "OutBoth",
    x == 10L ~ "DisqualifiedA",
    x == 11L ~ "DisqualifiedB",
    x == 12L ~ "DisqualifiedBoth",
    is.na(x) ~ "Missing",
    TRUE ~ paste0("Unknown_", x)
  )
}


build_elo_tournament_audit <- function(tournaments, config) {
  required <- c("no", "gender", "event_class")
  missing <- setdiff(required, names(tournaments))
  if (length(missing) > 0L) {
    stop(
      "Tournament data missing required fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  tournaments <- add_missing_columns(
    tournaments,
    list(
      season = NA_character_,
      name = NA_character_,
      title = NA_character_,
      vis_type_raw = NA_character_,
      vis_type_name = NA_character_,
      classification_source = NA_character_,
      classification_notes = NA_character_,
      country_code = NA_character_,
      organizer_type = NA_character_,
      organizer_code = NA_character_,
      start_date_qualification = NA_character_,
      end_date_qualification = NA_character_,
      start_date_main_draw = NA_character_,
      end_date_main_draw = NA_character_
    )
  )

  config_chr <- function(x) {
    if (is.null(x) || length(x) == 0L) return(character())
    as.character(unlist(x))
  }

  include_classes <- config_chr(config$selection$include_event_classes)
  continental_classes <- config_chr(
    config$selection$include_continental_event_classes
  )
  continental_organizers <- toupper(config_chr(
    config$selection$include_continental_organizer_codes
  ))
  continental_excluded_organizers <- toupper(config_chr(
    config$selection$exclude_continental_organizer_codes
  ))
  include_nos <- as.integer(config_chr(config$selection$include_tournament_nos))
  exclude_nos <- as.integer(config_chr(config$selection$exclude_tournament_nos))
  allowed_genders <- config_chr(config$selection$genders)

  start_limit <- as_date_or_null(config$selection$start_date)
  end_limit <- as_date_or_null(config$selection$end_date)

  out <- tournaments |>
    dplyr::mutate(
      tournament_no = as.integer(no),
      tournament_name = dplyr::coalesce(
        as.character(title),
        as.character(name)
      ),
      start_date = dplyr::coalesce(
        as.Date(start_date_qualification),
        as.Date(start_date_main_draw)
      ),
      end_date = dplyr::coalesce(
        as.Date(end_date_main_draw),
        as.Date(end_date_qualification)
      ),
      event_year = suppressWarnings(as.integer(format(start_date, "%Y"))),
      organizer_code_normalized = toupper(trimws(as.character(organizer_code))),
      tournament_text = toupper(paste(
        dplyr::coalesce(as.character(name), ""),
        dplyr::coalesce(as.character(title), "")
      )),
      continental_confederation = dplyr::case_when(
        organizer_code_normalized %in% c("AVC", "NORCECA", "CSV", "CEV", "CAVB") ~
          organizer_code_normalized,
        stringr::str_detect(tournament_text, "\\bNORCECA\\b") ~ "NORCECA",
        stringr::str_detect(tournament_text, "\\bAVC\\b") ~ "AVC",
        stringr::str_detect(tournament_text, "\\bCSV\\b") ~ "CSV",
        stringr::str_detect(tournament_text, "\\bCEV\\b") ~ "CEV",
        stringr::str_detect(tournament_text, "\\bCAVB\\b|\\bAFRICAN\\b") ~ "CAVB",
        TRUE ~ NA_character_
      ),
      youth_title_marker = stringr::str_detect(
        tournament_text,
        "(^|[^A-Z0-9])U\\s*-?\\s*(15|16|17|18|19|20|21|22|23)([^0-9]|$)|\\bUNDER\\s*-?\\s*(15|16|17|18|19|20|21|22|23)\\b"
      ),
      youth_class_marker = stringr::str_detect(
        dplyr::coalesce(as.character(event_class), ""),
        "U(15|16|17|18|19|20|21|22|23)|Youth|Junior"
      ),
      youth_excluded = youth_title_marker | youth_class_marker,
      gender_allowed = gender %in% allowed_genders,
      class_selected = !is.na(event_class) & event_class %in% include_classes,
      continental_candidate =
        !is.na(event_class) & event_class %in% continental_classes,
      continental_selected =
        continental_candidate &
        continental_confederation %in% continental_organizers,
      continental_explicit_exclude =
        continental_candidate &
        continental_confederation %in% continental_excluded_organizers,
      continental_unknown_organizer =
        continental_candidate &
        !continental_selected &
        !continental_explicit_exclude,
      manual_include = tournament_no %in% include_nos,
      manual_exclude = tournament_no %in% exclude_nos,
      unresolved_classification =
        is.na(event_class) |
        classification_source == "unclassified",
      within_start_date = if (is.null(start_limit)) {
        TRUE
      } else {
        !is.na(start_date) & start_date >= start_limit
      },
      within_end_date = if (is.null(end_limit)) {
        TRUE
      } else {
        !is.na(start_date) & start_date <= end_limit
      },
      within_date_window = within_start_date & within_end_date,
      elo_selection_status = dplyr::case_when(
        manual_exclude ~ "exclude",
        !gender_allowed ~ "exclude",
        !within_date_window ~ "exclude",
        youth_excluded ~ "exclude",
        manual_include ~ "include",
        class_selected ~ "include",
        continental_selected ~ "include",
        continental_explicit_exclude ~ "exclude",
        continental_unknown_organizer ~ "review",
        unresolved_classification ~ "review",
        TRUE ~ "exclude"
      ),
      elo_selection_reason = dplyr::case_when(
        manual_exclude ~ "manual tournament exclusion",
        !gender_allowed ~ "gender not selected by Elo profile",
        !within_date_window ~ "outside Elo profile date window",
        youth_excluded ~ "youth age-group marker in tournament title/class",
        manual_include ~ "manual tournament inclusion",
        class_selected ~ paste0("selected FIVB event class: ", event_class),
        continental_selected ~ paste0(
          "selected continental event: ",
          event_class,
          " / ",
          continental_confederation
        ),
        continental_explicit_exclude ~ paste0(
          "continental organizer explicitly excluded: ",
          continental_confederation
        ),
        continental_unknown_organizer ~ paste0(
          "continental event with unrecognized organizer: ",
          dplyr::coalesce(organizer_code_normalized, "NA")
        ),
        unresolved_classification ~ "unresolved tournament classification",
        TRUE ~ paste0(
          "event class not selected: ",
          dplyr::coalesce(event_class, "NA")
        )
      ),
      elo_profile = config$profile_name
    ) |>
    dplyr::select(
      elo_profile,
      elo_selection_status,
      elo_selection_reason,
      tournament_no,
      event_year,
      start_date,
      end_date,
      season,
      gender,
      event_class,
      organizer_type,
      organizer_code,
      organizer_code_normalized,
      continental_confederation,
      youth_title_marker,
      youth_class_marker,
      youth_excluded,
      vis_type_raw,
      vis_type_name,
      classification_source,
      classification_notes,
      country_code,
      name,
      title,
      tournament_name,
      dplyr::everything(),
      -no
    ) |>
    dplyr::arrange(start_date, gender, tournament_no)

  out
}

prepare_elo_matches <- function(matches, tournaments, config) {
  required_match <- c(
    "no", "no_tournament", "local_date",
    "no_team_a", "no_team_b",
    "no_player_a1", "no_player_a2", "no_player_b1", "no_player_b2",
    "match_points_a", "match_points_b"
  )
  missing_match <- setdiff(required_match, names(matches))
  if (length(missing_match) > 0L) {
    stop("Match data missing required fields: ", paste(missing_match, collapse = ", "))
  }

  matches <- add_missing_columns(
    matches,
    list(
      deleted_dt = NA_character_,
      round_name = NA_character_,
      round_code = NA_character_,
      local_time = NA_character_,
      team_a_federation_code = NA_character_,
      team_b_federation_code = NA_character_
    )
  )

  required_tournament <- c("no", "gender", "event_class")
  missing_tournament <- setdiff(required_tournament, names(tournaments))
  if (length(missing_tournament) > 0L) {
    stop(
      "Tournament data missing required fields: ",
      paste(missing_tournament, collapse = ", ")
    )
  }

  tournaments <- add_missing_columns(
    tournaments,
    list(
      name = NA_character_,
      title = NA_character_,
      start_date_qualification = NA_character_,
      start_date_main_draw = NA_character_
    )
  )

  tournament_audit <- build_elo_tournament_audit(tournaments, config)

  tournament_context <- tournament_audit |>
    dplyr::transmute(
      no_tournament = tournament_no,
      gender = as.character(gender),
      event_class = as.character(event_class),
      tournament_name_classified = tournament_name,
      tournament_start = start_date,
      elo_tournament_status = elo_selection_status,
      elo_tournament_reason = elo_selection_reason
    )

  out <- matches |>
    dplyr::left_join(tournament_context, by = "no_tournament") |>
    dplyr::mutate(
      local_date = as.Date(local_date),
      decisive_result = !is.na(match_points_a) &
        !is.na(match_points_b) &
        match_points_a != match_points_b,
      valid_player_ids = no_player_a1 > 0 &
        no_player_a2 > 0 &
        no_player_b1 > 0 &
        no_player_b2 > 0,
      valid_team_ids = no_team_a > 0 & no_team_b > 0,
      not_deleted = is.na(deleted_dt) | deleted_dt == "",
      is_qualification = stringr::str_detect(
        stringr::str_to_lower(
          paste(
            dplyr::coalesce(as.character(round_name), ""),
            dplyr::coalesce(as.character(round_code), "")
          )
        ),
        "qual"
      )
    ) |>
    dplyr::filter(
      decisive_result,
      valid_player_ids,
      valid_team_ids,
      not_deleted,
      gender %in% unlist(config$selection$genders)
    )

  include_match_nos <- as.integer(unlist(config$selection$include_match_nos))
  exclude_match_nos <- as.integer(unlist(config$selection$exclude_match_nos))
  exclude_nos <- as.integer(unlist(config$selection$exclude_tournament_nos))

  out <- out |>
    dplyr::filter(
      elo_tournament_status == "include" |
        no %in% include_match_nos
    )

  if (length(exclude_nos) > 0L) {
    out <- out |>
      dplyr::filter(!no_tournament %in% exclude_nos)
  }

  if (length(exclude_match_nos) > 0L) {
    out <- out |>
      dplyr::filter(!no %in% exclude_match_nos)
  }

  if (!isTRUE(config$selection$include_qualification)) {
    out <- out |>
      dplyr::filter(!is_qualification)
  }

  start_date <- as_date_or_null(config$selection$start_date)
  end_date <- as_date_or_null(config$selection$end_date)

  if (!is.null(start_date)) out <- out |> dplyr::filter(local_date >= start_date)
  if (!is.null(end_date)) out <- out |> dplyr::filter(local_date <= end_date)

  out |>
    dplyr::mutate(
      elo_profile = config$profile_name
    ) |>
    dplyr::arrange(
      local_date,
      dplyr::coalesce(as.character(local_time), "00:00:00"),
      no
    ) |>
    dplyr::distinct(no, .keep_all = TRUE)
}

elo_expected <- function(rating, opponent_rating, scale = 400) {
  1 / (1 + 10 ^ ((opponent_rating - rating) / scale))
}

calculate_athlete_elo <- function(matches, initial_rating = 1500,
                                  k_factor = 30, scale = 400) {
  if (nrow(matches) == 0L) return(tibble::tibble())

  matches <- matches |>
    dplyr::arrange(
      local_date,
      dplyr::coalesce(as.character(local_time), "00:00:00"),
      no
    )

  ratings <- new.env(parent = emptyenv(), hash = TRUE)

  get_rating <- function(player_id) {
    key <- as.character(player_id)
    if (!exists(key, envir = ratings, inherits = FALSE)) {
      assign(key, initial_rating, envir = ratings)
    }
    get(key, envir = ratings, inherits = FALSE)
  }

  set_rating <- function(player_id, value) {
    assign(as.character(player_id), value, envir = ratings)
  }

  result <- vector("list", nrow(matches))

  for (i in seq_len(nrow(matches))) {
    row <- matches[i, , drop = FALSE]

    ids <- c(
      row$no_player_a1[[1]],
      row$no_player_a2[[1]],
      row$no_player_b1[[1]],
      row$no_player_b2[[1]]
    )

    if (length(unique(ids)) != 4L) {
      stop("Match ", row$no[[1]], " does not contain four unique player IDs.")
    }

    pre <- vapply(ids, get_rating, numeric(1))
    names(pre) <- c("a1", "a2", "b1", "b2")

    opp_a <- mean(c(pre[["b1"]], pre[["b2"]]))
    opp_b <- mean(c(pre[["a1"]], pre[["a2"]]))

    expected <- c(
      a1 = elo_expected(pre[["a1"]], opp_a, scale),
      a2 = elo_expected(pre[["a2"]], opp_a, scale),
      b1 = elo_expected(pre[["b1"]], opp_b, scale),
      b2 = elo_expected(pre[["b2"]], opp_b, scale)
    )

    actual_a <- as.numeric(row$match_points_a[[1]] > row$match_points_b[[1]])
    actual <- c(a1 = actual_a, a2 = actual_a, b1 = 1 - actual_a, b2 = 1 - actual_a)
    delta <- k_factor * (actual - expected)
    post <- pre + delta

    purrr::walk2(ids, post, set_rating)

    team_a_fed <- if ("team_a_federation_code" %in% names(row)) {
      as.character(row$team_a_federation_code[[1]])
    } else NA_character_
    team_b_fed <- if ("team_b_federation_code" %in% names(row)) {
      as.character(row$team_b_federation_code[[1]])
    } else NA_character_

    tournament_name <- if ("tournament_name_classified" %in% names(row)) {
      as.character(row$tournament_name_classified[[1]])
    } else if ("tournament_name" %in% names(row)) {
      as.character(row$tournament_name[[1]])
    } else {
      NA_character_
    }

    result[[i]] <- tibble::tibble(
      match_no = rep(row$no[[1]], 4),
      tournament_no = rep(row$no_tournament[[1]], 4),
      date = rep(as.Date(row$local_date[[1]]), 4),
      local_time = rep(
        if ("local_time" %in% names(row)) as.character(row$local_time[[1]]) else NA_character_,
        4
      ),
      gender = rep(as.character(row$gender[[1]]), 4),
      event_class = rep(as.character(row$event_class[[1]]), 4),
      tournament_name = rep(tournament_name, 4),
      elo_profile = rep(as.character(row$elo_profile[[1]]), 4),
      athlete_id = as.character(ids),
      partner_id = as.character(c(ids[2], ids[1], ids[4], ids[3])),
      opponent1_id = as.character(c(ids[3], ids[3], ids[1], ids[1])),
      opponent2_id = as.character(c(ids[4], ids[4], ids[2], ids[2])),
      team_id = as.character(c(
        row$no_team_a[[1]], row$no_team_a[[1]],
        row$no_team_b[[1]], row$no_team_b[[1]]
      )),
      opponent_team_id = as.character(c(
        row$no_team_b[[1]], row$no_team_b[[1]],
        row$no_team_a[[1]], row$no_team_a[[1]]
      )),
      federation = c(team_a_fed, team_a_fed, team_b_fed, team_b_fed),
      actual_score = unname(actual),
      expected_score = unname(expected),
      athlete_elo_before = unname(pre),
      athlete_elo_change = unname(delta),
      athlete_elo_after = unname(post),
      k_factor = rep(k_factor, 4)
    )
  }

  dplyr::bind_rows(result) |>
    dplyr::group_by(athlete_id) |>
    dplyr::arrange(date, local_time, match_no, .by_group = TRUE) |>
    dplyr::mutate(career_match_number = dplyr::row_number()) |>
    dplyr::ungroup()
}

build_current_elo <- function(history) {
  if (nrow(history) == 0L) return(tibble::tibble())

  history |>
    dplyr::arrange(date, local_time, match_no) |>
    dplyr::group_by(athlete_id) |>
    dplyr::slice_tail(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::select(
      athlete_id, gender, federation, date, match_no,
      career_match_number, athlete_elo_after
    ) |>
    dplyr::arrange(gender, dplyr::desc(athlete_elo_after))
}
