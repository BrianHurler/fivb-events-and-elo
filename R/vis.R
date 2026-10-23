VIS_API_URL <- "https://www.fivb.org/Vis2009/XmlRequest.asmx"

snake_case_names <- function(x) {
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  tolower(gsub("^_|_$", "", x))
}

vis_parse_nodes <- function(xml_text, node_name) {
  doc <- xml2::read_xml(xml_text)

  bad_nodes <- xml2::xml_find_all(
    doc,
    ".//*[starts-with(local-name(), 'Bad') or local-name()='Error' or local-name()='Unknown']"
  )
  if (length(bad_nodes) > 0L) {
    stop(
      "VIS returned an error: ",
      paste(xml2::xml_text(bad_nodes), collapse = " | "),
      call. = FALSE
    )
  }

  nodes <- xml2::xml_find_all(doc, paste0(".//", node_name))
  if (length(nodes) == 0L) {
    return(tibble::tibble())
  }

  out <- purrr::map_dfr(nodes, function(node) {
    attrs <- as.list(xml2::xml_attrs(node))
    tibble::as_tibble(attrs)
  })

  names(out) <- snake_case_names(names(out))
  suppressMessages(
    readr::type_convert(
      out,
      na = c("", "NULL", "null"),
      trim_ws = TRUE
    )
  )
}

vis_post <- function(request_xml, api_url = VIS_API_URL) {
  response <- httr2::request(api_url) |>
    httr2::req_body_form(Request = request_xml) |>
    httr2::req_headers(
      Accept = "application/xml",
      `Accept-Encoding` = "gzip, deflate",
      `User-Agent` = "fivb-events-and-elo (USAV Beach performance analysis)"
    ) |>
    httr2::req_retry(max_tries = 5) |>
    httr2::req_timeout(seconds = 120) |>
    httr2::req_perform()

  httr2::resp_body_string(response)
}

vis_request_list <- function(type, fields, filter_xml = NULL, node_name,
                             api_url = VIS_API_URL) {
  request_xml <- paste0(
    '<Request Type="', type, '" Fields="', paste(fields, collapse = " "), '">',
    if (is.null(filter_xml)) "" else filter_xml,
    "</Request>"
  )

  vis_parse_nodes(
    vis_post(request_xml, api_url = api_url),
    node_name = node_name
  )
}

vis_get_events <- function(api_url = VIS_API_URL) {
  fields <- c(
    "No", "Code", "Name", "StartDate", "EndDate", "Type", "CountryCode",
    "HasBeachTournament", "HasMenTournament", "HasWomenTournament",
    "NoParentEvent", "OrganizerType", "OrganizerCode", "DeletedDT", "Version"
  )

  vis_request_list(
    type = "GetEventList",
    fields = fields,
    node_name = "Event",
    api_url = api_url
  )
}

vis_get_beach_tournaments <- function(api_url = VIS_API_URL) {
  fields <- c(
    "No", "NoEvent", "Code", "Name", "Title", "Gender", "Type", "Season",
    "CountryCode", "OrganizerType", "OrganizerCode", "Status", "IsVisManaged",
    "StartDateQualification", "EndDateQualification",
    "StartDateMainDraw", "EndDateMainDraw",
    "NbTeamsQualification", "NbTeamsMainDraw", "NbTeamsFromQualification",
    "DeletedDT", "LastChangeDT", "Version"
  )

  vis_request_list(
    type = "GetBeachTournamentList",
    fields = fields,
    node_name = "BeachTournament",
    api_url = api_url
  )
}

vis_get_beach_matches <- function(no_tournament, api_url = VIS_API_URL) {
  stopifnot(length(no_tournament) == 1L, !is.na(no_tournament))

  fields <- c(
    "No", "NoTournament", "NoInTournament", "LocalDate", "LocalTime",
    "Status", "ResultType", "NoRound", "RoundCode", "RoundName", "RoundPhase",
    "NoTeamA", "NoTeamB", "NoPlayerA1", "NoPlayerA2", "NoPlayerB1", "NoPlayerB2",
    "TeamAName", "TeamBName", "TeamAFederationCode", "TeamBFederationCode",
    "TeamAPositionInMainDraw", "TeamBPositionInMainDraw",
    "TeamAPositionInQualification", "TeamBPositionInQualification",
    "MatchPointsA", "MatchPointsB",
    "PointsTeamASet1", "PointsTeamBSet1",
    "PointsTeamASet2", "PointsTeamBSet2",
    "PointsTeamASet3", "PointsTeamBSet3",
    "WinnerRank", "LoserRank",
    "TournamentCode", "TournamentName", "TournamentTitle", "TournamentType",
    "DeletedDT", "LastChangeDT", "Version"
  )

  filter_xml <- paste0('<Filter NoTournament="', as.integer(no_tournament), '"/>')

  vis_request_list(
    type = "GetBeachMatchList",
    fields = fields,
    filter_xml = filter_xml,
    node_name = "BeachMatch",
    api_url = api_url
  )
}


vis_get_beach_players <- function(api_url = VIS_API_URL) {
  fields <- c(
    "No",
    "FederationCode",
    "FirstName",
    "LastName",
    "Gender",
    "NationalityCode",
    "TeamName",
    "PopularName",
    "PlaysBeach",
    "ActiveBeach"
  )

  players <- vis_request_list(
    type = "GetPlayerList",
    fields = fields,
    filter_xml = '<Filter PlaysBeach="1"/>',
    node_name = "Player",
    api_url = api_url
  )

  expected_player_columns <- c(
    "no", "federation_code", "first_name", "last_name", "gender",
    "nationality_code", "team_name", "popular_name",
    "plays_beach", "active_beach"
  )

  for (nm in setdiff(expected_player_columns, names(players))) {
    players[[nm]] <- NA_character_
  }

  players |>
    dplyr::mutate(
      athlete_id = as.character(no),
      first_name = as.character(first_name),
      last_name = as.character(last_name),
      full_name = stringr::str_squish(paste(
        dplyr::coalesce(first_name, ""),
        dplyr::coalesce(last_name, "")
      )),
      display_name = dplyr::coalesce(
        dplyr::na_if(full_name, ""),
        dplyr::na_if(as.character(popular_name), ""),
        dplyr::na_if(as.character(team_name), ""),
        athlete_id
      ),
      player_gender = normalize_event_gender(gender),
      player_federation = as.character(federation_code),
      player_nationality = as.character(nationality_code)
    ) |>
    dplyr::select(
      athlete_id,
      display_name,
      first_name,
      last_name,
      popular_name,
      team_name,
      player_gender,
      player_federation,
      player_nationality,
      plays_beach,
      active_beach,
      dplyr::everything(),
      -no,
      -gender,
      -federation_code,
      -nationality_code,
      -full_name
    ) |>
    dplyr::distinct(athlete_id, .keep_all = TRUE)
}


# Live (rally-by-rally) data -------------------------------------------------

# GetBeachLive returns the point sequence recorded by the VIS live scoring
# application. Options is an OR of export flags; 576 = RallyScore (64) +
# EventsRallies (512), the minimum that returns one <Rally> per point. Add
# Events (1) + EventsTimeOffsets (4) + Information (8) + Score (16) = 605 for
# serving team and timing. The request is public and works for finished matches.
VIS_BEACH_LIVE_OPTIONS <- 576L

vis_get_beach_live_xml <- function(no_match,
                                   options = VIS_BEACH_LIVE_OPTIONS,
                                   api_url = VIS_API_URL) {
  stopifnot(length(no_match) == 1L, !is.na(no_match))

  request_xml <- paste0(
    '<Request Type="GetBeachLive" No="', as.integer(no_match),
    '" Options="', as.integer(options), '"/>'
  )

  vis_post(request_xml, api_url = api_url)
}

# Flatten one GetBeachLive response to one row per rally. A response with no
# live data (<NoData>, <AccessDenied>, or a match element without rallies)
# returns zero rows with the reason in attr(, "live_status"); only malformed
# XML or a VIS error element stops. Replayed rallies (score unchanged after a
# challenge) are kept here and removed in the set-cut stage, so the raw layer
# stays faithful to the VIS record.
vis_parse_beach_live_rallies <- function(xml_text, no_match) {
  doc <- xml2::read_xml(xml_text)
  root_name <- xml2::xml_name(doc)

  empty <- tibble::tibble(
    match_no = integer(), set_no = integer(), rally_no = integer(),
    points_a = integer(), points_b = integer(),
    no_serving_team = integer(), rally_status = character(),
    is_canceled = logical()
  )

  if (root_name %in% c("NoData", "AccessDenied")) {
    attr(empty, "live_status") <- root_name
    return(empty)
  }

  bad_nodes <- xml2::xml_find_all(
    doc,
    ".//*[starts-with(local-name(), 'Bad') or local-name()='Error' or local-name()='Unknown']"
  )
  if (length(bad_nodes) > 0L) {
    stop(
      "VIS returned an error for GetBeachLive ", no_match, ": ",
      paste(xml2::xml_text(bad_nodes), collapse = " | "),
      call. = FALSE
    )
  }

  rallies <- xml2::xml_find_all(doc, ".//Rally")
  if (length(rallies) == 0L) {
    attr(empty, "live_status") <- "no_rallies"
    return(empty)
  }

  attr_or_na <- function(nodes, name) {
    x <- xml2::xml_attr(nodes, name)
    x[!nzchar(x)] <- NA_character_
    x
  }

  set_nodes <- xml2::xml_find_first(rallies, "ancestor::Set")

  out <- tibble::tibble(
    match_no = as.integer(no_match),
    set_no = as.integer(attr_or_na(set_nodes, "No")),
    rally_no = as.integer(attr_or_na(rallies, "No")),
    points_a = as.integer(attr_or_na(rallies, "PointsTeamA")),
    points_b = as.integer(attr_or_na(rallies, "PointsTeamB")),
    no_serving_team = as.integer(attr_or_na(rallies, "NoServingTeam")),
    rally_status = attr_or_na(rallies, "Status"),
    is_canceled = tolower(dplyr::coalesce(attr_or_na(rallies, "IsCanceled"), "false")) == "true"
  ) |>
    dplyr::arrange(set_no, rally_no)

  attr(out, "live_status") <- "rallies"
  out
}
