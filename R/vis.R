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
      `Accept-Encoding` = "gzip, deflate"
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
    "Nationality",
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
      player_nationality = as.character(nationality)
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
      -nationality,
      -full_name
    ) |>
    dplyr::distinct(athlete_id, .keep_all = TRUE)
}
