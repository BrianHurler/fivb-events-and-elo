enrich_elo_with_players <- function(data, players) {
  player_cols <- c(
    "display_name", "first_name", "last_name", "popular_name", "team_name",
    "player_gender", "player_federation", "player_nationality",
    "plays_beach", "active_beach"
  )

  data <- data |>
    dplyr::select(-dplyr::any_of(player_cols))

  players_key <- players |>
    dplyr::mutate(athlete_id = as.character(athlete_id)) |>
    dplyr::select(
      athlete_id,
      dplyr::any_of(player_cols)
    ) |>
    dplyr::distinct(athlete_id, .keep_all = TRUE)

  out <- data |>
    dplyr::mutate(athlete_id = as.character(athlete_id)) |>
    dplyr::left_join(players_key, by = "athlete_id")

  if ("federation" %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        federation = dplyr::coalesce(
          as.character(federation),
          as.character(player_federation)
        )
      )
  }

  out
}
