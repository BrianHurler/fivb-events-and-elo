infer_event_class_from_text <- function(name, title = name) {
  x <- stringr::str_to_lower(paste(name, title))

  dplyr::case_when(
    stringr::str_detect(x, "u17|u18|u19|u20|u21|u22|u23|youth|junior") ~ NA_character_,
    stringr::str_detect(x, "elite\\s*16|elite16") ~ "Elite16",
    stringr::str_detect(x, "pro tour.*final|beach pro tour.*final") ~ "Pro Tour Finals",
    stringr::str_detect(x, "world tour.*final") ~ "World Tour Finals",
    stringr::str_detect(x, "world championship") ~ "World Championship",
    stringr::str_detect(x, "olympic") ~ "Olympic Games",
    stringr::str_detect(x, "grand slam") ~ "Grand Slam",
    stringr::str_detect(x, "major") ~ "Major Series",
    stringr::str_detect(x, "5[ -]?star|five[ -]?star") ~ "5-Star",
    stringr::str_detect(x, "4[ -]?star|four[ -]?star") ~ "4-Star",
    stringr::str_detect(x, "3[ -]?star|three[ -]?star") ~ "3-Star",
    stringr::str_detect(x, "2[ -]?star|two[ -]?star") ~ "2-Star",
    stringr::str_detect(x, "1[ -]?star|one[ -]?star") ~ "1-Star",
    stringr::str_detect(x, "challenge") ~ "Challenge",
    stringr::str_detect(x, "future") ~ "Futures",
    stringr::str_detect(x, "challenger") ~ "Challenger",
    stringr::str_detect(x, "world series") ~ "World Series",
    stringr::str_detect(x, "\\bopen\\b") ~ "Open",
    TRUE ~ NA_character_
  )
}

classify_beach_tournaments <- function(tournaments, type_map) {
  required <- c("no", "type")
  missing <- setdiff(required, names(tournaments))
  if (length(missing) > 0L) {
    stop("Tournament data missing required fields: ", paste(missing, collapse = ", "))
  }

  if (!"name" %in% names(tournaments)) tournaments$name <- NA_character_
  if (!"title" %in% names(tournaments)) tournaments$title <- tournaments$name

  raw_type <- as.character(tournaments$type)
  numeric_type <- suppressWarnings(as.integer(raw_type))

  idx_value <- match(numeric_type, type_map$type_value)
  idx_name <- match(raw_type, type_map$type_name)
  idx <- idx_value
  idx[is.na(idx)] <- idx_name[is.na(idx)]

  mapped_class <- type_map$event_class[idx]
  mapped_type_name <- type_map$type_name[idx]
  senior <- type_map$senior_international[idx]

  inferred_class <- infer_event_class_from_text(
    tournaments$name,
    tournaments$title
  )

  use_inferred <- is.na(mapped_class) | mapped_class == "Other"

  tournaments |>
    dplyr::mutate(
      vis_type_raw = raw_type,
      vis_type_value = numeric_type,
      vis_type_name = mapped_type_name,
      event_class = dplyr::if_else(
        use_inferred & !is.na(inferred_class),
        inferred_class,
        mapped_class
      ),
      classification_source = dplyr::case_when(
        !use_inferred & !is.na(mapped_class) ~ "vis_type",
        use_inferred & !is.na(inferred_class) ~ "name_title_inference",
        TRUE ~ "unclassified"
      ),
      senior_international = senior
    )
}
