#!/usr/bin/env Rscript

# Stage 03d: derive alternative-format outcomes from the rally archive.
#
# For every set with rally data, record the set winner and the team that
# would have won had the set ended at the first score of 15 or more with a
# two-point lead (the golden-set rule, no cap). The set-1 cuts are what the
# BCS format study needs: "one set to 15", "one set to 21" (the actual set-1
# winner) and the full match, all observed on the same matches.
#
# Replayed rallies (a challenge that repeats the score) are dropped before
# cutting. Three QA checks are recorded per set, not enforced: the feed starts
# at the first point, every rally adds exactly one point, and the final rally
# equals the official VIS set score. A feed that starts late could hide which
# team reached 15 first, so it is not trusted.
# Downstream analysis should filter on qa_pass.
#
# Run explicitly after scripts/03c-pull-rallies.R:
#   source("scripts/03d-build-set-cuts.R")

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
source_project_functions()

rallies <- read_parquet("data-processed/beach_rallies.parquet")
matches <- read_parquet("data-processed/beach_matches.parquet")

golden_set_target <- 15L

# Drop cancelled rallies and replays: a replay repeats the previous total.
rallies <- rallies |>
  dplyr::filter(!dplyr::coalesce(is_canceled, FALSE)) |>
  dplyr::arrange(match_no, set_no, rally_no) |>
  dplyr::group_by(match_no, set_no) |>
  dplyr::mutate(
    total = points_a + points_b,
    is_replay = !is.na(dplyr::lag(total)) & total == dplyr::lag(total)
  ) |>
  dplyr::filter(!is_replay) |>
  dplyr::mutate(step = total - dplyr::lag(total)) |>
  dplyr::ungroup()

# Index of the first rally at or beyond the target with a two-point lead; NA
# if the set never got there (unfinished or shortened sets).
cut_index <- function(points_a, points_b, target) {
  which(pmax(points_a, points_b) >= target & abs(points_a - points_b) >= 2L)[1]
}

set_cuts <- rallies |>
  dplyr::group_by(match_no, set_no) |>
  dplyr::summarise(
    n_rallies = dplyr::n(),
    final_points_a = dplyr::last(points_a),
    final_points_b = dplyr::last(points_b),
    steps_valid = all(is.na(step) | step == 1L),
    starts_at_first_point = dplyr::first(total) == 1L,
    cut_i = cut_index(points_a, points_b, golden_set_target),
    cut_points_a = if (is.na(cut_i)) NA_integer_ else points_a[cut_i],
    cut_points_b = if (is.na(cut_i)) NA_integer_ else points_b[cut_i],
    .groups = "drop"
  ) |>
  dplyr::mutate(
    set_winner = dplyr::case_when(
      final_points_a > final_points_b ~ "A",
      final_points_b > final_points_a ~ "B",
      TRUE ~ NA_character_
    ),
    first_to_15 = dplyr::case_when(
      is.na(cut_i) ~ NA_character_,
      cut_points_a > cut_points_b ~ "A",
      TRUE ~ "B"
    ),
    first_to_15_score = dplyr::if_else(
      is.na(cut_i), NA_character_, paste0(cut_points_a, "-", cut_points_b)
    )
  ) |>
  dplyr::select(-cut_i, -cut_points_a, -cut_points_b)

# Official set scores from the match archive, long by set.
official_set <- function(k) {
  col_a <- paste0("points_team_aset", k)
  col_b <- paste0("points_team_bset", k)
  tibble::tibble(
    match_no = matches$no,
    set_no = k,
    official_points_a = matches[[col_a]],
    official_points_b = matches[[col_b]]
  )
}

official <- dplyr::bind_rows(official_set(1L), official_set(2L), official_set(3L)) |>
  dplyr::filter(!is.na(official_points_a), !is.na(official_points_b))

set_cuts <- set_cuts |>
  dplyr::left_join(official, by = c("match_no", "set_no")) |>
  dplyr::mutate(
    matches_official =
      !is.na(official_points_a) &
      final_points_a == official_points_a &
      final_points_b == official_points_b,
    qa_pass = steps_valid & starts_at_first_point & matches_official & !is.na(set_winner)
  ) |>
  dplyr::arrange(match_no, set_no)

write_parquet(set_cuts, "data-processed/beach_set_cuts.parquet")

# One row per match with rally data: the three format outcomes for the study.
match_cuts <- set_cuts |>
  dplyr::filter(set_no == 1L) |>
  dplyr::select(
    match_no,
    set1_winner = set_winner,
    set1_first_to_15 = first_to_15,
    set1_first_to_15_score = first_to_15_score,
    set1_rallies = n_rallies,
    set1_qa_pass = qa_pass
  ) |>
  dplyr::left_join(
    matches |>
      dplyr::select(match_no = no, match_points_a, match_points_b) |>
      dplyr::mutate(
        match_winner = dplyr::case_when(
          match_points_a > match_points_b ~ "A",
          match_points_b > match_points_a ~ "B",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::select(match_no, match_winner),
    by = "match_no"
  ) |>
  dplyr::left_join(
    set_cuts |>
      dplyr::group_by(match_no) |>
      dplyr::summarise(
        sets_with_rallies = dplyr::n(),
        all_sets_qa_pass = all(qa_pass),
        .groups = "drop"
      ),
    by = "match_no"
  ) |>
  dplyr::mutate(has_rally_data = TRUE) |>
  dplyr::arrange(match_no)

write_parquet(match_cuts, "data-processed/beach_match_set_cuts.parquet")

# QA summary: sets failing either check are listed for review, not dropped.
qa_failures <- set_cuts |>
  dplyr::filter(!qa_pass) |>
  dplyr::select(
    match_no, set_no, n_rallies, final_points_a, final_points_b,
    official_points_a, official_points_b, steps_valid, starts_at_first_point,
    matches_official
  )
readr::write_csv(qa_failures, "data-processed/set_cut_qa_failures.csv")

message(
  "Set cuts complete: ", nrow(set_cuts), " sets from ", nrow(match_cuts),
  " matches; ", sum(set_cuts$qa_pass), " sets pass QA (",
  nrow(qa_failures), " flagged)."
)
print(
  match_cuts |>
    dplyr::filter(set1_qa_pass, !is.na(set1_first_to_15), !is.na(match_winner)) |>
    dplyr::summarise(
      matches = dplyr::n(),
      first_to_15_matches_match_winner = mean(set1_first_to_15 == match_winner),
      set1_matches_match_winner = mean(set1_winner == match_winner)
    )
)
