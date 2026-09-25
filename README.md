# FIVB Events and Elo

Canonical R pipeline for building a reproducible FIVB/VIS beach-volleyball competition history and calculating configurable individual-athlete Elo ratings.

The repository has three jobs:

1. pull the full available FIVB VIS event and beach-tournament archive;
2. pull and preserve the full available FIVB beach-match archive;
3. calculate athlete Elo from an explicitly selected tournament/match universe.

The raw VIS layer is intentionally broader than any one downstream research project. Tournament classification and Elo eligibility are separate so that projects such as `beach-elite-pathways` can consume the same source data without reimplementing VIS logic or baking one definition of "elite" into the archive.

## Design principles

- **VIS identifiers are canonical.** Tournament, match, team, and player IDs are retained directly from VIS.
- **Raw first, classify later.** We preserve all retrieved tournament and match metadata before deciding what counts for Elo.
- **Configuration, not hard-coding.** Elo tournament selection lives in `config/elo.yml`.
- **Resumable backfill.** Matches are downloaded tournament-by-tournament and cached locally, so a long historical pull can restart without losing completed work.
- **No name matching for Elo.** Elo uses the four VIS player IDs on each match.
- **Reproducible outputs.** Classified tournaments, Elo-eligible matches, long athlete-match Elo history, and current ratings are written as versionable artifacts outside Git.

## Repository layout

```text
.
├── R/
│   ├── vis.R
│   ├── tournament-classification.R
│   └── elo.R
├── config/
│   ├── elo.yml
│   ├── tournament-types.csv
│   └── tournament-overrides.csv
├── scripts/
│   ├── _common.R
│   ├── 00-setup.R
│   ├── 01-pull-events.R
│   ├── 02-classify-tournaments.R
│   ├── 02b-build-elo-tournament-universe.R
│   ├── 03-pull-matches.R
│   ├── 04-build-elo-input.R
│   ├── 05-calculate-elo.R
│   ├── 06-validate-outputs.R
│   └── run-all.R
├── data-raw/                 # ignored
└── data-processed/           # ignored
```

## VIS sources

The pipeline uses the public FIVB VIS XML service:

```text
https://www.fivb.org/Vis2009/XmlRequest.asmx
```

Stage 01 requests both:

- `GetEventList` — the general VIS event container history;
- `GetBeachTournamentList` — the beach-tournament history used as the canonical competition table.

Stage 02b materializes the proposed tournament universe for Elo review before any match-level Elo selection. Stage 03 requests `GetBeachMatchList` separately for each beach tournament using its VIS `No` identifier.

## Tournament classification

VIS tournament type is the first classification layer. The mapping in `config/tournament-types.csv` preserves the official discrete type and assigns a practical `event_class`, including:

- Olympic Games
- World Championship
- World Tour / Pro Tour Finals
- Grand Slam
- Major Series
- 5-star through 1-star
- Elite16
- Challenge
- Futures
- Open
- Challenger
- World Series
- continental, national, youth, and other classes

The raw VIS type remains available even when a practical class is added.

Manual historical corrections belong in `config/tournament-overrides.csv`. An override is keyed by the stable VIS tournament number, replaces only the practical `event_class`, records a note, and is surfaced as `classification_source = "manual_override"`. This keeps one auditable home for the classification decisions that were previously repeated across projects.

## Elo selection

Before pulling or calculating Elo, run:

```r
source("scripts/02b-build-elo-tournament-universe.R")
```

This writes:

- `data-processed/elo_tournaments_proposed.csv` — tournaments currently proposed for inclusion;
- `data-processed/elo_tournament_selection_audit.csv` — every classified VIS tournament with include/exclude/review status and reason;
- `data-processed/elo_tournaments_needing_review.csv` — unresolved classifications that should be reviewed before a production Elo profile is frozen;
- `data-processed/elo_tournament_selection_summary.csv` — counts by selection status and event class.

Unresolved legacy tournaments are deliberately surfaced as `review`; they are not silently treated as Elo exclusions.

`config/elo.yml` controls which matches feed Elo. The initial profile is deliberately broad across senior international FIVB tour products so that historical coverage can be audited before narrower research-specific profiles are frozen.

Selection supports:

- included event classes;
- manually included tournament IDs;
- manually excluded tournament IDs;
- manually included match IDs;
- manually excluded match IDs;
- men/women;
- optional start/end dates;
- qualification inclusion;
- Elo K-factor and starting rating.

Explicit includes are additive to the class-based universe; explicit excludes are applied last. A downstream project should consume a named/frozen selection rather than edit historical data.

## Elo methodology

The default is aligned with the existing USAV Elo tracker convention:

```text
expected_score = 1 / (1 + 10 ^ ((opponent_team_mean_elo - athlete_elo) / 400))
new_elo        = athlete_elo + K * (actual_score - expected_score)
```

where:

- `K = 30` by default;
- `actual_score = 1` for a win and `0` for a loss;
- opponent strength is the equal-weight mean of the two opponent athletes;
- each athlete is updated individually;
- all four pre-match ratings are read before any of the four updates are applied.

The output is one row per athlete per match, with athlete, partner, opponents, event context, pre-match Elo, expected win probability, Elo change, and post-match Elo.

## Quick start

Install dependencies:

```r
install.packages(c(
  "arrow",
  "dplyr",
  "httr2",
  "purrr",
  "readr",
  "stringr",
  "tibble",
  "xml2",
  "yaml"
))
```

Then run:

```r
source("scripts/run-all.R")
```

For the initial archive build, Stage 03 can take a long time because it intentionally requests matches one tournament at a time. Completed tournament responses are cached under `data-raw/matches/by-tournament/`.

To force a fresh download of cached VIS files:

```r
Sys.setenv(FIVB_FORCE_REFRESH = "1")
source("scripts/01-pull-events.R")
source("scripts/03-pull-matches.R")
```

## Primary outputs

After a complete run:

```text
data-processed/vis_events.parquet
data-processed/beach_tournaments.parquet
data-processed/beach_tournaments_classified.parquet
data-processed/elo_tournaments_proposed.csv
data-processed/elo_tournament_selection_audit.csv
data-processed/elo_tournaments_needing_review.csv
data-processed/beach_matches.parquet
data-processed/elo_matches.parquet
data-processed/athlete_elo_history.parquet
data-processed/athlete_elo_current.parquet
data-processed/pipeline_validation.csv
```

## Recommended next validation

Before this repository becomes the source of truth for elite pathways:

1. backfill the full VIS archive;
2. compare tournament counts by year/sex/type against known FIVB calendars;
3. compare rebuilt eligible match counts against the current Elo source;
4. compare athlete Elo trajectories and current Elo values against the existing `long_matches_k_factor_30.rda`;
5. resolve classification or match-eligibility differences explicitly;
6. only then evaluate whether the new event universe or rebuilt Elo should alter the elite-pathways event-strength model.

The important architectural separation is that **VIS history, tournament classification, Elo eligibility, and elite-pathways event difficulty remain distinct layers**.

See `docs/decision-log.md` for accepted architecture, unresolved methodological choices, and the gate before this repository should change the elite-pathways model.


## Relationship to elite pathways

This repository should own **source history, classification, and Elo construction**. The elite-pathways repository should continue to own the separate research question of how tournament strength is measured and which achievements count toward a pathway endpoint.

A future integration should therefore pass stable artifacts such as classified tournaments, selected matches, and athlete Elo history into elite pathways. It should not copy VIS request code or maintain a second tournament taxonomy there.
