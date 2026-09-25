# Decision Log

This file records durable architecture choices and open methodological decisions for the FIVB events and Elo pipeline.

## Accepted architecture

### 2026-09-25 — VIS history is the source layer

The repository will preserve the broad public FIVB VIS beach-volleyball archive before applying research-specific selection rules.

Reason: tournament definitions have changed repeatedly across eras and projects. Raw history should not need to be re-pulled or rewritten when a downstream definition changes.

### 2026-09-25 — VIS numeric identifiers are canonical

Tournament `No`, match `No`, team IDs, and player IDs are retained directly from VIS.

Names and titles are descriptive attributes, not identity keys.

### 2026-09-25 — Classification is separate from extraction

The raw VIS tournament type is always retained. A practical `event_class` is derived from the official VIS type mapping, with name/title inference only where needed.

Known historical corrections belong in `config/tournament-overrides.csv` and must include a note.

### 2026-09-25 — Match archive is pulled tournament by tournament

Historical matches are requested with `GetBeachMatchList` filtered by the stable VIS tournament number and cached one tournament at a time.

Reason: the initial archive can be large. Per-tournament caching makes the backfill resumable and makes failed or anomalous tournaments auditable.

### 2026-09-25 — Raw archive and Elo universe are different objects

`data-processed/beach_matches.parquet` is the broad retrieved match archive.

`data-processed/elo_matches.parquet` is a selected analytical universe generated from `config/elo.yml`.

Changing an Elo definition must not delete or modify raw historical matches.

### 2026-09-25 — Elo is athlete-level by default

The initial engine uses:

- starting Elo 1500;
- K = 30;
- standard 400-point logistic scale;
- each athlete's own pre-match Elo;
- the mean pre-match Elo of the two opponents as opponent strength;
- all four ratings frozen before the match and updated only after all four expected scores are calculated.

This is intended to align with the current USAV Elo convention, but equivalence with the existing upstream S3 Elo artifact must be demonstrated empirically before replacement.

### 2026-09-25 — Elite pathways remains downstream

This repository owns VIS extraction, competition classification, Elo match selection, and Elo calculation.

`beach-elite-pathways` continues to own tournament-strength research, endpoint definitions, and pathway modeling. It should eventually consume frozen outputs from this repository rather than reproduce VIS logic.


### 2026-09-25 — Elo history begins on 2008-01-01

The raw VIS tournament archive remains intact, but Elo construction excludes all tournaments and matches before 2008-01-01.

Reason: pre-2008 VIS coverage is not considered reliable enough for a canonical Elo history.

### 2026-09-25 — Selected senior continental pathways are included in Elo

The Elo profile includes senior continental competition classes when the VIS organizer code is one of:

- AVC;
- NORCECA;
- CSV;
- CEV.

CAVB is explicitly excluded from this profile. Youth continental classes and zonal tours remain excluded.

Continental candidates with an unrecognized organizer code are marked for review rather than silently included or excluded. The same tournament selector drives both the review CSV and match-level Elo eligibility.

## Open decisions before production use

### Exact production Elo universe

The initial `senior_fivb_broad_v1` profile is deliberately broad. It is not yet declared equivalent to the current production Elo history.

We still need to determine the exact historical inclusion rule required for a production-compatible profile.

### Qualification matches

Qualification is included in the broad profile. We need to confirm whether this matches the legacy Elo pipeline and whether any downstream profile should differ.

### Forfeits, retirements, defaults, and unusual result types

The first implementation requires a decisive match-point result but does not yet freeze a policy for every VIS `ResultType`.

Before production use, enumerate result types in the archive, compare them with the legacy Elo source, and make the treatment explicit in configuration or preprocessing.

### Historical coverage gaps

"Since inception" means the complete history that VIS makes publicly available, not an assumption that VIS contains every match ever played.

After the backfill, audit first dates, annual event counts, annual match counts, missing player IDs, and known historical calendars.

### Legacy/type-0 competition classification

The official type mapping should resolve most tournaments. Any legacy records not sufficiently described by the type must be reviewed and, when necessary, entered as stable manual overrides.

### Elo parity tolerance

Before replacing the existing Elo source, define acceptable parity checks for:

- match-universe overlap;
- athlete career match counts;
- selected athlete Elo trajectories;
- current Elo values;
- event/category composition by era.

Differences should be explained rather than tuned away blindly.

## Gate before elite-pathways integration

Do not change the elite-pathways model simply because this new pipeline exists.

The integration gate is:

1. complete VIS archive backfill;
2. classify and audit event history;
3. freeze a production Elo profile;
4. compare it against the current S3 Elo source;
5. explain material differences;
6. only then rerun event-strength and pathway analyses using the new canonical artifacts.
