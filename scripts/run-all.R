#!/usr/bin/env Rscript

stages <- c(
  "scripts/00-setup.R",
  "scripts/01-pull-events.R",
  "scripts/02-classify-tournaments.R",
  "scripts/02b-build-elo-tournament-universe.R",
  "scripts/03-pull-matches.R",
  "scripts/04-build-elo-input.R",
  "scripts/05-calculate-elo.R",
  "scripts/06-validate-outputs.R"
)

for (stage in stages) {
  message("\n==> Running ", stage)
  source(stage, local = new.env(parent = globalenv()))
}

message("\nFull FIVB events and Elo pipeline complete.")
