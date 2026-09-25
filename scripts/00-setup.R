#!/usr/bin/env Rscript

source(file.path("scripts", "_common.R"))
assert_project_root()
check_packages()
initialize_directories()
source_project_functions()

environment <- tibble::tibble(
  component = c("R", required_packages()),
  version = c(
    as.character(getRversion()),
    vapply(
      required_packages(),
      function(pkg) as.character(utils::packageVersion(pkg)),
      character(1)
    )
  )
)

readr::write_csv(environment, "data-processed/execution_environment.csv")
message("Setup complete.")
