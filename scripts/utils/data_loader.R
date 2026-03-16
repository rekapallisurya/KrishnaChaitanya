suppressPackageStartupMessages({
  library(data.table)
})

get_project_root <- function() {
  getwd()
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

load_core_data <- function(project_root = getwd()) {
  data_dir <- file.path(project_root, "data")

  list(
    policies = fread(file.path(data_dir, "policies.csv")),
    exposures = fread(file.path(data_dir, "exposures.csv")),
    claims = fread(file.path(data_dir, "claims.csv")),
    claim_development = fread(file.path(data_dir, "claim_development.csv"))
  )
}

ensure_results_dir <- function(project_root = getwd()) {
  results_dir <- file.path(project_root, "results")
  if (!dir.exists(results_dir)) {
    dir.create(results_dir, recursive = TRUE)
  }
  invisible(results_dir)
}
