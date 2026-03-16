suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
})

source("scripts/utils/data_loader.R")

load_predictions <- function(project_root = getwd()) {
  result_dir <- file.path(project_root, "results")
  files <- list.files(result_dir, pattern = "^predictions_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No prediction files found. Run modelling scripts first.")
  }
  rbindlist(lapply(files, fread), fill = TRUE)
}

build_lift_chart <- function(project_root = getwd()) {
  preds <- load_predictions(project_root)

  lift <- preds %>%
    group_by(model) %>%
    mutate(risk_decile = ntile(predicted_loss, 10)) %>%
    group_by(model, risk_decile) %>%
    summarise(
      actual_loss = sum(actual_loss, na.rm = TRUE),
      predicted_loss = sum(predicted_loss, na.rm = TRUE),
      exposure = sum(exposure, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      actual_loss_cost = actual_loss / pmax(exposure, 1e-6),
      predicted_loss_cost = predicted_loss / pmax(exposure, 1e-6)
    )

  ensure_results_dir(project_root)
  fwrite(as.data.table(lift), file.path(project_root, "results", "lift_charts.csv"))

  lift
}

if (sys.nframe() == 0) {
  lift <- build_lift_chart()
  print(head(lift))
}
