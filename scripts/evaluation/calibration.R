suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

source("scripts/utils/data_loader.R")
source("scripts/evaluation/lift_chart.R")

build_calibration <- function(project_root = getwd()) {
  preds <- load_predictions(project_root)

  calibration <- preds %>%
    group_by(model) %>%
    mutate(risk_decile = ntile(predicted_loss, 10)) %>%
    group_by(model, risk_decile) %>%
    summarise(
      avg_actual = mean(actual_loss, na.rm = TRUE),
      avg_predicted = mean(predicted_loss, na.rm = TRUE),
      actual_total = sum(actual_loss, na.rm = TRUE),
      predicted_total = sum(predicted_loss, na.rm = TRUE),
      exposure = sum(exposure, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      calibration_ratio = actual_total / pmax(predicted_total, 1e-6),
      actual_loss_ratio = actual_total / pmax(exposure, 1e-6),
      predicted_loss_ratio = predicted_total / pmax(exposure, 1e-6)
    )

  ensure_results_dir(project_root)
  fwrite(as.data.table(calibration), file.path(project_root, "results", "calibration_results.csv"))

  calibration
}

if (sys.nframe() == 0) {
  cal <- build_calibration()
  print(head(cal))
}
