suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

source("scripts/utils/data_loader.R")
source("scripts/utils/feature_engineering.R")
source("scripts/evaluation/lift_chart.R")

build_exposure_analysis <- function(project_root = getwd()) {
  core <- load_core_data(project_root)
  modelling_df <- build_modelling_dataset(core$policies, core$exposures, core$claims)
  preds <- load_predictions(project_root)

  merged <- preds %>%
    left_join(
      modelling_df %>% select(policy_id, year, claim_count, naics_code, state),
      by = c("policy_id", "year")
    )

  analysis <- merged %>%
    group_by(model) %>%
    mutate(risk_decile = ntile(predicted_loss, 10)) %>%
    group_by(model, risk_decile) %>%
    summarise(
      total_claims = sum(claim_count, na.rm = TRUE),
      total_loss = sum(actual_loss, na.rm = TRUE),
      predicted_loss = sum(predicted_loss, na.rm = TRUE),
      exposure = sum(exposure, na.rm = TRUE),
      policy_count = n(),
      .groups = "drop"
    ) %>%
    mutate(loss_cost = total_loss / pmax(exposure, 1e-6))

  ensure_results_dir(project_root)
  fwrite(as.data.table(analysis), file.path(project_root, "results", "exposure_analysis.csv"))

  analysis
}

if (sys.nframe() == 0) {
  ex <- build_exposure_analysis()
  print(head(ex))
}
