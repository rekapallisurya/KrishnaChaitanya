suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

source("scripts/utils/data_loader.R")
source("scripts/utils/feature_engineering.R")
source("scripts/modelling/model_utils.R")

run_glm_model <- function(project_root = getwd(), seed = 42) {
  core <- load_core_data(project_root)
  model_df <- build_modelling_dataset(core$policies, core$exposures, core$claims)
  split <- split_train_test(model_df, seed = seed)

  train <- as.data.frame(split$train)
  test <- as.data.frame(split$test)

  freq_glm <- glm(
    claim_count ~ payroll + employees + years_in_business + prior_claims + naics_code + state,
    family = poisson(link = "log"),
    offset = log(pmax(exposure, 1e-4)),
    data = train
  )

  sev_train <- train[train$claim_count > 0 & train$total_loss > 0, ]
  sev_glm <- glm(
    I(total_loss / claim_count) ~ payroll + employees + years_in_business + prior_claims + naics_code + state,
    family = Gamma(link = "log"),
    data = sev_train
  )

  pred_freq <- predict(freq_glm, newdata = test, type = "response")
  pred_sev <- predict(sev_glm, newdata = test, type = "response")
  pred_loss <- pmax(0, pred_freq * pred_sev)

  pred_dt <- data.table(
    policy_id = test$policy_id,
    year = test$year,
    exposure = test$exposure,
    actual_loss = test$total_loss,
    predicted_loss = pred_loss,
    model = "GLM"
  )

  metrics <- collect_metrics(pred_dt$actual_loss, pred_dt$predicted_loss)
  metrics[, model := "GLM"]

  ensure_results_dir(project_root)
  fwrite(pred_dt, file.path(project_root, "results", "predictions_glm.csv"))

  metrics_path <- file.path(project_root, "results", "model_metrics.csv")
  if (file.exists(metrics_path)) {
    m <- fread(metrics_path)
    m <- m[model != "GLM"]
    m <- rbind(m, metrics, fill = TRUE)
  } else {
    m <- metrics
  }
  fwrite(m, metrics_path)

  list(predictions = pred_dt, metrics = metrics)
}

if (sys.nframe() == 0) {
  out <- run_glm_model()
  print(out$metrics)
}
