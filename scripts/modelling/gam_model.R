suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(mgcv)
})

source("scripts/utils/data_loader.R")
source("scripts/utils/feature_engineering.R")
source("scripts/modelling/model_utils.R")

run_gam_model <- function(project_root = getwd(), seed = 42) {
  core <- load_core_data(project_root)
  model_df <- build_modelling_dataset(core$policies, core$exposures, core$claims)
  split <- split_train_test(model_df, seed = seed)

  train <- as.data.frame(split$train)
  test <- as.data.frame(split$test)

  gam_fit <- mgcv::gam(
    total_loss ~ s(payroll, k = 8) + s(employees, k = 8) + s(years_in_business, k = 6) + s(prior_claims, k = 5) + naics_code + state + offset(log(pmax(exposure, 1e-4))),
    data = train,
    family = mgcv::tw(link = "log")
  )

  pred_loss <- predict(gam_fit, newdata = test, type = "response")

  pred_dt <- data.table(
    policy_id = test$policy_id,
    year = test$year,
    exposure = test$exposure,
    actual_loss = test$total_loss,
    predicted_loss = pmax(0, pred_loss),
    model = "GAM"
  )

  metrics <- collect_metrics(pred_dt$actual_loss, pred_dt$predicted_loss)
  metrics[, model := "GAM"]

  ensure_results_dir(project_root)
  fwrite(pred_dt, file.path(project_root, "results", "predictions_gam.csv"))

  metrics_path <- file.path(project_root, "results", "model_metrics.csv")
  if (file.exists(metrics_path)) {
    m <- fread(metrics_path)
    m <- m[model != "GAM"]
    m <- rbind(m, metrics, fill = TRUE)
  } else {
    m <- metrics
  }
  fwrite(m, metrics_path)

  list(predictions = pred_dt, metrics = metrics)
}

if (sys.nframe() == 0) {
  out <- run_gam_model()
  print(out$metrics)
}
