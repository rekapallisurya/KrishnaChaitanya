suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(xgboost)
})

source("scripts/utils/data_loader.R")
source("scripts/utils/feature_engineering.R")
source("scripts/modelling/model_utils.R")

run_xgboost_model <- function(project_root = getwd(), seed = 42) {
  core <- load_core_data(project_root)
  model_df <- build_modelling_dataset(core$policies, core$exposures, core$claims)
  split <- split_train_test(model_df, seed = seed)

  train <- as.data.frame(split$train)
  test <- as.data.frame(split$test)

  features <- c("payroll", "employees", "years_in_business", "prior_claims", "coverage_limit", "deductible", "latent_risk_factor", "industry_risk_factor", "naics_code", "state", "territory")

  x_train <- model.matrix(~ . - 1, data = train[, features])
  x_test <- model.matrix(~ . - 1, data = test[, features])

  dtrain <- xgb.DMatrix(data = x_train, label = train$total_loss, weight = pmax(train$exposure, 0.01))
  dtest <- xgb.DMatrix(data = x_test, label = test$total_loss, weight = pmax(test$exposure, 0.01))

  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.05,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

  fit <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 250,
    evals = list(train = dtrain, test = dtest),
    verbose = 0
  )

  pred_loss <- predict(fit, dtest)

  pred_dt <- data.table(
    policy_id = test$policy_id,
    year = test$year,
    exposure = test$exposure,
    actual_loss = test$total_loss,
    predicted_loss = pmax(0, pred_loss),
    model = "XGBoost"
  )

  metrics <- collect_metrics(pred_dt$actual_loss, pred_dt$predicted_loss)
  metrics[, model := "XGBoost"]

  ensure_results_dir(project_root)
  fwrite(pred_dt, file.path(project_root, "results", "predictions_xgboost.csv"))

  metrics_path <- file.path(project_root, "results", "model_metrics.csv")
  if (file.exists(metrics_path)) {
    m <- fread(metrics_path)
    m <- m[model != "XGBoost"]
    m <- rbind(m, metrics, fill = TRUE)
  } else {
    m <- metrics
  }
  fwrite(m, metrics_path)

  list(predictions = pred_dt, metrics = metrics, fit = fit, train_matrix = x_train, feature_names = colnames(x_train))
}

if (sys.nframe() == 0) {
  out <- run_xgboost_model()
  print(out$metrics)
}
