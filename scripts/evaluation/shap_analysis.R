suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(xgboost)
  library(DALEX)
  library(iml)
})

source("scripts/utils/data_loader.R")
source("scripts/utils/feature_engineering.R")

build_shap_importance <- function(project_root = getwd(), seed = 42) {
  set.seed(seed)
  core <- load_core_data(project_root)
  model_df <- as.data.frame(build_modelling_dataset(core$policies, core$exposures, core$claims))

  features <- c("payroll", "employees", "years_in_business", "prior_claims", "coverage_limit", "deductible", "latent_risk_factor", "industry_risk_factor")
  x <- model_df[, features]
  y <- model_df$total_loss

  train_idx <- sample(seq_len(nrow(model_df)), size = floor(0.8 * nrow(model_df)))
  x_train <- as.matrix(x[train_idx, , drop = FALSE])
  y_train <- y[train_idx]

  dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train)
  fit <- xgboost::xgb.train(
    params = list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = 0.05,
      max_depth = 5,
      subsample = 0.9,
      colsample_bytree = 0.9
    ),
    data = dtrain,
    nrounds = 200,
    verbose = 0
  )

  importance <- xgboost::xgb.importance(feature_names = colnames(x_train), model = fit)
  if (nrow(importance) == 0) {
    shap_dt <- data.table(feature = character(), gain = numeric(), cover = numeric(), frequency = numeric())
  } else {
    shap_dt <- as.data.table(importance)
    setnames(shap_dt, old = c("Feature", "Gain", "Cover", "Frequency"), new = c("feature", "gain", "cover", "frequency"))
  }

  x_small <- x[train_idx, , drop = FALSE]
  y_small <- y_train

  predictor <- iml::Predictor$new(
    model = fit,
    data = x_small,
    y = y_small,
    predict.function = function(model, newdata) {
      predict(model, as.matrix(newdata))
    }
  )
  iml_imp <- iml::FeatureImp$new(predictor, loss = "mae", n.repetitions = 5)
  iml_dt <- as.data.table(iml_imp$results)
  iml_dt <- iml_dt[, .(feature, iml_importance = importance)]

  explainer <- DALEX::explain(
    model = fit,
    data = x_small,
    y = y_small,
    predict_function = function(model, newdata) {
      predict(model, as.matrix(newdata))
    },
    label = "xgboost_pricing",
    verbose = FALSE
  )

  perf <- DALEX::model_parts(explainer, type = "difference", N = min(2000, nrow(x_small)))
  perf_dt <- as.data.table(perf$result)
  if (!all(c("variable", "dropout_loss") %in% names(perf_dt))) {
    perf_dt <- data.table(feature = character(), dalex_dropout_loss = numeric())
  } else {
    perf_dt <- perf_dt[!variable %in% c("_baseline_", "_full_model_")]
    perf_dt <- perf_dt[, .(feature = variable, dalex_dropout_loss = dropout_loss)]
  }

  shap_out <- merge(shap_dt, perf_dt, by = "feature", all = TRUE)
  shap_out <- merge(shap_out, iml_dt, by = "feature", all = TRUE)

  ensure_results_dir(project_root)
  fwrite(shap_out, file.path(project_root, "results", "shap_importance.csv"))

  shap_out[order(-gain)]
}

if (sys.nframe() == 0) {
  shp <- build_shap_importance()
  print(head(shp))
}
