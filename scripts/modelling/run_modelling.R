source("scripts/modelling/glm_model.R")
source("scripts/modelling/gam_model.R")
source("scripts/modelling/xgboost_model.R")
source("scripts/modelling/tweedie_gbm_model.R")

glm_out <- run_glm_model()
gam_out <- run_gam_model()
xgb_out <- run_xgboost_model()
twb_out <- run_tweedie_gbm_model()

message("Model training complete")
print(rbind(glm_out$metrics, gam_out$metrics, xgb_out$metrics, twb_out$metrics, fill = TRUE))
