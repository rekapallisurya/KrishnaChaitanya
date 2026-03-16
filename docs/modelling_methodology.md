# Modelling Methodology

## Objective

Estimate expected policy-year loss for General Liability pricing and compare model performance.

## Feature engineering

- Aggregate claims to policy-year records.
- Build target variables:
  - claim_count
  - total_loss
  - pure_premium
- Keep policy-level risk covariates and categorical segmentation.

## Models

1. GLM
- Poisson frequency with exposure offset.
- Gamma severity on positive-claim records.
- Combined expected loss = predicted frequency x predicted severity.

2. GAM (mgcv)
- Tweedie GAM with spline effects for non-linear drivers.

3. XGBoost
- Gradient boosted trees with mixed numeric and one-hot encoded categorical predictors.

4. Tweedie GBM
- gbm with Tweedie distribution to model zero-inflated continuous loss.

## Evaluation metrics

- RMSE
- MAE
- Gini coefficient
- Deviance

## Diagnostic outputs

- Lift chart by predicted decile
- Calibration by decile
- Exposure bin analysis with loss cost
- SHAP-style feature importance for XGBoost
