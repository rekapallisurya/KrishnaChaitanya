# insurance-pricing-case-study-R

Synthetic General Liability pricing case study in R with modular data generation, actuarial modelling, machine learning comparison, and an interactive Shiny dashboard.

## Project overview

This repository simulates a long-tail GL portfolio, trains four pricing models, evaluates model performance with insurance-focused diagnostics, and exposes outputs in a Shiny app.

## Repository structure

- data/: generated synthetic datasets
- scripts/data_generation/: policy, exposure, claims, and development generation scripts
- scripts/modelling/: GLM, GAM, XGBoost, Tweedie GBM modelling scripts
- scripts/evaluation/: lift, calibration, exposure bins, and SHAP-style explainability scripts
- scripts/utils/: data loading and feature engineering utilities
- notebooks/: RMarkdown analysis notebooks
- shiny_app/: interactive dashboard app
- results/: exported evaluation outputs
- docs/: methodology and assumptions documentation

## Required libraries

tidyverse, data.table, mgcv, xgboost, gbm, tweedie, caret, ggplot2, plotly, shiny, DT, iml, DALEX, SHAPforxgboost, stringi

## How to run end-to-end

1. Open an R session at repository root.
2. Install required packages if needed.
3. Run:

Rscript run_all.R

This executes:
- data simulation
- model training
- evaluation and CSV exports

## Run each stage separately

- Data generation: Rscript scripts/data_generation/run_data_generation.R
- Modelling: Rscript scripts/modelling/run_modelling.R
- Evaluation: Rscript scripts/evaluation/run_evaluation.R

## Launch Shiny dashboard

From repository root:

Rscript -e "shiny::runApp('shiny_app')"

## Exported outputs

Generated in results/:
- lift_charts.csv
- calibration_results.csv
- exposure_analysis.csv
- shap_importance.csv

## Notes

- All datasets are synthetic and intended for analytics experimentation.
- Default scripts assume working directory is the repository root.
