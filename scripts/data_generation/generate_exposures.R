suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

source("scripts/data_generation/generate_policies.R")

generate_exposures <- function(policies, start_year = 2018, end_year = 2024, seed = 2026, output_path = NULL) {
  set.seed(seed)

  all_years <- start_year:end_year

  exposures_list <- lapply(seq_len(nrow(policies)), function(i) {
    pol <- policies[i]
    inception <- sample(all_years, 1)
    term_years <- sample(1:4, 1, prob = c(0.35, 0.35, 0.2, 0.1))
    years <- inception:min(end_year, inception + term_years - 1)

    data.table(
      policy_id = pol$policy_id,
      year = years,
      exposure = round(runif(length(years), min = 0.6, max = 1.0), 3),
      payroll = round(pol$payroll * runif(length(years), 0.95, 1.1), 2)
    )
  })

  exposure_df <- rbindlist(exposures_list)

  if (!is.null(output_path)) {
    fwrite(exposure_df, output_path)
  }

  exposure_df
}

if (sys.nframe() == 0) {
  project_root <- getwd()
  data_dir <- file.path(project_root, "data")

  policies_path <- file.path(data_dir, "policies.csv")
  if (!file.exists(policies_path)) {
    policies <- generate_policies(output_path = policies_path)
  } else {
    policies <- fread(policies_path)
  }

  exposures <- generate_exposures(
    policies = policies,
    output_path = file.path(data_dir, "exposures.csv")
  )

  message("Generated exposures: ", nrow(exposures))
}
