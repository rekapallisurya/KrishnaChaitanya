suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringi)
})

generate_policies <- function(n_policies = 4000, seed = 2026, output_path = NULL) {
  set.seed(seed)

  naics_map <- data.table(
    naics_code = c("236220", "722511", "541330", "621111", "452319", "611110", "812112"),
    naics_description = c(
      "Construction",
      "Restaurants",
      "Engineering Services",
      "Physician Offices",
      "Retail",
      "Schools",
      "Beauty Salons"
    ),
    industry_risk_factor = c(1.35, 1.15, 0.85, 0.95, 1.05, 0.90, 1.10)
  )

  states <- data.table(
    state = c("CA", "TX", "FL", "NY", "IL", "GA", "WA", "AZ", "PA", "NC"),
    territory = c("West", "South", "South", "Northeast", "Midwest", "South", "West", "West", "Northeast", "South")
  )

  sampled_naics <- naics_map[sample(.N, n_policies, replace = TRUE)]
  sampled_state <- states[sample(.N, n_policies, replace = TRUE)]

  employees <- pmax(2, round(rlnorm(n_policies, meanlog = log(25), sdlog = 0.8)))
  payroll_per_emp <- runif(n_policies, min = 38000, max = 125000)
  payroll <- employees * payroll_per_emp
  revenue <- payroll * runif(n_policies, min = 1.5, max = 4.5)

  policies <- data.table(
    policy_id = sprintf("POL%06d", seq_len(n_policies)),
    insured_name = paste("Insured", stringi::stri_rand_strings(n_policies, 8, "[A-Z]")),
    naics_code = sampled_naics$naics_code,
    naics_description = sampled_naics$naics_description,
    state = sampled_state$state,
    territory = sampled_state$territory,
    employees = employees,
    payroll = round(payroll, 2),
    revenue = round(revenue, 2),
    years_in_business = pmax(1, round(rlnorm(n_policies, log(12), 0.65))),
    prior_claims = rpois(n_policies, lambda = pmax(0.2, sampled_naics$industry_risk_factor * 0.9)),
    coverage_limit = sample(c(1000000, 2000000, 5000000), n_policies, replace = TRUE, prob = c(0.5, 0.35, 0.15)),
    deductible = sample(c(1000, 2500, 5000, 10000, 25000), n_policies, replace = TRUE, prob = c(0.25, 0.3, 0.25, 0.15, 0.05)),
    latent_risk_factor = round(rlnorm(n_policies, meanlog = log(1), sdlog = 0.3), 4),
    industry_risk_factor = sampled_naics$industry_risk_factor
  )

  if (!is.null(output_path)) {
    fwrite(policies, output_path)
  }

  policies
}

if (sys.nframe() == 0) {
  project_root <- normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = FALSE)
  data_dir <- file.path(project_root, "data")
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

  policy_df <- generate_policies(output_path = file.path(data_dir, "policies.csv"))
  message("Generated policies: ", nrow(policy_df))
}
