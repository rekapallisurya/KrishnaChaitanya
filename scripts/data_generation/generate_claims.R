suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

source("scripts/data_generation/generate_exposures.R")

simulate_severity <- function(n, deductible) {
  if (n <= 0) return(numeric(0))

  component <- sample(c("small", "medium", "large"), n, replace = TRUE, prob = c(0.70, 0.25, 0.05))

  sev <- numeric(n)
  sev[component == "small"] <- rlnorm(sum(component == "small"), meanlog = log(6000), sdlog = 0.55)
  sev[component == "medium"] <- rlnorm(sum(component == "medium"), meanlog = log(45000), sdlog = 0.65)
  sev[component == "large"] <- rlnorm(sum(component == "large"), meanlog = log(350000), sdlog = 0.95)

  pmax(sev - deductible, 200)
}

generate_claims <- function(policies, exposures, seed = 2026, output_path = NULL) {
  set.seed(seed)

  merged <- as.data.table(exposures) %>%
    left_join(as.data.table(policies), by = "policy_id") %>%
    as.data.table()

  merged[, base_freq := 0.12 * exposure * industry_risk_factor * latent_risk_factor]
  merged[, prior_effect := 1 + 0.08 * pmin(prior_claims, 10)]
  merged[, lambda := pmax(0.01, base_freq * prior_effect)]
  merged[, annual_claims := rpois(.N, lambda = lambda)]

  claim_rows <- merged[annual_claims > 0]

  if (nrow(claim_rows) == 0) {
    claims <- data.table(
      claim_id = character(),
      policy_id = character(),
      accident_year = integer(),
      report_delay_months = integer(),
      claim_type = character(),
      litigation_flag = integer(),
      ultimate_loss = numeric()
    )
  } else {
    claims <- rbindlist(lapply(seq_len(nrow(claim_rows)), function(i) {
      row <- claim_rows[i]
      n_claims <- row$annual_claims

      claim_type <- sample(c("BI", "PD"), n_claims, replace = TRUE, prob = c(0.45, 0.55))
      litigation_flag <- rbinom(n_claims, 1, prob = ifelse(claim_type == "BI", 0.25, 0.08))

      severity <- simulate_severity(n_claims, deductible = row$deductible)
      severity <- severity * (1 + 0.15 * litigation_flag)
      severity <- pmin(severity, row$coverage_limit)

      data.table(
        policy_id = row$policy_id,
        accident_year = as.integer(row$year),
        report_delay_months = pmin(60L, pmax(1L, as.integer(round(rlnorm(n_claims, log(5), 0.7))))),
        claim_type = claim_type,
        litigation_flag = litigation_flag,
        ultimate_loss = round(severity, 2)
      )
    }), fill = TRUE)

    claims[, claim_id := sprintf("CLM%08d", seq_len(.N))]
    setcolorder(claims, c("claim_id", "policy_id", "accident_year", "report_delay_months", "claim_type", "litigation_flag", "ultimate_loss"))
  }

  if (!is.null(output_path)) {
    fwrite(claims, output_path)
  }

  claims
}

if (sys.nframe() == 0) {
  project_root <- getwd()
  data_dir <- file.path(project_root, "data")

  policies_path <- file.path(data_dir, "policies.csv")
  exposures_path <- file.path(data_dir, "exposures.csv")

  policies <- if (file.exists(policies_path)) fread(policies_path) else generate_policies(output_path = policies_path)
  exposures <- if (file.exists(exposures_path)) fread(exposures_path) else generate_exposures(policies, output_path = exposures_path)

  claims <- generate_claims(
    policies = policies,
    exposures = exposures,
    output_path = file.path(data_dir, "claims.csv")
  )

  message("Generated claims: ", nrow(claims))
}
