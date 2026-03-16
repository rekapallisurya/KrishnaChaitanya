suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

source("scripts/data_generation/generate_claims.R")

generate_claim_triangle <- function(claims, seed = 2026, output_path = NULL) {
  set.seed(seed)

  dev_months <- c(12L, 24L, 36L, 48L)
  base_pattern <- c(0.40, 0.65, 0.85, 1.00)

  if (nrow(claims) == 0) {
    triangle <- data.table(
      accident_year = integer(),
      development_month = integer(),
      cumulative_paid = numeric(),
      incremental_paid = numeric()
    )
  } else {
    expanded <- CJ(claim_id = claims$claim_id, development_month = dev_months)
    expanded <- expanded %>%
      left_join(claims, by = "claim_id") %>%
      as.data.table()

    expanded[, dev_idx := match(development_month, dev_months)]
    expanded[, noisy_pattern := pmin(1, pmax(0.01, base_pattern[dev_idx] + rnorm(.N, 0, 0.02)))]
    expanded[, cumulative_paid_claim := ultimate_loss * noisy_pattern]

    expanded <- expanded[order(claim_id, development_month)]
    expanded[, incremental_paid_claim := cumulative_paid_claim - shift(cumulative_paid_claim, fill = 0), by = claim_id]

    triangle <- expanded[, .(
      cumulative_paid = sum(cumulative_paid_claim, na.rm = TRUE),
      incremental_paid = sum(incremental_paid_claim, na.rm = TRUE)
    ), by = .(accident_year, development_month)][order(accident_year, development_month)]

    triangle[, cumulative_paid := round(cumulative_paid, 2)]
    triangle[, incremental_paid := round(incremental_paid, 2)]
  }

  if (!is.null(output_path)) {
    fwrite(triangle, output_path)
  }

  triangle
}

if (sys.nframe() == 0) {
  project_root <- getwd()
  data_dir <- file.path(project_root, "data")

  claims_path <- file.path(data_dir, "claims.csv")
  if (!file.exists(claims_path)) {
    stop("claims.csv not found. Run generate_claims.R first.")
  }

  claims <- fread(claims_path)
  triangle <- generate_claim_triangle(
    claims = claims,
    output_path = file.path(data_dir, "claim_development.csv")
  )

  message("Generated claim development rows: ", nrow(triangle))
}
