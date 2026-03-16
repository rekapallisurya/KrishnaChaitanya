suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

build_modelling_dataset <- function(policies, exposures, claims) {
  policies_dt <- as.data.table(copy(policies))
  exposures_dt <- as.data.table(copy(exposures))
  claims_dt <- as.data.table(copy(claims))

  if (nrow(claims_dt) == 0) {
    claims_agg <- exposures_dt[, .(
      claim_count = 0,
      total_loss = 0
    ), by = .(policy_id, year)]
  } else {
    claims_agg <- claims_dt[, .(
      claim_count = .N,
      total_loss = sum(ultimate_loss, na.rm = TRUE)
    ), by = .(policy_id, accident_year)]

    setnames(claims_agg, "accident_year", "year")
  }

  model_df <- exposures_dt %>%
    left_join(claims_agg, by = c("policy_id", "year")) %>%
    left_join(policies_dt, by = "policy_id") %>%
    mutate(
      payroll = dplyr::coalesce(payroll.x, payroll.y),
      claim_count = ifelse(is.na(claim_count), 0, claim_count),
      total_loss = ifelse(is.na(total_loss), 0, total_loss),
      pure_premium = ifelse(exposure > 0, total_loss / exposure, 0),
      has_claim = as.integer(claim_count > 0),
      naics_code = as.factor(naics_code),
      state = as.factor(state),
      territory = as.factor(territory),
      claim_frequency = ifelse(exposure > 0, claim_count / exposure, 0),
      avg_claim_severity = ifelse(claim_count > 0, total_loss / claim_count, 0)
    ) %>%
    select(-any_of(c("payroll.x", "payroll.y")))

  as.data.table(model_df)
}

split_train_test <- function(model_data, test_ratio = 0.2, seed = 42) {
  set.seed(seed)
  n <- nrow(model_data)
  test_idx <- sample(seq_len(n), size = floor(n * test_ratio))

  list(
    train = model_data[-test_idx],
    test = model_data[test_idx]
  )
}
