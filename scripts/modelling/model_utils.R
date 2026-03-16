suppressPackageStartupMessages({
  library(data.table)
})

rmse <- function(actual, pred) {
  sqrt(mean((actual - pred)^2, na.rm = TRUE))
}

mae <- function(actual, pred) {
  mean(abs(actual - pred), na.rm = TRUE)
}

gini_coefficient <- function(actual, pred) {
  dt <- data.table(actual = actual, pred = pred)
  dt <- dt[order(-pred)]
  dt[, cum_actual := cumsum(actual)]
  total_actual <- sum(dt$actual)
  if (total_actual <= 0) return(0)

  dt[, lorenz := cum_actual / total_actual]
  dt[, population := seq_len(.N) / .N]

  auc <- sum((shift(dt$lorenz, fill = 0) + dt$lorenz) * diff(c(0, dt$population)) / 2)
  2 * auc - 1
}

poisson_deviance <- function(actual, pred) {
  pred_safe <- pmax(pred, 1e-6)
  actual_safe <- pmax(actual, 1e-6)
  mean(2 * (actual_safe * log(actual_safe / pred_safe) - (actual_safe - pred_safe)))
}

collect_metrics <- function(actual, pred) {
  data.table(
    RMSE = rmse(actual, pred),
    MAE = mae(actual, pred),
    Gini = gini_coefficient(actual, pred),
    Deviance = poisson_deviance(actual, pred)
  )
}
