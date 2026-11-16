# Relative Strength Index (RSI) using Wilder's smoothing method.
rsi <- function(data, period) {
  if (!is.numeric(data) || length(data) < 2 || anyNA(data) || any(!is.finite(data))) {
    stop("data must contain at least two finite numeric prices")
  }
  if (length(period) != 1 || period < 1 || period != as.integer(period) ||
      period >= length(data)) {
    stop("period must be a positive integer smaller than the length of data")
  }
  period <- as.integer(period)
  diff_values <- diff(data)
  gains <- ifelse(diff_values > 0, diff_values, 0)
  losses <- ifelse(diff_values < 0, abs(diff_values), 0)
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])
  rsi_values <- rep(NA_real_, length(data))

  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    if (avg_loss == 0 && avg_gain == 0) {
      rsi_values[i] <- 50
    } else if (avg_loss == 0) {
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }
  return(rsi_values)
}
