# Stochastic RSI (%K and %D) using the assignment's global RSI min/max pseudocode.
# Requires rsi.R and sma.R to be sourced first.
stoch_rsi <- function(data, period, k_period, d_period) {
  rsi_values <- rsi(data, period)
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  if (length(valid_rsi) == 0) stop("Not enough data to calculate Stochastic RSI")
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  if (max_rsi == min_rsi) {
    normalized_rsi <- rep(0, length(rsi_values))
    normalized_rsi[is.na(rsi_values)] <- NA_real_
  } else {
    normalized_rsi <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }
  k_line <- sma(normalized_rsi[!is.na(normalized_rsi)], k_period)
  d_line <- sma(k_line, d_period)
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
