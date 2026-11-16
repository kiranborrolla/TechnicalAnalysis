# Simple Moving Average (SMA)
# Base R only; returns one value for each complete rolling window.
sma <- function(data, period) {
  if (!is.numeric(data) || length(data) == 0 || anyNA(data) || any(!is.finite(data))) {
    stop("data must be a non-empty numeric vector without NA or non-finite values")
  }
  if (length(period) != 1 || !is.finite(period) || period < 1 || period != as.integer(period)) {
    stop("period must be a positive integer")
  }
  period <- as.integer(period)
  if (length(data) < period) stop("Data length should be greater than or equal to the period")

  sma_values <- numeric(length(data) - period + 1)
  for (i in seq_along(sma_values)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }
  return(sma_values)
}
