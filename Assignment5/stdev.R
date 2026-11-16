# Population Standard Deviation (stdev), as specified by the assignment formula.
stdev <- function(data) {
  if (!is.numeric(data) || length(data) == 0 || anyNA(data) || any(!is.finite(data))) {
    stop("data must be a non-empty numeric vector without NA or non-finite values")
  }
  mean_value <- sum(data) / length(data)
  diff_values <- data - mean_value
  squared_diff <- diff_values * diff_values
  variance <- sum(squared_diff) / length(squared_diff)
  standard_deviation <- sqrt(variance)
  return(standard_deviation)
}
