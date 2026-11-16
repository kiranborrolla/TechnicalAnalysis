# Crossover: emits Up, Down, or None for each position.
crossover <- function(arr1, arr2) {
  if (!is.numeric(arr1) || !is.numeric(arr2) || length(arr1) != length(arr2)) {
    stop("Both arrays should be numeric and have the same length")
  }
  if (anyNA(arr1) || anyNA(arr2)) stop("Input arrays cannot contain NA values")
  crossover_signals <- rep("None", length(arr1))
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      }
    }
  }
  return(crossover_signals)
}
