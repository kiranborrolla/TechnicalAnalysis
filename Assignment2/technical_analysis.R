# BDA400 Assignment 2 - Technical Analysis using R (Preliminary Stage)
# Author: Borrolla Kiranreddy
# Portfolio symbols are stored in portfolio.txt, one per line.

required_packages <- c("quantmod", "TTR", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(quantmod)
library(TTR)
library(ggplot2)

# Read stock symbols from portfolio.txt
read_portfolio <- function(file = "portfolio.txt") {
  if (!file.exists(file)) stop("portfolio.txt was not found in the project folder.")
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- symbols[nzchar(symbols)]
  if (length(symbols) == 0) stop("portfolio.txt does not contain any stock symbols.")
  unique(toupper(symbols))
}

# Import daily stock prices from Yahoo Finance using quantmod.
# Returns a named list with one OHLCV data frame per stock.
load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- read_portfolio(file)
  stock_list <- list()

  for (symbol in symbols) {
    message("Downloading: ", symbol)
    xt <- tryCatch(
      suppressWarnings(
        getSymbols(symbol, src = "yahoo", from = from, to = to,
                   auto.assign = FALSE)
      ),
      error = function(e) {
        warning("Could not download ", symbol, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(xt)) {
      df <- data.frame(
        date = as.Date(index(xt)),
        coredata(xt),
        check.names = FALSE
      )
      names(df) <- sub(paste0("^", symbol, "\\."), "", names(df))
      stock_list[[symbol]] <- df
    }
  }

  if (length(stock_list) == 0) {
    stop("No stock data was downloaded. Check internet access and stock symbols.")
  }
  stock_list
}

# Calculate statistics from the Adjusted Close price series.
# Mode returns all values tied for the highest frequency.
calculate_statistics <- function(stock_df, moving_average_window = 20) {
  if (!"Adjusted" %in% names(stock_df)) {
    stop("The stock data must contain an Adjusted price column.")
  }

  prices <- as.numeric(stock_df$Adjusted)
  prices <- prices[is.finite(prices)]

  if (length(prices) == 0) stop("No valid adjusted prices were found.")

  value_counts <- table(prices)
  max_frequency <- max(value_counts)
  modes <- as.numeric(names(value_counts)[value_counts == max_frequency])

  ma <- TTR::SMA(prices, n = moving_average_window)

  data.frame(
    observations = length(prices),
    moving_average_window = moving_average_window,
    latest_adjusted_close = tail(prices, 1),
    latest_moving_average = if (all(is.na(ma))) NA_real_ else
      tail(ma[!is.na(ma)], 1),
    mean = mean(prices),
    mode = paste(round(modes, 4), collapse = ", "),
    median = median(prices),
    standard_deviation = sd(prices),
    stringsAsFactors = FALSE
  )
}

# Display the most recent rows for every stock.
display_stock_data <- function(stock_list, rows = 6) {
  for (symbol in names(stock_list)) {
    cat("\n================ ", symbol, " (latest rows) ================\n")
    print(utils::tail(stock_list[[symbol]], rows))
  }
}

# Build one combined statistics table.
build_statistics_table <- function(stock_list, moving_average_window = 20) {
  result <- lapply(names(stock_list), function(symbol) {
    cbind(symbol = symbol,
          calculate_statistics(stock_list[[symbol]], moving_average_window))
  })
  do.call(rbind, result)
}

# Save adjusted-close charts for each imported stock.
plot_stock_data <- function(stock_list, output_dir = "output") {
  dir.create(output_dir, showWarnings = FALSE)
  for (symbol in names(stock_list)) {
    df <- stock_list[[symbol]]
    plot_df <- data.frame(
      date = df$date,
      adjusted = as.numeric(df$Adjusted)
    )
    p <- ggplot(plot_df, aes(x = date, y = adjusted)) +
      geom_line(color = "steelblue", linewidth = 0.6) +
      labs(
        title = paste(symbol, "Adjusted Closing Price"),
        x = "Date",
        y = "Adjusted Close (USD)"
      ) +
      theme_minimal()
    ggsave(file.path(output_dir, paste0(symbol, "_adjusted_close.png")),
           plot = p, width = 8, height = 4.5, dpi = 120)
  }
}

# Main workflow
portfolio_symbols <- read_portfolio()
cat("Portfolio symbols:", paste(portfolio_symbols, collapse = ", "), "\n")

stock_data <- load_stock_data()
display_stock_data(stock_data)

statistics_table <- build_statistics_table(stock_data, moving_average_window = 20)
cat("\n================ Basic Statistics ================\n")
print(statistics_table, row.names = FALSE)

dir.create("output", showWarnings = FALSE)
write.csv(statistics_table, "output/stock_statistics.csv", row.names = FALSE)
for (symbol in names(stock_data)) {
  write.csv(stock_data[[symbol]],
            file.path("output", paste0(symbol, "_data.csv")),
            row.names = FALSE)
}
plot_stock_data(stock_data)

cat("\nCompleted. CSV files and charts are saved in the output/ folder.\n")
