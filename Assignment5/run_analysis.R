# Run from the project root in RStudio.
# Downloads daily AAPL OHLCV data from Stooq, then calculates indicators.
# Stooq may occasionally rate-limit or block automated requests; if that happens,
# download the CSV manually from a reputable provider and save it as data/AAPL.csv.

required_files <- c("R/sma.R", "R/ema.R", "R/macd.R", "R/stdev.R",
                    "R/linreg.R", "R/rsi.R", "R/stoch_rsi.R",
                    "R/crossover.R", "R/crossunder.R")
invisible(lapply(required_files, source))

dir.create("data", showWarnings = FALSE)
input_file <- "data/AAPL.csv"

if (!file.exists(input_file)) {
  data_url <- "https://stooq.com/q/d/l/?s=aapl.us&i=d"
  message("Downloading AAPL daily OHLCV data from Stooq...")
  download_result <- try(
    download.file(data_url, destfile = input_file, mode = "wb", quiet = FALSE),
    silent = TRUE
  )
  if (inherits(download_result, "try-error") ||
      !file.exists(input_file) || file.info(input_file)$size < 100) {
    if (file.exists(input_file)) file.remove(input_file)
    stop(paste(
      "Automatic download was unavailable (the provider may rate-limit requests).",
      "Download daily AAPL data from https://finance.yahoo.com/quote/AAPL/history/",
      "and save the CSV as data/AAPL.csv, then run this script again."
    ))
  }
}

stock <- try(read.csv(input_file, stringsAsFactors = FALSE), silent = TRUE)
if (inherits(stock, "try-error")) stop("Could not read data/AAPL.csv as a CSV file.")

required_columns <- c("Date", "Open", "High", "Low", "Close", "Volume")
if (!all(required_columns %in% names(stock))) {
  stop("CSV must contain Date, Open, High, Low, Close, and Volume columns.")
}
stock$Date <- as.Date(stock$Date)
numeric_columns <- c("Open", "High", "Low", "Close", "Volume")
for (column in numeric_columns) {
  stock[[column]] <- as.numeric(stock[[column]])
}
stock <- stock[complete.cases(stock[, required_columns]), ]
stock <- stock[order(stock$Date), ]
if (nrow(stock) < 30) stop("At least 30 valid daily rows are required for this analysis.")

close_price <- stock$Close
stock$SMA_14 <- c(rep(NA_real_, 13), sma(close_price, 14))
stock$EMA_14 <- ema(close_price, 14)
stock$EMA_25 <- ema(close_price, 25)
macd_result <- macd(close_price, 14, 25, 7)
stock$MACD <- macd_result$macd_line
stock$MACD_Signal <- macd_result$signal_line
stock$MACD_Histogram <- macd_result$histogram
stock$RSI_14 <- rsi(close_price, 14)
stock$Stdev_14 <- c(rep(NA_real_, 13),
                    vapply(seq_len(nrow(stock) - 13),
                           function(i) stdev(close_price[i:(i + 13)]),
                           numeric(1)))
stock$Crossover_EMA14_EMA25 <- crossover(stock$EMA_14, stock$EMA_25)
stock$Crossunder_EMA14_EMA25 <- crossunder(stock$EMA_14, stock$EMA_25)

dir.create("output", showWarnings = FALSE)
write.csv(stock, "output/AAPL_indicators.csv", row.names = FALSE)
cat("Saved output/AAPL_indicators.csv with", nrow(stock), "rows.\\n")
