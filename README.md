# BDA400 Assignment 5 — Technical Analysis Using R (Development Phase)

**Student:** Borrolla Kiranreddy  
**Course:** BDA400 — Data Science Tools and Techniques  
**Assignment:** Assignment 5 — Technical Analysis Using R, Development Phase  
**Stock symbol used for the project:** AAPL (Apple Inc.)  
**Data source:** Yahoo Finance historical data: https://finance.yahoo.com/quote/AAPL/history/

## Important course-alignment note
The Assignment 5 brief says to build on historical stock data obtained in Assignment 1. The supplied Assignment 1 PDF describes a synthetic sales project and does not document stock-market data. Therefore, this project uses AAPL historical daily OHLCV data as a clearly disclosed alternative. If your instructor expects the exact Assignment 1 stock dataset, replace `data/AAPL.csv` with that dataset before final submission and document its source.

## Data
The `run_analysis.R` script first attempts to download daily AAPL OHLCV data from Stooq automatically using base R `download.file()`. Stooq publishes daily CSV data in Date, Open, High, Low, Close, and Volume format, but may rate-limit or block automated requests. If automatic download fails, download the CSV from Yahoo Finance Historical Data and save it as `data/AAPL.csv`. Yahoo Finance historical data page: https://finance.yahoo.com/quote/AAPL/history/.

Yahoo Finance may restrict direct CSV download depending on account and licensing. Do not fabricate prices. Keep the source and download date in your final documentation.

## Project structure
- `R/` — one base-R script per required indicator
- `tests/test_indicators.R` — deterministic test cases using the assignment's example-style vectors
- `data/` — place the downloaded AAPL CSV here
- `run_analysis.R` — loads the stock CSV, calculates indicators, and writes an output CSV

## Restrictions
The nine indicator implementations use base R only. No technical-analysis libraries are used. `read.csv()` is used only to import the supplied historical CSV.

## Run in RStudio
1. Open this project folder in RStudio.
2. Put the downloaded file at `data/AAPL.csv`.
3. Run `source("tests/test_indicators.R")`.
4. Run `source("run_analysis.R")`.
5. Review `output/AAPL_indicators.csv`.

## Indicator implementation notes
- `sma()` returns rolling means for complete windows.
- `ema()` follows the supplied pseudocode: the first EMA equals the first observation.
- `macd()` returns MACD line, signal line, and histogram.
- `stdev()` implements the assignment's population standard deviation formula (divide by n).
- `linreg()` follows the assignment's index-based regression pseudocode and returns slope, intercept, predictions, and selected subset.
- `rsi()` uses Wilder smoothing as specified.
- `stoch_rsi()` uses the assignment pseudocode's normalization over the available RSI series, then calculates %K and %D with SMA.
- `crossover()` returns `Up`, `Down`, or `None`.
- `crossunder()` returns text values `True` or `False`, as in the supplied pseudocode.

## Repository
Create a separate repository for Assignment 5, for example `BDA400-Assignment-5-Technical-Analysis`. Add its URL to the cover page after creating it.
