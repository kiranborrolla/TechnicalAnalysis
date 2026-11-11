# BDA400 Assignment 2 – Technical Analysis using R

**Student:** Borrolla Kiranreddy

## Project contents
- `portfolio.txt`: stock symbols, one per line.
- `technical_analysis.R`: functions to import stock prices, calculate statistics, display data, and create charts.
- `output/`: generated CSV files and PNG charts after running the script.

## Requirements
- R and RStudio
- Internet access for Yahoo Finance data retrieval
- R packages: `quantmod`, `TTR`, `ggplot2`

## Run
1. Open this folder as an RStudio Project or set it as the working directory.
2. Open `technical_analysis.R`.
3. Run the complete script (Source).

The script reads symbols from `portfolio.txt`, downloads approximately one year of daily data, calculates a 20-day simple moving average plus mean, mode, median, and standard deviation of adjusted closing prices, prints recent observations, and saves tables/charts under `output/`.

## Portfolio symbols
The starter portfolio contains AAPL, MSFT, and GOOGL. You may replace these with your own symbols before running.

## Data note
Stock prices are retrieved from Yahoo Finance through `quantmod`. Availability, historical coverage, and ticker support depend on the data provider and internet access. This project is educational and is not investment advice.
