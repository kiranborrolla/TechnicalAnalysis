# BDA400 Assignment 6 - Technical Analysis Using R: Visualization Phase
# Run from this folder in RStudio with: shiny::runApp()
# Install once: install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "xts"))

required_packages <- c("shiny", "ggplot2", "quantmod", "TTR", "xts")
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install missing packages with: install.packages(c(",
       paste(sprintf('"%s"', missing), collapse = ", "), "))")
}

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(xts)

aggregate_ohlcv <- function(data, timeframe) {
  if (timeframe == "Daily" || nrow(data) < 2) return(data)
  key <- if (timeframe == "Weekly") format(data$date, "%G-%V") else format(data$date, "%Y-%m")
  groups <- split(data, key)
  out <- lapply(groups, function(g) {
    g <- g[order(g$date), ]
    data.frame(date = max(g$date), open = g$open[1],
               high = max(g$high, na.rm = TRUE), low = min(g$low, na.rm = TRUE),
               close = g$close[nrow(g)], volume = sum(g$volume, na.rm = TRUE))
  })
  result <- do.call(rbind, out)
  rownames(result) <- NULL
  result[order(result$date), ]
}

add_indicators <- function(data, short_period, long_period) {
  data <- data[order(data$date), ]
  data$SMA_Short <- as.numeric(TTR::SMA(data$close, n = short_period))
  data$SMA_Long <- as.numeric(TTR::SMA(data$close, n = long_period))
  data$RSI <- as.numeric(TTR::RSI(data$close, n = 14))
  macd <- TTR::MACD(data$close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  data$MACD <- as.numeric(macd[, "macd"])
  data$MACD_Signal <- as.numeric(macd[, "signal"])
  data$MACD_Histogram <- data$MACD - data$MACD_Signal

  prev_short <- c(NA_real_, head(data$SMA_Short, -1))
  prev_long <- c(NA_real_, head(data$SMA_Long, -1))
  buy <- !is.na(data$SMA_Short) & !is.na(data$SMA_Long) &
    !is.na(prev_short) & !is.na(prev_long) &
    data$SMA_Short > data$SMA_Long & prev_short <= prev_long
  sell <- !is.na(data$SMA_Short) & !is.na(data$SMA_Long) &
    !is.na(prev_short) & !is.na(prev_long) &
    data$SMA_Short < data$SMA_Long & prev_short >= prev_long
  data$Signal <- ifelse(buy, "Buy", ifelse(sell, "Sell", "Hold"))
  data
}

ui <- fluidPage(
  titlePanel("BDA400 - Interactive Stock Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock symbol:", "AAPL"),
      dateRangeInput("date_range", "Select date range:",
                     start = Sys.Date() - 365, end = Sys.Date(), max = Sys.Date()),
      selectInput("time_frame", "Select time frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Stock chart type:",
                  choices = c("Line", "Area", "Candlestick")),
      checkboxGroupInput("technical_indicators", "Show technical indicators:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = c("Moving Averages", "RSI", "MACD")),
      numericInput("short_period", "Short moving average period:", 20, min = 2, max = 200),
      numericInput("long_period", "Long moving average period:", 50, min = 3, max = 300),
      checkboxInput("show_signals", "Annotate Buy/Sell signals", TRUE),
      actionButton("load_data", "Load / refresh stock data", class = "btn-primary"),
      helpText("Source: Yahoo Finance. An internet connection is required.")
    ),
    mainPanel(
      h4(textOutput("summary_text")),
      plotOutput("stock_chart", height = "430px"),
      conditionalPanel("input.technical_indicators.includes('RSI')",
                       h4("Relative Strength Index (RSI)"),
                       plotOutput("rsi_chart", height = "220px")),
      conditionalPanel("input.technical_indicators.includes('MACD')",
                       h4("Moving Average Convergence Divergence (MACD)"),
                       plotOutput("macd_chart", height = "240px")),
      h4("Latest Buy / Sell signals"),
      tableOutput("signal_table")
    )
  )
)

server <- function(input, output, session) {
  # Step 1: Fetch historical stock data from Yahoo Finance.
  stock_raw <- eventReactive(input$load_data, {
    req(input$symbol, input$date_range)
    validate(
      need(input$date_range[1] < input$date_range[2],
           "Choose a start date earlier than the end date."),
      need(nzchar(trimws(input$symbol)), "Enter a stock symbol.")
    )
    symbol <- toupper(trimws(input$symbol))
    fetched <- tryCatch(
      quantmod::getSymbols(symbol, src = "yahoo",
                           from = as.Date(input$date_range[1]),
                           to = as.Date(input$date_range[2]) + 1,
                           auto.assign = FALSE),
      error = function(e) {
        showNotification(paste("Data download failed:", conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      }
    )
    validate(need(!is.null(fetched),
                  "No data returned. Check the symbol, date range, and internet connection."))
    data.frame(
      date = as.Date(zoo::index(fetched)),
      open = as.numeric(quantmod::Op(fetched)),
      high = as.numeric(quantmod::Hi(fetched)),
      low = as.numeric(quantmod::Lo(fetched)),
      close = as.numeric(quantmod::Cl(fetched)),
      volume = as.numeric(quantmod::Vo(fetched))
    )
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  # Steps 2-4: Aggregate data, calculate indicators, and derive trading signals.
  stock_data <- reactive({
    raw <- stock_raw()
    req(raw)
    validate(need(nrow(raw) > 0, "No observations for this selection."))
    d <- aggregate_ohlcv(raw, input$time_frame)
    validate(need(nrow(d) >= 3, "Choose a longer date range."))
    validate(need(input$short_period < input$long_period,
                  "Short period must be smaller than long period."))
    add_indicators(d, input$short_period, input$long_period)
  })

  output$summary_text <- renderText({
    d <- stock_data()
    paste0(toupper(input$symbol), " | ", nrow(d), " ",
           tolower(input$time_frame), " observations | ",
           format(min(d$date), "%Y-%m-%d"), " to ",
           format(max(d$date), "%Y-%m-%d"))
  })

  # Main price chart: line, area, or candlestick; moving averages are optional overlays.
  output$stock_chart <- renderPlot({
    d <- stock_data()
    indicators <- input$technical_indicators

    if (input$chart_type == "Line") {
      p <- ggplot(d, aes(date, close)) +
        geom_line(color = "#2457A6", linewidth = 0.7)
    } else if (input$chart_type == "Area") {
      p <- ggplot(d, aes(date, close)) +
        geom_area(fill = "#8DB7E8", alpha = 0.65) +
        geom_line(color = "#2457A6", linewidth = 0.5)
    } else {
      d$direction <- ifelse(d$close >= d$open, "Up", "Down")
      d$body_low <- pmin(d$open, d$close)
      d$body_high <- pmax(d$open, d$close)
      p <- ggplot(d, aes(x = date)) +
        geom_segment(aes(xend = date, y = low, yend = high),
                     color = "grey35", linewidth = 0.45) +
        geom_rect(aes(xmin = date - 0.3, xmax = date + 0.3,
                      ymin = body_low, ymax = body_high, fill = direction),
                  color = "grey25", linewidth = 0.25) +
        scale_fill_manual(values = c(Up = "#2E8B57", Down = "#C94C4C"))
    }

    if ("Moving Averages" %in% indicators) {
      p <- p +
        geom_line(aes(y = SMA_Short), color = "#E69500",
                  linewidth = 0.65, na.rm = TRUE) +
        geom_line(aes(y = SMA_Long), color = "#7A4EAB",
                  linewidth = 0.65, na.rm = TRUE)
    }

    if (isTRUE(input$show_signals)) {
      signals <- d[d$Signal %in% c("Buy", "Sell"), ]
      if (nrow(signals)) {
        p <- p +
          geom_point(data = signals,
                     aes(x = date, y = close, color = Signal, shape = Signal),
                     size = 2.8, inherit.aes = FALSE) +
          geom_text(data = signals,
                    aes(x = date, y = close, label = Signal, color = Signal),
                    vjust = -1, size = 3, show.legend = FALSE,
                    inherit.aes = FALSE) +
          scale_color_manual(values = c(Buy = "#16834A", Sell = "#C0392B"))
      }
    }

    p + labs(title = paste(toupper(input$symbol), "Stock Price"),
             subtitle = paste(input$chart_type, "chart with optional moving-average overlays"),
             x = "Date", y = "Price", fill = "Price direction",
             color = "Trading signal", shape = "Trading signal") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  # RSI and MACD use separate panels because their scales differ from stock price.
  output$rsi_chart <- renderPlot({
    req("RSI" %in% input$technical_indicators)
    d <- stock_data()
    ggplot(d, aes(date, RSI)) +
      geom_line(color = "#3A78B4", linewidth = 0.65, na.rm = TRUE) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed",
                 color = c("#16834A", "#C0392B")) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(title = "RSI (14)", x = "Date", y = "RSI") +
      theme_minimal(base_size = 11)
  })

  output$macd_chart <- renderPlot({
    req("MACD" %in% input$technical_indicators)
    d <- stock_data()
    ggplot(d, aes(x = date)) +
      geom_col(aes(y = MACD_Histogram), fill = "grey75", na.rm = TRUE) +
      geom_line(aes(y = MACD), color = "#2457A6", linewidth = 0.65, na.rm = TRUE) +
      geom_line(aes(y = MACD_Signal), color = "#E69500", linewidth = 0.65, na.rm = TRUE) +
      labs(title = "MACD (12, 26, 9)", x = "Date", y = "MACD") +
      theme_minimal(base_size = 11)
  })

  output$signal_table <- renderTable({
    d <- stock_data()
    signals <- d[d$Signal %in% c("Buy", "Sell"), c("date", "close", "Signal")]
    if (!nrow(signals)) {
      return(data.frame(Message = "No Buy/Sell crossover signals in this period."))
    }
    signals <- signals[order(signals$date, decreasing = TRUE), ]
    names(signals) <- c("Date", "Closing price", "Signal")
    head(signals, 10)
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui = ui, server = server)
