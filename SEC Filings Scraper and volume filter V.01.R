
# Load required packages
library(stringr)
library(data.table)
library(BatchGetSymbols)
library(quantmod)
library(dplyr)
library(PerformanceAnalytics)
library(pbapply)
library(rvest)
library(lubridate)
library(bizdays)
library(timeDate)
library(RQuantLib)
library(xts)
library(zoo)

# Set working directory
setwd("C:/Users/Dylan/Desktop/STONKS BACKTEST/new insdier project")

# Create necessary directories if they don't exist
if (!dir.exists(file.path(getwd(), "Filings"))) dir.create(file.path(getwd(), "Filings"))
if (!dir.exists(file.path(getwd(), "Insiders"))) dir.create(file.path(getwd(), "Insiders"))

# Define the formatVOL function to clean numeric data
formatVOL <- function(x) {
  as.numeric(gsub(",", "", x))
}

# **********************************************************************************
#                INSIDER TRANSACTIONS IMPORTING
# **********************************************************************************
# Scrape insider trading data from Finviz
url <- "https://finviz.com/insidertrading.ashx"
webpage <- read_html(url)

# Adjust the index to select the correct table (may need to update if Finviz changes)
tables <- webpage %>% html_nodes("table")
TABLE <- tables[[7]] %>% html_table(header = TRUE, fill = TRUE)

# Clean column names
noms <- names(TABLE)
noms <- gsub("#", "Num", noms)
noms <- gsub("\\$", "Dollar", noms)
noms <- gsub("\\(", "", noms)
noms <- gsub("\\)", "", noms)
noms <- gsub(" ", "", noms)
colnames(TABLE) <- noms

# Parse dates and numeric values
current_year <- year(Sys.Date())
TABLE$Date <- as.Date(paste0(TABLE$Date, " ", current_year), format = "%b %d %Y")
TABLE$NumShares <- formatVOL(TABLE$NumShares)
TABLE$ValueDollar <- formatVOL(TABLE$ValueDollar)
TABLE$NumSharesTotal <- formatVOL(TABLE$NumSharesTotal)
TABLE$SECForm4 <- paste0(TABLE$SECForm4, " ", current_year)
TABLE$SECForm4 <- as.POSIXct(TABLE$SECForm4, format = "%b %d %I:%M %p %Y")

# Remove duplicates and NAs
TABLE <- na.omit(TABLE)
TABLE <- unique(TABLE)

# Define the directory path for saving filings
directory_path <- file.path(getwd(), "Filings")

# Generate a unique filename with a timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
file_name <- paste0("TABLE_", timestamp, ".rds")
save_path <- file.path(directory_path, file_name)

# Save the TABLE data frame to the specified path
saveRDS(TABLE, file = save_path)

# **********************************************************************************
#                DATA PROCESSING AND FILTERING
# **********************************************************************************
# List all saved filings and combine them
FILES <- list.files(directory_path, full.names = TRUE)
ALL <- rbindlist(lapply(FILES, readRDS), use.names = TRUE)
ALL <- unique(ALL)

# Filter filings within a specific date range
START <- as.Date("2023-08-01")  # Adjusted to the current year
END <- Sys.Date()
ALL <- ALL %>% filter(Date >= START & Date <= END)

# Extract unique tickers
tickers <- unique(ALL$Ticker)

# Fetch stock data for the tickers
stock_data <- BatchGetSymbols(
  tickers = tickers,
  first.date = START - 30,  # Fetch data from 30 days before START for calculations
  last.date = END,
  thresh.bad.data = 0.75,
  bench.ticker = "^GSPC",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last"
)

# Filter out tickers with bad data
KEEP <- stock_data$df.control %>% filter(threshold.decision == "KEEP")
tickers <- KEEP$ticker

# Get stock data
data <- stock_data$df.tickers

# Apply Dollar Volume Filter (Top 0.5%)
DV <- data %>%
  group_by(ticker) %>%
  summarize(DVol = mean(price.close * volume, na.rm = TRUE)) %>%
  arrange(desc(DVol))

# Get the threshold for top 0.5%
threshold <- quantile(DV$DVol, probs = 0.995, na.rm = TRUE)
DV_filtered <- DV %>% filter(DVol >= threshold)
tickers <- DV_filtered$ticker

# Filter data for tickers within the price range $1 to $150
filtered_data <- data %>%
  filter(ticker %in% tickers) %>%
  filter(price.close >= 1 & price.close <= 150)

tickers <- unique(filtered_data$ticker)

# Update ALL to only include filtered tickers
ALL <- ALL %>% filter(Ticker %in% tickers)

# **********************************************************************************
#   FUNCTION TO CALCULATE METRICS AND GENERATE CHARTS
# **********************************************************************************
getInsiderMetrics <- function(symbol, hold = 14) {
  # Create NYSE calendar
  nyse_holidays <- as.Date(holidayNYSE(year = unique(year(ALL$Date))))
  create.calendar("NYSE", holidays = nyse_holidays, weekdays = c("saturday", "sunday"))
  
  # Subset stock data
  stk_data <- filtered_data %>% filter(ticker == symbol)
  stk_data$ref.date <- as.Date(stk_data$ref.date)
  
  # Subset insider transactions
  insdr_tr <- ALL %>% filter(Ticker == symbol)
  insdr_tr$SECForm4 <- as.Date(insdr_tr$SECForm4)
  
  # Adjust SECForm4 dates to next business day
  insdr_tr$SECForm4 <- adjust.next(insdr_tr$SECForm4, cal = "NYSE")
  
  # Initialize metrics data frame
  metrics_df <- data.frame()
  
  # Loop over each filing
  for (i in 1:nrow(insdr_tr)) {
    filing_date <- insdr_tr$SECForm4[i]
    # Get previous trading day
    prev_date <- adjust.previous(filing_date - 1, cal = "NYSE")
    # Get stock data for prev_date and filing_date
    prev_data <- stk_data %>% filter(ref.date == prev_date)
    next_data <- stk_data %>% filter(ref.date == filing_date)
    
    if (nrow(prev_data) > 0 & nrow(next_data) > 0) {
      # Prices
      prev_close <- prev_data$price.close
      next_open <- next_data$price.open
      next_high <- next_data$price.high
      next_low <- next_data$price.low
      next_close <- next_data$price.close
      
      # Calculate percentage changes
      close_to_open_gap <- (next_open - prev_close) / prev_close * 100
      open_to_high <- (next_high - next_open) / next_open * 100
      open_to_low <- (next_low - next_open) / next_open * 100
      open_to_close <- (next_close - next_open) / next_open * 100
      
      # Determine if the day was green (close > open)
      green_day <- next_close > next_open
      
      # Append to metrics_df
      metrics_df <- rbind(metrics_df, data.frame(
        symbol = symbol,
        date = filing_date,
        prev_close = prev_close,
        next_open = next_open,
        next_high = next_high,
        next_low = next_low,
        next_close = next_close,
        close_to_open_gap = close_to_open_gap,
        open_to_high = open_to_high,
        open_to_low = open_to_low,
        open_to_close = open_to_close,
        green_day = green_day
      ))
    }
  }
  
  if (nrow(metrics_df) > 0) {
    # Calculate summary metrics
    close_to_open_avg <- mean(metrics_df$close_to_open_gap, na.rm = TRUE)
    close_to_open_high <- max(metrics_df$close_to_open_gap, na.rm = TRUE)
    average_HOD <- mean(metrics_df$open_to_high, na.rm = TRUE)
    
    # High Return (green day) metrics
    green_days <- metrics_df %>% filter(green_day == TRUE)
    highest_gain_green <- max(green_days$open_to_high, na.rm = TRUE)
    average_gain_green <- mean(green_days$open_to_high, na.rm = TRUE)
    lowest_gain_green <- min(green_days$open_to_high, na.rm = TRUE)
    
    # Low Return (red day) metrics
    red_days <- metrics_df %>% filter(green_day == FALSE)
    highest_loss_red <- max(red_days$open_to_low, na.rm = TRUE)
    average_loss_red <- mean(red_days$open_to_low, na.rm = TRUE)
    lowest_loss_red <- min(red_days$open_to_low, na.rm = TRUE)
    
    # Hold for whole day metrics
    highest_open_to_close <- max(metrics_df$open_to_close, na.rm = TRUE)
    average_open_to_close <- mean(metrics_df$open_to_close, na.rm = TRUE)
    
    # Probability of green day
    green_day_prob <- sum(metrics_df$green_day, na.rm = TRUE) / nrow(metrics_df)
    
    # Create summary data frame
    metrics_summary <- data.frame(
      ticker = symbol,
      close_to_open_avg = close_to_open_avg,
      close_to_open_high = close_to_open_high,
      average_HOD = average_HOD,
      highest_gain_green = highest_gain_green,
      average_gain_green = average_gain_green,
      lowest_gain_green = lowest_gain_green,
      highest_loss_red = highest_loss_red,
      average_loss_red = average_loss_red,
      lowest_loss_red = lowest_loss_red,
      highest_open_to_close = highest_open_to_close,
      average_open_to_close = average_open_to_close,
      green_day_prob = green_day_prob
    )
  } else {
    metrics_summary <- data.frame(
      ticker = symbol,
      close_to_open_avg = NA,
      close_to_open_high = NA,
      average_HOD = NA,
      highest_gain_green = NA,
      average_gain_green = NA,
      lowest_gain_green = NA,
      highest_loss_red = NA,
      average_loss_red = NA,
      lowest_loss_red = NA,
      highest_open_to_close = NA,
      average_open_to_close = NA,
      green_day_prob = NA
    )
  }
  
  # Generate and save charts
  stk_xts <- xts(
    stk_data[, c("price.open", "price.high", "price.low", "price.close", "volume")],
    order.by = stk_data$ref.date
  )
  
  # Create transaction annotations
  transaction_dates <- insdr_tr$SECForm4
  transaction_signals <- ifelse(insdr_tr$Transaction == "Sale", -1, 1)
  stk_xts$Transaction <- 0
  stk_xts$Transaction[as.character(transaction_dates)] <- transaction_signals
  
  # Save chart as PDF
  pdf(file = file.path(getwd(), "Insiders", paste0(symbol, ".pdf")), width = 11, height = 8.5)
  chartSeries(stk_xts[, 1:5], name = symbol, theme = chartTheme("white"))
  addTA(stk_xts$Transaction, col = "red", type = "h", legend = "Insider Transactions")
  dev.off()
  
  return(list(metrics_summary = metrics_summary, metrics_df = metrics_df))
}

# **********************************************************************************
#   PROCESS ALL TICKERS AND COLLECT METRICS
# **********************************************************************************
# Initialize list to store metrics
metrics_list <- list()

# Loop over each ticker and calculate metrics
for (symbol in tickers) {
  result <- tryCatch({
    getInsiderMetrics(symbol, hold = 14)
  }, error = function(e) {
    message("Error processing ", symbol, ": ", e$message)
    NULL
  })
  
  if (!is.null(result)) {
    metrics_list[[symbol]] <- result$metrics_summary
  }
}

# Combine all metrics into a data frame
all_metrics <- do.call(rbind, metrics_list)

# Save the metrics to a CSV file
write.csv(all_metrics, file = file.path(getwd(), "InsiderMetricsSummary.csv"), row.names = FALSE)

# **********************************************************************************
#   BACKTESTING FRAMEWORK (Optional)
# **********************************************************************************
# Load required packages for backtesting
library(quantstrat)
library(blotter)

# Initialize portfolio, account, and orders
currency("USD")
stock(tickers, currency = "USD", multiplier = 1)

init_date <- as.character(START - 1)
init_equity <- 1000000
portfolio_st <- "InsiderPortfolio"
account_st <- "InsiderAccount"

rm.strat(portfolio_st)
rm.strat(account_st)

initPortf(portfolio_st, symbols = tickers, initDate = init_date)
initAcct(account_st, portfolios = portfolio_st, initDate = init_date, initEq = init_equity)
initOrders(portfolio_st, initDate = init_date)
strategy_st <- "InsiderStrategy"
rm.strat(strategy_st)
strategy(strategy_st, store = TRUE)

# Define strategy parameters
add.rule(strategy_st, name = "ruleSignal",
         arguments = list(sigcol = "Transaction", sigval = 1,
                          orderqty = 1000, ordertype = "market", orderside = "long"),
         type = "enter", label = "LongEntry")

add.rule(strategy_st, name = "ruleSignal",
         arguments = list(sigcol = "Transaction", sigval = -1,
                          orderqty = -1000, ordertype = "market", orderside = "short"),
         type = "enter", label = "ShortEntry")

# Apply strategy
applyStrategy(strategy = strategy_st, portfolios = portfolio_st)

# Update portfolio and account
updatePortf(portfolio_st)
updateAcct(account_st)
updateEndEq(account_st)

# Get trade statistics
tStats <- tradeStats(Portfolios = portfolio_st)
write.csv(tStats, file = file.path(getwd(), "BacktestResults.csv"), row.names = FALSE)

# **********************************************************************************
#   END OF SCRIPT
# **********************************************************************************

