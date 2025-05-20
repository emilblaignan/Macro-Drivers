#############################################
# Econ 103 Project - Data Acquisition
# Team Members: Emil Blaignan, Robert Sellman, Sean Eizadi
#############################################

library(fredr)
library(tidyverse)
library(quantmod)
library(lubridate)
library(here)

# Set FRED API key (should be in .Renviron file)
fredr_set_key(Sys.getenv("FRED_API_KEY"))

start_date <- "2000-01-01"
end_date <- "2025-03-31"

# Get S&P 500 data using quantmod
sp500 <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
sp500 <- data.frame(date = index(sp500), coredata(sp500))
names(sp500) <- c("date", "open", "high", "low", "close", "volume", "adjusted")

# FRED series based on potential predictors list
fred_symbols <- c(
  # Interest Rates & Yields
  "GS10",      # 10-Year Treasury Rate
  "TB3MS",     # 3-Month Treasury Rate
  "T10Y2Y",    # 10Y-2Y Spread
  "T10Y3M",    # 10Y-3M Spread
  "BAA",       # Moody's Baa Corporate Bond Yield
  "AAA",       # Moody's Aaa Corporate Bond Yield
  "FEDFUNDS",  # Federal Funds Rate
  
  # Inflation & Prices
  "CPIAUCSL",  # CPI
  "PPIACO",    # PPI
  
  # Economic Activity
  "INDPRO",    # Industrial Production
  "UNRATE",    # Unemployment Rate
  "GDPC1",     # Real GDP (Quarterly)
  "PAYEMS",    # Non-farm Payrolls
  
  # Money Supply
  "M2SL",      # M2 Money Stock
  
  # Leading Indicators
  "USSLIND",   # Leading Index for the US
  
  # Other
  "VIXCLS",    # VIX Index
  "DTWEXM",    # Trade Weighted Dollar Index
  
  # Recession Indicator
  "USREC"      # NBER Recession Indicator
)

# Get Func for FRED series
get_fred_series <- function(series_ids, start_date, end_date) {
  result_df <- data.frame()
  
  for (id in series_ids) {
    cat("Downloading series:", id, "\n")
    tryCatch({
      data <- fredr(
        series_id = id,
        observation_start = as.Date(start_date),
        observation_end = as.Date(end_date)
      )
      result_df <- bind_rows(result_df, data)
    }, error = function(e) {
      cat("Error downloading series", id, ":", e$message, "\n")
    })
    # small delay to avoid hitting rate limits
    Sys.sleep(0.5)
  }
  
  return(result_df)
}

# Get FRED data
fred_data <- get_fred_series(fred_symbols, start_date, end_date)

# Save raw data
dir.create("data", showWarnings = FALSE)
write_csv(sp500, "data/sp500_raw.csv")
write_csv(fred_data, "data/fred_raw.csv")

cat("Data acquisition complete... Files saved to the 'data' folder.\n")