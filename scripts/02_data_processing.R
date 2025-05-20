#############################################
# Econ 103 Project - Data Processing
# Team Members: Emil Blaignan, Robert Sellman, Sean Eizadi
#############################################

# Load required packages
library(tidyverse)
library(lubridate)
library(here)

# Load raw data
sp500 <- read_csv("data/sp500_raw.csv")
fred_data <- read_csv("data/fred_raw.csv")

# Print data dimensions for debugging
cat("S&P 500 data dimensions:", dim(sp500)[1], "rows,", dim(sp500)[2], "columns\n")
cat("FRED data dimensions:", dim(fred_data)[1], "rows,", dim(fred_data)[2], "columns\n")

# Reshape FRED data to wide format
fred_wide <- fred_data %>%
  pivot_wider(
    id_cols = date,
    names_from = series_id,
    values_from = value
  )

cat("FRED wide format dimensions:", dim(fred_wide)[1], "rows,", dim(fred_wide)[2], "columns\n")

# Calculate monthly S&P 500 returns
sp500_monthly <- sp500 %>%
  mutate(year_month = floor_date(date, "month")) %>%
  group_by(year_month) %>%
  summarize(
    close_price = last(close),
    monthly_return = (last(adjusted)/first(adjusted)) - 1
  )

cat("S&P 500 monthly returns dimensions:", dim(sp500_monthly)[1], "rows\n")

# Create monthly fred data
fred_monthly <- fred_data %>%
  mutate(year_month = floor_date(date, "month")) %>%
  pivot_wider(
    id_cols = year_month,
    names_from = series_id,
    values_from = value,
    values_fn = list(value = ~ last(na.omit(.))),
    values_fill = list(value = NA_real_)
  )

cat("FRED monthly data dimensions:", dim(fred_monthly)[1], "rows\n")

# Merge datasets - fixed to avoid duplicate column names
all_data <- fred_monthly %>%
  left_join(sp500_monthly, by = "year_month") %>%
  rename(date = year_month)

cat("Merged data dimensions:", dim(all_data)[1], "rows\n")

# Convert all columns to appropriate types
all_data <- all_data %>%
  # First ensure the date column is properly formatted
  mutate(date = as.Date(date)) %>%
  # Convert all numeric columns that might be characters to numeric
  mutate(across(-date, ~as.numeric(.)))

# Create derived variables
all_data <- all_data %>%
  arrange(date) %>%
  mutate(
    # Year-over-year percentage changes
    CPI_YoY = (CPIAUCSL / lag(CPIAUCSL, 12) - 1) * 100,
    INDPRO_YoY = (INDPRO / lag(INDPRO, 12) - 1) * 100,
    M2_YoY = (M2SL / lag(M2SL, 12) - 1) * 100,
    
    # Yield spreads
    BAA_AAA_spread = BAA - AAA,
    
    # Create Fed tightening cycle dummy
    tightening = as.numeric(FEDFUNDS > lag(FEDFUNDS))
  )

# Generate NA summary
na_summary <- all_data %>%
  summarise(across(everything(), ~sum(is.na(.))))

print("NA Summary:")
print(na_summary)

# Data cleaning - be less strict with filtering
cleaned_data <- all_data %>%
  # Only filter rows where the target variable is NA
  filter(!is.na(monthly_return)) %>%
  # Handle any remaining NAs with median imputation
  mutate(across(where(is.numeric), ~if_else(is.na(.), median(., na.rm = TRUE), .)))

cat("Cleaned data dimensions:", dim(cleaned_data)[1], "rows\n")

# Verify column types
str(cleaned_data)

# Final check
if(nrow(cleaned_data) == 0) {
  stop("Error: The cleaned_data dataframe is empty. Check the filters and joins.")
}

# Save processed data with type conversion
write_csv(cleaned_data, "data/processed_data.csv")

cat("Data processing complete... Processed data saved to 'data/processed_data.csv'.\n")