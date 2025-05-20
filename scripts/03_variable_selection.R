#############################################
# Econ 103 Project - Data Processing
# Team Members: Emil Blaignan, Robert Sellman, Sean Eizadi
#############################################

library(tidyverse)
library(Boruta)
library(corrplot)

# Load processed data
cleaned_data <- read_csv("data/processed_data.csv")

# ======== Boruta Algorithm for Variable Selection =========

# Prepare data for Boruta (exclude target variable, factor variables, and date)
boruta_data <- cleaned_data %>%
  select(-date, -USREC, -tightening) %>% # Remove date and factor variables
  select(where(is.numeric)) # Keep only numeric columns

cols_to_use <- names(boruta_data)

# Make sure target variable is included
if("monthly_return" %in% names(boruta_data) && !("monthly_return" %in% cols_to_use)) {
  cols_to_use <- c(cols_to_use, "monthly_return")
}

boruta_data <- boruta_data[, cols_to_use]

# Check that there are no more NAs (impute if necessary)
if(sum(is.na(boruta_data)) > 0) {
  boruta_data <- boruta_data %>%
    mutate(across(everything(), ~if_else(is.na(.), median(., na.rm = TRUE), .)))
}

# Create formula for Boruta
predictors <- cols_to_use[cols_to_use != "monthly_return"]
formula_str <- paste("monthly_return ~", paste(predictors, collapse = " + "))
boruta_formula <- as.formula(formula_str)

# Run Boruta feature selection
set.seed(123) # For reproducibility
cat("Running Boruta algorithm. This may take a few minutes...\n")
boruta_results <- Boruta(boruta_formula, data = boruta_data, doTrace = 2, maxRuns = 100)

# Create output directory if it doesn't exist
dir.create("output", showWarnings = FALSE)

# Save the Boruta results object
saveRDS(boruta_results, "output/boruta_results.rds")

# Print results
print(boruta_results)

# Get confirmed important variables
important_vars <- getSelectedAttributes(boruta_results, withTentative = FALSE)
cat("\nImportant variables selected by Boruta:\n")
print(important_vars)

# Plot Boruta results
png("output/boruta_plot.png", width = 1200, height = 800)
plot(boruta_results)
dev.off()

# ======== Test Significance of Factor Variables ========

# Test significance of NBER recession indicator
recession_test <- t.test(
  cleaned_data$monthly_return[cleaned_data$USREC == 1],
  cleaned_data$monthly_return[cleaned_data$USREC == 0]
)
print("Recession Indicator T-Test:")
print(recession_test)

# Test significance of Fed tightening dummy
tightening_test <- t.test(
  cleaned_data$monthly_return[cleaned_data$tightening == 1],
  cleaned_data$monthly_return[cleaned_data$tightening == 0]
)
print("Fed Tightening Indicator T-Test:")
print(tightening_test)

# Visualize with box plots
png("output/factor_boxplots.png", width = 1000, height = 500)
par(mfrow = c(1, 2))
boxplot(monthly_return ~ USREC, data = cleaned_data, 
        main = "S&P 500 Returns by Recession Status", 
        xlab = "Recession (1=Yes, 0=No)", ylab = "Monthly Return")
boxplot(monthly_return ~ tightening, data = cleaned_data, 
        main = "S&P 500 Returns by Fed Tightening Status", 
        xlab = "Tightening Cycle (1=Yes, 0=No)", ylab = "Monthly Return")
dev.off()

# Save test results to a text file
sink("output/factor_tests.txt")
cat("Recession Indicator T-Test:\n")
print(recession_test)
cat("\n\nFed Tightening Indicator T-Test:\n")
print(tightening_test)
sink()

# ======== Correlation Analysis ========

# Use important variables from Boruta for correlation analysis
if(length(important_vars) > 0) {
  selected_vars <- c(important_vars, "monthly_return")
  
  # Subset the data to include only selected variables
  selected_data <- boruta_data %>% 
    select(all_of(selected_vars))
  
  # Calculate correlation matrix
  cor_matrix <- cor(selected_data, use = "pairwise.complete.obs")
  
  # Save correlation matrix
  write.csv(cor_matrix, "output/correlation_matrix.csv")
  
  # Plot correlation matrix
  png("output/correlation_plot.png", width = 1000, height = 800, res = 100)
  corrplot(cor_matrix, method = "circle", type = "upper", 
           tl.col = "black", tl.srt = 45, 
           title = "Correlation Matrix of Selected Variables")
  dev.off()
  
  cat("Correlation analysis complete! Results saved to output folder.\n")
} else {
  cat("No important variables were found by Boruta. Skipping correlation analysis.\n")
}

cat("Variable selection process complete\n")