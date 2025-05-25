#############################################
# Econ 103 Project - Descriptive Analysis
# Team Members: Emil Blaignan, Robert Sellman, Sean Eizadi
#############################################

# Load required packages
library(tidyverse)
library(corrplot)
library(VIM)
library(moments)
library(car)
library(gridExtra)
library(ggplot2)
library(lubridate)

# Load processed data
cleaned_data <- read_csv("data/processed_data.csv")

# Create output directory if it doesn't exist
dir.create("output", showWarnings = FALSE)

# Define selected variables based on sophisticated economic methodology
# Selected using Boruta importance + economic theory + multicollinearity avoidance
selected_vars <- c("VIXCLS", "INDPRO", "AAA", "CPIAUCSL", "FEDFUNDS")
factor_vars <- c("USREC", "tightening")
target_var <- "monthly_return"

# Create working dataset
analysis_data <- cleaned_data %>%
  select(date, all_of(selected_vars), all_of(factor_vars), all_of(target_var))

cat("=== DESCRIPTIVE ANALYSIS STARTING ===\n")
cat("Dataset dimensions:", dim(analysis_data), "\n")

# ======== 1. DATA QUALITY ASSESSMENT ========

# Check for missing values
cat("\n=== MISSING VALUES ANALYSIS ===\n")
missing_summary <- analysis_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_pct = (missing_count / nrow(analysis_data)) * 100) %>%
  arrange(desc(missing_count))

print(missing_summary)

# Determine missing data statement
total_missing <- sum(missing_summary$missing_count)
if(total_missing == 0) {
  missing_statement <- "no missing values"
} else {
  missing_pct <- (total_missing / (nrow(analysis_data) * ncol(analysis_data))) * 100
  missing_statement <- sprintf("%.1f%% missing values", missing_pct)
}

cat("Missing Data Handling: Our preprocessed dataset contains", missing_statement, "\n")

# ======== 2. OUTLIER DETECTION AND TREATMENT ========

cat("\n=== OUTLIER ANALYSIS ===\n")

# Function to detect outliers using IQR method
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  return(x < lower | x > upper)
}

# Outlier summary for all numeric variables
outlier_summary <- analysis_data %>%
  select(all_of(c(selected_vars, target_var))) %>%
  summarise(across(everything(), ~sum(detect_outliers(.), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "outlier_count") %>%
  mutate(outlier_pct = (outlier_count / nrow(analysis_data)) * 100) %>%
  arrange(desc(outlier_count))

print(outlier_summary)

# Winsorize S&P 500 returns (top/bottom 1% as per proposal)
analysis_data <- analysis_data %>%
  mutate(
    monthly_return_original = monthly_return,
    monthly_return = case_when(
      monthly_return > quantile(monthly_return, 0.99, na.rm = TRUE) ~ 
        quantile(monthly_return, 0.99, na.rm = TRUE),
      monthly_return < quantile(monthly_return, 0.01, na.rm = TRUE) ~ 
        quantile(monthly_return, 0.01, na.rm = TRUE),
      TRUE ~ monthly_return
    )
  )

cat("Winsorization applied to monthly_return at 1% and 99% percentiles\n")

# ======== 3. UNIVARIATE ANALYSIS ========

cat("\n=== UNIVARIATE ANALYSIS ===\n")

# Summary statistics for all numeric variables
numeric_vars <- c(selected_vars, target_var)
summary_stats <- analysis_data %>%
  select(all_of(numeric_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    skewness = moments::skewness(value, na.rm = TRUE),
    kurtosis = moments::kurtosis(value, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)

# Save summary statistics
write_csv(summary_stats, "output/summary_statistics.csv")

# ======== 4. DISTRIBUTION ANALYSIS ========

# Create histograms with density curves and fitted normal distributions
create_histogram_plots <- function(data, vars) {
  plots <- list()
  
  for(var in vars) {
    p <- ggplot(data, aes_string(x = var)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", 
                     alpha = 0.7, color = "black") +
      geom_density(color = "red", size = 1, alpha = 0.8) +
      stat_function(fun = dnorm, 
                    args = list(mean = mean(data[[var]], na.rm = TRUE), 
                                sd = sd(data[[var]], na.rm = TRUE)),
                    color = "blue", size = 1, linetype = "dashed") +
      labs(title = paste("Distribution of", var),
           subtitle = "Red = Actual Density, Blue Dashed = Normal Distribution",
           x = var, y = "Density") +
      theme_minimal()
    
    plots[[var]] <- p
  }
  
  return(plots)
}

# Generate histogram plots
hist_plots <- create_histogram_plots(analysis_data, numeric_vars)

# Save histogram plots
png("output/histograms_page1.png", width = 1400, height = 1000)
do.call(grid.arrange, c(hist_plots[1:3], ncol = 2))
dev.off()

png("output/histograms_page2.png", width = 1400, height = 1000)
do.call(grid.arrange, c(hist_plots[4:6], ncol = 2))
dev.off()

# ======== 5. NORMALITY TESTING ========

cat("\n=== NORMALITY TESTING ===\n")

# QQ-plots for normality assessment
create_qq_plots <- function(data, vars) {
  plots <- list()
  
  for(var in vars) {
    p <- ggplot(data, aes_string(sample = var)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(title = paste("Q-Q Plot:", var),
           subtitle = "Points should follow red line for normality") +
      theme_minimal()
    
    plots[[var]] <- p
  }
  
  return(plots)
}

# Generate QQ plots
qq_plots <- create_qq_plots(analysis_data, numeric_vars)

# Save QQ plots
png("output/qq_plots_page1.png", width = 1400, height = 1000)
do.call(grid.arrange, c(qq_plots[1:3], ncol = 2))
dev.off()

png("output/qq_plots_page2.png", width = 1400, height = 1000)
do.call(grid.arrange, c(qq_plots[4:6], ncol = 2))
dev.off()

# ======== 6. TRANSFORMATION ANALYSIS ========

cat("\n=== TRANSFORMATION ANALYSIS ===\n")

# Identify variables requiring transformation based on skewness
transform_candidates <- summary_stats %>%
  filter(abs(skewness) > 0.5, variable != "monthly_return") %>%
  pull(variable)

cat("Variables with |skewness| > 0.5 requiring transformation:\n")
print(transform_candidates)

# Test different transformations for right-skewed variables
if(length(transform_candidates) > 0) {
  transformation_comparison <- tibble()
  
  for(var in transform_candidates) {
    original_data <- analysis_data[[var]]
    original_data <- original_data[!is.na(original_data) & original_data > 0]
    
    if(length(original_data) > 0) {
      # Original skewness
      orig_skew <- moments::skewness(original_data, na.rm = TRUE)
      
      # Log transformation
      log_data <- log(original_data)
      log_skew <- moments::skewness(log_data, na.rm = TRUE)
      
      # Square root transformation
      sqrt_data <- sqrt(original_data)
      sqrt_skew <- moments::skewness(sqrt_data, na.rm = TRUE)
      
      # Determine best transformation
      best_transform <- case_when(
        abs(log_skew) < abs(orig_skew) & abs(log_skew) < abs(sqrt_skew) ~ "log",
        abs(sqrt_skew) < abs(orig_skew) & abs(sqrt_skew) < abs(log_skew) ~ "sqrt",
        TRUE ~ "none"
      )
      
      # Store results
      transformation_comparison <- bind_rows(
        transformation_comparison,
        tibble(
          variable = var,
          original_skewness = orig_skew,
          log_skewness = log_skew,
          sqrt_skewness = sqrt_skew,
          best_transformation = best_transform
        )
      )
    }
  }
  
  print("Transformation Analysis:")
  print(transformation_comparison)
  write_csv(transformation_comparison, "output/transformation_analysis.csv")
  
  # Create transformation recommendation text
  if(nrow(transformation_comparison) > 0) {
    log_vars <- transformation_comparison$variable[transformation_comparison$best_transformation == "log"]
    sqrt_vars <- transformation_comparison$variable[transformation_comparison$best_transformation == "sqrt"]
    
    transformation_text <- ""
    if(length(log_vars) > 0) {
      transformation_text <- paste(transformation_text, 
                                   paste(log_vars, collapse = " and "), 
                                   ifelse(length(log_vars) > 1, "require", "requires"), 
                                   "log transformation")
    }
    if(length(sqrt_vars) > 0) {
      if(transformation_text != "") transformation_text <- paste(transformation_text, "and")
      transformation_text <- paste(transformation_text, 
                                   paste(sqrt_vars, collapse = " and "), 
                                   ifelse(length(sqrt_vars) > 1, "require", "requires"), 
                                   "square root transformation")
    }
  }
} else {
  transformation_text <- "No variables require transformation based on skewness analysis"
}

# ======== 7. CORRELATION ANALYSIS ========

cat("\n=== CORRELATION ANALYSIS ===\n")

# Calculate correlation matrix for numeric variables
cor_matrix <- analysis_data %>%
  select(all_of(numeric_vars)) %>%
  cor(use = "pairwise.complete.obs")

print("Correlation Matrix:")
print(round(cor_matrix, 3))

# Save correlation matrix
write_csv(as.data.frame(cor_matrix), "output/correlation_matrix.csv")

# Create correlation heatmap
png("output/correlation_heatmap.png", width = 1000, height = 800)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix of Selected Variables",
         addCoef.col = "black", number.cex = 0.8,
         mar = c(0,0,2,0))
dev.off()

# Identify high correlations (potential multicollinearity issues)
high_correlations <- expand_grid(
  var1 = rownames(cor_matrix),
  var2 = colnames(cor_matrix)
) %>%
  filter(var1 != var2) %>%
  mutate(correlation = map2_dbl(var1, var2, ~cor_matrix[.x, .y])) %>%
  filter(abs(correlation) > 0.7) %>%
  arrange(desc(abs(correlation)))

# Get correlation with target variable
target_correlations <- cor_matrix[, "monthly_return"]
strongest_predictor <- names(target_correlations)[which.max(abs(target_correlations[names(target_correlations) != "monthly_return"]))]
strongest_correlation <- target_correlations[strongest_predictor]

if(nrow(high_correlations) > 0) {
  cat("High correlations (|r| > 0.7) detected:\n")
  print(high_correlations)
  multicollinearity_statement <- sprintf("High correlation detected requiring attention in model building.")
} else {
  cat("No problematic multicollinearity (|r| > 0.7) detected among predictors.\n")
  multicollinearity_statement <- "No severe multicollinearity issues detected among predictors, confirming our selection strategy."
}

# ======== 8. FACTOR VARIABLE ANALYSIS ========

cat("\n=== FACTOR VARIABLE ANALYSIS ===\n")

# Summary of factor variables
factor_summary <- analysis_data %>%
  select(all_of(factor_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = (count / sum(count)) * 100)

print("Factor Variable Summary:")
print(factor_summary)

# Box plots for factor variables vs target
factor_boxplots <- list()

for(var in factor_vars) {
  p <- ggplot(analysis_data, aes_string(x = paste0("factor(", var, ")"), y = target_var)) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = paste("S&P 500 Returns by", var),
         x = var, y = "Monthly Return") +
    theme_minimal()
  
  factor_boxplots[[var]] <- p
}

# Save factor variable plots
png("output/factor_boxplots.png", width = 1200, height = 600)
do.call(grid.arrange, c(factor_boxplots, ncol = 2))
dev.off()

# ======== 9. TIME SERIES PLOTS ========

cat("\n=== TIME SERIES VISUALIZATION ===\n")

# Create time series plots for all variables
ts_data_long <- analysis_data %>%
  select(date, all_of(numeric_vars)) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value")

# Plot target variable over time
p_target <- ggplot(analysis_data, aes(x = date, y = monthly_return)) +
  geom_line(color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "S&P 500 Monthly Returns Over Time",
       x = "Date", y = "Monthly Return") +
  theme_minimal()

ggsave("output/target_timeseries.png", p_target, width = 12, height = 6)

# Plot predictors over time (normalized for comparison)
ts_data_normalized <- analysis_data %>%
  select(date, all_of(selected_vars)) %>%
  mutate(across(-date, ~scale(.)[,1])) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value")

p_predictors <- ggplot(ts_data_normalized, aes(x = date, y = value, color = variable)) +
  geom_line(alpha = 0.7) +
  labs(title = "Normalized Predictor Variables Over Time",
       x = "Date", y = "Normalized Value") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/predictors_timeseries.png", p_predictors, width = 12, height = 8)

# ======== 10. FINAL DATA PREPARATION ========

# Create final analysis dataset with any necessary transformations
final_data <- analysis_data

# Apply transformations if recommended
if(exists("transformation_comparison") && nrow(transformation_comparison) > 0) {
  for(i in 1:nrow(transformation_comparison)) {
    var <- transformation_comparison$variable[i]
    transform <- transformation_comparison$best_transformation[i]
    
    if(transform == "log" && all(final_data[[var]] > 0, na.rm = TRUE)) {
      final_data[[paste0(var, "_log")]] <- log(final_data[[var]])
      cat("Applied log transformation to", var, "\n")
    } else if(transform == "sqrt" && all(final_data[[var]] >= 0, na.rm = TRUE)) {
      final_data[[paste0(var, "_sqrt")]] <- sqrt(final_data[[var]])
      cat("Applied sqrt transformation to", var, "\n")
    }
  }
}

# Save final analysis dataset
write_csv(final_data, "data/final_analysis_data.csv")

# ======== 11. SUMMARY REPORT ========

cat("\n=== DESCRIPTIVE ANALYSIS SUMMARY ===\n")
cat("Total observations:", nrow(analysis_data), "\n")
cat("Date range:", min(analysis_data$date), "to", max(analysis_data$date), "\n")
cat("Selected quantitative predictors:", length(selected_vars), "\n")
cat("Factor variables:", length(factor_vars), "\n")

# Create summary report
summary_report <- list(
  data_info = list(
    n_obs = nrow(analysis_data),
    date_range = c(min(analysis_data$date), max(analysis_data$date)),
    variables = list(
      quantitative = selected_vars,
      factors = factor_vars,
      target = target_var
    )
  ),
  missing_values = missing_summary,
  outliers = outlier_summary,
  summary_stats = summary_stats,
  correlations = high_correlations,
  transformations = if(exists("transformation_comparison")) transformation_comparison else NULL,
  factor_summary = factor_summary
)

# Save summary report as RDS
saveRDS(summary_report, "output/descriptive_summary.rds")

cat("\n=== DESCRIPTIVE ANALYSIS COMPLETE ===\n")
cat("All outputs saved to 'output/' directory\n")
cat("Key files created:\n")
cat("- summary_statistics.csv\n")
cat("- correlation_heatmap.png\n")
cat("- histograms_page1.png & histograms_page2.png\n")
cat("- qq_plots_page1.png & qq_plots_page2.png\n")
cat("- factor_boxplots.png\n")
cat("- target_timeseries.png\n")
cat("- predictors_timeseries.png\n")
cat("- final_analysis_data.csv\n")

# Print next steps recommendations
cat("\n=== NEXT STEPS RECOMMENDATIONS ===\n")
cat("1. Review all plots for distributional assumptions\n")
cat("2. Consider transformations for highly skewed variables\n")
cat("3. Address any multicollinearity issues found\n")
cat("4. Proceed to model building with cleaned dataset\n")