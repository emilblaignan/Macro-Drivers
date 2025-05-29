# Macroeconomic Drivers of S&P 500 Returns: A Boruta-Selected Regression Analysis

## Team Members

* Emil Blaignan
* Robert Sellman
* Sean Eizadi

## Project Overview
This project aims to identify key macroeconomic predictors of S&P 500 returns using Boruta feature selection and multiple regression analysis. We analyze various economic indicators (e.g., yield curve, inflation, unemployment) to determine which are most statistically significant in predicting S&P 500 returns, and how factor variables like recession indicators and Fed rate hike cycles improve model fit.

## Setup Instructions

### 1. Prerequisites

* R and RStudio
* FRED API key (obtain from [https://fred.stlouisfed.org/docs/api/api_key.html](https://fred.stlouisfed.org/docs/api/api_key.html))

### 2. Initial Setup

* Clone this repository:
    ```bash
    git clone https://github.com/yourusername/Macro-Drivers.git
    cd Macro-Drivers
    ```
* Set up your FRED API key:
    * Copy `.Renviron.template` to a file named `.Renviron` in your home directory.
    * Edit `.Renviron` and replace `YOUR_KEY_HERE` with your actual FRED API key.
    * Restart RStudio for the changes to take effect.

* Install required packages:
    ```r
    install.packages(c("fredr", "tidyverse", "Boruta", "corrplot", 
                      "zoo", "lubridate", "here", "knitr", "quantmod"))
    ```

### 3. Running the Analysis
Execute the scripts in this order:

* Data acquisition:
    ```r
    source("scripts/01_data_acquisition.R")
    ```
* Data processing:
    ```r
    source("scripts/02_data_processing.R")
    ```
* Variable selection:
    ```r
    source("scripts/03_variable_selection.R")
    ```

### 4. Viewing Results
After running the scripts, you can:

* Knit `report.Rmd` to generate the report.
* View results in the `output` folder, including:
    * Boruta variable importance plot
    * Correlation matrix
    * Factor variable significance tests

## Project Structure

* `data/`: Raw and processed data files
* `scripts/`: R scripts for data acquisition and analysis
* `output/`: Analysis results and visualizations
* `report.Rmd`: R Markdown report for the project
