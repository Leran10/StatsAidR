# StatsAid for R

A comprehensive toolkit for data exploration, cleaning, and analysis in research.

## Overview

StatsAid helps researchers analyze their data with advanced statistical methods and intuitive visualizations:

- **Data Exploration**: Automatically analyze distributions, correlations, and patterns
- **Missing Values**: Identify patterns, test mechanisms (MCAR/MAR/MNAR), and compare imputation methods
- **Distribution Analysis**: Test for normality, detect probability distributions, optimize transformations
- **Statistical Test Selection**: Get intelligent recommendations for statistical tests based on your study design
- **Effect Size Analysis**: Calculate and interpret appropriate effect size measures
- **Automated Reporting**: Generate comprehensive reports with insights and visualizations

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("Leran10/StatsAidR")
```

## Quick Start

```r
library(StatsAidR)

# Load your data
data <- read.csv("your_data.csv")

# Get data overview with comprehensive statistics
overview <- explore(data)

# Analyze missing values and patterns
# Option 1 - Full analysis with plots (may have issues in some environments)
missing_analysis <- analyze_missing_patterns(data)

# Option 2 - Analysis without plots (more reliable across all environments)
missing_analysis <- analyze_missing_patterns(data, plot = FALSE)

# Option 3 - If you encounter RStudio errors with the above methods
# Source the standalone helper function
source("https://raw.githubusercontent.com/Leran10/StatsAidR/main/R/missing_data_standalone.R")
missing_analysis <- missing_data_analysis(data)

# Test distributions and find transformations
normality_results <- test_normality(data)

# Get model recommendations
# Option 1 - Basic model suggestions with a simple design type
models <- suggest_models(data, design = "case_control")

# Option 2 - More detailed model suggestions with a design specification object
design_spec <- study_design_spec(
  design_type = "case_control", 
  response_type = "binary"
)
models <- suggest_models(data, design = design_spec, outcome_var = "outcome_column")

# Create visualizations
plots <- plot_distributions(data)
corr_plot <- plot_correlation_matrix(data)

# Generate a complete report
create_report(data, title = "My Dataset Analysis", file = "report.html")
```

## About Recent Improvements

### Robustness with Missing Data and RStudio Compatibility

StatsAidR v0.1.0 and later includes several improvements for handling real-world data:

1. **Robust Correlation Matrix**: The `plot_correlation_matrix()` function now:
   - Automatically filters out columns with excessive missing values
   - Handles correlation tests with insufficient data
   - Provides clear indicators when correlations cannot be computed
   - Customizable thresholds for minimum observations and missing data filtering

2. **Safe RStudio Detection**: The package now uses robust methods to detect whether it's running in RStudio without causing errors.

3. **Error-Tolerant Visualizations**: All visualization functions now have improved error handling.

4. **Alternative Display Options**: New utility functions that work across all environments.

### Troubleshooting Visualization Issues

If you still encounter errors like `Error in rstudio$.rs.isDesktop()` when trying to view plots:

1. Use the `safe_plot()` function to display plots safely:
   ```r
   plots <- plot_distributions(data)
   safe_plot(plots$numeric[[1]])  # View the first numeric plot safely
   ```

2. Use the `view_all_plots()` function to browse through all plots:
   ```r
   plots <- plot_distributions(data)
   view_all_plots(plots)  # View all plots with pauses between them
   view_all_plots(plots, type = "numeric")  # View only numeric plots
   view_all_plots(plots, pause = FALSE)  # View without pauses
   ```

### Troubleshooting Missing Data Analysis

If you encounter errors with `analyze_missing_patterns()`:

1. Use the `analyze_missing_data()` function which is completely standalone and avoids all RStudio-related issues:
   ```r
   # Fully standalone implementation with no RStudio dependencies
   missing_analysis <- analyze_missing_data(data)
   missing_analysis  # Print the results
   ```

2. Alternatively, use `analyze_missing_patterns(data, plot = FALSE)` to skip visualizations

The `analyze_missing_data()` function provides the same core analysis but with:
- No dependency on RStudio detection
- No dependency on visualization packages
- Built-in printing method that doesn't rely on external libraries
- Direct access to missing patterns without RStudio errors

## Key Features

### 1. Enhanced Missing Data Analysis
- Pattern detection and visualization
- MCAR/MAR/MNAR mechanism testing
- Imputation method comparison

### 2. Distribution Analysis
- Multi-test normality assessment
- Automated probability distribution fitting
- Optimal transformation selection

### 3. Advanced Statistical Support
- Study design-specific test selection
- Multiple comparison corrections
- Effect size calculations with interpretation

### 4. Elegant Visualization
- Missing data heatmaps
- Distribution plots with key statistics
- Correlation matrices with significance testing
- Customizable report generation

### 5. Intelligent Model Suggestions
- Automatic data structure analysis
- Clustering detection
- Detailed model recommendations based on data characteristics
- Support for complex study designs
- Interactive model selection guide

## Advanced Model Suggestions

StatsAid now offers enhanced model recommendation capabilities:

```r
# Comprehensive data structure analysis
data_analysis <- analyze_data_structure(data, outcome_var = "outcome")
print(data_analysis)

# Detailed study design specification
design <- study_design_spec(
  design_type = "longitudinal",
  response_type = "continuous",
  clustering = "patient_id",
  repeated_measures = TRUE
)

# Get detailed model recommendations
model_suggestions <- suggest_models(
  data = data,
  design = design,
  outcome_var = "outcome",
  detailed = TRUE
)

# Interactive model selection guide
guide_model_selection(data)
```

The enhanced model suggestions will:

1. Automatically analyze your data's structure
2. Detect clustering and nested relationships
3. Check distributions and dependencies
4. Recommend appropriate models with implementation code
5. Provide diagnostics and alternatives
6. Guide you through model selection interactively

## Function Reference

### Core Analysis Functions

- `explore()`: Generate a comprehensive overview of a dataset
- `suggest_preprocessing()`: Receive preprocessing suggestions based on data
- `analyze_missing_patterns()`: Analyze missing data patterns and mechanisms
- `test_normality()`: Test variables for normality with multiple methods
- `suggest_models()`: Get model recommendations for your study design
- `create_report()`: Generate a comprehensive HTML report

### Visualization Functions

- `plot_missing_values()`: Create visualizations of missing values
- `plot_distributions()`: Create distribution plots for variables
- `plot_correlation_matrix()`: Create correlation matrix heatmaps
- `plot_outliers()`: Create boxplots to detect outliers
- `plot_pairplot()`: Create matrix of pairwise scatter plots

### RStudio-Compatible Utilities

These functions provide robust alternatives that work even in environments where RStudio detection might fail:

- `safe_plot()`: Safely display a plot object, handling potential RStudio errors
- `view_all_plots()`: Browse through all plots from a `plot_distributions()` result safely

### Advanced Model Suggestion Functions

- `analyze_data_structure()`: Comprehensive data structure analysis
- `analyze_distributions()`: Detailed distribution analysis
- `analyze_dependencies()`: Multicollinearity and correlation analysis
- `detect_clusters()`: Identify clustering in your data
- `analyze_outcome()`: Analyze outcome variable characteristics
- `study_design_spec()`: Create detailed study design specifications
- `guide_model_selection()`: Interactive model selection guide

## Working with Plots

### Distribution Plots

The `plot_distributions()` function creates a list of plots that you can explore in several ways:

```r
# Create plots
plots <- plot_distributions(data)

# Option 1: Use standard plotting (may have issues in some environments)
plot(plots$numeric[[1]])  # View first numeric variable

# Option 2: Use the safe_plot function (robust across all environments)
safe_plot(plots$numeric[[1]])  # View first numeric variable
safe_plot(plots$categorical[[2]])  # View second categorical variable

# Option 3: Automatically view all plots with the helper function
view_all_plots(plots)  # View all plots (with prompt between each)
view_all_plots(plots, type = "numeric")  # View only numeric plots
view_all_plots(plots, pause = FALSE)  # View all plots without pausing
```

### Correlation Matrix with Missing Data

When working with datasets containing missing values, you can use the enhanced `plot_correlation_matrix()` function:

```r
# Default usage - will filter out variables with >30% missing values
corr_plot <- plot_correlation_matrix(data)

# Adjust filtering threshold - include variables with at least 40% complete values
corr_plot <- plot_correlation_matrix(data, filter_missing = 0.4)

# Require more complete observation pairs for correlation tests
corr_plot <- plot_correlation_matrix(data, min_obs = 5)

# Use Spearman correlation which is more robust to outliers
corr_plot <- plot_correlation_matrix(data, method = "spearman")
```

## Dependencies

The package relies on several R packages including:

- Core packages: dplyr, ggplot2, tidyr, stats
- Visualization: GGally, corrplot, RColorBrewer
- Missing data: naniar, visdat, mice, VIM
- Statistical modeling: lme4, nlme, MASS, survival
- Reporting: knitr, rmarkdown

## License

MIT