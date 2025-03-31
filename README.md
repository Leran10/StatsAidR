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

### Troubleshooting

### Missing Data Analysis Issues

If you encounter errors with `analyze_missing_patterns()` related to RStudio functions:

1. Try using `analyze_missing_patterns(data, plot = FALSE)` to skip visualizations
2. Update to the latest version which includes robust RStudio error handling

### Visualization Issues

If you encounter errors like `Error in rstudio$.rs.isDesktop()` when trying to view plots:

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

Key functions include:

- `explore()`: Generate a comprehensive overview of a dataset
- `suggest_preprocessing()`: Receive preprocessing suggestions based on data
- `analyze_missing_patterns()`: Analyze missing data patterns and mechanisms
- `test_normality()`: Test variables for normality with multiple methods
- `plot_missing_values()`: Create visualizations of missing values
- `plot_distributions()`: Create distribution plots for variables
- `plot_correlation_matrix()`: Create correlation matrix heatmaps
- `plot_outliers()`: Create boxplots to detect outliers
- `suggest_models()`: Get model recommendations for your study design
- `create_report()`: Generate a comprehensive HTML report

Advanced model suggestion functions:

- `analyze_data_structure()`: Comprehensive data structure analysis
- `analyze_distributions()`: Detailed distribution analysis
- `analyze_dependencies()`: Multicollinearity and correlation analysis
- `detect_clusters()`: Identify clustering in your data
- `analyze_outcome()`: Analyze outcome variable characteristics
- `study_design_spec()`: Create detailed study design specifications
- `guide_model_selection()`: Interactive model selection guide

## Dependencies

The package relies on several R packages including:

- Core packages: dplyr, ggplot2, tidyr, stats
- Visualization: GGally, corrplot, RColorBrewer
- Missing data: naniar, visdat, mice, VIM
- Statistical modeling: lme4, nlme, MASS, survival
- Reporting: knitr, rmarkdown

## License

MIT