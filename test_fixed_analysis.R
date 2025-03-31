# Test script for the fixed missing data analysis

# Create sample data with missing values
set.seed(123)
  
# Create data frame with different types of variables and missing values
sample_data <- data.frame(
  id = 1:100,
  age = sample(18:80, 100, replace = TRUE),
  income = rnorm(100, 50000, 15000),
  education = sample(c("High School", "Bachelor", "Master", "PhD"), 100, replace = TRUE),
  satisfaction = sample(1:10, 100, replace = TRUE),
  region = sample(c("North", "South", "East", "West"), 100, replace = TRUE)
)

# Introduce missing values
# 5% missing in age
sample_data$age[sample(1:100, 5)] <- NA

# 15% missing in income
sample_data$income[sample(1:100, 15)] <- NA

# 10% missing in education
sample_data$education[sample(1:100, 10)] <- NA

# 25% missing in satisfaction
sample_data$satisfaction[sample(1:100, 25)] <- NA

# Source the fixed implementation (or use the package function if installed)
# If using as package: library(StatsAidR)
source("R/missing_data_analysis.R")

# Run the fixed missing data analysis
missing_analysis <- analyze_missing_data(sample_data)

# Display the analysis results
print(missing_analysis)

# Verify imputation_recommendations is present and populated
cat("\n=== Verification of imputation_recommendations ===\n")
cat("- overall recommendations present:", !is.null(missing_analysis$imputation_recommendations$overall), "\n")
cat("- by_column recommendations present:", !is.null(missing_analysis$imputation_recommendations$by_column), "\n")
cat("- packages recommendations present:", !is.null(missing_analysis$imputation_recommendations$packages), "\n")

# Demonstrate accessing column-specific implementation code
cat("\n=== Column-specific implementation example ===\n")
# Get first column with missing values
col_name <- missing_analysis$missing_by_column$column[which(missing_analysis$missing_by_column$missing > 0)[1]]
cat("Implementation code for", col_name, ":\n")
cat("```r\n")
cat(missing_analysis$imputation_recommendations$by_column[[col_name]]$implementation$r, sep="\n")
cat("```\n")