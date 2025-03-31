# Test script for the fixed missing data analysis implementation

# Load the fixed implementation
source("fixed_missing_analysis.R")

# Create sample data with missing values
create_sample_data <- function() {
  # Set seed for reproducibility
  set.seed(123)
  
  # Create data frame with different types of variables and missing values
  data <- data.frame(
    id = 1:100,
    age = sample(18:80, 100, replace = TRUE),
    income = rnorm(100, 50000, 15000),
    education = sample(c("High School", "Bachelor", "Master", "PhD"), 100, replace = TRUE),
    satisfaction = sample(1:10, 100, replace = TRUE),
    region = sample(c("North", "South", "East", "West"), 100, replace = TRUE)
  )
  
  # Introduce missing values
  # 5% missing in age
  data$age[sample(1:100, 5)] <- NA
  
  # 15% missing in income
  data$income[sample(1:100, 15)] <- NA
  
  # 10% missing in education
  data$education[sample(1:100, 10)] <- NA
  
  # 25% missing in satisfaction
  data$satisfaction[sample(1:100, 25)] <- NA
  
  return(data)
}

# Create sample data
sample_data <- create_sample_data()

# Display basic info about the data
cat("Sample data dimensions:", dim(sample_data)[1], "rows,", dim(sample_data)[2], "columns\n")
cat("Column names:", paste(names(sample_data), collapse=", "), "\n\n")

# Run the fixed missing data analysis
missing_analysis <- analyze_missing_data_fixed(sample_data)

# Display the analysis results
print(missing_analysis)

# Verify imputation_recommendations is present and populated
cat("\nVerifying imputation_recommendations is present and correctly populated:\n")
cat("- overall recommendations present:", !is.null(missing_analysis$imputation_recommendations$overall), "\n")
cat("- by_column recommendations present:", !is.null(missing_analysis$imputation_recommendations$by_column), "\n")
cat("- packages recommendations present:", !is.null(missing_analysis$imputation_recommendations$packages), "\n")

# Demonstrate accessing column-specific implementation code
cat("\nDemonstrating access to column-specific implementation code:\n")
col_name <- names(missing_analysis$missing_by_column[missing_analysis$missing_by_column$missing > 0, "column"])[1]
cat("Implementation code for", col_name, ":\n")
cat("```\n")
cat(missing_analysis$imputation_recommendations$by_column[[col_name]]$implementation$r, sep="\n")
cat("```\n")