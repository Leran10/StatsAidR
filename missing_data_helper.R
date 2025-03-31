# Helper function for analyzing missing data without visualization dependencies
analyze_missing_data <- function(data) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Check if required packages are available
  if (!requireNamespace("naniar", quietly = TRUE)) {
    stop("Package 'naniar' is needed for this function to work.")
  }
  
  # Initialize results
  results <- list()
  
  # Basic missing statistics
  results$missing_summary <- list(
    total_missing = sum(is.na(data)),
    total_values = prod(dim(data)),
    percent_missing = sum(is.na(data)) / prod(dim(data)) * 100,
    rows_missing = sum(apply(data, 1, function(x) any(is.na(x)))),
    rows_complete = sum(complete.cases(data)),
    percent_rows_missing = sum(apply(data, 1, function(x) any(is.na(x)))) / nrow(data) * 100,
    cols_missing = sum(apply(data, 2, function(x) any(is.na(x)))),
    cols_complete = sum(!apply(data, 2, function(x) any(is.na(x)))),
    percent_cols_missing = sum(apply(data, 2, function(x) any(is.na(x)))) / ncol(data) * 100
  )
  
  # Column-wise missing counts and percentages
  results$missing_by_column <- data.frame(
    column = names(data),
    missing = sapply(data, function(x) sum(is.na(x))),
    percent = sapply(data, function(x) mean(is.na(x)) * 100)
  )
  
  # Row-wise missing counts
  missing_by_row <- data.frame(
    row = 1:nrow(data),
    missing = apply(data, 1, function(x) sum(is.na(x))),
    percent = apply(data, 1, function(x) mean(is.na(x)) * 100)
  )
  
  # Summarize row-wise missing
  results$missing_by_row_summary <- list(
    mean_per_row = mean(missing_by_row$missing),
    median_per_row = stats::median(missing_by_row$missing),
    max_per_row = max(missing_by_row$missing),
    distribution = table(missing_by_row$missing)
  )
  
  # Missing patterns
  miss_patterns <- naniar::miss_var_summary(data)
  
  # Missing combinations
  miss_combinations <- naniar::miss_var_table(data)
  
  results$missing_patterns <- miss_patterns
  results$missing_combinations <- miss_combinations
  
  # No plots for this simplified version
  
  # Simple print method to view results without special class handling
  print_missing_analysis <- function(x) {
    cat("Missing Data Analysis\n")
    cat("==========================\n\n")
    
    # Overall missing summary
    cat("Overall Missing Summary:\n")
    cat("  Missing values:", x$missing_summary$total_missing, 
        sprintf("(%.1f%%)", x$missing_summary$percent_missing), "\n")
    cat("  Rows with any missing:", x$missing_summary$rows_missing, 
        sprintf("(%.1f%%)", x$missing_summary$percent_rows_missing), "\n")
    cat("  Columns with any missing:", x$missing_summary$cols_missing, 
        sprintf("(%.1f%%)", x$missing_summary$percent_cols_missing), "\n\n")
    
    # Columns with highest missingness
    if (nrow(x$missing_by_column) > 0) {
      cat("Top columns with missing values:\n")
      top_cols <- head(x$missing_by_column[order(-x$missing_by_column$percent), ], 5)
      print(top_cols, row.names = FALSE)
      cat("\n")
    }
  }
  
  # Assign print function
  results$print <- print_missing_analysis
  
  return(results)
}

# Instructions for use:
cat("To analyze missing data, run this code:\n\n")
cat("source('missing_data_helper.R')\n")
cat("results <- analyze_missing_data(your_data)\n")
cat("results$print(results)  # To view the results\n\n")
cat("# To explore specific parts of the results, try:\n")
cat("results$missing_by_column  # To see missing data by column\n")
cat("results$missing_patterns   # To see patterns of missing data\n")