#' Analyze missing data patterns - Standalone version
#'
#' This is a standalone version of the missing data analysis function that
#' doesn't rely on other package functions or visualizations.
#'
#' @param data A data frame or tibble
#'
#' @return A list containing missing data analysis without plots
#' @export
missing_data_analysis <- function(data) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
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
  
  # Try to get missing patterns if naniar is available
  if (requireNamespace("naniar", quietly = TRUE)) {
    tryCatch({
      results$missing_patterns <- naniar::miss_var_summary(data)
      results$missing_combinations <- naniar::miss_var_table(data)
    }, error = function(e) {
      message("Could not generate naniar missing data patterns: ", e$message)
    })
  }
  
  # Print the results
  cat("Missing Data Analysis\n")
  cat("=======================\n\n")
  
  # Overall missing summary
  cat("Overall Missing Summary:\n")
  cat("  Missing values:", results$missing_summary$total_missing, 
      sprintf("(%.1f%%)", results$missing_summary$percent_missing), "\n")
  cat("  Rows with any missing:", results$missing_summary$rows_missing, 
      sprintf("(%.1f%%)", results$missing_summary$percent_rows_missing), "\n")
  cat("  Columns with any missing:", results$missing_summary$cols_missing, 
      sprintf("(%.1f%%)", results$missing_summary$percent_cols_missing), "\n\n")
  
  # Columns with highest missingness
  if (nrow(results$missing_by_column) > 0) {
    cat("Top columns with missing values:\n")
    missing_cols <- results$missing_by_column[results$missing_by_column$missing > 0, ]
    if (nrow(missing_cols) > 0) {
      top_cols <- head(missing_cols[order(-missing_cols$percent), ], 5)
      print(top_cols, row.names = FALSE)
    } else {
      cat("No missing values found in any column\n")
    }
    cat("\n")
  }
  
  # Return the results silently
  invisible(results)
}