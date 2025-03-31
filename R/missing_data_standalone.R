#' Analyze missing data patterns - Completely Standalone Version
#'
#' This is a completely standalone version of the missing data analysis function that
#' doesn't rely on any external packages like naniar or RStudio-specific functionality.
#' This version avoids all potential RStudio detection issues.
#'
#' @param data A data frame or tibble
#'
#' @return A list containing missing data analysis
#' @export
analyze_missing_data <- function(data) {
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
  
  # Create a simple version of missing patterns without dependency on naniar
  # This creates a basic version of what naniar::miss_var_summary would produce
  results$missing_patterns <- data.frame(
    variable = names(data),
    n_miss = sapply(data, function(x) sum(is.na(x))),
    pct_miss = sapply(data, function(x) mean(is.na(x)) * 100)
  )
  results$missing_patterns <- results$missing_patterns[order(-results$missing_patterns$pct_miss), ]
  
  # Create a simple version of missing combinations
  # Count different missing patterns
  pattern_matrix <- is.na(data)
  pattern_strings <- apply(pattern_matrix, 1, function(x) paste(as.integer(x), collapse = ""))
  pattern_table <- table(pattern_strings)
  
  if (length(pattern_table) <= 20) {  # Only process if not too many patterns
    results$missing_combinations <- data.frame(
      pattern = names(pattern_table),
      count = as.numeric(pattern_table),
      percent = as.numeric(pattern_table) / nrow(data) * 100
    )
    results$missing_combinations <- results$missing_combinations[order(-results$missing_combinations$count), ]
  } else {
    results$missing_combinations <- data.frame(
      pattern = "Too many patterns to display",
      count = nrow(data),
      percent = 100
    )
  }
  
  # Simple test for missing mechanism (MCAR vs MAR)
  miss_mechanism <- analyze_missing_mechanism_simple(data)
  results$missing_mechanism <- miss_mechanism
  
  # Return results with class for custom printing
  class(results) <- c("standalone_missing_analysis", "list")
  return(results)
}

#' Analyze missing data mechanism using simplified approach
#'
#' This function analyzes the missing data pattern to determine whether the
#' data is missing completely at random (MCAR), missing at random (MAR),
#' or missing not at random (MNAR).
#'
#' @param data A data frame
#'
#' @return A list with the likely mechanism and recommendations
#' @keywords internal
analyze_missing_mechanism_simple <- function(data) {
  # Only use numeric columns for this analysis
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  
  if (ncol(numeric_data) < 2) {
    return(list(
      likely_mechanism = "Undetermined",
      confidence = "Low",
      recommendation = "Not enough numeric columns for reliable testing. Consider manual inspection."
    ))
  }
  
  # Check for columns with missing values
  missing_cols <- names(which(sapply(numeric_data, function(x) any(is.na(x)))))
  
  if (length(missing_cols) == 0) {
    return(list(
      likely_mechanism = "No missing values in numeric columns",
      confidence = "High",
      recommendation = "No imputation needed for numeric variables."
    ))
  }
  
  # Test for correlations between missingness and other variables
  mar_evidence <- FALSE
  total_cors <- 0
  significant_cors <- 0
  
  for (col in missing_cols) {
    # Create missingness indicator
    miss_indicator <- as.numeric(is.na(numeric_data[[col]]))
    
    if (sum(miss_indicator) > 0 && sum(miss_indicator) < length(miss_indicator)) {
      # Test correlations with other variables
      for (other_col in setdiff(names(numeric_data), col)) {
        # Skip if other column has too many missing values
        if (sum(is.na(numeric_data[[other_col]])) > 0.5 * nrow(numeric_data)) {
          next
        }
        
        # Create complete cases for testing
        complete_data <- !is.na(numeric_data[[other_col]])
        
        if (sum(complete_data) >= 5) {  # Need at least 5 observations
          total_cors <- total_cors + 1
          
          # Check correlation
          tryCatch({
            cor_test <- stats::cor.test(miss_indicator[complete_data], 
                                      numeric_data[[other_col]][complete_data])
            
            if (cor_test$p.value < 0.05) {
              significant_cors <- significant_cors + 1
            }
          }, error = function(e) {
            # If error in cor.test, just continue
          })
        }
      }
    }
  }
  
  # Determine mechanism based on correlations
  if (total_cors == 0) {
    mechanism <- "Undetermined"
    confidence <- "Low"
    recommendation <- "Not enough data for reliable testing. Consider specialized missing data analysis."
  } else if (significant_cors / total_cors < 0.1) {
    mechanism <- "MCAR (Missing Completely At Random)"
    confidence <- "Moderate"
    recommendation <- "You can use complete case analysis or any imputation method."
  } else {
    mechanism <- "MAR (Missing At Random) or MNAR"
    confidence <- "Moderate"
    recommendation <- "Consider multiple imputation or methods that account for missing data patterns."
  }
  
  return(list(
    likely_mechanism = mechanism,
    confidence = confidence,
    recommendation = recommendation
  ))
}

#' Print method for standalone_missing_analysis objects
#'
#' @param x An object of class standalone_missing_analysis
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.standalone_missing_analysis <- function(x, ...) {
  cat("StatsAidR Missing Data Analysis\n")
  cat("=============================\n\n")
  
  # Overall missing summary
  cat("Overall Missing Summary:\n")
  cat("  Missing values:", x$missing_summary$total_missing, 
      sprintf("(%.1f%%)", x$missing_summary$percent_missing), "\n")
  cat("  Rows with any missing:", x$missing_summary$rows_missing, 
      sprintf("(%.1f%%)", x$missing_summary$percent_rows_missing), "\n")
  cat("  Columns with any missing:", x$missing_summary$cols_missing, 
      sprintf("(%.1f%%)", x$missing_summary$percent_cols_missing), "\n\n")
  
  # Missing by column
  if (nrow(x$missing_by_column) > 0) {
    missing_cols <- x$missing_by_column[x$missing_by_column$missing > 0, ]
    if (nrow(missing_cols) > 0) {
      cat("Columns with missing values (sorted by % missing):\n")
      top_cols <- head(missing_cols[order(-missing_cols$percent), ], 10)
      print(top_cols[, c("column", "missing", "percent")], row.names = FALSE)
      
      if (nrow(missing_cols) > 10) {
        cat("... and", nrow(missing_cols) - 10, "more columns with missing values\n")
      }
      cat("\n")
    }
  }
  
  # Missing patterns
  if (!is.null(x$missing_patterns) && nrow(x$missing_patterns) > 0) {
    cat("Missing Patterns (top 10):\n")
    print(head(x$missing_patterns, 10), row.names = FALSE)
    cat("\n")
  }
  
  # Missing mechanism
  if (!is.null(x$missing_mechanism)) {
    cat("Missing Mechanism Analysis:\n")
    cat("  Likely mechanism:", x$missing_mechanism$likely_mechanism, "\n")
    cat("  Confidence:", x$missing_mechanism$confidence, "\n")
    cat("  Recommendation:", x$missing_mechanism$recommendation, "\n\n")
  }
  
  invisible(x)
}

#' Missing Data Analysis Helper (for backwards compatibility)
#'
#' @param data A data frame or tibble
#'
#' @return A list containing missing data analysis
#' @export
missing_data_analysis <- function(data) {
  # Simply call the new function
  analyze_missing_data(data)
}