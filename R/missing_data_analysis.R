#' Analyze missing data patterns with imputation recommendations
#'
#' This function analyzes missing data patterns in a dataset and provides imputation recommendations.
#'
#' @param data A data frame or tibble
#' @param include_recommendations Logical, whether to include imputation recommendations (default: TRUE)
#'
#' @return A list containing missing data analysis and imputation recommendations
#' @export
analyze_missing_data <- function(data, include_recommendations = TRUE) {
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
  
  # Create a simple version of missing patterns
  results$missing_patterns <- data.frame(
    variable = names(data),
    n_miss = sapply(data, function(x) sum(is.na(x))),
    pct_miss = sapply(data, function(x) mean(is.na(x)) * 100)
  )
  results$missing_patterns <- results$missing_patterns[order(-results$missing_patterns$pct_miss), ]
  
  # Simple missing mechanism analysis
  miss_mechanism <- analyze_missing_mechanism_simple(data)
  results$missing_mechanism <- miss_mechanism
  
  # Generate imputation recommendations
  # Initialize recommendations structure
  imputation_recommendations <- list(
    overall = list(),
    by_column = list(),
    packages = list(
      r = character(),
      python = character()
    )
  )
  
  # Overall recommendation based on missing mechanism
  if (miss_mechanism$likely_mechanism == "MCAR (Missing Completely At Random)") {
    imputation_recommendations$overall$primary <- "Complete case analysis or any imputation method"
    imputation_recommendations$overall$rationale <- "Since data appears to be missing completely at random, most imputation approaches should work well."
    imputation_recommendations$overall$caution <- "Even with MCAR data, imputation can provide better statistical power than complete case analysis."
  } else if (miss_mechanism$likely_mechanism == "MAR (Missing At Random) or MNAR") {
    imputation_recommendations$overall$primary <- "Multiple imputation"
    imputation_recommendations$overall$rationale <- "When data is MAR or potentially MNAR, multiple imputation accounts for uncertainty in the imputed values."
    imputation_recommendations$overall$caution <- "Simple imputation methods may introduce bias with MAR/MNAR data."
  } else {
    imputation_recommendations$overall$primary <- "Explore multiple approaches"
    imputation_recommendations$overall$rationale <- "Since the missing mechanism is undetermined, try multiple approaches and compare results."
    imputation_recommendations$overall$caution <- "Be cautious about conclusions with high rates of missingness."
  }
  
  # Column-specific recommendations
  for (col in names(data)) {
    # Skip if no missing values
    if (sum(is.na(data[[col]])) == 0) {
      next
    }
    
    # Calculate missingness percentage
    miss_pct <- mean(is.na(data[[col]])) * 100
    
    # Base recommendation on data type and missingness rate
    if (is.numeric(data[[col]])) {
      if (miss_pct > 50) {
        imputation_recommendations$by_column[[col]] <- list(
          methods = c("Multiple imputation", "Drop column"),
          rationale = paste0("High missingness (", round(miss_pct, 1), "%) makes imputation less reliable."),
          implementation = list(
            r = "mice::mice(data, method = 'pmm')",
            python = "from sklearn.impute import IterativeImputer\nimputer = IterativeImputer()\nimputed_data = imputer.fit_transform(data)"
          )
        )
      } else if (miss_pct > 20) {
        imputation_recommendations$by_column[[col]] <- list(
          methods = c("Predictive mean matching", "Random forest imputation", "Multiple imputation"),
          rationale = paste0("Moderate missingness (", round(miss_pct, 1), "%)."),
          implementation = list(
            r = "mice::mice(data, method = 'pmm')",
            python = "from sklearn.impute import IterativeImputer\nimputer = IterativeImputer()\nimputed_data = imputer.fit_transform(data)"
          )
        )
      } else {
        imputation_recommendations$by_column[[col]] <- list(
          methods = c("Mean/median imputation", "K-NN imputation", "Predictive mean matching"),
          rationale = paste0("Low missingness (", round(miss_pct, 1), "%)."),
          implementation = list(
            r = c(
              "# Mean imputation",
              paste0("data$", col, "[is.na(data$", col, ")] <- mean(data$", col, ", na.rm = TRUE)"),
              "",
              "# K-NN imputation",
              "library(VIM)",
              "imputed_data <- kNN(data)"
            ),
            python = c(
              "# Mean imputation",
              "from sklearn.impute import SimpleImputer",
              "imputer = SimpleImputer(strategy='mean')",
              "imputed_data = imputer.fit_transform(data)",
              "",
              "# K-NN imputation",
              "from sklearn.impute import KNNImputer",
              "imputer = KNNImputer(n_neighbors=5)",
              "imputed_data = imputer.fit_transform(data)"
            )
          )
        )
      }
    } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
      if (miss_pct > 50) {
        imputation_recommendations$by_column[[col]] <- list(
          methods = c("Multiple imputation", "Drop column"),
          rationale = paste0("High missingness (", round(miss_pct, 1), "%) makes imputation less reliable."),
          implementation = list(
            r = "mice::mice(data, method = 'polyreg')",
            python = "# For categorical data\nfrom sklearn.impute import SimpleImputer\nimputer = SimpleImputer(strategy='most_frequent')"
          )
        )
      } else {
        imputation_recommendations$by_column[[col]] <- list(
          methods = c("Mode imputation", "Multiple imputation for categorical"),
          rationale = paste0("Categorical variable with ", round(miss_pct, 1), "% missing."),
          implementation = list(
            r = c(
              "# Mode imputation",
              paste0("mode_value <- names(sort(table(data$", col, "), decreasing = TRUE))[1]"),
              paste0("data$", col, "[is.na(data$", col, ")] <- mode_value"),
              "",
              "# Multiple imputation",
              "library(mice)",
              "imputed_data <- mice(data, method = 'polyreg')"
            ),
            python = c(
              "# Mode imputation",
              "from sklearn.impute import SimpleImputer",
              "imputer = SimpleImputer(strategy='most_frequent')",
              "imputed_data = imputer.fit_transform(data)"
            )
          )
        )
      }
    }
  }
  
  # Recommend R packages
  imputation_recommendations$packages$r <- c(
    "mice" = "Multiple imputation by chained equations",
    "VIM" = "Visualization and imputation of missing values",
    "missForest" = "Random forest imputation",
    "Amelia" = "Multiple imputation with bootstrapping",
    "missMDA" = "Imputation for continuous/categorical variables using PCA"
  )
  
  # Recommend Python packages
  imputation_recommendations$packages$python <- c(
    "scikit-learn" = "Simple, KNN, and iterative imputation",
    "missingpy" = "Extension to scikit-learn for missing data",
    "fancyimpute" = "Matrix completion and advanced imputation methods",
    "impyute" = "Data imputations for missing values"
  )
  
  # IMPORTANT FIX: Always set the imputation_recommendations (even if include_recommendations is FALSE)
  # This ensures missing_analysis$imputation_recommendations is never NULL
  results$imputation_recommendations <- imputation_recommendations
  
  # Return results with class for custom printing
  class(results) <- c("statsaid_missing", "list")
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

#' Print method for statsaid_missing objects
#'
#' @param x An object of class statsaid_missing
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_missing <- function(x, ...) {
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
  
  # Missing mechanism
  if (!is.null(x$missing_mechanism)) {
    cat("Missing Mechanism Analysis:\n")
    cat("  Likely mechanism:", x$missing_mechanism$likely_mechanism, "\n")
    cat("  Confidence:", x$missing_mechanism$confidence, "\n")
    cat("  Recommendation:", x$missing_mechanism$recommendation, "\n\n")
  }
  
  # Imputation recommendations - Always show this section
  cat("Imputation Recommendations:\n")
  cat("  Overall strategy:", x$imputation_recommendations$overall$primary, "\n")
  cat("  Rationale:", x$imputation_recommendations$overall$rationale, "\n")
  cat("  Caution:", x$imputation_recommendations$overall$caution, "\n\n")
  
  # Column-specific recommendations (limit to top 5)
  if (length(x$imputation_recommendations$by_column) > 0) {
    cat("Column-Specific Imputation Methods (top 5):\n")
    
    # Get top 5 columns with highest missingness
    missing_cols <- x$missing_by_column[x$missing_by_column$missing > 0, ]
    if (nrow(missing_cols) > 0) {
      top_cols <- head(missing_cols[order(-missing_cols$percent), ], 5)
      
      for (col in top_cols$column) {
        if (!is.null(x$imputation_recommendations$by_column[[col]])) {
          cat("  -", col, ":", paste(x$imputation_recommendations$by_column[[col]]$methods[1:2], collapse = " or "), "\n")
          cat("    Rationale:", x$imputation_recommendations$by_column[[col]]$rationale, "\n")
        }
      }
      cat("\n")
    }
  }
  
  # Recommended packages
  cat("Recommended Imputation Packages:\n")
  cat("  R packages:\n")
  for (i in seq_along(x$imputation_recommendations$packages$r)) {
    pkg_name <- names(x$imputation_recommendations$packages$r)[i]
    pkg_desc <- x$imputation_recommendations$packages$r[i]
    cat("    -", pkg_name, ":", pkg_desc, "\n")
  }
  
  cat("\n  Python packages:\n")
  for (i in seq_along(x$imputation_recommendations$packages$python)) {
    pkg_name <- names(x$imputation_recommendations$packages$python)[i]
    pkg_desc <- x$imputation_recommendations$packages$python[i]
    cat("    -", pkg_name, ":", pkg_desc, "\n")
  }
  cat("\n")
  
  cat("To access implementation code for a specific column:\n")
  cat("  # Get first column with missing values\n")
  cat("  col_name <- missing_analysis$missing_by_column$column[which(missing_analysis$missing_by_column$missing > 0)[1]]\n")
  cat("  # Display the implementation code\n")
  cat("  cat(missing_analysis$imputation_recommendations$by_column[[col_name]]$implementation$r, sep=\"\\n\")\n")
  
  invisible(x)
}