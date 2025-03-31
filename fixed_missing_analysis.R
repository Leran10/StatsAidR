# StatsAidR - Fixed Missing Data Analysis Function
# This implementation ensures imputation_recommendations are properly set

analyze_missing_data_fixed <- function(data, include_recommendations = TRUE) {
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
  results$missing_patterns <- data.frame(
    variable = names(data),
    n_miss = sapply(data, function(x) sum(is.na(x))),
    pct_miss = sapply(data, function(x) mean(is.na(x)) * 100)
  )
  results$missing_patterns <- results$missing_patterns[order(-results$missing_patterns$pct_miss), ]
  
  # Simple missing mechanism analysis
  miss_mechanism <- list(
    likely_mechanism = "Undetermined (simplified analysis)",
    confidence = "Low",
    recommendation = "Consider using specialized missing data analysis tools."
  )
  
  # Generate imputation recommendations
  imputation_recommendations <- list(
    overall = list(),
    by_column = list(),
    packages = list(
      r = character(),
      python = character()
    )
  )
  
  # Set default overall recommendation
  imputation_recommendations$overall$primary <- "Multiple imputation or context-appropriate method"
  imputation_recommendations$overall$rationale <- "Based on the pattern of missingness in your data."
  imputation_recommendations$overall$caution <- "Always validate imputation results and consider sensitivity analyses."
  
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
  
  # Always set the imputation_recommendations (even if include_recommendations is FALSE)
  results$imputation_recommendations <- imputation_recommendations
  
  # Return results with class for custom printing
  class(results) <- c("statsaid_missing_fixed", "list")
  return(results)
}

# Print method for statsaid_missing_fixed objects
print.statsaid_missing_fixed <- function(x, ...) {
  cat("StatsAidR Missing Data Analysis (Fixed Version)\n")
  cat("==========================================\n\n")
  
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
  cat("  column_name <- names(missing_analysis$missing_by_column[missing_analysis$missing_by_column$missing > 0, \"column\"])[1]\n")
  cat("  cat(missing_analysis$imputation_recommendations$by_column[[column_name]]$implementation$r, sep=\"\\n\")\n")
  
  invisible(x)
}

# Usage instructions
cat("\nTo analyze missing data with guaranteed imputation recommendations, run this code:\n\n")
cat("source('fixed_missing_analysis.R')\n")
cat("missing_analysis <- analyze_missing_data_fixed(your_data)\n\n")
cat("# Access imputation recommendations directly:\n")
cat("recommendations <- missing_analysis$imputation_recommendations\n\n")
cat("# Access column-specific implementation code:\n")
cat("col_name <- names(missing_analysis$missing_by_column[missing_analysis$missing_by_column$missing > 0, \"column\"])[1]\n")
cat("cat(recommendations$by_column[[col_name]]$implementation$r, sep=\"\\n\")\n")