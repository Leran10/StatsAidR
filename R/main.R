#' StatsAid: A comprehensive toolkit for data exploration, cleaning, and analysis in research
#'
#' StatsAid helps researchers analyze their data with advanced statistical methods and
#' intuitive visualizations. It provides tools for data exploration, missing value analysis,
#' distribution analysis, statistical test selection, effect size analysis, power analysis,
#' and automated reporting.
#'
#' @docType package
#' @name StatsAid
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @importFrom stats cor cor.test median quantile sd var qnorm anova aov as.formula aggregate chisq.test lm na.omit table
#' @importFrom utils head tail
#' @importFrom reshape2 melt
#' @importFrom rlang sym
NULL

#' Analyze missing data patterns
#'
#' This function analyzes missing data patterns in a dataset.
#'
#' @param data A data frame or tibble
#' @param plot Logical value indicating whether to create plots, default is TRUE
#'
#' @return A list containing missing data analysis and optionally plots
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   a = c(1, 2, 3, NA),
#'   b = c("x", "y", NA, "x")
#' )
#' missing_analysis <- analyze_missing_patterns(data)
#' }
analyze_missing_patterns <- function(data, plot = TRUE) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Check if required packages are available
  if (!requireNamespace("naniar", quietly = TRUE) ||
      !requireNamespace("visdat", quietly = TRUE)) {
    stop("Packages 'naniar' and 'visdat' are needed for this function to work.")
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
    median_per_row = median(missing_by_row$missing),
    max_per_row = max(missing_by_row$missing),
    distribution = table(missing_by_row$missing)
  )
  
  # Missing patterns
  miss_patterns <- naniar::miss_var_summary(data)
  
  # Missing combinations
  miss_combinations <- naniar::miss_var_table(data)
  
  results$missing_patterns <- miss_patterns
  results$missing_combinations <- miss_combinations
  
  # Generate plots if requested
  if (plot) {
    results$plots <- list()
    
    # Missing values heatmap
    results$plots$heatmap <- plot_missing_values(data)
    
    # Missing values bar chart
    results$plots$bar <- plot_missing_bar(data)
    
    # Missing patterns visualization
    results$plots$patterns <- visdat::vis_miss(data)
    
    # Missing combinations
    results$plots$combinations <- naniar::gg_miss_upset(data)
  }
  
  # Detect potential MCAR/MAR patterns
  results$missing_mechanism <- test_missing_mechanism(data)
  
  # Return results
  class(results) <- c("statsaid_missing", "list")
  return(results)
}

#' Print method for statsaid_missing objects
#'
#' @param x An object of class statsaid_missing
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_missing <- function(x, ...) {
  cat("StatsAid Missing Data Analysis\n")
  cat("=============================\n\n")
  
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
  
  # Missing mechanism
  if (!is.null(x$missing_mechanism)) {
    cat("Missing Mechanism Analysis:\n")
    cat("  Likely mechanism:", x$missing_mechanism$likely_mechanism, "\n")
    cat("  Confidence:", x$missing_mechanism$confidence, "\n")
    cat("  Recommendation:", x$missing_mechanism$recommendation, "\n\n")
  }
  
  # Plots information
  if (!is.null(x$plots)) {
    cat("Plots available in the 'plots' element of this object.\n")
    cat("Use plot() with specific plot elements for visualization.\n")
  }
  
  invisible(x)
}

#' Test for missing data mechanism (MCAR/MAR/MNAR)
#'
#' This function tests for missing data mechanisms.
#'
#' @param data A data frame or tibble
#'
#' @return A list containing missing mechanism analysis
#' @keywords internal
test_missing_mechanism <- function(data) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Select only numeric columns for testing
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  
  if (ncol(numeric_data) < 2) {
    return(list(
      likely_mechanism = "Undetermined",
      confidence = "Low",
      test_results = NULL,
      recommendation = "Not enough numeric columns for reliable testing. Consider manual inspection."
    ))
  }
  
  # Check if any columns have missing values
  missing_cols <- names(which(sapply(numeric_data, function(x) any(is.na(x)))))
  
  if (length(missing_cols) == 0) {
    return(list(
      likely_mechanism = "No missing values",
      confidence = "High",
      test_results = NULL,
      recommendation = "No imputation needed for numeric variables."
    ))
  }
  
  # Initialize results
  results <- list(
    tests = list(),
    correlations = list()
  )
  
  # Little's MCAR test if naniar is available
  if (requireNamespace("naniar", quietly = TRUE)) {
    tryCatch({
      mcar_test <- naniar::mcar_test(numeric_data)
      results$tests$littles_mcar <- list(
        statistic = mcar_test$statistic,
        df = mcar_test$df,
        p_value = mcar_test$p.value,
        is_mcar = mcar_test$p.value > 0.05
      )
    }, error = function(e) {
      results$tests$littles_mcar <- list(
        error = TRUE,
        message = as.character(e)
      )
    })
  }
  
  # Check for correlations between missingness and observed variables
  # This helps detect potential MAR patterns
  results$correlations <- list()
  
  for (col in missing_cols) {
    # Create missingness indicator
    miss_indicator <- as.numeric(is.na(numeric_data[[col]]))
    
    if (sum(miss_indicator) > 0 && sum(miss_indicator) < length(miss_indicator)) {
      # Test correlations with other variables
      cor_results <- list()
      
      for (other_col in setdiff(names(numeric_data), col)) {
        cor_test <- cor.test(miss_indicator, numeric_data[[other_col]], 
                             use = "pairwise.complete.obs")
        
        cor_results[[other_col]] <- list(
          correlation = cor_test$estimate,
          p_value = cor_test$p.value,
          significant = cor_test$p.value < 0.05
        )
      }
      
      results$correlations[[col]] <- cor_results
    }
  }
  
  # Make a determination based on test results
  mcar_evidence <- FALSE
  mar_evidence <- FALSE
  
  # Check Little's MCAR test
  if (!is.null(results$tests$littles_mcar) && !results$tests$littles_mcar$error) {
    mcar_evidence <- results$tests$littles_mcar$is_mcar
  }
  
  # Check correlations for MAR evidence
  significant_cors <- 0
  total_cors <- 0
  
  for (col in names(results$correlations)) {
    for (other_col in names(results$correlations[[col]])) {
      total_cors <- total_cors + 1
      if (results$correlations[[col]][[other_col]]$significant) {
        significant_cors <- significant_cors + 1
      }
    }
  }
  
  if (total_cors > 0 && significant_cors / total_cors > 0.1) {
    mar_evidence <- TRUE
  }
  
  # Determine likely mechanism
  if (mcar_evidence && !mar_evidence) {
    mechanism <- "MCAR (Missing Completely At Random)"
    confidence <- "Moderate"
    recommendation <- "You can use complete case analysis or any imputation method."
  } else if (mar_evidence) {
    mechanism <- "MAR (Missing At Random)"
    confidence <- "Moderate"
    recommendation <- "Consider multiple imputation or methods that account for missing data patterns."
  } else {
    mechanism <- "MNAR (Missing Not At Random) or MAR"
    confidence <- "Low"
    recommendation <- "Examine the data collection process. Consider pattern mixture models or sensitivity analysis."
  }
  
  # Return results
  list(
    likely_mechanism = mechanism,
    confidence = confidence,
    test_results = results,
    recommendation = recommendation
  )
}

#' Test normality of variables
#'
#' This function tests the normality of variables in a dataset.
#'
#' @param data A data frame or tibble
#' @param columns Character vector specifying the columns to test, default is NULL (all numeric columns)
#' @param tests Character vector specifying the tests to perform, options are "shapiro", "ks", "ad", "jb"
#'
#' @return A list containing normality test results
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   a = rnorm(100),
#'   b = rexp(100)
#' )
#' test_normality(data)
#' }
test_normality <- function(data, columns = NULL, 
                          tests = c("shapiro", "jb")) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Select columns to test
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  } else {
    # Check if specified columns exist and are numeric
    non_numeric <- columns[!columns %in% names(data)[sapply(data, is.numeric)]]
    if (length(non_numeric) > 0) {
      warning("Some specified columns are not numeric or don't exist: ", 
              paste(non_numeric, collapse = ", "))
      columns <- setdiff(columns, non_numeric)
    }
  }
  
  if (length(columns) == 0) {
    stop("No numeric columns available for testing")
  }
  
  # Check for required packages
  if ("jb" %in% tests && !requireNamespace("moments", quietly = TRUE)) {
    warning("Package 'moments' needed for Jarque-Bera test. Skipping this test.")
    tests <- setdiff(tests, "jb")
  }
  
  if ("ad" %in% tests && !requireNamespace("nortest", quietly = TRUE)) {
    warning("Package 'nortest' needed for Anderson-Darling test. Skipping this test.")
    tests <- setdiff(tests, "ad")
  }
  
  # Initialize results
  results <- list()
  
  # Run tests for each column
  for (col in columns) {
    # Skip if too many missing values
    if (mean(is.na(data[[col]])) > 0.5) {
      results[[col]] <- list(
        skipped = TRUE,
        reason = "Too many missing values"
      )
      next
    }
    
    # Get complete cases
    x <- data[[col]][!is.na(data[[col]])]
    
    # Skip if too few values
    if (length(x) < 3) {
      results[[col]] <- list(
        skipped = TRUE,
        reason = "Too few values"
      )
      next
    }
    
    # Initialize column results
    col_results <- list(
      n = length(x),
      mean = mean(x),
      median = median(x),
      sd = sd(x),
      skewness = NA,
      kurtosis = NA,
      tests = list()
    )
    
    # Calculate skewness and kurtosis if moments package is available
    if (requireNamespace("moments", quietly = TRUE)) {
      col_results$skewness <- moments::skewness(x)
      col_results$kurtosis <- moments::kurtosis(x)
    }
    
    # Run selected tests
    if ("shapiro" %in% tests && length(x) <= 5000) {
      # Shapiro-Wilk test (limited to 5000 observations)
      sw_test <- shapiro.test(x)
      col_results$tests$shapiro <- list(
        statistic = sw_test$statistic,
        p_value = sw_test$p.value,
        normal = sw_test$p.value >= 0.05
      )
    }
    
    if ("ks" %in% tests) {
      # Kolmogorov-Smirnov test
      # Standardizing data for comparison with standard normal
      x_std <- (x - mean(x)) / sd(x)
      ks_test <- suppressWarnings(ks.test(x_std, "pnorm"))
      col_results$tests$ks <- list(
        statistic = ks_test$statistic,
        p_value = ks_test$p.value,
        normal = ks_test$p.value >= 0.05
      )
    }
    
    if ("ad" %in% tests && requireNamespace("nortest", quietly = TRUE)) {
      # Anderson-Darling test
      ad_test <- suppressWarnings(nortest::ad.test(x))
      col_results$tests$ad <- list(
        statistic = ad_test$statistic,
        p_value = ad_test$p.value,
        normal = ad_test$p.value >= 0.05
      )
    }
    
    if ("jb" %in% tests && requireNamespace("moments", quietly = TRUE)) {
      # Jarque-Bera test
      jb_test <- suppressWarnings(moments::jarque.test(x))
      col_results$tests$jb <- list(
        statistic = jb_test$statistic,
        p_value = jb_test$p.value,
        normal = jb_test$p.value >= 0.05
      )
    }
    
    # Overall normality assessment
    normal_results <- sapply(col_results$tests, function(test) test$normal)
    
    if (length(normal_results) > 0) {
      col_results$normal <- mean(normal_results) >= 0.5
      col_results$normal_tests <- sum(normal_results)
      col_results$total_tests <- length(normal_results)
    } else {
      col_results$normal <- NA
      col_results$normal_tests <- 0
      col_results$total_tests <- 0
    }
    
    # Recommended transformation based on skewness
    if (!is.na(col_results$skewness)) {
      if (abs(col_results$skewness) <= 0.5) {
        col_results$transformation <- "None needed"
      } else if (col_results$skewness > 0.5) {
        if (col_results$skewness > 1) {
          if (all(x > 0)) {
            col_results$transformation <- "Log transformation"
          } else {
            col_results$transformation <- "Square root of absolute value"
          }
        } else {
          col_results$transformation <- "Square root transformation"
        }
      } else {  # Negative skewness
        if (col_results$skewness < -1) {
          col_results$transformation <- "Cube transformation"
        } else {
          col_results$transformation <- "Square transformation"
        }
      }
    } else {
      col_results$transformation <- "Unable to determine"
    }
    
    # Store column results
    results[[col]] <- col_results
  }
  
  # Return results
  class(results) <- c("statsaid_normality", "list")
  return(results)
}

#' Print method for statsaid_normality objects
#'
#' @param x An object of class statsaid_normality
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_normality <- function(x, ...) {
  cat("StatsAid Normality Test Results\n")
  cat("===============================\n\n")
  
  for (col in names(x)) {
    cat("Variable:", col, "\n")
    
    if (!is.null(x[[col]]$skipped) && x[[col]]$skipped) {
      cat("  Skipped:", x[[col]]$reason, "\n\n")
      next
    }
    
    # Print basic statistics
    cat("  n =", x[[col]]$n, "\n")
    cat("  Mean =", round(x[[col]]$mean, 3), "\n")
    cat("  Median =", round(x[[col]]$median, 3), "\n")
    cat("  SD =", round(x[[col]]$sd, 3), "\n")
    
    if (!is.na(x[[col]]$skewness)) {
      cat("  Skewness =", round(x[[col]]$skewness, 3), "\n")
    }
    
    if (!is.na(x[[col]]$kurtosis)) {
      cat("  Kurtosis =", round(x[[col]]$kurtosis, 3), "\n")
    }
    
    # Print test results
    if (length(x[[col]]$tests) > 0) {
      cat("  Test Results:\n")
      
      for (test in names(x[[col]]$tests)) {
        result <- x[[col]]$tests[[test]]
        cat("    ", toupper(test), ": p =", 
            sprintf("%.4f", result$p_value), 
            ifelse(result$normal, "(Normal)", "(Non-normal)"), "\n")
      }
    }
    
    # Print overall assessment
    if (!is.na(x[[col]]$normal)) {
      normal_str <- ifelse(x[[col]]$normal, "Likely Normal", "Likely Non-normal")
      cat("  Overall:", normal_str, "(", x[[col]]$normal_tests, "of", 
          x[[col]]$total_tests, "tests indicate normality)", "\n")
    }
    
    # Print transformation recommendation
    if (!is.null(x[[col]]$transformation)) {
      cat("  Recommended transformation:", x[[col]]$transformation, "\n")
    }
    
    cat("\n")
  }
  
  invisible(x)
}

#' Create a complete analysis report for a dataset
#'
#' This function performs a comprehensive analysis of a dataset and generates a report.
#'
#' @param data A data frame or tibble
#' @param title Character string specifying the report title
#' @param file Character string specifying the output file (HTML or PDF)
#' @param include_code Logical value indicating whether to include R code in the report
#'
#' @return Invisibly returns TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   age = c(25, 30, 35, 40, NA),
#'   weight = c(70, 75, NA, 85, 90),
#'   sex = c("M", "F", "M", "F", "M")
#' )
#' create_report(data, title = "My Dataset Analysis", file = "report.html")
#' }
create_report <- function(data, title = "Data Analysis Report", 
                          file = "report.html", include_code = FALSE) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Check if required packages are available
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is needed for this function to work.")
  }
  
  # Create temporary Rmd file
  tmp_rmd <- tempfile(fileext = ".Rmd")
  
  # Write Rmd content
  cat("---\n", file = tmp_rmd)
  cat(paste0("title: \"", title, "\"\n"), file = tmp_rmd, append = TRUE)
  cat("date: \"`r Sys.Date()`\"\n", file = tmp_rmd, append = TRUE)
  cat("output: html_document\n", file = tmp_rmd, append = TRUE)
  cat("---\n\n", file = tmp_rmd, append = TRUE)
  
  # Setup chunk options
  cat("```{r setup, include=FALSE}\n", file = tmp_rmd, append = TRUE)
  cat(paste0("knitr::opts_chunk$set(echo = ", tolower(include_code), ")\n"), 
      file = tmp_rmd, append = TRUE)
  cat("library(StatsAid)\n", file = tmp_rmd, append = TRUE)
  cat("library(ggplot2)\n", file = tmp_rmd, append = TRUE)
  cat("library(dplyr)\n", file = tmp_rmd, append = TRUE)
  cat("library(knitr)\n", file = tmp_rmd, append = TRUE)
  cat("```\n\n", file = tmp_rmd, append = TRUE)
  
  # Save data as object in environment
  data_obj_name <- "analysis_data"
  assign(data_obj_name, data, envir = .GlobalEnv)
  
  # Data Overview section
  cat("## Data Overview\n\n", file = tmp_rmd, append = TRUE)
  cat("```{r data-overview}\n", file = tmp_rmd, append = TRUE)
  cat(paste0("data_overview <- explore(", data_obj_name, ")\n"), file = tmp_rmd, append = TRUE)
  cat("print(data_overview)\n", file = tmp_rmd, append = TRUE)
  cat("```\n\n", file = tmp_rmd, append = TRUE)
  
  # Dataset dimensions
  cat("The dataset contains ", nrow(data), " observations and ", ncol(data), 
      " variables.\n\n", file = tmp_rmd, append = TRUE)
  
  # Variable types summary
  cat("### Variable Types\n\n", file = tmp_rmd, append = TRUE)
  cat("```{r var-types}\n", file = tmp_rmd, append = TRUE)
  cat("# Variable types count\n", file = tmp_rmd, append = TRUE)
  cat("var_types <- c(\n", file = tmp_rmd, append = TRUE)
  cat("  Numeric = length(data_overview$variable_types$numeric),\n", file = tmp_rmd, append = TRUE)
  cat("  Categorical = length(data_overview$variable_types$categorical),\n", file = tmp_rmd, append = TRUE)
  cat("  Date = length(data_overview$variable_types$date),\n", file = tmp_rmd, append = TRUE)
  cat("  Logical = length(data_overview$variable_types$logical)\n", file = tmp_rmd, append = TRUE)
  cat(")\n", file = tmp_rmd, append = TRUE)
  cat("kable(data.frame(Type = names(var_types), Count = var_types))\n", file = tmp_rmd, append = TRUE)
  cat("```\n\n", file = tmp_rmd, append = TRUE)
  
  # Missing Data section
  cat("## Missing Data Analysis\n\n", file = tmp_rmd, append = TRUE)
  cat("```{r missing-data}\n", file = tmp_rmd, append = TRUE)
  cat(paste0("missing_analysis <- analyze_missing_patterns(", data_obj_name, ")\n"), 
      file = tmp_rmd, append = TRUE)
  cat("print(missing_analysis)\n", file = tmp_rmd, append = TRUE)
  cat("```\n\n", file = tmp_rmd, append = TRUE)
  
  # Missing data visualizations
  cat("### Missing Data Visualizations\n\n", file = tmp_rmd, append = TRUE)
  cat("```{r missing-viz, fig.width=10, fig.height=6}\n", file = tmp_rmd, append = TRUE)
  cat("if (!is.null(missing_analysis$plots$heatmap)) print(missing_analysis$plots$heatmap)\n", 
      file = tmp_rmd, append = TRUE)
  cat("if (!is.null(missing_analysis$plots$bar)) print(missing_analysis$plots$bar)\n", 
      file = tmp_rmd, append = TRUE)
  cat("```\n\n", file = tmp_rmd, append = TRUE)
  
  # Distribution Analysis section
  cat("## Distribution Analysis\n\n", file = tmp_rmd, append = TRUE)
  
  # Numeric distributions
  cat("### Numeric Variables\n\n", file = tmp_rmd, append = TRUE)
  cat("```{r numeric-dist, fig.width=10, fig.height=6, warning=FALSE}\n", 
      file = tmp_rmd, append = TRUE)
  cat(paste0("distribution_plots <- plot_distributions(", data_obj_name, ")\n"), 
      file = tmp_rmd, append = TRUE)
  cat("# Plot numeric distributions\n", file = tmp_rmd, append = TRUE)
  cat("if (!is.null(distribution_plots$numeric)) {\n", file = tmp_rmd, append = TRUE)
  cat("  for (i in seq_along(distribution_plots$numeric)) {\n", 
      file = tmp_rmd, append = TRUE)
  cat("    print(distribution_plots$numeric[[i]])\n", file = tmp_rmd, append = TRUE)
  cat("  }\n", file = tmp_rmd, append = TRUE)
  cat("}\n", file = tmp_rmd, append = TRUE)
  cat("```\n\n", file = tmp_rmd, append = TRUE)
  
  # Categorical distributions
  cat("### Categorical Variables\n\n", file = tmp_rmd, append = TRUE)
  cat("```{r cat-dist, fig.width=10, fig.height=6, warning=FALSE}\n", 
      file = tmp_rmd, append = TRUE)
  cat("# Plot categorical distributions\n", file = tmp_rmd, append = TRUE)
  cat("if (!is.null(distribution_plots$categorical)) {\n", file = tmp_rmd, append = TRUE)
  cat("  for (i in seq_along(distribution_plots$categorical)) {\n", 
      file = tmp_rmd, append = TRUE)
  cat("    print(distribution_plots$categorical[[i]])\n", file = tmp_rmd, append = TRUE)
  cat("  }\n", file = tmp_rmd, append = TRUE)
  cat("}\n", file = tmp_rmd, append = TRUE)
  cat("```\n\n", file = tmp_rmd, append = TRUE)
  
  # Normality tests for numeric variables
  cat("### Normality Tests\n\n", file = tmp_rmd, append = TRUE)
  cat("```{r normality}\n", file = tmp_rmd, append = TRUE)
  cat(paste0("normality_results <- test_normality(", data_obj_name, ")\n"), 
      file = tmp_rmd, append = TRUE)
  cat("print(normality_results)\n", file = tmp_rmd, append = TRUE)
  cat("```\n\n", file = tmp_rmd, append = TRUE)
  
  # Outlier detection
  cat("### Outlier Detection\n\n", file = tmp_rmd, append = TRUE)
  cat("```{r outliers, fig.width=10, fig.height=6}\n", file = tmp_rmd, append = TRUE)
  cat(paste0("outlier_plot <- plot_outliers(", data_obj_name, ")\n"), 
      file = tmp_rmd, append = TRUE)
  cat("print(outlier_plot)\n", file = tmp_rmd, append = TRUE)
  cat("```\n\n", file = tmp_rmd, append = TRUE)
  
  # Correlation Analysis section (if applicable)
  cat("## Correlation Analysis\n\n", file = tmp_rmd, append = TRUE)
  cat("```{r correlation, fig.width=10, fig.height=8}\n", file = tmp_rmd, append = TRUE)
  cat(paste0("corr_plot <- plot_correlation_matrix(", data_obj_name, ")\n"), 
      file = tmp_rmd, append = TRUE)
  cat("print(corr_plot)\n", file = tmp_rmd, append = TRUE)
  cat("```\n\n", file = tmp_rmd, append = TRUE)
  
  # Preprocessing Suggestions section
  cat("## Preprocessing Suggestions\n\n", file = tmp_rmd, append = TRUE)
  cat("```{r preprocessing}\n", file = tmp_rmd, append = TRUE)
  cat(paste0("preprocessing_suggestions <- suggest_preprocessing(", data_obj_name, ")\n"), 
      file = tmp_rmd, append = TRUE)
  cat("print(preprocessing_suggestions)\n", file = tmp_rmd, append = TRUE)
  cat("```\n\n", file = tmp_rmd, append = TRUE)
  
  # Render the report
  rmarkdown::render(tmp_rmd, output_file = file, quiet = TRUE)
  
  # Clean up
  unlink(tmp_rmd)
  
  # Return success
  message("Report saved to: ", file)
  invisible(TRUE)
}

#' Suggest appropriate statistical models and packages
#'
#' This function suggests appropriate statistical models and packages based on the dataset,
#' study design, and data characteristics.
#'
#' @param data A data frame or tibble
#' @param study_design Character string specifying the study design. Options include
#'   'case_control', 'cohort', 'cross_sectional', 'longitudinal', 'rct', or NULL.
#' @param outcome_var Optional character string specifying the outcome variable name
#' @param detailed Logical, if TRUE, provides more detailed recommendations
#'
#' @return A list containing suggested models and packages
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   a = c(1, 2, 3, 4),
#'   b = c("x", "y", "z", "x")
#' )
#' suggest_models(data, study_design = "cross_sectional")
#' }
suggest_models <- function(data, study_design = NULL, outcome_var = NULL, detailed = FALSE) {
  # Check if we can call the enhanced version from model_suggestion.R
  if (exists("suggest_models", where = asNamespace("StatsAid"), mode = "function") &&
      !identical(suggest_models, get("suggest_models", envir = parent.frame()))) {
    # Call the enhanced version
    return(get("suggest_models", envir = asNamespace("StatsAid"))(
      data = data, 
      design = study_design,
      outcome_var = outcome_var,
      detailed = detailed
    ))
  }
  
  # Fallback to basic implementation if enhanced version is not available
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Initialize suggestions
  suggestions <- list(
    models = character(),
    packages = list(
      r = character(),
      python = character()
    )
  )
  
  # Get data overview
  overview <- explore(data)
  
  # Basic assumptions
  num_features <- length(overview$variable_types$numeric)
  num_samples <- overview$shape[1]
  has_categorical <- length(overview$variable_types$categorical) > 0
  
  # Default packages
  suggestions$packages$r <- c("stats", "lme4", "ggplot2")
  suggestions$packages$python <- c("scikit-learn", "statsmodels", "scipy")
  
  # Study design specific suggestions
  if (is.null(study_design)) {
    # General suggestions based on data characteristics
    if (num_samples < 30) {
      suggestions$models <- c(suggestions$models, "Non-parametric tests (small sample size)")
      suggestions$packages$r <- c(suggestions$packages$r, "nonpar")
      
    } else {
      suggestions$models <- c(suggestions$models,
                              "Linear regression",
                              "Logistic regression",
                              "Random Forest",
                              "Gradient Boosting")
      suggestions$packages$r <- c(suggestions$packages$r, "randomForest", "gbm", "xgboost")
      suggestions$packages$python <- c(suggestions$packages$python, "xgboost", "lightgbm")
    }
    
  } else if (study_design == "case_control") {
    suggestions$models <- c(suggestions$models,
                            "Logistic regression",
                            "Conditional logistic regression",
                            "Fisher's exact test (small sample size)")
    suggestions$packages$r <- c(suggestions$packages$r, "survival", "epitools", "epiR")
    suggestions$packages$python <- c(suggestions$packages$python, "lifelines")
    
  } else if (study_design == "cohort") {
    suggestions$models <- c(suggestions$models,
                            "Cox proportional hazards",
                            "Kaplan-Meier survival analysis",
                            "Poisson regression",
                            "Negative binomial regression")
    suggestions$packages$r <- c(suggestions$packages$r, "survival", "survminer", "MASS")
    suggestions$packages$python <- c(suggestions$packages$python, "lifelines", "statsmodels.discrete_models")
    
  } else if (study_design == "cross_sectional") {
    suggestions$models <- c(suggestions$models,
                            "Chi-square test of independence",
                            "Fisher's exact test",
                            "Logistic regression",
                            "Multinomial logistic regression")
    suggestions$packages$r <- c(suggestions$packages$r, "vcd", "nnet", "MASS")
    
  } else if (study_design == "longitudinal") {
    suggestions$models <- c(suggestions$models,
                            "Mixed effects models",
                            "Generalized estimating equations (GEE)",
                            "Repeated measures ANOVA")
    suggestions$packages$r <- c(suggestions$packages$r, "lme4", "nlme", "geepack")
    suggestions$packages$python <- c(suggestions$packages$python, "statsmodels.genmod.gee")
    
  } else if (study_design == "rct") {
    suggestions$models <- c(suggestions$models,
                            "Independent t-test",
                            "Paired t-test",
                            "Mixed ANOVA",
                            "ANCOVA",
                            "Intention-to-treat analysis")
    suggestions$packages$r <- c(suggestions$packages$r, "car", "emmeans", "effectsize")
  }
  
  # Add suggestions based on data features
  if (has_categorical) {
    if (!"Chi-square test" %in% suggestions$models) {
      suggestions$models <- c(suggestions$models, "Chi-square test")
    }
    
    if (!"Fisher's exact test" %in% suggestions$models) {
      suggestions$models <- c(suggestions$models, "Fisher's exact test (for small cell counts)")
    }
  }
  
  # Handle high-dimensional data
  if (num_features > 100) {
    suggestions$models <- c(suggestions$models,
                            "Principal Component Analysis (PCA)",
                            "LASSO regression",
                            "Ridge regression",
                            "Elastic Net",
                            "Dimensionality reduction techniques")
    suggestions$packages$r <- c(suggestions$packages$r, "glmnet", "caret", "pcaMethods")
    suggestions$packages$python <- c(suggestions$packages$python, "sklearn.decomposition")
  }
  
  # Handle imbalanced data (assuming classification)
  # This is a simplified check - you'd want to actually check the target variable
  if (!is.null(study_design) && study_design == "case_control") {
    suggestions$models <- c(suggestions$models,
                            "SMOTE for class imbalance",
                            "Weighted models",
                            "AUC-ROC as evaluation metric")
    suggestions$packages$r <- c(suggestions$packages$r, "ROSE", "DMwR")
    suggestions$packages$python <- c(suggestions$packages$python, "imbalanced-learn")
  }
  
  # Remove duplicates and sort
  suggestions$models <- sort(unique(suggestions$models))
  suggestions$packages$r <- sort(unique(suggestions$packages$r))
  suggestions$packages$python <- sort(unique(suggestions$packages$python))
  
  # Return suggestions
  class(suggestions) <- c("statsaid_model_suggestions", "list")
  return(suggestions)
}