#' Analyze distributions of variables in a dataset
#'
#' This function analyzes the distributions of numeric variables in a dataset,
#' checking for normality, zero-inflation, heavy tails, and detecting possible
#' underlying distributions.
#'
#' @param data A data frame or tibble
#'
#' @return A list containing distribution analysis for each numeric variable
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   normal = rnorm(100),
#'   skewed = rexp(100),
#'   count = rpois(100, 3)
#' )
#' analyze_distributions(data)
#' }
analyze_distributions <- function(data) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Initialize results
  results <- list()
  
  # Analyze numeric variables
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  for (col in numeric_cols) {
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
    
    # Test for normality
    normality <- NA
    if (length(x) <= 5000) {
      # Shapiro-Wilk test (limited to 5000 observations)
      sw_test <- suppressWarnings(shapiro.test(x))
      normality <- sw_test$p.value >= 0.05
    } else {
      # For larger datasets, use Anderson-Darling if available
      if (requireNamespace("nortest", quietly = TRUE)) {
        ad_test <- suppressWarnings(nortest::ad.test(x))
        normality <- ad_test$p.value >= 0.05
      } else {
        # Fall back to visual inspection via skewness/kurtosis
        if (requireNamespace("moments", quietly = TRUE)) {
          skewness <- moments::skewness(x)
          kurtosis <- moments::kurtosis(x)
          normality <- abs(skewness) < 1 && abs(kurtosis - 3) < 1
        }
      }
    }
    
    # Check for zero-inflation (for count data)
    zero_inflated <- FALSE
    if (all(x >= 0) && all(x == floor(x))) {  # Check if counts
      zero_proportion <- mean(x == 0)
      if (zero_proportion > 0.3) {  # Arbitrary threshold
        zero_inflated <- TRUE
      }
    }
    
    # Check for heavy tails
    heavy_tailed <- FALSE
    if (!is.na(normality) && !normality) {
      # Simple check based on quantiles
      q <- quantile(x, probs = c(0.01, 0.05, 0.95, 0.99))
      tail_ratio <- (q[4] - q[3]) / (q[2] - q[1])
      heavy_tailed <- tail_ratio > 3  # Arbitrary threshold
    }
    
    # Calculate basic statistics
    basic_stats <- list(
      mean = mean(x),
      median = median(x),
      sd = stats::sd(x),
      min = min(x),
      max = max(x),
      range = max(x) - min(x),
      iqr = IQR(x)
    )
    
    # Calculate skewness and kurtosis if available
    if (requireNamespace("moments", quietly = TRUE)) {
      basic_stats$skewness <- moments::skewness(x)
      basic_stats$kurtosis <- moments::kurtosis(x)
    }
    
    # Detect possible distributions
    possible_distributions <- detect_distributions(x)
    
    # Store results
    results[[col]] <- list(
      n = length(x),
      normal = normality,
      zero_inflated = zero_inflated,
      heavy_tailed = heavy_tailed,
      stats = basic_stats,
      possible_distributions = possible_distributions
    )
  }
  
  # Add class for printing
  class(results) <- c("statsaid_distribution_analysis", "list")
  return(results)
}

#' Print method for statsaid_distribution_analysis objects
#'
#' @param x An object of class statsaid_distribution_analysis
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_distribution_analysis <- function(x, ...) {
  cat("StatsAid Distribution Analysis\n")
  cat("=============================\n\n")
  
  for (col in names(x)) {
    cat("Variable:", col, "\n")
    
    if (!is.null(x[[col]]$skipped) && x[[col]]$skipped) {
      cat("  Skipped:", x[[col]]$reason, "\n\n")
      next
    }
    
    # Basic stats
    cat("  Sample size:", x[[col]]$n, "\n")
    
    if (!is.null(x[[col]]$stats)) {
      cat("  Mean:", round(x[[col]]$stats$mean, 2), "\n")
      cat("  Median:", round(x[[col]]$stats$median, 2), "\n")
      cat("  SD:", round(x[[col]]$stats$sd, 2), "\n")
      
      if (!is.null(x[[col]]$stats$skewness)) {
        cat("  Skewness:", round(x[[col]]$stats$skewness, 2), 
            if(abs(x[[col]]$stats$skewness) > 1) " (substantial)" else "", "\n")
      }
      
      if (!is.null(x[[col]]$stats$kurtosis)) {
        cat("  Kurtosis:", round(x[[col]]$stats$kurtosis, 2), "\n")
      }
    }
    
    # Distribution characteristics
    if (!is.na(x[[col]]$normal)) {
      cat("  Normal distribution:", if(x[[col]]$normal) "Yes" else "No", "\n")
    }
    
    if (x[[col]]$zero_inflated) {
      cat("  Zero-inflated: Yes\n")
    }
    
    if (x[[col]]$heavy_tailed) {
      cat("  Heavy-tailed: Yes\n")
    }
    
    # Potential distributions
    if (length(x[[col]]$possible_distributions) > 0) {
      cat("  Possible distributions:\n")
      for (dist in names(x[[col]]$possible_distributions)) {
        cat("    -", dist, ":", x[[col]]$possible_distributions[[dist]], "\n")
      }
    }
    
    cat("\n")
  }
  
  invisible(x)
}

#' Detect possible probability distributions for a numeric vector
#'
#' @param x A numeric vector
#'
#' @return A list of possible distributions with confidence levels
#' @keywords internal
detect_distributions <- function(x) {
  # Initialize list of possible distributions
  distributions <- list()
  
  # Check if data might be from common distributions
  
  # Check for binary/binomial
  if (all(x %in% c(0, 1)) || all(x == floor(x) & x >= 0)) {
    if (all(x %in% c(0, 1))) {
      distributions$binary <- "Highly Likely"
    } else {
      # Check variance relative to binomial expectation
      if (var(x) < mean(x) * (1 - mean(x))) {
        distributions$binomial <- "Likely"
      } else {
        distributions$binomial <- "Possible"
      }
    }
  }
  
  # Check for Poisson (count data)
  if (all(x == floor(x) & x >= 0)) {
    if (abs(mean(x) - var(x)) / mean(x) < 0.2) {
      distributions$poisson <- "Likely"
    } else if (var(x) > mean(x)) {
      distributions$negative_binomial <- "Likely"
    }
    
    # Check for zero-inflation
    if (mean(x == 0) > 0.3) {
      distributions$zero_inflated <- "Likely"
    }
  }
  
  # Check for continuous distributions
  if (requireNamespace("moments", quietly = TRUE)) {
    skewness <- moments::skewness(x)
    kurtosis <- moments::kurtosis(x)
    
    if (abs(skewness) < 0.5 && abs(kurtosis - 3) < 1) {
      distributions$normal <- "Likely"
    } else {
      if (min(x) >= 0) {
        # Check for right-skewed distributions
        if (skewness > 1) {
          if (var(x) / mean(x)^2 > 1) {
            distributions$gamma <- "Possible"
          }
          
          if (all(x > 0)) {
            # Test fit of lognormal
            log_x <- log(x)
            if (abs(moments::skewness(log_x)) < 0.5) {
              distributions$lognormal <- "Likely"
            } else {
              distributions$lognormal <- "Possible"
            }
            
            distributions$weibull <- "Possible"
          }
        }
      }
      
      # Check for t-distribution (heavy tails)
      if (kurtosis > 4) {
        distributions$t_distribution <- "Possible"
      }
    }
  }
  
  return(distributions)
}

#' Analyze dependencies and multicollinearity in a dataset
#'
#' This function analyzes dependencies between variables in a dataset,
#' checking for high correlations and multicollinearity.
#'
#' @param data A data frame or tibble
#' @param correlation_threshold Numeric threshold above which correlations are considered high (default: 0.8)
#'
#' @return A list containing dependency analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' # Create data with correlated variables
#' x <- rnorm(100)
#' data <- data.frame(
#'   a = x + rnorm(100, 0, 0.1),
#'   b = x + rnorm(100, 0, 0.1),
#'   c = rnorm(100)
#' )
#' analyze_dependencies(data)
#' }
analyze_dependencies <- function(data, correlation_threshold = 0.8) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Initialize results
  results <- list(
    high_correlations = list(),
    multicollinearity = FALSE,
    condition_number = NA
  )
  
  # Select numeric variables
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  
  if (ncol(numeric_data) < 2) {
    results$message <- "At least two numeric columns are required for dependency analysis"
    class(results) <- c("statsaid_dependency_analysis", "list")
    return(results)
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  results$correlation_matrix <- cor_matrix
  
  # Check for high correlations
  high_cor <- which(abs(cor_matrix) > correlation_threshold & abs(cor_matrix) < 1, arr.ind = TRUE)
  
  if (nrow(high_cor) > 0) {
    for (i in 1:nrow(high_cor)) {
      var1 <- rownames(cor_matrix)[high_cor[i, 1]]
      var2 <- colnames(cor_matrix)[high_cor[i, 2]]
      
      # Avoid duplicates due to symmetry
      if (var1 != var2 && !paste(var2, var1, sep = "_") %in% names(results$high_correlations)) {
        pair_name <- paste(var1, var2, sep = "_")
        results$high_correlations[[pair_name]] <- cor_matrix[high_cor[i, 1], high_cor[i, 2]]
      }
    }
  }
  
  # Check for multicollinearity using condition number if enough observations
  if (nrow(numeric_data) > ncol(numeric_data) * 2) {
    tryCatch({
      # Handle missing values
      complete_data <- stats::na.omit(numeric_data)
      
      if (nrow(complete_data) > ncol(complete_data)) {
        X <- as.matrix(scale(complete_data))
        svd_result <- svd(X)
        results$condition_number <- max(svd_result$d) / min(svd_result$d)
        results$multicollinearity <- results$condition_number > 30
        
        # Calculate VIF if fewer than 20 variables
        if (ncol(X) < 20) {
          results$vif <- calculate_vif(complete_data)
        }
      }
    }, error = function(e) {
      results$multicollinearity_error <- as.character(e)
    })
  }
  
  # Add class for printing
  class(results) <- c("statsaid_dependency_analysis", "list")
  return(results)
}

#' Print method for statsaid_dependency_analysis objects
#'
#' @param x An object of class statsaid_dependency_analysis
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_dependency_analysis <- function(x, ...) {
  cat("StatsAid Dependency Analysis\n")
  cat("============================\n\n")
  
  if (!is.null(x$message)) {
    cat(x$message, "\n\n")
    return(invisible(x))
  }
  
  # High correlations
  if (length(x$high_correlations) > 0) {
    cat("High correlations detected:\n")
    for (pair in names(x$high_correlations)) {
      var_names <- strsplit(pair, "_")[[1]]
      cat("  -", var_names[1], "and", var_names[2], ":", 
          round(x$high_correlations[[pair]], 3), "\n")
    }
    cat("\n")
  } else {
    cat("No high correlations detected.\n\n")
  }
  
  # Multicollinearity
  if (!is.na(x$condition_number)) {
    cat("Condition number:", round(x$condition_number, 1), "\n")
    
    if (x$multicollinearity) {
      cat("Multicollinearity detected (condition number > 30).\n")
      cat("Consider: regularization, principal component analysis, or removing variables.\n\n")
    } else {
      cat("No severe multicollinearity detected.\n\n")
    }
  }
  
  # VIF values if available
  if (!is.null(x$vif)) {
    cat("Variance Inflation Factors (VIF):\n")
    vif_df <- data.frame(
      Variable = names(x$vif),
      VIF = sapply(x$vif, function(v) round(v, 2))
    )
    vif_df <- vif_df[order(-vif_df$VIF), ]
    print(vif_df, row.names = FALSE)
    cat("\nVIF > 10 indicates problematic multicollinearity\n")
  }
  
  invisible(x)
}

#' Calculate Variance Inflation Factors (VIF)
#'
#' @param data A data frame of numeric variables
#'
#' @return A named vector of VIF values
#' @keywords internal
calculate_vif <- function(data) {
  if (ncol(data) < 2) {
    return(NULL)
  }
  
  # Initialize results
  vif_values <- numeric(ncol(data))
  names(vif_values) <- names(data)
  
  # Calculate VIF for each variable
  for (i in 1:ncol(data)) {
    # Create formula with current variable as outcome and others as predictors
    predictors <- setdiff(names(data), names(data)[i])
    if (length(predictors) > 0) {
      formula <- as.formula(paste(names(data)[i], "~", paste(predictors, collapse = " + ")))
      
      # Fit linear model
      model <- try(lm(formula, data = data), silent = TRUE)
      
      if (!inherits(model, "try-error")) {
        # Calculate VIF = 1/(1-R²)
        vif_values[i] <- 1 / (1 - summary(model)$r.squared)
      } else {
        vif_values[i] <- NA
      }
    } else {
      vif_values[i] <- 1  # No multicollinearity with single predictor
    }
  }
  
  return(vif_values)
}

#' Analyze an outcome variable
#'
#' This function analyzes an outcome variable for its type, distribution,
#' and other important characteristics.
#'
#' @param data A data frame or tibble
#' @param outcome_var Character string specifying the outcome variable name
#'
#' @return A list containing outcome analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   y_continuous = rnorm(100),
#'   y_binary = sample(c(0, 1), 100, replace = TRUE),
#'   y_count = rpois(100, 3)
#' )
#' analyze_outcome(data, "y_continuous")
#' analyze_outcome(data, "y_binary")
#' analyze_outcome(data, "y_count")
#' }
analyze_outcome <- function(data, outcome_var) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Check if outcome variable exists
  if (!outcome_var %in% names(data)) {
    stop("Outcome variable '", outcome_var, "' not found in dataset")
  }
  
  # Initialize results
  results <- list(
    variable = outcome_var
  )
  
  outcome <- data[[outcome_var]]
  
  # Basic statistics
  results$n <- length(outcome)
  results$missing <- sum(is.na(outcome))
  results$missing_proportion <- mean(is.na(outcome))
  
  # Determine variable type
  if (is.factor(outcome) || is.character(outcome)) {
    results$type <- "categorical"
    results$levels <- length(unique(outcome[!is.na(outcome)]))
    
    if (results$levels == 2) {
      results$subtype <- "binary"
      
      # Check class imbalance
      tab <- table(outcome)
      min_class_prop <- min(tab) / sum(tab)
      results$balanced <- min_class_prop >= 0.2  # Arbitrary threshold
      results$minority_class_proportion <- min_class_prop
      
      # Store class frequencies
      results$class_frequencies <- as.list(tab)
    } else if (results$levels <= 10) {
      # Check if ordered
      if (is.ordered(outcome)) {
        results$subtype <- "ordinal"
      } else {
        results$subtype <- "nominal"
      }
      
      # Store class frequencies
      results$class_frequencies <- as.list(table(outcome))
    } else {
      results$subtype <- "high_cardinality"
      results$top_categories <- as.list(sort(table(outcome), decreasing = TRUE)[1:min(10, results$levels)])
    }
  } else if (is.numeric(outcome)) {
    # Calculate basic statistics
    results$mean <- mean(outcome, na.rm = TRUE)
    results$median <- median(outcome, na.rm = TRUE)
    results$sd <- sd(outcome, na.rm = TRUE)
    results$min <- min(outcome, na.rm = TRUE)
    results$max <- max(outcome, na.rm = TRUE)
    
    # Determine if continuous or discrete
    unique_vals <- unique(outcome[!is.na(outcome)])
    
    if (length(unique_vals) <= 10 && all(unique_vals == floor(unique_vals))) {
      results$type <- "discrete"
      
      # Check if count data
      if (all(unique_vals >= 0)) {
        results$subtype <- "count"
        
        # Check for zero-inflation
        zero_prop <- mean(outcome == 0, na.rm = TRUE)
        results$zero_inflated <- zero_prop > 0.3  # Arbitrary threshold
        results$zero_proportion <- zero_prop
      } else {
        results$subtype <- "integer"
      }
      
      # Store value frequencies
      results$value_frequencies <- as.list(table(outcome))
    } else {
      results$type <- "continuous"
      
      # Check for censoring (common in survival analysis)
      if (any(grepl("time|survival|duration", outcome_var, ignore.case = TRUE)) && 
          any(grepl("status|event|censor", names(data), ignore.case = TRUE))) {
        results$subtype <- "possibly_censored"
      } else {
        # Check distribution
        if (requireNamespace("moments", quietly = TRUE)) {
          results$skewness <- moments::skewness(outcome, na.rm = TRUE)
          results$kurtosis <- moments::kurtosis(outcome, na.rm = TRUE)
          
          if (results$skewness > 1) {
            results$subtype <- "right_skewed"
          } else if (results$skewness < -1) {
            results$subtype <- "left_skewed"
          } else {
            results$subtype <- "symmetric"
          }
        }
      }
    }
  } else if (inherits(outcome, "Date") || inherits(outcome, "POSIXt")) {
    results$type <- "datetime"
    results$min <- min(outcome, na.rm = TRUE)
    results$max <- max(outcome, na.rm = TRUE)
    results$range_days <- as.numeric(difftime(max(outcome, na.rm = TRUE), 
                                             min(outcome, na.rm = TRUE), 
                                             units = "days"))
  } else {
    results$type <- "unknown"
  }
  
  # Test for normality if continuous
  if (!is.null(results$type) && results$type == "continuous" && 
      sum(!is.na(outcome)) >= 3) {
    results$normality <- test_normality_single(outcome)
  }
  
  # Add class for printing
  class(results) <- c("statsaid_outcome_analysis", "list")
  return(results)
}

#' Print method for statsaid_outcome_analysis objects
#'
#' @param x An object of class statsaid_outcome_analysis
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_outcome_analysis <- function(x, ...) {
  cat("StatsAid Outcome Analysis:", x$variable, "\n")
  cat(paste(rep("=", nchar("StatsAid Outcome Analysis:") + nchar(x$variable) + 1), collapse = ""), "\n\n")
  
  # Basic information
  cat("Sample size:", x$n, "\n")
  if (x$missing > 0) {
    cat("Missing values:", x$missing, sprintf("(%.1f%%)", x$missing_proportion * 100), "\n")
  }
  
  # Type information
  cat("Variable type:", x$type)
  if (!is.null(x$subtype)) {
    cat(" (", x$subtype, ")", sep = "")
  }
  cat("\n\n")
  
  # Type-specific information
  if (x$type == "categorical") {
    cat("Number of levels:", x$levels, "\n")
    
    if (x$subtype == "binary") {
      cat("Class balance: ", sprintf("%.1f%% / %.1f%%", 
                                 x$minority_class_proportion * 100,
                                 (1 - x$minority_class_proportion) * 100), "\n")
      if (!x$balanced) {
        cat("Note: Class imbalance detected\n")
      }
      
      cat("Class frequencies:\n")
      for (class in names(x$class_frequencies)) {
        cat("  -", class, ":", x$class_frequencies[[class]], "\n")
      }
    } else if (x$subtype %in% c("nominal", "ordinal") && length(x$class_frequencies) <= 10) {
      cat("Class frequencies:\n")
      for (class in names(x$class_frequencies)) {
        cat("  -", class, ":", x$class_frequencies[[class]], "\n")
      }
    } else if (x$subtype == "high_cardinality") {
      cat("Top categories (out of", x$levels, "):\n")
      for (i in seq_along(x$top_categories)) {
        cat("  -", names(x$top_categories)[i], ":", x$top_categories[[i]], "\n")
      }
    }
  } else if (x$type %in% c("continuous", "discrete")) {
    # Statistics
    cat("Summary statistics:\n")
    cat("  - Mean:", round(x$mean, 2), "\n")
    cat("  - Median:", round(x$median, 2), "\n")
    cat("  - SD:", round(x$sd, 2), "\n")
    cat("  - Min:", round(x$min, 2), "\n")
    cat("  - Max:", round(x$max, 2), "\n")
    
    if (!is.null(x$skewness)) {
      cat("  - Skewness:", round(x$skewness, 2))
      if (abs(x$skewness) > 1) {
        cat(" (substantial)")
      }
      cat("\n")
    }
    
    if (x$type == "discrete" && x$subtype == "count") {
      cat("\nZero proportion:", sprintf("%.1f%%", x$zero_proportion * 100))
      if (x$zero_inflated) {
        cat(" (zero-inflated)")
      }
      cat("\n")
    }
    
    if (!is.null(x$normality)) {
      cat("\nNormality tests:\n")
      for (test in names(x$normality$tests)) {
        result <- x$normality$tests[[test]]
        cat("  - ", toupper(test), ": p = ", sprintf("%.4f", result$p_value), 
            " (", ifelse(result$normal, "Normal", "Non-normal"), ")\n", sep = "")
      }
      
      cat("\nOverall assessment: ")
      if (x$normality$normal) {
        cat("Likely normally distributed\n")
      } else {
        cat("Likely not normally distributed\n")
      }
    }
  } else if (x$type == "datetime") {
    cat("Date range:", as.character(x$min), "to", as.character(x$max), "\n")
    cat("Range (days):", round(x$range_days, 1), "\n")
  }
  
  # Model suggestions
  if (!is.null(x$model_suggestions)) {
    cat("\nModel Suggestions:\n")
    for (model in x$model_suggestions) {
      cat("  -", model, "\n")
    }
  }
  
  cat("\n")
  invisible(x)
}

#' Test normality of a single numeric vector
#'
#' @param x A numeric vector
#' @param tests Character vector of tests to perform
#'
#' @return A list with normality test results
#' @keywords internal
test_normality_single <- function(x, tests = c("shapiro", "jb")) {
  # Remove missing values
  x <- x[!is.na(x)]
  
  # Skip if too few values
  if (length(x) < 3) {
    return(list(
      skipped = TRUE,
      reason = "Too few values"
    ))
  }
  
  # Initialize results
  results <- list(
    tests = list(),
    skewness = NA,
    kurtosis = NA
  )
  
  # Calculate skewness and kurtosis if moments package is available
  if (requireNamespace("moments", quietly = TRUE)) {
    results$skewness <- moments::skewness(x)
    results$kurtosis <- moments::kurtosis(x)
  }
  
  # Run selected tests
  if ("shapiro" %in% tests && length(x) <= 5000) {
    # Shapiro-Wilk test (limited to 5000 observations)
    sw_test <- suppressWarnings(shapiro.test(x))
    results$tests$shapiro <- list(
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
    results$tests$ks <- list(
      statistic = ks_test$statistic,
      p_value = ks_test$p.value,
      normal = ks_test$p.value >= 0.05
    )
  }
  
  if ("ad" %in% tests && requireNamespace("nortest", quietly = TRUE)) {
    # Anderson-Darling test
    ad_test <- suppressWarnings(nortest::ad.test(x))
    results$tests$ad <- list(
      statistic = ad_test$statistic,
      p_value = ad_test$p.value,
      normal = ad_test$p.value >= 0.05
    )
  }
  
  if ("jb" %in% tests && requireNamespace("moments", quietly = TRUE)) {
    # Jarque-Bera test
    jb_test <- suppressWarnings(moments::jarque.test(x))
    results$tests$jb <- list(
      statistic = jb_test$statistic,
      p_value = jb_test$p.value,
      normal = jb_test$p.value >= 0.05
    )
  }
  
  # Overall normality assessment
  normal_results <- sapply(results$tests, function(test) test$normal)
  
  if (length(normal_results) > 0) {
    results$normal <- mean(normal_results) >= 0.5
    results$normal_tests <- sum(normal_results)
    results$total_tests <- length(normal_results)
  } else {
    results$normal <- NA
    results$normal_tests <- 0
    results$total_tests <- 0
  }
  
  return(results)
}

#' Detect clusters in data
#'
#' This function attempts to identify variables that might indicate clustering
#' in the dataset, such as repeated subject IDs or grouping variables.
#'
#' @param data A data frame or tibble
#'
#' @return A list containing cluster detection results
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   patient_id = rep(1:20, each = 3),
#'   visit = rep(1:3, times = 20),
#'   measure = rnorm(60)
#' )
#' detect_clusters(data)
#' }
detect_clusters <- function(data) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Initialize results
  results <- list(
    has_clustering = FALSE,
    potential_cluster_vars = character(0),
    cluster_analysis = list(),
    recommended_approach = NULL
  )
  
  # Look for potential ID variables that might indicate clustering
  potential_ids <- character(0)
  
  for (col in names(data)) {
    col_data <- data[[col]]
    
    if (is.factor(col_data) || is.character(col_data) || is.numeric(col_data)) {
      # Check if values are repeated
      value_counts <- table(col_data)
      repeats <- value_counts > 1
      
      if (any(repeats)) {
        # Check if name suggests ID
        name_suggests_id <- grepl("id$|^id|_id|subject|patient|cluster|group|site|center", 
                               col, ignore.case = TRUE)
        
        # Heuristics: Look for repeated values that suggest clustering
        n_unique <- length(unique(col_data))
        
        # If number of unique values is much smaller than number of rows,
        # and values are repeated, this might be a clustering variable
        if (n_unique < nrow(data) * 0.5) {
          # Higher score for variables with names suggesting IDs
          score <- ifelse(name_suggests_id, 3, 1)
          
          # Higher score for variables with a good cluster size distribution
          avg_cluster_size <- nrow(data) / n_unique
          if (avg_cluster_size >= 2 && avg_cluster_size <= 10) {
            score <- score + 1
          }
          
          # Save potential ID with score
          potential_ids <- c(potential_ids, col)
          results$cluster_scores[[col]] <- score
        }
      }
    }
  }
  
  # If no obvious candidate, look for correlated observations
  if (length(potential_ids) == 0) {
    # This would require more complex analysis and is skipped for now
    results$has_clustering <- FALSE
    results$message <- "No clear clustering structure detected"
    class(results) <- c("statsaid_cluster_analysis", "list")
    return(results)
  }
  
  # If we found potential clustering variables
  if (length(potential_ids) > 0) {
    results$has_clustering <- TRUE
    results$potential_cluster_vars <- potential_ids
    
    # Analyze clustering structure
    results$cluster_analysis <- lapply(potential_ids, function(id_var) {
      # Count observations per cluster
      cluster_counts <- table(data[[id_var]])
      
      list(
        variable = id_var,
        n_clusters = length(cluster_counts),
        min_cluster_size = min(cluster_counts),
        max_cluster_size = max(cluster_counts),
        mean_cluster_size = mean(cluster_counts),
        median_cluster_size = median(as.numeric(cluster_counts)),
        cluster_size_sd = sd(as.numeric(cluster_counts)),
        balanced = max(cluster_counts) / min(cluster_counts) < 3  # Arbitrary threshold
      )
    })
    
    # Check for multi-level clustering
    results$multi_level_clustering <- check_multilevel_clustering(data, potential_ids)
    
    # Recommend approach based on clustering structure
    if (any(sapply(results$cluster_analysis, function(x) x$n_clusters < 5))) {
      results$recommended_approach <- "fixed_effects"
    } else if (any(sapply(results$cluster_analysis, function(x) !x$balanced))) {
      results$recommended_approach <- "mixed_model"
    } else {
      results$recommended_approach <- "mixed_model_or_gee"
    }
  }
  
  # Add class for printing
  class(results) <- c("statsaid_cluster_analysis", "list")
  return(results)
}

#' Print method for statsaid_cluster_analysis objects
#'
#' @param x An object of class statsaid_cluster_analysis
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_cluster_analysis <- function(x, ...) {
  cat("StatsAid Cluster Analysis\n")
  cat("========================\n\n")
  
  if (!is.null(x$message)) {
    cat(x$message, "\n\n")
    return(invisible(x))
  }
  
  if (x$has_clustering) {
    cat("Clustering detected in the dataset\n\n")
    
    cat("Potential clustering variables:\n")
    for (id_var in x$potential_cluster_vars) {
      analysis <- x$cluster_analysis[[which(sapply(x$cluster_analysis, function(a) a$variable == id_var))]]
      
      cat("  - ", id_var, ":\n", sep = "")
      cat("    Clusters:", analysis$n_clusters, "\n")
      cat("    Cluster sizes: min =", analysis$min_cluster_size, 
          ", max =", analysis$max_cluster_size, 
          ", mean =", round(analysis$mean_cluster_size, 1), "\n")
      cat("    Balanced:", ifelse(analysis$balanced, "Yes", "No"), "\n")
    }
    
    if (!is.null(x$multi_level_clustering) && length(x$multi_level_clustering) > 0) {
      cat("\nPotential multi-level clustering detected:\n")
      for (i in seq_along(x$multi_level_clustering)) {
        relationship <- x$multi_level_clustering[[i]]
        cat("  - ", relationship$lower, " nested within ", relationship$higher, "\n", sep = "")
      }
    }
    
    cat("\nRecommended analysis approach: ", x$recommended_approach, "\n", sep = "")
    
    if (x$recommended_approach == "fixed_effects") {
      cat("Note: Few clusters detected. Consider using fixed effects.\n")
    } else if (x$recommended_approach == "mixed_model") {
      cat("Note: Unbalanced cluster sizes detected. Mixed-effects models recommended.\n")
    } else if (x$recommended_approach == "mixed_model_or_gee") {
      cat("Note: Both mixed-effects models and GEE approaches may be appropriate.\n")
    }
  } else {
    cat("No clustering detected in the dataset\n\n")
  }
  
  invisible(x)
}

#' Check for multi-level clustering in data
#'
#' @param data A data frame
#' @param cluster_vars Character vector of potential clustering variables
#'
#' @return A list of potential nesting relationships
#' @keywords internal
check_multilevel_clustering <- function(data, cluster_vars) {
  if (length(cluster_vars) < 2) {
    return(list())
  }
  
  # Initialize results
  relationships <- list()
  
  # Check all pairs of clustering variables
  for (i in 1:(length(cluster_vars) - 1)) {
    for (j in (i + 1):length(cluster_vars)) {
      var1 <- cluster_vars[i]
      var2 <- cluster_vars[j]
      
      # Check if var1 is nested within var2
      df_nested <- data.frame(
        var1 = data[[var1]],
        var2 = data[[var2]]
      )
      
      # Count unique combinations
      combinations <- nrow(unique(df_nested))
      
      # Count unique values of var1 and var2
      unique_var1 <- length(unique(data[[var1]]))
      unique_var2 <- length(unique(data[[var2]]))
      
      # If combinations equals unique_var1, var1 might be nested within var2
      if (combinations == unique_var1 && unique_var1 > unique_var2) {
        # Check more rigorously with aggregation
        nested <- all(sapply(split(data[[var1]], data[[var2]]), function(x) length(unique(x)) == 1))
        
        if (nested) {
          relationships <- c(relationships, list(list(
            lower = var1,
            higher = var2,
            type = "nested"
          )))
        }
      } 
      # Check if var2 is nested within var1
      else if (combinations == unique_var2 && unique_var2 > unique_var1) {
        # Check more rigorously with aggregation
        nested <- all(sapply(split(data[[var2]], data[[var1]]), function(x) length(unique(x)) == 1))
        
        if (nested) {
          relationships <- c(relationships, list(list(
            lower = var2,
            higher = var1,
            type = "nested"
          )))
        }
      }
      # If neither is nested, they might be crossed
      else if (combinations < unique_var1 * unique_var2) {
        relationships <- c(relationships, list(list(
          var1 = var1,
          var2 = var2,
          type = "crossed"
        )))
      }
    }
  }
  
  return(relationships)
}

#' Comprehensive data structure analysis
#'
#' This function performs a comprehensive analysis of the data structure,
#' including distributions, dependencies, clustering, and outcome analysis.
#'
#' @param data A data frame or tibble
#' @param outcome_var Optional character string specifying the outcome variable name
#'
#' @return A list containing comprehensive data analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   subject_id = rep(1:20, each = 2),
#'   treatment = rep(c("A", "B"), times = 20),
#'   age = rep(runif(20, 20, 80), each = 2),
#'   response = rnorm(40)
#' )
#' analyze_data_structure(data, "response")
#' }
analyze_data_structure <- function(data, outcome_var = NULL) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Initialize results
  results <- list()
  
  # Basic data properties
  results$dimensions <- list(
    rows = nrow(data),
    columns = ncol(data),
    complete_cases = sum(complete.cases(data)),
    complete_case_proportion = mean(complete.cases(data))
  )
  
  # Outcome analysis (if provided)
  if (!is.null(outcome_var)) {
    if (outcome_var %in% names(data)) {
      results$outcome_analysis <- analyze_outcome(data, outcome_var)
      
      # Remove outcome from predictor analysis
      predictor_data <- data[, setdiff(names(data), outcome_var), drop = FALSE]
    } else {
      warning("Outcome variable '", outcome_var, "' not found in dataset")
      predictor_data <- data
    }
  } else {
    predictor_data <- data
  }
  
  # Distribution analysis for all variables (or just predictors if outcome specified)
  results$distribution_analysis <- analyze_distributions(predictor_data)
  
  # Dependency analysis for predictors
  results$dependency_analysis <- analyze_dependencies(predictor_data)
  
  # Cluster detection
  results$cluster_analysis <- detect_clusters(data)
  
  # Add class for printing
  class(results) <- c("statsaid_data_structure", "list")
  return(results)
}

#' Print method for statsaid_data_structure objects
#'
#' @param x An object of class statsaid_data_structure
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_data_structure <- function(x, ...) {
  cat("StatsAid Data Structure Analysis\n")
  cat("===============================\n\n")
  
  # Basic dimensions
  cat("Dataset dimensions:", x$dimensions$rows, "rows,", x$dimensions$columns, "columns\n")
  cat("Complete cases:", x$dimensions$complete_cases, 
      sprintf("(%.1f%%)", x$dimensions$complete_case_proportion * 100), "\n\n")
  
  # Outcome analysis (if available)
  if (!is.null(x$outcome_analysis)) {
    cat("Outcome Variable:", x$outcome_analysis$variable, "\n")
    cat("  Type:", x$outcome_analysis$type)
    if (!is.null(x$outcome_analysis$subtype)) {
      cat(" (", x$outcome_analysis$subtype, ")", sep = "")
    }
    cat("\n\n")
  }
  
  # Clustering information (if detected)
  if (!is.null(x$cluster_analysis) && x$cluster_analysis$has_clustering) {
    cat("Clustering detected:\n")
    
    for (id_var in x$cluster_analysis$potential_cluster_vars) {
      analysis <- x$cluster_analysis$cluster_analysis[[which(sapply(x$cluster_analysis$cluster_analysis, 
                                                              function(a) a$variable == id_var))]]
      
      cat("  - ", id_var, ": ", analysis$n_clusters, " clusters, ", 
          "mean size = ", round(analysis$mean_cluster_size, 1), "\n", sep = "")
    }
    
    if (!is.null(x$cluster_analysis$recommended_approach)) {
      cat("\nRecommended approach: ", x$cluster_analysis$recommended_approach, "\n", sep = "")
    }
    
    cat("\n")
  }
  
  # Dependency warnings (if detected)
  if (!is.null(x$dependency_analysis) && length(x$dependency_analysis$high_correlations) > 0) {
    cat("High correlations detected:", length(x$dependency_analysis$high_correlations), "pairs\n")
    
    if (!is.na(x$dependency_analysis$multicollinearity) && 
        x$dependency_analysis$multicollinearity) {
      cat("Multicollinearity detected (condition number =", 
          round(x$dependency_analysis$condition_number, 1), ")\n")
    }
    
    cat("\n")
  }
  
  # Distribution issues
  non_normal_vars <- names(which(sapply(x$distribution_analysis, function(v) {
    !is.null(v$normal) && !is.na(v$normal) && !v$normal
  })))
  
  if (length(non_normal_vars) > 0) {
    cat("Non-normal distributions detected in", length(non_normal_vars), "variables\n")
    
    # Count skewed variables
    skewed_vars <- names(which(sapply(x$distribution_analysis, function(v) {
      !is.null(v$stats$skewness) && abs(v$stats$skewness) > 1
    })))
    
    if (length(skewed_vars) > 0) {
      cat("Skewed variables:", length(skewed_vars), "\n")
    }
    
    cat("\n")
  }
  
  # Zero-inflated count variables
  zero_inflated_vars <- names(which(sapply(x$distribution_analysis, function(v) {
    !is.null(v$zero_inflated) && v$zero_inflated
  })))
  
  if (length(zero_inflated_vars) > 0) {
    cat("Zero-inflated count variables detected:", length(zero_inflated_vars), "\n\n")
  }
  
  cat("Use print() on individual analysis components for more details.\n")
  
  invisible(x)
}