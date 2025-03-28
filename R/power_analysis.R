#' Power Analysis Functions for Research Design
#'
#' This file contains functions for performing power analysis and sample size calculations
#' for various research designs and statistical tests.

#' Calculate sample size for a t-test
#'
#' This function calculates the required sample size for a t-test based on desired power.
#'
#' @param d Numeric value specifying the expected effect size (Cohen's d)
#' @param power Numeric value specifying the desired power (default = 0.8)
#' @param sig_level Numeric value specifying the significance level (default = 0.05)
#' @param type Character string specifying the type of t-test ("two.sample", "one.sample", or "paired")
#' @param alternative Character string specifying the alternative hypothesis ("two.sided", "less", or "greater")
#' @param allocation_ratio Numeric value specifying the ratio of n2/n1 for two-sample tests (default = 1)
#'
#' @return A list containing sample size calculations and power analysis parameters
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate sample size for a two-sample t-test with medium effect size
#' calculate_t_test_sample_size(d = 0.5)
#' 
#' # Calculate sample size for a paired t-test with large effect size
#' calculate_t_test_sample_size(d = 0.8, type = "paired")
#' }
calculate_t_test_sample_size <- function(d, power = 0.8, sig_level = 0.05, 
                                       type = "two.sample", 
                                       alternative = "two.sided",
                                       allocation_ratio = 1) {
  
  # Check if pwr package is available
  if (!requireNamespace("pwr", quietly = TRUE)) {
    stop("Package 'pwr' needed for this function to work. Please install it.")
  }
  
  # Validate parameters
  if (!is.numeric(d) || d <= 0) {
    stop("Effect size (d) must be a positive number")
  }
  
  if (!is.numeric(power) || power <= 0 || power >= 1) {
    stop("Power must be between 0 and 1")
  }
  
  if (!is.numeric(sig_level) || sig_level <= 0 || sig_level >= 1) {
    stop("Significance level must be between 0 and 1")
  }
  
  valid_types <- c("two.sample", "one.sample", "paired")
  if (!(type %in% valid_types)) {
    stop("Type must be one of: ", paste(valid_types, collapse = ", "))
  }
  
  valid_alternatives <- c("two.sided", "less", "greater")
  if (!(alternative %in% valid_alternatives)) {
    stop("Alternative must be one of: ", paste(valid_alternatives, collapse = ", "))
  }
  
  # Calculate sample size based on test type
  result <- list(
    effect_size = d,
    power = power,
    sig_level = sig_level,
    test_type = type,
    alternative = alternative
  )
  
  if (type == "two.sample") {
    # Two-sample t-test
    pwr_result <- pwr::pwr.t.test(d = d, 
                                power = power, 
                                sig.level = sig_level, 
                                type = "two.sample", 
                                alternative = alternative)
    
    if (allocation_ratio != 1) {
      # Adjust for unequal group sizes
      n1 <- pwr_result$n * (1 + allocation_ratio) / (2 * allocation_ratio)
      n2 <- n1 * allocation_ratio
      
      result$n1 <- ceiling(n1)
      result$n2 <- ceiling(n2)
      result$total_n <- ceiling(n1 + n2)
      result$allocation_ratio <- allocation_ratio
      result$note <- "Sample sizes adjusted for allocation ratio"
    } else {
      result$n_per_group <- ceiling(pwr_result$n)
      result$total_n <- ceiling(pwr_result$n * 2)
    }
    
  } else if (type == "one.sample") {
    # One-sample t-test
    pwr_result <- pwr::pwr.t.test(d = d, 
                                power = power, 
                                sig.level = sig_level, 
                                type = "one.sample", 
                                alternative = alternative)
    result$total_n <- ceiling(pwr_result$n)
    
  } else if (type == "paired") {
    # Paired t-test
    pwr_result <- pwr::pwr.t.test(d = d, 
                                power = power, 
                                sig.level = sig_level, 
                                type = "paired", 
                                alternative = alternative)
    result$total_n <- ceiling(pwr_result$n)
    result$note <- "n refers to the number of pairs"
  }
  
  # Add the full pwr object for reference
  result$pwr_object <- pwr_result
  
  # Add effect size interpretation
  result$effect_size_interpretation <- interpret_effect_size(d, "d")
  
  class(result) <- c("statsaid_power", "list")
  return(result)
}

#' Calculate sample size for ANOVA
#'
#' This function calculates the required sample size for an ANOVA test.
#'
#' @param f Numeric value specifying the expected effect size (Cohen's f)
#' @param groups Integer specifying the number of groups
#' @param power Numeric value specifying the desired power (default = 0.8)
#' @param sig_level Numeric value specifying the significance level (default = 0.05)
#'
#' @return A list containing sample size calculations and power analysis parameters
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate sample size for an ANOVA with 3 groups and medium effect size
#' calculate_anova_sample_size(f = 0.25, groups = 3)
#' }
calculate_anova_sample_size <- function(f, groups, power = 0.8, sig_level = 0.05) {
  
  # Check if pwr package is available
  if (!requireNamespace("pwr", quietly = TRUE)) {
    stop("Package 'pwr' needed for this function to work. Please install it.")
  }
  
  # Validate parameters
  if (!is.numeric(f) || f <= 0) {
    stop("Effect size (f) must be a positive number")
  }
  
  if (!is.numeric(groups) || groups < 2 || (groups %% 1) != 0) {
    stop("Number of groups must be an integer greater than 1")
  }
  
  if (!is.numeric(power) || power <= 0 || power >= 1) {
    stop("Power must be between 0 and 1")
  }
  
  if (!is.numeric(sig_level) || sig_level <= 0 || sig_level >= 1) {
    stop("Significance level must be between 0 and 1")
  }
  
  # Calculate sample size
  pwr_result <- pwr::pwr.anova.test(k = groups, 
                                  f = f, 
                                  power = power, 
                                  sig.level = sig_level)
  
  # Prepare result
  result <- list(
    effect_size = f,
    power = power,
    sig_level = sig_level,
    groups = groups,
    n_per_group = ceiling(pwr_result$n),
    total_n = ceiling(pwr_result$n * groups),
    pwr_object = pwr_result
  )
  
  # Add effect size interpretation
  result$effect_size_interpretation <- interpret_effect_size(f, "f")
  
  class(result) <- c("statsaid_power", "list")
  return(result)
}

#' Calculate sample size for correlation test
#'
#' This function calculates the required sample size for a correlation test.
#'
#' @param r Numeric value specifying the expected correlation coefficient
#' @param power Numeric value specifying the desired power (default = 0.8)
#' @param sig_level Numeric value specifying the significance level (default = 0.05)
#' @param alternative Character string specifying the alternative hypothesis ("two.sided", "less", or "greater")
#'
#' @return A list containing sample size calculations and power analysis parameters
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate sample size for detecting a correlation of 0.3
#' calculate_correlation_sample_size(r = 0.3)
#' }
calculate_correlation_sample_size <- function(r, power = 0.8, sig_level = 0.05, 
                                           alternative = "two.sided") {
  
  # Check if pwr package is available
  if (!requireNamespace("pwr", quietly = TRUE)) {
    stop("Package 'pwr' needed for this function to work. Please install it.")
  }
  
  # Validate parameters
  if (!is.numeric(r) || abs(r) >= 1) {
    stop("Correlation coefficient (r) must be between -1 and 1, exclusive")
  }
  
  if (!is.numeric(power) || power <= 0 || power >= 1) {
    stop("Power must be between 0 and 1")
  }
  
  if (!is.numeric(sig_level) || sig_level <= 0 || sig_level >= 1) {
    stop("Significance level must be between 0 and 1")
  }
  
  valid_alternatives <- c("two.sided", "less", "greater")
  if (!(alternative %in% valid_alternatives)) {
    stop("Alternative must be one of: ", paste(valid_alternatives, collapse = ", "))
  }
  
  # Calculate sample size
  pwr_result <- pwr::pwr.r.test(r = r, 
                              power = power, 
                              sig.level = sig_level, 
                              alternative = alternative)
  
  # Prepare result
  result <- list(
    effect_size = r,
    power = power,
    sig_level = sig_level,
    alternative = alternative,
    total_n = ceiling(pwr_result$n),
    pwr_object = pwr_result
  )
  
  # Add effect size interpretation
  result$effect_size_interpretation <- interpret_effect_size(abs(r), "r")
  
  class(result) <- c("statsaid_power", "list")
  return(result)
}

#' Calculate sample size for chi-square test
#'
#' This function calculates the required sample size for a chi-square test of independence.
#'
#' @param w Numeric value specifying the expected effect size (Cohen's w)
#' @param df Integer specifying the degrees of freedom
#' @param power Numeric value specifying the desired power (default = 0.8)
#' @param sig_level Numeric value specifying the significance level (default = 0.05)
#'
#' @return A list containing sample size calculations and power analysis parameters
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate sample size for a chi-square test with df=2 and medium effect size
#' calculate_chi_square_sample_size(w = 0.3, df = 2)
#' 
#' # Calculate sample size for a 3x3 contingency table with small effect size
#' # df = (rows-1) * (cols-1) = 2 * 2 = 4
#' calculate_chi_square_sample_size(w = 0.1, df = 4)
#' }
calculate_chi_square_sample_size <- function(w, df, power = 0.8, sig_level = 0.05) {
  
  # Check if pwr package is available
  if (!requireNamespace("pwr", quietly = TRUE)) {
    stop("Package 'pwr' needed for this function to work. Please install it.")
  }
  
  # Validate parameters
  if (!is.numeric(w) || w <= 0) {
    stop("Effect size (w) must be a positive number")
  }
  
  if (!is.numeric(df) || df < 1 || (df %% 1) != 0) {
    stop("Degrees of freedom must be a positive integer")
  }
  
  if (!is.numeric(power) || power <= 0 || power >= 1) {
    stop("Power must be between 0 and 1")
  }
  
  if (!is.numeric(sig_level) || sig_level <= 0 || sig_level >= 1) {
    stop("Significance level must be between 0 and 1")
  }
  
  # Calculate sample size
  pwr_result <- pwr::pwr.chisq.test(w = w, 
                                  df = df, 
                                  power = power, 
                                  sig.level = sig_level)
  
  # Prepare result
  result <- list(
    effect_size = w,
    power = power,
    sig_level = sig_level,
    df = df,
    total_n = ceiling(pwr_result$N),
    pwr_object = pwr_result
  )
  
  # Add effect size interpretation
  result$effect_size_interpretation <- interpret_effect_size(w, "w")
  
  # Add contingency table hints
  if (df == 1) {
    result$contingency_hint <- "2x2 table"
  } else {
    # Try to determine dimensions based on df
    factors <- get_factors(df + 1)
    if (length(factors) >= 2) {
      r <- factors[1] + 1
      c <- factors[2] + 1
      result$contingency_hint <- paste0(r, "x", c, " table")
    }
  }
  
  class(result) <- c("statsaid_power", "list")
  return(result)
}

#' Calculate sample size for logistic regression
#'
#' This function calculates the required sample size for a logistic regression.
#'
#' @param p0 Numeric value specifying the baseline probability of the outcome
#' @param p1 Numeric value specifying the probability of the outcome under the alternative hypothesis
#' @param r2_other Numeric value specifying the expected R-squared of the predictor of interest with other predictors (default = 0)
#' @param power Numeric value specifying the desired power (default = 0.8)
#' @param sig_level Numeric value specifying the significance level (default = 0.05)
#' @param distribution Character string specifying the predictor distribution ("normal" or "binomial")
#' @param prop_1 Numeric value specifying the proportion of subjects with predictor value 1 (for binomial distribution)
#'
#' @return A list containing sample size calculations and power analysis parameters
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate sample size for logistic regression
#' # Baseline probability of outcome is 0.2, expected to increase to 0.35 with predictor
#' calculate_logistic_regression_sample_size(p0 = 0.2, p1 = 0.35)
#' }
calculate_logistic_regression_sample_size <- function(p0, p1, r2_other = 0, 
                                                   power = 0.8, sig_level = 0.05,
                                                   distribution = "normal", 
                                                   prop_1 = 0.5) {
  
  # Validate parameters
  if (!is.numeric(p0) || p0 <= 0 || p0 >= 1) {
    stop("p0 must be between 0 and 1")
  }
  
  if (!is.numeric(p1) || p1 <= 0 || p1 >= 1) {
    stop("p1 must be between 0 and 1")
  }
  
  if (!is.numeric(r2_other) || r2_other < 0 || r2_other >= 1) {
    stop("r2_other must be between 0 and 1")
  }
  
  if (!is.numeric(power) || power <= 0 || power >= 1) {
    stop("Power must be between 0 and 1")
  }
  
  if (!is.numeric(sig_level) || sig_level <= 0 || sig_level >= 1) {
    stop("Significance level must be between 0 and 1")
  }
  
  valid_distributions <- c("normal", "binomial")
  if (!(distribution %in% valid_distributions)) {
    stop("Distribution must be one of: ", paste(valid_distributions, collapse = ", "))
  }
  
  if (distribution == "binomial" && (!is.numeric(prop_1) || prop_1 <= 0 || prop_1 >= 1)) {
    stop("prop_1 must be between 0 and 1")
  }
  
  # Calculate using Hsieh's method (1998)
  # Log-odds ratio
  odds0 <- p0 / (1 - p0)
  odds1 <- p1 / (1 - p1)
  OR <- odds1 / odds0
  B <- log(OR)
  
  # Calculate variance inflation factor due to other predictors
  vif <- 1 / (1 - r2_other)
  
  # Calculate variance of X
  var_x <- if (distribution == "normal") 1 else prop_1 * (1 - prop_1)
  
  # Calculate average probability for expected standard deviation
  p_avg <- (p0 + p1) / 2
  sd_y <- sqrt(p_avg * (1 - p_avg))
  
  # Calculate required sample size
  z_alpha <- qnorm(1 - sig_level / 2)
  z_beta <- qnorm(power)
  
  n <- vif * ((z_alpha + z_beta)^2) / (B^2 * var_x * (1 - p_avg)^2)
  
  # Prepare result
  result <- list(
    p0 = p0,
    p1 = p1,
    odds_ratio = OR,
    r2_other = r2_other,
    power = power,
    sig_level = sig_level,
    distribution = distribution,
    prop_1 = if (distribution == "binomial") prop_1 else NA,
    total_n = ceiling(n),
    method = "Hsieh et al. (1998)"
  )
  
  # Calculate effect size (odds ratio) interpretation
  result$effect_size_interpretation <- interpret_odds_ratio(OR)
  
  class(result) <- c("statsaid_power", "list")
  return(result)
}

#' Calculate sample size for multiple regression
#'
#' This function calculates the required sample size for a multiple regression analysis.
#'
#' @param f2 Numeric value specifying the expected effect size (Cohen's f²)
#' @param predictors Integer specifying the number of predictors
#' @param power Numeric value specifying the desired power (default = 0.8)
#' @param sig_level Numeric value specifying the significance level (default = 0.05)
#' @param test_predictors Integer specifying the number of predictors being tested (default = 1)
#'
#' @return A list containing sample size calculations and power analysis parameters
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate sample size for multiple regression with 5 predictors and medium effect size
#' calculate_regression_sample_size(f2 = 0.15, predictors = 5)
#' 
#' # Testing a subset of predictors (testing 2 predictors in a model with 5 total predictors)
#' calculate_regression_sample_size(f2 = 0.15, predictors = 5, test_predictors = 2)
#' }
calculate_regression_sample_size <- function(f2, predictors, power = 0.8, 
                                          sig_level = 0.05, test_predictors = 1) {
  
  # Check if pwr package is available
  if (!requireNamespace("pwr", quietly = TRUE)) {
    stop("Package 'pwr' needed for this function to work. Please install it.")
  }
  
  # Validate parameters
  if (!is.numeric(f2) || f2 <= 0) {
    stop("Effect size (f²) must be a positive number")
  }
  
  if (!is.numeric(predictors) || predictors < 1 || (predictors %% 1) != 0) {
    stop("Number of predictors must be a positive integer")
  }
  
  if (!is.numeric(test_predictors) || test_predictors < 1 || test_predictors > predictors || (test_predictors %% 1) != 0) {
    stop("Number of tested predictors must be a positive integer less than or equal to total predictors")
  }
  
  if (!is.numeric(power) || power <= 0 || power >= 1) {
    stop("Power must be between 0 and 1")
  }
  
  if (!is.numeric(sig_level) || sig_level <= 0 || sig_level >= 1) {
    stop("Significance level must be between 0 and 1")
  }
  
  # Calculate sample size for testing a subset of predictors
  pwr_result <- pwr::pwr.f2.test(u = test_predictors, 
                               v = NULL, 
                               f2 = f2, 
                               sig.level = sig_level, 
                               power = power)
  
  # Error df = n - k - 1 where k is the total number of predictors
  # So n = v + k + 1
  total_n <- ceiling(pwr_result$v + predictors + 1)
  
  # Prepare result
  result <- list(
    effect_size = f2,
    power = power,
    sig_level = sig_level,
    predictors = predictors,
    test_predictors = test_predictors,
    total_n = total_n,
    pwr_object = pwr_result
  )
  
  # Add effect size interpretation
  result$effect_size_interpretation <- interpret_effect_size(f2, "f2")
  
  # Add rule of thumb from G*Power
  result$rule_of_thumb <- paste0("Rule of thumb: need at least 10-20 observations per predictor = ", 
                               (predictors+1) * 10, " to ", (predictors+1) * 20, " observations")
  
  class(result) <- c("statsaid_power", "list")
  return(result)
}

#' Create a power curve
#'
#' This function creates a power curve for a specified statistical test across
#' different sample sizes or effect sizes.
#'
#' @param test Character string specifying the test type ("t", "anova", "correlation", "chi-square", "regression")
#' @param vary Character string specifying what to vary ("n" or "effect")
#' @param effect_size Numeric value specifying the effect size (when vary = "n")
#' @param n Integer specifying the sample size (when vary = "effect")
#' @param range Numeric vector specifying the range for n or effect size
#' @param ... Additional parameters passed to the specific test function
#'
#' @return A ggplot object with the power curve
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a power curve varying sample size for a t-test with medium effect size
#' plot_power_curve(test = "t", vary = "n", effect_size = 0.5, range = seq(10, 200, by = 10))
#' 
#' # Create a power curve varying effect size for regression with fixed sample size
#' plot_power_curve(test = "regression", vary = "effect", n = 100, 
#'                 range = seq(0.01, 0.4, by = 0.01), predictors = 5)
#' }
plot_power_curve <- function(test, vary = "n", effect_size = NULL, n = NULL, 
                           range = NULL, ...) {
  
  # Check if required packages are available
  if (!requireNamespace("pwr", quietly = TRUE)) {
    stop("Package 'pwr' needed for this function to work. Please install it.")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed for this function to work. Please install it.")
  }
  
  # Validate parameters
  valid_tests <- c("t", "anova", "correlation", "chi-square", "regression")
  if (!(test %in% valid_tests)) {
    stop("Test must be one of: ", paste(valid_tests, collapse = ", "))
  }
  
  valid_vary <- c("n", "effect")
  if (!(vary %in% valid_vary)) {
    stop("vary must be one of: ", paste(valid_vary, collapse = ", "))
  }
  
  if (vary == "n" && is.null(effect_size)) {
    stop("When varying sample size, effect_size must be provided")
  }
  
  if (vary == "effect" && is.null(n)) {
    stop("When varying effect size, n must be provided")
  }
  
  if (is.null(range)) {
    stop("range must be provided")
  }
  
  # Default significance level
  sig_level <- 0.05
  
  # Calculate power for each value in range
  result_df <- data.frame(x = range, power = NA)
  
  # Customize x-axis label based on test and what's being varied
  if (vary == "n") {
    x_label <- "Sample Size"
  } else {
    # Effect size label depends on test
    if (test == "t") x_label <- "Effect Size (Cohen's d)"
    else if (test == "anova") x_label <- "Effect Size (Cohen's f)"
    else if (test == "correlation") x_label <- "Correlation Coefficient (r)"
    else if (test == "chi-square") x_label <- "Effect Size (Cohen's w)"
    else if (test == "regression") x_label <- "Effect Size (Cohen's f²)"
  }
  
  # Calculate power for each value in range
  for (i in seq_along(range)) {
    val <- range[i]
    
    if (test == "t") {
      # T-test
      params <- list(...)
      type <- if (!is.null(params$type)) params$type else "two.sample"
      alternative <- if (!is.null(params$alternative)) params$alternative else "two.sided"
      
      if (vary == "n") {
        # For one-sample or paired tests, n is the sample size
        # For two-sample tests, n is per group, so divide by 2
        if (type == "two.sample") {
          n_per_group <- floor(val / 2)
          if (n_per_group < 2) next
        } else {
          n_per_group <- val
        }
        
        pwr_result <- pwr::pwr.t.test(n = n_per_group, 
                                     d = effect_size, 
                                     sig.level = sig_level, 
                                     type = type,
                                     alternative = alternative,
                                     power = NULL)
      } else {
        pwr_result <- pwr::pwr.t.test(n = n, 
                                     d = val, 
                                     sig.level = sig_level, 
                                     type = type,
                                     alternative = alternative,
                                     power = NULL)
      }
      result_df$power[i] <- pwr_result$power
      
    } else if (test == "anova") {
      # ANOVA
      params <- list(...)
      groups <- if (!is.null(params$groups)) params$groups else 3
      
      if (vary == "n") {
        # val is total sample size, divide by groups to get per group
        n_per_group <- floor(val / groups)
        if (n_per_group < 2) next
        
        pwr_result <- pwr::pwr.anova.test(k = groups, 
                                         n = n_per_group, 
                                         f = effect_size,
                                         sig.level = sig_level,
                                         power = NULL)
      } else {
        pwr_result <- pwr::pwr.anova.test(k = groups, 
                                         n = n, 
                                         f = val,
                                         sig.level = sig_level,
                                         power = NULL)
      }
      result_df$power[i] <- pwr_result$power
      
    } else if (test == "correlation") {
      # Correlation
      params <- list(...)
      alternative <- if (!is.null(params$alternative)) params$alternative else "two.sided"
      
      if (vary == "n") {
        if (val < 4) next
        pwr_result <- pwr::pwr.r.test(n = val, 
                                     r = effect_size, 
                                     sig.level = sig_level,
                                     alternative = alternative,
                                     power = NULL)
      } else {
        pwr_result <- pwr::pwr.r.test(n = n, 
                                     r = val, 
                                     sig.level = sig_level,
                                     alternative = alternative,
                                     power = NULL)
      }
      result_df$power[i] <- pwr_result$power
      
    } else if (test == "chi-square") {
      # Chi-square
      params <- list(...)
      df <- if (!is.null(params$df)) params$df else 1
      
      if (vary == "n") {
        if (val < 5) next
        pwr_result <- pwr::pwr.chisq.test(N = val, 
                                         w = effect_size, 
                                         df = df,
                                         sig.level = sig_level,
                                         power = NULL)
      } else {
        pwr_result <- pwr::pwr.chisq.test(N = n, 
                                         w = val, 
                                         df = df,
                                         sig.level = sig_level,
                                         power = NULL)
      }
      result_df$power[i] <- pwr_result$power
      
    } else if (test == "regression") {
      # Regression
      params <- list(...)
      predictors <- if (!is.null(params$predictors)) params$predictors else 1
      test_predictors <- if (!is.null(params$test_predictors)) params$test_predictors else 1
      
      if (vary == "n") {
        if (val <= predictors + 1) next
        v <- val - predictors - 1  # v is error df
        
        pwr_result <- pwr::pwr.f2.test(u = test_predictors, 
                                      v = v, 
                                      f2 = effect_size,
                                      sig.level = sig_level,
                                      power = NULL)
      } else {
        v <- n - predictors - 1  # v is error df
        if (v <= 0) stop("Sample size is too small for the number of predictors")
        
        pwr_result <- pwr::pwr.f2.test(u = test_predictors, 
                                      v = v, 
                                      f2 = val,
                                      sig.level = sig_level,
                                      power = NULL)
      }
      result_df$power[i] <- pwr_result$power
    }
  }
  
  # Remove any NA values
  result_df <- result_df[!is.na(result_df$power), ]
  
  # Create power curve
  p <- ggplot2::ggplot(result_df, ggplot2::aes(x = x, y = power)) +
    ggplot2::geom_line(color = "#1F77B4", size = 1.2) +
    ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", color = "#FF7F0E") +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    ggplot2::labs(
      title = paste("Power Analysis for", toupper(test), "Test"),
      subtitle = if (vary == "n") {
        paste("Effect size =", effect_size)
      } else {
        paste("Sample size =", n)
      },
      x = x_label,
      y = "Power (1 - β)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12)
    )
  
  return(p)
}

#' Estimate Effect Size from Existing Data
#'
#' This function estimates the effect size from existing data for various test types.
#'
#' @param data A data frame containing the data
#' @param test_type Character string specifying the test type 
#'        ("t.test", "anova", "correlation", "chi-square", "regression")
#' @param vars Character vector specifying the variables to use
#' @param group_var Character string specifying the grouping variable for t-test and ANOVA
#'
#' @return A list containing effect size estimates and related information
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate some sample data
#' set.seed(123)
#' data <- data.frame(
#'   group = factor(rep(c("A", "B", "C"), each = 30)),
#'   score = c(rnorm(30, 10, 3), rnorm(30, 12, 3), rnorm(30, 15, 3)),
#'   x = rnorm(90),
#'   y = rnorm(90)
#' )
#' 
#' # Estimate effect size for t-test
#' estimate_effect_size(data, test_type = "t.test", vars = "score", group_var = "group")
#' 
#' # Estimate effect size for ANOVA
#' estimate_effect_size(data, test_type = "anova", vars = "score", group_var = "group")
#' 
#' # Estimate effect size for correlation
#' estimate_effect_size(data, test_type = "correlation", vars = c("x", "y"))
#' }
estimate_effect_size <- function(data, test_type, vars, group_var = NULL) {
  
  # Check if required packages are available
  if (!requireNamespace("effsize", quietly = TRUE)) {
    stop("Package 'effsize' needed for this function to work. Please install it.")
  }
  
  if (!requireNamespace("car", quietly = TRUE) && test_type == "anova") {
    stop("Package 'car' needed for ANOVA effect size calculations.")
  }
  
  # Validate parameters
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  valid_test_types <- c("t.test", "anova", "correlation", "chi-square", "regression")
  if (!(test_type %in% valid_test_types)) {
    stop("test_type must be one of: ", paste(valid_test_types, collapse = ", "))
  }
  
  if (!is.character(vars) || length(vars) < 1) {
    stop("vars must be a character vector with at least one variable name")
  }
  
  for (var in vars) {
    if (!(var %in% names(data))) {
      stop("Variable '", var, "' not found in data")
    }
  }
  
  if (!is.null(group_var) && !(group_var %in% names(data))) {
    stop("Group variable '", group_var, "' not found in data")
  }
  
  # Prepare result
  result <- list(test_type = test_type)
  
  # Estimate effect size based on test type
  if (test_type == "t.test") {
    if (is.null(group_var)) {
      stop("group_var must be specified for t.test")
    }
    
    # Check if grouping variable has exactly two levels for t-test
    if (length(unique(na.omit(data[[group_var]]))) != 2) {
      stop("Grouping variable must have exactly two levels for t-test")
    }
    
    # Split data by group
    groups <- split(data[[vars[1]]], data[[group_var]])
    group_names <- names(groups)
    
    # Calculate Cohen's d
    d <- effsize::cohen.d(groups[[1]], groups[[2]])
    
    result$effect_size_type <- "Cohen's d"
    result$effect_size <- abs(d$estimate)
    result$interpretation <- interpret_effect_size(abs(d$estimate), "d")
    result$confidence_interval <- c(d$conf.int[1], d$conf.int[2])
    result$group_means <- c(
      mean(groups[[1]], na.rm = TRUE),
      mean(groups[[2]], na.rm = TRUE)
    )
    names(result$group_means) <- group_names
    result$group_sds <- c(
      sd(groups[[1]], na.rm = TRUE),
      sd(groups[[2]], na.rm = TRUE)
    )
    names(result$group_sds) <- group_names
    
  } else if (test_type == "anova") {
    if (is.null(group_var)) {
      stop("group_var must be specified for ANOVA")
    }
    
    # Fit ANOVA model
    formula <- stats::as.formula(paste(vars[1], "~", group_var))
    aov_model <- stats::aov(formula, data = data)
    
    # Calculate eta-squared and convert to Cohen's f
    etasq <- car::etasq(aov_model)
    eta_squared <- as.numeric(etasq[1, 1])
    f <- sqrt(eta_squared / (1 - eta_squared))
    
    # Get group means
    group_means <- stats::aggregate(data[[vars[1]]], by = list(Group = data[[group_var]]), 
                            FUN = mean, na.rm = TRUE)
    names(group_means) <- c("Group", "Mean")
    
    result$effect_size_type <- "Cohen's f"
    result$effect_size <- f
    result$interpretation <- interpret_effect_size(f, "f")
    result$eta_squared <- eta_squared
    result$group_means <- group_means
    result$anova_summary <- summary(aov_model)
    
  } else if (test_type == "correlation") {
    if (length(vars) != 2) {
      stop("Exactly two variables must be specified for correlation")
    }
    
    # Calculate correlation
    cor_test <- stats::cor.test(data[[vars[1]]], data[[vars[2]]], 
                        use = "pairwise.complete.obs")
    
    result$effect_size_type <- "Correlation coefficient (r)"
    result$effect_size <- abs(cor_test$estimate)
    result$interpretation <- interpret_effect_size(abs(cor_test$estimate), "r")
    result$p_value <- cor_test$p.value
    result$confidence_interval <- cor_test$conf.int
    
  } else if (test_type == "chi-square") {
    if (length(vars) != 2) {
      stop("Exactly two variables must be specified for chi-square test")
    }
    
    # Create contingency table
    contingency_table <- stats::table(data[[vars[1]]], data[[vars[2]]])
    
    # Chi-square test
    chi_test <- stats::chisq.test(contingency_table)
    
    # Calculate effect size (Cramer's V)
    n <- sum(contingency_table)
    df <- min(nrow(contingency_table) - 1, ncol(contingency_table) - 1)
    cramers_v <- sqrt(chi_test$statistic / (n * df))
    
    result$effect_size_type <- "Cramer's V"
    result$effect_size <- as.numeric(cramers_v)
    result$interpretation <- interpret_effect_size(as.numeric(cramers_v), "w")  # w and v are interpreted similarly
    result$chi_square <- chi_test$statistic
    result$p_value <- chi_test$p.value
    result$df <- chi_test$parameter
    result$contingency_table <- contingency_table
    
  } else if (test_type == "regression") {
    # Assuming first variable is outcome, rest are predictors
    if (length(vars) < 2) {
      stop("At least two variables must be specified for regression")
    }
    
    # Create formula for regression
    formula <- stats::as.formula(paste(vars[1], "~", paste(vars[-1], collapse = " + ")))
    
    # Fit regression model
    model <- stats::lm(formula, data = data)
    summary_model <- summary(model)
    
    # Calculate f² = R² / (1 - R²)
    r_squared <- summary_model$r.squared
    f_squared <- r_squared / (1 - r_squared)
    
    result$effect_size_type <- "Cohen's f²"
    result$effect_size <- f_squared
    result$interpretation <- interpret_effect_size(f_squared, "f2")
    result$r_squared <- r_squared
    result$adjusted_r_squared <- summary_model$adj.r.squared
    result$model_summary <- summary_model
  }
  
  # Add class for print method
  class(result) <- c("statsaid_effect_size", "list")
  return(result)
}

#' Interpret the magnitude of an effect size
#'
#' Internal function to interpret the magnitude of an effect size.
#'
#' @param effect_size Numeric value specifying the effect size
#' @param type Character string specifying the effect size type
#'
#' @return Character string with interpretation
#' @keywords internal
interpret_effect_size <- function(effect_size, type) {
  
  if (type == "d") {
    # Cohen's d
    if (effect_size < 0.2) {
      return("Very small effect")
    } else if (effect_size < 0.5) {
      return("Small effect")
    } else if (effect_size < 0.8) {
      return("Medium effect")
    } else if (effect_size < 1.3) {
      return("Large effect")
    } else {
      return("Very large effect")
    }
  } else if (type == "f") {
    # Cohen's f
    if (effect_size < 0.1) {
      return("Very small effect")
    } else if (effect_size < 0.25) {
      return("Small effect")
    } else if (effect_size < 0.4) {
      return("Medium effect")
    } else {
      return("Large effect")
    }
  } else if (type == "r") {
    # Correlation coefficient
    if (effect_size < 0.1) {
      return("Very small effect")
    } else if (effect_size < 0.3) {
      return("Small effect")
    } else if (effect_size < 0.5) {
      return("Medium effect")
    } else if (effect_size < 0.7) {
      return("Large effect")
    } else {
      return("Very large effect")
    }
  } else if (type == "w") {
    # Cohen's w
    if (effect_size < 0.1) {
      return("Very small effect")
    } else if (effect_size < 0.3) {
      return("Small effect")
    } else if (effect_size < 0.5) {
      return("Medium effect")
    } else {
      return("Large effect")
    }
  } else if (type == "f2") {
    # Cohen's f²
    if (effect_size < 0.02) {
      return("Very small effect")
    } else if (effect_size < 0.15) {
      return("Small effect")
    } else if (effect_size < 0.35) {
      return("Medium effect")
    } else {
      return("Large effect")
    }
  } else {
    return("Unknown effect size type")
  }
}

#' Interpret the magnitude of an odds ratio
#'
#' Internal function to interpret the magnitude of an odds ratio.
#'
#' @param odds_ratio Numeric value specifying the odds ratio
#'
#' @return Character string with interpretation
#' @keywords internal
interpret_odds_ratio <- function(odds_ratio) {
  
  if (odds_ratio < 1) {
    # Convert to inverse for interpretation of values less than 1
    inverse_or <- 1 / odds_ratio
    prefix <- "Negative "
  } else {
    inverse_or <- odds_ratio
    prefix <- ""
  }
  
  if (inverse_or < 1.5) {
    return(paste0(prefix, "small effect"))
  } else if (inverse_or < 3.5) {
    return(paste0(prefix, "medium effect"))
  } else if (inverse_or < 9) {
    return(paste0(prefix, "large effect"))
  } else {
    return(paste0(prefix, "very large effect"))
  }
}

#' Get factors of a number
#'
#' Internal function to find factors of a number.
#'
#' @param n Integer
#'
#' @return Vector of factors
#' @keywords internal
get_factors <- function(n) {
  
  factors <- c()
  for (i in 1:sqrt(n)) {
    if (n %% i == 0) {
      factors <- c(factors, i, n/i)
    }
  }
  
  return(sort(unique(factors)))
}

#' Print method for statsaid_power objects
#'
#' @param x An object of class statsaid_power
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_power <- function(x, ...) {
  cat("StatsAid Power Analysis\n")
  cat("=======================\n\n")
  
  # Print test-specific information
  if (!is.null(x$test_type)) {
    cat("Test Type:", x$test_type, "\n")
  }
  
  # Print common parameters
  cat("Significance Level (α):", x$sig_level, "\n")
  cat("Desired Power (1 - β):", x$power, "\n")
  
  # Print effect size information
  if (!is.null(x$effect_size)) {
    if (!is.null(x$effect_size_type)) {
      cat("Effect Size (", x$effect_size_type, "): ", x$effect_size, "\n", sep = "")
    } else {
      cat("Effect Size:", x$effect_size, "\n")
    }
    
    if (!is.null(x$effect_size_interpretation)) {
      cat("Interpretation:", x$effect_size_interpretation, "\n")
    }
  }
  
  # Print odds ratio for logistic regression
  if (!is.null(x$odds_ratio)) {
    cat("Odds Ratio:", x$odds_ratio, "\n")
    
    if (!is.null(x$p0) && !is.null(x$p1)) {
      cat("Baseline Probability (p0):", x$p0, "\n")
      cat("Alternative Probability (p1):", x$p1, "\n")
    }
  }
  
  # Print test-specific parameters
  if (!is.null(x$groups)) {
    cat("Number of Groups:", x$groups, "\n")
  }
  
  if (!is.null(x$df)) {
    cat("Degrees of Freedom:", x$df, "\n")
    if (!is.null(x$contingency_hint)) {
      cat("Contingency Table:", x$contingency_hint, "\n")
    }
  }
  
  if (!is.null(x$predictors)) {
    cat("Number of Predictors:", x$predictors, "\n")
    if (!is.null(x$test_predictors) && x$test_predictors != x$predictors) {
      cat("Number of Tested Predictors:", x$test_predictors, "\n")
    }
  }
  
  if (!is.null(x$alternative)) {
    cat("Alternative Hypothesis:", x$alternative, "\n")
  }
  
  # Print allocation ratio for unequal group sizes
  if (!is.null(x$allocation_ratio) && x$allocation_ratio != 1) {
    cat("Allocation Ratio (n2/n1):", x$allocation_ratio, "\n")
  }
  
  # Print sample size results
  cat("\nSample Size Results:\n")
  
  if (!is.null(x$n_per_group)) {
    cat("Sample Size per Group:", x$n_per_group, "\n")
  }
  
  if (!is.null(x$n1) && !is.null(x$n2)) {
    cat("Sample Size Group 1:", x$n1, "\n")
    cat("Sample Size Group 2:", x$n2, "\n")
  }
  
  cat("Total Sample Size:", x$total_n, "\n")
  
  # Print additional notes
  if (!is.null(x$note)) {
    cat("\nNote:", x$note, "\n")
  }
  
  if (!is.null(x$rule_of_thumb)) {
    cat("\n", x$rule_of_thumb, "\n")
  }
  
  invisible(x)
}

#' Print method for statsaid_effect_size objects
#'
#' @param x An object of class statsaid_effect_size
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_effect_size <- function(x, ...) {
  cat("StatsAid Effect Size Estimation\n")
  cat("==============================\n\n")
  
  cat("Test Type:", x$test_type, "\n")
  cat("Effect Size Type:", x$effect_size_type, "\n")
  cat("Effect Size:", round(x$effect_size, 4), "\n")
  cat("Interpretation:", x$interpretation, "\n")
  
  if (!is.null(x$confidence_interval)) {
    cat("95% Confidence Interval: [", 
        round(x$confidence_interval[1], 4), ", ", 
        round(x$confidence_interval[2], 4), "]\n", sep = "")
  }
  
  if (!is.null(x$p_value)) {
    cat("p-value:", format.pval(x$p_value, digits = 4), "\n")
  }
  
  # Test-specific additional information
  if (x$test_type == "t.test" && !is.null(x$group_means)) {
    cat("\nGroup Means:\n")
    for (i in 1:length(x$group_means)) {
      cat("  ", names(x$group_means)[i], ": ", round(x$group_means[i], 4), 
          " (SD = ", round(x$group_sds[i], 4), ")\n", sep = "")
    }
  } else if (x$test_type == "anova") {
    cat("\nEta-squared:", round(x$eta_squared, 4), "\n")
    
    if (!is.null(x$group_means)) {
      cat("\nGroup Means:\n")
      print(x$group_means)
    }
  } else if (x$test_type == "regression") {
    cat("\nR-squared:", round(x$r_squared, 4), "\n")
    cat("Adjusted R-squared:", round(x$adjusted_r_squared, 4), "\n")
  }
  
  invisible(x)
}

#' Recommend study design based on research question and constraints
#'
#' This function provides recommendations for study design based on the research
#' question and practical constraints.
#'
#' @param outcome_type Character string specifying the outcome type ("continuous", "binary", "count", "time-to-event")
#' @param predictor_type Character string specifying the main predictor type ("continuous", "categorical", "mixed")
#' @param setting Character string specifying the research setting ("observational", "experimental", "survey")
#' @param constraints List of constraints (e.g., budget, time)
#' @param target_power Numeric value specifying the desired power (default = 0.8)
#' @param expected_effect Character string specifying the expected effect size ("small", "medium", "large")
#'
#' @return A list containing study design recommendations
#' @export
#'
#' @examples
#' \dontrun{
#' # Get recommendations for an experimental study with continuous outcome
#' recommend_study_design(
#'   outcome_type = "continuous",
#'   predictor_type = "categorical",
#'   setting = "experimental",
#'   expected_effect = "medium"
#' )
#' }
recommend_study_design <- function(outcome_type, predictor_type, setting, 
                                  constraints = list(), target_power = 0.8,
                                  expected_effect = "medium") {
  
  # Validate parameters
  valid_outcome_types <- c("continuous", "binary", "count", "time-to-event", "ordinal")
  if (!(outcome_type %in% valid_outcome_types)) {
    stop("outcome_type must be one of: ", paste(valid_outcome_types, collapse = ", "))
  }
  
  valid_predictor_types <- c("continuous", "categorical", "mixed")
  if (!(predictor_type %in% valid_predictor_types)) {
    stop("predictor_type must be one of: ", paste(valid_predictor_types, collapse = ", "))
  }
  
  valid_settings <- c("observational", "experimental", "survey")
  if (!(setting %in% valid_settings)) {
    stop("setting must be one of: ", paste(valid_settings, collapse = ", "))
  }
  
  valid_effect_sizes <- c("small", "medium", "large")
  if (!(expected_effect %in% valid_effect_sizes)) {
    stop("expected_effect must be one of: ", paste(valid_effect_sizes, collapse = ", "))
  }
  
  # Convert effect size descriptor to numeric
  effect_size_values <- list(
    # for Cohen's d
    d = list(small = 0.2, medium = 0.5, large = 0.8),
    # for Cohen's f
    f = list(small = 0.1, medium = 0.25, large = 0.4),
    # for correlation
    r = list(small = 0.1, medium = 0.3, large = 0.5),
    # for odds ratio
    or = list(small = 1.5, medium = 3.0, large = 5.0)
  )
  
  # Initialize results
  recommended_design <- list(
    outcome_type = outcome_type,
    predictor_type = predictor_type,
    setting = setting,
    expected_effect = expected_effect,
    target_power = target_power,
    design_options = list(),
    sample_size = list(),
    analysis_methods = list(),
    potential_issues = character(),
    recommendations = list()
  )
  
  # Determine appropriate study designs based on setting and types
  if (setting == "experimental") {
    if (predictor_type == "categorical") {
      recommended_design$design_options$primary <- "Randomized Controlled Trial (RCT)"
      recommended_design$design_options$variations <- c(
        "Parallel groups design",
        "Crossover design (if appropriate)",
        "Factorial design (for multiple interventions)"
      )
      
      # Sample size calculation
      if (outcome_type == "continuous") {
        # Use t-test power calculation for basic estimate
        d <- effect_size_values$d[[expected_effect]]
        sample_size <- calculate_t_test_sample_size(d = d, power = target_power)
        recommended_design$sample_size$estimate <- sample_size$total_n
        recommended_design$sample_size$calculation_method <- "t-test (Cohen's d)"
        recommended_design$sample_size$effect_size_used <- d
        
        # Analysis methods
        recommended_design$analysis_methods <- c(
          "t-test (for 2 groups)",
          "ANOVA (for >2 groups)",
          "ANCOVA (to adjust for covariates)",
          "Mixed models (for repeated measures)"
        )
        
      } else if (outcome_type == "binary") {
        # Use proportion difference for binary outcomes
        or <- effect_size_values$or[[expected_effect]]
        
        # Convert OR to probabilities (assuming baseline of 0.2)
        p0 <- 0.2
        odds0 <- p0 / (1 - p0)
        odds1 <- odds0 * or
        p1 <- odds1 / (1 + odds1)
        
        # Estimate sample size
        recommended_design$sample_size$estimate <- ceiling(4 * (100 / (p1 - p0)^2))
        recommended_design$sample_size$calculation_method <- "Difference in proportions"
        recommended_design$sample_size$effect_size_used <- or
        recommended_design$sample_size$baseline_rate <- p0
        recommended_design$sample_size$expected_rate <- p1
        
        # Analysis methods
        recommended_design$analysis_methods <- c(
          "Chi-square test",
          "Fisher's exact test (small samples)",
          "Logistic regression (to adjust for covariates)"
        )
      } else if (outcome_type == "count") {
        # Use Poisson regression approach
        recommended_design$sample_size$calculation_method <- "Specialized method needed"
        recommended_design$sample_size$note <- "For count data, consider consulting a statistician for precise calculations"
        
        # Analysis methods
        recommended_design$analysis_methods <- c(
          "Poisson regression",
          "Negative binomial regression (for overdispersion)"
        )
      } else if (outcome_type == "time-to-event") {
        # Survival analysis approach
        recommended_design$sample_size$calculation_method <- "Specialized method needed"
        recommended_design$sample_size$note <- "For survival data, sample size depends on expected event rates and follow-up time"
        
        # Analysis methods
        recommended_design$analysis_methods <- c(
          "Kaplan-Meier curves with log-rank test",
          "Cox proportional hazards regression"
        )
      }
      
    } else if (predictor_type == "continuous") {
      recommended_design$design_options$primary <- "Dose-response study"
      recommended_design$design_options$variations <- c(
        "Continuous dose variation",
        "Grouped dose levels"
      )
      
      # Sample size calculation based on correlation or regression
      r <- effect_size_values$r[[expected_effect]]
      sample_size <- calculate_correlation_sample_size(r = r, power = target_power)
      recommended_design$sample_size$estimate <- sample_size$total_n
      recommended_design$sample_size$calculation_method <- "Correlation (r)"
      recommended_design$sample_size$effect_size_used <- r
      
      # Analysis methods
      if (outcome_type == "continuous") {
        recommended_design$analysis_methods <- c(
          "Linear regression",
          "Polynomial regression (for non-linear relationships)"
        )
      } else if (outcome_type == "binary") {
        recommended_design$analysis_methods <- c(
          "Logistic regression"
        )
      } else if (outcome_type == "count") {
        recommended_design$analysis_methods <- c(
          "Poisson regression",
          "Negative binomial regression"
        )
      } else if (outcome_type == "time-to-event") {
        recommended_design$analysis_methods <- c(
          "Cox proportional hazards regression"
        )
      }
    } else if (predictor_type == "mixed") {
      recommended_design$design_options$primary <- "Factorial design or complex intervention study"
      
      # For mixed predictors, use more complex power calculation
      f <- effect_size_values$f[[expected_effect]]
      predictors <- 3  # Assuming a few predictors
      sample_size <- calculate_regression_sample_size(f2 = f^2, predictors = predictors, power = target_power)
      recommended_design$sample_size$estimate <- sample_size$total_n
      recommended_design$sample_size$calculation_method <- "Multiple regression (f)"
      recommended_design$sample_size$effect_size_used <- f
      
      # Analysis methods - more complex
      if (outcome_type == "continuous") {
        recommended_design$analysis_methods <- c(
          "Multiple regression",
          "ANCOVA",
          "Mixed models"
        )
      } else if (outcome_type == "binary") {
        recommended_design$analysis_methods <- c(
          "Logistic regression"
        )
      } else if (outcome_type == "count") {
        recommended_design$analysis_methods <- c(
          "Poisson/Negative binomial regression"
        )
      } else if (outcome_type == "time-to-event") {
        recommended_design$analysis_methods <- c(
          "Cox regression with multiple predictors"
        )
      }
    }
    
  } else if (setting == "observational") {
    if (predictor_type == "categorical") {
      recommended_design$design_options$primary <- "Cohort or case-control study"
      recommended_design$design_options$variations <- c(
        "Prospective cohort (following subjects forward in time)",
        "Retrospective cohort (using historical data)",
        "Case-control (comparing cases and controls)",
        "Cross-sectional (single time point assessment)"
      )
      
      # Add warning about confounding
      recommended_design$potential_issues <- c(
        recommended_design$potential_issues,
        "Confounding variables may bias results - consider matching or statistical adjustment",
        "Selection bias is a concern - ensure representative sampling"
      )
      
      # Sample size similar to experimental but with adjustment for confounding
      if (outcome_type == "continuous") {
        d <- effect_size_values$d[[expected_effect]]
        sample_size <- calculate_t_test_sample_size(d = d, power = target_power)
        # Increase by 20% to account for confounding
        recommended_design$sample_size$estimate <- ceiling(sample_size$total_n * 1.2)
        recommended_design$sample_size$calculation_method <- "t-test with 20% increase for confounding"
        recommended_design$sample_size$effect_size_used <- d
      } else if (outcome_type == "binary") {
        or <- effect_size_values$or[[expected_effect]]
        recommended_design$sample_size$calculation_method <- "Logistic regression"
        recommended_design$sample_size$note <- "For case-control studies, matching can improve efficiency"
        
        # Very rough estimate based on rule of thumb
        events_per_variable <- 10
        predictors <- 5  # Assuming exposure plus confounders
        events_needed <- events_per_variable * predictors
        
        # Assuming 20% event rate
        recommended_design$sample_size$estimate <- ceiling(events_needed / 0.2)
      }
      
    } else if (predictor_type %in% c("continuous", "mixed")) {
      recommended_design$design_options$primary <- "Cohort study"
      
      # For continuous or mixed predictors in observational studies
      recommended_design$potential_issues <- c(
        recommended_design$potential_issues,
        "Ensure adequate range and variation in predictor variables",
        "Consider non-linear relationships",
        "Ensure appropriate adjustment for confounders"
      )
      
      # Sample size based on regression
      f <- effect_size_values$f[[expected_effect]]
      predictors <- 5  # Exposure plus confounders
      sample_size <- calculate_regression_sample_size(f2 = f^2, predictors = predictors, power = target_power)
      recommended_design$sample_size$estimate <- sample_size$total_n
      recommended_design$sample_size$calculation_method <- "Multiple regression accounting for confounders"
      recommended_design$sample_size$effect_size_used <- f
    }
    
  } else if (setting == "survey") {
    recommended_design$design_options$primary <- "Cross-sectional survey"
    recommended_design$design_options$variations <- c(
      "Simple random sampling",
      "Stratified sampling",
      "Cluster sampling"
    )
    
    # For surveys, precision-based sample size
    recommended_design$sample_size$calculation_method <- "Margin of error approach"
    
    if (outcome_type == "continuous") {
      # Assuming standard deviation of 1
      margin_of_error <- 0.2  # Desired precision
      recommended_design$sample_size$estimate <- ceiling((1.96 / margin_of_error)^2)
      recommended_design$sample_size$note <- "Based on 95% confidence and margin of error of 0.2 standard deviations"
    } else if (outcome_type == "binary") {
      # For proportions, most conservative is p=0.5
      margin_of_error <- 0.05  # 5% precision
      recommended_design$sample_size$estimate <- ceiling(0.25 * (1.96 / margin_of_error)^2)
      recommended_design$sample_size$note <- "Based on 95% confidence and 5% margin of error for proportions"
    }
    
    recommended_design$potential_issues <- c(
      recommended_design$potential_issues,
      "Ensure representative sampling",
      "Consider response rate in final sample size calculation",
      "Address potential sampling and non-response bias"
    )
  }
  
  # General recommendations
  recommended_design$recommendations <- c(
    paste0("Aim for a minimum sample size of ", recommended_design$sample_size$estimate, " participants"),
    "Consider a pilot study to refine methodology and confirm feasibility",
    "Plan for potential attrition by increasing initial sample size",
    "Use validated measures and standardized protocols",
    "Pre-register your study design and analysis plan"
  )
  
  # Add constraints-based recommendations
  if (!is.null(constraints$budget) && constraints$budget == "limited") {
    recommended_design$recommendations <- c(
      recommended_design$recommendations,
      "Consider a more focused research question to reduce required sample size",
      "Explore options for efficient data collection methods"
    )
  }
  
  if (!is.null(constraints$time) && constraints$time == "limited") {
    recommended_design$recommendations <- c(
      recommended_design$recommendations,
      "Consider cross-sectional rather than longitudinal design",
      "Focus on outcomes that can be measured quickly"
    )
  }
  
  class(recommended_design) <- c("statsaid_study_design", "list")
  return(recommended_design)
}

#' Print method for statsaid_study_design objects
#'
#' @param x An object of class statsaid_study_design
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_study_design <- function(x, ...) {
  cat("StatsAid Study Design Recommendations\n")
  cat("====================================\n\n")
  
  cat("Research Characteristics:\n")
  cat("  Outcome Type:", x$outcome_type, "\n")
  cat("  Predictor Type:", x$predictor_type, "\n")
  cat("  Research Setting:", x$setting, "\n")
  cat("  Expected Effect Size:", x$expected_effect, "\n")
  cat("  Target Power:", x$target_power, "\n\n")
  
  cat("Recommended Study Design:\n")
  cat("  Primary Design:", x$design_options$primary, "\n")
  
  if (length(x$design_options$variations) > 0) {
    cat("  Design Variations:\n")
    for (variation in x$design_options$variations) {
      cat("    - ", variation, "\n", sep = "")
    }
  }
  
  cat("\nSample Size Recommendation:\n")
  cat("  Estimated Required N:", x$sample_size$estimate, "\n")
  cat("  Calculation Method:", x$sample_size$calculation_method, "\n")
  
  if (!is.null(x$sample_size$note)) {
    cat("  Note:", x$sample_size$note, "\n")
  }
  
  cat("\nRecommended Analysis Methods:\n")
  for (method in x$analysis_methods) {
    cat("  - ", method, "\n", sep = "")
  }
  
  if (length(x$potential_issues) > 0) {
    cat("\nPotential Issues to Address:\n")
    for (issue in x$potential_issues) {
      cat("  - ", issue, "\n", sep = "")
    }
  }
  
  cat("\nKey Recommendations:\n")
  for (recommendation in x$recommendations) {
    cat("  - ", recommendation, "\n", sep = "")
  }
  
  invisible(x)
}