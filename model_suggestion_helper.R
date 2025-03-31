# Helper function for suggesting statistical models
# This standalone version handles both parameter naming conventions

suggest_statistical_models <- function(data, design_type = NULL) {
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
  
  # Get basic data characteristics
  num_samples <- nrow(data)
  num_features <- ncol(data)
  has_categorical <- any(sapply(data, function(x) is.factor(x) || is.character(x)))
  
  # Default packages
  suggestions$packages$r <- c("stats", "lme4", "ggplot2")
  suggestions$packages$python <- c("scikit-learn", "statsmodels", "scipy")
  
  # Study design specific suggestions
  if (is.null(design_type)) {
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
  } else if (design_type == "case_control") {
    suggestions$models <- c(suggestions$models,
                          "Logistic regression",
                          "Conditional logistic regression",
                          "Fisher's exact test (small sample size)")
    suggestions$packages$r <- c(suggestions$packages$r, "survival", "epitools", "epiR")
    suggestions$packages$python <- c(suggestions$packages$python, "lifelines")
  } else if (design_type == "cohort") {
    suggestions$models <- c(suggestions$models,
                          "Cox proportional hazards",
                          "Kaplan-Meier survival analysis",
                          "Poisson regression",
                          "Negative binomial regression")
    suggestions$packages$r <- c(suggestions$packages$r, "survival", "survminer", "MASS")
    suggestions$packages$python <- c(suggestions$packages$python, "lifelines", "statsmodels.discrete_models")
  } else if (design_type == "cross_sectional") {
    suggestions$models <- c(suggestions$models,
                          "Chi-square test of independence",
                          "Fisher's exact test",
                          "Logistic regression",
                          "Multinomial logistic regression")
    suggestions$packages$r <- c(suggestions$packages$r, "vcd", "nnet", "MASS")
  } else if (design_type == "longitudinal") {
    suggestions$models <- c(suggestions$models,
                          "Mixed effects models",
                          "Generalized estimating equations (GEE)",
                          "Repeated measures ANOVA")
    suggestions$packages$r <- c(suggestions$packages$r, "lme4", "nlme", "geepack")
    suggestions$packages$python <- c(suggestions$packages$python, "statsmodels.genmod.gee")
  } else if (design_type == "rct") {
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
  
  # Format and print results
  cat("Suggested Statistical Models\n")
  cat("===========================\n\n")
  
  # Models section
  if (length(suggestions$models) > 0) {
    cat("Suggested Models:\n")
    for (model in suggestions$models) {
      cat("  - ", model, "\n", sep = "")
    }
    cat("\n")
  }
  
  # R packages section
  if (length(suggestions$packages$r) > 0) {
    cat("Recommended R Packages:\n")
    for (pkg in suggestions$packages$r) {
      cat("  - ", pkg, "\n", sep = "")
    }
    cat("\n")
  }
  
  # Python packages section
  if (length(suggestions$packages$python) > 0) {
    cat("Equivalent Python Packages:\n")
    for (pkg in suggestions$packages$python) {
      cat("  - ", pkg, "\n", sep = "")
    }
    cat("\n")
  }
  
  # Return suggestions invisibly
  invisible(suggestions)
}

# Instructions for use
cat("To get model suggestions, run this code:\n\n")
cat("source('model_suggestion_helper.R')\n")
cat("suggest_statistical_models(your_data, design_type = 'case_control')\n\n")
cat("# Available design types include:\n")
cat("# - case_control\n")
cat("# - cohort\n")
cat("# - cross_sectional\n")
cat("# - longitudinal\n")
cat("# - rct\n")