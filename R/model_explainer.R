#' Enhanced model suggestions with detailed explanations
#'
#' This function extends the suggest_models() functionality to provide more detailed
#' explanations about why specific models are recommended for your data and design.
#'
#' @param data A data frame or tibble
#' @param outcome_var Optional character string specifying the outcome variable name
#' @param design Character string specifying the study design, or a design specification object
#' @param include_alternatives Logical, whether to include alternative models (default: TRUE)
#' @param include_code Logical, whether to include implementation code examples (default: TRUE)
#'
#' @return A list of class 'model_explanations' containing suggested models with detailed explanations
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   a = c(1, 2, 3, 4),
#'   b = c("x", "y", "z", "x")
#' )
#' explain_model_suggestions(data, outcome_var = "a", design = "cross_sectional")
#' }
explain_model_suggestions <- function(data, outcome_var = NULL, design = NULL, 
                                     include_alternatives = TRUE,
                                     include_code = TRUE) {
  
  # First get the detailed model suggestions
  suggestions <- suggest_models(data, outcome_var = outcome_var, 
                               design = design, detailed = TRUE)
  
  # Create the explanations structure
  explanations <- list(
    primary_model = list(),
    alternatives = list(),
    why_recommended = "",
    advantages = list(),
    limitations = list(),
    best_practices = list(),
    common_pitfalls = list(),
    references = list()
  )
  
  # Add the primary model information
  if (!is.null(suggestions$models$primary)) {
    primary <- suggestions$models$primary
    explanations$primary_model <- primary
    
    # Add more detailed explanations based on the model type
    model_type <- primary$name
    explanations <- add_model_explanations(explanations, model_type, outcome_var, design)
  }
  
  # Add alternative models if requested
  if (include_alternatives && length(suggestions$models$alternatives) > 0) {
    explanations$alternatives <- suggestions$models$alternatives
    
    # Add comparison with primary model
    explanations$comparison <- "Comparison with primary model:"
    for (i in seq_along(suggestions$models$alternatives)) {
      alt <- suggestions$models$alternatives[[i]]
      explanations$comparison <- c(
        explanations$comparison,
        paste0("- ", alt$name, ": ", generate_comparison(primary$name, alt$name))
      )
    }
  }
  
  # Add simplified code examples if requested
  if (include_code && !is.null(primary$implementation)) {
    explanations$code_examples <- list(
      r = primary$implementation
    )
    
    # Add explanatory comments to the code
    explanations$code_examples$r_explained <- add_code_explanation(
      primary$implementation, primary$name
    )
  }
  
  # Add model diagnostics
  if (length(suggestions$diagnostics) > 0) {
    explanations$diagnostics <- suggestions$diagnostics
    explanations$why_diagnostics <- "These diagnostics are important because they help validate the model assumptions and ensure the results are reliable."
  }
  
  # Add package recommendations
  if (length(suggestions$packages$r) > 0) {
    explanations$packages <- suggestions$packages$r
    explanations$package_details <- list()
    
    # Add details for common packages
    for (pkg in suggestions$packages$r) {
      explanations$package_details[[pkg]] <- get_package_details(pkg)
    }
  }
  
  # Extract warnings and considerations
  if (length(suggestions$warnings) > 0) {
    explanations$considerations <- suggestions$warnings
  }
  
  # Set the class for printing
  class(explanations) <- c("model_explanations", "list")
  
  return(explanations)
}

#' Add detailed explanations based on model type
#'
#' @param explanations The explanations list to enhance
#' @param model_type The name of the model
#' @param outcome_var The outcome variable name (if available)
#' @param design The study design (if available)
#'
#' @return Enhanced explanations list
#' @keywords internal
add_model_explanations <- function(explanations, model_type, outcome_var, design) {
  
  # Linear regression explanations
  if (grepl("Linear Regression|Multiple Linear Regression", model_type)) {
    explanations$why_recommended <- paste0(
      "Linear regression is recommended for your data because it's appropriate for analyzing the relationship ",
      "between continuous predictors and a continuous outcome variable. ",
      if (!is.null(outcome_var)) paste0("Your outcome variable '", outcome_var, "' appears to be continuous, ") else "",
      "Linear regression allows you to model the direct effects of multiple independent variables, ",
      "control for confounders, and test hypotheses about relationships in your data."
    )
    
    explanations$advantages <- list(
      "Interpretability" = "Coefficients have clear interpretations as the change in the outcome associated with a one-unit change in the predictor, holding other variables constant.",
      "Statistical inference" = "Provides p-values, confidence intervals, and effect sizes that are well understood and widely accepted.",
      "Flexibility" = "Can accommodate continuous and categorical predictors (through dummy variables).",
      "Efficiency" = "Computationally efficient, even with large datasets.",
      "Prediction" = "Can be used to predict outcomes for new data."
    )
    
    explanations$limitations <- list(
      "Linearity assumption" = "Assumes relationships between variables are linear, which may not always be the case.",
      "Independence assumption" = "Assumes observations are independent of each other.",
      "Homoscedasticity" = "Assumes constant variance of errors across all levels of predictors.",
      "Normality" = "For inference, assumes errors are normally distributed.",
      "No multicollinearity" = "Problems can arise when predictors are highly correlated with each other."
    )
    
    explanations$best_practices <- list(
      "Residual analysis" = "Always check residual plots to verify assumptions.",
      "Outlier detection" = "Identify and handle influential observations appropriately.",
      "Feature scaling" = "Consider standardizing predictors for better interpretation.",
      "Transformation" = "Transform skewed variables if necessary.",
      "Interaction terms" = "Consider including interactions between predictors if relationships are complex."
    )
    
    explanations$common_pitfalls <- list(
      "Overfitting" = "Including too many predictors relative to sample size.",
      "Omitted variable bias" = "Failing to include important confounding variables.",
      "Extrapolation" = "Making predictions outside the range of the training data.",
      "Causality claims" = "Inferring causal relationships from observational data without caution."
    )
    
    explanations$references <- list(
      "Fox, J. (2015). Applied Regression Analysis and Generalized Linear Models.",
      "James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An Introduction to Statistical Learning.",
      "UCLA Statistical Consulting: https://stats.oarc.ucla.edu/r/dae/regression-analysis/"
    )
  }
  
  # Logistic regression explanations
  else if (grepl("Logistic Regression", model_type)) {
    explanations$why_recommended <- paste0(
      "Logistic regression is recommended for your data because it's appropriate for modeling binary outcomes. ",
      if (!is.null(outcome_var)) paste0("Your outcome variable '", outcome_var, "' appears to be binary, ") else "",
      if (!is.null(design) && design == "case_control") 
        "It's particularly suitable for case-control study designs. " else "",
      "Logistic regression allows you to estimate the probability of an outcome occurring given a set of predictors, ",
      "and express the results in terms of odds ratios, which are intuitive measures of association."
    )
    
    explanations$advantages <- list(
      "Binary outcome modeling" = "Specifically designed for binary dependent variables.",
      "Odds ratios" = "Produces easily interpretable odds ratios to measure associations.",
      "No normality assumption" = "Does not require the outcome to follow a normal distribution.",
      "Flexibility" = "Can include continuous and categorical predictors.",
      "Established method" = "Widely accepted in many fields and well-understood."
    )
    
    explanations$limitations <- list(
      "Sample size" = "Requires adequate sample size, especially for rare outcomes.",
      "Complete separation" = "May fail to converge with perfect or quasi-complete separation.",
      "Non-linearity" = "Assumes linear relationship between predictors and log-odds of outcome.",
      "Independence" = "Assumes independent observations."
    )
    
    explanations$best_practices <- list(
      "Variable selection" = "Carefully select predictors based on substantive knowledge.",
      "Interaction assessment" = "Test for interactions between key predictors.",
      "Goodness-of-fit" = "Assess model fit using tests like Hosmer-Lemeshow.",
      "Validation" = "Validate predictions using ROC curves, sensitivity, specificity."
    )
    
    explanations$common_pitfalls <- list(
      "Class imbalance" = "Poor performance with highly imbalanced outcomes.",
      "Overfitting" = "Including too many predictors relative to outcome events.",
      "Multicollinearity" = "Correlated predictors can lead to unstable estimates.",
      "Categorizing continuous predictors" = "Loss of information when continuous variables are categorized."
    )
    
    explanations$references <- list(
      "Hosmer, D. W., & Lemeshow, S. (2000). Applied Logistic Regression.",
      "Agresti, A. (2012). Categorical Data Analysis.",
      "UCLA Statistical Consulting: https://stats.oarc.ucla.edu/r/dae/logit-regression/"
    )
  }
  
  # Linear Mixed Models explanations
  else if (grepl("Linear Mixed Model|Mixed Effect", model_type)) {
    explanations$why_recommended <- paste0(
      "Linear Mixed Models are recommended for your data because ",
      if (!is.null(design) && grepl("longitudinal|repeated", design)) 
        "your design involves repeated measures or longitudinal data. " 
      else "your data appears to have a hierarchical or clustered structure. ",
      "Mixed models account for the non-independence of observations by including both fixed and random effects. ",
      "This approach properly handles within-subject correlation and allows for modeling individual differences."
    )
    
    explanations$advantages <- list(
      "Handles clustered data" = "Properly accounts for non-independent observations.",
      "Flexible modeling" = "Can model both within-subject and between-subject effects.",
      "Missing data" = "Can handle unbalanced designs and missing data better than repeated measures ANOVA.",
      "Individual differences" = "Can model random slopes to assess variability in effects across subjects.",
      "Efficiency" = "Uses all available data rather than requiring complete cases."
    )
    
    explanations$limitations <- list(
      "Complexity" = "More complex to specify, estimate, and interpret than simple regression.",
      "Convergence issues" = "May encounter convergence problems with complex random effects structures.",
      "Computational intensity" = "Can be computationally intensive for large datasets.",
      "Distributional assumptions" = "Assumes normality of residuals and random effects."
    )
    
    explanations$best_practices <- list(
      "Model building approach" = "Start with simpler models and gradually add complexity.",
      "Random effects structure" = "Consider which random effects are theoretically justified.",
      "Covariance structure" = "Choose appropriate covariance structure for random effects.",
      "Model comparison" = "Use likelihood ratio tests or information criteria for model selection.",
      "Residual diagnostics" = "Check residuals at each level of the model."
    )
    
    explanations$common_pitfalls <- list(
      "Overparameterization" = "Specifying too complex random effects structure for the data.",
      "Ignoring model assumptions" = "Not checking residual normality and heteroscedasticity.",
      "Misinterpreting coefficients" = "Confusing between-subject and within-subject effects.",
      "p-value estimation" = "Debates about degrees of freedom for significance testing."
    )
    
    explanations$references <- list(
      "Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting Linear Mixed-Effects Models Using lme4.",
      "Pinheiro, J. C., & Bates, D. M. (2000). Mixed-Effects Models in S and S-PLUS.",
      "Snijders, T. A., & Bosker, R. J. (2011). Multilevel Analysis: An Introduction to Basic and Advanced Multilevel Modeling."
    )
  }
  
  # Generalized Linear Mixed Models (GLMM) for binary outcomes
  else if (grepl("Generalized Linear Mixed Model.*Logistic", model_type)) {
    explanations$why_recommended <- paste0(
      "A Generalized Linear Mixed Model with logistic link is recommended because your data involves ",
      "both a binary outcome and a clustered/hierarchical structure. This combines the benefits of ",
      "logistic regression for binary outcomes with mixed effects to account for non-independent observations."
    )
    
    explanations$advantages <- list(
      "Binary outcomes in clusters" = "Properly models binary outcomes with clustered or repeated measures data.",
      "Subject-specific effects" = "Estimates both population (fixed) and subject-specific (random) effects.",
      "Flexible correlation structures" = "Can accommodate various within-cluster correlation patterns.",
      "Missing data handling" = "Better handles unbalanced designs than traditional approaches."
    )
    
    explanations$limitations <- list(
      "Computational complexity" = "More computationally intensive than simpler models.",
      "Convergence issues" = "May face convergence problems with complex random effects or sparse data.",
      "Interpretation complexity" = "Results more complex to interpret than simple logistic regression.",
      "Sample size requirements" = "Requires sufficient data at each level for reliable estimation."
    )
    
    explanations$references <- list(
      "Bolker, B. M., et al. (2009). Generalized linear mixed models: a practical guide for ecology and evolution.",
      "Agresti, A. (2015). Foundations of Linear and Generalized Linear Models."
    )
  }
  
  # Cox Proportional Hazards for survival analysis
  else if (grepl("Cox Proportional Hazards", model_type)) {
    explanations$why_recommended <- "Cox Proportional Hazards model is recommended because your study appears to involve time-to-event data or survival analysis. This semi-parametric approach is the most widely used method for analyzing the effect of several variables on survival time."
    
    explanations$advantages <- list(
      "Handles censoring" = "Properly accounts for right-censored data common in survival analysis.",
      "Semi-parametric" = "Does not require specification of the baseline hazard function.",
      "Interpretable" = "Hazard ratios are easily interpretable as relative risks.",
      "Flexibility" = "Can incorporate time-varying covariates and stratification."
    )
    
    explanations$limitations <- list(
      "Proportional hazards assumption" = "Assumes the hazard ratio is constant over time.",
      "No absolute risk estimation" = "Provides relative rather than absolute risk measures.",
      "Ties handling" = "Different approximations needed when multiple events occur at the same time.",
      "Influential observations" = "Sensitive to outliers and influential observations."
    )
    
    explanations$best_practices <- list(
      "PH assumption testing" = "Always check the proportional hazards assumption using Schoenfeld residuals.",
      "Stratification" = "Consider stratification for variables violating the PH assumption.",
      "Competing risks" = "Account for competing risks when appropriate.",
      "Extended models" = "Consider time-dependent coefficients if PH assumption is violated."
    )
    
    explanations$references <- list(
      "Cox, D. R. (1972). Regression models and life-tables.",
      "Therneau, T. M., & Grambsch, P. M. (2000). Modeling Survival Data: Extending the Cox Model.",
      "Klein, J. P., & Moeschberger, M. L. (2003). Survival Analysis: Techniques for Censored and Truncated Data."
    )
  }
  
  # Add generic explanations for any other model types
  else {
    explanations$why_recommended <- paste0(
      "The ", model_type, " is recommended for your data based on its structure and characteristics. ",
      "This statistical approach is well-suited to address your research questions ",
      "while accounting for the specific features of your dataset."
    )
    
    # Add general advantages and limitations
    explanations$advantages <- list(
      "Appropriateness" = "Selected based on your data structure and research questions.",
      "Statistical validity" = "Provides valid statistical inference under the stated assumptions.",
      "Established methodology" = "Uses well-established statistical methods with supporting literature."
    )
    
    explanations$limitations <- list(
      "Assumptions" = "Like all statistical models, requires certain assumptions to be met.",
      "Interpretation" = "Results should be interpreted within the context of your study design.",
      "Data quality" = "The quality of results depends on the quality of input data."
    )
    
    explanations$best_practices <- list(
      "Model validation" = "Validate model assumptions using appropriate diagnostic tools.",
      "Sensitivity analysis" = "Consider alternative specifications to check robustness.",
      "Context" = "Interpret results in the context of your research domain."
    )
  }
  
  return(explanations)
}

#' Add explanatory comments to code
#'
#' @param code_str The code string to enhance
#' @param model_type The name of the model
#'
#' @return Enhanced code with comments
#' @keywords internal
add_code_explanation <- function(code_str, model_type) {
  # We'll add model-specific explanations as comments to the code
  
  # Basic pattern for different models
  if (grepl("lm\\(", code_str)) {
    return(paste0(
      "# Linear regression model implementation\n",
      "# Replace 'predictors' with your actual predictors (e.g., x1 + x2 + x3)\n",
      code_str, "\n\n",
      "# After fitting the model, you can:\n",
      "# 1. Get summary statistics: summary(model)\n",
      "# 2. Check assumptions: plot(model)\n",
      "# 3. Get predictions: predict(model, newdata)\n",
      "# 4. Get confidence intervals: confint(model)"
    ))
  } 
  else if (grepl("glm\\(.*family\\s*=\\s*binomial", code_str)) {
    return(paste0(
      "# Logistic regression model implementation\n",
      "# Replace 'predictors' with your actual predictors (e.g., x1 + x2 + x3)\n",
      code_str, "\n\n",
      "# After fitting the model, you can:\n",
      "# 1. Get summary statistics: summary(model)\n",
      "# 2. Get odds ratios: exp(coef(model))\n",
      "# 3. Get predicted probabilities: predict(model, newdata, type = 'response')\n",
      "# 4. Assess model fit: pROC::roc(outcome ~ fitted(model))"
    ))
  }
  else if (grepl("lmer\\(", code_str)) {
    return(paste0(
      "# Linear mixed effects model implementation\n",
      "# Replace 'predictors' with your fixed effects (e.g., x1 + x2 + x3)\n",
      "# The (1|cluster_var) term specifies a random intercept for each cluster\n",
      code_str, "\n\n",
      "# After fitting the model, you can:\n",
      "# 1. Get summary: summary(model)\n",
      "# 2. Test significance: lmerTest::anova(model) # requires lmerTest package\n",
      "# 3. Extract random effects: ranef(model)\n",
      "# 4. Check model fit: performance::check_model(model) # requires performance package"
    ))
  }
  else if (grepl("glmer\\(.*family\\s*=\\s*binomial", code_str)) {
    return(paste0(
      "# Generalized linear mixed model (logistic) implementation\n",
      "# Replace 'predictors' with your fixed effects (e.g., x1 + x2 + x3)\n",
      "# The (1|cluster_var) term specifies a random intercept for each cluster\n",
      code_str, "\n\n",
      "# After fitting the model, you can:\n",
      "# 1. Get summary: summary(model)\n",
      "# 2. Get odds ratios: exp(fixef(model))\n",
      "# 3. Extract random effects: ranef(model)\n",
      "# 4. Check for convergence issues: check_convergence <- model@optinfo$conv$lme4$messages"
    ))
  }
  else if (grepl("coxph\\(", code_str)) {
    return(paste0(
      "# Cox Proportional Hazards model implementation\n",
      "# Surv(time, event) creates a survival object\n",
      "# Replace 'predictors' with your actual predictors (e.g., x1 + x2 + x3)\n",
      code_str, "\n\n",
      "# After fitting the model, you can:\n",
      "# 1. Get summary: summary(model)\n",
      "# 2. Get hazard ratios: exp(coef(model))\n",
      "# 3. Check proportional hazards assumption: cox.zph(model)\n",
      "# 4. Plot survival curves: survminer::ggsurvplot(survfit(model))"
    ))
  }
  else {
    # Generic explanation
    return(paste0(
      "# Model implementation for ", model_type, "\n",
      "# Replace any placeholders with your actual variables\n",
      code_str, "\n\n",
      "# After fitting the model, examine results with:\n",
      "# summary(model)"
    ))
  }
}

#' Generate comparison between primary and alternative model
#'
#' @param primary_model Name of primary model
#' @param alt_model Name of alternative model
#'
#' @return Comparison text
#' @keywords internal
generate_comparison <- function(primary_model, alt_model) {
  # Linear vs. Robust Linear
  if (grepl("Linear Regression", primary_model) && grepl("Robust", alt_model)) {
    return("More robust to outliers and non-normal errors, but can be less efficient with normally distributed data.")
  }
  # Linear vs. Ridge/Lasso
  else if (grepl("Linear Regression", primary_model) && grepl("Ridge|Lasso|Elastic Net", alt_model)) {
    return("Handles multicollinearity and can prevent overfitting through regularization, but may introduce some bias.")
  }
  # Logistic vs. weighted
  else if (grepl("Logistic Regression$", primary_model) && grepl("class weights", alt_model)) {
    return("Better handles class imbalance by giving more weight to minority class, but may change the interpretation of probabilities.")
  }
  # ANOVA vs. ANCOVA
  else if (grepl("ANOVA", primary_model) && grepl("ANCOVA", alt_model)) {
    return("Includes covariates for better precision and to control for confounding, but requires additional assumptions.")
  }
  # Poisson vs. Negative Binomial
  else if (grepl("Poisson", primary_model) && grepl("Negative Binomial", alt_model)) {
    return("Better handles overdispersion in count data, but has an extra parameter to estimate.")
  }
  # Mixed model vs. GEE
  else if (grepl("Mixed Model", primary_model) && grepl("GEE", alt_model)) {
    return("Focuses on population-average rather than subject-specific effects, and is more robust to misspecification of random effects.")
  }
  # Cox vs. Parametric
  else if (grepl("Cox", primary_model) && grepl("Parametric", alt_model)) {
    return("Makes assumptions about the baseline hazard function, potentially providing more efficient estimates if those assumptions are correct.")
  }
  # Generic comparison
  else {
    return(paste("An alternative approach that may be appropriate in certain contexts or when assumptions of the primary model are not met."))
  }
}

#' Get package details
#'
#' @param package_name Name of the R package
#'
#' @return Package details description
#' @keywords internal
get_package_details <- function(package_name) {
  package_details <- list(
    "lme4" = "A powerful package for fitting linear and generalized linear mixed-effects models. It provides efficient computational methods for estimating parameters and includes functions for model comparison and visualization.",
    
    "lmerTest" = "Extends lme4 to provide p-values for fixed effects in linear mixed effects models using Satterthwaite's and Kenward-Roger methods for denominator degrees of freedom.",
    
    "nlme" = "Fits linear and nonlinear mixed-effects models with various correlation structures and variance functions. Offers more flexibility in modeling covariance structures compared to lme4, but can be less efficient.",
    
    "MASS" = "Contains functions for fitting robust regression, ordinal regression (polr), and negative binomial models (glm.nb), among many other statistical methods.",
    
    "glmnet" = "Fits lasso, ridge, and elastic-net regularized generalized linear models. Particularly useful for high-dimensional data or when dealing with multicollinearity.",
    
    "survival" = "The essential package for survival analysis in R. Contains functions for fitting Cox proportional hazards models, parametric survival models, and creating survival curves.",
    
    "survminer" = "Creates elegant and informative survival plots using ggplot2. Useful for visualizing Kaplan-Meier curves and results from Cox models.",
    
    "geepack" = "Implements Generalized Estimating Equations (GEE) for analyzing clustered correlated data focusing on population-average effects.",
    
    "pscl" = "Contains functions for fitting zero-inflated models for count data that have excess zeros.",
    
    "brms" = "Implements Bayesian mixed models using Stan. Offers great flexibility in model specification and prior distributions.",
    
    "car" = "Contains useful functions for regression diagnostics, hypothesis tests, and ANOVA.",
    
    "nnet" = "Fits multinomial logistic regression models for nominal categorical outcomes.",
    
    "ordinal" = "Specialized for fitting ordinal regression models with various link functions.",
    
    "performance" = "Provides tools for model checking, comparison, and performance assessment across various modeling packages.",
    
    "emmeans" = "Computes estimated marginal means (least-squares means) for many models. Useful for follow-up analyses after fitting models.",
    
    "stats" = "Base R package containing fundamental statistical functions including linear models, ANOVA, and statistical tests."
  )
  
  # Return details if available, or a generic description if not
  if (package_name %in% names(package_details)) {
    return(package_details[[package_name]])
  } else {
    return(paste("R package for statistical modeling and analysis relevant to the recommended models."))
  }
}

#' Print method for model_explanations objects
#'
#' @param x An object of class model_explanations
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
#' @method print model_explanations
print.model_explanations <- function(x, ...) {
  cat("StatsAidR Detailed Model Explanation\n")
  cat("==================================\n\n")
  
  # Primary model and why it's recommended
  if (!is.null(x$primary_model) && !is.null(x$primary_model$name)) {
    cat("Recommended Model: ", x$primary_model$name, "\n\n", sep = "")
    
    if (!is.null(x$why_recommended) && x$why_recommended != "") {
      cat("Why this model is recommended:\n")
      cat(strwrap(x$why_recommended, width = 80), sep = "\n")
      cat("\n\n")
    }
    
    if (!is.null(x$primary_model$description)) {
      cat("Model description: ", x$primary_model$description, "\n\n", sep = "")
    }
  }
  
  # Model advantages
  if (length(x$advantages) > 0) {
    cat("Key Advantages:\n")
    for (i in seq_along(x$advantages)) {
      advantage_name <- names(x$advantages)[i]
      advantage_desc <- x$advantages[[i]]
      cat("• ", advantage_name, ": ", advantage_desc, "\n", sep = "")
    }
    cat("\n")
  }
  
  # Model limitations
  if (length(x$limitations) > 0) {
    cat("Limitations to Consider:\n")
    for (i in seq_along(x$limitations)) {
      limitation_name <- names(x$limitations)[i]
      limitation_desc <- x$limitations[[i]]
      cat("• ", limitation_name, ": ", limitation_desc, "\n", sep = "")
    }
    cat("\n")
  }
  
  # Implementation
  if (!is.null(x$primary_model$implementation)) {
    cat("Implementation in R:\n")
    cat("```r\n")
    cat(x$primary_model$implementation, "\n")
    cat("```\n\n")
  }
  
  # Best practices
  if (length(x$best_practices) > 0) {
    cat("Best Practices:\n")
    for (i in seq_along(x$best_practices)) {
      practice_name <- names(x$best_practices)[i]
      practice_desc <- x$best_practices[[i]]
      cat("• ", practice_name, ": ", practice_desc, "\n", sep = "")
    }
    cat("\n")
  }
  
  # Common pitfalls
  if (length(x$common_pitfalls) > 0) {
    cat("Common Pitfalls to Avoid:\n")
    for (i in seq_along(x$common_pitfalls)) {
      pitfall_name <- names(x$common_pitfalls)[i]
      pitfall_desc <- x$common_pitfalls[[i]]
      cat("• ", pitfall_name, ": ", pitfall_desc, "\n", sep = "")
    }
    cat("\n")
  }
  
  # Alternative models
  if (!is.null(x$alternatives) && length(x$alternatives) > 0) {
    cat("Alternative Models to Consider:\n")
    for (i in seq_along(x$alternatives)) {
      alt <- x$alternatives[[i]]
      cat(i, ". ", alt$name, "\n", sep = "")
      if (!is.null(alt$description)) {
        cat("   Description: ", alt$description, "\n", sep = "")
      }
      if (!is.null(alt$when_to_use)) {
        cat("   When to use: ", alt$when_to_use, "\n", sep = "")
      }
      cat("\n")
    }
  }
  
  # Comparison with alternatives
  if (!is.null(x$comparison)) {
    cat("Comparison with Alternatives:\n")
    for (comp in x$comparison) {
      cat(comp, "\n")
    }
    cat("\n")
  }
  
  # Detailed code example with explanation
  if (!is.null(x$code_examples) && !is.null(x$code_examples$r_explained)) {
    cat("Detailed Code Example with Explanations:\n")
    cat("```r\n")
    cat(x$code_examples$r_explained, "\n")
    cat("```\n\n")
  }
  
  # Diagnostics
  if (!is.null(x$diagnostics) && length(x$diagnostics) > 0) {
    cat("Recommended Diagnostics:\n")
    for (diag in x$diagnostics) {
      cat("• ", diag, "\n", sep = "")
    }
    if (!is.null(x$why_diagnostics)) {
      cat("\n", strwrap(x$why_diagnostics, width = 80), sep = "\n")
    }
    cat("\n")
  }
  
  # Package details
  if (!is.null(x$packages) && length(x$packages) > 0) {
    cat("Required R Packages:\n")
    for (pkg in names(x$package_details)) {
      cat("• ", pkg, "\n", sep = "")
      if (!is.null(x$package_details[[pkg]])) {
        cat("  ", strwrap(x$package_details[[pkg]], width = 76, indent = 2, exdent = 2), sep = "\n")
      }
      cat("\n")
    }
  }
  
  # References
  if (!is.null(x$references) && length(x$references) > 0) {
    cat("References and Further Reading:\n")
    for (ref in x$references) {
      cat("• ", ref, "\n", sep = "")
    }
    cat("\n")
  }
  
  invisible(x)
}