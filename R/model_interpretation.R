#' Model Interpretation Module
#'
#' Functions for interpreting statistical models and generating plain-language
#' interpretations of results.
#'
#' @name model_interpretation
NULL

#' Run a model using the specified model type and formula
#'
#' This function fits a statistical model based on the model type, formula and data.
#'
#' @param data A data frame containing the variables in the model
#' @param model_type Character string specifying the model type
#' @param formula Model formula (as a string or formula object)
#' @param ... Additional arguments passed to the model fitting function
#'
#' @return A fitted model object
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   y = rnorm(100),
#'   x1 = rnorm(100),
#'   x2 = rnorm(100),
#'   group = factor(rep(1:10, each = 10))
#' )
#' model <- fit_model(data, "linear", "y ~ x1 + x2")
#' }
fit_model <- function(data, model_type, formula, ...) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Convert string formula to formula object if needed
  if (is.character(formula)) {
    formula <- as.formula(formula)
  }
  
  # Handle different model types
  model_obj <- switch(
    tolower(model_type),
    "linear" = stats::lm(formula, data = data, ...),
    "logistic" = stats::glm(formula, family = binomial(), data = data, ...),
    "poisson" = stats::glm(formula, family = poisson(), data = data, ...),
    "negative_binomial" = {
      if (!requireNamespace("MASS", quietly = TRUE)) {
        stop("Package 'MASS' needed for negative binomial regression.")
      }
      MASS::glm.nb(formula, data = data, ...)
    },
    "mixed" = {
      if (!requireNamespace("lme4", quietly = TRUE)) {
        stop("Package 'lme4' needed for mixed-effects models.")
      }
      lme4::lmer(formula, data = data, ...)
    },
    "mixed_logistic" = {
      if (!requireNamespace("lme4", quietly = TRUE)) {
        stop("Package 'lme4' needed for mixed-effects models.")
      }
      lme4::glmer(formula, family = binomial(), data = data, ...)
    },
    "cox" = {
      if (!requireNamespace("survival", quietly = TRUE)) {
        stop("Package 'survival' needed for Cox proportional hazards model.")
      }
      survival::coxph(formula, data = data, ...)
    },
    "gee" = {
      if (!requireNamespace("geepack", quietly = TRUE)) {
        stop("Package 'geepack' needed for GEE models.")
      }
      geepack::geeglm(formula, data = data, ...)
    },
    "anova" = stats::aov(formula, data = data, ...),
    "ordinal" = {
      if (!requireNamespace("MASS", quietly = TRUE)) {
        stop("Package 'MASS' needed for ordinal regression.")
      }
      MASS::polr(formula, data = data, ...)
    },
    stop("Unsupported model type: ", model_type)
  )
  
  # Add attributes for interpretation
  attr(model_obj, "model_type") <- model_type
  attr(model_obj, "formula_string") <- deparse(formula)
  
  return(model_obj)
}

#' Extract key statistics from a model
#'
#' This function extracts key statistics from a fitted model.
#'
#' @param model A fitted model object
#'
#' @return A list containing key statistics from the model
#' @export
#'
#' @examples
#' \dontrun{
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' extract_model_stats(model)
#' }
extract_model_stats <- function(model) {
  # Initialize results list
  results <- list(
    model_type = attr(model, "model_type"),
    formula = attr(model, "formula_string")
  )
  
  # If model_type attribute is not available, try to detect it
  if (is.null(results$model_type)) {
    results$model_type <- detect_model_type(model)
  }
  
  # Basic model information
  model_class <- class(model)[1]
  results$model_class <- model_class
  
  # Extract different statistics based on model class
  if (model_class == "lm") {
    # Linear model
    summ <- summary(model)
    results$r_squared <- summ$r.squared
    results$adj_r_squared <- summ$adj.r.squared
    results$f_statistic <- summ$fstatistic
    results$f_p_value <- pf(
      summ$fstatistic[1], 
      summ$fstatistic[2], 
      summ$fstatistic[3],
      lower.tail = FALSE
    )
    results$residual_std_error <- summ$sigma
    results$df <- summ$df
    
  } else if (model_class == "glm") {
    # Generalized linear model
    summ <- summary(model)
    results$family <- model$family$family
    results$link <- model$family$link
    results$aic <- model$aic
    results$null_deviance <- model$null.deviance
    results$residual_deviance <- model$deviance
    results$df_null <- model$df.null
    results$df_residual <- model$df.residual
    
    # Calculate pseudo R² (Nagelkerke)
    n <- nrow(model$model)
    results$pseudo_r_squared <- 1 - exp(-2/n * (model$null.deviance - model$deviance))
    
  } else if (model_class == "lmerMod" || model_class == "glmerMod") {
    # Mixed-effects model
    if (requireNamespace("lme4", quietly = TRUE) && 
        requireNamespace("performance", quietly = TRUE)) {
      
      # Get ICC
      icc <- tryCatch(
        performance::icc(model),
        error = function(e) NULL
      )
      if (!is.null(icc)) {
        results$icc <- icc$ICC
      }
      
      # Get R² for mixed models
      r2 <- tryCatch(
        performance::r2(model),
        error = function(e) NULL
      )
      if (!is.null(r2)) {
        results$r2_conditional <- r2$R2_conditional
        results$r2_marginal <- r2$R2_marginal
      }
      
      # Get AIC, BIC
      results$aic <- AIC(model)
      results$bic <- BIC(model)
      
      # Get residual degrees of freedom
      results$df_residual <- df.residual(model)
      
      # Get random effects variances
      vc <- lme4::VarCorr(model)
      results$random_effects_variance <- sapply(vc, function(x) attr(x, "stddev")^2)
      results$residual_variance <- attr(vc, "sc")^2
    }
    
  } else if (model_class == "polr") {
    # Ordinal regression
    summ <- summary(model)
    results$aic <- AIC(model)
    results$edf <- summ$edf
    
  } else if (model_class == "coxph") {
    # Cox proportional hazards
    summ <- summary(model)
    results$concordance <- summ$concordance
    results$logtest <- summ$logtest
    results$waldtest <- summ$waldtest
    results$sctest <- summ$sctest
    
  } else if (model_class == "geeglm") {
    # GEE model
    summ <- summary(model)
    results$correlation <- model$corstr
    results$scale <- model$scale
    
  } else if (model_class == "aov") {
    # ANOVA
    summ <- summary(model)
    results$df <- attr(summ[[1]], "heading")[2]
  }
  
  # Extract coefficients for all models
  if (!is.null(coef(model))) {
    coef_table <- tryCatch(
      {
        ct <- as.data.frame(summary(model)$coefficients)
        if (ncol(ct) >= 4) {
          colnames(ct)[1:4] <- c("Estimate", "Std.Error", "t/z.value", "p.value")
        }
        ct
      },
      error = function(e) {
        as.data.frame(coef(model))
      }
    )
    
    results$coefficients <- coef_table
    
    # Calculate odds ratios for logistic models
    if (model_class == "glm" && model$family$family == "binomial") {
      results$odds_ratios <- exp(coef(model))
      if ("Std.Error" %in% colnames(coef_table)) {
        results$odds_ratios_ci <- data.frame(
          OR = exp(coef_table$Estimate),
          Lower = exp(coef_table$Estimate - 1.96 * coef_table$Std.Error),
          Upper = exp(coef_table$Estimate + 1.96 * coef_table$Std.Error)
        )
      }
    }
    
    # Calculate hazard ratios for Cox models
    if (model_class == "coxph") {
      results$hazard_ratios <- exp(coef(model))
      if ("Std.Error" %in% colnames(coef_table)) {
        results$hazard_ratios_ci <- data.frame(
          HR = exp(coef_table$Estimate),
          Lower = exp(coef_table$Estimate - 1.96 * coef_table$Std.Error),
          Upper = exp(coef_table$Estimate + 1.96 * coef_table$Std.Error)
        )
      }
    }
  }
  
  # Add class for printing
  class(results) <- c("statsaid_model_stats", "list")
  return(results)
}

#' Print method for statsaid_model_stats objects
#'
#' @param x An object of class statsaid_model_stats
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_model_stats <- function(x, ...) {
  cat("StatsAid Model Statistics\n")
  cat("========================\n\n")
  
  # Print model type and formula
  if (!is.null(x$model_type)) {
    cat("Model type:", x$model_type, "\n")
  } else {
    cat("Model class:", x$model_class, "\n")
  }
  
  if (!is.null(x$formula)) {
    cat("Formula:", x$formula, "\n\n")
  }
  
  # Print model fit statistics
  cat("Model Fit:\n")
  
  if (!is.null(x$r_squared)) {
    cat("  R² =", round(x$r_squared, 3), "\n")
    cat("  Adjusted R² =", round(x$adj_r_squared, 3), "\n")
    cat("  F-statistic =", round(x$f_statistic[1], 2), "on", 
        x$f_statistic[2], "and", x$f_statistic[3], "DF, p-value =", 
        format.pval(x$f_p_value, digits = 3), "\n")
    cat("  Residual standard error =", round(x$residual_std_error, 3), 
        "on", x$df[2], "degrees of freedom\n")
  }
  
  if (!is.null(x$family)) {
    cat("  Family:", x$family, "with", x$link, "link\n")
    cat("  AIC =", round(x$aic, 2), "\n")
    cat("  Null deviance =", round(x$null_deviance, 2), "on", x$df_null, "degrees of freedom\n")
    cat("  Residual deviance =", round(x$residual_deviance, 2), 
        "on", x$df_residual, "degrees of freedom\n")
    if (!is.null(x$pseudo_r_squared)) {
      cat("  Pseudo R² (Nagelkerke) =", round(x$pseudo_r_squared, 3), "\n")
    }
  }
  
  if (!is.null(x$icc)) {
    cat("  ICC =", round(x$icc, 3), "\n")
  }
  
  if (!is.null(x$r2_conditional)) {
    cat("  R² (conditional) =", round(x$r2_conditional, 3), "\n")
    cat("  R² (marginal) =", round(x$r2_marginal, 3), "\n")
  }
  
  if (!is.null(x$concordance)) {
    cat("  Concordance =", round(x$concordance[1], 3), "\n")
    cat("  Likelihood ratio test =", round(x$logtest[1], 2), 
        "on", x$logtest[2], "df, p =", format.pval(x$logtest[3], digits = 3), "\n")
  }
  
  cat("\n")
  
  # Print coefficients
  if (!is.null(x$coefficients)) {
    cat("Coefficients:\n")
    
    # Format coefficients table for printing
    coef_table <- x$coefficients
    
    # Add significance stars
    if ("p.value" %in% colnames(coef_table)) {
      stars <- sapply(coef_table$p.value, function(p) {
        if (is.na(p)) return("")
        if (p < 0.001) return(" ***")
        if (p < 0.01) return(" **")
        if (p < 0.05) return(" *")
        if (p < 0.1) return(" .")
        return("")
      })
      
      # Round values for display
      coef_table <- round(coef_table, 4)
      
      # Format p-values
      if ("p.value" %in% colnames(coef_table)) {
        coef_table$p.value <- sapply(x$coefficients$p.value, 
                                    function(p) format.pval(p, digits = 3))
      }
      
      # Add stars to table
      row_names <- rownames(coef_table)
      for (i in seq_along(row_names)) {
        row_names[i] <- paste0(row_names[i], stars[i])
      }
      rownames(coef_table) <- row_names
    } else {
      coef_table <- round(coef_table, 4)
    }
    
    print(coef_table)
    cat("\n")
    
    # Print odds/hazard ratios if available
    if (!is.null(x$odds_ratios_ci)) {
      cat("Odds Ratios (with 95% CI):\n")
      or_table <- round(x$odds_ratios_ci, 3)
      print(or_table)
      cat("\n")
    } else if (!is.null(x$hazard_ratios_ci)) {
      cat("Hazard Ratios (with 95% CI):\n")
      hr_table <- round(x$hazard_ratios_ci, 3)
      print(hr_table)
      cat("\n")
    }
  }
  
  # Print signif. codes
  if (!is.null(x$coefficients) && "p.value" %in% colnames(x$coefficients)) {
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  }
  
  invisible(x)
}

#' Detect the type of a fitted model
#'
#' @param model A fitted model object
#'
#' @return A character string indicating the model type
#' @keywords internal
detect_model_type <- function(model) {
  model_class <- class(model)[1]
  
  if (model_class == "lm") {
    return("linear")
  } else if (model_class == "glm") {
    if (model$family$family == "binomial") {
      return("logistic")
    } else if (model$family$family == "poisson") {
      return("poisson")
    } else if (model$family$family == "Negative Binomial") {
      return("negative_binomial")
    } else {
      return(paste0("glm_", model$family$family))
    }
  } else if (model_class == "lmerMod") {
    return("mixed")
  } else if (model_class == "glmerMod") {
    if (model@resp$family$family == "binomial") {
      return("mixed_logistic")
    } else {
      return(paste0("mixed_", model@resp$family$family))
    }
  } else if (model_class == "coxph") {
    return("cox")
  } else if (model_class == "geeglm") {
    return("gee")
  } else if (model_class == "aov") {
    return("anova")
  } else if (model_class == "polr") {
    return("ordinal")
  } else {
    return(model_class)
  }
}

#' Generate a plain-language interpretation of model results
#'
#' This function provides an accessible interpretation of statistical model results,
#' explaining key findings in plain language.
#'
#' @param model A fitted model object or model statistics from extract_model_stats
#' @param data Original data used to fit the model (optional)
#' @param outcome_var The name of the outcome variable (optional)
#' @param study_design Study design information (optional)
#' @param detail_level Character string indicating the level of detail in the interpretation
#'   (options: "brief", "standard", "detailed", default: "standard")
#'
#' @return A list containing plain-language interpretations of model results
#' @export
#'
#' @examples
#' \dontrun{
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' interpret_model(model, mtcars, outcome_var = "mpg")
#' }
interpret_model <- function(model, data = NULL, outcome_var = NULL, 
                           study_design = NULL, detail_level = "standard") {
  
  # Extract model statistics if a model object was provided
  if (!inherits(model, "statsaid_model_stats")) {
    model_stats <- extract_model_stats(model)
  } else {
    model_stats <- model
  }
  
  # Initialize results
  results <- list(
    summary = "",
    main_findings = list(),
    model_fit = "",
    coefficients = list(),
    assumptions = list(),
    limitations = list(),
    recommendations = list(),
    report_sections = list()
  )
  
  # Get model characteristics
  model_type <- model_stats$model_type
  if (is.null(model_type)) {
    model_type <- model_stats$model_class
  }
  
  # Get formula and extract outcome variable if not provided
  formula_str <- model_stats$formula
  if (is.null(outcome_var) && !is.null(formula_str)) {
    outcome_var <- strsplit(formula_str, "~")[[1]][1]
    outcome_var <- trimws(outcome_var)
  }
  
  # Generate main interpretation based on model type
  if (model_type == "linear") {
    results <- interpret_linear_model(model_stats, data, outcome_var, 
                                     study_design, detail_level, results)
  } else if (model_type == "logistic") {
    results <- interpret_logistic_model(model_stats, data, outcome_var, 
                                       study_design, detail_level, results)
  } else if (model_type == "poisson" || model_type == "negative_binomial") {
    results <- interpret_count_model(model_stats, data, outcome_var, 
                                    study_design, detail_level, results)
  } else if (model_type == "mixed" || model_type == "mixed_logistic") {
    results <- interpret_mixed_model(model_stats, data, outcome_var, 
                                    study_design, detail_level, results)
  } else if (model_type == "cox") {
    results <- interpret_survival_model(model_stats, data, outcome_var, 
                                       study_design, detail_level, results)
  } else if (model_type == "anova") {
    results <- interpret_anova_model(model_stats, data, outcome_var, 
                                    study_design, detail_level, results)
  } else if (model_type == "ordinal") {
    results <- interpret_ordinal_model(model_stats, data, outcome_var, 
                                      study_design, detail_level, results)
  } else {
    # Generic interpretation for other model types
    results$summary <- paste("This analysis used a", model_type, 
                           "model to analyze the relationship between predictors and", 
                           outcome_var, ".")
    results$main_findings <- list(
      paste("The model examined the effects of multiple variables on", outcome_var, ".")
    )
    results$model_fit <- "Model fit statistics are available in the detailed output."
  }
  
  # Generate report sections
  results$report_sections <- generate_report_sections(results, model_stats, 
                                                   model_type, outcome_var)
  
  # Add class for printing
  class(results) <- c("statsaid_interpretation", "list")
  return(results)
}

#' Print method for statsaid_interpretation objects
#'
#' @param x An object of class statsaid_interpretation
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_interpretation <- function(x, ...) {
  cat("StatsAid Model Interpretation\n")
  cat("============================\n\n")
  
  # Print summary
  cat("Summary:\n")
  cat(x$summary, "\n\n")
  
  # Print main findings
  cat("Main Findings:\n")
  for (i in seq_along(x$main_findings)) {
    cat(paste0("- ", x$main_findings[[i]]), "\n")
  }
  cat("\n")
  
  # Print model fit
  cat("Model Fit:\n")
  cat(x$model_fit, "\n\n")
  
  # Print significant coefficients
  if (length(x$coefficients) > 0) {
    cat("Key Variables:\n")
    for (i in seq_along(x$coefficients)) {
      cat(paste0("- ", x$coefficients[[i]]), "\n")
    }
    cat("\n")
  }
  
  # Print assumptions
  if (length(x$assumptions) > 0) {
    cat("Assumptions and Diagnostics:\n")
    for (i in seq_along(x$assumptions)) {
      cat(paste0("- ", x$assumptions[[i]]), "\n")
    }
    cat("\n")
  }
  
  # Print limitations
  if (length(x$limitations) > 0) {
    cat("Limitations:\n")
    for (i in seq_along(x$limitations)) {
      cat(paste0("- ", x$limitations[[i]]), "\n")
    }
    cat("\n")
  }
  
  # Print recommendations
  if (length(x$recommendations) > 0) {
    cat("Recommendations:\n")
    for (i in seq_along(x$recommendations)) {
      cat(paste0("- ", x$recommendations[[i]]), "\n")
    }
    cat("\n")
  }
  
  invisible(x)
}

#' Generate a report based on model interpretation
#'
#' This function creates a formatted report of model results with interpretations.
#'
#' @param interpretation A statsaid_interpretation object or model object
#' @param model_stats Model statistics (optional if interpretation is provided)
#' @param file Output file path (e.g., "report.html", "report.docx")
#' @param title Report title
#' @param include_code Logical indicating whether to include R code
#' @param include_plots Logical indicating whether to include diagnostic plots
#'
#' @return Invisibly returns TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' interp <- interpret_model(model, mtcars, "mpg")
#' generate_model_report(interp, file = "model_report.html")
#' }
generate_model_report <- function(interpretation, model_stats = NULL, 
                                 file = "model_report.html", 
                                 title = "Statistical Model Results", 
                                 include_code = FALSE,
                                 include_plots = TRUE) {
  
  # Check if rmarkdown is available
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is needed for this function to work.")
  }
  
  # Check if interpretation is a model and convert if needed
  if (!inherits(interpretation, "statsaid_interpretation")) {
    if (is.null(model_stats)) {
      model_stats <- extract_model_stats(interpretation)
    }
    # Pass model_stats directly as the model parameter, not as a named parameter
    interpretation <- interpret_model(model_stats)
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
  cat(paste0("knitr::opts_chunk$set(echo = ", tolower(include_code), ", warning = FALSE, message = FALSE)\n"), 
      file = tmp_rmd, append = TRUE)
  cat("library(StatsAid)\n", file = tmp_rmd, append = TRUE)
  cat("library(knitr)\n", file = tmp_rmd, append = TRUE)
  cat("```\n\n", file = tmp_rmd, append = TRUE)
  
  # Summary section
  cat("## Summary\n\n", file = tmp_rmd, append = TRUE)
  cat(interpretation$summary, "\n\n", file = tmp_rmd, append = TRUE)
  
  # Main Findings section
  cat("## Main Findings\n\n", file = tmp_rmd, append = TRUE)
  for (finding in interpretation$main_findings) {
    cat(paste0("- ", finding), "\n", file = tmp_rmd, append = TRUE)
  }
  cat("\n", file = tmp_rmd, append = TRUE)
  
  # Model Fit section
  cat("## Model Fit\n\n", file = tmp_rmd, append = TRUE)
  cat(interpretation$model_fit, "\n\n", file = tmp_rmd, append = TRUE)
  
  # Model coefficients section
  if (!is.null(model_stats) && !is.null(model_stats$coefficients)) {
    cat("## Model Coefficients\n\n", file = tmp_rmd, append = TRUE)
    
    cat("```{r}\n", file = tmp_rmd, append = TRUE)
    cat("coef_table <- model_stats$coefficients\n", file = tmp_rmd, append = TRUE)
    cat("kable(coef_table, digits = 4, caption = 'Model Coefficients')\n", file = tmp_rmd, append = TRUE)
    cat("```\n\n", file = tmp_rmd, append = TRUE)
    
    # Add odds/hazard ratios if available
    if (!is.null(model_stats$odds_ratios_ci)) {
      cat("### Odds Ratios\n\n", file = tmp_rmd, append = TRUE)
      cat("```{r}\n", file = tmp_rmd, append = TRUE)
      cat("kable(model_stats$odds_ratios_ci, digits = 3, caption = 'Odds Ratios with 95% Confidence Intervals')\n", 
          file = tmp_rmd, append = TRUE)
      cat("```\n\n", file = tmp_rmd, append = TRUE)
    } else if (!is.null(model_stats$hazard_ratios_ci)) {
      cat("### Hazard Ratios\n\n", file = tmp_rmd, append = TRUE)
      cat("```{r}\n", file = tmp_rmd, append = TRUE)
      cat("kable(model_stats$hazard_ratios_ci, digits = 3, caption = 'Hazard Ratios with 95% Confidence Intervals')\n", 
          file = tmp_rmd, append = TRUE)
      cat("```\n\n", file = tmp_rmd, append = TRUE)
    }
  }
  
  # Interpretation of coefficients
  if (length(interpretation$coefficients) > 0) {
    cat("## Interpretation of Key Variables\n\n", file = tmp_rmd, append = TRUE)
    for (coef_interp in interpretation$coefficients) {
      cat(paste0("- ", coef_interp), "\n", file = tmp_rmd, append = TRUE)
    }
    cat("\n", file = tmp_rmd, append = TRUE)
  }
  
  # Diagnostic plots (if requested and available)
  if (include_plots && !is.null(model_stats$model_type)) {
    cat("## Diagnostic Plots\n\n", file = tmp_rmd, append = TRUE)
    
    if (model_stats$model_type == "linear") {
      cat("```{r, fig.width=10, fig.height=8}\n", file = tmp_rmd, append = TRUE)
      cat("if (exists('model') && inherits(model, 'lm')) {\n", file = tmp_rmd, append = TRUE)
      cat("  par(mfrow = c(2, 2))\n", file = tmp_rmd, append = TRUE)
      cat("  plot(model)\n", file = tmp_rmd, append = TRUE)
      cat("}\n", file = tmp_rmd, append = TRUE)
      cat("```\n\n", file = tmp_rmd, append = TRUE)
    } else if (model_stats$model_type %in% c("logistic", "poisson", "negative_binomial")) {
      cat("```{r, fig.width=10, fig.height=8}\n", file = tmp_rmd, append = TRUE)
      cat("if (exists('model') && inherits(model, 'glm')) {\n", file = tmp_rmd, append = TRUE)
      cat("  par(mfrow = c(2, 2))\n", file = tmp_rmd, append = TRUE)
      cat("  plot(model)\n", file = tmp_rmd, append = TRUE)
      cat("}\n", file = tmp_rmd, append = TRUE)
      cat("```\n\n", file = tmp_rmd, append = TRUE)
    } else if (model_stats$model_type %in% c("mixed", "mixed_logistic")) {
      cat("```{r, fig.width=10, fig.height=6}\n", file = tmp_rmd, append = TRUE)
      cat("if (exists('model') && (inherits(model, 'lmerMod') || inherits(model, 'glmerMod'))) {\n", 
          file = tmp_rmd, append = TRUE)
      cat("  if (requireNamespace('lattice', quietly = TRUE)) {\n", 
          file = tmp_rmd, append = TRUE)
      cat("    print(lattice::qqmath(lme4::ranef(model)))\n", 
          file = tmp_rmd, append = TRUE)
      cat("  }\n", file = tmp_rmd, append = TRUE)
      cat("}\n", file = tmp_rmd, append = TRUE)
      cat("```\n\n", file = tmp_rmd, append = TRUE)
    }
  }
  
  # Assumptions section
  if (length(interpretation$assumptions) > 0) {
    cat("## Assumptions and Diagnostics\n\n", file = tmp_rmd, append = TRUE)
    for (assumption in interpretation$assumptions) {
      cat(paste0("- ", assumption), "\n", file = tmp_rmd, append = TRUE)
    }
    cat("\n", file = tmp_rmd, append = TRUE)
  }
  
  # Limitations section
  if (length(interpretation$limitations) > 0) {
    cat("## Limitations\n\n", file = tmp_rmd, append = TRUE)
    for (limitation in interpretation$limitations) {
      cat(paste0("- ", limitation), "\n", file = tmp_rmd, append = TRUE)
    }
    cat("\n", file = tmp_rmd, append = TRUE)
  }
  
  # Recommendations section
  if (length(interpretation$recommendations) > 0) {
    cat("## Recommendations\n\n", file = tmp_rmd, append = TRUE)
    for (recommendation in interpretation$recommendations) {
      cat(paste0("- ", recommendation), "\n", file = tmp_rmd, append = TRUE)
    }
    cat("\n", file = tmp_rmd, append = TRUE)
  }
  
  # Add model stats to environment for the Rmd
  model_stats_name <- "model_stats"
  assign(model_stats_name, model_stats, envir = .GlobalEnv)
  
  # Render the report
  rmarkdown::render(tmp_rmd, output_file = file, quiet = TRUE)
  
  # Clean up
  unlink(tmp_rmd)
  
  # Return success
  message("Model report saved to: ", file)
  invisible(TRUE)
}

#' Generate report sections from interpretation
#'
#' @param results Interpretation results
#' @param model_stats Model statistics
#' @param model_type Model type
#' @param outcome_var Outcome variable name
#'
#' @return List with report sections
#' @keywords internal
generate_report_sections <- function(results, model_stats, model_type, outcome_var) {
  report_sections <- list()
  
  # Introduction section
  report_sections$introduction <- paste0(
    "This report presents the results of a ", model_type, " analysis ",
    "examining the relationship between various predictors and ", outcome_var, "."
  )
  
  # Methods section
  methods_text <- paste0(
    "A ", model_type, " model was used to analyze the data. ",
    "This statistical approach was chosen "
  )
  
  if (model_type == "linear") {
    methods_text <- paste0(methods_text, 
                          "because the outcome variable is continuous and the relationship ",
                          "was assumed to be linear. ")
  } else if (model_type == "logistic") {
    methods_text <- paste0(methods_text,
                          "because the outcome variable is binary. ")
  } else if (model_type %in% c("poisson", "negative_binomial")) {
    methods_text <- paste0(methods_text,
                          "because the outcome variable represents count data. ")
  } else if (model_type %in% c("mixed", "mixed_logistic")) {
    methods_text <- paste0(methods_text,
                          "because the data has a hierarchical structure with ",
                          "clustered or repeated observations. ")
  } else if (model_type == "cox") {
    methods_text <- paste0(methods_text,
                          "because the outcome is time-to-event data with potential censoring. ")
  } else if (model_type == "anova") {
    methods_text <- paste0(methods_text,
                          "to compare means across different groups or conditions. ")
  }
  
  report_sections$methods <- methods_text
  
  # Results section
  report_sections$results <- results$summary
  
  for (finding in results$main_findings) {
    report_sections$results <- paste0(report_sections$results, " ", finding)
  }
  
  # Discussion section
  discussion_text <- "The findings of this analysis have several implications. "
  
  # Add model fit discussion
  discussion_text <- paste0(discussion_text, results$model_fit, " ")
  
  # Add limitations
  if (length(results$limitations) > 0) {
    discussion_text <- paste0(discussion_text, "However, there are important limitations to consider: ")
    for (limitation in results$limitations) {
      discussion_text <- paste0(discussion_text, limitation, " ")
    }
  }
  
  report_sections$discussion <- discussion_text
  
  # Conclusion section
  if (length(results$recommendations) > 0) {
    conclusion_text <- "In conclusion, based on this analysis, we recommend: "
    for (recommendation in results$recommendations) {
      conclusion_text <- paste0(conclusion_text, recommendation, " ")
    }
    report_sections$conclusion <- conclusion_text
  } else {
    report_sections$conclusion <- "In conclusion, this analysis provides valuable insights into the factors affecting the outcome variable."
  }
  
  return(report_sections)
}

#' Interpret results from a linear regression model
#'
#' @param model_stats Model statistics
#' @param data Original data (optional)
#' @param outcome_var Outcome variable name
#' @param study_design Study design information (optional)
#' @param detail_level Detail level for interpretation
#' @param results Results list to populate
#'
#' @return Updated results list with interpretation
#' @keywords internal
interpret_linear_model <- function(model_stats, data, outcome_var, 
                                  study_design, detail_level, results) {
  
  # Generate summary
  results$summary <- paste0(
    "This linear regression analysis examines the relationship between several predictors and ",
    outcome_var, ". "
  )
  
  # Add R² information
  if (!is.null(model_stats$r_squared)) {
    r2_desc <- ""
    if (model_stats$r_squared < 0.3) {
      r2_desc <- "relatively small"
    } else if (model_stats$r_squared < 0.5) {
      r2_desc <- "moderate"
    } else if (model_stats$r_squared < 0.7) {
      r2_desc <- "substantial"
    } else {
      r2_desc <- "large"
    }
    
    results$summary <- paste0(
      results$summary,
      "The model explains ", round(model_stats$r_squared * 100, 1), 
      "% of the variance in ", outcome_var, 
      " (R² = ", round(model_stats$r_squared, 3), "), which is a ", 
      r2_desc, " amount."
    )
  }
  
  # Add F-test information
  if (!is.null(model_stats$f_p_value)) {
    if (model_stats$f_p_value < 0.05) {
      results$summary <- paste0(
        results$summary,
        " The overall model is statistically significant (F(",
        model_stats$f_statistic[2], ", ", model_stats$f_statistic[3], ") = ",
        round(model_stats$f_statistic[1], 2), ", p ", 
        format_p_value(model_stats$f_p_value), ")."
      )
    } else {
      results$summary <- paste0(
        results$summary,
        " The overall model is not statistically significant (F(",
        model_stats$f_statistic[2], ", ", model_stats$f_statistic[3], ") = ",
        round(model_stats$f_statistic[1], 2), ", p ", 
        format_p_value(model_stats$f_p_value), ")."
      )
    }
  }
  
  # Main findings based on coefficients
  if (!is.null(model_stats$coefficients)) {
    # Extract significant predictors
    coef_table <- model_stats$coefficients
    significant_predictors <- list()
    
    if ("p.value" %in% colnames(coef_table)) {
      for (var in rownames(coef_table)) {
        if (var != "(Intercept)" && !is.na(coef_table[var, "p.value"]) && 
            coef_table[var, "p.value"] < 0.05) {
          
          estimate <- coef_table[var, "Estimate"]
          p_value <- coef_table[var, "p.value"]
          
          direction <- ifelse(estimate > 0, "positive", "negative")
          magnitude <- abs(estimate)
          
          # Format variable name (remove interaction terms, etc.)
          var_name <- gsub(":", "×", var)  # Replace : with × for interactions
          
          significant_predictors[[var]] <- list(
            name = var_name,
            estimate = estimate,
            p_value = p_value,
            direction = direction,
            magnitude = magnitude
          )
        }
      }
    }
    
    # Generate main findings
    if (length(significant_predictors) > 0) {
      results$main_findings <- list(
        paste0("The analysis identified ", length(significant_predictors), 
              " statistically significant predictor", 
              ifelse(length(significant_predictors) > 1, "s", ""), 
              " of ", outcome_var, ".")
      )
      
      # Add key coefficient interpretations
      for (var in names(significant_predictors)) {
        pred <- significant_predictors[[var]]
        
        coef_text <- paste0(
          pred$name, " has a statistically significant ", pred$direction, 
          " relationship with ", outcome_var, 
          " (β = ", sprintf("%.3f", pred$estimate), 
          ", p ", format_p_value(pred$p_value), ")."
        )
        
        # Add plain language interpretation
        coef_text <- paste0(
          coef_text, " For each one-unit increase in ", pred$name, 
          ", ", outcome_var, " ", 
          ifelse(pred$direction == "positive", "increases", "decreases"), 
          " by ", sprintf("%.3f", abs(pred$estimate)), " units, holding other variables constant."
        )
        
        results$coefficients <- c(results$coefficients, coef_text)
      }
    } else {
      results$main_findings <- list(
        paste0("The analysis did not identify any statistically significant predictors of ", 
              outcome_var, ".")
      )
    }
  }
  
  # Model fit information
  if (!is.null(model_stats$r_squared)) {
    results$model_fit <- paste0(
      "The model explains ", round(model_stats$r_squared * 100, 1), 
      "% of the variance in ", outcome_var, 
      " (adjusted R² = ", round(model_stats$adj_r_squared, 3), ")."
    )
    
    if (!is.null(model_stats$f_p_value)) {
      results$model_fit <- paste0(
        results$model_fit, " The overall model is ",
        ifelse(model_stats$f_p_value < 0.05, "statistically significant", "not statistically significant"),
        " (F(", model_stats$f_statistic[2], ", ", model_stats$f_statistic[3], ") = ",
        round(model_stats$f_statistic[1], 2), ", p ", format_p_value(model_stats$f_p_value), ")."
      )
    }
  }
  
  # Add assumptions and diagnostics
  results$assumptions <- c(
    "Linear relationship between predictors and outcome",
    "Independence of observations",
    "Normality of residuals",
    "Homoscedasticity (constant variance of residuals)",
    "Absence of multicollinearity between predictors"
  )
  
  # Add limitations based on data and model
  results$limitations <- c(
    "This analysis shows associations, not necessarily causal relationships.",
    "The model assumes linear relationships, which may not capture complex patterns.",
    "Unmeasured confounding variables could affect the observed relationships."
  )
  
  # Add recommendations
  results$recommendations <- c(
    "Consider validating the model with a separate dataset.",
    "For variables with substantial effects, further investigation may be warranted."
  )
  
  if (!is.null(model_stats$r_squared) && model_stats$r_squared < 0.3) {
    results$recommendations <- c(
      results$recommendations,
      "The low R² suggests important predictors may be missing from the model. Consider including additional relevant variables."
    )
  }
  
  return(results)
}

#' Interpret results from a logistic regression model
#'
#' @param model_stats Model statistics
#' @param data Original data (optional)
#' @param outcome_var Outcome variable name
#' @param study_design Study design information (optional)
#' @param detail_level Detail level for interpretation
#' @param results Results list to populate
#'
#' @return Updated results list with interpretation
#' @keywords internal
interpret_logistic_model <- function(model_stats, data, outcome_var, 
                                    study_design, detail_level, results) {
  
  # Generate summary
  results$summary <- paste0(
    "This logistic regression analysis examines the relationship between several predictors and the probability of ",
    outcome_var, ". "
  )
  
  # Add model fit information
  if (!is.null(model_stats$pseudo_r_squared)) {
    pseudo_r2_desc <- ""
    if (model_stats$pseudo_r_squared < 0.2) {
      pseudo_r2_desc <- "relatively small"
    } else if (model_stats$pseudo_r_squared < 0.4) {
      pseudo_r2_desc <- "moderate"
    } else {
      pseudo_r2_desc <- "substantial"
    }
    
    results$summary <- paste0(
      results$summary,
      "The model explains a ", pseudo_r2_desc, " amount of variation in the outcome ",
      "(Nagelkerke's R² = ", round(model_stats$pseudo_r_squared, 3), ")."
    )
  }
  
  # Add likelihood ratio test information
  if (!is.null(model_stats$null_deviance) && !is.null(model_stats$residual_deviance)) {
    lr_chisq <- model_stats$null_deviance - model_stats$residual_deviance
    lr_df <- model_stats$df_null - model_stats$df_residual
    lr_p_value <- 1 - pchisq(lr_chisq, lr_df)
    
    if (lr_p_value < 0.05) {
      results$summary <- paste0(
        results$summary,
        " The model is statistically significant compared to a null model ",
        "(χ²(", lr_df, ") = ", round(lr_chisq, 2), ", p ", 
        format_p_value(lr_p_value), ")."
      )
    } else {
      results$summary <- paste0(
        results$summary,
        " The model is not statistically significant compared to a null model ",
        "(χ²(", lr_df, ") = ", round(lr_chisq, 2), ", p ", 
        format_p_value(lr_p_value), ")."
      )
    }
  }
  
  # Main findings based on odds ratios
  if (!is.null(model_stats$odds_ratios_ci)) {
    # Extract significant predictors
    coef_table <- model_stats$coefficients
    significant_predictors <- list()
    
    if ("p.value" %in% colnames(coef_table)) {
      for (var in rownames(coef_table)) {
        if (var != "(Intercept)" && !is.na(coef_table[var, "p.value"]) && 
            coef_table[var, "p.value"] < 0.05) {
          
          estimate <- coef_table[var, "Estimate"]
          p_value <- coef_table[var, "p.value"]
          odds_ratio <- model_stats$odds_ratios_ci[var, "OR"]
          ci_lower <- model_stats$odds_ratios_ci[var, "Lower"]
          ci_upper <- model_stats$odds_ratios_ci[var, "Upper"]
          
          direction <- ifelse(odds_ratio > 1, "positive", "negative")
          effect_size <- ifelse(
            odds_ratio > 1, 
            odds_ratio, 
            1 / odds_ratio
          )
          effect_desc <- ""
          if (effect_size < 1.5) {
            effect_desc <- "small"
          } else if (effect_size < 3) {
            effect_desc <- "moderate"
          } else if (effect_size < 5) {
            effect_desc <- "large"
          } else {
            effect_desc <- "very large"
          }
          
          # Format variable name
          var_name <- gsub(":", "×", var)  # Replace : with × for interactions
          
          significant_predictors[[var]] <- list(
            name = var_name,
            estimate = estimate,
            p_value = p_value,
            odds_ratio = odds_ratio,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            direction = direction,
            effect_desc = effect_desc
          )
        }
      }
    }
    
    # Generate main findings
    if (length(significant_predictors) > 0) {
      results$main_findings <- list(
        paste0("The analysis identified ", length(significant_predictors), 
              " statistically significant predictor", 
              ifelse(length(significant_predictors) > 1, "s", ""), 
              " of ", outcome_var, ".")
      )
      
      # Add key coefficient interpretations
      for (var in names(significant_predictors)) {
        pred <- significant_predictors[[var]]
        
        # Format odds ratio with CI
        or_text <- sprintf(
          "Odds Ratio = %.2f (95%% CI: %.2f to %.2f)", 
          pred$odds_ratio, pred$ci_lower, pred$ci_upper
        )
        
        coef_text <- paste0(
          pred$name, " has a statistically significant relationship with ", outcome_var, 
          " (", or_text, ", p ", format_p_value(pred$p_value), ")."
        )
        
        # Add plain language interpretation
        if (pred$direction == "positive") {
          if (abs(pred$odds_ratio - 1) <= 0.05) {
            effect_text <- "is not substantially associated with a change in"
          } else {
            effect_text <- paste0(
              "is associated with ", round((pred$odds_ratio - 1) * 100), 
              "% higher odds of"
            )
          }
        } else {
          if (abs(pred$odds_ratio - 1) <= 0.05) {
            effect_text <- "is not substantially associated with a change in"
          } else {
            effect_text <- paste0(
              "is associated with ", round((1 - pred$odds_ratio) * 100), 
              "% lower odds of"
            )
          }
        }
        
        coef_text <- paste0(
          coef_text, " For each one-unit increase in ", pred$name, 
          ", there ", effect_text, " ", outcome_var, "."
        )
        
        results$coefficients <- c(results$coefficients, coef_text)
      }
    } else {
      results$main_findings <- list(
        paste0("The analysis did not identify any statistically significant predictors of ", 
              outcome_var, ".")
      )
    }
  }
  
  # Model fit information
  model_fit_text <- ""
  
  if (!is.null(model_stats$aic)) {
    model_fit_text <- paste0(
      "The model's AIC is ", round(model_stats$aic, 2), ". "
    )
  }
  
  if (!is.null(model_stats$pseudo_r_squared)) {
    model_fit_text <- paste0(
      model_fit_text,
      "The model explains ", round(model_stats$pseudo_r_squared * 100, 1), 
      "% of the variation in the outcome (Nagelkerke's R² = ", 
      round(model_stats$pseudo_r_squared, 3), "). "
    )
  }
  
  if (!is.null(model_stats$null_deviance) && !is.null(model_stats$residual_deviance)) {
    lr_chisq <- model_stats$null_deviance - model_stats$residual_deviance
    lr_df <- model_stats$df_null - model_stats$df_residual
    lr_p_value <- 1 - pchisq(lr_chisq, lr_df)
    
    model_fit_text <- paste0(
      model_fit_text,
      "The model is ", 
      ifelse(lr_p_value < 0.05, "statistically significant", "not statistically significant"),
      " compared to a null model (χ²(", lr_df, ") = ", 
      round(lr_chisq, 2), ", p ", format_p_value(lr_p_value), ")."
    )
  }
  
  results$model_fit <- model_fit_text
  
  # Add assumptions and diagnostics
  results$assumptions <- c(
    "Linear relationship between predictors and log-odds of the outcome",
    "Independence of observations",
    "No perfect separation in the data",
    "Absence of multicollinearity between predictors"
  )
  
  # Add limitations based on data and model
  results$limitations <- c(
    "This analysis shows associations, not necessarily causal relationships.",
    "Predicted probabilities may be sensitive to the sample composition.",
    "Rare outcomes or small sample sizes may affect the stability of the estimates."
  )
  
  # Add recommendations
  results$recommendations <- c(
    "Consider validating the model with a separate dataset.",
    "For variables with substantial odds ratios, further investigation may be warranted.",
    "Calculate predicted probabilities for specific scenarios to enhance interpretation."
  )
  
  return(results)
}

#' Interpret results from a count regression model (Poisson or Negative Binomial)
#'
#' @param model_stats Model statistics
#' @param data Original data (optional)
#' @param outcome_var Outcome variable name
#' @param study_design Study design information (optional)
#' @param detail_level Detail level for interpretation
#' @param results Results list to populate
#'
#' @return Updated results list with interpretation
#' @keywords internal
interpret_count_model <- function(model_stats, data, outcome_var, 
                                 study_design, detail_level, results) {
  
  # Determine model type
  model_type <- model_stats$model_type
  model_name <- ifelse(model_type == "negative_binomial", "Negative Binomial", "Poisson")
  
  # Generate summary
  results$summary <- paste0(
    "This ", model_name, " regression analysis examines the relationship between several predictors and the count of ",
    outcome_var, ". "
  )
  
  # Add model fit information
  if (!is.null(model_stats$null_deviance) && !is.null(model_stats$residual_deviance)) {
    lr_chisq <- model_stats$null_deviance - model_stats$residual_deviance
    lr_df <- model_stats$df_null - model_stats$df_residual
    lr_p_value <- 1 - pchisq(lr_chisq, lr_df)
    
    if (lr_p_value < 0.05) {
      results$summary <- paste0(
        results$summary,
        "The model is statistically significant compared to a null model ",
        "(χ²(", lr_df, ") = ", round(lr_chisq, 2), ", p ", 
        format_p_value(lr_p_value), ")."
      )
    } else {
      results$summary <- paste0(
        results$summary,
        "The model is not statistically significant compared to a null model ",
        "(χ²(", lr_df, ") = ", round(lr_chisq, 2), ", p ", 
        format_p_value(lr_p_value), ")."
      )
    }
  }
  
  # Main findings based on coefficients (exponentiated for rate ratios)
  if (!is.null(model_stats$coefficients)) {
    # Extract significant predictors
    coef_table <- model_stats$coefficients
    significant_predictors <- list()
    
    if ("p.value" %in% colnames(coef_table)) {
      for (var in rownames(coef_table)) {
        if (var != "(Intercept)" && !is.na(coef_table[var, "p.value"]) && 
            coef_table[var, "p.value"] < 0.05) {
          
          estimate <- coef_table[var, "Estimate"]
          p_value <- coef_table[var, "p.value"]
          rate_ratio <- exp(estimate)
          se <- coef_table[var, "Std.Error"]
          ci_lower <- exp(estimate - 1.96 * se)
          ci_upper <- exp(estimate + 1.96 * se)
          
          direction <- ifelse(rate_ratio > 1, "positive", "negative")
          effect_size <- ifelse(
            rate_ratio > 1, 
            rate_ratio, 
            1 / rate_ratio
          )
          effect_desc <- ""
          if (effect_size < 1.5) {
            effect_desc <- "small"
          } else if (effect_size < 3) {
            effect_desc <- "moderate"
          } else if (effect_size < 5) {
            effect_desc <- "large"
          } else {
            effect_desc <- "very large"
          }
          
          # Format variable name
          var_name <- gsub(":", "×", var)
          
          significant_predictors[[var]] <- list(
            name = var_name,
            estimate = estimate,
            p_value = p_value,
            rate_ratio = rate_ratio,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            direction = direction,
            effect_desc = effect_desc
          )
        }
      }
    }
    
    # Generate main findings
    if (length(significant_predictors) > 0) {
      results$main_findings <- list(
        paste0("The analysis identified ", length(significant_predictors), 
              " statistically significant predictor", 
              ifelse(length(significant_predictors) > 1, "s", ""), 
              " of ", outcome_var, ".")
      )
      
      # Add key coefficient interpretations
      for (var in names(significant_predictors)) {
        pred <- significant_predictors[[var]]
        
        # Format rate ratio with CI
        rr_text <- sprintf(
          "Rate Ratio = %.2f (95%% CI: %.2f to %.2f)", 
          pred$rate_ratio, pred$ci_lower, pred$ci_upper
        )
        
        coef_text <- paste0(
          pred$name, " has a statistically significant relationship with ", outcome_var, 
          " (", rr_text, ", p ", format_p_value(pred$p_value), ")."
        )
        
        # Add plain language interpretation
        if (pred$direction == "positive") {
          if (abs(pred$rate_ratio - 1) <= 0.05) {
            effect_text <- "is not substantially associated with a change in"
          } else {
            effect_text <- paste0(
              "is associated with a ", round((pred$rate_ratio - 1) * 100), 
              "% increase in"
            )
          }
        } else {
          if (abs(pred$rate_ratio - 1) <= 0.05) {
            effect_text <- "is not substantially associated with a change in"
          } else {
            effect_text <- paste0(
              "is associated with a ", round((1 - pred$rate_ratio) * 100), 
              "% decrease in"
            )
          }
        }
        
        coef_text <- paste0(
          coef_text, " For each one-unit increase in ", pred$name, 
          ", there ", effect_text, " the expected count of ", outcome_var, "."
        )
        
        results$coefficients <- c(results$coefficients, coef_text)
      }
    } else {
      results$main_findings <- list(
        paste0("The analysis did not identify any statistically significant predictors of ", 
              outcome_var, ".")
      )
    }
  }
  
  # Model fit information
  model_fit_text <- ""
  
  if (!is.null(model_stats$aic)) {
    model_fit_text <- paste0(
      "The model's AIC is ", round(model_stats$aic, 2), ". "
    )
  }
  
  if (!is.null(model_stats$null_deviance) && !is.null(model_stats$residual_deviance)) {
    lr_chisq <- model_stats$null_deviance - model_stats$residual_deviance
    lr_df <- model_stats$df_null - model_stats$df_residual
    lr_p_value <- 1 - pchisq(lr_chisq, lr_df)
    
    model_fit_text <- paste0(
      model_fit_text,
      "The model is ", 
      ifelse(lr_p_value < 0.05, "statistically significant", "not statistically significant"),
      " compared to a null model (χ²(", lr_df, ") = ", 
      round(lr_chisq, 2), ", p ", format_p_value(lr_p_value), ")."
    )
  }
  
  results$model_fit <- model_fit_text
  
  # Add assumptions and diagnostics
  results$assumptions <- c(
    "Linear relationship between predictors and log of the outcome rate",
    "Independence of observations"
  )
  
  if (model_type == "poisson") {
    results$assumptions <- c(
      results$assumptions,
      "Equality of mean and variance (equidispersion)",
      "No excessive zeros in the data"
    )
  } else if (model_type == "negative_binomial") {
    results$assumptions <- c(
      results$assumptions,
      "Allows for overdispersion (variance > mean)",
      "Shape parameter accurately captures the dispersion"
    )
  }
  
  # Add limitations based on data and model
  results$limitations <- c(
    "This analysis shows associations, not necessarily causal relationships.",
    "Count data with excess zeros might require zero-inflated models."
  )
  
  if (model_type == "poisson") {
    results$limitations <- c(
      results$limitations,
      "The Poisson model assumes equality of mean and variance, which may not hold in practice."
    )
  }
  
  # Add recommendations
  recommendations <- c(
    "Consider validating the model with a separate dataset.",
    "For variables with substantial rate ratios, further investigation may be warranted."
  )
  
  if (model_type == "poisson") {
    recommendations <- c(
      recommendations,
      "Test for overdispersion; if present, consider using a Negative Binomial model instead."
    )
  }
  
  results$recommendations <- recommendations
  
  return(results)
}

#' Interpret results from a mixed-effects model
#'
#' @param model_stats Model statistics
#' @param data Original data (optional)
#' @param outcome_var Outcome variable name
#' @param study_design Study design information (optional)
#' @param detail_level Detail level for interpretation
#' @param results Results list to populate
#'
#' @return Updated results list with interpretation
#' @keywords internal
interpret_mixed_model <- function(model_stats, data, outcome_var, 
                                 study_design, detail_level, results) {
  
  # Determine model type
  model_type <- model_stats$model_type
  is_logistic <- model_type == "mixed_logistic"
  model_name <- ifelse(is_logistic, "Mixed-effects logistic regression", "Linear mixed-effects")
  
  # Generate summary
  results$summary <- paste0(
    "This ", model_name, " analysis examines the relationship between predictors and ",
    outcome_var, ", while accounting for clustering or grouping in the data. "
  )
  
  # Add ICC information
  if (!is.null(model_stats$icc)) {
    icc_desc <- ""
    if (model_stats$icc < 0.1) {
      icc_desc <- "relatively small"
    } else if (model_stats$icc < 0.3) {
      icc_desc <- "moderate"
    } else {
      icc_desc <- "substantial"
    }
    
    results$summary <- paste0(
      results$summary,
      "The intraclass correlation coefficient (ICC) is ", round(model_stats$icc, 3), 
      ", indicating a ", icc_desc, " amount of clustering in the data. "
    )
  }
  
  # Add R² information
  if (!is.null(model_stats$r2_marginal) && !is.null(model_stats$r2_conditional)) {
    r2_desc <- ""
    if (model_stats$r2_conditional < 0.3) {
      r2_desc <- "relatively small"
    } else if (model_stats$r2_conditional < 0.5) {
      r2_desc <- "moderate"
    } else {
      r2_desc <- "substantial"
    }
    
    results$summary <- paste0(
      results$summary,
      "The fixed effects explain ", round(model_stats$r2_marginal * 100, 1), 
      "% of the variance (marginal R² = ", round(model_stats$r2_marginal, 3), 
      "), while the full model including both fixed and random effects explains ", 
      round(model_stats$r2_conditional * 100, 1), "% of the variance (conditional R² = ", 
      round(model_stats$r2_conditional, 3), "), which is a ", r2_desc, " amount."
    )
  }
  
  # Main findings based on coefficients
  if (!is.null(model_stats$coefficients)) {
    # Extract significant predictors
    coef_table <- model_stats$coefficients
    significant_predictors <- list()
    
    if ("p.value" %in% colnames(coef_table)) {
      for (var in rownames(coef_table)) {
        if (var != "(Intercept)" && !is.na(coef_table[var, "p.value"]) && 
            coef_table[var, "p.value"] < 0.05) {
          
          estimate <- coef_table[var, "Estimate"]
          p_value <- coef_table[var, "p.value"]
          
          # For logistic mixed models, calculate odds ratio
          if (is_logistic) {
            effect_size <- exp(estimate)
            direction <- ifelse(effect_size > 1, "positive", "negative")
          } else {
            effect_size <- abs(estimate)
            direction <- ifelse(estimate > 0, "positive", "negative")
          }
          
          # Format variable name
          var_name <- gsub(":", "×", var)
          
          significant_predictors[[var]] <- list(
            name = var_name,
            estimate = estimate,
            p_value = p_value,
            effect_size = effect_size,
            direction = direction
          )
        }
      }
    }
    
    # Generate main findings
    if (length(significant_predictors) > 0) {
      results$main_findings <- list(
        paste0("The analysis identified ", length(significant_predictors), 
              " statistically significant fixed effect", 
              ifelse(length(significant_predictors) > 1, "s", ""), 
              " on ", outcome_var, ".")
      )
      
      # Add key coefficient interpretations
      for (var in names(significant_predictors)) {
        pred <- significant_predictors[[var]]
        
        if (is_logistic) {
          # For logistic mixed models
          coef_text <- paste0(
            pred$name, " has a statistically significant ", pred$direction, 
            " relationship with ", outcome_var, 
            " (β = ", sprintf("%.3f", pred$estimate), 
            ", OR = ", sprintf("%.2f", pred$effect_size),
            ", p ", format_p_value(pred$p_value), ")."
          )
          
          # Add plain language interpretation
          if (pred$direction == "positive") {
            effect_text <- paste0(
              "is associated with ", round((pred$effect_size - 1) * 100), 
              "% higher odds of"
            )
          } else {
            effect_text <- paste0(
              "is associated with ", round((1 - pred$effect_size) * 100), 
              "% lower odds of"
            )
          }
          
          coef_text <- paste0(
            coef_text, " For each one-unit increase in ", pred$name, 
            ", there ", effect_text, " ", outcome_var, 
            ", after accounting for the clustering in the data."
          )
        } else {
          # For linear mixed models
          coef_text <- paste0(
            pred$name, " has a statistically significant ", pred$direction, 
            " relationship with ", outcome_var, 
            " (β = ", sprintf("%.3f", pred$estimate), 
            ", p ", format_p_value(pred$p_value), ")."
          )
          
          # Add plain language interpretation
          coef_text <- paste0(
            coef_text, " For each one-unit increase in ", pred$name, 
            ", ", outcome_var, " ", 
            ifelse(pred$direction == "positive", "increases", "decreases"), 
            " by ", sprintf("%.3f", abs(pred$estimate)), " units, after accounting for the clustering in the data."
          )
        }
        
        results$coefficients <- c(results$coefficients, coef_text)
      }
      
      # Add information about random effects
      if (!is.null(model_stats$random_effects_variance)) {
        random_text <- "The model includes random effects to account for clustering in the data. "
        
        # Add random effects variance
        if (length(model_stats$random_effects_variance) > 0) {
          random_text <- paste0(
            random_text,
            "The variance of the random effects is ", 
            paste(names(model_stats$random_effects_variance), "=", 
                 round(model_stats$random_effects_variance, 3), 
                 collapse = ", "), "."
          )
          
          results$main_findings <- c(results$main_findings, random_text)
        }
      }
    } else {
      results$main_findings <- list(
        paste0("The analysis did not identify any statistically significant fixed effects on ", 
              outcome_var, ".")
      )
    }
  }
  
  # Model fit information
  model_fit_text <- ""
  
  if (!is.null(model_stats$aic)) {
    model_fit_text <- paste0(
      "The model's AIC is ", round(model_stats$aic, 2), ". "
    )
  }
  
  if (!is.null(model_stats$r2_marginal) && !is.null(model_stats$r2_conditional)) {
    model_fit_text <- paste0(
      model_fit_text,
      "The fixed effects explain ", round(model_stats$r2_marginal * 100, 1), 
      "% of the variance (marginal R² = ", round(model_stats$r2_marginal, 3), 
      "), while the full model including both fixed and random effects explains ", 
      round(model_stats$r2_conditional * 100, 1), "% of the variance (conditional R² = ", 
      round(model_stats$r2_conditional, 3), "). "
    )
  }
  
  if (!is.null(model_stats$icc)) {
    model_fit_text <- paste0(
      model_fit_text,
      "The intraclass correlation coefficient (ICC) is ", round(model_stats$icc, 3), 
      ", indicating that ", round(model_stats$icc * 100, 1), 
      "% of the total variance can be attributed to differences between clusters. "
    )
  }
  
  results$model_fit <- model_fit_text
  
  # Add assumptions and diagnostics
  if (is_logistic) {
    results$assumptions <- c(
      "Linear relationship between predictors and log-odds of the outcome",
      "Independence between clusters",
      "Normality of random effects",
      "No perfect separation in the data"
    )
  } else {
    results$assumptions <- c(
      "Linear relationship between predictors and outcome",
      "Independence between clusters",
      "Normality of residuals and random effects",
      "Homoscedasticity within clusters"
    )
  }
  
  # Add limitations based on data and model
  results$limitations <- c(
    "This analysis shows associations, not necessarily causal relationships.",
    "The model assumes that the random effects structure correctly captures the clustering in the data.",
    "Misspecification of the random effects structure can affect fixed effect estimates."
  )
  
  # Add recommendations
  results$recommendations <- c(
    "Consider validating the model with a separate dataset.",
    "Compare different random effects structures to ensure optimal model specification.",
    "For variables with substantial effects, further investigation may be warranted."
  )
  
  if (!is.null(model_stats$icc) && model_stats$icc < 0.05) {
    results$recommendations <- c(
      results$recommendations,
      "The low ICC suggests that clustering effects are minimal. Consider if a simpler non-mixed model would be adequate."
    )
  } else if (!is.null(model_stats$icc) && model_stats$icc > 0.3) {
    results$recommendations <- c(
      results$recommendations,
      "The high ICC indicates strong clustering effects. Consider exploring cluster-level predictors to explain this variation."
    )
  }
  
  return(results)
}

#' Format p-value for display
#'
#' @param p_value Numeric p-value
#'
#' @return Formatted p-value string
#' @keywords internal
format_p_value <- function(p_value) {
  if (is.na(p_value)) {
    return("= NA")
  }
  
  if (p_value < 0.001) {
    return("< 0.001")
  } else {
    return(paste0("= ", sprintf("%.3f", p_value)))
  }
}