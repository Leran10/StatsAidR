#' Study design specification
#'
#' Creates a structured specification of a study design for model suggestion.
#'
#' @param design_type Character string specifying the basic design type
#' @param response_type Character string specifying the response variable type
#' @param clustering Clustering specification (character or list)
#' @param repeated_measures Logical indicating if there are repeated measures
#' @param balanced Logical indicating if the design is balanced
#' @param sample_size Numeric sample size (approximate)
#' @param assumptions Character vector of assumptions user is willing to make
#' @param control_group Logical indicating if there is a control group
#' @param multiple_treatments Logical indicating if there are multiple treatment arms
#'
#' @return A list with study design specification
#' @export
#'
#' @examples
#' \dontrun{
#' design <- study_design_spec(
#'   design_type = "longitudinal",
#'   response_type = "continuous",
#'   clustering = "patient_id",
#'   repeated_measures = TRUE
#' )
#' }
study_design_spec <- function(
  design_type,
  response_type = NULL,
  clustering = NULL,
  repeated_measures = NULL,
  balanced = NULL,
  sample_size = NULL,
  assumptions = NULL,
  control_group = FALSE,
  multiple_treatments = FALSE
) {
  # Validate design type
  valid_designs <- c("cross_sectional", "longitudinal", "clustered", "case_control", 
                    "cohort", "rct", "repeated_measures", "hierarchical")
  
  if (!design_type %in% valid_designs) {
    warning("Design type '", design_type, "' not recognized. Using as-is.")
  }
  
  # Validate response type if provided
  if (!is.null(response_type)) {
    valid_responses <- c("continuous", "binary", "count", "survival", "ordinal", "nominal")
    
    if (!response_type %in% valid_responses) {
      warning("Response type '", response_type, "' not recognized. Using as-is.")
    }
  }
  
  # Return a structured object
  structure(list(
    design_type = design_type,
    response_type = response_type,
    clustering = clustering,
    repeated_measures = repeated_measures,
    balanced = balanced,
    sample_size = sample_size,
    assumptions = assumptions,
    control_group = control_group,
    multiple_treatments = multiple_treatments
  ), class = "statsaid_design")
}

#' Print method for statsaid_design objects
#'
#' @param x An object of class statsaid_design
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_design <- function(x, ...) {
  cat("StatsAid Study Design Specification\n")
  cat("==================================\n\n")
  
  cat("Design type:", x$design_type, "\n")
  
  if (!is.null(x$response_type)) {
    cat("Response type:", x$response_type, "\n")
  }
  
  if (!is.null(x$clustering)) {
    if (is.character(x$clustering) && length(x$clustering) == 1) {
      cat("Clustering variable:", x$clustering, "\n")
    } else if (is.list(x$clustering)) {
      cat("Clustering structure:", "\n")
      for (i in seq_along(x$clustering)) {
        if (i > 1) cat(" within ")
        cat(names(x$clustering)[i])
      }
      cat("\n")
    }
  }
  
  if (!is.null(x$repeated_measures)) {
    cat("Repeated measures:", ifelse(x$repeated_measures, "Yes", "No"), "\n")
  }
  
  if (!is.null(x$balanced)) {
    cat("Balanced design:", ifelse(x$balanced, "Yes", "No"), "\n")
  }
  
  if (!is.null(x$sample_size)) {
    cat("Sample size:", x$sample_size, "\n")
  }
  
  if (!is.null(x$assumptions) && length(x$assumptions) > 0) {
    cat("Assumptions:\n")
    for (assumption in x$assumptions) {
      cat("  - ", assumption, "\n", sep = "")
    }
  }
  
  cat("Control group:", ifelse(x$control_group, "Yes", "No"), "\n")
  cat("Multiple treatments:", ifelse(x$multiple_treatments, "Yes", "No"), "\n")
  
  invisible(x)
}

#' Cluster specification
#'
#' Creates a structured specification of clustering for model suggestion.
#'
#' @param cluster_var Character string with cluster identifier column
#' @param cluster_size Character describing cluster size ("fixed" or "variable")
#' @param levels Integer indicating number of clustering levels
#' @param nested_in Character string with higher-level clustering variable
#' @param crossed_with Character string with crossed clustering variable
#' @param icc Numeric intraclass correlation coefficient (if known)
#'
#' @return A list with cluster specification
#' @export
#'
#' @examples
#' \dontrun{
#' # Single-level clustering
#' cluster_spec("patient_id")
#' 
#' # Multi-level clustering
#' cluster_spec("patient_id", nested_in = "hospital_id", levels = 2)
#' }
cluster_spec <- function(
  cluster_var,
  cluster_size = NULL,
  levels = 1,
  nested_in = NULL,
  crossed_with = NULL,
  icc = NULL
) {
  # Validate levels
  if (levels < 1) {
    stop("Levels must be at least 1")
  }
  
  # Return a structured object
  structure(list(
    cluster_var = cluster_var,
    cluster_size = cluster_size,
    levels = levels,
    nested_in = nested_in,
    crossed_with = crossed_with,
    icc = icc
  ), class = "statsaid_cluster_spec")
}

#' Print method for statsaid_cluster_spec objects
#'
#' @param x An object of class statsaid_cluster_spec
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_cluster_spec <- function(x, ...) {
  cat("StatsAid Cluster Specification\n")
  cat("=============================\n\n")
  
  cat("Cluster variable:", x$cluster_var, "\n")
  
  if (!is.null(x$cluster_size)) {
    cat("Cluster size:", x$cluster_size, "\n")
  }
  
  cat("Number of levels:", x$levels, "\n")
  
  if (!is.null(x$nested_in)) {
    cat("Nested in:", x$nested_in, "\n")
  }
  
  if (!is.null(x$crossed_with)) {
    cat("Crossed with:", x$crossed_with, "\n")
  }
  
  if (!is.null(x$icc)) {
    cat("ICC:", x$icc, "\n")
  }
  
  invisible(x)
}

#' Enhanced model suggestion based on data analysis and study design
#'
#' This function suggests appropriate statistical models based on data
#' structure analysis and study design.
#'
#' @param data A data frame or tibble
#' @param outcome_var Optional character string specifying outcome variable
#' @param design A study design specification (character or statsaid_design object)
#' @param detailed Logical, if TRUE, provides more detailed recommendations
#'
#' @return A list with model suggestions
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' data <- data.frame(
#'   patient_id = rep(1:20, each = 3),
#'   treatment = rep(c("A", "B"), times = 30),
#'   outcome = rnorm(60)
#' )
#' suggest_models(data, outcome_var = "outcome", design = "longitudinal")
#' 
#' # More detailed specification
#' design <- study_design_spec(
#'   design_type = "longitudinal",
#'   response_type = "continuous",
#'   clustering = "patient_id"
#' )
#' suggest_models(data, outcome_var = "outcome", design = design)
#' }
suggest_models <- function(
  data,
  outcome_var = NULL,
  design = NULL,
  detailed = FALSE
) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Initialize results
  suggestions <- list(
    models = list(
      primary = NULL,
      variations = list(),
      alternatives = list()
    ),
    warnings = list(),
    data_characteristics = list(),
    packages = list(),
    diagnostics = list()
  )
  
  # Analyze data structure
  data_analysis <- analyze_data_structure(data, outcome_var)
  suggestions$data_characteristics <- data_analysis
  
  # Convert string design to design object if needed
  if (is.character(design) && length(design) == 1) {
    design <- study_design_spec(design_type = design)
  }
  
  # If outcome is provided, try to determine response type if not specified in design
  if (!is.null(outcome_var) && outcome_var %in% names(data)) {
    outcome_analysis <- data_analysis$outcome_analysis
    
    # Use outcome type if design doesn't specify response type
    if (!is.null(design) && is.null(design$response_type)) {
      if (outcome_analysis$type == "continuous") {
        design$response_type <- "continuous"
      } else if (outcome_analysis$type == "categorical" && outcome_analysis$subtype == "binary") {
        design$response_type <- "binary"
      } else if (outcome_analysis$type == "discrete" && outcome_analysis$subtype == "count") {
        design$response_type <- "count"
      } else if (outcome_analysis$type == "categorical" && outcome_analysis$subtype == "ordinal") {
        design$response_type <- "ordinal"
      } else if (outcome_analysis$type == "categorical" && outcome_analysis$subtype == "nominal") {
        design$response_type <- "nominal"
      }
    }
  }
  
  # If clustering is detected but not specified in design, update design
  if (data_analysis$cluster_analysis$has_clustering && 
      (!is.null(design) && is.null(design$clustering))) {
    # Use first detected clustering variable
    design$clustering <- data_analysis$cluster_analysis$potential_cluster_vars[1]
  }
  
  # Handle different scenarios based on design and data characteristics
  
  # 1. Clustered data
  if ((is.list(design) && !is.null(design$clustering)) || 
      data_analysis$cluster_analysis$has_clustering) {
    
    # Determine clustering variable
    if (is.list(design) && !is.null(design$clustering)) {
      if (is.character(design$clustering)) {
        cluster_var <- design$clustering
      } else if (inherits(design$clustering, "statsaid_cluster_spec")) {
        cluster_var <- design$clustering$cluster_var
      } else {
        cluster_var <- names(design$clustering)[1]
      }
    } else {
      cluster_var <- data_analysis$cluster_analysis$potential_cluster_vars[1]
    }
    
    # Choose appropriate model based on response type
    if (is.null(design) || is.null(design$response_type) || 
        design$response_type == "continuous") {
      # Linear mixed models for continuous outcomes
      suggestions$models$primary <- list(
        name = "Linear Mixed Model",
        description = "Accounts for clustering while modeling continuous outcome",
        implementation = paste0("lmer(", outcome_var, " ~ predictors + (1|", cluster_var, "), data = data)"),
        package = "lme4",
        assumptions = c(
          "Residuals are normally distributed",
          "Random effects are normally distributed",
          "Homogeneity of variance across clusters"
        )
      )
      
      suggestions$packages$r <- c(suggestions$packages$r, "lme4", "lmerTest")
      
      # Add alternative models
      suggestions$models$alternatives <- list(
        list(
          name = "GEE with Gaussian family",
          description = "Alternative approach focusing on population-average effects",
          implementation = paste0("geeglm(", outcome_var, " ~ predictors, id = ", 
                                cluster_var, ", family = gaussian, data = data, corstr = 'exchangeable')"),
          package = "geepack",
          when_to_use = "When focus is on population-average effects rather than cluster-specific effects"
        ),
        list(
          name = "Fixed effects model",
          description = "Alternative treating clusters as fixed effects",
          implementation = paste0("lm(", outcome_var, " ~ predictors + as.factor(", cluster_var, "), data = data)"),
          when_to_use = "When there are few clusters and you want to control for between-cluster differences"
        )
      )
      
      # Add diagnostic tests
      suggestions$diagnostics <- c(suggestions$diagnostics,
                                 "Check residual normality with qqnorm(resid(model))",
                                 "Check homogeneity of variances with plot(model)",
                                 "Calculate ICC with performance::icc(model)")
      
    } else if (design$response_type == "binary") {
      # Generalized linear mixed models for binary outcomes
      suggestions$models$primary <- list(
        name = "Generalized Linear Mixed Model (Logistic)",
        description = "Accounts for clustering while modeling binary outcome",
        implementation = paste0("glmer(", outcome_var, " ~ predictors + (1|", cluster_var, "), family = binomial, data = data)"),
        package = "lme4",
        assumptions = c(
          "Random effects are normally distributed",
          "No unmodeled interactions affecting outcome"
        )
      )
      
      suggestions$packages$r <- c(suggestions$packages$r, "lme4")
      
      # Add alternative models
      suggestions$models$alternatives <- list(
        list(
          name = "GEE with Binomial family",
          description = "Alternative approach focusing on population-average effects",
          implementation = paste0("geeglm(", outcome_var, " ~ predictors, id = ", 
                                cluster_var, ", family = binomial, data = data, corstr = 'exchangeable')"),
          package = "geepack",
          when_to_use = "When focus is on population-average effects rather than cluster-specific effects"
        )
      )
      
    } else if (design$response_type == "count") {
      # Check for zero-inflation
      zero_inflated <- FALSE
      if (!is.null(outcome_var) && outcome_var %in% names(data_analysis$outcome_analysis)) {
        zero_inflated <- !is.null(data_analysis$outcome_analysis$zero_inflated) && 
          data_analysis$outcome_analysis$zero_inflated
      }
      
      if (zero_inflated) {
        suggestions$models$primary <- list(
          name = "Zero-inflated Mixed Model",
          description = "Handles excess zeros and clustering in count data",
          implementation = paste0("glmmTMB(", outcome_var, " ~ predictors + (1|", 
                               cluster_var, "), family = poisson, ziformula = ~1, data = data)"),
          package = "glmmTMB",
          assumptions = c(
            "Zero values arise from two different processes",
            "Non-zero counts follow Poisson or negative binomial distribution"
          )
        )
        
        suggestions$packages$r <- c(suggestions$packages$r, "glmmTMB")
        
      } else {
        # Standard Poisson/Negative Binomial GLMM
        suggestions$models$primary <- list(
          name = "Poisson/Negative Binomial Mixed Model",
          description = "Accounts for clustering in count data",
          implementation = paste0("glmer(", outcome_var, " ~ predictors + (1|", 
                               cluster_var, "), family = poisson, data = data)"),
          package = "lme4",
          assumptions = c(
            "Count data follows Poisson distribution (or negative binomial if overdispersed)",
            "Random effects are normally distributed"
          )
        )
        
        suggestions$packages$r <- c(suggestions$packages$r, "lme4")
        
        # Add negative binomial alternative
        suggestions$models$alternatives <- list(
          list(
            name = "Negative Binomial Mixed Model",
            description = "Handles overdispersion in count data with clustering",
            implementation = paste0("glmer.nb(", outcome_var, " ~ predictors + (1|", 
                                  cluster_var, "), data = data)"),
            package = "lme4",
            when_to_use = "When count data shows overdispersion (variance > mean)"
          )
        )
      }
    }
    
    # Add other diagnostics for all clustered models
    suggestions$diagnostics <- c(suggestions$diagnostics, 
                               "Examine clustering effect with dotplot(ranef(model))",
                               "Check for convergence issues with summary(model)")
    
  } else if (!is.null(design)) {
    # Handle different study designs without clustering
    
    if (design$design_type == "case_control") {
      if (is.null(design$response_type) || design$response_type == "binary") {
        suggestions$models$primary <- list(
          name = "Logistic Regression",
          description = "Standard approach for case-control studies",
          implementation = paste0("glm(", outcome_var, " ~ predictors, family = binomial, data = data)"),
          assumptions = c(
            "Cases and controls are representative of their respective populations",
            "Independence between observations"
          )
        )
        
        if (!is.null(outcome_var) && outcome_var %in% names(data) && 
            !is.null(data_analysis$outcome_analysis$balanced) && 
            !data_analysis$outcome_analysis$balanced) {
          # Add warning and alternatives for imbalanced data
          suggestions$warnings$imbalance <- "Class imbalance detected in outcome variable"
          
          suggestions$models$alternatives <- list(
            list(
              name = "Logistic Regression with class weights",
              description = "Addresses class imbalance in case-control data",
              implementation = paste0("glm(", outcome_var, " ~ predictors, family = binomial, weights = case_weights, data = data)"),
              when_to_use = "When there is significant class imbalance"
            ),
            list(
              name = "Penalized Logistic Regression",
              description = "Provides more stable estimates with small samples or class imbalance",
              implementation = paste0("glmnet(x, y, family = 'binomial', alpha = 0.5)"),
              package = "glmnet",
              when_to_use = "With small sample sizes or many predictors"
            )
          )
        }
        
        suggestions$packages$r <- c(suggestions$packages$r, "stats", "MASS")
        
        # Special considerations for matched case-control
        if (!is.null(design$matched) && design$matched) {
          suggestions$models$primary <- list(
            name = "Conditional Logistic Regression",
            description = "Appropriate for matched case-control studies",
            implementation = paste0("clogit(", outcome_var, " ~ predictors + strata(matching_var), data = data)"),
            package = "survival",
            assumptions = c(
              "Matched cases and controls are correctly paired",
              "Matching factors are correctly specified"
            )
          )
          
          suggestions$packages$r <- c(suggestions$packages$r, "survival")
        }
      }
      
    } else if (design$design_type == "cohort") {
      if (is.null(design$response_type) || design$response_type == "survival") {
        suggestions$models$primary <- list(
          name = "Cox Proportional Hazards",
          description = "Standard method for time-to-event analysis in cohort studies",
          implementation = paste0("coxph(Surv(time, event) ~ predictors, data = data)"),
          package = "survival",
          assumptions = c(
            "Proportional hazards assumption",
            "Non-informative censoring",
            "Independence between observations"
          )
        )
        
        suggestions$packages$r <- c(suggestions$packages$r, "survival", "survminer")
        
        suggestions$models$alternatives <- list(
          list(
            name = "Parametric Survival Models",
            description = "Alternative when distributional assumptions can be made",
            implementation = paste0("survreg(Surv(time, event) ~ predictors, dist = 'weibull', data = data)"),
            package = "survival",
            when_to_use = "When time-to-event follows a known distribution"
          ),
          list(
            name = "Random Forest Survival Analysis",
            description = "Non-parametric machine learning approach",
            implementation = paste0("rfsrc(Surv(time, event) ~ predictors, data = data)"),
            package = "randomForestSRC",
            when_to_use = "When relationship between predictors and survival is complex or non-linear"
          )
        )
      } else if (design$response_type == "binary") {
        suggestions$models$primary <- list(
          name = "Logistic Regression",
          description = "For binary outcomes in cohort studies",
          implementation = paste0("glm(", outcome_var, " ~ predictors, family = binomial, data = data)"),
          assumptions = c(
            "Independence between observations",
            "Linearity between predictors and log-odds"
          )
        )
      } else if (design$response_type == "continuous") {
        suggestions$models$primary <- list(
          name = "Linear Regression",
          description = "For continuous outcomes in cohort studies",
          implementation = paste0("lm(", outcome_var, " ~ predictors, data = data)"),
          assumptions = c(
            "Normality of residuals",
            "Homoscedasticity",
            "Independence between observations"
          )
        )
      }
      
    } else if (design$design_type == "cross_sectional") {
      if (is.null(design$response_type) || design$response_type == "binary") {
        suggestions$models$primary <- list(
          name = "Logistic Regression",
          description = "Standard approach for binary outcomes in cross-sectional studies",
          implementation = paste0("glm(", outcome_var, " ~ predictors, family = binomial, data = data)"),
          assumptions = c(
            "Independence between observations",
            "Linearity between predictors and log-odds"
          )
        )
      } else if (design$response_type == "continuous") {
        # Check data characteristics for continuous outcome
        non_normal <- FALSE
        if (!is.null(outcome_var) && outcome_var %in% names(data) && 
            !is.null(data_analysis$outcome_analysis$normality) && 
            !data_analysis$outcome_analysis$normality$normal) {
          non_normal <- TRUE
        }
        
        if (non_normal) {
          suggestions$models$primary <- list(
            name = "Robust Linear Regression",
            description = "Handles non-normality and outliers",
            implementation = paste0("rlm(", outcome_var, " ~ predictors, data = data)"),
            package = "MASS",
            assumptions = c(
              "Independence between observations",
              "Linearity between predictors and outcome (but robust to outliers)"
            )
          )
          
          suggestions$packages$r <- c(suggestions$packages$r, "MASS")
          
        } else {
          suggestions$models$primary <- list(
            name = "Linear Regression",
            description = "Standard approach for continuous outcomes",
            implementation = paste0("lm(", outcome_var, " ~ predictors, data = data)"),
            assumptions = c(
              "Normality of residuals",
              "Homoscedasticity",
              "Independence between observations"
            )
          )
        }
      } else if (design$response_type == "count") {
        zero_inflated <- FALSE
        if (!is.null(outcome_var) && outcome_var %in% names(data) && 
            !is.null(data_analysis$outcome_analysis$zero_inflated) && 
            data_analysis$outcome_analysis$zero_inflated) {
          zero_inflated <- TRUE
        }
        
        if (zero_inflated) {
          suggestions$models$primary <- list(
            name = "Zero-inflated Poisson/Negative Binomial",
            description = "Handles excess zeros in count data",
            implementation = paste0("zeroinfl(", outcome_var, " ~ predictors, data = data)"),
            package = "pscl",
            assumptions = c(
              "Zero values arise from two different processes",
              "Non-zero counts follow Poisson or negative binomial distribution"
            )
          )
          
          suggestions$packages$r <- c(suggestions$packages$r, "pscl")
          
        } else {
          suggestions$models$primary <- list(
            name = "Poisson Regression",
            description = "Standard approach for count outcomes",
            implementation = paste0("glm(", outcome_var, " ~ predictors, family = poisson, data = data)"),
            assumptions = c(
              "Count data follows Poisson distribution (mean = variance)",
              "Independence between observations"
            )
          )
          
          suggestions$models$alternatives <- list(
            list(
              name = "Negative Binomial Regression",
              description = "Handles overdispersion in count data",
              implementation = paste0("glm.nb(", outcome_var, " ~ predictors, data = data)"),
              package = "MASS",
              when_to_use = "When count data shows overdispersion (variance > mean)"
            )
          )
          
          suggestions$packages$r <- c(suggestions$packages$r, "MASS")
        }
      } else if (design$response_type == "ordinal") {
        suggestions$models$primary <- list(
          name = "Ordinal Regression (Proportional Odds)",
          description = "Standard approach for ordinal outcomes",
          implementation = paste0("polr(", outcome_var, " ~ predictors, data = data)"),
          package = "MASS",
          assumptions = c(
            "Proportional odds assumption",
            "Independence between observations"
          )
        )
        
        suggestions$packages$r <- c(suggestions$packages$r, "MASS")
        
        suggestions$models$alternatives <- list(
          list(
            name = "Continuation Ratio Model",
            description = "Alternative when proportional odds assumption is violated",
            implementation = "# Requires custom implementation with multiple logistic regressions",
            when_to_use = "When proportional odds assumption doesn't hold"
          )
        )
      }
      
    } else if (design$design_type == "rct") {
      suggestions$models$primary <- list(
        name = "ANOVA/t-test",
        description = "Standard approach for comparing groups in RCTs",
        implementation = paste0("aov(", outcome_var, " ~ treatment, data = data)"),
        assumptions = c(
          "Normality of outcome within groups",
          "Homogeneity of variance across groups",
          "Independence between observations"
        )
      )
      
      suggestions$models$alternatives <- list(
        list(
          name = "ANCOVA",
          description = "Improves precision by adjusting for baseline covariates",
          implementation = paste0("aov(", outcome_var, " ~ treatment + baseline_covariates, data = data)"),
          when_to_use = "When baseline measurements or important covariates are available"
        ),
        list(
          name = "Linear mixed models",
          description = "For repeated measures or multi-center trials",
          implementation = paste0("lmer(", outcome_var, " ~ treatment + (1|center), data = data)"),
          package = "lme4",
          when_to_use = "With multiple measurements per subject or clustering (e.g., multi-center)"
        )
      )
      
      suggestions$packages$r <- c(suggestions$packages$r, "stats", "lme4")
    } else if (design$design_type == "longitudinal" || design$design_type == "repeated_measures") {
      # Longitudinal studies generally require mixed models or GEE
      if (is.null(design$response_type) || design$response_type == "continuous") {
        suggestions$models$primary <- list(
          name = "Linear Mixed Model",
          description = "Standard approach for longitudinal continuous outcomes",
          implementation = paste0("lmer(", outcome_var, " ~ time + predictors + (1|subject_id), data = data)"),
          package = "lme4",
          assumptions = c(
            "Normality of residuals",
            "Random effects are normally distributed",
            "Linear change over time (if modeled as continuous)"
          )
        )
        
        suggestions$packages$r <- c(suggestions$packages$r, "lme4", "lmerTest")
        
        suggestions$models$alternatives <- list(
          list(
            name = "GEE with Gaussian family",
            description = "Alternative focusing on population-average effects",
            implementation = paste0("geeglm(", outcome_var, " ~ time + predictors, id = subject_id, 
                                   family = gaussian, corstr = 'ar1', data = data)"),
            package = "geepack",
            when_to_use = "When focus is on population-average effects rather than subject-specific effects"
          )
        )
      } else if (design$response_type == "binary") {
        suggestions$models$primary <- list(
          name = "Generalized Linear Mixed Model (Logistic)",
          description = "For longitudinal binary outcomes",
          implementation = paste0("glmer(", outcome_var, " ~ time + predictors + (1|subject_id), 
                                family = binomial, data = data)"),
          package = "lme4",
          assumptions = c(
            "Random effects are normally distributed",
            "Linear relationship on logit scale"
          )
        )
        
        suggestions$packages$r <- c(suggestions$packages$r, "lme4")
      }
    }
  } else {
    # Fall back to basic suggestions based on data characteristics
    if (!is.null(outcome_var) && outcome_var %in% names(data)) {
      outcome_analysis <- data_analysis$outcome_analysis
      
      if (outcome_analysis$type == "continuous") {
        # Use robust methods if non-normal or with outliers
        if ((!is.null(outcome_analysis$normality) && !outcome_analysis$normality$normal) ||
            (!is.null(outcome_analysis$subtype) && outcome_analysis$subtype %in% c("right_skewed", "left_skewed"))) {
          
          suggestions$models$primary <- list(
            name = "Robust Linear Regression",
            description = "Handles non-normality and outliers",
            implementation = paste0("rlm(", outcome_var, " ~ predictors, data = data)"),
            package = "MASS"
          )
          
          suggestions$packages$r <- c(suggestions$packages$r, "MASS")
          
          # Add transformation suggestion
          if (!is.null(outcome_analysis$subtype) && outcome_analysis$subtype == "right_skewed") {
            suggestions$warnings$transformation <- "Right-skewed outcome may benefit from log or square root transformation"
          } else if (!is.null(outcome_analysis$subtype) && outcome_analysis$subtype == "left_skewed") {
            suggestions$warnings$transformation <- "Left-skewed outcome may benefit from square or cube transformation"
          }
          
        } else {
          suggestions$models$primary <- list(
            name = "Multiple Linear Regression",
            description = "Standard approach for continuous outcomes",
            implementation = paste0("lm(", outcome_var, " ~ predictors, data = data)")
          )
        }
        
      } else if (outcome_analysis$type == "categorical" && outcome_analysis$subtype == "binary") {
        suggestions$models$primary <- list(
          name = "Logistic Regression",
          description = "Standard approach for binary outcomes",
          implementation = paste0("glm(", outcome_var, " ~ predictors, family = binomial, data = data)")
        )
        
        # Check for class imbalance
        if (!outcome_analysis$balanced) {
          suggestions$warnings$imbalance <- "Class imbalance detected in outcome variable"
          
          suggestions$models$alternatives <- list(
            list(
              name = "Logistic Regression with class weights",
              description = "Addresses class imbalance",
              implementation = paste0("glm(", outcome_var, " ~ predictors, family = binomial, weights = case_weights, data = data)"),
              when_to_use = "When there is significant class imbalance"
            )
          )
        }
        
      } else if (outcome_analysis$type == "discrete" && outcome_analysis$subtype == "count") {
        # Check for zero-inflation
        if (outcome_analysis$zero_inflated) {
          suggestions$models$primary <- list(
            name = "Zero-inflated Poisson/Negative Binomial",
            description = "Handles excess zeros in count data",
            implementation = paste0("zeroinfl(", outcome_var, " ~ predictors, data = data)"),
            package = "pscl"
          )
          
          suggestions$packages$r <- c(suggestions$packages$r, "pscl")
          
        } else {
          suggestions$models$primary <- list(
            name = "Poisson Regression",
            description = "Standard approach for count outcomes",
            implementation = paste0("glm(", outcome_var, " ~ predictors, family = poisson, data = data)")
          )
          
          suggestions$models$alternatives <- list(
            list(
              name = "Negative Binomial Regression",
              description = "Handles overdispersion in count data",
              implementation = paste0("glm.nb(", outcome_var, " ~ predictors, data = data)"),
              package = "MASS",
              when_to_use = "When count data shows overdispersion (variance > mean)"
            )
          )
          
          suggestions$packages$r <- c(suggestions$packages$r, "MASS")
        }
        
      } else if (outcome_analysis$type == "categorical" && outcome_analysis$subtype == "ordinal") {
        suggestions$models$primary <- list(
          name = "Ordinal Regression (Proportional Odds)",
          description = "Standard approach for ordinal outcomes",
          implementation = paste0("polr(", outcome_var, " ~ predictors, data = data)"),
          package = "MASS"
        )
        
        suggestions$packages$r <- c(suggestions$packages$r, "MASS")
        
      } else if (outcome_analysis$type == "categorical" && outcome_analysis$subtype == "nominal") {
        suggestions$models$primary <- list(
          name = "Multinomial Logistic Regression",
          description = "Standard approach for nominal outcomes",
          implementation = paste0("multinom(", outcome_var, " ~ predictors, data = data)"),
          package = "nnet"
        )
        
        suggestions$packages$r <- c(suggestions$packages$r, "nnet")
      }
    } else {
      # Basic suggestions without outcome variable or specific design
      suggestions$models$primary <- list(
        name = "General Data Exploration",
        description = "Since no outcome variable or specific design was provided, consider these general approaches:",
        implementation = NULL
      )
      
      if (data_analysis$cluster_analysis$has_clustering) {
        suggestions$models$alternatives <- c(suggestions$models$alternatives, list(
          list(
            name = "Mixed-Effects Models",
            description = "For clustered data detected in your dataset",
            implementation = "lmer(outcome ~ predictors + (1|cluster_var), data = data)",
            package = "lme4"
          )
        ))
        
        suggestions$packages$r <- c(suggestions$packages$r, "lme4")
      }
      
      suggestions$models$alternatives <- c(suggestions$models$alternatives, list(
        list(
          name = "Regression Models",
          description = "Standard parametric approaches",
          implementation = "Type depends on outcome variable type (continuous, binary, count, etc.)"
        ),
        list(
          name = "Machine Learning Methods",
          description = "Flexible approaches for prediction",
          implementation = "Methods like random forest, boosting, etc., depending on your goals",
          package = "caret"
        )
      ))
      
      suggestions$packages$r <- c(suggestions$packages$r, "stats", "caret")
    }
  }
  
  # Check for dependency issues
  if (!is.null(data_analysis$dependency_analysis) && 
      length(data_analysis$dependency_analysis$high_correlations) > 0) {
    
    suggestions$warnings$collinearity <- paste0(
      "High correlations detected among predictors: ", 
      paste(names(data_analysis$dependency_analysis$high_correlations), collapse = ", ")
    )
    
    # Suggest regularization methods for multicollinearity
    suggestions$models$alternatives <- c(suggestions$models$alternatives, list(
      list(
        name = "Ridge Regression",
        description = "Handles multicollinearity through L2 regularization",
        implementation = "glmnet(x, y, alpha = 0)",
        package = "glmnet",
        when_to_use = "When predictors show high correlations"
      )
    ))
    
    suggestions$packages$r <- unique(c(suggestions$packages$r, "glmnet"))
  }
  
  # Organize output
  class(suggestions) <- c("statsaid_model_suggestions", "list")
  
  # Return simplified format if detailed is FALSE
  if (!detailed) {
    # Convert to simpler format
    simple_suggestions <- list(
      models = character(),
      packages = list(
        r = unique(suggestions$packages$r),
        python = character()  # For compatibility with the original function
      )
    )
    
    # Add primary model
    if (!is.null(suggestions$models$primary)) {
      simple_suggestions$models <- c(simple_suggestions$models, suggestions$models$primary$name)
    }
    
    # Add alternatives
    for (alt in suggestions$models$alternatives) {
      simple_suggestions$models <- c(simple_suggestions$models, alt$name)
    }
    
    # Sort and ensure unique
    simple_suggestions$models <- sort(unique(simple_suggestions$models))
    simple_suggestions$packages$r <- sort(unique(simple_suggestions$packages$r))
    
    # Add class for backward compatibility
    class(simple_suggestions) <- c("statsaid_model_suggestions", "list")
    return(simple_suggestions)
  }
  
  return(suggestions)
}

#' Print method for statsaid_model_suggestions objects
#'
#' @param x An object of class statsaid_model_suggestions
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_model_suggestions <- function(x, ...) {
  cat("StatsAid Model Suggestions\n")
  cat("==========================\n\n")
  
  # Print basic model list (compatible with original function)
  if (is.character(x$models)) {
    cat("Suggested Statistical Models:\n")
    for (model in x$models) {
      cat("  - ", model, "\n", sep = "")
    }
    cat("\n")
    
    # R packages section
    if (length(x$packages$r) > 0) {
      cat("Recommended R Packages:\n")
      for (pkg in x$packages$r) {
        cat("  - ", pkg, "\n", sep = "")
      }
      cat("\n")
    }
    
    # Python packages section (for compatibility)
    if (length(x$packages$python) > 0) {
      cat("Equivalent Python Packages:\n")
      for (pkg in x$packages$python) {
        cat("  - ", pkg, "\n", sep = "")
      }
      cat("\n")
    }
    
    invisible(x)
    return()
  }
  
  # Print detailed suggestions
  # Warnings section
  if (length(x$warnings) > 0) {
    cat("Important considerations:\n")
    for (warning_name in names(x$warnings)) {
      cat("  - ", x$warnings[[warning_name]], "\n", sep = "")
    }
    cat("\n")
  }
  
  # Primary model
  if (!is.null(x$models$primary)) {
    cat("Primary Recommended Model:\n")
    cat("  ", x$models$primary$name, "\n", sep = "")
    
    if (!is.null(x$models$primary$description)) {
      cat("  Description: ", x$models$primary$description, "\n", sep = "")
    }
    
    if (!is.null(x$models$primary$implementation)) {
      cat("  Implementation: ", x$models$primary$implementation, "\n", sep = "")
    }
    
    if (!is.null(x$models$primary$package) && x$models$primary$package != "stats") {
      cat("  Required package: ", x$models$primary$package, "\n", sep = "")
    }
    
    if (!is.null(x$models$primary$assumptions) && length(x$models$primary$assumptions) > 0) {
      cat("  Key assumptions:\n")
      for (assumption in x$models$primary$assumptions) {
        cat("    - ", assumption, "\n", sep = "")
      }
    }
    
    cat("\n")
  }
  
  # Alternative models
  if (length(x$models$alternatives) > 0) {
    cat("Alternative Models to Consider:\n")
    
    for (i in seq_along(x$models$alternatives)) {
      alt <- x$models$alternatives[[i]]
      
      cat("  ", i, ". ", alt$name, "\n", sep = "")
      
      if (!is.null(alt$description)) {
        cat("     Description: ", alt$description, "\n", sep = "")
      }
      
      if (!is.null(alt$when_to_use)) {
        cat("     When to use: ", alt$when_to_use, "\n", sep = "")
      }
      
      if (!is.null(alt$implementation)) {
        cat("     Implementation: ", alt$implementation, "\n", sep = "")
      }
      
      if (!is.null(alt$package) && alt$package != "stats") {
        cat("     Required package: ", alt$package, "\n", sep = "")
      }
      
      cat("\n")
    }
  }
  
  # Diagnostics to consider
  if (length(x$diagnostics) > 0) {
    cat("Recommended Diagnostics:\n")
    for (diagnostic in x$diagnostics) {
      cat("  - ", diagnostic, "\n", sep = "")
    }
    cat("\n")
  }
  
  # R packages summary
  if (length(x$packages$r) > 0) {
    cat("Required R Packages: ", paste(unique(x$packages$r), collapse = ", "), "\n\n", sep = "")
  }
  
  invisible(x)
}

#' Interactive model selection guide
#'
#' This function guides the user through an interactive selection of appropriate
#' statistical models based on their study design and data characteristics.
#'
#' @param data A data frame or tibble
#'
#' @return A list with model suggestions
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   patient_id = rep(1:20, each = 3),
#'   treatment = rep(c("A", "B"), times = 30),
#'   outcome = rnorm(60)
#' )
#' guide_model_selection(data)
#' }
guide_model_selection <- function(data) {
  cat("StatsAid Interactive Model Selection Guide\n")
  cat("=========================================\n\n")
  
  cat("Let's determine the appropriate model for your analysis.\n\n")
  
  # Ask about outcome variable
  cat("Available variables in your dataset:\n")
  for (i in seq_along(names(data))) {
    var_name <- names(data)[i]
    var_type <- class(data[[var_name]])[1]
    var_class <- if (is.numeric(data[[var_name]])) {
      "numeric"
    } else if (is.factor(data[[var_name]])) {
      paste0("factor (", nlevels(data[[var_name]]), " levels)")
    } else if (is.character(data[[var_name]])) {
      "character"
    } else if (is.logical(data[[var_name]])) {
      "logical"
    } else {
      var_type
    }
    cat(paste0("  ", i, ". ", var_name, " (", var_class, ")\n"))
  }
  
  outcome_var <- readline("\nWhich variable is your outcome/dependent variable? (enter name or number): ")
  
  # Convert numeric input to variable name
  if (grepl("^[0-9]+$", outcome_var)) {
    outcome_idx <- as.integer(outcome_var)
    if (outcome_idx >= 1 && outcome_idx <= length(names(data))) {
      outcome_var <- names(data)[outcome_idx]
    } else {
      cat("Invalid number. Using first variable as outcome.\n")
      outcome_var <- names(data)[1]
    }
  }
  
  # Validate outcome variable
  if (!outcome_var %in% names(data)) {
    cat("Variable '", outcome_var, "' not found. Using first variable as outcome.\n", sep = "")
    outcome_var <- names(data)[1]
  }
  
  cat("\nSelected outcome variable:", outcome_var, "\n\n")
  
  # Check for clustering
  clustering <- readline("Does your data contain multiple observations from the same subject/cluster? (y/n): ")
  has_clustering <- tolower(clustering) == "y"
  
  cluster_var <- NULL
  if (has_clustering) {
    cluster_prompt <- "Which variable identifies the clusters/subjects? (enter name or number): "
    cluster_var <- readline(cluster_prompt)
    
    # Convert numeric input to variable name
    if (grepl("^[0-9]+$", cluster_var)) {
      cluster_idx <- as.integer(cluster_var)
      if (cluster_idx >= 1 && cluster_idx <= length(names(data))) {
        cluster_var <- names(data)[cluster_idx]
      } else {
        cat("Invalid number. Attempting to detect clustering automatically.\n")
        cluster_var <- NULL
      }
    }
    
    # Validate cluster variable
    if (!is.null(cluster_var) && !cluster_var %in% names(data)) {
      cat("Variable '", cluster_var, "' not found. Attempting to detect clustering automatically.\n", sep = "")
      cluster_var <- NULL
    }
    
    if (is.null(cluster_var)) {
      # Try to detect clustering
      cluster_analysis <- detect_clusters(data)
      if (cluster_analysis$has_clustering && length(cluster_analysis$potential_cluster_vars) > 0) {
        cluster_var <- cluster_analysis$potential_cluster_vars[1]
        cat("Detected potential clustering variable:", cluster_var, "\n")
      }
    }
    
    # Check for nested clustering
    if (!is.null(cluster_var)) {
      nested <- readline("Is there nested clustering (e.g., patients within clinics)? (y/n): ")
      has_nested <- tolower(nested) == "y"
      
      if (has_nested) {
        higher_cluster_prompt <- "Which variable identifies the higher-level clusters? (enter name or number): "
        higher_cluster <- readline(higher_cluster_prompt)
        
        # Convert numeric input to variable name
        if (grepl("^[0-9]+$", higher_cluster)) {
          higher_idx <- as.integer(higher_cluster)
          if (higher_idx >= 1 && higher_idx <= length(names(data))) {
            higher_cluster <- names(data)[higher_idx]
          } else {
            cat("Invalid number. Using single-level clustering.\n")
            higher_cluster <- NULL
          }
        }
        
        # Validate higher cluster variable
        if (!is.null(higher_cluster) && !higher_cluster %in% names(data)) {
          cat("Variable '", higher_cluster, "' not found. Using single-level clustering.\n", sep = "")
          higher_cluster <- NULL
        }
        
        if (!is.null(higher_cluster)) {
          cluster_var <- list(level1 = cluster_var, level2 = higher_cluster)
        }
      }
    }
  }
  
  # Ask about study design
  cat("\nStudy Design Options:\n")
  cat("  1. Cross-sectional (data collected at a single time point)\n")
  cat("  2. Longitudinal/Repeated Measures (multiple observations over time)\n")
  cat("  3. Case-control (comparing cases with a condition to controls without it)\n")
  cat("  4. Cohort (following groups over time to observe outcomes)\n")
  cat("  5. Randomized Controlled Trial (RCT)\n")
  cat("  6. Other/Unknown\n")
  
  design_choice <- readline("\nWhich best describes your study design? (enter number): ")
  
  # Default to "unknown" if invalid input
  design_type <- "unknown"
  if (grepl("^[1-6]$", design_choice)) {
    design_idx <- as.integer(design_choice)
    designs <- c("cross_sectional", "longitudinal", "case_control", "cohort", "rct", "unknown")
    design_type <- designs[design_idx]
  }
  
  # Create design specification
  design <- study_design_spec(
    design_type = design_type,
    clustering = cluster_var
  )
  
  # Get and return model suggestions
  cat("\nAnalyzing your data and generating model suggestions...\n\n")
  suggestions <- suggest_models(data, outcome_var = outcome_var, design = design, detailed = TRUE)
  
  print(suggestions)
  
  invisible(suggestions)
}