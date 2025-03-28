#' Load data from various file formats
#'
#' This function loads data from various file formats including CSV, TSV, Excel, and RDS.
#'
#' @param file_path Character string specifying the path to the data file
#' @param ... Additional arguments to pass to the read function
#'
#' @return A data frame or tibble containing the loaded data
#' @export
#'
#' @examples
#' \dontrun{
#' data <- load_data("path/to/data.csv")
#' data <- load_data("path/to/data.xlsx", sheet = "Sheet1")
#' }
load_data <- function(file_path, ...) {
  
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Get file extension
  file_ext <- tolower(tools::file_ext(file_path))
  
  # Load data based on file extension
  if (file_ext == "csv") {
    return(utils::read.csv(file_path, stringsAsFactors = FALSE, ...))
  } else if (file_ext %in% c("tsv", "txt")) {
    return(utils::read.delim(file_path, stringsAsFactors = FALSE, ...))
  } else if (file_ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' needed for this function to work. Please install it.")
    }
    return(readxl::read_excel(file_path, ...))
  } else if (file_ext == "rds") {
    return(readRDS(file_path, ...))
  } else if (file_ext == "rdata" || file_ext == "rda") {
    env <- new.env()
    load(file_path, envir = env, ...)
    # Return the first object
    return(env[[ls(env)[1]]])
  } else {
    stop("Unsupported file format: ", file_ext)
  }
}

#' Generate a comprehensive overview of the dataset
#'
#' This function analyzes a dataset and returns a list containing various summary statistics.
#'
#' @param data A data frame or tibble
#'
#' @return A list containing summary statistics and dataset characteristics
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   a = c(1, 2, 3, NA),
#'   b = c("x", "y", "z", "x")
#' )
#' explore(data)
#' }
explore <- function(data) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Initialize results list
  results <- list()
  
  # Basic information
  results$shape <- dim(data)
  results$columns <- names(data)
  results$dtypes <- sapply(data, function(x) class(x)[1])
  
  # Summary statistics
  results$summary <- lapply(data, function(x) {
    if (is.numeric(x)) {
      c(
        min = min(x, na.rm = TRUE),
        q1 = stats::quantile(x, 0.25, na.rm = TRUE),
        median = stats::median(x, na.rm = TRUE),
        mean = mean(x, na.rm = TRUE),
        q3 = stats::quantile(x, 0.75, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        sd = stats::sd(x, na.rm = TRUE),
        na_count = sum(is.na(x))
      )
    } else if (is.factor(x) || is.character(x)) {
      tab <- table(x, useNA = "ifany")
      c(
        unique_count = length(unique(x[!is.na(x)])),
        mode = names(which.max(tab[!is.na(names(tab))])),
        mode_freq = max(tab[!is.na(names(tab))]),
        na_count = sum(is.na(x))
      )
    } else if (inherits(x, "Date") || inherits(x, "POSIXt")) {
      c(
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        range_days = as.numeric(difftime(max(x, na.rm = TRUE), 
                                         min(x, na.rm = TRUE), 
                                         units = "days")),
        na_count = sum(is.na(x))
      )
    } else {
      c(
        class = class(x)[1],
        na_count = sum(is.na(x))
      )
    }
  })
  
  # Missing values
  results$missing <- list(
    total = sapply(data, function(x) sum(is.na(x))),
    percentage = sapply(data, function(x) mean(is.na(x)) * 100)
  )
  
  # Detect variable types
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  categorical_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
  date_cols <- names(data)[sapply(data, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
  logical_cols <- names(data)[sapply(data, is.logical)]
  
  results$variable_types <- list(
    numeric = numeric_cols,
    categorical = categorical_cols,
    date = date_cols,
    logical = logical_cols
  )
  
  # Check for outliers in numeric columns
  if (length(numeric_cols) > 0) {
    results$outliers <- lapply(data[numeric_cols], function(x) {
      if (sum(!is.na(x)) > 3) {  # Only calculate if we have enough data
        q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
        q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        lower_bound <- q1 - 1.5 * iqr
        upper_bound <- q3 + 1.5 * iqr
        outliers <- x[x < lower_bound | x > upper_bound]
        list(
          count = sum(!is.na(outliers)),
          percentage = sum(!is.na(outliers)) / length(x) * 100,
          range = c(lower_bound, upper_bound)
        )
      } else {
        list(
          count = NA,
          percentage = NA,
          range = c(NA, NA)
        )
      }
    })
  }
  
  # Check for unique values in categorical columns
  if (length(categorical_cols) > 0) {
    results$categorical_counts <- lapply(data[categorical_cols], function(x) {
      counts <- table(x, useNA = "ifany")
      top_values <- sort(counts[!is.na(names(counts))], decreasing = TRUE)
      if (length(top_values) > 10) {
        top_values <- top_values[1:10]
      }
      list(
        unique_count = length(unique(x[!is.na(x)])),
        top_values = as.list(top_values)
      )
    })
  }
  
  # Return results
  class(results) <- c("statsaid_explore", "list")
  return(results)
}

#' Print method for statsaid_explore objects
#'
#' @param x An object of class statsaid_explore
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_explore <- function(x, ...) {
  cat("StatsAid Data Exploration\n")
  cat("==========================\n\n")
  
  # Basic info
  cat("Dataset dimensions:", x$shape[1], "rows,", x$shape[2], "columns\n\n")
  
  # Variable types
  cat("Variable types:\n")
  cat("  Numeric:", length(x$variable_types$numeric), "\n")
  cat("  Categorical:", length(x$variable_types$categorical), "\n")
  cat("  Date/Time:", length(x$variable_types$date), "\n")
  cat("  Logical:", length(x$variable_types$logical), "\n\n")
  
  # Missing data
  missing_cols <- names(which(x$missing$total > 0))
  if (length(missing_cols) > 0) {
    cat("Missing data:\n")
    missing_df <- data.frame(
      Column = missing_cols,
      Missing = x$missing$total[missing_cols],
      Percentage = round(x$missing$percentage[missing_cols], 2)
    )
    print(missing_df, row.names = FALSE)
    cat("\n")
  } else {
    cat("No missing data\n\n")
  }
  
  # Outliers
  if (!is.null(x$outliers)) {
    outlier_cols <- names(which(sapply(x$outliers, function(o) !is.na(o$count) && o$count > 0)))
    if (length(outlier_cols) > 0) {
      cat("Potential outliers detected in:\n")
      outlier_df <- data.frame(
        Column = outlier_cols,
        Count = sapply(x$outliers[outlier_cols], function(o) o$count),
        Percentage = round(sapply(x$outliers[outlier_cols], function(o) o$percentage), 2)
      )
      print(outlier_df, row.names = FALSE)
      cat("\n")
    }
  }
  
  invisible(x)
}

#' Suggest preprocessing steps based on the dataset characteristics
#'
#' This function analyzes a dataset and returns preprocessing suggestions.
#'
#' @param data A data frame or tibble
#'
#' @return A list containing preprocessing suggestions
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   a = c(1, 2, 3, NA),
#'   b = c("x", "y", "z", "x")
#' )
#' suggest_preprocessing(data)
#' }
suggest_preprocessing <- function(data) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Get data overview
  overview <- explore(data)
  
  # Initialize suggestions
  suggestions <- list()
  
  # Suggest handling missing values
  missing_pct <- overview$missing$percentage
  suggestions$missing_values <- list()
  
  for (col in names(which(missing_pct > 0))) {
    if (missing_pct[col] > 50) {
      suggestions$missing_values[[col]] <- "Consider dropping this column due to high missingness (>50%)"
    } else if (missing_pct[col] > 20) {
      suggestions$missing_values[[col]] <- "Consider imputation or creating a 'missing' indicator"
    } else {
      if (col %in% overview$variable_types$numeric) {
        suggestions$missing_values[[col]] <- "Impute with mean, median, or use KNN imputation"
      } else if (col %in% overview$variable_types$categorical) {
        suggestions$missing_values[[col]] <- "Impute with mode or create a new 'missing' category"
      }
    }
  }
  
  # Suggest normalization for numeric features
  suggestions$normalization <- list()
  for (col in overview$variable_types$numeric) {
    # Check if column might benefit from transformation
    if (!is.null(overview$outliers[[col]]) && !is.na(overview$outliers[[col]]$count) && 
        overview$outliers[[col]]$count > 0) {
      
      # Calculate skewness if moments package is available
      if (requireNamespace("moments", quietly = TRUE)) {
        skewness <- moments::skewness(data[[col]], na.rm = TRUE)
        if (abs(skewness) > 1) {
          if (skewness > 0) {
            suggestions$normalization[[col]] <- "Consider log or square root transformation (right-skewed)"
          } else {
            suggestions$normalization[[col]] <- "Consider square or cube transformation (left-skewed)"
          }
        } else {
          suggestions$normalization[[col]] <- "Consider min-max scaling or standardization"
        }
      } else {
        suggestions$normalization[[col]] <- "Consider scaling or transformation to handle outliers"
      }
    } else {
      suggestions$normalization[[col]] <- "Standard scaling should be sufficient"
    }
  }
  
  # Suggest encoding for categorical features
  suggestions$encoding <- list()
  for (col in overview$variable_types$categorical) {
    unique_count <- overview$categorical_counts[[col]]$unique_count
    if (unique_count == 2) {
      suggestions$encoding[[col]] <- "Binary encoding (0/1)"
    } else if (unique_count > 2 && unique_count <= 10) {
      suggestions$encoding[[col]] <- "One-hot encoding"
    } else {
      suggestions$encoding[[col]] <- "Consider label encoding, target encoding, or embedding techniques"
    }
  }
  
  # Return suggestions
  class(suggestions) <- c("statsaid_suggestions", "list")
  return(suggestions)
}

#' Print method for statsaid_suggestions objects
#'
#' @param x An object of class statsaid_suggestions
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_suggestions <- function(x, ...) {
  cat("StatsAid Preprocessing Suggestions\n")
  cat("==================================\n\n")
  
  # Missing values section
  if (length(x$missing_values) > 0) {
    cat("Missing Values:\n")
    for (col in names(x$missing_values)) {
      cat("  ", col, ": ", x$missing_values[[col]], "\n", sep = "")
    }
    cat("\n")
  }
  
  # Normalization section
  if (length(x$normalization) > 0) {
    cat("Feature Scaling/Normalization:\n")
    for (col in names(x$normalization)) {
      cat("  ", col, ": ", x$normalization[[col]], "\n", sep = "")
    }
    cat("\n")
  }
  
  # Encoding section
  if (length(x$encoding) > 0) {
    cat("Categorical Encoding:\n")
    for (col in names(x$encoding)) {
      cat("  ", col, ": ", x$encoding[[col]], "\n", sep = "")
    }
    cat("\n")
  }
  
  invisible(x)
}

#' Suggest appropriate statistical models and packages
#'
#' This function suggests appropriate statistical models and packages based on the dataset
#' and study design.
#'
#' @param data A data frame or tibble
#' @param study_design Character string specifying the study design. Options include
#'   'case_control', 'cohort', 'cross_sectional', 'longitudinal', 'rct', or NULL.
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
suggest_models <- function(data, study_design = NULL) {
  
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
  
  # Models section
  if (length(x$models) > 0) {
    cat("Suggested Statistical Models:\n")
    for (model in x$models) {
      cat("  - ", model, "\n", sep = "")
    }
    cat("\n")
  }
  
  # R packages section
  if (length(x$packages$r) > 0) {
    cat("Recommended R Packages:\n")
    for (pkg in x$packages$r) {
      cat("  - ", pkg, "\n", sep = "")
    }
    cat("\n")
  }
  
  # Python packages section
  if (length(x$packages$python) > 0) {
    cat("Equivalent Python Packages:\n")
    for (pkg in x$packages$python) {
      cat("  - ", pkg, "\n", sep = "")
    }
    cat("\n")
  }
  
  invisible(x)
}