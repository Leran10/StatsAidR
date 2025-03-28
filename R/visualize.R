#' Create a ggplot2 heatmap of missing values
#'
#' This function creates a heatmap of missing values in the dataset.
#'
#' @param data A data frame or tibble
#' @param title Character string specifying the plot title, default is "Missing Values Heatmap"
#' @param limit Integer specifying the maximum number of rows to display, default is 100
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   a = c(1, 2, 3, NA),
#'   b = c("x", "y", NA, "x")
#' )
#' plot_missing_values(data)
#' }
plot_missing_values <- function(data, title = "Missing Values Heatmap", limit = 100) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Check if required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE) ||
      !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Packages 'ggplot2', 'tidyr', and 'dplyr' are needed for this function to work.")
  }
  
  # Sample data if too large
  if (nrow(data) > limit) {
    data <- data[sample(1:nrow(data), limit), ]
  }
  
  # Prepare data for visualization
  missing_data <- data %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = -row_id, names_to = "column", values_to = "value") %>%
    dplyr::mutate(is_missing = is.na(value))
  
  # Create heatmap
  ggplot(missing_data, aes(x = column, y = row_id, fill = is_missing)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(values = c("FALSE" = "#FFFFFF", "TRUE" = "#2171b5"),
                               name = "Missing") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank()) +
    ggplot2::labs(title = title,
                  x = "Features",
                  y = "Samples")
}

#' Create a bar chart showing missing value percentages
#'
#' This function creates a bar chart showing the percentage of missing values per column.
#'
#' @param data A data frame or tibble
#' @param threshold Numeric value specifying the minimum percentage to display, default is 0
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   a = c(1, 2, 3, NA),
#'   b = c("x", "y", NA, "x")
#' )
#' plot_missing_bar(data)
#' }
plot_missing_bar <- function(data, threshold = 0) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Check if required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE) ||
      !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Packages 'ggplot2', 'tidyr', and 'dplyr' are needed for this function to work.")
  }
  
  # Calculate percentage of missing values per column
  missing_pct <- sapply(data, function(x) mean(is.na(x)) * 100)
  
  # Filter columns with missing values above threshold
  missing_pct <- missing_pct[missing_pct > threshold]
  
  if (length(missing_pct) == 0) {
    # No missing values above threshold
    empty_plot <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, 
                        label = "No missing values above threshold in the dataset") +
      ggplot2::theme_void()
    return(empty_plot)
  }
  
  # Create data frame for plotting
  missing_df <- data.frame(
    column = names(missing_pct),
    percent = missing_pct
  ) %>%
    dplyr::arrange(dplyr::desc(percent))
  
  # Create bar chart
  ggplot2::ggplot(missing_df, aes(x = stats::reorder(column, percent), y = percent)) +
    ggplot2::geom_bar(stat = "identity", fill = "#2171b5") +
    ggplot2::geom_text(aes(label = sprintf("%.1f%%", percent)), 
                        hjust = -0.1, size = 3) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Percentage of Missing Values by Feature",
                  x = "Features",
                  y = "Missing (%)") +
    ggplot2::scale_y_continuous(limits = c(0, max(missing_df$percent) * 1.1))
}

#' Create distribution plots for numeric and categorical variables
#'
#' This function creates distribution plots for numeric and categorical variables.
#'
#' @param data A data frame or tibble
#' @param max_cols Integer specifying the maximum number of columns to plot, default is 12
#' @param numeric_cols Character vector specifying the numeric columns to plot, 
#'   default is NULL (all numeric columns)
#' @param categorical_cols Character vector specifying the categorical columns to plot,
#'   default is NULL (all categorical columns)
#'
#' @return A list of ggplot2 objects
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   a = c(1, 2, 3, 4),
#'   b = c("x", "y", "z", "x")
#' )
#' plots <- plot_distributions(data)
#' }
plot_distributions <- function(data, max_cols = 12, numeric_cols = NULL, categorical_cols = NULL) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Check if required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE) || 
      !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Packages 'ggplot2' and 'dplyr' are needed for this function to work.")
  }
  
  # Initialize results list
  plots <- list()
  
  # Identify variable types if not specified
  if (is.null(numeric_cols)) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
  }
  
  if (is.null(categorical_cols)) {
    categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  }
  
  # Limit the number of columns to plot
  if (length(numeric_cols) > max_cols) {
    numeric_cols <- numeric_cols[1:max_cols]
  }
  
  if (length(categorical_cols) > max_cols) {
    categorical_cols <- categorical_cols[1:max_cols]
  }
  
  # Plot distributions for numeric variables
  if (length(numeric_cols) > 0) {
    plots$numeric <- list()
    
    for (col in numeric_cols) {
      # Skip if too many missing values
      if (mean(is.na(data[[col]])) > 0.9) {
        next
      }
      
      # Calculate statistics for annotations
      stats_text <- sprintf(
        "Mean: %.2f\nMedian: %.2f\nStd: %.2f\nMissing: %d (%.1f%%)",
        mean(data[[col]], na.rm = TRUE),
        median(data[[col]], na.rm = TRUE),
        sd(data[[col]], na.rm = TRUE),
        sum(is.na(data[[col]])),
        mean(is.na(data[[col]])) * 100
      )
      
      # Create histogram with density
      p <- ggplot2::ggplot(data, aes_string(x = col)) +
        ggplot2::geom_histogram(aes(y = ..density..), 
                                fill = "#69b3a2", 
                                color = "white", 
                                alpha = 0.7,
                                bins = 30) +
        ggplot2::geom_density(alpha = 0.2, fill = "#404080") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = paste("Distribution of", col),
                      x = col,
                      y = "Density") +
        ggplot2::annotate("text", x = Inf, y = Inf, 
                           label = stats_text, 
                           hjust = 1.1, vjust = 1.1, 
                           size = 3)
      
      plots$numeric[[col]] <- p
    }
  }
  
  # Plot distributions for categorical variables
  if (length(categorical_cols) > 0) {
    plots$categorical <- list()
    
    for (col in categorical_cols) {
      # Skip if too many missing values
      if (mean(is.na(data[[col]])) > 0.9) {
        next
      }
      
      # Get value counts
      value_counts <- data %>%
        dplyr::count(!!rlang::sym(col), sort = TRUE) %>%
        dplyr::filter(!is.na(!!rlang::sym(col)))
      
      # Limit categories if too many
      if (nrow(value_counts) > 15) {
        other_count <- sum(value_counts$n[15:nrow(value_counts)])
        value_counts <- value_counts[1:14, ]
        value_counts <- rbind(value_counts, 
                              data.frame(value_counts[1, 1, drop = FALSE], n = other_count))
        value_counts[15, 1] <- "Other"
      }
      
      # Calculate statistics for annotations
      stats_text <- sprintf(
        "Unique values: %d\nMost common: %s\nMissing: %d (%.1f%%)",
        length(unique(data[[col]][!is.na(data[[col]])])),
        as.character(value_counts[[1]][1]),
        sum(is.na(data[[col]])),
        mean(is.na(data[[col]])) * 100
      )
      
      # Create bar plot
      p <- ggplot2::ggplot(value_counts, aes_string(x = col, y = "n")) +
        ggplot2::geom_bar(stat = "identity", fill = "#69b3a2", alpha = 0.7) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(title = paste("Distribution of", col),
                      x = col,
                      y = "Count") +
        ggplot2::annotate("text", x = Inf, y = Inf, 
                           label = stats_text, 
                           hjust = 1.1, vjust = 1.1, 
                           size = 3)
      
      plots$categorical[[col]] <- p
    }
  }
  
  # Return list of plots
  class(plots) <- c("statsaid_plots", "list")
  return(plots)
}

#' Print method for statsaid_plots objects
#'
#' @param x An object of class statsaid_plots
#' @param ... Additional arguments
#'
#' @return Invisibly returns the object
#' @export
print.statsaid_plots <- function(x, ...) {
  # Check if we have anything to print
  if (length(x) == 0) {
    cat("No plots to display\n")
    return(invisible(x))
  }
  
  # Print message about plotting
  cat("StatsAid Distribution Plots:\n")
  
  # Count numeric plots
  if (!is.null(x$numeric)) {
    cat("Numeric variables:", length(x$numeric), "\n")
  }
  
  # Count categorical plots
  if (!is.null(x$categorical)) {
    cat("Categorical variables:", length(x$categorical), "\n")
  }
  
  cat("\nUse plot() on individual elements to display them.\n")
  cat("For example: plot(x$numeric[[1]]) or plot(x$categorical[[1]])\n")
  
  # Return invisibly
  invisible(x)
}

#' Create a correlation matrix heatmap
#'
#' This function creates a correlation matrix heatmap for numeric variables.
#'
#' @param data A data frame or tibble
#' @param method Character string specifying the correlation method, default is "pearson"
#' @param use Character string specifying how to handle missing values, default is "pairwise.complete.obs"
#' @param sig_level Numeric value specifying the significance level for correlation testing, default is 0.05
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   a = c(1, 2, 3, 4),
#'   b = c(2, 3, 4, 5),
#'   c = c(0, 1, 0, 1)
#' )
#' plot_correlation_matrix(data)
#' }
plot_correlation_matrix <- function(data, method = "pearson", 
                                    use = "pairwise.complete.obs", 
                                    sig_level = 0.05) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Check if required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE) ||
      !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Packages 'ggplot2', 'tidyr', and 'dplyr' are needed for this function to work.")
  }
  
  # Select only numeric columns
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  
  if (ncol(numeric_data) <= 1) {
    # Not enough numeric columns for correlation
    empty_plot <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, 
                        label = "Not enough numeric columns to compute correlation matrix") +
      ggplot2::theme_void()
    return(empty_plot)
  }
  
  # Compute correlation matrix
  corr_matrix <- cor(numeric_data, method = method, use = use)
  
  # Compute p-values
  p_matrix <- matrix(NA, nrow = ncol(numeric_data), ncol = ncol(numeric_data))
  
  for (i in 1:ncol(numeric_data)) {
    for (j in 1:ncol(numeric_data)) {
      if (i != j) {
        test <- cor.test(numeric_data[, i], numeric_data[, j], method = method)
        p_matrix[i, j] <- test$p.value
      }
    }
  }
  
  rownames(p_matrix) <- colnames(p_matrix) <- colnames(numeric_data)
  
  # Prepare data for plotting
  corr_df <- reshape2::melt(corr_matrix) %>%
    dplyr::rename(variable1 = Var1, variable2 = Var2, correlation = value)
  
  p_df <- reshape2::melt(p_matrix) %>%
    dplyr::rename(variable1 = Var1, variable2 = Var2, p_value = value)
  
  # Combine correlation values and p-values
  plot_df <- dplyr::left_join(corr_df, p_df, by = c("variable1", "variable2")) %>%
    dplyr::mutate(
      significance = ifelse(is.na(p_value), "", 
                           ifelse(p_value < 0.001, "***",
                                 ifelse(p_value < 0.01, "**",
                                       ifelse(p_value < 0.05, "*", "")))),
      label = ifelse(variable1 == variable2, "",
                    sprintf("%.2f%s", correlation, significance))
    )
  
  # Create heatmap
  ggplot2::ggplot(plot_df, aes(x = variable1, y = variable2, fill = correlation)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(aes(label = label), size = 3) +
    ggplot2::scale_fill_gradient2(low = "#0061AA", mid = "white", high = "#ED0400",
                                 midpoint = 0, limits = c(-1, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = paste(method, "Correlation Matrix"),
                 x = "",
                 y = "",
                 fill = "Correlation")
}

#' Create boxplots to visualize outliers
#'
#' This function creates boxplots to visualize outliers in numeric variables.
#'
#' @param data A data frame or tibble
#' @param columns Character vector specifying the columns to plot, default is NULL (all numeric columns)
#' @param max_cols Integer specifying the maximum number of columns to plot, default is 12
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   a = c(1, 2, 3, 30),
#'   b = c(2, 3, 4, 5),
#'   c = c(0, 1, 0, 10)
#' )
#' plot_outliers(data)
#' }
plot_outliers <- function(data, columns = NULL, max_cols = 12) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Check if required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE)) {
    stop("Packages 'ggplot2' and 'tidyr' are needed for this function to work.")
  }
  
  # Select columns to plot
  if (is.null(columns)) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    if (length(numeric_cols) == 0) {
      # No numeric columns
      empty_plot <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                          label = "No numeric columns to check for outliers") +
        ggplot2::theme_void()
      return(empty_plot)
    }
    
    # Limit the number of columns
    if (length(numeric_cols) > max_cols) {
      numeric_cols <- numeric_cols[1:max_cols]
    }
  } else {
    numeric_cols <- columns
  }
  
  # Prepare data for plotting
  plot_data <- data[, numeric_cols, drop = FALSE] %>%
    tidyr::pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
  
  # Create boxplot
  ggplot2::ggplot(plot_data, aes(x = variable, y = value)) +
    ggplot2::geom_boxplot(fill = "#69b3a2", alpha = 0.7, outlier.color = "red", outlier.alpha = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = "Boxplots for Outlier Detection",
                 x = "Variables",
                 y = "Values")
}

#' Create pairwise plots for numeric variables
#'
#' This function creates pairwise scatterplots for numeric variables with optional grouping.
#'
#' @param data A data frame or tibble
#' @param columns Character vector specifying the columns to plot, default is NULL (all numeric columns)
#' @param max_cols Integer specifying the maximum number of columns to include, default is 5
#' @param hue Character string specifying the column to use for color encoding, default is NULL
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   a = c(1, 2, 3, 4),
#'   b = c(2, 3, 4, 5),
#'   c = c(0, 1, 0, 1),
#'   group = c("A", "A", "B", "B")
#' )
#' plot_pairplot(data, hue = "group")
#' }
plot_pairplot <- function(data, columns = NULL, max_cols = 5, hue = NULL) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }
  
  # Check if GGally is available
  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("Package 'GGally' is needed for this function to work.")
  }
  
  # Select columns to plot
  if (is.null(columns)) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    if (length(numeric_cols) <= 1) {
      # Not enough numeric columns for pairplot
      empty_plot <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                          label = "Not enough numeric columns to create pairplot") +
        ggplot2::theme_void()
      return(empty_plot)
    }
    
    # Limit the number of columns
    if (length(numeric_cols) > max_cols) {
      # Select columns with highest variance
      variances <- sapply(data[numeric_cols], var, na.rm = TRUE)
      numeric_cols <- names(sort(variances, decreasing = TRUE))[1:max_cols]
    }
  } else {
    numeric_cols <- columns
  }
  
  # Create subset of data for plotting
  plot_data <- data[, c(numeric_cols, hue), drop = FALSE]
  
  # Create pairplot
  if (!is.null(hue) && hue %in% names(data)) {
    GGally::ggpairs(
      plot_data,
      columns = 1:length(numeric_cols),
      mapping = ggplot2::aes(color = .data[[hue]], alpha = 0.7),
      upper = list(continuous = "cor"),
      lower = list(continuous = "points"),
      diag = list(continuous = "densityDiag"),
      title = "Pairwise Relationships"
    )
  } else {
    GGally::ggpairs(
      plot_data,
      columns = 1:length(numeric_cols),
      upper = list(continuous = "cor"),
      lower = list(continuous = "points"),
      diag = list(continuous = "densityDiag"),
      title = "Pairwise Relationships"
    )
  }
}