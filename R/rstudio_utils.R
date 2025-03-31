#' Safely detect if running in RStudio
#'
#' @return Logical indicating whether code is running in RStudio
#' @keywords internal
is_running_in_rstudio <- function() {
  # Method 1: Check for RStudio API
  if (exists(".rs.api.getActiveProject", mode = "function")) {
    return(TRUE)
  }
  
  # Method 2: Check environment variable
  if (Sys.getenv("RSTUDIO") == "1") {
    return(TRUE)
  }
  
  # Method 3: Safe check for rstudio object and isDesktop function
  has_rs_desktop <- tryCatch({
    exists("rstudio", mode = "environment") && 
      exists(".rs.isDesktop", envir = rstudio, mode = "function")
  }, error = function(e) {
    return(FALSE)
  })
  
  if (has_rs_desktop) {
    # Safely try to call the function
    rs_is_desktop <- tryCatch({
      get(".rs.isDesktop", envir = rstudio, mode = "function")()
    }, error = function(e) {
      return(FALSE)
    })
    
    return(!is.null(rs_is_desktop) && rs_is_desktop)
  }
  
  return(FALSE)
}

#' Safely execute code that might interact with RStudio
#'
#' @param expr An expression to evaluate
#' @param fallback_expr An expression to evaluate if the main expression fails due to RStudio issues
#'
#' @return The result of expr or fallback_expr
#' @keywords internal
with_safe_rstudio <- function(expr, fallback_expr = NULL) {
  result <- tryCatch({
    eval(expr)
  }, error = function(e) {
    # Check if error is related to RStudio detection
    if (grepl("rstudio", e$message, fixed = TRUE)) {
      if (!is.null(fallback_expr)) {
        return(eval(fallback_expr))
      } else {
        warning("RStudio detection error: ", e$message)
        return(NULL)
      }
    } else {
      # Re-throw other errors
      stop(e)
    }
  })
  
  return(result)
}

#' Safely display a plot object
#'
#' This function safely displays plot objects, handling potential RStudio errors
#'
#' @param plot_obj A ggplot object to display
#'
#' @return The plot object invisibly
#' @export
safe_plot <- function(plot_obj) {
  if (is.null(plot_obj)) {
    message("No plot object to display")
    return(invisible(NULL))
  }
  
  # Handle non-ggplot objects
  if (!requireNamespace("ggplot2", quietly = TRUE) || 
      !inherits(plot_obj, c("gg", "ggplot"))) {
    return(plot(plot_obj))
  }
  
  tryCatch({
    print(plot_obj)
  }, error = function(e) {
    # If there's an RStudio-related error
    if (grepl("rstudio", e$message, fixed = TRUE)) {
      message("Using alternative display method due to RStudio-related error")
      # Use grid functions to render the plot
      if (requireNamespace("grid", quietly = TRUE)) {
        grid::grid.newpage()
        grid::grid.draw(ggplot2::ggplotGrob(plot_obj))
      } else {
        # Last resort - try to use base plotting
        warning("Package 'grid' is needed for better plot display. Using simplified output.")
        
        # Extract data from ggplot if possible
        if (is.list(plot_obj$data) || is.data.frame(plot_obj$data)) {
          # Very basic representation - not a perfect representation
          # but better than nothing
          if (length(plot_obj$layers) > 0 && 
              grepl("Histogram", plot_obj$labels$title, fixed = TRUE)) {
            # Attempt to recreate a basic histogram
            x_var <- as.character(plot_obj$mapping$x)[2]
            if (!is.null(x_var) && x_var %in% names(plot_obj$data)) {
              hist(plot_obj$data[[x_var]], 
                   main = plot_obj$labels$title,
                   xlab = plot_obj$labels$x)
            }
          } else if (length(plot_obj$layers) > 0 && 
                     grepl("bar", class(plot_obj$layers[[1]]$geom)[1], fixed = TRUE)) {
            # Attempt to recreate a basic bar chart
            x_var <- as.character(plot_obj$mapping$x)[2]
            if (!is.null(x_var) && x_var %in% names(plot_obj$data)) {
              barplot(table(plot_obj$data[[x_var]]), 
                      main = plot_obj$labels$title)
            }
          }
        }
      }
    } else {
      # For other errors, show the error
      stop(e)
    }
  })
  
  invisible(plot_obj)
}

#' View all plots from a plot_distributions() result
#'
#' @param plots A list of plots from plot_distributions()
#' @param type Character, either "numeric", "categorical", or "all"
#' @param pause Logical, whether to pause between plots
#'
#' @return NULL invisibly
#' @export
view_all_plots <- function(plots, type = "all", pause = TRUE) {
  if (!is.list(plots)) {
    stop("Input must be a list of plots")
  }
  
  # Check which types of plots to show
  show_numeric <- type %in% c("all", "numeric") && !is.null(plots$numeric) && length(plots$numeric) > 0
  show_categorical <- type %in% c("all", "categorical") && !is.null(plots$categorical) && length(plots$categorical) > 0
  
  # Show numeric plots
  if (show_numeric) {
    cat("\nNumeric Variables:\n")
    cat("=================\n\n")
    
    # Get names of numeric plots
    numeric_names <- names(plots$numeric)
    
    # Loop through numeric plots
    for (i in seq_along(plots$numeric)) {
      var_name <- ifelse(is.null(numeric_names[i]) || numeric_names[i] == "", 
                        paste("Variable", i), 
                        numeric_names[i])
      
      cat("Plotting:", var_name, "\n")
      
      # Try to safely display the plot
      result <- tryCatch({
        safe_plot(plots$numeric[[i]])
        TRUE
      }, error = function(e) {
        # Create a basic summary instead
        message("Could not display plot: ", e$message)
        FALSE
      })
      
      # Pause between plots if requested
      if (pause && i < length(plots$numeric) && result) {
        readline(prompt = "Press Enter for next plot...")
      }
    }
  }
  
  # Show categorical plots
  if (show_categorical) {
    cat("\nCategorical Variables:\n")
    cat("====================\n\n")
    
    # Get names of categorical plots
    cat_names <- names(plots$categorical)
    
    # Loop through categorical plots
    for (i in seq_along(plots$categorical)) {
      var_name <- ifelse(is.null(cat_names[i]) || cat_names[i] == "", 
                        paste("Variable", i), 
                        cat_names[i])
      
      cat("Plotting:", var_name, "\n")
      
      # Try to safely display the plot
      result <- tryCatch({
        safe_plot(plots$categorical[[i]])
        TRUE
      }, error = function(e) {
        # Create a basic summary instead
        message("Could not display plot: ", e$message)
        FALSE
      })
      
      # Pause between plots if requested
      if (pause && i < length(plots$categorical) && result) {
        readline(prompt = "Press Enter for next plot...")
      }
    }
  }
  
  invisible(NULL)
}