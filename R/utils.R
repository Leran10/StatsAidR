#' Check if code is running in RStudio safely
#'
#' This function checks if the code is running in RStudio without causing errors
#'
#' @return Logical indicating whether the code is running in RStudio
#' @keywords internal
is_rstudio <- function() {
  # First approach - Check for RStudio API
  if (exists(".rs.api.getActiveProject", mode = "function")) {
    return(TRUE)
  }
  
  # Second approach - Check environment variable
  if (Sys.getenv("RSTUDIO") == "1") {
    return(TRUE)
  }
  
  # Third approach - Safely check for rstudio package
  rstudio_available <- tryCatch({
    exists("rstudio", mode = "environment") && 
      exists(".rs.isDesktop", envir = rstudio, mode = "function")
  }, error = function(e) {
    return(FALSE)
  })
  
  return(rstudio_available)
}

#' Safe wrapper for visdat::vis_miss function
#'
#' @param data A data frame to visualize
#' @param ... Additional arguments passed to vis_miss
#'
#' @return A ggplot object or NULL if visualization fails
#' @keywords internal
safe_vis_miss <- function(data, ...) {
  if (!requireNamespace("visdat", quietly = TRUE)) {
    warning("Package 'visdat' is required but not available")
    return(NULL)
  }
  
  result <- tryCatch({
    # If in RStudio, try the normal way
    if (is_rstudio()) {
      visdat::vis_miss(data, ...)
    } else {
      # Custom implementation to avoid RStudio dependencies
      # Basic version that works without RStudio functions
      missing_data <- reshape2::melt(
        is.na(data), 
        varnames = c("row", "variable"), 
        value.name = "missing"
      )
      
      ggplot2::ggplot(missing_data, ggplot2::aes(x = variable, y = row, fill = missing)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_manual(values = c("FALSE" = "grey80", "TRUE" = "#0172B1")) +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Missing values", 
                     x = "Variables", 
                     y = "Observations", 
                     fill = "Missing") +
        ggplot2::theme(axis.text.y = ggplot2::element_blank())
    }
  }, error = function(e) {
    warning("Could not generate missing patterns visualization: ", e$message)
    return(NULL)
  })
  
  return(result)
}

#' Safe wrapper for naniar::gg_miss_upset function
#'
#' @param data A data frame to visualize
#' @param ... Additional arguments passed to gg_miss_upset
#'
#' @return A ggplot object or NULL if visualization fails
#' @keywords internal
safe_gg_miss_upset <- function(data, ...) {
  if (!requireNamespace("naniar", quietly = TRUE)) {
    warning("Package 'naniar' is required but not available")
    return(NULL)
  }
  
  result <- tryCatch({
    naniar::gg_miss_upset(data, ...)
  }, error = function(e) {
    warning("Could not generate missing combinations visualization: ", e$message)
    return(NULL)
  })
  
  return(result)
}