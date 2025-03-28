context("Visualization functions")

test_that("plot_missing_values creates a ggplot object", {
  # Skip if ggplot2 is not available
  skip_if_not_installed("ggplot2")
  
  # Create test data with missing values
  test_data <- data.frame(
    a = c(1, 2, 3, NA, 5),
    b = c("x", NA, "z", "x", "y"),
    c = c(TRUE, FALSE, TRUE, NA, TRUE)
  )
  
  # Run the function
  result <- plot_missing_values(test_data)
  
  # Check if result is a ggplot object
  expect_is(result, "ggplot")
})

test_that("plot_missing_bar creates a ggplot object", {
  # Skip if ggplot2 is not available
  skip_if_not_installed("ggplot2")
  
  # Create test data with missing values
  test_data <- data.frame(
    a = c(1, 2, 3, NA, 5),
    b = c("x", NA, "z", "x", "y"),
    c = c(TRUE, FALSE, TRUE, NA, TRUE)
  )
  
  # Run the function
  result <- plot_missing_bar(test_data)
  
  # Check if result is a ggplot object
  expect_is(result, "ggplot")
})

test_that("plot_distributions creates a list of plots", {
  # Skip if ggplot2 is not available
  skip_if_not_installed("ggplot2")
  
  # Create test data
  test_data <- data.frame(
    a = rnorm(100),
    b = factor(sample(c("A", "B", "C"), 100, replace = TRUE))
  )
  
  # Run the function
  result <- plot_distributions(test_data)
  
  # Check if result has the right class
  expect_is(result, "statsaid_plots")
  
  # Check if numeric plots are present
  expect_true(!is.null(result$numeric))
  
  # Check if categorical plots are present
  expect_true(!is.null(result$categorical))
  
  # Check if at least one plot is a ggplot object
  expect_is(result$numeric[[1]], "ggplot")
})

test_that("plot_correlation_matrix creates a ggplot object", {
  # Skip if ggplot2 is not available
  skip_if_not_installed("ggplot2")
  
  # Create test data with numeric columns
  test_data <- data.frame(
    a = rnorm(100),
    b = rnorm(100),
    c = rnorm(100)
  )
  
  # Run the function
  result <- plot_correlation_matrix(test_data)
  
  # Check if result is a ggplot object
  expect_is(result, "ggplot")
})

test_that("plot_outliers creates a ggplot object", {
  # Skip if ggplot2 is not available
  skip_if_not_installed("ggplot2")
  
  # Create test data with outliers
  test_data <- data.frame(
    a = c(rnorm(95), 10, -10, 15, -15, 20),
    b = c(rnorm(95), 8, -8, 12, -12, 15)
  )
  
  # Run the function
  result <- plot_outliers(test_data)
  
  # Check if result is a ggplot object
  expect_is(result, "ggplot")
})