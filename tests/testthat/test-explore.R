context("Data exploration functions")

test_that("explore function returns correct structure", {
  # Create test data
  test_data <- data.frame(
    a = c(1, 2, 3, NA),
    b = c("x", "y", "z", "x"),
    c = c(TRUE, FALSE, TRUE, TRUE)
  )
  
  # Run explore function
  result <- explore(test_data)
  
  # Check class
  expect_is(result, "statsaid_explore")
  
  # Check essential elements
  expect_equal(result$shape, c(4, 3))
  expect_equal(result$columns, c("a", "b", "c"))
  
  # Check variable types
  expect_equal(result$variable_types$numeric, "a")
  expect_equal(result$variable_types$categorical, "b")
  expect_equal(result$variable_types$logical, "c")
  
  # Check missing values
  expect_equal(result$missing$total[["a"]], 1)
  expect_equal(result$missing$total[["b"]], 0)
  expect_equal(result$missing$total[["c"]], 0)
})

test_that("suggest_preprocessing returns correct suggestions", {
  # Create test data
  test_data <- data.frame(
    a = c(1, 2, 3, NA, 5, 6, 7, 8, 9, 100),  # With outlier
    b = c("x", "y", "z", "x", "x", "y", "z", "x", "y", "z")
  )
  
  # Run suggest_preprocessing function
  result <- suggest_preprocessing(test_data)
  
  # Check class
  expect_is(result, "statsaid_suggestions")
  
  # Check if a's normalization suggestion is present (due to outlier)
  expect_true(!is.null(result$normalization[["a"]]))
  
  # Check if b's encoding suggestion is present
  expect_true(!is.null(result$encoding[["b"]]))
  expect_equal(result$encoding[["b"]], "One-hot encoding")
})

test_that("suggest_models returns appropriate models", {
  # Create test data
  test_data <- data.frame(
    a = rnorm(100),
    b = rnorm(100),
    c = factor(sample(c("yes", "no"), 100, replace = TRUE))
  )
  
  # Test without study design
  result1 <- suggest_models(test_data)
  expect_is(result1, "statsaid_model_suggestions")
  expect_true(length(result1$models) > 0)
  expect_true(length(result1$packages$r) > 0)
  
  # Test with study design
  result2 <- suggest_models(test_data, study_design = "case_control")
  expect_true("Logistic regression" %in% result2$models)
  expect_true("Fisher's exact test" %in% result2$models)
})