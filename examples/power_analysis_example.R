# StatsAid Power Analysis Example

# Create directory if it doesn't exist
if (!dir.exists("examples")) {
  dir.create("examples")
}

# Load the necessary libraries
# In a real application, you would use: library(StatsAid)
# But for this example, we'll source the R files directly
source("R/power_analysis.R")

#' Example 1: Calculate sample size for a t-test
cat("\n\nExample 1: T-test sample size calculation\n")
cat("=====================================\n")

# Calculate sample size for a two-sample t-test with medium effect size
t_test_sample <- calculate_t_test_sample_size(d = 0.5)
print(t_test_sample)

# Calculate sample size for a paired t-test with large effect size
paired_test_sample <- calculate_t_test_sample_size(d = 0.8, type = "paired")
print(paired_test_sample)

# Calculate sample size for unequal group allocation (2:1 ratio)
unequal_groups_sample <- calculate_t_test_sample_size(d = 0.5, allocation_ratio = 2)
print(unequal_groups_sample)

#' Example 2: Calculate sample size for ANOVA
cat("\n\nExample 2: ANOVA sample size calculation\n")
cat("====================================\n")

# Calculate sample size for an ANOVA with 3 groups and medium effect size
anova_sample <- calculate_anova_sample_size(f = 0.25, groups = 3)
print(anova_sample)

# Calculate sample size for an ANOVA with 4 groups and small effect size
anova_small_effect <- calculate_anova_sample_size(f = 0.1, groups = 4)
print(anova_small_effect)

#' Example 3: Calculate sample size for correlation
cat("\n\nExample 3: Correlation sample size calculation\n")
cat("==========================================\n")

# Calculate sample size for detecting a correlation of 0.3 (medium effect)
correlation_sample <- calculate_correlation_sample_size(r = 0.3)
print(correlation_sample)

# Calculate sample size for detecting a correlation of 0.1 (small effect)
correlation_small_effect <- calculate_correlation_sample_size(r = 0.1)
print(correlation_small_effect)

#' Example 4: Calculate sample size for chi-square test
cat("\n\nExample 4: Chi-square sample size calculation\n")
cat("=========================================\n")

# Calculate sample size for a chi-square test with df=2 and medium effect size
chi_square_sample <- calculate_chi_square_sample_size(w = 0.3, df = 2)
print(chi_square_sample)

# Calculate sample size for a 3x3 contingency table with small effect size
# df = (rows-1) * (cols-1) = 2 * 2 = 4
chi_square_table <- calculate_chi_square_sample_size(w = 0.1, df = 4)
print(chi_square_table)

#' Example 5: Calculate sample size for logistic regression
cat("\n\nExample 5: Logistic regression sample size calculation\n")
cat("=================================================\n")

# Calculate sample size for logistic regression
# Baseline probability of outcome is 0.2, expected to increase to 0.35 with predictor
logistic_sample <- calculate_logistic_regression_sample_size(p0 = 0.2, p1 = 0.35)
print(logistic_sample)

# Calculate sample size for logistic regression with categorical predictor
# and adjustment for correlated predictors
logistic_complex <- calculate_logistic_regression_sample_size(
  p0 = 0.2,
  p1 = 0.35,
  r2_other = 0.3,
  distribution = "binomial",
  prop_1 = 0.4
)
print(logistic_complex)

#' Example 6: Calculate sample size for multiple regression
cat("\n\nExample 6: Multiple regression sample size calculation\n")
cat("=================================================\n")

# Calculate sample size for multiple regression with 5 predictors and medium effect size
regression_sample <- calculate_regression_sample_size(f2 = 0.15, predictors = 5)
print(regression_sample)

# Calculate sample size for testing a subset of predictors
# Testing 2 predictors in a model with 5 total predictors
regression_subset <- calculate_regression_sample_size(f2 = 0.15, predictors = 5, test_predictors = 2)
print(regression_subset)

#' Example 7: Create power curves
cat("\n\nExample 7: Power curves\n")
cat("===================\n")

# Create and save example plots
if (!dir.exists("examples/output")) {
  dir.create("examples/output", recursive = TRUE)
}

# Create power curve varying sample size for a t-test with medium effect size
ttest_curve <- plot_power_curve(
  test = "t", 
  vary = "n", 
  effect_size = 0.5, 
  range = seq(10, 200, by = 10)
)
ggsave("examples/output/ttest_power_curve.png", ttest_curve, width = 7, height = 5)

# Create power curve varying effect size for regression with fixed sample size
regression_curve <- plot_power_curve(
  test = "regression", 
  vary = "effect", 
  n = 100, 
  range = seq(0.01, 0.4, by = 0.01), 
  predictors = 5
)
ggsave("examples/output/regression_power_curve.png", regression_curve, width = 7, height = 5)

cat("Power curve plots saved in examples/output/\n")

#' Example 8: Estimate effect size from data
cat("\n\nExample 8: Effect size estimation\n")
cat("=============================\n")

# Generate example data
set.seed(123)
example_data <- data.frame(
  group = factor(rep(c("A", "B", "C"), each = 30)),
  score = c(rnorm(30, 10, 3), rnorm(30, 12, 3), rnorm(30, 15, 3)),
  x = rnorm(90),
  y = rnorm(90)
)

# Save example data
saveRDS(example_data, "examples/effect_size_data.rds")

# Estimate effect size for t-test (comparing first two groups)
ttest_data <- example_data[example_data$group %in% c("A", "B"), ]
effect_t <- estimate_effect_size(ttest_data, test_type = "t.test", vars = "score", group_var = "group")
print(effect_t)

# Estimate effect size for ANOVA
effect_anova <- estimate_effect_size(example_data, test_type = "anova", vars = "score", group_var = "group")
print(effect_anova)

# Estimate effect size for correlation
effect_cor <- estimate_effect_size(example_data, test_type = "correlation", vars = c("x", "y"))
print(effect_cor)

#' Example 9: Study design recommendations
cat("\n\nExample 9: Study design recommendations\n")
cat("===================================\n")

# Get recommendations for an experimental study with continuous outcome
design_rct <- recommend_study_design(
  outcome_type = "continuous",
  predictor_type = "categorical",
  setting = "experimental",
  expected_effect = "medium"
)
print(design_rct)

# Get recommendations for an observational study with binary outcome
design_obs <- recommend_study_design(
  outcome_type = "binary",
  predictor_type = "mixed",
  setting = "observational",
  constraints = list(budget = "limited", time = "limited"),
  expected_effect = "small"
)
print(design_obs)

cat("\nPower analysis examples completed.\n")