# StatsAid Model Suggestions Example

# Create directory if it doesn't exist
if (!dir.exists("examples")) {
  dir.create("examples")
}

# Load the necessary libraries
# In a real application, you would use: library(StatsAid)
# But for this example, we'll source the R files directly
source("R/explore.R")
source("R/visualize.R")
source("R/data_analysis.R")
source("R/model_suggestion.R")
source("R/main.R")

# Generate example dataset with clustering
set.seed(123)
n_subjects <- 30
measures_per_subject <- 3
n_total <- n_subjects * measures_per_subject

# Create sample data with various data types and some missing values
sample_data <- data.frame(
  subject_id = rep(1:n_subjects, each = measures_per_subject),
  visit = rep(1:measures_per_subject, times = n_subjects),
  treatment = factor(rep(c("A", "B"), each = n_total/2)),
  age = rep(round(runif(n_subjects, 20, 80)), each = measures_per_subject),
  sex = factor(rep(c("M", "F"), ceiling(n_subjects/2))[1:n_subjects], 
               each = measures_per_subject),
  weight = rep(rnorm(n_subjects, 70, 10), each = measures_per_subject) + 
    rnorm(n_total, 0, 2),  # Add some within-subject variability
  height = rep(rnorm(n_subjects, 170, 10), each = measures_per_subject),
  biomarker1 = rep(rnorm(n_subjects), each = measures_per_subject) + 
    rnorm(n_total, 0, 0.5) + 
    rep(rep(c(0, 0.5), each = n_subjects/2), each = measures_per_subject),  # Treatment effect
  biomarker2 = rpois(n_total, lambda = 3),  # Count data
  response = factor(sample(c("Yes", "No"), n_total, 
                           replace = TRUE, prob = c(0.3, 0.7)))  # Binary outcome
)

# Introduce some missing values
set.seed(456)
sample_data$weight[sample(1:n_total, 5)] <- NA
sample_data$biomarker1[sample(1:n_total, 8)] <- NA
sample_data$biomarker2[sample(1:n_total, 10)] <- NA

# Save the dataset
saveRDS(sample_data, "examples/sample_data.rds")

# Example 1: Basic model suggestions
cat("\n\nExample 1: Basic model suggestions\n")
cat("====================================\n")
basic_suggestions <- suggest_models(
  data = sample_data,
  study_design = "longitudinal"
)
print(basic_suggestions)

# Example 2: Detailed model suggestions with outcome specification
cat("\n\nExample 2: Detailed model suggestions with outcome specification\n")
cat("==============================================================\n")
detailed_suggestions <- suggest_models(
  data = sample_data,
  study_design = "longitudinal",
  outcome_var = "biomarker1",
  detailed = TRUE
)
print(detailed_suggestions)

# Example 3: Enhanced model suggestions with data structure analysis
cat("\n\nExample 3: Full data structure analysis\n")
cat("=======================================\n")
data_analysis <- analyze_data_structure(
  data = sample_data,
  outcome_var = "biomarker1"
)
print(data_analysis)

# Example 4: Study design specification
cat("\n\nExample 4: Using the study design specification\n")
cat("==============================================\n")
design <- study_design_spec(
  design_type = "longitudinal",
  response_type = "continuous",
  clustering = "subject_id",
  repeated_measures = TRUE
)
print(design)

model_suggestions <- suggest_models(
  data = sample_data,
  design = design,
  outcome_var = "biomarker1",
  detailed = TRUE
)
print(model_suggestions)

# Example 5: Interactive model selection (uncomment to run interactively)
cat("\n\nExample 5: Interactive model selection\n")
cat("======================================\n")
cat("To use the interactive guide, run:\n")
cat("guide_model_selection(sample_data)\n")
# Uncomment the next line to run interactively
# guide_model_selection(sample_data)