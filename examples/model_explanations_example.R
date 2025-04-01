# Example: Getting detailed model explanations in StatsAidR
library(StatsAidR)

# Create example data
set.seed(123)
data <- data.frame(
  # Subject ID (for clustering)
  subject_id = rep(1:30, each = 3),
  
  # Time points
  time = rep(1:3, times = 30),
  
  # Treatment group
  treatment = rep(c("Control", "Treatment"), each = 45),
  
  # Continuous outcome variable with treatment effect and subject variability
  outcome = rnorm(90) + 
    rep(c(0, 0.8), each = 45) +  # Treatment effect
    rep(rnorm(30, sd = 0.5), each = 3)  # Subject random effect
)

# Additional characteristics to demographic data
data$age <- round(rnorm(90, mean = 45, sd = 10))
data$sex <- sample(c("Male", "Female"), 90, replace = TRUE)

# Add missing values to make it realistic
missing_indices <- sample(1:90, 10)
data$outcome[missing_indices] <- NA

#-------------------------------------------------------
# Option 1: Use suggest_models() with detailed=TRUE
#-------------------------------------------------------
detailed_suggestions <- suggest_models(
  data = data,
  outcome_var = "outcome",
  design = "longitudinal",
  detailed = TRUE  # This is the key parameter!
)

# Print the detailed suggestions
print(detailed_suggestions)

#-------------------------------------------------------
# Option 2: Use the specialized explain_model_suggestions() function
#-------------------------------------------------------
model_explanations <- explain_model_suggestions(
  data = data,
  outcome_var = "outcome",
  design = "longitudinal"
)

# Print the comprehensive explanations
print(model_explanations)

# The explain_model_suggestions() function provides more comprehensive information:
# - Detailed rationale for why the model is appropriate for your data
# - Key advantages of using this model
# - Limitations and assumptions to be aware of
# - Best practices for implementing the model
# - Common pitfalls to avoid
# - Detailed implementation code with comments
# - References to statistical literature