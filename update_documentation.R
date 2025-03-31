#!/usr/bin/env Rscript
# Script to update documentation and NAMESPACE

# Install packages if needed
if (!requireNamespace("roxygen2", quietly = TRUE)) {
  install.packages("roxygen2")
}

if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Set working directory to package root
# Uncomment and adjust if needed
# setwd("/path/to/StatsAid-R")

# Run roxygen2 documentation generator
roxygen2::roxygenise()

# Run R CMD check (optional)
# devtools::check()

cat("Documentation updated.\n")
cat("NAMESPACE updated.\n")
cat("Run R CMD INSTALL . to install the package locally.\n")