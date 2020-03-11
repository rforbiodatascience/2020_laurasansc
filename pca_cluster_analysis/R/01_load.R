# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("datamicroarray")

# Define functions
# ------------------------------------------------------------------------------
source(file = "pca_cluster_analysis/R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
data("gravier", package = "datamicroarray")

# Wrangle data
# ------------------------------------------------------------------------------
set.seed(676571)
cancer_data <- mutate(as_tibble(pluck(gravier, "x")), 
                      y = pluck(gravier, "y"), 
                      pt_id = 1:length(pluck(gravier, "y")), 
                      age = round(rnorm(length(pluck(gravier, "y")), mean = 55, sd = 10), 1))
cancer_data <- rename(cancer_data, event_label = y)

# Write data
# ------------------------------------------------------------------------------
write.csv(cancer_data, "pca_cluster_analysis/data/01_gravier_data.csv", row.names=FALSE)
