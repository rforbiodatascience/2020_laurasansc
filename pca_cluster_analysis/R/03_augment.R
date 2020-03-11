# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")

# Define functions
# ------------------------------------------------------------------------------
source(file = "pca_cluster_analysis/R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
cancer_data <- read_csv(file = "pca_cluster_analysis/data/01_gravier_data.csv")

# Wrangle data
# ------------------------------------------------------------------------------
cancer_data$age_group <- cut(cancer_data$age, breaks = seq(10, 100, by = 10))

# Write data
# ------------------------------------------------------------------------------
write.csv(cancer_data,"pca_cluster_analysis/data/03_gravier_data_aug.csv", row.names=FALSE)