# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("modelr")
library("patchwork")
library("RColorBrewer")

# Define functions
# ------------------------------------------------------------------------------
source(file = "pca_cluster_analysis/R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
cancer_data <- read_csv(file = "pca_cluster_analysis/data/01_gravier_data.csv")


# Wrangle data
# ------------------------------------------------------------------------------

# Model data
# ------------------------------------------------------------------------------
# Do a PCA
cancer_pca <- cancer_data %>%
  select(-event_label) %>%
  prcomp(center = TRUE, scale = TRUE)

# Do a PCA with augment cancer_pca
cancer_aug_pca <- cancer_pca %>%
  broom::augment(cancer_data)

# Cluster genes
cluster_original <- cancer_aug_pca %>%
  select(contains("g")) %>%
  kmeans(centers = 2)

# Add clusters to PCA
cluster_original_aug <- 
  cluster_original %>%
  broom::augment (cancer_aug_pca) %>% 
  rename(cluster_org = .cluster)

# Do the clustering regarding only the first 2 PC
cluster_pca <- cancer_aug_pca %>%
  select(.fittedPC1, .fittedPC2) %>%
  kmeans(centers = 2)

complete_cluster <- 
  cluster_pca %>% 
  broom::augment (cluster_original_aug) %>% 
  rename(cluster_pca = .cluster)


complete_cluster %>%
  select(event_label, cluster_org, cluster_pca) %>%
  mutate(cluster_org = case_when(cluster_org == 1 ~ "good",
                                 cluster_org == 2 ~ "poor"),
         cluster_pca = case_when(cluster_pca == 1 ~ "good",
                                 cluster_pca == 2 ~ "poor"),
         cluster_org_correct = case_when(event_label == cluster_org ~ 1,
                                         event_label != cluster_org ~ 0),
         cluster_pca_correct = case_when(event_label == cluster_pca ~ 1,
                                         event_label != cluster_pca ~ 0)) %>% 
  summarise(score_org = mean(cluster_org_correct),
            score_pca = mean(cluster_pca_correct))

# Visualise data
# ------------------------------------------------------------------------------
# Variance explained Scree Plot
p1 <- cancer_pca %>%
  broom::tidy("pcs") %>%
  ggplot(aes(x = PC, y = percent, fill = percent, color = percent)) +
  geom_col() +
  scale_fill_distiller(palette = "Spectral") +
  scale_color_distiller(palette = "Spectral") +
  labs(x = "Principal Component", y = "Proportion of variance", title = "Scree Plot: variance explained") +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.line = element_line(colour = "black", size = 0),
    axis.text.x = element_text(face = "bold", color = "#000000"),
    axis.text.y = element_text(face = "bold", color = "#000000")
)

# PC1 vs PC2
p2 <- cancer_aug_pca %>%
  ggplot(aes(
    x = .fittedPC1,
    y = .fittedPC2,
    colour = event_label
  )) +
  geom_point() +
  scale_fill_brewer(palette = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "PC1", y = "PC2",fill = "Cancer events prognosis") +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.line = element_line(colour = "black", size = 0),
    axis.text.x = element_text(face = "bold", color = "#000000"),
    axis.text.y = element_text(face = "bold", color = "#000000")
  )

# Cluster on event label original
p3 <- complete_cluster %>%
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             colour = event_label)) +
  geom_point() +
  scale_fill_brewer(palette = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "PC1", y = "PC2",fill = "Cancer events prognosis") +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.line = element_line(colour = "black", size = 0),
    axis.text.x = element_text(face = "bold", color = "#000000"),
    axis.text.y = element_text(face = "bold", color = "#000000")
  )


# Cluster on original data
p4 <- complete_cluster %>%
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2,
             colour = cluster_org)) +
  geom_point() +
  scale_fill_brewer(palette = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "PC1", y = "PC2",fill = "Cancer events prognosis, cluster on original data") +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.line = element_line(colour = "black", size = 0),
    axis.text.x = element_text(face = "bold", color = "#000000"),
    axis.text.y = element_text(face = "bold", color = "#000000")
  )

# Cluster on PCA reduced data
p5 <- complete_cluster %>%
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             colour = cluster_pca)) +
  geom_point() +
  scale_fill_brewer(palette = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "PC1", y = "PC2",fill = "Cancer events prognosis, cluster on PCA data") +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.line = element_line(colour = "black", size = 0),
    axis.text.x = element_text(face = "bold", color = "#000000"),
    axis.text.y = element_text(face = "bold", color = "#000000")
  )

# Write data
# ------------------------------------------------------------------------------
ggsave("pca_cluster_analysis/results/screeplot_01.png", plot = p1)
ggsave("pca_cluster_analysis/results/PC1_PC2_01.png", plot = p2)
ggsave("pca_cluster_analysis/results/PC1_PC2_02_cluster.png", plot = p3)
ggsave("pca_cluster_analysis/results/PC1_PC2_03_cluster.png", plot = p4)
ggsave("pca_cluster_analysis/results/PC1_PC2_04_cluster.png", plot = p5)