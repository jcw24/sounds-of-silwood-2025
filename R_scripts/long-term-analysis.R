# Long-term models
require(tidyverse)
library(vegan)
require(lme4)
require(lmerTest)

d2025 <- read.csv('precalls_2025.csv')
d2023 <- read.csv('precalls_2023.csv')
d2021 <- read.csv('precalls_2021_3&4.csv')

d2025$Site <- as.character(d2025$Site)
d2023$Site <- as.character(d2023$Site)

lump <- bind_rows(d2025, d2023, d2021)

# 1. Species Richness per Site
species_richness <- lump %>%
  group_by(Site, habitat) %>%
  summarise(
    species_richness = n_distinct(Common.Name),
    total_detections = n(),
    .groups = 'drop'
  )

# 2. Site-habitat data
site_habitat <- lump %>%
  select(Site, habitat) %>%
  distinct()

# Community Composition - Create species abundance matrix
species_abundance <- lump %>%
  group_by(Site, Common.Name) %>%
  summarise(presence = 1, .groups = 'drop') %>%  # Create presence indicator
  # Convert to wide format for community analysis
  tidyr::pivot_wider(
    names_from = Common.Name, 
    values_from = presence, 
    values_fill = 0  # Absent species get 0
  ) %>%
  # Join back with habitat information
  left_join(site_habitat, by = "Site")

# Extract just the species abundance matrix 
community_matrix <- species_abundance %>%
  select(-Site, -habitat) %>%
  as.matrix()

# Add site names as row names
rownames(community_matrix) <- species_abundance$Site

# 7. PCoA ordination for community composition
# This shows how similar/different the communities are between sites
bray_dist <- vegdist(community_matrix, method = "bray", binary = TRUE)
pcoa_result <- cmdscale(bray_dist, k = 2, eig = TRUE)

# Calculate percentage of variation explained by each axis
percent_var <- round(pcoa_result$eig[1:2] / sum(pcoa_result$eig) * 100, 1)

# Create PCoA dataframe with habitat information
pcoa_data <- data.frame(
  Site = rownames(community_matrix),
  PCoA1 = pcoa_result$points[,1],
  PCoA2 = pcoa_result$points[,2]
) %>%
  left_join(site_habitat, by = "Site")

print("\n=== PCoA DATA WITH HABITAT ===")
print(pcoa_data)

# Plot PCoA with habitat shapes and colors
pcoa_plot <- ggplot(pcoa_data, aes(x = PCoA1, y = PCoA2)) +
  geom_point(aes(shape = habitat, color = habitat), size = 3, alpha = 0.7) +
  #geom_text(aes(label = Site), hjust = 1.2, vjust = 1.2, size = 2.5, alpha = 0.7) +
  labs(
    x = paste0("PCoA1 (", percent_var[1], "%)"),
    y = paste0("PCoA2 (", percent_var[2], "%)"),
    shape = "Habitat",
    color = "Habitat"
  ) +
  scale_shape_manual(
    values = c("edge" = 15, "interior" = 16),  # 15 = square, 16 = circle
    labels = c("edge" = "Edge", "interior" = "Interior")
  ) +
  scale_color_manual(
    values = c("edge" = "grey60", "interior" = "black"),
    labels = c("edge" = "Edge", "interior" = "Interior")
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 13, face = "bold", family = "Calibri"),
    axis.text.y = element_text(size = 13, family = "Calibri"),
    axis.text.x = element_text(size = 13, family = "Calibri"),
    axis.title = element_text(size = 13, family = "Calibri"),
    legend.text = element_text(size = 13, family = "Calibri"),    # Optional: legend text
    legend.title = element_text(size = 13, family = "Calibri"))
print(pcoa_plot)

# Calculate sampling effort as number of distinct source files per site
sampling_effort <- lump %>%
  group_by(Site) %>%
  summarise(effort = n_distinct(Source_File), .groups = 'drop')  # distinct files per site

# Combine everything
model_data <- pcoa_data %>%
  left_join(sampling_effort, by = "Site")

everything <- species_richness %>%
  left_join(model_data, by ="Site") %>%
  select(-habitat.x) %>%
  rename(habitat = habitat.y)

# Species richness model

model_quasi <- glm(species_richness ~ habitat, 
                   family = quasipoisson, 
                   data = everything,
                   offset = log(sqrt(effort)))
summary(model_quasi)

model_results <- tidy(model_quasi, conf.int = FALSE) 
write.csv(model_results, "long-sprich-model.csv", row.names = FALSE)

# Sampling effort
sampling_effort <- lump %>%
  group_by(Site) %>%
  summarise(effort = n(), .groups = 'drop')  # total observations per site

# Combine everything
everything <- pcoa_data %>%
  left_join(sampling_effort, by = "Site")

# Linear models
lm_pcoa1 <- lm(PCoA1 ~ habitat + offset(log(effort)), data = everything)
lm_pcoa2 <- lm(PCoA2 ~ habitat + offset(log(effort)), data = everything)

summary(lm_pcoa1)
summary(lm_pcoa2)

# Model diagnostics
par(mfrow = c(2,2))
plot(model_pcoa1)
plot(model_pcoa2)

# Save model tables

model_results_1 <- tidy(lm_pcoa1, conf.int = FALSE) 
write.csv(model_results_1, "long-pcoa1-model.csv", row.names = FALSE)

model_results_2 <- tidy(lm_pcoa2, conf.int = FALSE) 
write.csv(model_results_2, "long-pcoa2-model.csv", row.names = FALSE)
