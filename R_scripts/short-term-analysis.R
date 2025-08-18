# Short-term year:site models

require(tidyverse)
require(vegan)
require(lme4)
require(glmmTMB)
require(broom.mixed)

d2025 <- read.csv('precalls_2025.csv')
d2025$year <- 2025
d2023 <- read.csv('precalls_2023.csv')
d2023$year <- 2023
d2021 <- read.csv('precalls_2021_3&4.csv')
d2021$year <- 2021

d2025$Site <- as.character(d2025$Site)
d2023$Site <- as.character(d2023$Site)

all <- bind_rows(d2025, d2023, d2021)

# ============================================================================
# 1. DATA PREPARATION
# ============================================================================

# Prepare data with sampling effort as unique source files
all <- all %>%
  mutate(
    # Create year-Site combination (assuming you have a Site column, or using X as Site ID)
 # Change this to your actual Site column name if different
    year_Site = paste(year, Site, sep = "_"),
    # Extract species name (assuming it's in Common.Name or Species.Name)
    species = Common.Name # or Species.Name depending on your preference
  ) %>%
  filter(!is.na(species), !is.na(Source_File))

# Check the data structure
cat("Data dimensions:", dim(all), "\n")
cat("Unique species:", length(unique(all$species)), "\n")
cat("Unique year-Site combinations:", length(unique(all$year_Site)), "\n")
cat("Unique Sites:", length(unique(all$Site)), "\n")
cat("Years:", sort(unique(all$year)), "\n")
cat("habitats:", unique(all$habitat), "\n")

# ============================================================================
# 2. CREATE PRESENCE-ABSENCE MATRIX (year_Site x species)
# ============================================================================

# Create presence-absence matrix
pa_matrix <- all %>%
  # Get unique species per year-Site combination
  distinct(year_Site, species, .keep_all = TRUE) %>%
  # Create presence (1) indicator
  mutate(presence = 1) %>%
  # Spread to wide format
  select(year_Site, species, presence) %>%
  pivot_wider(names_from = species, values_from = presence, values_fill = 0) %>%
  column_to_rownames("year_Site")

cat("Presence-absence matrix dimensions:", dim(pa_matrix), "\n")

# Create Site metadata for analysis
Site_metadata <- all %>%
  group_by(year_Site) %>%
  summarise(
    year = first(year),
    Site = first(Site),
    habitat = first(habitat),
    # Sampling effort = number of unique source files (each = 1 minute)
    sampling_effort_minutes = n_distinct(Source_File),
    total_detections = n(),
    unique_species = n_distinct(species),
    .groups = "drop"
  ) %>%
  # Ensure row names match the matrix
  filter(year_Site %in% rownames(pa_matrix)) %>%
  arrange(match(year_Site, rownames(pa_matrix)))

# ============================================================================
# 3. SPECIES RICHNESS CALCULATION
# ============================================================================

# Calculate species richness per Site
Site_metadata$species_richness <- rowSums(pa_matrix)

# ============================================================================
# 4. PCOA ANALYSIS
# ============================================================================

# Calculate bray distance matrix
bray_dist <- vegdist(pa_matrix, method = "bray", binary = TRUE)

# Perform PCoA
pcoa_result <- cmdscale(bray_dist, k = min(nrow(pa_matrix)-1, 10), eig = TRUE)

# Extract PCoA axes
pcoa_axes <- as.data.frame(pcoa_result$points)
colnames(pcoa_axes) <- paste0("PCoA", 1:ncol(pcoa_axes))

# Calculate proportion of variance explained
eigenvals <- pcoa_result$eig[pcoa_result$eig > 0]
prop_var <- eigenvals / sum(eigenvals)

cat("PCoA - Proportion of variance explained by first 4 axes:\n")
print(round(prop_var[1:min(4, length(prop_var))], 3))

# Add PCoA axes to Site metadata
Site_data <- cbind(Site_metadata, pcoa_axes)

all_env_data <- read.csv('all_env_data.csv')

all_env_data$year <- as.integer(all_env_data$year)

Site_data <- Site_data %>%
  left_join(all_env_data, by = c('year', 'Site'))

# ============================================================================
# 5. VISUALIZE PCOA
# ============================================================================

# Plot PCoA
pcoa_plot <- ggplot(Site_data, aes(x = PCoA1, y = PCoA2)) +
  geom_point(aes(color = as.factor(year), shape = habitat), size = 3, alpha = 0.7) +
  #geom_text(aes(label = Site), hjust = 1.2, vjust = 1.2, size = 2.5, alpha = 0.7) +
  labs(
    x = paste0("PCoA1 (", round(prop_var[1] * 100, 1), "%)"),
    y = paste0("PCoA2 (", round(prop_var[2] * 100, 1), "%)"),
    color = "Year",
    shape = "Habitat"
  ) +
  scale_shape_manual(
    values = c("edge" = 15, "interior" = 16),  # 15 = square, 16 = circle
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

# ============================================================================
# 6. SPECIES RICHNESS GLMM WITH OFFSET
# ============================================================================

# Final model
model_pois <- glm(species_richness ~ factor(year) + habitat + canopy_cover, 
                  family = poisson,
                  offset = log(sampling_effort_minutes),
                  data = Site_data)
summary(model_pois)

# Extract model results
model_results <- tidy(model_pois, conf.int = FALSE) 
write.csv(model_results, "short-sprich-model.csv", row.names = FALSE)

# Model with (1|Site)
model_pois2 <- glmer(species_richness ~ factor(year) + habitat + canopy_cover + (1|Site), 
                    family = poisson,
                    offset = log(sampling_effort_minutes),
                    data = Site_data)

summary(model_pois)

# Check overdispersion
library(AER)
dispersiontest(model_pois)

plot(fitted(model_pois), residuals(model_pois))
abline(h = 0, lty = 2)

# Check residuals vs sampling effort (your original concern)
plot(log(Site_data$sampling_effort_minutes), residuals(model_pois))
abline(h = 0, lty = 2)
plot(log(Site_data$sampling_effort_minutes), residuals(s))
abline(h = 0, lty = 2)

library(DHARMa)

# Create DHARMa residuals for your Poisson model
simulationOutput <- simulateResiduals(fittedModel = model_pois, plot = FALSE)

# Overall residual plot with tests
plot(simulationOutput)

# Test for correct dispersion (overdispersion/underdispersion)
testDispersion(simulationOutput)

# Test for zero-inflation 
testZeroInflation(simulationOutput)

# Test for overall uniformity of residuals
testUniformity(simulationOutput)

# Test for outliers
testOutliers(simulationOutput)

plotResiduals(simulationOutput, Site_data$sampling_effort_minutes)
cor.test(simulationOutput$scaledResiduals, Site_data$sampling_effort_minutes)

# Plot species richness x canopy cover

ggplot(Site_data, aes(x = species_richness, y = canopy_cover)) +
  geom_point() +
  theme_classic() + 
  labs(x = 'Species Richness', y = 'Canopy Cover (%)') +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 13, face = "bold", family = "Calibri"),
    axis.text.y = element_text(size = 13, family = "Calibri"),
    axis.text.x = element_text(size = 13, family = "Calibri"),
    axis.title = element_text(size = 13, family = "Calibri"),
    legend.text = element_text(size = 13, family = "Calibri"),  
    legend.title = element_text(size = 13, family = "Calibri"))

# ============================================================================
# 7. COMMUNITY COMPOSITION GLMM USING PCOA AXES
# ============================================================================

# PCoA1
pcoa1_model <- lm(PCoA1 ~ factor(year) + habitat + canopy_cover, 
                        data = Site_data,
                        weights = sampling_effort_minutes)

summary(pcoa1_model)
plot(pcoa1_model)

model_sqrt <- lm(PCoA1_sqrt ~ factor(year) + habitat + canopy_cover, 
                 data = Site_data, weights = sampling_effort_minutes)
summary(model_sqrt)

# PCoA2
pcoa2_model <- lm(PCoA2 ~ factor(year) + habitat + canopy_cover, 
                        data = Site_data,
                        weights = sampling_effort_minutes)

summary(pcoa2_model)
plot(pcoa2_model)


model_results_2 <- tidy(pcoa2_model, conf.int = FALSE) 
write.csv(model_results_2, "short-pcoa2-model.csv", row.names = FALSE)

# ============================================================================
# 9. SUMMARY STATISTICS
# ============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")
cat("Total species detected:", ncol(pa_matrix), "\n")
cat("Total Sites surveyed:", nrow(pa_matrix), "\n")
cat("Mean species richness per Site:", round(mean(Site_data$species_richness), 2), "\n")
cat("Total minutes sampled per Site (range):", min(Site_data$sampling_effort_minutes), "-", max(Site_data$sampling_effort_minutes), "\n")
cat("Mean sampling effort per Site:", round(mean(Site_data$sampling_effort_minutes), 1), "minutes\n")

# Summary by habitat and year
summary_stats <- Site_data %>%
  group_by(year, habitat) %>%
  summarise(
    n_Sites = n_distinct(Site),
    mean_richness = round(mean(species_richness), 2),
    sd_richness = round(sd(species_richness), 2),
    mean_effort_minutes = round(mean(sampling_effort_minutes), 1),
    .groups = "drop"
  )

print(summary_stats)

# Save results
# write.csv(Site_data, "Site_analysis_results.csv", row.names = FALSE)
# write.csv(pa_matrix, "presence_absence_matrix.csv", row.names = TRUE)