# Site-Level Rarefaction Analysis (All Years Combined)
require(tidyverse)
require(vegan)

# Load data
d2025 <- read.csv('precalls_2025.csv')
d2025$year <- 2025
d2023 <- read.csv('precalls_2023.csv')
d2023$year <- 2023
d2021 <- read.csv('precalls_2021_3&4.csv')
d2021$year <- 2021

d2025$Site <- as.character(d2025$Site)
d2023$Site <- as.character(d2023$Site)

all_years_pre <- bind_rows(d2025, d2023, d2021)
all_years_pre <- all_years_pre %>%
  select(Common.Name, Site, Confidence, Source_File, Date, Time, habitat, year)

print("=== SITE-LEVEL RAREFACTION ANALYSIS ===")
print("Pooling all years together for each site")

# Step 1: Calculate total sampling effort per site (across all years)
sampling_effort_site <- all_years_pre %>%
  group_by(Site) %>%
  summarise(
    total_minutes = n_distinct(Source_File),
    total_detections = n(),
    unique_species = n_distinct(Common.Name),
    years_sampled = n_distinct(year(Date)),
    date_range = paste(min(Date), "to", max(Date)),
    .groups = 'drop'
  )

print("Total sampling effort per site (all years combined):")
print(sampling_effort_site)

# Step 2: Find minimum sampling time across all sites
min_sampling_minutes <- min(sampling_effort_site$total_minutes)
cat("\nMinimum site sampling time to rarefy to:", min_sampling_minutes, "minutes\n")

# Step 3: Site habitat classification
site_habitat <- data.frame(
  Site = unique(all_years_pre$Site)
) %>%
  mutate(habitat_type = case_when(
    Site %in% c("233", "213", "321", "113", "F") ~ "edge",
    Site %in% c("232", "121", "332", "133", "222") ~ "interior",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(habitat_type))

print("\nSite habitat classification:")
print(site_habitat)

# Filter to only include sites with habitat classification
all_years_pre_filtered <- all_years_pre %>%
  filter(Site %in% site_habitat$Site)

# Step 4: Function to rarefy one site to target minutes (across all years)
rarefy_site_to_minutes <- function(data, target_minutes) {
  # Get unique source files for this site across all years
  unique_files <- unique(data$Source_File)
  
  # Randomly sample the target number of files
  if (length(unique_files) <= target_minutes) {
    selected_files <- unique_files
  } else {
    selected_files <- sample(unique_files, target_minutes, replace = FALSE)
  }
  
  # Filter data to only include detections from selected files
  rarefied_data <- data %>%
    filter(Source_File %in% selected_files)
  
  return(rarefied_data)
}

# Step 5: Single rarefaction example
single_rarefaction_site <- all_years_pre_filtered %>%
  group_by(Site) %>%
  group_modify(~rarefy_site_to_minutes(.x, min_sampling_minutes)) %>%
  ungroup()

# Calculate species richness from single rarefaction per site
single_richness_site <- single_rarefaction_site %>%
  group_by(Site) %>%
  summarise(
    species_richness = n_distinct(Common.Name),
    total_detections = n(),
    sampling_minutes = n_distinct(Source_File),
    years_represented = n_distinct(year(Date)),
    species_list = paste(sort(unique(Common.Name)), collapse = "; "),
    .groups = 'drop'
  )

print("\nSingle rarefaction results by site:")
print(single_richness_site)

# Step 6: Bootstrap rarefaction for confidence intervals
bootstrap_rarefaction_site <- function(data, target_minutes, n_iterations = 1000) {
  
  # Function for one bootstrap iteration
  one_iteration <- function(iter) {
    rarefied <- data %>%
      group_by(Site) %>%
      group_modify(~rarefy_site_to_minutes(.x, target_minutes)) %>%
      ungroup()
    
    # Calculate richness for this iteration
    richness <- rarefied %>%
      group_by(Site) %>%
      summarise(
        species_richness = n_distinct(Common.Name),
        total_detections = n(),
        sampling_minutes = n_distinct(Source_File),
        iteration = iter,
        .groups = 'drop'
      )
    
    return(richness)
  }
  
  # Run bootstrap iterations
  cat("Running", n_iterations, "bootstrap iterations for site-level analysis...\n")
  results <- map_dfr(1:n_iterations, one_iteration)
  
  return(results)
}

# Run bootstrap rarefaction
bootstrap_results_site <- bootstrap_rarefaction_site(all_years_pre_filtered, min_sampling_minutes, 1000)

# Step 7: Calculate summary statistics with confidence intervals
richness_summary_site <- bootstrap_results_site %>%
  group_by(Site) %>%
  summarise(
    n_sites = n(),
    mean_richness = mean(species_richness),
    median_richness = median(species_richness),
    sd_richness = sd(species_richness),
    se_median_richness = sd(species_richness) / sqrt(n()),
    ci_lower = quantile(species_richness, 0.025),
    ci_upper = quantile(species_richness, 0.975),
    median_richness = median(species_richness),
    min_richness = min(species_richness),
    max_richness = max(species_richness),
    .groups = 'drop'
  )

site_habitat <- data.frame(
  Site = unique(all_years_pre$Site)
) %>%
  mutate(habitat_type = case_when(
    Site %in% c("233", "213", "321", "113", "F") ~ "edge",
    Site %in% c("232", "121", "332", "133", "222") ~ "interior",
    TRUE ~ NA_character_
  ))

# FIGURE
bootstrap_results_site <- bootstrap_results_site %>%
  left_join(site_habitat, by = "Site")  

habitat_summary_site <- bootstrap_results_site %>%
  group_by(habitat_type.x) %>%
  summarise(
    n_sites = n(),
    mean_richness = mean(species_richness),
    median_richness = median(species_richness),
    sd_richness = sd(species_richness),
    se_richness = sd(species_richness) / sqrt(n()),
    ci_lower = quantile(species_richness, 0.025),
    ci_upper = quantile(species_richness, 0.975),
    median_richness = median(species_richness),
    min_richness = min(species_richness),
    max_richness = max(species_richness),
    .groups = 'drop'
  )

# Add habitat classification
richness_summary_site_habitat <- richness_summary_site %>%
  left_join(site_habitat, by = "Site")

print("\nBootstrap rarefaction summary by site (1000 iterations):")
print(richness_summary_site_habitat)

# Step 9: Plotting
# Create boxplot data
boxplot_data_site <- richness_summary_site_habitat %>%
  select(Site, habitat_type, min_richness, median_richness, max_richness, 
         mean_richness, ci_lower, ci_upper)

# Step 10: Summary statistics
habitat_summary <- richness_summary_site_habitat %>%
  group_by(habitat_type) %>%
  summarise(
    n_sites = n(),
    mean_median_richness = mean(median_richness),
    se_median_richness = sd(median_richness) / sqrt(n()),
    min_median_richness = min(median_richness),
    max_median_richness = max(median_richness),
    .groups = 'drop'
  )

print("\nSummary statistics by habitat type:")
print(habitat_summary)

si_summary <- richness_summary_site_habitat %>%
  group_by(Site) %>%
  summarise(
    n_sites = n(),
    mean_median_richness = mean(median_richness),
    se_median_richness = sd(median_richness) / sqrt(n()),
    min_median_richness = min(median_richness),
    max_median_richness = max(median_richness),
    .groups = 'drop'
  )

print("\nSummary statistics by site type:")
print(si_summary)
print("\n=== SITE-LEVEL RAREFACTION ANALYSIS COMPLETE ===")
print(paste("Rarefied to minimum sampling effort of", min_sampling_minutes, "minutes per site"))
print(paste("Analysis includes", nrow(site_habitat), "sites with habitat classification"))
print("All years have been pooled together for each site")
print("Files saved:")
print("- single_rarefaction_site.csv (rarefied detection data)")
print("- single_richness_site.csv (single rarefaction richness per site)")  
print("- richness_summary_site.csv (bootstrap summary statistics)")
print("- species_occurrence_matrix_site.csv (sites x species matrix)")
print("- bootstrap_results_site.csv (all bootstrap iterations)")
print("- habitat_summary_site.csv (summary by habitat type)")

# Step 9: Plotting
bootstrap_plot_data <- bootstrap_results_site %>%
  left_join(site_habitat, by = "Site")

# Plot 1: Individual sites with bootstrap distribution
site_boxplot <- ggplot(bootstrap_plot_data, 
                       aes(x = factor(Site), 
                           y = species_richness,
                           fill = habitat_type)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  
  scale_fill_manual(values = c("edge" = "lightgrey", "interior" = "black"),
                    name = "Habitat Type",
                    labels = c("edge" = "Edge", "interior" = "Interior")) +
  
  labs(
    x = "Site",
    y = "Species Richness"
  ) +
  theme_classic() +
  theme(
    legend.position = "right"
  )

print(site_boxplot)

# Plot 2: Edge vs Interior comparison
edge_interior_boxplot <- ggplot(bootstrap_plot_data, 
                                aes(x = habitat_type, 
                                    y = species_richness,
                                    fill = habitat_type)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  scale_x_discrete(labels = c("edge" = "Edge", "interior" = "Interior")) +
  scale_fill_manual(values = c("edge" = "lightgrey", "interior" = "black"),
                    name = "Habitat Type",
                    labels = c("edge" = "Edge", "interior" = "Interior")) +
  labs(
    x = "Habitat Type",
    y = "Species Richness"
  ) +
  theme_classic()

print(edge_interior_boxplot)

