# Rarefying to Year:Site level
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

# Step 1: Calculate sampling effort per year:site combination
sampling_effort_yearsite <- all_years_pre %>%
  mutate(year = year(Date)) %>%
  group_by(year, Site) %>%
  summarise(
    total_minutes = n_distinct(Source_File),
    total_detections = n(),
    unique_species = n_distinct(Common.Name),
    .groups = 'drop'
  )

print("Sampling effort per year:site combination:")
print(sampling_effort_yearsite)

# Step 2: Find minimum sampling time across all year:site combinations
min_sampling_minutes_yearsite <- min(sampling_effort_yearsite$total_minutes)
cat("\nMinimum year:site sampling time to rarefy to:", min_sampling_minutes_yearsite, "minutes\n")

# Step 3: Function to rarefy one year:site combination to target minutes
rarefy_yearsite_to_minutes <- function(data, target_minutes) {
  # Get unique source files for this year:site combination
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

# Step 4: Single rarefaction example
single_rarefaction_yearsite <- all_years_pre %>%
  mutate(year = year(Date)) %>%
  group_by(year, Site) %>%
  group_modify(~rarefy_yearsite_to_minutes(.x, min_sampling_minutes_yearsite)) %>%
  ungroup()

# Calculate species richness from single rarefaction per year:site
single_richness_yearsite <- single_rarefaction_yearsite %>%
  group_by(year, Site) %>%
  summarise(
    species_richness = n_distinct(Common.Name),
    total_detections = n(),
    sampling_minutes = n_distinct(Source_File),
    .groups = 'drop'
  )

print("\nSingle rarefaction results by year:site:")
print(single_richness_yearsite)

# Step 5: Bootstrap rarefaction for confidence intervals (year:site level)
bootstrap_rarefaction_yearsite <- function(data, target_minutes, n_iterations = 1000) {
  
  # Function for one bootstrap iteration
  one_iteration <- function(iter) {
    rarefied <- data %>%
      mutate(year = year(Date)) %>%
      group_by(year, Site) %>%
      group_modify(~rarefy_yearsite_to_minutes(.x, target_minutes)) %>%
      ungroup()
    
    # Calculate richness for this iteration
    richness <- rarefied %>%
      group_by(year, Site) %>%
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
  cat("Running", n_iterations, "bootstrap iterations for year:site combinations...\n")
  results <- map_dfr(1:n_iterations, one_iteration)
  
  return(results)
}

# Run bootstrap rarefaction
bootstrap_results_yearsite <- bootstrap_rarefaction_yearsite(all_years_pre, min_sampling_minutes_yearsite, 1000)

# Step 6: Calculate summary statistics with confidence intervals
richness_summary_yearsite <- bootstrap_results_yearsite %>%
  group_by(year, Site) %>%
  summarise(
    mean_richness = mean(species_richness),
    se = sd(species_richness) / sqrt(n()),
    sd_richness = sd(species_richness),
    ci_lower = quantile(species_richness, 0.025),
    ci_upper = quantile(species_richness, 0.975),
    median_richness = median(species_richness),
    min_richness = min(species_richness),
    max_richness = max(species_richness),
    .groups = 'drop'
  )

richness_summary_yearhabitat <- bootstrap_results_yearsite %>%
  group_by(habitat_type) %>%
  summarise(
    mean_richness = mean(species_richness),
    se = sd(species_richness) / sqrt(n()),
    sd_richness = sd(species_richness),
    ci_lower = quantile(species_richness, 0.025),
    ci_upper = quantile(species_richness, 0.975),
    median_richness = median(species_richness),
    min_richness = min(species_richness),
    max_richness = max(species_richness),
    .groups = 'drop'
  )

print("\nBootstrap rarefaction summary by year:site (1000 iterations):")
print(richness_summary_yearsite)

# Adding site data
site_habitat <- data.frame(
  Site = unique(richness_summary_yearsite$Site)
) %>%
  mutate(habitat_type = case_when(
    Site %in% c("233", "213", "321", "113", "F") ~ "edge",
    Site %in% c("232", "121", "332", "133", "222") ~ "interior",
    TRUE ~ NA_character_
  ))

# FIGURE
bootstrap_results_yearsite <- bootstrap_results_yearsite %>%
  left_join(site_habitat, by = "Site")   # Remove sites without habitat classification

boxplot_data <- richness_summary_yearsite_habitat %>%
  rowwise() %>%
  do({
    site_year <- .
    # Create data points representing the distribution
    data.frame(
      Site = site_year$Site,
      year = site_year$year,
      habitat_type = site_year$habitat_type,
      min_richness = site_year$min_richness,
      median_richness = site_year$median_richness,
      max_richness = site_year$max_richness,
      mean_richness = site_year$mean_richness,
      ci_lower = site_year$ci_lower,
      ci_upper = site_year$ci_upper
    )
  }) %>%
  ungroup()

# Edge vs Interior boxplot across years
edge_interior_boxplot <- ggplot(richness_summary_yearsite_habitat, 
                                aes(x = factor(habitat_type), 
                                    y = median_richness,
                                    fill = habitat_type)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  geom_point(position = position_jitterdodge(dodge.width = 0.6, jitter.width = 0.2), 
             size = 1.8, alpha = 0.6) +
  
  # Colorblind-friendly palette
  scale_fill_manual(values = c("edge" = "lightgrey", "interior" = "black"),
                    name = "Habitat Type") +
  scale_x_discrete(labels = c("edge" = "Edge", "interior" = "Interior")) +
  labs(
    x = "Site",
    y = "Species Richness"
  ) +
  scale_fill_manual(values = c("edge" = "lightgrey", "interior" = "black"),
                    name = "Habitat Type",
                    labels = c("edge" = "Edge", "interior" = "Interior")) +
  theme_classic() +
  theme(
    legend.position = "right"
  )

print(edge_interior_boxplot)

print("\n=== RAREFACTION COMPLETE ===")
print(paste("Rarefied to minimum sampling effort of", min_sampling_minutes_yearsite, "minutes per year:site combination"))
print("Created matrices for community composition analysis:")
print("1. richness_matrix_year_x_site.csv - Years as rows, sites as columns, values = species richness")
print("2. species_occurrence_matrix.csv - Year_site combinations as rows, species as columns, values = presence/absence")
print("3. Year-specific occurrence matrices for within-year site comparisons")