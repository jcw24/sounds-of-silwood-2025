# Calls per minute qualitative analysis

require(tidyverse)

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

lump <- all_years_pre %>%
  select(Common.Name, Site, Confidence, Source_File, Date, Time, habitat, year)

# Function to calculate calls per minute
calculate_calls_per_minute <- function(data) {
  
  # Step 1: Count total detections (calls) per species per year:site:habitat
  calls_summary <- data %>%
    group_by(Site, habitat, Common.Name) %>%
    summarise(
      total_calls = n(),
      .groups = 'drop'
    )
  
  # Step 2: Count unique source files (minutes) per year:site:habitat
  minutes_summary <- data %>%
    group_by(Site, habitat) %>%
    summarise(
      total_minutes = n_distinct(Source_File),
      .groups = 'drop'
    )
  
  # Step 3: Combine and calculate calls per minute
  calls_per_minute <- calls_summary %>%
    left_join(minutes_summary, by = c("Site", "habitat")) %>%
    mutate(
      calls_per_minute = total_calls / total_minutes,
      calls_per_minute = round(calls_per_minute, 3)
    ) %>%
    arrange(Site, habitat, Common.Name)
  
  return(calls_per_minute)
}

# Basic calculation
clump <- calculate_calls_per_minute(lump)

# First, get the top species overall (across all sites)
top_species_overall <- clump %>%
  group_by(Common.Name) %>%
  summarise(
    mean_calls_per_minute = mean(calls_per_minute),
    total_observations = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_calls_per_minute)) %>%
  slice_head(n = 5)  # Top 5 species overall

print("Top 5 species overall:")
print(top_species_overall)

# Get the species names
top_species_names <- top_species_overall$Common.Name

# Filter data to top species
data_top_species <- clump %>%
  filter(Common.Name %in% top_species_names)

# Prepare stacked data with only the top species
stacked_data <- data_top_species %>%
  group_by(Site, habitat, Common.Name) %>%
  summarise(
    mean_calls_per_minute = mean(calls_per_minute),
    .groups = 'drop'
  )

## Plots
p2 <- ggplot(stacked_data, aes(x = mean_calls_per_minute, y = Common.Name, fill = habitat)) +
  geom_col(alpha = 0.8, position = "stack") +
  labs(
    x = "Average Calls per Minute",
    y = "Species",
    fill = "Habitat"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("edge" = "grey", "interior" = "black"),
                    labels = c("edge" = "Edge", "interior" = "Interior")) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 9)
  )

print(p2)

ggplot(clump, aes(x = habitat, y = calls_per_minute)) +
  geom_violin() +
  theme_classic()

# Top 10 species

top_by_habitat <- clump %>%
  group_by(habitat, Common.Name) %>%
  summarise(
    mean_calls_per_minute = mean(calls_per_minute),
    total_observations = n(),
    .groups = 'drop'
  ) %>%
  group_by(habitat) %>%  # Group by both year AND habitat
  arrange(desc(mean_calls_per_minute)) %>%
  slice_head(n = 10) %>%  # Top 3 species per year-habitat combination
  ungroup()

# Habitat top
results_table2 <- top_by_habitat %>%
  group_by(habitat) %>%
  slice_head(n = 10) %>%
  mutate(
    rank = row_number(),
    calls_per_min = round(mean_calls_per_minute, 3)
  ) %>%
  select(habitat, rank, Common.Name, calls_per_min, total_observations) %>%
  pivot_wider(
    names_from = rank, 
    values_from = c(Common.Name, calls_per_min, total_observations),
    names_sep = "_"
  ) %>%
  arrange(habitat)

# Habitat summary
overall_habitat_stats <- clump %>%
  group_by(habitat) %>%
  summarise(
    n_observations = n(),
    mean_calls = mean(calls_per_minute),
    se_calls = sd(calls_per_minute) / sqrt(n()),
    median_calls = median(calls_per_minute),
    min_calls = min(calls_per_minute),
    max_calls = max(calls_per_minute),
    .groups = 'drop'
  )



