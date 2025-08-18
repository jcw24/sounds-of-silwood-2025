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

all_years_pre <- all_years_pre %>%
  select(Common.Name, Site, Confidence, Source_File, Date, Time, habitat, year)

# Calculate calls per minute for each species at year:site level

# Function to calculate calls per minute
calculate_calls_per_minute <- function(data) {
  
  # Step 1: Count total detections (calls) per species per year:site:habitat
  calls_summary <- data %>%
    group_by(year, Site, habitat, Common.Name) %>%
    summarise(
      total_calls = n(),
      .groups = 'drop'
    )
  
  # Step 2: Count unique source files (minutes) per year:site:habitat
  minutes_summary <- data %>%
    group_by(year, Site, habitat) %>%
    summarise(
      total_minutes = n_distinct(Source_File),
      .groups = 'drop'
    )
  
  # Step 3: Combine and calculate calls per minute
  calls_per_minute <- calls_summary %>%
    left_join(minutes_summary, by = c("year", "Site", "habitat")) %>%
    mutate(
      calls_per_minute = total_calls / total_minutes,
      calls_per_minute = round(calls_per_minute, 3)
    ) %>%
    arrange(year, Site, habitat, Common.Name)
  
  return(calls_per_minute)
}

# Basic calculation
cpm <- calculate_calls_per_minute(all_years_pre)

top_species_cpm <- cpm %>%
  group_by(Common.Name) %>%
  summarise(
    mean_calls_per_minute = mean(calls_per_minute),
    total_observations = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_calls_per_minute)) %>%
  slice_head(n = 5) 

# Filter data to top species
top_species_cpm_names <- top_species_cpm$Common.Name
data_top_species_cpm <- cpm %>%
  filter(Common.Name %in% top_species_cpm_names)

# Calculate mean calls per minute for each year:habitat:species combination
stacked_data_cpm <- data_top_species_cpm %>%
  group_by(year, habitat, Common.Name) %>%
  summarise(
    mean_calls_per_minute = mean(calls_per_minute),
    .groups = 'drop'
  )

## Plots
p1 <- ggplot(stacked_data_cpm, aes(x = mean_calls_per_minute, y = Common.Name, fill = habitat)) +
  geom_col(alpha = 0.8, position = "stack") +
  facet_wrap(~Site, ncol = 3) +
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
    strip.text = element_text(size = 13, face = "bold", family = "Calibri"),
    axis.text.y = element_text(size = 13, family = "Calibri"),
    axis.text.x = element_text(size = 13, family = "Calibri"),
    axis.title = element_text(size = 13, family = "Calibri"),
    legend.text = element_text(size = 13, family = "Calibri"),  
    legend.title = element_text(size = 13, family = "Calibri"))

print(p1)

p3 <- ggplot(stacked_data_cpm, aes(x = mean_calls_per_minute, y = Common.Name, fill = habitat)) +
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
print(p3)

# Violin plot
ggplot(cpm, aes(x = habitat, y = calls_per_minute)) +
  geom_violin() +
  theme_classic()

# Habitat and year summary
top_by_year_habitat <- cpm %>%
  group_by(year, habitat, Common.Name) %>%
  summarise(
    mean_calls_per_minute = mean(calls_per_minute),
    total_observations = n(),
    .groups = 'drop'
  ) %>%
  group_by(year, habitat) %>%  # Group by both year AND habitat
  arrange(desc(mean_calls_per_minute)) %>%
  slice_head(n = 10) %>%  # Top 3 species per year-habitat combination
  ungroup()

# Create a table
results_table <- top_by_year_habitat %>%
  group_by(year, habitat) %>%
  slice_head(n = 10) %>%
  mutate(
    rank = row_number(),
    calls_per_min = round(mean_calls_per_minute, 3)
  ) %>%
  select(year, habitat, rank, Common.Name, calls_per_min, total_observations) %>%
  pivot_wider(
    names_from = rank, 
    values_from = c(Common.Name, calls_per_min, total_observations),
    names_sep = "_"
  ) %>%
  arrange(year, habitat)


write.csv(results_table, "Table_S1_top_species_by_year_habitat.csv", row.names = FALSE)


