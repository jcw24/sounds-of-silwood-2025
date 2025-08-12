# 2025 data processing

require(tidyverse)
require(vegan)
require(indicspecies)

# ============================================================================
# 1. LOAD BIRDNET TEXT FILES
# ============================================================================

before <- read.delim("before_all.txt", header = TRUE)
second <- read.delim("second_all.txt", header = TRUE)

sites_second <- subset(second, Site %in% c(233, 321, 121, 133, 332))

sites_before <- subset(before, Site %in% c(213, 113, 232))

twentyfive <- rbind(sites_second, sites_before)

unique(twentyfive$Date)

# ============================================================================
# 2. FILTER DATA FOR SITES AND TIMES
# ============================================================================

# filter for sites, dawn an dusk and add habitat classifications
tfive_site <- twentyfive %>%
  dplyr::filter(Site %in% c('113', '121', '133', '213', '232', '233', '321', '332')) %>% # 8 2025 sites
  dplyr::filter(
    (Time >= "05:30:00" & Time <= "09:00:00") | 
      (Time >= "17:00:00" & Time <= "20:30:00")) %>% # dawn and dusk times 
  mutate(habitat = case_when(
    Site %in% c("233", "213", "113", "321") ~ "edge",
    Site %in% c("232", "121", "133", "332") ~ "interior",
    TRUE ~ NA_character_))

# Filter to one detection per species per minute per source file
# Keep the detection with highest confidence within each minute
tfive_filter <- tfive_site %>%
  # Create minute grouping variable
  mutate(
    # Extract hour and minute from Time column
    Time_minute = floor_date(as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"), "minute")
    # Alternative if Time is already in HH:MM:SS format:
    # Time_minute = substr(Time, 1, 5)  # Takes HH:MM part only
  ) %>%
  # Group by source file, species, date, and minute
  group_by(Source_File, Common.Name, Date, Time_minute) %>%
  # Keep ONLY the detection with highest confidence score
  # If confidence scores are tied, keeps the first occurrence
  slice_max(order_by = Confidence, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # Remove the helper column
  select(-Time_minute) %>%
  filter(Common.Name != 'nocall')

sort(table(tfive_filter$Common.Name), decreasing = TRUE)

# ============================================================================
# 3. CREATE BIRDNET VALIDATION DATASET
# ============================================================================

# checking <20 species
tfive_under20 <- tfive_filter %>%
  add_count(Common.Name, name = "detections") %>%
  filter(detections < 20)

write.csv(tfive_under20, "2025_under20_val.csv", row.names = FALSE)

unique(tfive_under20)

length(unique(tfive_species$Common.Name))
sort(table(tfive_species$Common.Name), decreasing = TRUE)

### Stratified sampling based on confidence scores for species validation
set.seed(123)  

# First, calculate total detections per species
species_totals <- tfive_filter %>%
  group_by(Common.Name) %>%
  summarise(Total_Detections = n(), .groups = 'drop')

# Stratified sampling function
stratified_sample_by_confidence <- function(species_data) {
  # Define confidence strata using quantiles
  conf_quantiles <- quantile(species_data$Confidence, probs = c(0, 1/3, 2/3, 1))
  
  # Assign strata
  species_stratified <- species_data %>%
    mutate(
      Stratum = case_when(
        Confidence >= conf_quantiles[1] & Confidence < conf_quantiles[2] ~ "Low",
        Confidence >= conf_quantiles[2] & Confidence < conf_quantiles[3] ~ "Middle", 
        Confidence >= conf_quantiles[3] & Confidence <= conf_quantiles[4] ~ "High",
        TRUE ~ "High"  
      )
    )
  
  # Sample from each stratum: 4 low, 6 middle, 10 high
  sampled_data <- species_stratified %>%
    group_by(Stratum) %>%
    do({
      stratum_name <- unique(.$Stratum)
      target_n <- case_when(
        stratum_name == "Low" ~ 4,
        stratum_name == "Middle" ~ 6,
        stratum_name == "High" ~ 10
      )
      # If stratum has fewer observations than target, take all available
      actual_n <- min(nrow(.), target_n)
      slice_sample(., n = actual_n)
    }) %>%
    ungroup()
  
  return(sampled_data)
}

# Apply stratified sampling to each species
verification_sample <- tfive_filter %>%
  group_by(Common.Name) %>%
  do(stratified_sample_by_confidence(.)) %>%
  ungroup() %>%
  # Join with species totals to add total detection count
  left_join(species_totals, by = "Common.Name") %>%
  # Select the required columns including Confidence and Total_Detections
  select(Source_File, Site, Date, Time, Begin.Time..s., End.Time..s., Common.Name, Confidence, Total_Detections, Stratum) %>%
  # Sort by species for easier manual verification
  arrange(Site, Date, Time)

# Display summary of sampling
cat("Summary of stratified sampling by confidence score:\n")
verification_summary <- verification_sample %>%
  group_by(Common.Name) %>%
  summarise(
    Sampled_Files = n(),
    Total_Detections = first(Total_Detections),
    Sampling_Rate = paste0(round((n()/first(Total_Detections))*100, 1), "%"),
    High_Stratum = sum(Stratum == "High", na.rm = TRUE),
    Middle_Stratum = sum(Stratum == "Middle", na.rm = TRUE),
    Low_Stratum = sum(Stratum == "Low", na.rm = TRUE),
    Min_Confidence = round(min(Confidence, na.rm = TRUE), 3),
    Max_Confidence = round(max(Confidence, na.rm = TRUE), 3),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Detections))

print(verification_summary)

# Display overall stratum distribution
cat("\nOverall stratum distribution:\n")
stratum_summary <- verification_sample %>%
  group_by(Stratum) %>%
  summarise(
    Count = n(),
    Mean_Confidence = round(mean(Confidence), 3),
    Min_Confidence = round(min(Confidence), 3),
    Max_Confidence = round(max(Confidence), 3),
    .groups = 'drop'
  )
print(stratum_summary)

# Show confidence score distribution
cat("\nConfidence score distribution in sample:\n")
print(summary(verification_sample$Confidence))

# Export to CSV
#output_filename <- paste0("all2025_strata_samples_", Sys.Date(), ".csv")
#write.csv(verification_sample, output_filename, row.names = FALSE)

cat(paste("\nStratified sample exported to:", output_filename))
cat(paste("\nTotal files selected:", nrow(verification_sample)))
cat(paste("\nTotal species sampled:", length(unique(verification_sample$Common.Name))))

# ============================================================================
# 4. CALCULATE SPECIES-SPECIFIC THRESHOLDS
# ============================================================================
# Function to find minimum confidence threshold for each species to achieve 90% precision
find_species_thresholds <- function(df, target_precision = 0.90) {
  
  # Remove nocall and treat U as FP
  df_clean <- df %>%
    filter(Common.Name != "nocall") %>%
    mutate(Status = ifelse(Status == "U", "FP", Status)) %>%
    filter(Status %in% c("TP", "FP"))
  
  # Get unique species
  species_list <- unique(df_clean$Common.Name)
  thresholds <- list()
  removed_species <- data.frame(
    Species = character(),
    Total_Samples = numeric(),
    TP_Count = numeric(),
    FP_Count = numeric(),
    Max_Precision_Achieved = numeric(),
    Reason = character(),
    stringsAsFactors = FALSE
  )
  
  cat("Finding minimum thresholds for", length(species_list), "species:\n\n")
  
  for (species in species_list) {
    
    species_data <- df_clean %>% filter(Common.Name == species)
    
    # Test thresholds from 0.80 to 0.99
    threshold_range <- seq(0.80, 0.99, by = 0.01)
    max_precision <- 0
    
    for (threshold in threshold_range) {
      
      # Filter detections above threshold
      above_threshold <- species_data %>% filter(Confidence.x >= threshold)
      
      if (nrow(above_threshold) > 0) {
        tp_count <- sum(above_threshold$Status == "TP")
        fp_count <- sum(above_threshold$Status == "FP")
        precision <- tp_count / (tp_count + fp_count)
        max_precision <- max(max_precision, precision)
        
        # If this threshold achieves target precision, save it and break
        if (precision >= target_precision) {
          thresholds[[species]] <- threshold
          cat(sprintf("%-25s: %.2f (Precision: %.3f)\n", species, threshold, precision))
          break
        }
      }
    }
    
    # If no threshold worked, add to removed species
    if (!(species %in% names(thresholds))) {
      cat(sprintf("%-25s: Cannot achieve %.0f%% precision (Max: %.3f)\n", 
                  species, target_precision * 100, max_precision))
      
      removed_species <- rbind(removed_species, data.frame(
        Species = species,
        Total_Samples = nrow(species_data),
        TP_Count = sum(species_data$Status == "TP"),
        FP_Count = sum(species_data$Status == "FP"),
        Max_Precision_Achieved = max_precision,
        Reason = "Cannot achieve 90% precision",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Store removed species in global environment for access
  assign("removed_species_df", removed_species, envir = .GlobalEnv)
  
  return(thresholds)
}

# Function to filter data using species-specific thresholds
filter_by_thresholds <- function(df, thresholds) {
  
  # Remove nocall and treat U as FP 
  df_clean <- df %>%
    filter(Common.Name != "nocall") %>%
    mutate(Status = ifelse(Status == "U", "FP", Status))
  
  # Apply thresholds
  filtered_data <- data.frame()
  
  for (species in names(thresholds)) {
    threshold <- thresholds[[species]]
    
    species_detections <- df_clean %>%
      filter(Common.Name == species, Confidence.x >= threshold)
    
    filtered_data <- rbind(filtered_data, species_detections)
  }
  
  cat("\nFiltered from", nrow(df_clean), "to", nrow(filtered_data), "detections\n")
  
  return(filtered_data)
}

#  Apply thresholds to dataset 
filter_new_data_by_thresholds <- function(df, thresholds, confidence_col = "Confidence") {
  
  # Remove nocall detections
  df_clean <- df %>%
    filter(Common.Name != "nocall")
  
  # Apply thresholds
  filtered_data <- data.frame()
  
  cat("Applying thresholds to new dataset:\n")
  
  for (species in names(thresholds)) {
    threshold <- thresholds[[species]]
    
    # Filter species detections above threshold
    species_detections <- df_clean %>%
      filter(Common.Name == species, .data[[confidence_col]] >= threshold)
    
    if (nrow(species_detections) > 0) {
      filtered_data <- rbind(filtered_data, species_detections)
      cat(sprintf("%-25s: %d detections above %.2f threshold\n", 
                  species, nrow(species_detections), threshold))
    }
  }
  
  cat(sprintf("\nTotal filtered from %d to %d detections (%.1f%% reduction)\n", 
              nrow(df_clean), nrow(filtered_data), 
              (1 - nrow(filtered_data)/nrow(df_clean)) * 100))
  
  return(filtered_data)
}

# 1. Load verified data to calculate thresholds
bird_data <- read.csv("2025_strata_done.csv")

# 2. Find thresholds using  verified data
thresholds <- find_species_thresholds(bird_data, target_precision = 0.90)

# 2b. Display and save species removed for not achieving 90% precision
cat("\n" + paste(rep("=", 50), collapse="") + "\n")
cat("SPECIES REMOVED FOR NOT ACHIEVING 90% PRECISION:\n")
cat(paste(rep("=", 50), collapse="") + "\n")

if (nrow(removed_species_df) > 0) {
  print(removed_species_df)
  
  # Save removed species to CSV
  write.csv(removed_species_df, "2025_species_removed_low_precision.csv", row.names = FALSE)
  cat(sprintf("\n%d species removed for not achieving 90%% precision\n", nrow(removed_species_df)))
} else {
  cat("All species achieved 90% precision!\n")
}

# 4. Apply thresholds to the new dataset 
filtered_tfive <- filter_new_data_by_thresholds(tfive_filter, thresholds, confidence_col = "Confidence")

# 5. Check results
cat("\nUnique species in filtered dataset:\n")
print(sort(unique(filtered_tfive$Common.Name)))

# 6. Save the filtered dataset
write.csv(filtered_tfive, "2025_filtered.csv", row.names = FALSE)

# 7. Save the thresholds for reference
thresholds_df <- data.frame(
  Species = names(thresholds),
  Threshold = unlist(thresholds)
)
#write.csv(thresholds_df, "species_thresholds_90percent_precision.csv", row.names = FALSE)

# ============================================================================
# 5. POST-VALIDATION REMOVAL/SWAPS
# ============================================================================

f_2025 <- read.csv("2025_filtered.csv")
unique(f_2025$Common.Name)

# Species removed because of unlikely detection/swapped subspecies

# Remove specified species and rename Green-winged teal to Eurasian teal
f_2025_species <- f_2025 %>%
  filter(!Common.Name %in% c("Red-billed Chough", "Hooded Crow", "Yellowhammer")) %>%
  mutate(Common.Name = ifelse(Common.Name == "Green-winged Teal", "Eurasian Teal", Common.Name))

unique(f_2025_species$Common.Name)

# Detection per species

precalls_2025 <- f_2025_species
write.csv(precalls_2025, 'precalls_2025.csv')

detections_2025 <- precalls_2025 %>%
  group_by(Common.Name) %>%
  summarise(detections = n()) %>%
  ungroup()
write.csv(detections_2025,'detections_2025.csv')

# Convert to calls per minute

table(f_2025_species$Site)

# Calculate calls per minute and create community matrix
callspermin_2025 <- f_2025_species %>%
  # Count unique recording times per site to get total recording minutes
  group_by(Site, Date) %>%
  summarise(
    total_recording_minutes = n_distinct(Time),
    .groups = 'keep'
  ) %>%
  # Join back with species detection counts
  left_join(
    f_2025_species %>%
      group_by(Site, Date, Common.Name) %>%
      summarise(total_detections = n(), .groups = 'drop'),
    by = c("Site", "Date")
  ) %>%
  # Calculate calls per minute
  mutate(calls_per_minute = total_detections / total_recording_minutes) %>%
  ungroup()

# add habitat
calls_habitat_2025 <- callspermin_2025 %>%
  mutate(habitat = case_when(
    Site %in% c("233", "213", "113", "321") ~ "edge",
    Site %in% c("232", "121", "133", "332") ~ "interior",
    TRUE ~ NA_character_))

#write.csv(calls_habitat_2025, '2025_statsdf.csv', row.names = FALSE)

df_2025 <- read.csv('2025_statsdf.csv', header = TRUE)

detections_2025 <- df_2025 %>%
  group_by(Common.Name) %>%
  summarise(detections = n()) %>%
  ungroup()

# Finding the species removed i.e. the species present in tfive_filter but not in df_2025

missing_2025 <- tfive_filter %>%
  anti_join(df_2025, by = "Common.Name") %>%
  distinct(Common.Name)

write.csv(missing_2025, 'missing_2025.csv')













