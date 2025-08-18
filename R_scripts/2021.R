# 2021 weeks 3 & 4 data processing

require(tidyverse)
require(vegan)

# ============================================================================
# 1. LOAD BIRDNET TEXT FILES
# ============================================================================

twentyone <- read.delim("2021_all.txt", header = TRUE)
twentyone$Site <- str_remove(twentyone$Site, "-.*")

# ============================================================================
# 2. FILTER DATA
# ============================================================================
# filter for sites, dawn an dusk and add habitat classifications
tone <- twentyone %>%
  dplyr::filter(Site %in% c('121', '113', '133', '213', '222', '232', '233', '321', '332', 'F')) %>% # 10 2021 sites
  dplyr::filter(
    (Time >= "05:30:00" & Time <= "09:00:00") | 
      (Time >= "17:00:00" & Time <= "20:30:00")) %>% # dawn and dusk times 
  mutate(habitat = case_when(
    Site %in% c("233", "213", "321", "113", "F") ~ "edge",
    Site %in% c("232", "121", "332", "133", "222") ~ "interior",
    TRUE ~ NA_character_))

tone_filter <- tone %>%
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
  select(-Time_minute)

unique(tone$Site)
unique(tone$Date)
table(tone$Site)

# filter our species with <20 detections
tone_species <- tone %>%
  group_by(Common.Name) %>%
  filter(n() >= 20) %>%
  ungroup()

tone_under_20 <- tone %>%
  group_by(Common.Name) %>%
  filter(n() < 20) %>%
  ungroup()
unique(tone_under_20$Common.Name)

# ============================================================================
# 3. CREATE SPECIES-SPECIFIC THRESHOLDS
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
      above_threshold <- species_data %>% filter(Confidence >= threshold)
      
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
      filter(Common.Name == species, Confidence >= threshold)
    
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

# 1. Load your verified data to calculate thresholds
bird_data <- read.csv("2021_verified.csv")

# 2. Find thresholds using your verified data
thresholds <- find_species_thresholds(bird_data, target_precision = 0.90)

# 2b. Display and save species removed for not achieving 90% precision
cat("\n" + paste(rep("=", 50), collapse="") + "\n")
cat("SPECIES REMOVED FOR NOT ACHIEVING 90% PRECISION:\n")
cat(paste(rep("=", 50), collapse="") + "\n")

if (nrow(removed_species_df) > 0) {
  print(removed_species_df) }
  

# 4. Apply thresholds to the new dataset (without Status column)
# Note: Adjust confidence_col parameter if your confidence column has a different name
filtered_one <- filter_new_data_by_thresholds(tone_filter, thresholds, confidence_col = "Confidence")

# 5. Check results
cat("\nUnique species in filtered dataset:\n")
print(sort(unique(filtered_one$Common.Name)))


# 7. Save the thresholds for reference
thresholds_df <- data.frame(
  Species = names(thresholds),
  Threshold = unlist(thresholds)
)

# ============================================================================
# 4. POST-VALIDATIN REMOVAL
# ============================================================================

unique(f_2021_all$Common.Name)

# Species removed because of unlikely detection/swapped subspecies

# Remove specified species and rename Green-winged teal to Eurasian teal
f_2021_species <- filtered_one %>%
  filter(!Common.Name %in% c("Red-billed Chough", "Hooded Crow", "Yellowhammer", "Eurasian Curlew")) %>%
  mutate(Common.Name = ifelse(Common.Name == "Green-winged Teal", "Eurasian Teal", Common.Name))


precalls_2021_34 <- f_2021_species
write.csv(f_2021_species, 'precalls_2021_3&4.csv')

detections_2021_34 <- f_2021_species %>%
  group_by(Common.Name) %>%
  summarise(detections = n()) %>%
  ungroup()
write.csv(detections_2021_34, 'detections_2021_3&4.csv')

# Calculate calls per minute and create community matrix
callspermin_2021 <- f_2021_species %>%
  # Count unique recording times per site to get total recording minutes
  group_by(Site, Date) %>%
  summarise(
    total_recording_minutes = n_distinct(Time),
    .groups = 'keep'
  ) %>%
  # Join back with species detection counts
  left_join(
    f_2021_species %>%
      group_by(Site, Date, Common.Name) %>%
      summarise(total_detections = n(), .groups = 'drop'),
    by = c("Site", "Date")
  ) %>%
  # Calculate calls per minute
  mutate(calls_per_minute = total_detections / total_recording_minutes) %>%
  ungroup()

# add habitat
calls_habitat_2021<- callspermin_2021 %>%
  mutate(habitat = case_when(
    Site %in% c("233", "213", "321", "113", "F") ~ "edge",
    Site %in% c("232", "121", "332", "133", "222") ~ "interior",
    TRUE ~ NA_character_))


write.csv(calls_habitat_2021, '2021_3&4_statsdf.csv')
