# Script to see which of Professor Crawley's vegetation sampling points are closest to my own so I can classify them by habitat
# 20/05/25

# packages

require(geosphere)
require(dplyr)
require(tidyr) 

sites <- read.csv("Sites.csv")
habitats <- read.csv("SilwoodPlants_sites.csv")

## Find closest site to each of my sampling points

# function to return closest habitat site to my sampling points
get_closest_habitat <- function(lat, lon, habitats) {
  site_coord <- c(lon, lat)  
  habitat_coords <- cbind(habitats$longitude, habitats$latitude)
  
  distances <- distHaversine(site_coord, habitat_coords)
  
  closest_index <- which.min(distances)
  return(habitats[closest_index, ])
}

# Apply to each row of the sites table
classified_sites <- sites %>%
  rowwise() %>%
  mutate(
    closest_habitat_row = list(get_closest_habitat(lat, long, habitats))
  ) %>%
  unnest_wider(closest_habitat_row, names_sep = "_")

classified_sites <- sites %>%
  rowwise() %>%
  mutate(
    closest_habitat_row = list(get_closest_habitat(lat, long, habitats))
  ) %>%
  unnest_wider(closest_habitat_row, names_sep = "_")

## Create a column for the distance
get_closest_habitat_with_distance <- function(lat, lon, habitats) {
  site_coord <- c(lon, lat)
  habitat_coords <- cbind(habitats$longitude, habitats$latitude)
  
  distances <- distHaversine(site_coord, habitat_coords)
  closest_index <- which.min(distances)
  
  result <- habitats[closest_index, ]
  result$distance_m <- distances[closest_index]
  return(result)
}

classified_sites <- sites %>%
  rowwise() %>%
  mutate(
    closest_habitat_row = list(get_closest_habitat_with_distance(lat, long, habitats))
  ) %>%
  unnest_wider(closest_habitat_row, names_sep = "_")

# select relevant sites
site_habitats <- classified_sites %>%
  select(sample_point, lat, long, closest_habitat_row_site.number, closest_habitat_row_latitude, closest_habitat_row_longitude, closest_habitat_row_habitat, closest_habitat_row_distance_m)

# save as a csv

write.csv(site_habitats, 'site_habitats.csv')








