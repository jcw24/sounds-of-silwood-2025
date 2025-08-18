# Script to estimate canopy properties using hemispherical photography
# 11/05/2025

# Load package
library(hemispheR)

# List of image file names
image_files <- c("S113.jpeg", "S121.jpeg", "S133.jpeg", "S213.jpeg",
                 "S222.jpeg", "S232.jpeg", "S233.jpeg", "S321.jpeg",
                 "S332.jpeg", "SA.jpeg", "SD.jpeg", "SF.jpeg")

# Function to process each image
process_image <- function(filename) {
  message(paste("Processing:", filename))
  
  # Import fisheye image
  S <- import_fisheye(filename, channel = 3, circ.mask = NULL, circular = TRUE,
                      display = TRUE, message = TRUE)
  
  # Binarize image
  B <- binarize_fisheye(S, method = "Otsu", display = TRUE, export = TRUE)
  
  # Calculate gap fraction
  GF <- gapfrac_fisheye(B, lens = 'equidistant', nrings = 1, nseg = 1, 
                        startVZA = 0, endVZA = 60, display = TRUE, message = TRUE)
  
  # Estimate canopy attributes
  CP <- canopy_fisheye(GF)
  
  return(CP)
}

# Apply the function to each file
results <- lapply(image_files, process_image)

# Convert the results into a data frame
results_df <- do.call(rbind, lapply(results, function(x) as.data.frame(t(unlist(x)))))

# Add filenames as a column
results_df$image <- rownames(results_df)

# Write to CSV
write.csv(results_df, "canopy_attributes_results.csv", row.names = FALSE)

# Second batch of photos 24/05/2025

# Load package
library(hemispheR)

# List of image file names
image_files <- c("113.jpeg", "121.jpeg", "133.jpeg", "213.jpeg",
                 "222.jpeg", "232.jpeg", "233.jpeg", "321.jpeg",
                 "332.jpeg", "A.jpeg", "D.jpeg", "F.jpeg")

# Function to process each image
process_image <- function(filename) {
  message(paste("Processing:", filename))
  
  # Import fisheye image
  S <- import_fisheye(filename, channel = 3, circ.mask = NULL, circular = TRUE,
                      display = TRUE, message = TRUE)
  
  # Binarize image
  B <- binarize_fisheye(S, method = "Otsu", display = TRUE, export = TRUE)
  
  # Calculate gap fraction
  GF <- gapfrac_fisheye(B, lens = 'equidistant', nrings = 1, nseg = 1, 
                        startVZA = 0, endVZA = 60, display = TRUE, message = TRUE)
  
  # Estimate canopy attributes
  CP <- canopy_fisheye(GF)
  
  return(CP)
}

# Apply the function to each file
results2 <- lapply(image_files, process_image)

# Optionally name the results list by file name
names(results2) <- image_files

# Convert the results into a data frame
results_df2 <- do.call(rbind, lapply(results2, function(x) as.data.frame(t(unlist(x)))))

# Add filenames as a column
results_df2$image <- rownames(results_df2)

# Reorder columns to put image name first
results_df2 <- results_df2[, c("image", setdiff(names(results_df2), "image"))]

# Write to CSV
write.csv(results_df2, "canopy_attributes_results2.csv", row.names = FALSE)

message("CSV file saved as 'canopy_attributes_results.csv'")


