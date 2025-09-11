# Load necessary libraries
library(tigris)
library(sf)
library(dplyr)

# Set options for tigris
options(tigris_use_cache = TRUE)

# Define the states of interest
states <- c("California", "Oregon", "Washington")

# Get county geometries
counties <- counties(state = states, cb = TRUE, class = "sf")

# Extract bounding box coordinates for each county
bbox_values <- st_bbox(counties)

# Minimum and maximum latitudes
min_lat <- bbox_values["ymin"]
max_lat <- bbox_values["ymax"]

# Print results
cat("Minimum Latitude:", min_lat, "\n")
cat("Maximum Latitude:", max_lat, "\n")

# counties names
county <- counties$NAME

# initialize empty list
results <- list()
min_lat <- matrix()
max_lat <- matrix()

# loop through all multipolygons

for (i in 1:length(counties$geometry)) {
  coords <- st_coordinates(counties$geometry[i])
  latmin <- min(coords[,2])
  latmax <- max(coords[,2])
  min_lat[i] <- latmin
  max_lat[i] <- latmax
  
  county <- toupper(counties$NAME[i])
  results[[i]] <- c(latmin, latmax, county) }

results <- cbind.data.frame(min_lat, max_lat, counties$NAME)
names(results) <- c("latmin","latmax", "county")

results$county <- toupper(results$county)

head(results)