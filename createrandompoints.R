# Load required libraries
library(terra)
library(sf)

# Function to generate random points from raster
raster_to_random_points <- function(raster_file, output_file = NULL) {
  
  # Read the raster
  r <- rast(raster_file)
  
  # Convert raster to data frame with coordinates
  # This gets cell values and their center coordinates
  r_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
  # Get raster resolution (cell size)
  res_x <- res(r)[1]
  res_y <- res(r)[2]
  
  # Initialize empty list to store points
  all_points <- list()
  
  # Loop through each cell
  for (i in 1:nrow(r_df)) {
    cell_value <- r_df$value[i]
    
    # Skip if value is 0 or negative
    if (cell_value <= 0) next
    
    # Round to nearest integer if not already
    n_points <- round(cell_value)
    
    if (n_points > 0) {
      # Calculate cell boundaries
      x_min <- r_df$x[i] - res_x/2
      x_max <- r_df$x[i] + res_x/2
      y_min <- r_df$y[i] - res_y/2
      y_max <- r_df$y[i] + res_y/2
      
      # Generate random points within the cell
      rand_x <- runif(n_points, min = x_min, max = x_max)
      rand_y <- runif(n_points, min = y_min, max = y_max)
      
      # Store points with original cell value
      all_points[[i]] <- data.frame(
        x = rand_x,
        y = rand_y,
        cell_value = cell_value,
        cell_id = i
      )
    }
  }
  
  # Combine all points
  points_df <- do.call(rbind, all_points)
  
  # Convert to sf object
  points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = crs(r))
  
  # Save if output file specified
  if (!is.null(output_file)) {
    st_write(points_sf, output_file, append = FALSE)
    message(paste("Points saved to:", output_file))
  }
  
  # Print summary
  message(paste("Total points generated:", nrow(points_sf)))
  message(paste("From", length(unique(points_df$cell_id)), "cells"))
  
  return(points_sf)
}

# Example usage:
# points <- raster_to_random_points("path/to/your/raster.tif", "output_points.shp")

# Alternative: More memory-efficient version for large rasters
raster_to_random_points_efficient <- function(raster_file, output_file = NULL, 
                                               max_points_per_cell = 1000) {
  
  r <- rast(raster_file)
  res_x <- res(r)[1]
  res_y <- res(r)[2]
  
  # Process in chunks to save memory
  all_points <- data.frame()
  
  for (i in 1:ncell(r)) {
    cell_value <- r[i][1]
    
    if (is.na(cell_value) || cell_value <= 0) next
    
    n_points <- min(round(cell_value), max_points_per_cell)
    
    if (n_points > 0) {
      # Get cell coordinates
      cell_coords <- xyFromCell(r, i)
      x_center <- cell_coords[1]
      y_center <- cell_coords[2]
      
      # Generate random points
      rand_x <- runif(n_points, x_center - res_x/2, x_center + res_x/2)
      rand_y <- runif(n_points, y_center - res_y/2, y_center + res_y/2)
      
      # Append to dataframe
      all_points <- rbind(all_points, data.frame(
        x = rand_x,
        y = rand_y,
        cell_value = cell_value
      ))
    }
  }
  
  # Convert to sf
  points_sf <- st_as_sf(all_points, coords = c("x", "y"), crs = crs(r))
  
  if (!is.null(output_file)) {
    st_write(points_sf, output_file, append = FALSE)
  }
  
  message(paste("Total points generated:", nrow(points_sf)))
  
  return(points_sf)
}

# Quick visualization example (if you want to check results)
# plot(r)
# plot(st_geometry(points), add = TRUE, pch = 20, cex = 0.5, col = "red")