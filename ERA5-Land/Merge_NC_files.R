# Function to properly extract NetCDF files from ZIP archives
extract_netcdf_files <- function() {
  setwd("/Users/emmariley/Documents/NT_Hydrology/R_Scripts/Packages/ERA5")
  
  # Find all ZIP files
  zip_files <- list.files(pattern = "total_precipitation_.*\\.nc$")
  
  cat(sprintf(" Found %d ZIP files to extract\n", length(zip_files)))
  
  extracted_files <- c()
  
  for (zip_file in zip_files) {
    cat(sprintf(" Extracting %s...\n", zip_file))
    
    tryCatch({
      # Extract to temporary directory
      temp_dir <- tempdir()
      unzip(zip_file, exdir = temp_dir)
      
      # Find the data_0.nc file
      data_file <- file.path(temp_dir, "data_0.nc")
      
      if (file.exists(data_file)) {
        # Create new filename based on original ZIP name
        new_name <- gsub("\\.nc$", "_extracted.nc", zip_file)
        
        # Copy the NetCDF file to main directory
        file.copy(data_file, new_name)
        extracted_files <- c(extracted_files, new_name)
        
        cat(sprintf("✅ Extracted to %s\n", new_name))
        
        # Clean up temp file
        file.remove(data_file)
      } else {
        cat(sprintf("❌ No data_0.nc found in %s\n", zip_file))
      }
      
    }, error = function(e) {
      cat(sprintf("❌ Error extracting %s: %s\n", zip_file, e$message))
    })
  }
  
  return(extracted_files)
}

# Run the extraction
extracted_files <- extract_netcdf_files()

# Check what we got
cat(sprintf("\n Successfully extracted %d NetCDF files:\n", length(extracted_files)))
print(extracted_files)

# Complete merge script for extracted NetCDF files
merge_extracted_precipitation <- function() {
  # Get all extracted files
  extracted_files <- list.files(pattern = "_extracted\\.nc$")
  
  if (length(extracted_files) == 0) {
    stop("No extracted NetCDF files found!")
  }
  
  cat(sprintf(" Found %d extracted NetCDF files\n", length(extracted_files)))
  
  # Sort files chronologically
  extracted_files <- extracted_files[order(extracted_files)]
  
  # Load all files
  all_rasters <- list()
  successful_files <- c()
  
  for (i in seq_along(extracted_files)) {
    file <- extracted_files[i]
    cat(sprintf(" Loading file %d/%d: %s\n", i, length(extracted_files), file))
    
    tryCatch({
      raster_data <- terra::rast(file)
      all_rasters[[length(all_rasters) + 1]] <- raster_data
      successful_files <- c(successful_files, file)
      cat(sprintf("✅ Successfully loaded %s\n", file))
    }, error = function(e) {
      cat(sprintf("❌ Error loading %s: %s\n", file, e$message))
    })
  }
  
  if (length(all_rasters) == 0) {
    stop("No files could be loaded successfully!")
  }
  
  cat(sprintf("\n Successfully loaded %d out of %d files\n", length(all_rasters), length(extracted_files)))
  
  # Merge all rasters
  cat(" Merging all rasters...\n")
  merged_raster <- do.call(c, all_rasters)
  
  # Set proper names
  names(merged_raster) <- paste0("precip_", 1:nlyr(merged_raster))
  
  cat(sprintf("✅ Successfully merged %d layers\n", nlyr(merged_raster)))
  
  return(merged_raster)
}

# Run the merge
merged_data <- merge_extracted_precipitation()

# Save the merged data
cat(" Saving merged data...\n")
terra::writeCDF(merged_data, "total_precipitation_merged_2000_2024.nc", 
                overwrite = TRUE, 
                compression = 9,
                varname = "total_precipitation",
                longname = "Total Precipitation",
                unit = "m")

cat("✅ All done! Merged data saved as total_precipitation_merged_2000_2024.nc\n")
