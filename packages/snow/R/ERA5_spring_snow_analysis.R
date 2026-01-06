# ERA5 Snow Depth Analysis - Spring Maximum (March-May) 1995-2025
`%>%` <- magrittr::`%>%`

#define user
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME"))) # PC version

#install libraries
#may have to download KrigR manually from here: https://github.com/ErikKusch/KrigR and save locally
#download KrigR dependencies:
options(download.file.method = "wininet", timeout = 300)
install.packages(c("ecmwfr", "ncdf4", "automap", "foreach", "doSNOW", "pbapply", "cowplot"),
                 dependencies = TRUE)
install.packages(paste0(user, "/Documents/R_Scripts/Packages/github_clones/KrigR-master"), repos = NULL, type = "source")

# Required libraries
library(KrigR)
library(terra)
library(dplyr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(ggrepel)

# Define spatial extent
lat_min = 51
lat_max = 80
long_min = -140
long_max = -95
interval = 0.1

#define api connection details
api_user <- #API USER
api_key <- #API KEY

#define file names
file <- "spring_max_"

# Create extent object for KrigR
study_extent <- terra::ext(c(long_min, long_max, lat_min, lat_max))

# Create empty dataframe to store results
spring_max_snow <- data.frame(
  Year = numeric(),
  Max_Snow_Depth = numeric(),
  Date_of_Max = character()  # Added to track when maximum occurs
)

# Function to download and process spring snow depth for a single year
get_spring_snow <- function(year) {
  # Define date range for spring (March-May)
  date_start <- paste0(year, "-03-01 00:00") #change these each time according to date. default "-03-01 00:00"
  date_stop <- paste0(year, "-05-31 23:00") #change these each time according to date. default "-05-31 23:00"
  
  # Download data using CDownloadS
  snow_data <- CDownloadS(
    Variable = "snow_depth",
    DataSet = "reanalysis-era5-land",  # ERA5-Land dataset
    Type = NA,  # Not needed for ERA5-Land
    DateStart = date_start,
    DateStop = date_stop,
    TZone = "UTC",
    TResolution = "hour",  # Get hourly data
    TStep = 1,  # 1-hour steps
    FUN = "none",  # Don't aggregate - we want raw hourly values
    Extent = study_extent,
    Dir = getwd(),
    FileName = paste0("snow_depth_", file, year),
    FileExtension = ".nc",
    API_User = api_user,  # You'll need to replace these with your actual credentials
    API_Key = api_key,    # You'll need to replace these with your actual credentials
    verbose = TRUE
  )
  
  return(snow_data)
}

# Process each year 
for(year in 1995:2025) {
  tryCatch({
    cat(sprintf("\nProcessing year %d...\n", year))
    
    # Get spring snow data for the year
    snow_data <- get_spring_snow(year)
    
  }, error = function(e) {
    message(sprintf("Error processing year %d: %s", year, e$message))
  })
}

combined_max_snow <- NULL

#combine nc files into one dataframe
for (year in 1995:2025) {
  tryCatch({
    cat(sprintf("\nProcessing year %d...\n", year))

    nc_file <- sprintf(paste0("snow_depth_", file, "%d.nc"), year)
    if (!file.exists(nc_file)) stop("NetCDF file not found.")

    snow_data <- terra::rast(nc_file)

    # Confirm dimensions
    cat(sprintf("  → Snow data has %d layers (time steps)\n", terra::nlyr(snow_data)))

    # Generate grid of coordinates
    lat_seq <- seq(lat_min, lat_max, by = interval)
    lon_seq <- seq(long_min, long_max, by = interval)
    coord_grid <- expand.grid(lon = lon_seq, lat = lat_seq)
    
    points_vect <- terra::vect(coord_grid, geom = c("lon", "lat"), crs = terra::crs(snow_data))

    point_values <- terra::extract(snow_data, points_vect)[, -1]  # drop ID col

    # Diagnostic check on extraction
    if (ncol(point_values) != terra::nlyr(snow_data)) {
      stop("Mismatch between extracted columns and raster layers.")
    }

    # Optionally show first row's time series
    cat("  → Sample snow depth time series at first point:\n")
    print(round(point_values[1, 1:min(10, ncol(point_values))], 2))  # first 10 time values

    # Compute max across time (each row = a point)
    point_max <- apply(point_values, 1, max, na.rm = TRUE)
    # Alternatively, use: point_max <- matrixStats::rowMaxs(as.matrix(point_values), na.rm = TRUE)

    # Replace Inf values
    inf_count <- sum(is.infinite(point_max))
    if (inf_count > 0) cat(sprintf(" Found %d Inf values. Replacing with NA...\n", inf_count))
    point_max[is.infinite(point_max)] <- NA

    # Check summary of values
    cat("  → Max snow depth summary:\n")
    print(summary(point_max))

    # Sanity check: are all values NA or zero?
    if (all(is.na(point_max))) warning("All point max values are NA for year ", year)
    if (all(point_max == 0, na.rm = TRUE)) warning("All point max values are zero for year ", year)
    if (length(unique(na.omit(point_max))) == 1) warning("All point max values are identical for year ", year)

    # Build tibble for this year
    year_df <- dplyr::tibble(
      lon = coord_grid$lon,
      lat = coord_grid$lat,
      !!paste0("Snow_", year) := point_max
    )

    # Merge into wide format
    if (is.null(combined_max_snow)) {
      combined_max_snow <- year_df
    } else {
      combined_max_snow <- full_join(combined_max_snow, year_df, by = c("lon", "lat"))
    }

    terra::tmpFiles(remove = TRUE)
    gc()

  }, error = function(e) {
    message(sprintf("❌ Error processing year %d: %s", year, e$message))
  })
}

write.csv(combined_max_snow, (paste0(user, "/Documents/R_Scripts/Packages/ERA5/data/SWEmax_dataset/", file, "_combined_max_snow.csv")))
