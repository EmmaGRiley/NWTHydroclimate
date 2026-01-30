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
api_user <- ""
api_key <- ""

#define file names
file <- "Feb_max_"

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
  date_start <- paste0(year, "-02-01 00:00") #change these each time according to date. default "-02-01 00:00"
  date_stop <- paste0(year, "-03-01 23:00") #change these each time according to date. default "-05-31 23:00"
  
  # Download data using CDownloadS
  snow_data <- CDownloadS(
    Variable = "snow_depth_water_equivalent",
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
    
    cat("Raw range:   ", range(point_values, na.rm = TRUE), "\n")
    
    #filtering values
    FILL <- -1.175494e+38 #.nc file fill value
    vals_clean <- point_values
    vals_clean[vals_clean <= -1e30] <- NA          # remove fill
    vals_clean[vals_clean == 10] <- NA             # glacier flag
    vals_clean[vals_clean < 0 & abs(vals_clean) < 1e-12] <- 0  # clamp underflow
    vals_clean[vals_clean < 0 | vals_clean > 10] <- NA         # physical bounds
    
    cat("Clean range: ", range(vals_clean, na.rm = TRUE), "\n")
    
    # Compute per-point max over time
    point_max <- apply(vals_clean, 1, max, na.rm = TRUE)

    # Compute max across time (each row = a point)
    point_max <- apply(vals_clean, 1, max, na.rm = TRUE)

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

#combining february max snow and march 1 - May 31st march snow
#replace max snow values in march 1-may 31 data if the february value is higher

combined_max_snow_feb <-  read.csv(paste0(user, "/Documents/R_Scripts/Packages/ERA5/data/SWEmax_dataset/feb_max__combined_max_snow.csv")) %>%
  dplyr::select(-c(X))

combined_max_snow_marchmay <-  read.csv(paste0(user, "/Documents/R_Scripts/Packages/ERA5/data/SWEmax_dataset/combined_max_snow_march_may.csv")) %>%
  dplyr::select(-c(X))
  
#pivot dataframe

combined_max_snow_marchmay <- combined_max_snow_marchmay %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("Snow_") & !dplyr::matches("Snow_Avg"),
                      names_to = "sd_year",
                      names_prefix = "Snow_",
                      values_to = "sd") %>%
  dplyr::rename(Latitude = lat,
                Longitude = lon) 

combined_max_snow_marchmay <- combined_max_snow_marchmay %>%
  dplyr::select(Latitude, Longitude, sd, sd_year)

combined_max_snow_feb <- combined_max_snow_feb %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("Snow_") & !dplyr::matches("Snow_Avg"),
                      names_to = "sd_year",
                      names_prefix = "Snow_",
                      values_to = "sd") %>%
  dplyr::rename(Latitude = lat,
                Longitude = lon)

combined_max_snow_feb <- combined_max_snow_feb %>%
  dplyr::select(Latitude, Longitude, sd, sd_year) 

#filter datasets to MRB
proj <- '+proj=longlat +datum=WGS84'

combined_max_snow_sf <- sf::st_as_sf(combined_max_snow_feb, coords = c("Longitude", "Latitude"), crs = proj)
combined_max_snow_marchmay_sf <- sf::st_as_sf(combined_max_snow_marchmay, coords = c("Longitude", "Latitude"), crs = proj)

Mack <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/MackenzieRiverBasin_FDA.shp"),
                    layer = "MackenzieRiverBasin_FDA")
Mack <- sf::st_transform(Mack, sp::CRS(proj)) # Change the projection
Mack <- sf::st_zm(Mack)
combined_max_snow_sf <- sf::st_intersection(combined_max_snow_sf, Mack)
combined_max_snow_marchmay_sf <- sf::st_intersection(combined_max_snow_marchmay_sf, Mack)

#update march_may max where february max is higher

add_coords <- function(x) {
  xy <- sf::st_coordinates(x)
  x %>%
    sf::st_drop_geometry() %>%
    mutate(
      Longitude = xy[, 1],
      Latitude  = xy[, 2]
    )
}

round_key <- function(x, digits = 4) round(x, digits)

# Extract coords + standardize keys

safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else max(x)
}

feb_df <- add_coords(combined_max_snow_sf) %>%
  mutate(
    Longitude = round_key(Longitude, 4),
    Latitude  = round_key(Latitude, 4),
    year      = as.integer(sd_year),
    sd_all    = sd
  ) %>%
  select(Longitude, Latitude, year, sd_all)

mm_df <- add_coords(combined_max_snow_marchmay_sf) %>%
  mutate(
    Longitude = round_key(Longitude, 4),
    Latitude  = round_key(Latitude, 4),
    year      = as.integer(sd_year),
    sd_mm     = sd
  ) %>%
  select(Longitude, Latitude, year, sd_mm)


# De-duplicate (st_intersection can replicate boundary points)

feb_df <- feb_df %>%
  group_by(Longitude, Latitude, year) %>%
  summarise(sd_all = safe_max(sd_all), .groups = "drop")

mm_df <- mm_df %>%
  group_by(Longitude, Latitude, year) %>%
  summarise(sd_mm = safe_max(sd_mm), .groups = "drop")


# Join + replace where february is higher
# Keep the March–May grid as the "base" and update it.

mm_updated <- mm_df %>%
  left_join(feb_df, by = c("Longitude", "Latitude", "year")) %>%
  mutate(
    sd_updated = case_when(
      !is.na(sd_all) & !is.na(sd_mm) & sd_all > sd_mm ~ sd_all,
      TRUE ~ sd_mm
    ),
    was_replaced = !is.na(sd_all) & !is.na(sd_mm) & (sd_all > sd_mm),
    diff_added = if_else(was_replaced, sd_all - sd_mm, 0)
  )

# QA summaries

qa_overall <- mm_updated %>%
  summarise(
    n_total = n(),
    n_matched_all = sum(!is.na(sd_all)),
    n_replaced = sum(was_replaced, na.rm = TRUE),
    replaced_pct_of_total = 100 * n_replaced / n_total,
    added_mean = mean(diff_added[was_replaced], na.rm = TRUE),
    added_median = median(diff_added[was_replaced], na.rm = TRUE),
    added_max = max(diff_added[was_replaced], na.rm = TRUE)
  )

qa_by_year <- mm_updated %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    n_replaced = sum(was_replaced, na.rm = TRUE),
    replaced_pct = 100 * n_replaced / n_total,
    added_mean = mean(diff_added[was_replaced], na.rm = TRUE),
    added_max = max(diff_added[was_replaced], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

print(qa_overall)
print(head(qa_by_year, 10))

write.csv(mm_updated, paste0(getwd(), "/ERA5_updatedfeb_may.csv"))
