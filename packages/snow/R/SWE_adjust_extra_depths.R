#extra depths adjustments - using extra depths to multiply density. 
#This is attempting to correct for greater variation in snow cover properties above treeline.

#bring in ss data (as GNWT employee)
source(paste0(user, "Documents/R_Scripts/Packages/snow/R/R_oracle_connect.R"))

# for non-GNWT users, download data (point_data.csv and sites.csv) from https://doi.org/10.46887/2025-005
# or use data from the /data folder of this package
md_3 <- readRDS(paste0(user,"/Documents/R_Scripts/Packages/snow/data/md_3.rds"))
sites <- readRDS(paste0(user,"/Documents/R_Scripts/Packages/snow/data/sites.rds"))
depths <- readRDS(paste0(user,"/Documents/R_Scripts/Packages/snow/data/depths.rds"))

sites_treeline <- c("Christison Lake", "Big Lake", " White Wolf Lake", "Winter Lake", "Nonacho Lake", "Gray Lake", "Dymond Lake")

treeline_sites <- md_3 %>%
  dplyr::filter(site %in% sites_treeline,
                year >= 2019,
                surface_type == "upland") %>%
  dplyr::mutate(date_time = as.Date(date_time)) 

treeline_sites_extra_depths <- depths %>%
  dplyr::filter(data_flag == "ED",
                site %in% sites_treeline,
                surface_type == "upland") %>%
  dplyr::mutate(date_time = as.Date(date_time)) 

treeline_sites_extra_depths_2 <- depths %>%
  dplyr::filter(instrument == "magnaprobe",
                site %in% sites_treeline)

treeline_sites_extra_depths <- rbind(treeline_sites_extra_depths, treeline_sites_extra_depths_2)

results <- data.frame()

# Outer loop for sites
for(i in sites_treeline) {
  # Filter data for this site
  site_data <- treeline_sites %>%
    dplyr::filter(site == i)
  
  site_depths <- treeline_sites_extra_depths %>%
    dplyr::filter(site == i)
  
  # Process each unique date
  unique_dates <- unique(site_data$date_time) 
  
  for(current_date in unique_dates) {
    # Get data for this date
    date_data <- site_data %>%
      dplyr::filter(date_time == current_date)
    
    date_depths <- site_depths %>%
      dplyr::filter(date_time == current_date)
    
    # Calculate mean density for this date
    mean_density <- mean(date_data$density, na.rm = TRUE)
    
    # Calculate mean of all extra depths for this date
    mean_extra_depth <- mean(date_depths$snow_depth_cm, na.rm = TRUE)
    
    original_mean_swe <- mean(date_data$swe_cm, na.rm = T)
    
    # Calculate adjusted SWE using mean density and mean extra depth
    adj_swe <- mean_density * mean_extra_depth
    
    # Add new row to results
    results <- rbind(results, data.frame(
      Date = as.Date(current_date),
      site = i,
      adj_swe = adj_swe,
      mean_extra_depth = mean_extra_depth,
      mean_density = mean_density,
      original_swe = original_mean_swe))
  }
}

# Print summary to verify we have data for all sites
print(table(results$site))

# Process all sites
final_results <- results %>%
  dplyr::filter(adj_swe >0)

write.csv(final_results, paste0(user, "/Documents/R_Scripts/Packages/snow/data/adjusted_swe_values.csv"))

