# =============================================================================
# ERA5 Snow Trends Mapping - Main Analysis Script
# =============================================================================
# This script performs Mann-Kendall trend analysis on ERA5-Land snow data
# and compares results with manual snow survey data from 1995-2024
# 
# Workflow:
# 1. Load and prepare data
# 2. Perform ERA5-Land trend analysis
# 3. Perform manual survey trend analysis  
# 4. Create comparison maps
# =============================================================================

# Specify pathways
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")))
savepath <- paste0(user, "/Documents/R_Scripts/Packages/snow/data/")

#set web shot path
Sys.setenv(PATH = paste0(user, "/Documents/Modelling/phantomjs/phantomjs/bin")) #can install phantomjs executable here: webshot::install_phantomjs()

# Define operators
`%>%` <- magrittr::`%>%`
'%!in%' = Negate('%in%')

# Load other scripts
source(paste0(user, "/Documents/R_Scripts/Packages/snow/R/ERA5_snowtrends_map_hourly_functions.R"))

# =============================================================================
# DATA LOADING AND PREPARATION
# =============================================================================

# Load NWT manual survey data (returns md_3, sites, depths, snow, swe)
# Can only use this as a GNWT employee
# source(paste0(user, "/Documents/R_Scripts/Packages/snow/R/R_Oracle_Connect.R"))

# for non-GNWT users, download data (point_data.csv and sites.csv) from https://doi.org/10.46887/2025-005
# or use data from the /data folder of this package
md_3 <- readRDS(paste0(user,"/Documents/R_Scripts/Packages/snow/data/md_3.rds"))

sites <- readRDS(paste0(user,"/Documents/R_Scripts/Packages/snow/data/sites.rds"))

# Load CanSWE data - download version 7 of Canswe and run CanSWE_processing_V7.R first
canswe <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/", "CanSWE_v7.csv")) %>%
  dplyr::rename("site" = "station_name") %>%
  dplyr::filter(type_mes == 0, # multi point snow survey only
                source != "Government of Northwest Territories")

# Process elevation data from CanSWE for NWT sites
nwt_elevation_data <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/", "CanSWE_v7.csv")) %>%
  dplyr::rename("site" = "station_name") %>%
  dplyr::filter(source == "Government of Northwest Territories") %>%
  dplyr::select(site, elevation, Latitude, Longitude) %>%
  dplyr::mutate(site = stringr::str_to_title(site)) %>%
  dplyr::mutate(site = dplyr::case_when(
    site == "Checkpoint (Formerly Jean Marie Creek" ~ "Checkpoint",
    site == "Tibbitt Lake (Ingraham Tr 64 Nw)" ~ "IT64NW",
    site == "Ingraham Trail Km 64 Se" ~ "IT64SE",
    site == "Tibbitt Lake Muskeg" ~ "Tibbitt Lake_m",
    site == "Crown Fire Site" ~ "Crown Fire",
    site == "Fort Liard (Forestry)" ~ "Fort Liard_f",
    site == "Wrigley (Forestry)" ~ "Wrigley_f",
    site == "Trout Lake (Forestry)" ~ "Trout Lake_f",
    site == "Jean Marie River (Forestry)" ~ "Jean Marie River_f",
    site == "Powder Lake (Forestry)" ~ "Powder Lake",
    site == "Hay River (Forestry)" ~ "Hay River_f",
    site == "Pine Point A" ~ "Pine Point",
    site == "Sharples Lake East" ~ "Sharples Lake",
    site == "Swede Creek (Forestry)" ~ "Swede Creek_f",
    TRUE ~ site
  ))

nwt_elevation_data <- unique(nwt_elevation_data)

# bring in high res coordinates for later mapping and analysis
coordinates_high_res <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/site_coordinates_highres.csv"))

# =============================================================================
# ANALYSIS PARAMETERS
# =============================================================================

# Spatial parameters
interval <- 0.1
lat_min <- 51
lat_max <- 80
long_min <- -140
long_max <- -95

# Analysis settings
Mack_basin <- TRUE
Mack_basin_outline <- TRUE
NWT_border <- TRUE
save <- TRUE
significantonly <- FALSE
comparemanual <- TRUE

# Data parameters
data <- md_3
start_year <- 1995
end_year <- 2025
min_year <- (31) * 0.75
p.value <- 0.05
exclude_sites <- NA

# Filtering parameters
act <- c("IA", "A")
surface <- c("upland")
flags <- c("Y", "Sk", "P", "Sk_2")
hdensity <- 0.34
ldensity <- 0.1

# =============================================================================
# ERA5-LAND TREND ANALYSIS
# =============================================================================
# This section performs Mann-Kendall trend analysis on ERA5-Land snow data
# Note: This analysis only needs to be run once and results are saved to CSV
# =============================================================================

# Load ERA5-Land snow data
# must run ERA5_spring_snow_analysis.R first to create the spring max snow depth dataset

ERA5_snow <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/ERA5_updatedfeb_may.csv")) %>%
  dplyr::select(-c(X))

ERA5_snow <- ERA5_snow %>%
  dplyr::select(Latitude, Longitude, sd_updated, year) %>%
  dplyr::rename("sd" = "sd_updated",
                "sd_year" = "year")
  
  # Define the bounding box and coordinate resolution (rounded to the nearest 0.1)
  lat_seq <- seq(lat_min, lat_max, by = interval)
  lon_seq <- seq(long_min, long_max, by = interval)

  # Prepare an empty data frame to store the results
  trend_results <- data.frame(
    Latitude = numeric(),
    Longitude = numeric(),
    Tau = numeric(),
    P_value = numeric(),
    Sen_slope = numeric()
  )

  autocorr_summary <- data.frame(
    Latitude = numeric(),
    Longitude = numeric(),
    ACF_value = numeric(),
    P_value = numeric(),
    stringsAsFactors = FALSE
  )


  # Iterate over each coordinate within the bounding box
  for (lat in lat_seq) {
    for (lon in lon_seq) {

      max_snow <- ERA5_snow %>%
        dplyr::filter(Latitude == lat,
                      Longitude == lon)

        if (length(max_snow$sd) == 0) {
          next  # Skip if no valid indices for this lat/lon
        }
      
      if (all(is.na(max_snow$sd)) || length(na.omit(max_snow$sd)) < 3) {
        next  # Skip if no usable data
      }

      #perform modifiedmk::mmkh test if lat/long is autocorrelated
      acf_result = acf(max_snow$sd, plot = FALSE)
      p_value = Box.test(max_snow$sd, type = "Ljung-Box")$p.value

      if (!is.na(p_value) && p_value < 0.05) {
        autocorr_summary <- rbind(autocorr_summary, data.frame(
          Latitude = lat,
          Longitude = lon,
          ACF_value = acf_result$acf[2],  # lag-1 autocorrelation
          P_value = p_value
        ))
      }

      if (!is.na(p_value) && p_value < 0.05) {
        # Perform the modified Mann-Kendall trend test
        test_mk <- modifiedmk::mmkh(max_snow$sd)
      } else {
        # Perform the Mann-Kendall trend test
        test_mk <- Kendall::MannKendall(max_snow$sd)
      }

      sen_slope <- trend::sens.slope(na.omit(max_snow$sd))

      if(!is.na(p_value) && p_value < 0.05) {
        # Store the results in the data frame
        trend_results <- trend_results %>%
          dplyr::add_row(
            Latitude = lat,
            Longitude = lon,
            Tau = test_mk[1],
            P_value = test_mk[2],
            Sen_slope = test_mk[7]
          )
      }else{
        trend_results <- trend_results %>%
          dplyr::add_row(
            Latitude = lat,
            Longitude = lon,
            Tau = test_mk$tau,
            P_value = test_mk$sl,
            Sen_slope = sen_slope$estimates
          )
      }
    }
  }

  autocorr_stats <- list(
    total_points = nrow(trend_results),
    autocorrelated_points = nrow(autocorr_summary),
    percent_autocorrelated = (nrow(autocorr_summary) / nrow(trend_results)) * 100,
    mean_acf = mean(abs(autocorr_summary$ACF_value)),
    acf_strength = table(cut(abs(autocorr_summary$ACF_value),
                             breaks = c(0, 0.1, 0.2, 0.4, 1),
                             labels = c("Weak", "Moderate", "Strong", "Very Strong"))))
  
  #computer meanmaxswe column
  meanmax_by_cell <- ERA5_snow %>%
    dplyr::mutate(
      sd_year = as.integer(sd_year),
      sd = as.numeric(sd)
    ) %>%
    dplyr::group_by(Latitude, Longitude) %>%
    dplyr::summarise(
      meanmaxswe = mean(sd, na.rm = TRUE)
    )
  
  trend_results <- trend_results %>%
    dplyr::left_join(meanmax_by_cell, by = c("Latitude", "Longitude"))


  #option to save and then bring in trend_results locally
  write.csv(trend_results, paste0(savepath, "trendresults_serialautocorr_hourly_95_24_MRB.csv"))
  write.csv(autocorr_stats, paste0(savepath, "autocorr_stats_hourly_95_24_MRB.csv"))
  write.csv(autocorr_summary, paste0(savepath, "autocorr_summary_hourly_95_24_MRB.csv"))
  
 
  ##################################################################################################################################
  
  #bringing in csv files from lines above
  trend_results <- read.csv(paste0(savepath, "trendresults_serialautocorr_hourly_95_24_MRB.csv"))
  autocorr_stats <- read.csv(paste0(savepath, "autocorr_stats_hourly_95_24_MRB.csv"))
  autocorr_summary <- read.csv(paste0(savepath, "autocorr_summary_hourly_95_24_MRB.csv"))
  
  trend_results <- trend_results %>%
    dplyr::select("Longitude", "Latitude", "Tau", "P_value", "Sen_slope", "meanmaxswe")
  
  #Define projection
  proj <- '+proj=longlat +datum=WGS84'
  
  # Convert trend results to an sf object
  trend_sf <- sf::st_as_sf(trend_results, coords = c("Longitude", "Latitude"), crs = proj)
  
  # #filter significant results only
  # if (significantonly == T){
  #   trend_sf <- trend_sf[trend_sf$P_value < 0.05, ]
  # }
  
  if(Mack_basin == T) {
    Mack <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/MackenzieRiverBasin_FDA.shp"),
                        layer = "MackenzieRiverBasin_FDA")
    Mack <- sf::st_transform(Mack, sp::CRS(proj)) # Change the projection
    Mack <- sf::st_zm(Mack)
    trend_sf <- sf::st_intersection(trend_sf, Mack)
  } 
  
  # Ensure the sf object is not empty
  if (nrow(trend_sf) == 0) stop("No data to plot after clipping.")
  
  #Create a column for opacity
  trend_sf <- trend_sf %>%
    dplyr::mutate(
      opacity = ifelse(P_value < 0.05, 1, 0.5),
      #create a column for units (m -> mm/decade)
      mm_SWE_per_decade = Sen_slope*10000,
      Bin = cut(
        mm_SWE_per_decade,
        breaks = c(-10000, -20, -10, -5, -3, 3, 5, 10, 20, 10000),
        labels = c("<(-20)", "(-20) - (-10)", "(-10) - (-5)", "(-5) - (-3)", 
                   "(-3) - 3", "3 - 5", "5 - 10", "10 - 20", "> 20"),
        include.lowest = TRUE
      )
    )
  
# =============================================================================
# MANUAL SURVEY TREND ANALYSIS
# =============================================================================
# This section performs Mann-Kendall trend analysis on manual snow survey data
# and compares results with ERA5-Land analysis
# =============================================================================
    
    sitename = unique(data$site[!(data$site%in% exclude_sites)])
    table <- data.frame()
    
    proj <- '+proj=longlat +datum=WGS84'
    
    # First check for autocorrelation - need to fix below
    acf_results <- serial_autocorr_test(data = data,
                                        start_year = start_year,
                                        end_year = end_year,
                                        flags = flags,
                                        hdensity = hdensity,
                                        ldensity = ldensity,
                                        surface = surface,
                                        act = act,
                                        exclude_sites = exclude_sites)
    
    acf_results_canswe <- serial_autocorr_test_canswe(data = canswe,
                                                      start_year = start_year,
                                                      end_year = end_year,
                                                      min_year = min_year)
    
    # Get interpretation of autocorrelation
    autocorr_sites <- interpret_acf(acf_results, only_autocorrelated = TRUE)

    # Get interpretation of autocorrelation
    autocorr_sites_canswe <- interpret_acf_canswe(acf_results_canswe, only_autocorrelated =  TRUE)
    
    for (i in sitename){
      
      TA = data[data$site[!(data$site%in% exclude_sites)] == i, ] %>%
        dplyr::filter (year >= start_year,
                       year <= end_year,
                       data_flag_1%!in% flags,
                       data_flag_2%!in% flags,
                       is.na(density)|density < hdensity,
                       is.na(density)|density > ldensity,
                       surface_type %in% surface,
                       activity %in% act) %>%
        dplyr::filter(dplyr::n_distinct(year)>3)%>%
        dplyr::group_by (year, surface_type) %>%
        dplyr::reframe(meanswe = mean(swe_cm, na.rm=TRUE),
                       Site_name = sample(i),
                       meandensity = mean(density, na.rm=TRUE),
                       meandepth = mean(snow_depth_cm, na.rm=T)) %>%
        dplyr::group_by(Site_name) %>%
        dplyr::mutate(sitemeanswe = mean(meanswe, na.rm=T)) %>%
        dplyr::ungroup()
      
      #adjust md_3 values -> using csv from SWE_adjust_extra_depths
      
      adjust_swe <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/adjusted_swe_values.csv")) %>%
        dplyr::select(c(-1,)) %>%
        dplyr::rename("date_time" = "Date",
                      "Site_name" = "site") %>%# remove column "x" 
        dplyr::mutate(year = lubridate::year(date_time))
      
      # Update TA with adjusted values
      TA <- TA %>%
        dplyr::left_join(
          adjust_swe %>% 
            dplyr::select(Site_name, year, adj_swe),  # Select only the columns we need
          by = c("Site_name", "year")
        ) %>%
        dplyr::mutate(
          meanswe = ifelse(!is.na(adj_swe), adj_swe, meanswe)  # Replace meanswe with adj_swe where available
        ) %>%
        dplyr::select(-adj_swe)  # Remove the temporary adj_swe column
      
        if (length(TA$meanswe) == 0) {} else {
          # Check if site is autocorrelated
          is_autocorr <- i %in% autocorr_sites$site
          
          if(is_autocorr) {
            # Use modified Mann-Kendall for autocorrelated sites
            test.mk <- modifiedmk::mmkh(TA$meanswe)
            test.ss <- trend::sens.slope(na.omit(TA$meanswe), conf.level = 0.95)
            p.value.function(p.value, c(test.mk[1], test.mk[2]))
            
            TA["p-value"] = round(as.numeric(test.mk[2]), 5)
            TA["Magnitude"] = round(as.numeric(test.ss[1]), 5)
            TA["Significance"] = if (test.mk[2] < p.value) {"yes"} else {"no"}
            TA["Years of Record"] = dplyr::n_distinct(TA$year)
            TA["Method"] = "Modified MK"
          } else {
            # Use regular Mann-Kendall for non-autocorrelated sites
            test.mk <- Kendall::MannKendall(TA$meanswe)
            test.ss <- trend::sens.slope(na.omit(TA$meanswe), conf.level = 0.95)
            p.value.function(p.value, test.mk)
            
            TA["p-value"] = round(as.numeric(test.mk[2]), 5)
            TA["Magnitude"] = round(as.numeric(test.ss[1]), 5)
            TA["Significance"] = if (test.mk[2] < p.value) {"yes"} else {"no"}
            TA["Years of Record"] = dplyr::n_distinct(TA$year)
            TA["Method"] = "Standard MK"
          }
          
          table <- rbind(table, TA)
        }
    } 
    
    #trend for canswe sites
    #filter data
    canswe <- canswe %>%
      dplyr::group_by(site) %>%
      dplyr::filter(year >= start_year,
                    year <= end_year) %>%
      dplyr::mutate(no_years = dplyr::n_distinct(year)) %>%
      dplyr::filter(no_years > min_year) %>%
      dplyr::select(site, Latitude, Longitude, year, swe_cm, no_years, elevation) %>%
      dplyr::group_by(site) %>%
      dplyr::mutate(sitemeanswe = mean(swe_cm, na.rm=T)) %>%
      dplyr::ungroup()
    
    canswe <- unique(canswe)
    
    site_name <- unique(canswe$site[!(canswe$site%in% exclude_sites)])
                        
    table_canswe <- data.frame()
      
    for (i in site_name){
      
      TA_canswe = canswe[canswe$site== i, ] 
      
      TA_canswe <- TA_canswe %>%
        dplyr::arrange(year) %>%  # Make sure data is in chronological order
        dplyr::mutate(
          # handle zeros - depending on your needs:
          swe_cm = ifelse(swe_cm == 0, NA, swe_cm)
        )
      
      # Before running MMK test, check data
      #summary(TA_canswe$swe_cm)
      
      # Add minimum valid observations check
      valid_obs <- sum(!is.na(TA_canswe$swe_cm))
      
      if (valid_obs < 10) {  # Adjust minimum number as needed
        next  # Skip to next iteration if too few valid observations
      }
      
      if (length(TA_canswe$swe_cm) == 0) {
        next
      } else {
        # Check if site is autocorrelated
        is_autocorr <- i %in% autocorr_sites_canswe$site
        
        tryCatch({
          if(is_autocorr) {
            # Use modified Mann-Kendall for autocorrelated sites
            test.mk <- modifiedmk::mmkh(TA_canswe$swe_cm)
            test.ss <- trend::sens.slope(na.omit(TA_canswe$swe_cm), conf.level = 0.95)
            
            # Check if test.mk[2] is NA before comparison
            if (!is.na(test.mk[2])) {
              TA_canswe["p-value"] = round(as.numeric(test.mk[2]), 5)
              TA_canswe["Magnitude"] = round(as.numeric(test.ss[1]), 5)
              TA_canswe["Significance"] = ifelse(test.mk[2] < p.value, "yes", "no")
              TA_canswe["Years of Record"] = dplyr::n_distinct(TA_canswe$year)
              TA_canswe["Method"] = "Modified MK"
            }
          } else {
            # Use regular Mann-Kendall for non-autocorrelated sites
            test.mk <- Kendall::MannKendall(TA_canswe$swe_cm)
            test.ss <- trend::sens.slope(na.omit(TA_canswe$swe_cm), conf.level = 0.95)
            
            # Check if test.mk[2] is NA before comparison
            if (!is.na(test.mk[2])) {
              TA_canswe["p-value"] = round(as.numeric(test.mk[2]), 5)
              TA_canswe["Magnitude"] = round(as.numeric(test.ss[1]), 5)
              TA_canswe["Significance"] = ifelse(test.mk[2] < p.value, "yes", "no")
              TA_canswe["Years of Record"] = dplyr::n_distinct(TA_canswe$year)
              TA_canswe["Method"] = "Standard MK"
            }
          }
          
          if (!is.na(test.mk[2])) {  # Only add to table if we got valid results
            table_canswe <- rbind(table_canswe, TA_canswe)
          }
        }, error = function(e) {
          warning(sprintf("Error processing site %s: %s", i, e$message))
        })
      }
    }
    
    SiteTable <- table %>%
      dplyr::group_by(Site_name) %>%
      dplyr::reframe(Magnitude = dplyr::first(Magnitude*100),
                     `p-value` = dplyr::first(`p-value`),
                     Significance = dplyr::first(Significance),
                     `Years of Record` = dplyr::first(`Years of Record`),
                     Method = dplyr::first(Method),
                     sitemeanswe = dplyr::first(sitemeanswe)) %>%  # Add Method to output
      dplyr::filter(`Years of Record` > min_year)
    
    SiteTable_canswe <- table_canswe %>%
      dplyr::group_by(site) %>%
      dplyr::reframe(Magnitude = dplyr::first(Magnitude*100),
                     `p-value` = dplyr::first(`p-value`),
                     Significance = dplyr::first(Significance),
                     `Years of Record` = dplyr::first(`Years of Record`),
                     Method = dplyr::first(Method),
                     sitemeanswe = dplyr::first(sitemeanswe)) %>%  # Add Method to output
      dplyr::filter(`Years of Record` > min_year)
    
    longitude = appendvar(var1=SiteTable_canswe$site, var2 = coordinates_high_res$Site_name, var3 = coordinates_high_res$Longitude)
    latitude = appendvar(var1=SiteTable_canswe$site, var2 = coordinates_high_res$Site_name, var3 = coordinates_high_res$Latitude)
    elevation = appendvar(var1=SiteTable_canswe$site, var2 = canswe$site, var3 = canswe$elevation)
    MapTable_canswe<-cbind(SiteTable_canswe, longitude, latitude, elevation) 
    
    MapTable_canswe <- MapTable_canswe %>%
      dplyr::rename("Site_name" = "site")
    
    longitude = appendvar(var1=SiteTable$Site_name, var2 = coordinates_high_res$Site_name, var3 = coordinates_high_res$Longitude)
    latitude = appendvar(var1=SiteTable$Site_name, var2 = coordinates_high_res$Site_name, var3 = coordinates_high_res$Latitude)
    elevation = appendvar(var1 = SiteTable$Site_name, var2 = nwt_elevation_data$site, nwt_elevation_data$elevation)
    
    MapTable<-cbind(SiteTable, longitude, latitude, elevation)
    MT = rbind(MapTable, MapTable_canswe) %>%
      dplyr::filter(is.na(longitude)==F)
    
    # #filter significant results only
    if (significantonly == T){
      MT <- MT[MT$`p-value` < 0.05, ]
    }
    
    #clip MT to Mackenzie River basin
    # Convert MT results to an sf object
    MT <- sf::st_as_sf(MT, coords = c("longitude", "latitude"), crs = proj)
    
    if(Mack_basin == T) {
      Mack <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/MackenzieRiverBasin_FDA.shp"),
                          layer = "MackenzieRiverBasin_FDA")
      Mack <- sf::st_transform(Mack, sp::CRS(proj)) # Change the projection
      Mack <- sf::st_zm(Mack)
      MT <- sf::st_intersection(MT, Mack)
    } 
    
    #option to save MT for ERA5_snowcomparison_ER.R
    # Extract coordinates and convert back to a regular table
    coords <- sf::st_coordinates(MT)
    
    MT_tbl <- MT %>%
      dplyr::mutate(
        Longitude = coords[, "X"],
        Latitude  = coords[, "Y"]
      ) %>%
      dplyr::select(
        Site_name, Magnitude, `p.value`, Significance,
        `Years.of.Record`, Method, elevation,
        Latitude, Longitude, sitemeanswe
      ) %>%
      sf::st_drop_geometry()
    
    #write.csv(MT_tbl, paste0(user, "/Documents/R_Scripts/Packages/snow/data/MRB_snowsurvey_trends.csv"))
  
    trends <- trend_sf %>%
      dplyr::select(P_value, mm_SWE_per_decade, Bin, geometry)
    
    xy <- sf::st_coordinates(trends)
    
    trends_df <- dplyr::bind_cols(
      sf::st_drop_geometry(trends),
      tibble::tibble(lon = xy[,1], lat = xy[,2])
    ) %>%
      dplyr::filter(is.finite(lon), is.finite(lat))
    
    sig_df <- trends_df %>%
      dplyr::filter(P_value < 0.05)
    
    PerCol_gridded <- leaflet::colorFactor(
      palette  = "RdYlBu",
      domain   = trends_df$Bin,
      na.color = "#BDBDBD"
    )
    
    PerCol_gridded_sig <- leaflet::colorFactor(
      palette  = "RdYlBu",
      domain   = sig_df$Bin,
      na.color = "#BDBDBD"
    )
    
    make_grid_chunks <- function(df, nx = 5, ny = 5) {
      xr <- range(df$lon, finite = TRUE)
      yr <- range(df$lat, finite = TRUE)
      
      x_breaks <- seq(xr[1], xr[2], length.out = nx + 1)
      y_breaks <- seq(yr[1], yr[2], length.out = ny + 1)
      
      df %>%
        mutate(
          xbin = cut(lon, breaks = x_breaks, include.lowest = TRUE, labels = FALSE),
          ybin = cut(lat, breaks = y_breaks, include.lowest = TRUE, labels = FALSE),
          chunk_id = paste0("x", xbin, "_y", ybin)
        ) %>%
        filter(!is.na(chunk_id))  # defensive
    }
    
    add_chunk <- function(map, df_chunk, pal) {
      if (nrow(df_chunk) == 0) return(map)
      
      map %>%
        leaflet::addCircleMarkers(
          data = df_chunk,
          lng = ~lon, lat = ~lat,
          fillColor   = ~PerCol_gridded(Bin),
          fillOpacity = 0.5,
          weight      = 0,            # performance: drop stroke
          radius      = 1.5,
          stroke      = F        # performance: huge win
        )
    }
    
    trends_chunked <- make_grid_chunks(trends_df, nx = 5, ny = 5)
    
    map <- leaflet::leaflet(trends_df, options = leaflet::leafletOptions(preferCanvas = F)) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels) %>%
      leaflet::fitBounds(min(trends_df$lon), min(trends_df$lat),
                         max(trends_df$lon), max(trends_df$lat))
    
    for (id in sort(unique(trends_chunked$chunk_id))) {
      map <- add_chunk(map, dplyr::filter(trends_chunked, chunk_id == id), PerCol_gridded)
    }
    
    map <- map %>%
      leaflet::addCircleMarkers(data = sig_df,
                                lng = ~lon, lat = ~lat,
                                fillColor   = ~PerCol_gridded_sig(Bin),
                                fillOpacity = 1,
                                color = "black",
                                weight      = 1,            
                                radius      = 1.5,
                                stroke      = T) %>%
      leaflet::addScaleBar(position = "bottomright")
    
    #Add NWT border if desired
    if (NWT_border == T){
    NWT <- sf::st_read(paste0(user,
                              "/Documents/R_Scripts/Packages/snow/data/Shapefiles/NWT_ENR_BND_FND.shp"),
                       layer = "NWT_ENR_BND_FND")
    NWT <- sf::st_transform(NWT, sp::CRS(proj)) # Change the projection
    NWT <- sf::st_zm(NWT)
    map <- map  %>%
      leaflet::addPolygons(data = NWT, color = "black", weight = 1, opacity = 0.7, fillOpacity = 0)
    }
    
    #Add Mack basin outline if desired
    if(Mack_basin_outline == T) {
      map <- map %>%
        leaflet::addPolygons(data = Mack, color = "grey", weight = 2, opacity = 1, fillOpacity = 0, group = "NWT Border")
    }
    
    legend_numbers <-  c(-1200, -20, -10, -5, -3, 3, 5, 10, 20, 1200)
    legend_labels <-  c("<(-20)", "(-20) - (-10)", "(-10) - (-5)", "(-5) - (-3)", "(-3) - 3", "3 - 5", "5 - 10", "10 - 20", ">20")
    
    MT$Bin <- cut(MT$Magnitude, 
                  legend_numbers, 
                  include.lowest = T,
                  labels = legend_labels)
    
    PerCol <- leaflet::colorFactor(palette = "RdYlBu", MT$Bin, na.color = NA)
    
    map = map %>%
      leaflet::addCircleMarkers(data = MT, 
                                fillColor = ~PerCol(Bin), #
                                fillOpacity = ifelse(MT$Significance == "yes", 1, 1),
                                label = MT$Site_name,
                                radius = 10,
                                weight = ifelse(MT$Significance == "yes", 2, 2),
                                color = "black",
                                popup =~ paste0("Site name: ", MT$Site_name, "<br>",
                                                "Years of data between ", paste(start_year), "-", paste(end_year), ": ", MT$Years.of.Record, "<br>",
                                                "Magnitude of change: ", round(MT$Magnitude, 2), " mm SWE/decade", "<br>",
                                                "p value: ", MT$p.value, "<br>",
                                                "Method: ", MT$Method, "<br>",
                                                "Elevation: ", MT$elevation)) #%>%
      
     #Legend commented out for recreation in CorelDraw
      # leaflet::addLegend(
      #   position = "bottomleft",
      #   pal = PerCol,
      #   values = MT$Bin,
      #   title = paste0("Change in SWE", "<br>",
      #                  "(mm/decade)"))
    
    
    
  
  
  
  map
  
  if(save == T){
    Sys.setenv(PATH = paste0(user, "/Documents/Modelling/phantomjs/phantomjs/bin"))
    htmlwidgets::saveWidget(map, file = paste0(savepath, "/swemap.html"), selfcontained = TRUE)
    webshot::webshot(paste0(savepath, "/swemap.html"), 
                     paste0(savepath, "/swemap_", ".png"), 
                     delay = 8, vwidth = 1000, vheight = 1050, zoom = 3)
  }
  

# Create snow site map - Figure 1
map <- leaflet::leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels)

#Add basin outlines 

#bring in other basin shapefiles

#Yellowknife river basin
YK <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/07SB002_DrainageBasin_BassinDeDrainage.shp"),
                  layer = "07SB002_DrainageBasin_BassinDeDrainage")
YK <- sf::st_transform(YK, proj) # Change the projection
YK <- sf::st_zm(YK)

#Snare river basin

Snare <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/07SA003_DrainageBasin_BassinDeDrainage.shp"),
                     layer = "07SA003_DrainageBasin_BassinDeDrainage")
Snare <- sf::st_transform(Snare, proj) # Change the projection
Snare <- sf::st_zm(Snare)

#Taltson

Taltson <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/07QA001_DrainageBasin_BassinDeDrainage.shp"),
                       layer = "07QA001_DrainageBasin_BassinDeDrainage")
Taltson <- sf::st_transform(Taltson, proj) # Change the projection
Taltson <- sf::st_zm(Taltson)

#Peace

Peace <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/07KC005_DrainageBasin_BassinDeDrainage.shp"),
                     layer = "07KC005_DrainageBasin_BassinDeDrainage")
Peace <- sf::st_transform(Peace, proj) # Change the projection
Peace <- sf::st_zm(Peace)
Peace <- sf::st_make_valid(Peace)

#Athabasca 

AB <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/07DD010_DrainageBasin_BassinDeDrainage.shp"),
                  layer = "07DD010_DrainageBasin_BassinDeDrainage")
AB <- sf::st_transform(AB, proj) # Change the projection
AB <- sf::st_zm(AB)
AB <- sf::st_make_valid(AB)

#Liard

Liard <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/10ED002_DrainageBasin_BassinDeDrainage.shp"),
                     layer = "10ED002_DrainageBasin_BassinDeDrainage")
Liard <- sf::st_transform(Liard, proj) # Change the projection
Liard <- sf::st_zm(Liard)

#Peel

Peel <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/10MC022_DrainageBasin_BassinDeDrainage.shp"),
                    layer = "10MC022_DrainageBasin_BassinDeDrainage")
Peel <- sf::st_transform(Peel, proj) # Change the projection
Peel <- sf::st_zm(Peel)

#Hay

Hay <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/07OB001_DrainageBasin_BassinDeDrainage.shp"),
                   layer = "07OB001_DrainageBasin_BassinDeDrainage")
Hay <- sf::st_transform(Hay, proj) # Change the projection
Hay <- sf::st_zm(Hay)

#Lower Slave

lower_slave <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/lower_slave_clean.shp"),
                   layer = "lower_slave_clean")
lower_slave <- sf::st_transform(lower_slave, proj) # Change the projection
lower_slave <- sf::st_zm(lower_slave)


#MT <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/MRB_snowsurvey_trends.csv"))
# MT <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/pts_basin.csv")) %>%
#   dplyr::rename("Latitude"="MT_Latitude",
#                 "Longitude"="MT_Longitude")

map = map %>%
  leaflet::addPolygons(data = Hay,
                       color = "darkgrey",
                       fillColor = "lightgrey" ) %>%
  leaflet::addPolygons(data = Snare,
                       color = "darkgrey",
                       fillColor = "lightgrey") %>%
  leaflet::addPolygons(data = YK,
                       color = "darkgrey",
                       fillColor = "lightgrey") %>%
  leaflet::addPolygons(data = lower_slave,
                       color = "darkgrey",
                       fillColor = "lightgrey") %>%
  leaflet::addPolygons(data = Peel, 
                       color = "darkgrey",
                       fillColor = "lightgrey") %>%
  leaflet::addPolygons(data = Liard, 
                       color = "darkgrey",
                       fillColor = "lightgrey") %>%
  leaflet::addPolygons(data = AB, 
                       color = "darkgrey",
                       fillColor = "lightgrey") %>%
  leaflet::addPolygons(data = Peace, 
                       color = "darkgrey",
                       fillColor = "lightgrey") %>%
  leaflet::addPolygons(data = Taltson, 
                       color = "darkgrey",
                       fillColor = "lightgrey")

#Add NWT border if desired
if (NWT_border == T){
  NWT <- sf::st_read(paste0("C:/Users/",
                            tolower(Sys.getenv("USERNAME")),
                            "/Documents/R_Scripts/Packages/snow/data/Shapefiles/NWT_ENR_BND_FND.shp"),
                     layer = "NWT_ENR_BND_FND")
  NWT <- sf::st_transform(NWT, sp::CRS(proj)) # Change the projection
  NWT <- sf::st_zm(NWT)
  map <- map  %>%
    leaflet::addPolygons(data = NWT, color = "black", weight = 1, opacity = 0.7, fillOpacity = 0, stroke = T )
}

#Add Mack basin outline if desired
if(Mack_basin_outline == T) {
  map <- map %>%
    leaflet::addPolygons(data = Mack, color = "black", weight = 2, opacity = 1, fillOpacity = 0, group = "NWT Border", stroke = T)
}

#Add colouring for basins - same as ggplot Figure 3
basin_cols <- c(
  "Athabasca"      = "#F8766D", 
  "Great Slave Lake - other"    = "#D89000",  
  "Hay"            = "#B79F00",  
  "Liard"          = "#7CAE00",  
  "Lower Slave"    = "#00BA38", 
  "Mackenzie River main stem"= "#00BFC4",  
  "Peace"          = "#00C1DE",  
  "Peel"           = "#00B0F6",  
  "Snare"          = "#9590FF",  
  "Taltson"        = "#E76BF3", 
  "Yellowknife"    = "#FF61C3"   
)

pal_basin <- leaflet::colorFactor(
  palette  = basin_cols,
  domain   = names(basin_cols),
  na.color = "#BDBDBD"
)

map = map %>%
  leaflet::addCircleMarkers(data = MT, 
                            fillColor = ~pal_basin(basin), #
                            lat = MT$Latitude,
                            lng = MT$Longitude,
                            fillOpacity = ifelse(MT$Significance == "yes", 1, 1),
                            label = MT$Site_name,
                            radius = 7,
                            weight = ifelse(MT$Significance == "yes", 1, 1),
                            color = "black",
                            popup =~ paste0("Site name: ", MT$Site_name, "<br>",
                                            "Years of data between ", paste(start_year), "-", paste(end_year), ": ", MT$Years.of.Record, "<br>",
                                            "Magnitude of change: ", round(MT$Magnitude, 2), " mm SWE/decade", "<br>",
                                            "p value: ", MT$p.value, "<br>",
                                            "Method: ", MT$Method, "<br>",
                                            "Latitude: ", MT$Latitude, "<br>",
                                            "Longitude: ", MT$Longitude, "<br>",
                                            "Elevation: ", MT$elevation)) %>%
  leaflet::addScaleBar(position = "bottomright")

map 

if(save == T){
  Sys.setenv(PATH = paste0(user, "/Documents/Modelling/phantomjs/phantomjs/bin"))
  htmlwidgets::saveWidget(map, file = paste0(savepath, "/swesitemap.html"), selfcontained = TRUE)
  webshot::webshot(paste0(savepath, "/swesitemap.html"), 
                   paste0(savepath, "/swesitemap_", ".png"), 
                   delay = 5, vwidth = 1000, vheight = 1000, zoom = 3)
}


##################################################################################################