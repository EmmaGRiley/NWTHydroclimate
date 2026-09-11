#ERA5-land and manual SS
#testing SWE bias

library(dplyr)
library(sf)

# Call in source functions - provides haversine_dist() and appendvar()
source(paste0("C:/Users/", tolower(Sys.getenv("USERNAME")),
              "/Documents/R_Scripts/Packages/snow/R/ERA5_snowtrends_map_hourly_functions.R"))

# Specify pathways
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")))
savepath <- paste0(user, "/Documents/R_Scripts/Packages/snow/data/")

# Define operators
`%>%` <- magrittr::`%>%`
'%!in%' = Negate('%in%')

#data parameters
# NOTE `data <- md_3` moved below, md_3 is not loaded until further down
start_year <- 1995
end_year <- 2025
min_year <- abs(start_year - end_year) * 0.75
exclude_sites <- NA
act <- c("IA", "A")
surface <- c("upland")
flags <- c("Y", "Sk", "P", "Sk_2")
hdensity <- 0.34
ldensity <- 0.1

# --- analysis scope ----------------------------------------------------------
# "NWT" = clip to the NWT boundary, GNWT manual surveys only (md_3)
# "MRB" = clip to the Mackenzie River Basin, md_3 plus non-GNWT CanSWE sites,
#         matching the original scope of ERA5_snowcomparison_ER.R
scope <- "NWT"

# CanSWE sites are only needed to fill in the MRB outside the NWT
include_canswe <- (scope == "MRB")

# drop any site whose nearest ERA5 cell is further away than this
max_match_km <- 50

# sites excluded from the trend analysis, kept out here too for consistency
map_exclude_sites <- c("FORT SMITH A", "NORMAN WELLS A")

shp_path <- paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/")

# ERA5-Land grid: the two extracts cover different extents, so the scope has to
# pick the matching one.
#   ERA5_updatedfeb_may.csv     lat 52.2 - 69.7  covers the MRB
#   ERA5_updatedfeb_mayNWT.csv  lat 60.0 - 78.7  covers the NWT including the islands
era5_file <- if (scope == "MRB") "ERA5_updatedfeb_may.csv" else "ERA5_updatedfeb_mayNWT.csv"

#statial parameters
proj <- '+proj=longlat +datum=WGS84'
crs_proj <- 3347  # NAD83 / Statistics Canada Lambert (projected, meters)

# Load ERA5-Land snow data
# must run ERA5_spring_snow_analysis.R first to create the spring max snow depth dataset

ERA5_snow <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/SWEmax_dataset/", era5_file)) %>%
  dplyr::select(-c(X))

ERA5_snow <- ERA5_snow %>%
  dplyr::select(Latitude, Longitude, sd_updated, year) %>%
  dplyr::rename("sd" = "sd_updated",
                "sd_year" = "year")

#filter for NA values for sd 
ERA5_snow <- na.omit(ERA5_snow)

#manual SS snow
md_3 <- readRDS(paste0(user,"/Documents/R_Scripts/Packages/snow/data/md_3.rds"))

sites <- readRDS(paste0(user,"/Documents/R_Scripts/Packages/snow/data/sites.rds"))

data <- md_3

# Non-GNWT CanSWE sites, used to fill out the MRB beyond the NWT boundary.
# type_mes == 0 keeps multi-point snow surveys only, as in ERA5_snowtrends_map_hourly.R
canswe <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/", "CanSWE_v7.csv")) %>%
  dplyr::rename("site" = "station_name") %>%
  dplyr::filter(type_mes == 0,
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

#optional filter for nwt sites only
if(scope == "NWT"){
  coordinates_high_res <- coordinates_high_res[c(1:50), ]}

#create table
#manual SS 
sitename = unique(data$site[!(data$site%in% exclude_sites)])
table <- data.frame()

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

table <- rbind(table, TA)
}

#append lat/long/elevation
sites <- sites %>%
  dplyr::rename("Latitude" = "lat",
                "Longitude" = "lng",
                "Site_name" = "site_name") %>%
  dplyr::select("Site_name", "Latitude", "Longitude")

coordinates_sites <- dplyr::rows_update(sites, coordinates_high_res)
longitude = appendvar(var1=table$Site_name, var2 = coordinates_sites$Site_name, var3 = coordinates_sites$Longitude)
latitude = appendvar(var1=table$Site_name, var2 = coordinates_sites$Site_name, var3 = coordinates_sites$Latitude)
elevation = appendvar(var1 = table$Site_name, var2 = nwt_elevation_data$site, nwt_elevation_data$elevation)
table<-cbind(table, longitude, latitude, elevation)

# -----------------------------------------------------------------------------
# Scope: add CanSWE sites for the MRB, then clip to the scope boundary
# -----------------------------------------------------------------------------
# CanSWE carries no density / depth / flag fields, so those columns come through
# as NA. The NWT half of an MRB run is still quality filtered by the loop above,
# the CanSWE half is not - worth remembering when reading a mixed-scope result.

if (include_canswe) {
  table_canswe <- canswe %>%
    dplyr::filter(year >= start_year, year <= end_year, !is.na(swe_cm)) %>%
    dplyr::group_by(Site_name = site, year) %>%
    dplyr::summarise(
      meanswe   = mean(swe_cm, na.rm = TRUE),
      longitude = dplyr::first(Longitude),
      latitude  = dplyr::first(Latitude),
      elevation = dplyr::first(elevation),
      .groups   = "drop"
    ) %>%
    dplyr::group_by(Site_name) %>%
    dplyr::filter(dplyr::n_distinct(year) > 3) %>%
    dplyr::mutate(sitemeanswe = mean(meanswe, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      surface_type = NA_character_,
      meandensity  = NA_real_,
      meandepth    = NA_real_
    )

  table <- dplyr::bind_rows(table, table_canswe)
}

table <- table %>%
  dplyr::filter(!is.na(longitude), !is.na(latitude),
                Site_name %!in% map_exclude_sites)

# clip to the NWT boundary or the Mackenzie River Basin
scope_layer <- if (scope == "MRB") "MackenzieRiverBasin_FDA" else "NWT_ENR_BND_FND"

scope_poly <- sf::st_read(paste0(shp_path, scope_layer, ".shp"), layer = scope_layer) %>%
  sf::st_transform(proj) %>%
  sf::st_zm(drop = TRUE, what = "ZM") %>%
  sf::st_make_valid()

table_sf <- sf::st_as_sf(table, coords = c("longitude", "latitude"),
                         crs = proj, remove = FALSE)

n_before <- dplyr::n_distinct(table$Site_name)

table <- table_sf %>%
  sf::st_filter(scope_poly, .predicate = sf::st_intersects) %>%
  sf::st_drop_geometry()

cat("\nScope:", scope, "- sites retained:", dplyr::n_distinct(table$Site_name),
    "of", n_before, "\n")

# -----------------------------------------------------------------------------
# Match each site to its nearest ERA5-Land cell
# -----------------------------------------------------------------------------
# Matched ONCE per site, then joined by year, so a site cannot drift between
# cells from one year to the next. Also avoids the many-to-many join, which for
# the NWT grid (844k rows) would build a table of tens of millions of rows.

era5_cells <- ERA5_snow %>%
  dplyr::distinct(Latitude, Longitude)

site_pts <- table %>%
  dplyr::distinct(Site_name, longitude, latitude)

cells_sf <- sf::st_as_sf(era5_cells, coords = c("Longitude", "Latitude"),
                         crs = proj, remove = FALSE)
sites_sf <- sf::st_as_sf(site_pts, coords = c("longitude", "latitude"),
                         crs = proj, remove = FALSE)

nn <- sf::st_nearest_feature(sites_sf, cells_sf)

site_cells <- site_pts %>%
  dplyr::mutate(
    Latitude  = era5_cells$Latitude[nn],
    Longitude = era5_cells$Longitude[nn]
  ) %>%
  dplyr::mutate(distance = haversine_dist(Latitude, Longitude, latitude, longitude))

# guard against sites sitting outside the ERA5 extent being matched to a cell
# hundreds of km away
far <- site_cells %>% dplyr::filter(distance > max_match_km)
if (nrow(far) > 0) {
  cat("\nDropped", nrow(far), "site(s) with no ERA5 cell within",
      max_match_km, "km:\n")
  print(far %>% dplyr::select(Site_name, latitude, longitude, distance))
}
site_cells <- site_cells %>% dplyr::filter(distance <= max_match_km)

matched_data <- table %>%
  dplyr::inner_join(site_cells, by = c("Site_name", "longitude", "latitude")) %>%
  dplyr::inner_join(ERA5_snow,
                    by = dplyr::join_by(Latitude, Longitude, year == sd_year))

#bring in basin and ecoregion as well

#summarize stats by basin 
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

#lower slave

lower_slave_clean <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/lower_slave_clean.shp"),
                                 layer = "lower_slave_clean")
lower_slave_clean <- sf::st_transform(lower_slave_clean, proj)
lower_slave_clean <- sf::st_make_valid(lower_slave_clean) 

matched_data_sf <- sf::st_as_sf(
  matched_data,
  coords = c("longitude", "latitude"),
  crs = proj,
  remove = FALSE
)


basins_sf <- dplyr::bind_rows(
  as_basin_sf(Peace,            "Peace"),
  as_basin_sf(AB,               "Athabasca"),
  as_basin_sf(YK,               "Yellowknife"),
  as_basin_sf(Snare,            "Snare"),
  as_basin_sf(Taltson,          "Taltson"),
  as_basin_sf(Liard,            "Liard"),
  as_basin_sf(Peel,             "Peel"),
  as_basin_sf(lower_slave_clean,"Lower Slave"),
  as_basin_sf(Hay, "Hay")
)


if (sf::st_crs(basins_sf) != sf::st_crs(matched_data_sf)) {
  matched_data_sf <- sf::st_transform(matched_data_sf, sf::st_crs(basins_sf))
}

matched_data_sf_basin <- sf::st_join(matched_data_sf, basins_sf, join = sf::st_within, left = TRUE)

# If any points fall just outside boundaries (common), assign nearest basin
missing <- is.na(matched_data_sf_basin$basin)
max_km <- 50

# Ensure diagnostics columns exist
matched_data_sf_basin$dist_to_basin_m <- NA_real_
matched_data_sf_basin$was_missing     <- FALSE
matched_data_sf_basin$assigned_nearest <- FALSE

if (any(missing)) {
  matched_data_sf_miss <- matched_data_sf[missing, , drop = FALSE]
  
  # nearest polygon index (planar selection; OK because we verify with geodesic distance)
  nn <- sf::st_nearest_feature(matched_data_sf_miss, basins_sf)
  
  # geodesic distance (meters) in lon/lat CRS
  d <- sf::st_distance(matched_data_sf_miss, basins_sf[nn, ], by_element = TRUE)
  
  ok <- as.numeric(d) <= (max_km * 1000)
  
  # nn indexes basins_sf, not matched_data_sf_basin - take the label from the polygons
  matched_data_sf_basin$basin[missing] <- ifelse(ok, basins_sf$basin[nn], NA_character_)
  matched_data_sf_basin$dist_to_basin_m[missing] <- as.numeric(d)
  matched_data_sf_basin$was_missing[missing] <- TRUE
  matched_data_sf_basin$assigned_nearest[missing] <- ok
  
  # keep indices for mapping
  matched_data_sf_basin$nn_basin_i <- NA_integer_
  matched_data_sf_basin$nn_basin_i[missing] <- nn
} else {
  matched_data_sf_basin$nn_basin_i <- NA_integer_
}

#assign missing points to more general groupings
#Mackenzie River main stem
MRBsites <- c("Tulita", "NORMAN WELLS A", "Norman Wells", "Fort Good Hope", "Caribou Creek", "Trout Lake_f", "Colville Lake", "Wrigley_f", "Fort Providence", "Deline", "Kimble Tower", "Ndulee Crossing", "Inuvik A")
GSL_othsites <- c("Nyarling River", "Kakisa River", "Pine Point", "Thubun Lake", "Crown Fire", "Mosquito Creek")
matched_data_sf_basin$basin <- ifelse(is.na(matched_data_sf_basin$basin == T)&matched_data_sf_basin$Site_name %in%MRBsites, "Mackenzie River main stem", matched_data_sf_basin$basin)
matched_data_sf_basin$basin <- ifelse(is.na(matched_data_sf_basin$basin == T)&matched_data_sf_basin$Site_name %in%GSL_othsites, "Great Slave Lake - other", matched_data_sf_basin$basin)

matched_data_basin <- matched_data_sf_basin %>%
  sf::st_drop_geometry()

#variability/bias test
#signed: positive = ERA5-Land reads higher than the manual survey
matched_data_basin["diff"] <- (matched_data_basin$sd*100 - matched_data_basin$meanswe)
matched_data_basin["absdiff"] <- abs(matched_data_basin$sd*100 - matched_data_basin$meanswe)

#group bias based on basin and year

matched_data_grouped <- matched_data_basin %>%
  dplyr::group_by(basin, year) %>%
  dplyr::mutate(n_sites = n_distinct(Site_name)) %>%
  dplyr::filter(n_sites>3) %>%
  dplyr::mutate(meanbias = mean(diff, na.rm=T),
                meanswebasin = mean(meanswe, na.rm=T)) %>%
  dplyr::mutate(percent_bias = meanbias/meanswebasin)

#plotting bias based on basin and year
plot <- ggplot2::ggplot(matched_data_grouped, ggplot2::aes(x = meanswe, y = diff)) +
  # ggplot2::geom_smooth(method = "lm", se = TRUE, linetype = "solid",
  #                      color = "black", linewidth = 1, fill = "lightgrey", alpha = 0.6) +
  # ggplot2::geom_smooth(data = dplyr::filter(dt_plot, group == "NWT"), method = "lm", se = TRUE,
  #                      color = "dodgerblue4", fill = "dodgerblue3",
  #                      linewidth = 1.2, alpha = 0.35, size = 7) +
  ggplot2::geom_point(data = matched_data_grouped, ggplot2::aes(fill = basin, color = basin),
                      size = 7, shape = 21, 
                      stroke =  1, 
                      alpha = 0.5) +
  # ggplot2::geom_point(data = dplyr::filter(dt_plot, group == "NWT"), ggplot2::aes(fill = basin),
  #                     size = 7, shape = 21, color = "black", 
  #                     stroke =  1, 
  #                     alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::labs(
    title = "",
    x = "year",
    y = "difference between manual snow survey SWE and ERA5-Land SWE",
    fill = "Basin"
  )

plot

#look at correlation between bias and magnitude
basinsel <- "Liard"

corr_bias <- stats::cor.test(
  matched_data_grouped$diff[matched_data_grouped$basin == basinsel],
  matched_data_grouped$meanswe[matched_data_grouped$basin == basinsel],
  method = "spearman"
)

corr_bias


# =============================================================================
# DIRECTIONAL BIAS OVER TIME, GROUPED BY BASIN AND ECOZONE
# Mirrors the basin/ecozone structure of ERA5_snowcomparison_ER.R, but on
# per-year signed bias rather than on trends.
# =============================================================================

library(ggplot2)

# minimum sites required before a region-year mean is reported
min_sites <- 3
# minimum years required before a region gets its own statistics
min_years <- 5

# -----------------------------------------------------------------------------
# 1) Bias columns
# -----------------------------------------------------------------------------
# bias is SIGNED throughout: positive = ERA5-Land reads higher than the survey.
# bias_anom removes each site's own long-run mean bias, so a region-year mean
# is not driven by which sites happened to be surveyed that year.

matched_data_basin <- matched_data_basin %>%
  dplyr::mutate(
    swe_era = sd * 100,                 # ERA5-Land m w.e. -> cm, matches swe_cm
    swe_obs = meanswe,
    bias    = swe_era - swe_obs,
    abs_err = abs(bias)
  ) %>%
  dplyr::group_by(Site_name) %>%
  dplyr::mutate(
    site_n_years   = dplyr::n(),
    site_mean_bias = mean(bias, na.rm = TRUE),
    bias_anom      = bias - site_mean_bias
  ) %>%
  dplyr::ungroup()

# -----------------------------------------------------------------------------
# 2) Attach ecozone and ecoregion
#    layer/col options: "mackenzie_ecozonesv2" & "ECOZONE_NA"
#                       "mackenzie_ecoregions" & "ECOREGION1"
#                       "mackenzie_ecoprovinces" & "ECOPROVI_1"
# -----------------------------------------------------------------------------

ecozones <- sf::st_read(paste0(shp_path, "mackenzie_ecozonesv2.shp")) %>%
  sf::st_transform(proj)

ecoregions <- sf::st_read(paste0(shp_path, "mackenzie_ecoregions.shp")) %>%
  sf::st_transform(proj)

pts_eco <- as_points_sf(matched_data_basin,
                        lon_col = "longitude",
                        lat_col = "latitude",
                        crs = proj)

pts_eco <- attach_region(pts_eco, ecozones, "ECOZONE_NA",
                         max_km = 50, region_out_col = "ecozone")
pts_eco$dist_to_region_m <- NULL   # drop so the second call can add its own

pts_eco <- attach_region(pts_eco, ecoregions, "ECOREGION1",
                         max_km = 50, region_out_col = "ecoregion")

bias_data <- pts_eco %>%
  sf::st_drop_geometry()

# coverage check - anything unlabelled will silently vanish from the summaries
cat("\nUnassigned basin  :", sum(is.na(bias_data$basin)), "rows\n")
cat("Unassigned ecozone:", sum(is.na(bias_data$ecozone)), "rows\n")
print(sort(unique(bias_data$ecozone)))

# -----------------------------------------------------------------------------
# 3) Stack basin and ecozone so one pipeline serves both groupings
# -----------------------------------------------------------------------------

bias_long <- dplyr::bind_rows(
  bias_data %>% dplyr::mutate(region_type = "Basin",     region = basin),
  bias_data %>% dplyr::mutate(region_type = "Ecozone",   region = ecozone),
  bias_data %>% dplyr::mutate(region_type = "Ecoregion", region = ecoregion)
) %>%
  dplyr::filter(!is.na(region), !is.na(bias))

# -----------------------------------------------------------------------------
# 4) Region-by-year bias
# -----------------------------------------------------------------------------

region_year <- bias_long %>%
  dplyr::group_by(region_type, region, year) %>%
  dplyr::summarise(
    n_sites        = dplyr::n_distinct(Site_name),
    mean_bias      = mean(bias, na.rm = TRUE),        # raw signed bias
    mean_bias_anom = mean(bias_anom, na.rm = TRUE),   # composition-controlled
    median_bias    = stats::median(bias, na.rm = TRUE),
    mean_abs_err   = mean(abs_err, na.rm = TRUE),
    rmse           = sqrt(mean(bias^2, na.rm = TRUE)),
    mean_obs       = mean(swe_obs, na.rm = TRUE),
    mean_era       = mean(swe_era, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(percent_bias = 100 * mean_bias / mean_obs) %>%
  dplyr::filter(n_sites >= min_sites)

# -----------------------------------------------------------------------------
# 5) Per-region statistics
#    - is the bias different from zero across years?          (Wilcoxon)
#    - is the bias drifting over time?                        (lm + Mann-Kendall)
#    - does the bias scale with how much snow there was?      (Spearman)
# -----------------------------------------------------------------------------

region_stats <- region_year %>%
  dplyr::group_by(region_type, region) %>%
  dplyr::filter(dplyr::n() >= min_years) %>%
  dplyr::group_modify(function(.x, .y) {

    .x <- dplyr::arrange(.x, year)

    w   <- stats::wilcox.test(.x$mean_bias, mu = 0,
                              alternative = "two.sided", exact = FALSE)
    fit <- stats::lm(mean_bias ~ year, data = .x)
    sl  <- broom::tidy(fit) %>% dplyr::filter(term == "year")
    mk  <- Kendall::MannKendall(.x$mean_bias)
    ct  <- stats::cor.test(.x$mean_bias, .x$mean_obs,
                           method = "spearman", exact = FALSE)

    dplyr::tibble(
      n_years        = nrow(.x),
      mean_bias      = mean(.x$mean_bias),
      median_bias    = stats::median(.x$mean_bias),
      percent_bias   = 100 * sum(.x$mean_bias) / sum(.x$mean_obs),
      bias_vs0_p     = w$p.value,                 # bias differs from zero?
      bias_trend     = sl$estimate,               # cm per year
      bias_trend_p   = sl$p.value,
      mk_tau         = as.numeric(mk$tau),
      mk_p           = as.numeric(mk$sl),
      rho_bias_swe   = unname(ct$estimate),       # bias vs snow amount
      rho_bias_swe_p = ct$p.value
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(region_type, dplyr::desc(abs(mean_bias)))

print(region_stats, n = 50)

# per-site bias, to see which sites drive a region
site_bias <- bias_data %>%
  dplyr::group_by(basin, ecozone, Site_name) %>%
  dplyr::summarise(
    n_years   = dplyr::n(),
    mean_bias = mean(bias, na.rm = TRUE),
    mean_obs  = mean(swe_obs, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  dplyr::mutate(percent_bias = 100 * mean_bias / mean_obs) %>%
  dplyr::arrange(dplyr::desc(abs(mean_bias)))

# -----------------------------------------------------------------------------
# 6) Plots
# -----------------------------------------------------------------------------

# helper so basin / ecozone / ecoregion all plot the same way
plot_bias_series <- function(rt, anomaly = FALSE) {
  d <- dplyr::filter(region_year, region_type == rt)
  yv <- if (anomaly) "mean_bias_anom" else "mean_bias"
  ylab <- if (anomaly) {
    "Bias anomaly (cm SWE, site means removed)"
  } else {
    "ERA5-Land - manual survey SWE (cm)"
  }

  ggplot2::ggplot(d, ggplot2::aes(x = year, y = .data[[yv]], colour = region)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.6) +
    ggplot2::geom_line(linewidth = 0.7, alpha = 0.85) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ region) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = NULL, y = ylab, title = paste0("Directional SWE bias by ", rt))
}

p_bias_basin_ts <- plot_bias_series("Basin")
p_bias_eco_ts   <- plot_bias_series("Ecozone")
p_bias_basin_ts
p_bias_eco_ts

# distribution of the signed bias per region
plot_bias_box <- function(rt) {
  d <- dplyr::filter(bias_long, region_type == rt)

  ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(region, bias, FUN = stats::median),
                                  y = bias, fill = region)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.6) +
    ggplot2::geom_boxplot(alpha = 0.6, outlier.size = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = rt, y = "ERA5-Land - manual survey SWE (cm)",
                  title = paste0("Directional SWE bias by ", rt))
}

p_bias_basin_box <- plot_bias_box("Basin")
p_bias_eco_box   <- plot_bias_box("Ecozone")
p_bias_basin_box
p_bias_eco_box

# does the bias scale with snow amount, by region
p_bias_vs_swe <- ggplot2::ggplot(
  dplyr::filter(region_year, region_type == "Basin"),
  ggplot2::aes(x = mean_obs, y = mean_bias, fill = region)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey30") +
  ggplot2::geom_smooth(method = "lm", se = TRUE, colour = "black",
                       fill = "lightgrey", linewidth = 0.8, alpha = 0.5) +
  ggplot2::geom_point(shape = 21, size = 4, stroke = 0.8, alpha = 0.75) +
  ggplot2::theme_classic() +
  ggplot2::labs(x = "Manual survey mean SWE (cm)",
                y = "ERA5-Land - manual survey SWE (cm)",
                fill = "Basin")
p_bias_vs_swe

# ggplot2::ggsave("p_bias_basin_ts.png",  p_bias_basin_ts,  dpi = 900, path = savepath)
# ggplot2::ggsave("p_bias_eco_ts.png",    p_bias_eco_ts,    dpi = 900, path = savepath)
# ggplot2::ggsave("p_bias_basin_box.png", p_bias_basin_box, dpi = 900, path = savepath)
# ggplot2::ggsave("p_bias_eco_box.png",   p_bias_eco_box,   dpi = 900, path = savepath)
