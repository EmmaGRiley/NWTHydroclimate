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
# a site needs more than this many years of record to be used, matching the
# threshold in ERA5_snowtrends_map_hourly.R so the bias and trend analyses run
# on the same set of sites. 1995-2025 -> 22.5, so effectively 23 years.
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
#
# WHICH BASINS EACH SCOPE CAN SPEAK TO. Several basins only clip the NWT
# boundary, so an NWT run keeps a couple of their sites and those sites are not
# representative of the basin. Once min_year drops the short records, they fall
# below min_sites and leave the tables on their own - that is correct, not a
# bug, and they should be read from an MRB run instead:
#   NWT-scope basins  Snare, Taltson, Yellowknife, Mackenzie River main stem,
#                     Lower Slave, Great Slave Lake - other
#   MRB-scope only    Peace, Athabasca, Liard, Hay, Peel
scope <- "MRB"

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

#nwt sites only
coordinates_high_res <- coordinates_high_res[c(1:50), ]

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
  dplyr::filter(dplyr::n_distinct(year) > min_year)%>%
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
    dplyr::filter(dplyr::n_distinct(year) > min_year) %>%
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

# -----------------------------------------------------------------------------
# Drop lake-contaminated cells and re-match to the nearest real cell
# -----------------------------------------------------------------------------
# ERA5-Land cells that overlap a large lake carry artificially low SWE, and a
# survey site beside a big lake gets matched straight into one. The signature is
# a cell far drier than everything around it: Nonacho Lake matched a cell
# holding 6.6 mm against a neighbourhood median of 114 mm, a 94% deficit that
# is water, not snow. Thekulthili Lake, Deline, Thubun Lake and Little Buffalo
# Tower are the others in the NWT grid.
#
# The site itself is on land, so this is a MATCHING error rather than a model
# error - hence re-match rather than drop the site. A cell is called bad when
# its long-run mean SWE is below lake_ratio_cut of the median over the ring
# around it; the real distribution is tight (median 1.00, quartiles 0.99-1.02)
# with a small hard tail, so the cut lands in an empty gap rather than slicing
# through a continuum. About 3-4% of cells fail.
#
# Re-run this per dataset. The bad cells are not the same in the SWEmax and the
# hourly trend grids - Thubun and Little Buffalo are contaminated in one and
# clean in the other - so a bad-cell list cannot be carried between them.

lake_ratio_cut <- 0.8    # cell mean SWE / neighbourhood median, below = bad
ring_inner_km  <- 8      # ignore the cell itself and its immediate neighbours
ring_outer_km  <- 30

cell_means <- ERA5_snow %>%
  dplyr::group_by(Latitude, Longitude) %>%
  dplyr::summarise(cell_swe = mean(sd, na.rm = TRUE), .groups = "drop")

# neighbourhood median for every cell, on the regular 0.1 degree lattice.
# Indexed rather than distance-matrixed - 26k cells would be 700M distances.
step  <- 0.1
ilon  <- round((cell_means$Longitude - min(cell_means$Longitude)) / step) + 1
ilat  <- round((cell_means$Latitude  - min(cell_means$Latitude )) / step) + 1
M     <- matrix(NA_real_, max(ilon), max(ilat))
M[cbind(ilon, ilat)] <- cell_means$cell_swe

# ring radius in cell counts; longitude degrees shrink with latitude
mid_lat  <- mean(cell_means$Latitude)
km_lon   <- 111 * cos(mid_lat * pi / 180)
r_lat    <- ceiling(ring_outer_km / 111 / step)
r_lon    <- ceiling(ring_outer_km / km_lon / step)
r_lat_in <- floor(ring_inner_km / 111 / step)
r_lon_in <- floor(ring_inner_km / km_lon / step)

# The cell and its immediate neighbours are held out of their own reference
# median. Leaving them in drags the median toward the cell being tested, which
# hides exactly the case this is looking for - a cluster of lake cells, where
# each one makes the others look normal.
cell_means$ring_swe <- vapply(seq_len(nrow(cell_means)), function(k) {
  xs <- max(1, ilon[k] - r_lon):min(nrow(M), ilon[k] + r_lon)
  ys <- max(1, ilat[k] - r_lat):min(ncol(M), ilat[k] + r_lat)
  v  <- M[xs, ys, drop = FALSE]
  inner <- outer(abs(xs - ilon[k]) <= r_lon_in,
                 abs(ys - ilat[k]) <= r_lat_in, "&")
  v  <- v[!inner]
  v  <- v[is.finite(v)]
  if (length(v) < 8) NA_real_ else stats::median(v)
}, numeric(1))

cell_means <- cell_means %>%
  dplyr::mutate(
    lake_ratio = cell_swe / ring_swe,
    bad_cell   = is.finite(lake_ratio) & lake_ratio < lake_ratio_cut
  )

cat("\nLake-contaminated cells:", sum(cell_means$bad_cell), "of",
    nrow(cell_means), sprintf("(%.1f%%)\n", 100 * mean(cell_means$bad_cell)))

# re-match any site sitting on a bad cell to its nearest good cell
good_cells <- cell_means %>% dplyr::filter(!bad_cell)

site_cells <- site_cells %>%
  dplyr::left_join(dplyr::select(cell_means, Latitude, Longitude,
                                 lake_ratio, bad_cell),
                   by = c("Latitude", "Longitude"))

if (any(site_cells$bad_cell, na.rm = TRUE)) {

  hit <- which(site_cells$bad_cell)

  bad_sf  <- sf::st_as_sf(site_cells[hit, ], coords = c("longitude", "latitude"),
                          crs = proj, remove = FALSE)
  good_sf <- sf::st_as_sf(good_cells, coords = c("Longitude", "Latitude"),
                          crs = proj, remove = FALSE)
  gi <- sf::st_nearest_feature(bad_sf, good_sf)

  before <- site_cells[hit, ]

  site_cells$Latitude[hit]   <- good_cells$Latitude[gi]
  site_cells$Longitude[hit]  <- good_cells$Longitude[gi]
  site_cells$lake_ratio[hit] <- good_cells$lake_ratio[gi]
  site_cells$bad_cell[hit]   <- FALSE
  site_cells$distance[hit]   <- haversine_dist(site_cells$Latitude[hit],
                                               site_cells$Longitude[hit],
                                               site_cells$latitude[hit],
                                               site_cells$longitude[hit])
  site_cells$rematched       <- FALSE
  site_cells$rematched[hit]  <- TRUE

  cat("\nRe-matched", length(hit), "site(s) off a lake-contaminated cell:\n")
  print(data.frame(
    Site_name   = before$Site_name,
    old_ratio   = round(before$lake_ratio, 3),
    old_dist_km = round(before$distance, 1),
    new_dist_km = round(site_cells$distance[hit], 1)
  ))

  # a re-match that lands a long way off is worse than the disease
  still_far <- site_cells$rematched & site_cells$distance > max_match_km
  if (any(still_far)) {
    cat("Dropped", sum(still_far),
        "re-matched site(s) now beyond", max_match_km, "km\n")
    site_cells <- site_cells[!still_far, ]
  }
} else {
  site_cells$rematched <- FALSE
  cat("\nNo site sat on a lake-contaminated cell.\n")
}

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

#optional write out
#write.csv(matched_data_basin, paste0(savepath, "matched_data_basin", scope, ".csv"), row.names = F)
matched_data_basin <- read.csv(paste0(savepath, "matched_data_basin", scope, ".csv"))

#variability/bias test
#signed: positive = ERA5-Land reads higher than the manual survey
# UNITS: manual surveys arrive in cm and ERA5-Land in m w.e.; everything from
# here down is mm, matching ERA5_snowcomparison_ER.R
matched_data_basin$meanswe_mm <- matched_data_basin$meanswe * 10
matched_data_basin["diff"] <- (matched_data_basin$sd*1000 - matched_data_basin$meanswe_mm)
matched_data_basin["absdiff"] <- abs(matched_data_basin$sd*1000 - matched_data_basin$meanswe_mm)

#group bias based on basin and year

matched_data_grouped <- matched_data_basin %>%
  dplyr::group_by(basin, year) %>%
  dplyr::mutate(n_sites = n_distinct(Site_name)) %>%
  dplyr::filter(n_sites>3) %>%
  dplyr::mutate(meanbias = mean(diff, na.rm=T),
                meanswebasin = mean(meanswe_mm, na.rm=T)) %>%
  dplyr::mutate(percent_bias = meanbias/meanswebasin)

#plotting bias based on basin and year
plot <- ggplot2::ggplot(matched_data_grouped, ggplot2::aes(x = percent_bias, y = diff)) +
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
    x = "Manual survey mean SWE (mm)",
    y = "ERA5-Land - manual survey SWE (mm)",
    fill = "Basin"
  )

plot


# =============================================================================
# DIRECTIONAL BIAS OVER TIME, GROUPED BY BASIN AND ECOZONE
# Mirrors the basin/ecozone structure of ERA5_snowcomparison_ER.R, but on
# per-year signed bias rather than on trends.
# =============================================================================

library(ggplot2)

# minimum sites required before a region-year mean is reported
min_sites <- 3
# minimum years required before a REGION gets its own statistics.
# NOTE this is not min_year (22.5), which is the record length required of a
# SITE up at the top. The names are one letter apart - min_year gates sites,
# min_years gates regions.
min_years <- 5

# -----------------------------------------------------------------------------
# 1) Bias columns
# -----------------------------------------------------------------------------
# bias is SIGNED throughout: positive = ERA5-Land reads higher than the survey.
# bias_anom removes each site's own long-run mean bias, so a region-year mean
# is not driven by which sites happened to be surveyed that year.

matched_data_basin <- matched_data_basin %>%
  dplyr::mutate(
    swe_era = sd * 1000,                # ERA5-Land m w.e. -> mm
    swe_obs = meanswe * 10,             # manual surveys cm -> mm
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
# 2) Attach ecozone, ecoprovince and ecoregion
#    layer/col: "mackenzie_ecozonesv2"    & "ECOZONE_NA"   - coarsest
#               "mackenzie_ecoprovinces"  & "ECOPROVI_1"
#               "mackenzie_ecoregions"    & "ECOREGION1"   - finest
# -----------------------------------------------------------------------------

ecozones <- sf::st_read(paste0(shp_path, "mackenzie_ecozonesv2.shp")) %>%
  sf::st_transform(proj)

ecoprovinces <- sf::st_read(paste0(shp_path, "mackenzie_ecoprovinces.shp")) %>%
  sf::st_transform(proj)

ecoregions <- sf::st_read(paste0(shp_path, "mackenzie_ecoregions.shp")) %>%
  sf::st_transform(proj)

pts_eco <- as_points_sf(matched_data_basin,
                        lon_col = "longitude",
                        lat_col = "latitude",
                        crs = proj)

pts_eco <- attach_region(pts_eco, ecozones, "ECOZONE_NA",
                         max_km = 50, region_out_col = "ecozone")
pts_eco$dist_to_region_m <- NULL   # drop so the next call can add its own

pts_eco <- attach_region(pts_eco, ecoprovinces, "ECOPROVI_1",
                         max_km = 50, region_out_col = "ecoprovince")
pts_eco$dist_to_region_m <- NULL

pts_eco <- attach_region(pts_eco, ecoregions, "ECOREGION1",
                         max_km = 50, region_out_col = "ecoregion")

bias_data <- pts_eco %>%
  sf::st_drop_geometry()

# coverage check - anything unlabelled will silently vanish from the summaries
cat("\nUnassigned basin       :", sum(is.na(bias_data$basin)), "rows\n")
cat("Unassigned ecozone     :", sum(is.na(bias_data$ecozone)), "rows\n")
cat("Unassigned ecoprovince :", sum(is.na(bias_data$ecoprovince)), "rows\n")
cat("Unassigned ecoregion   :", sum(is.na(bias_data$ecoregion)), "rows\n")
print(sort(unique(bias_data$ecozone)))
print(sort(unique(bias_data$ecoprovince)))

# -----------------------------------------------------------------------------
# 3) Stack the groupings so one pipeline serves all of them
# -----------------------------------------------------------------------------

bias_long <- dplyr::bind_rows(
  bias_data %>% dplyr::mutate(region_type = "Basin",     region = basin),
  bias_data %>% dplyr::mutate(region_type = "Ecozone",   region = ecozone),
  bias_data %>% dplyr::mutate(region_type = "Ecoprovince", region = ecoprovince),
  bias_data %>% dplyr::mutate(region_type = "Ecoregion",   region = ecoregion)
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
      bias_trend     = sl$estimate,               # mm per year
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
# 6) Plot styling
# -----------------------------------------------------------------------------
# The bias-by-basin and bias-by-ecozone figures that used to live here have been
# removed: ERA5_snowcomparison_ER.R already covers that ground visually with
# p_swe, the paired ERA5 / manual mean SWE boxplots. The tables above are kept
# because the ER script has no per-year signed bias - region_stats$bias_vs0_p is
# the "there is a bias at all" baseline that section 7 builds on. Everything
# below section 7 is about how that bias varies with snow magnitude.

# shared box plot styling, matching the basin/ecozone boxplots in
# ERA5_snowcomparison_ER.R so the two sets of figures sit together in a report:
# region on the x axis with 45 degree labels, default ggplot fill palette,
# boxed panel, 12 pt text.
# Axis label angle follows ERA5_snowcomparison_ER.R so the two sets of figures
# match in the manuscript: ECOREGION panels use 90 degrees there (Figs A10-A12),
# everything else uses 45.
#
# margin_l widens the left plot margin. A 45 degree label runs down and to the
# LEFT of its tick, so the leftmost category on a long-named axis gets clipped at
# the plot edge unless room is made for it; left_margin_for() sizes that room
# from the longest label. A 90 degree label sits directly under its tick and
# needs none, so the margin is only applied at 45.
theme_box_er <- function(margin_l = 5.5, angle = 45) {
  list(
    ggplot2::theme_classic(),
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(angle = angle, hjust = 1),
      panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
      axis.text    = ggplot2::element_text(size = 12),
      axis.title   = ggplot2::element_text(size = 12),
      legend.text  = ggplot2::element_text(size = 12),
      plot.margin  = ggplot2::margin(5.5, 5.5, 5.5, margin_l, "pt")
    )
  )
}

# horizontal reach of the longest label once rotated 45 degrees, in points:
# characters x mean glyph width at 12 pt x cos(45). Floored at the ggplot
# default so short-labelled panels are unaffected.
# 90 degrees for Ecoregion, 45 elsewhere - matching the comparison script
label_angle_for <- function(rt) if (identical(rt, "Ecoregion")) 90 else 45

left_margin_for <- function(labels) {
  n <- suppressWarnings(max(nchar(as.character(labels)), na.rm = TRUE))
  if (!is.finite(n)) return(5.5)
  max(5.5, n * 6.6 * 0.707)
}

# =============================================================================
# 7) DOES THE BIAS CHANGE BETWEEN HIGH AND LOW SNOW YEARS?
# Same basin / ecozone / ecoprovince / ecoregion structure as above, but the year axis is
# replaced by how much snow that year actually had, expressed as an anomaly so
# that regions with very different snow climates can be compared directly.
# =============================================================================

# -----------------------------------------------------------------------------
# 7.0 Parameters
# -----------------------------------------------------------------------------
# What defines a "high" or "low" snow year:
#   "mean" = average of ERA5-Land and the survey (Bland-Altman style)
#   "obs"  = the manual surveys alone
#   "era"  = ERA5-Land alone
#
# IMPORTANT: bias = swe_era - swe_obs, so swe_obs sits on both sides of any
# bias ~ obs relationship. Survey noise alone then produces a negative slope
# (a low survey year looks like an ERA5 overestimate) even with no real
# state-dependence. Averaging the two (Bland-Altman) splits that error between
# the axes and removes most of the artifact, which is why it is the default.
# It is not a complete fix: if the year-to-year noise sits mostly in one of the
# two datasets rather than being shared, the average still tilts the slope,
# towards the noisier dataset's sign. So the obs-only slope is carried through
# to 7.4 as slope_obs_basis - if the two disagree in sign, or the "mean" slope
# is much the weaker of the pair, treat the result as measurement error rather
# than a real snow-year effect.
snow_year_basis <- "mean"

# a site needs this many years before its own anomaly is stable enough to use.
# Tied to min_year so there is one record-length knob; the site loop above has
# already applied the same threshold, so this normally does not bind.
min_site_years_anom <- min_year
# a tercile needs this many region-years before it gets compared
min_tercile_n <- 3

# -----------------------------------------------------------------------------
# 7.1 Site-level snow anomalies
# -----------------------------------------------------------------------------
# Standardised within each site, so "a high snow year" means high for THAT site
# rather than high in absolute mm. Without this, a region-year mean would move
# simply because a deep-snow site was or was not surveyed that year - the same
# composition problem that bias_anom already handles on the bias side.

snow_site <- bias_data %>%
  dplyr::mutate(
    swe_ref = if (snow_year_basis == "era") {
      swe_era
    } else if (snow_year_basis == "obs") {
      swe_obs
    } else {
      (swe_era + swe_obs) / 2
    }
  ) %>%
  dplyr::group_by(Site_name) %>%
  dplyr::mutate(
    site_n_ref  = sum(!is.na(swe_ref)),
    site_mn_ref = mean(swe_ref, na.rm = TRUE),
    site_sd_ref = stats::sd(swe_ref, na.rm = TRUE),
    z_ref       = (swe_ref - site_mn_ref) / site_sd_ref,
    # obs-only anomaly, carried alongside as the artifact check
    site_mn_obs = mean(swe_obs, na.rm = TRUE),
    site_sd_obs = stats::sd(swe_obs, na.rm = TRUE),
    z_obs       = (swe_obs - site_mn_obs) / site_sd_obs
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    z_ref = ifelse(site_n_ref >= min_site_years_anom & site_sd_ref > 0, z_ref, NA_real_),
    z_obs = ifelse(site_n_ref >= min_site_years_anom & site_sd_obs > 0, z_obs, NA_real_)
  )

cat("\nSites with a usable snow anomaly:",
    dplyr::n_distinct(snow_site$Site_name[!is.na(snow_site$z_ref)]),
    "of", dplyr::n_distinct(snow_site$Site_name), "\n")

# stack the three groupings exactly as bias_long does
snow_long <- dplyr::bind_rows(
  snow_site %>% dplyr::mutate(region_type = "Basin",     region = basin),
  snow_site %>% dplyr::mutate(region_type = "Ecozone",   region = ecozone),
  snow_site %>% dplyr::mutate(region_type = "Ecoprovince", region = ecoprovince),
  snow_site %>% dplyr::mutate(region_type = "Ecoregion",   region = ecoregion)
) %>%
  dplyr::filter(!is.na(region), !is.na(bias), !is.na(z_ref))

# -----------------------------------------------------------------------------
# 7.2 Region-year snow index, and high / normal / low classification
# -----------------------------------------------------------------------------
# snow_index is the mean site anomaly for that region-year: +1 means the region
# was roughly one standard deviation snowier than its own normal. Terciles are
# cut WITHIN each region, so "high" is high for that region.

region_year_snow <- snow_long %>%
  dplyr::group_by(region_type, region, year) %>%
  dplyr::summarise(
    n_sites        = dplyr::n_distinct(Site_name),
    snow_index     = mean(z_ref, na.rm = TRUE),
    snow_index_obs = mean(z_obs, na.rm = TRUE),
    mean_bias      = mean(bias, na.rm = TRUE),
    mean_bias_anom = mean(bias_anom, na.rm = TRUE),
    mean_abs_err   = mean(abs_err, na.rm = TRUE),
    mean_obs       = mean(swe_obs, na.rm = TRUE),
    mean_era       = mean(swe_era, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(percent_bias = 100 * mean_bias / mean_obs) %>%
  dplyr::filter(n_sites >= min_sites) %>%
  dplyr::group_by(region_type, region) %>%
  dplyr::filter(dplyr::n() >= min_years) %>%
  dplyr::mutate(
    snow_class = cut(
      snow_index,
      breaks = c(-Inf, stats::quantile(snow_index, c(1/3, 2/3), na.rm = TRUE), Inf),
      labels = c("Low", "Normal", "High"),
      include.lowest = TRUE
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(snow_class = factor(snow_class, levels = c("Low", "Normal", "High")))

# -----------------------------------------------------------------------------
# 7.3 Bias by snow tercile
# -----------------------------------------------------------------------------

tercile_summary <- region_year_snow %>%
  dplyr::group_by(region_type, region, snow_class) %>%
  dplyr::summarise(
    n_years      = dplyr::n(),
    mean_snow    = mean(snow_index),
    mean_obs     = mean(mean_obs),
    mean_bias    = mean(mean_bias),
    median_bias  = stats::median(mean_bias),
    percent_bias = 100 * sum(mean_bias) / sum(mean_obs),
    mean_abs_err = mean(mean_abs_err),
    .groups      = "drop"
  )

print(tercile_summary, n = 100)

# -----------------------------------------------------------------------------
# 7.4 Per-region test: is the bias different in high vs low snow years?
# -----------------------------------------------------------------------------
#   - high vs low tercile difference                (Wilcoxon rank sum)
#   - bias sensitivity to snow amount               (lm slope, mm per 1 SD)
#   - the same slope on percent bias                (lm slope, % per 1 SD)
#   - monotonic association                         (Spearman)
#   - the obs-only slope, as the artifact check     (lm slope on snow_index_obs)
# A negative slope means ERA5-Land loses ground - relatively less snow than the
# surveys - as the snowpack gets deeper.

snow_year_stats <- region_year_snow %>%
  dplyr::group_by(region_type, region) %>%
  dplyr::group_modify(function(.x, .y) {

    lo <- .x$mean_bias[.x$snow_class == "Low"]
    hi <- .x$mean_bias[.x$snow_class == "High"]

    testable <- length(lo) >= min_tercile_n && length(hi) >= min_tercile_n

    w_p <- if (testable) {
      stats::wilcox.test(hi, lo, exact = FALSE)$p.value
    } else NA_real_

    fit  <- stats::lm(mean_bias ~ snow_index, data = .x)
    sl   <- broom::tidy(fit) %>% dplyr::filter(term == "snow_index")
    ci   <- stats::confint(fit)["snow_index", ]

    fitp <- stats::lm(percent_bias ~ snow_index, data = .x)
    slp  <- broom::tidy(fitp) %>% dplyr::filter(term == "snow_index")

    fito <- stats::lm(mean_bias ~ snow_index_obs, data = .x)
    slo  <- broom::tidy(fito) %>% dplyr::filter(term == "snow_index_obs")

    ct <- stats::cor.test(.x$mean_bias, .x$snow_index,
                          method = "spearman", exact = FALSE)

    dplyr::tibble(
      n_years         = nrow(.x),
      n_low           = length(lo),
      n_high          = length(hi),
      bias_low        = if (length(lo)) mean(lo) else NA_real_,
      bias_high       = if (length(hi)) mean(hi) else NA_real_,
      bias_high_minus_low = if (testable) mean(hi) - mean(lo) else NA_real_,
      highlow_p       = w_p,
      slope_mm_per_sd = sl$estimate,          # mm of bias per 1 SD of snow
      slope_lwr       = ci[1],
      slope_upr       = ci[2],
      slope_p         = sl$p.value,
      slope_pct_per_sd = slp$estimate,        # percent bias per 1 SD of snow
      slope_pct_p      = slp$p.value,
      slope_obs_basis  = slo$estimate,        # artifact check, see 7.0
      slope_obs_p      = slo$p.value,
      r2               = summary(fit)$r.squared,
      rho              = unname(ct$estimate),
      rho_p            = ct$p.value
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(region_type, slope_mm_per_sd)

print(snow_year_stats, n = 50)

# -----------------------------------------------------------------------------
# 7.5 Site-level version, controlling for the site
# -----------------------------------------------------------------------------
# The region-year table throws away within-year spread and gives every year the
# same weight. This uses every site-year instead, with the site's own mean bias
# already removed (bias_anom), so the slope is a purely WITHIN-site response to
# snow amount: it cannot be produced by deep-snow sites simply being biased
# differently from shallow ones. Treat it as the primary result and 7.4 as the
# region-scale summary of it.

site_level_snow_stats <- snow_long %>%
  dplyr::group_by(region_type, region) %>%
  dplyr::filter(dplyr::n_distinct(Site_name) >= min_sites,
                dplyr::n_distinct(year) >= min_years) %>%
  dplyr::group_modify(function(.x, .y) {

    fit <- stats::lm(bias_anom ~ z_ref, data = .x)
    sl  <- broom::tidy(fit) %>% dplyr::filter(term == "z_ref")
    ci  <- stats::confint(fit)["z_ref", ]

    dplyr::tibble(
      n_obs           = nrow(.x),
      n_sites         = dplyr::n_distinct(.x$Site_name),
      n_years         = dplyr::n_distinct(.x$year),
      slope_mm_per_sd = sl$estimate,
      slope_lwr       = ci[1],
      slope_upr       = ci[2],
      slope_p         = sl$p.value,
      r2              = summary(fit)$r.squared
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(region_type, slope_mm_per_sd)

print(site_level_snow_stats, n = 50)

# pooled over the whole study area, for a single headline number
pooled_snow_fit <- stats::lm(bias_anom ~ z_ref,
                             data = dplyr::filter(snow_long, region_type == "Basin"))
cat("\nPooled within-site bias sensitivity (mm SWE per 1 SD of snow):\n")
print(broom::tidy(pooled_snow_fit))
# -----------------------------------------------------------------------------
# 7.6 Site level: do high snow years carry a different bias than low snow years?
# -----------------------------------------------------------------------------
# Each site is split into its OWN high / normal / low thirds, and the mean bias
# in its high years is compared against the mean bias in its low years. One
# number per site.
#
# This is the cleanest form of the question. 7.5 fits a slope across every
# site-year at once, which treats site-years as independent when sites in the
# same basin share weather, so its p-values are optimistic. Here each site
# contributes a single difference and the sites become the replicates, so the
# regional test below is not inflated by how many years happen to be on record.


# years needed in each of a site's own high and low thirds
min_site_tercile_n <- 4

site_years <- snow_site %>%
  dplyr::filter(!is.na(z_ref), !is.na(bias)) %>%
  dplyr::group_by(Site_name) %>%
  dplyr::filter(dplyr::n() >= min_site_years_anom) %>%
  dplyr::mutate(
    snow_class_site = cut(
      z_ref,
      breaks = c(-Inf, stats::quantile(z_ref, c(1/3, 2/3), na.rm = TRUE), Inf),
      labels = c("Low", "Normal", "High"),
      include.lowest = TRUE
    )
  ) %>%
  dplyr::ungroup()

# per-site high minus low contrast.
site_highlow <- site_years %>%
  dplyr::group_by(basin, ecozone, ecoprovince, ecoregion, Site_name) %>%
  dplyr::group_modify(function(.x, .y) {

    lo <- .x[.x$snow_class_site == "Low",  ]
    hi <- .x[.x$snow_class_site == "High", ]

    diff_mm <- mean(hi$bias) - mean(lo$bias)

    dplyr::tibble(
      n_years   = nrow(.x),
      n_low     = nrow(lo),
      n_high    = nrow(hi),
      obs_low   = mean(lo$swe_obs),
      obs_high  = mean(hi$swe_obs),
      bias_low  = mean(lo$bias),
      bias_high = mean(hi$bias),
      diff      = diff_mm,
      # percent bias in each third, so a proportional bias can be spotted
      pct_low   = 100 * mean(lo$bias) / mean(lo$swe_obs),
      pct_high  = 100 * mean(hi$bias) / mean(hi$swe_obs),
      # within-site test. Only a handful of years per third, so this is
      # underpowered on its own - the regional test below is the real one.
      p_site    = if (nrow(lo) >= min_site_tercile_n &&
                      nrow(hi) >= min_site_tercile_n) {
                    stats::wilcox.test(hi$bias, lo$bias, exact = FALSE)$p.value
                  } else NA_real_
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n_low >= min_site_tercile_n, n_high >= min_site_tercile_n) %>%
  dplyr::arrange(dplyr::desc(abs(diff)))

print(site_highlow, n = 60)

# stack for the three groupings, as elsewhere
site_highlow_long <- dplyr::bind_rows(
  site_highlow %>% dplyr::mutate(region_type = "Basin",     region = basin),
  site_highlow %>% dplyr::mutate(region_type = "Ecozone",   region = ecozone),
  site_highlow %>% dplyr::mutate(region_type = "Ecoprovince", region = ecoprovince),
  site_highlow %>% dplyr::mutate(region_type = "Ecoregion",   region = ecoregion)
) %>%
  dplyr::filter(!is.na(region))

# regional test, with the SITES as the replicates: are the per-site high-minus-
# low differences consistently away from zero?
site_highlow_stats <- site_highlow_long %>%
  dplyr::group_by(region_type, region) %>%
  dplyr::filter(dplyr::n() >= min_sites) %>%
  dplyr::group_modify(function(.x, .y) {

    tt <- stats::t.test(.x$diff, mu = 0)
    wt <- stats::wilcox.test(.x$diff, mu = 0, exact = FALSE)
    bt <- stats::binom.test(sum(.x$diff > 0), nrow(.x))

    dplyr::tibble(
      n_sites       = nrow(.x),
      n_positive    = sum(.x$diff > 0),
      mean_bias_low = mean(.x$bias_low),
      mean_bias_high= mean(.x$bias_high),
      mean_diff     = mean(.x$diff),        # high minus low
      ci_lwr        = tt$conf.int[1],
      ci_upr        = tt$conf.int[2],
      p_t           = tt$p.value,
      p_wilcox      = wt$p.value,
      p_sign        = bt$p.value,
      mean_pct_low  = mean(.x$pct_low),
      mean_pct_high = mean(.x$pct_high)
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(region_type, dplyr::desc(mean_diff))

print(site_highlow_stats, n = 50)

# whole study area, sites pooled
cat("\nAll sites pooled - high minus low snow year bias difference (mm SWE):\n")
print(stats::t.test(site_highlow$diff, mu = 0))
cat("Sites with a positive difference:", sum(site_highlow$diff > 0),
    "of", nrow(site_highlow), "\n")
# -----------------------------------------------------------------------------
# 7.7 Year-by-year: site-level bias against site-level snow magnitude
# -----------------------------------------------------------------------------
# One point per SITE-YEAR, per basin. This is the plain version of the question:
# in a year when a site had more snow than usual, was ERA5-Land further off?
#
# Magnitude is centred on the site (mag_anom, mm above or below that site's own
# normal) but NOT scaled by the site's standard deviation, so the slope is
# dimensionless - mm of bias per mm of snow - and reads directly across basins.
# 7.5 does the same fit in SD units; this is the same relationship in mm.
#
# Centring is kept because without it the slope would blend the year-to-year
# question with a between-site one - chronically deep sites sitting permanently
# at high magnitude - and that second question is not the one being asked here.


site_year_mag <- snow_site %>%
  dplyr::filter(!is.na(z_ref), !is.na(bias)) %>%
  dplyr::mutate(mag_anom = swe_ref - site_mn_ref)   # mm from the site's normal

site_year_mag_long <- dplyr::bind_rows(
  site_year_mag %>% dplyr::mutate(region_type = "Basin",     region = basin),
  site_year_mag %>% dplyr::mutate(region_type = "Ecozone",   region = ecozone),
  site_year_mag %>% dplyr::mutate(region_type = "Ecoprovince", region = ecoprovince),
  site_year_mag %>% dplyr::mutate(region_type = "Ecoregion",   region = ecoregion)
) %>%
  dplyr::filter(!is.na(region))

site_year_mag_stats <- site_year_mag_long %>%
  dplyr::group_by(region_type, region) %>%
  dplyr::filter(dplyr::n_distinct(Site_name) >= min_sites,
                dplyr::n_distinct(year) >= min_years) %>%
  dplyr::group_modify(function(.x, .y) {

    fit <- stats::lm(bias ~ mag_anom, data = .x)
    sl  <- broom::tidy(fit) %>% dplyr::filter(term == "mag_anom")
    ci  <- stats::confint(fit)["mag_anom", ]

    pe <- stats::cor.test(.x$bias, .x$mag_anom, method = "pearson")
    sp <- stats::cor.test(.x$bias, .x$mag_anom, method = "spearman", exact = FALSE)

    dplyr::tibble(
      n_obs     = nrow(.x),
      n_sites   = dplyr::n_distinct(.x$Site_name),
      n_years   = dplyr::n_distinct(.x$year),
      slope_mm  = sl$estimate,          # mm of bias per mm of snow anomaly
      slope_lwr = ci[1],
      slope_upr = ci[2],
      slope_p   = sl$p.value,
      r_pearson = unname(pe$estimate),
      rho_spear = unname(sp$estimate),
      rho_p     = sp$p.value,
      r2        = summary(fit)$r.squared
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(region_type, dplyr::desc(slope_mm))

print(site_year_mag_stats, n = 50)

# NOTE the same caveat as 7.5: these p-values treat site-years as independent
# when sites in a basin share weather, so they are optimistic. The site-level
# test in 7.6 is the one to quote.

# -----------------------------------------------------------------------------
# 7.8 ANOVA: does the bias differ between low, normal and high snow years?
# -----------------------------------------------------------------------------
# One-way ANOVA of bias on snow_class, run separately within each region, on the
# REGION-YEAR means - so the years are the replicates, as in 7.4. Running it on
# site-years instead would treat sites in the same winter as independent and
# inflate every F.
#
# Run on BOTH metrics, because they can disagree and the disagreement is the
# result. A region that separates in mm but not in percent has a bias that is a
# roughly constant fraction of the snowpack: the mm gap widens only because
# there is more snow, not because ERA5-Land is relatively further off. The Peace
# is that case.
#
# WHICH p TO QUOTE. The unit here is a region-year mean over several sites, so
# the residuals are near normal even though the raw site-year bias is skewed -
# checked on the real data, 0 of 22 basin fits failed Shapiro or Bartlett.
# Normality is therefore not the thing to worry about at this level; unequal
# variance between the three snow classes is, since bias spread widens with
# snow. So:
#   p_welch    default. Does not assume equal variance, still tests means.
#   p_anova    for F and eta_sq, and for the Tukey contrast.
#   p_kruskal  robustness check. It is NOT assumption free - it assumes similar
#              distribution shapes and is also sensitive to unequal variance -
#              and it tests ranks, not means, so it cannot give a mm effect
#              size. Use it to see whether a result depends on a few extreme
#              years, not as the primary test.
# Where all three disagree, the region has both unequal variance and outliers;
# report the effect size and the spread rather than a p-value.
#
# At the SITE level (7.6) the reverse holds: there the unit is a site, the
# distribution really is skewed and outlier-driven, and the rank and sign tests
# are the ones to believe.

# Games-Howell pairwise comparison: the Welch-consistent analogue of Tukey.
# Uses a separate variance and Welch-Satterthwaite df for each pair, so it does
# not assume equal variance, and takes p from the studentised range so the three
# comparisons are corrected as a family. Where variances really are equal it
# returns almost exactly what Tukey returns.
games_howell <- function(x, g, conf = 0.95) {

  g   <- droplevels(factor(g))
  lev <- levels(g)
  k   <- length(lev)
  n   <- tapply(x, g, length)
  m   <- tapply(x, g, mean)
  v   <- tapply(x, g, stats::var)

  out <- list()
  for (i in seq_len(k)) for (j in seq_len(k)) {
    if (j <= i) next
    a <- lev[j]; b <- lev[i]          # contrast is a - b, matching "High-Low"
    if (is.na(n[a]) || is.na(n[b]) || n[a] < 2 || n[b] < 2) next
    se2 <- v[a]/n[a] + v[b]/n[b]
    if (!is.finite(se2) || se2 <= 0) next
    df <- se2^2 / ((v[a]/n[a])^2/(n[a]-1) + (v[b]/n[b])^2/(n[b]-1))
    dm <- m[a] - m[b]
    q  <- abs(dm) / sqrt(se2 / 2)     # studentised range statistic
    pv <- stats::ptukey(q, nmeans = k, df = df, lower.tail = FALSE)
    cr <- stats::qtukey(conf, nmeans = k, df = df) * sqrt(se2 / 2)
    out[[paste0(a, "-", b)]] <- c(diff = unname(dm), lwr = unname(dm - cr),
                                  upr = unname(dm + cr), p = unname(pv),
                                  q = unname(q), df = unname(df))
  }
  if (!length(out)) return(matrix(numeric(0), nrow = 0, ncol = 6,
                                  dimnames = list(NULL, c("diff","lwr","upr","p","q","df"))))
  do.call(rbind, out)
}

anova_by_class <- function(d, yv) {

  d <- d[is.finite(d[[yv]]), , drop = FALSE]
  d$snow_class <- droplevels(d$snow_class)

  na_row <- dplyr::tibble(
    n = nrow(d), n_low = NA_integer_, n_normal = NA_integer_, n_high = NA_integer_,
    mean_low = NA_real_, mean_normal = NA_real_, mean_high = NA_real_,
    sd_low = NA_real_, sd_normal = NA_real_, sd_high = NA_real_,
    F_value = NA_real_, p_anova = NA_real_,
    welch_F = NA_real_, welch_df1 = NA_real_, welch_df2 = NA_real_, p_welch = NA_real_,
    eta_sq = NA_real_, kruskal_H = NA_real_, p_kruskal = NA_real_,
    high_minus_low = NA_real_, hl_lwr = NA_real_, hl_upr = NA_real_, hl_p = NA_real_,
    high_minus_norm = NA_real_, hn_lwr = NA_real_, hn_upr = NA_real_, hn_p = NA_real_,
    norm_minus_low = NA_real_, nl_lwr = NA_real_, nl_upr = NA_real_, nl_p = NA_real_,
    gh_hl_p = NA_real_, gh_hl_lwr = NA_real_, gh_hl_upr = NA_real_,
    gh_hn_p = NA_real_, gh_hn_lwr = NA_real_, gh_hn_upr = NA_real_,
    gh_nl_p = NA_real_,
    gh_hl_q = NA_real_, gh_hl_df = NA_real_, gh_hn_q = NA_real_, gh_hn_df = NA_real_,
    gh_nl_q = NA_real_, gh_nl_df = NA_real_,
    pattern = NA_character_,
    bartlett_K2 = NA_real_, bartlett_df = NA_real_, p_bartlett = NA_real_,
    shapiro_W = NA_real_, p_shapiro = NA_real_
  )

  # need at least two classes with two or more years apiece
  tab <- table(d$snow_class)
  if (sum(tab >= 2) < 2) return(na_row)

  fml <- stats::reformulate("snow_class", yv)
  out <- tryCatch({

    fit <- stats::aov(fml, data = d)
    sm  <- summary(fit)[[1]]
    kw  <- stats::kruskal.test(fml, data = d)

    cls_mean_ <- function(k) if (sum(d$snow_class == k) > 0) {
      mean(d[[yv]][d$snow_class == k])
    } else NA_real_
    cls_mean <- cls_mean_
    cls_sd <- function(k) if (sum(d$snow_class == k) > 1) {
      stats::sd(d[[yv]][d$snow_class == k])
    } else NA_real_

    # full test objects kept so the statistic and df can be reported, not just p
    wt <- tryCatch(stats::oneway.test(fml, data = d, var.equal = FALSE),
                   error = function(e) NULL)
    bt <- tryCatch(stats::bartlett.test(fml, data = d), error = function(e) NULL)
    st <- tryCatch(stats::shapiro.test(stats::residuals(fit)), error = function(e) NULL)

    # Tukey pairwise. All three contrasts are kept: a claim that high years
    # differ from BOTH normal and low needs High-Low and High-Normal, not just
    # the first. p values are Tukey-adjusted for the three comparisons.
    tk  <- stats::TukeyHSD(fit)$snow_class
    grab <- function(m, nm) if (nm %in% rownames(m)) m[nm, ] else rep(NA_real_, ncol(m))
    hl <- grab(tk, "High-Low")
    hn <- grab(tk, "High-Normal")
    nl <- grab(tk, "Normal-Low")

    # Games-Howell is the one to report: it matches the Welch omnibus test.
    # Tukey is kept alongside so the two can be shown to agree where the
    # variances are homogeneous.
    gh    <- games_howell(d[[yv]], d$snow_class)
    gh_hl <- grab(gh, "High-Low")
    gh_hn <- grab(gh, "High-Normal")
    gh_nl <- grab(gh, "Normal-Low")

    # plain-language ordering, so the pattern can be read without decoding
    # three p values. "=" means not separable at 0.05, not proven equal.
    sig <- function(v) is.finite(v[4]) && v[4] < 0.05
    pattern <- local({
      hl <- gh_hl; hn <- gh_hn; nl <- gh_nl      # pattern follows Games-Howell
    if (!any(is.finite(c(hl[4], hn[4], nl[4])))) {
      NA_character_
    } else if (sig(hl) && sig(hn) && sig(nl)) {
      "all three differ"
    } else if (sig(hl) && sig(hn)) {
      "High differs from Normal and Low"
    } else if (sig(hl) && sig(nl)) {
      "Low differs from Normal and High"
    } else if (sig(hn) && sig(nl)) {
      "Normal differs from High and Low"
    } else if (sig(hl)) {
      "High differs from Low only"
    } else if (sig(hn)) {
      "High differs from Normal only"
    } else if (sig(nl)) {
      "Normal differs from Low only"
    } else {
      "no pair differs"
    }})


    dplyr::tibble(
      n              = nrow(d),
      n_low          = as.integer(sum(d$snow_class == "Low")),
      n_normal       = as.integer(sum(d$snow_class == "Normal")),
      n_high         = as.integer(sum(d$snow_class == "High")),
      mean_low       = cls_mean("Low"),
      mean_normal    = cls_mean("Normal"),
      mean_high      = cls_mean("High"),
      sd_low         = cls_sd("Low"),
      sd_normal      = cls_sd("Normal"),
      sd_high        = cls_sd("High"),
      F_value        = sm[["F value"]][1],
      p_anova        = sm[["Pr(>F)"]][1],
      # Welch does not assume equal variance across the three snow classes.
      # Prefer it to p_anova: it costs almost nothing when the variances match
      # and is the correct test when they do not.
      welch_F        = if (is.null(wt)) NA_real_ else unname(wt$statistic),
      welch_df1      = if (is.null(wt)) NA_real_ else unname(wt$parameter[1]),
      welch_df2      = if (is.null(wt)) NA_real_ else unname(wt$parameter[2]),
      p_welch        = if (is.null(wt)) NA_real_ else wt$p.value,
      # share of variance explained by snow class
      eta_sq         = sm[["Sum Sq"]][1] / sum(sm[["Sum Sq"]]),
      kruskal_H      = unname(kw$statistic),
      p_kruskal      = kw$p.value,
      # High - Low
      high_minus_low = hl[1],
      hl_lwr         = hl[2],
      hl_upr         = hl[3],
      hl_p           = hl[4],
      # High - Normal
      high_minus_norm = hn[1],
      hn_lwr          = hn[2],
      hn_upr          = hn[3],
      hn_p            = hn[4],
      # Normal - Low
      norm_minus_low  = nl[1],
      nl_lwr          = nl[2],
      nl_upr          = nl[3],
      nl_p            = nl[4],
      # Games-Howell equivalents - report these, not the Tukey p values
      gh_hl_p         = gh_hl[4],
      gh_hl_lwr       = gh_hl[2],
      gh_hl_upr       = gh_hl[3],
      gh_hn_p         = gh_hn[4],
      gh_hn_lwr       = gh_hn[2],
      gh_hn_upr       = gh_hn[3],
      gh_nl_p         = gh_nl[4],
      # Games-Howell studentised range statistic and Welch df for each pair
      gh_hl_q         = gh_hl[5],
      gh_hl_df        = gh_hl[6],
      gh_hn_q         = gh_hn[5],
      gh_hn_df        = gh_hn[6],
      gh_nl_q         = gh_nl[5],
      gh_nl_df        = gh_nl[6],
      pattern        = pattern,
      bartlett_K2    = if (is.null(bt)) NA_real_ else unname(bt$statistic),
      bartlett_df    = if (is.null(bt)) NA_real_ else unname(bt$parameter),
      p_bartlett     = if (is.null(bt)) NA_real_ else bt$p.value,
      shapiro_W      = if (is.null(st)) NA_real_ else unname(st$statistic),
      p_shapiro      = if (is.null(st)) NA_real_ else st$p.value
    )
  }, error = function(e) na_row)

  out
}

run_anova <- function(yv, label) {
  region_year_snow %>%
    dplyr::group_by(region_type, region) %>%
    dplyr::filter(dplyr::n() >= min_years) %>%
    dplyr::group_modify(function(.x, .y) anova_by_class(.x, yv)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(metric = label)
}

snow_class_anova <- dplyr::bind_rows(
  run_anova("mean_bias",    "Bias (mm)"),
  run_anova("percent_bias", "Bias (%)")
) %>%
  dplyr::select(metric, region_type, region, dplyr::everything()) %>%
  dplyr::arrange(region_type, region, dplyr::desc(metric))

print(snow_class_anova, n = 100)

# the comparison that matters: significant in mm but not in percent means a
# proportional bias, not one that grows relative to the snowpack
cat("\nmm vs percent, do they agree?\n")
print(
  snow_class_anova %>%
    dplyr::select(region_type, region, metric, p_anova, p_kruskal) %>%
    tidyr::pivot_wider(names_from = metric,
                       values_from = c(p_anova, p_kruskal)) %>%
    dplyr::mutate(reading = dplyr::case_when(
      `p_anova_Bias (mm)` <  0.05 & `p_anova_Bias (%)` >= 0.05 ~ "proportional only",
      `p_anova_Bias (mm)` <  0.05 & `p_anova_Bias (%)` <  0.05 ~ "relative effect too",
      `p_anova_Bias (mm)` >= 0.05 & `p_anova_Bias (%)` <  0.05 ~ "relative only",
      TRUE                                                     ~ "no difference"
    )) %>%
    dplyr::arrange(region_type, region),
  n = 50
)

# -----------------------------------------------------------------------------
# 7.8b Could a snow-class bias leak into the ERA5-Land trend?
# -----------------------------------------------------------------------------
# A bias that differs between high and low snow years only changes the ERA5-Land
# TREND if those years are unevenly spread through the record (e.g. the high
# years bunched early). If they are spread across the record, the bias has no
# trend of its own and the ERA5-Land and manual trends are unaffected.
# Two checks per region:
#   - timing: mean year of High vs Low years, and how many of each fall in the
#     first vs second half of the record (Wilcoxon on year, High vs Low)
#   - net effect: Mann-Kendall / Sen's slope on the region-year bias itself,
#     expressed as % of mean SWE per decade so it sits on the same scale as the
#     trend differences in Figs A8 / A13 (a bias trend IS the ERA5 - manual
#     trend difference for that region)

mid_year <- (start_year + end_year) / 2

snow_class_timing <- region_year_snow %>%
  dplyr::group_by(region_type, region) %>%
  dplyr::group_modify(function(.x, .y) {
    hi <- .x$year[.x$snow_class == "High"]
    lo <- .x$year[.x$snow_class == "Low"]
    .x <- dplyr::arrange(.x, year)
    sen_mm  <- trend::sens.slope(.x$mean_bias)$estimates      # mm per year
    sen_pct <- trend::sens.slope(.x$percent_bias)$estimates   # % points per year
    mk      <- Kendall::MannKendall(.x$mean_bias)
    mkp     <- Kendall::MannKendall(.x$percent_bias)
    wt      <- if (length(hi) >= 2 && length(lo) >= 2) {
      stats::wilcox.test(hi, lo, exact = FALSE)
    } else NULL
    dplyr::tibble(
      n_years          = nrow(.x),
      first_year       = min(.x$year),
      last_year        = max(.x$year),
      mean_year_high   = mean(hi),
      mean_year_low    = mean(lo),
      n_high_early     = sum(hi <= mid_year),
      n_high_late      = sum(hi >  mid_year),
      n_low_early      = sum(lo <= mid_year),
      n_low_late       = sum(lo >  mid_year),
      timing_W         = if (!is.null(wt)) unname(wt$statistic) else NA_real_,
      timing_p         = if (!is.null(wt)) wt$p.value else NA_real_,
      bias_mk_tau      = as.numeric(mk$tau),
      bias_mk_p        = as.numeric(mk$sl),
      bias_sen_mm_dec  = 10 * unname(sen_mm),
      # Sen's slope of the bias relative to mean SWE, % per decade
      bias_sen_pct_dec = 100 * 10 * unname(sen_mm) / mean(.x$mean_obs),
      pctbias_mk_tau   = as.numeric(mkp$tau),
      pctbias_mk_p     = as.numeric(mkp$sl),
      pctbias_sen_dec  = 10 * unname(sen_pct)
    )
  }) %>%
  dplyr::ungroup()

# flag the regions where the snow class mattered (Welch p < 0.05 on either metric)
snow_class_timing <- snow_class_timing %>%
  dplyr::left_join(
    snow_class_anova %>%
      dplyr::group_by(region_type, region) %>%
      dplyr::summarise(min_p_welch = min(p_welch, na.rm = TRUE), .groups = "drop"),
    by = c("region_type", "region")
  ) %>%
  dplyr::mutate(class_sig = min_p_welch < 0.05) %>%
  dplyr::arrange(region_type, dplyr::desc(class_sig), region)

cat("\nSnow-class timing and bias drift (regions with a significant snow-class effect first):\n")
print(snow_class_timing %>%
        dplyr::select(region_type, region, class_sig, mean_year_high, mean_year_low,
                      n_high_early, n_high_late, n_low_early, n_low_late, timing_W, timing_p,
                      bias_mk_tau, bias_mk_p, bias_sen_pct_dec),
      n = 100, width = Inf)

# -----------------------------------------------------------------------------
# 7.9 Plots
# -----------------------------------------------------------------------------

# bias against the snow index, one panel per region
plot_bias_vs_snow <- function(rt) {
  d <- dplyr::filter(region_year_snow, region_type == rt)

  ggplot2::ggplot(d, ggplot2::aes(x = snow_index, y = mean_bias)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey80", linetype = "dashed") +
    ggplot2::geom_smooth(method = "lm", se = TRUE, colour = "black",
                         fill = "lightgrey", linewidth = 0.8, alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(fill = region), shape = 21, size = 3,
                        stroke = 0.7, alpha = 0.8) +
    ggplot2::facet_wrap(~ region) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Snow year anomaly (SD from the region's normal)",
                  y = "ERA5-Land - manual survey SWE (mm)",
                  title = paste0("SWE bias vs snow year magnitude by ", rt))
}

# the same thing as terciles, which is easier to read in a report. Laid out like
# the grouped basin boxplots in ERA5_snowcomparison_ER.R - region on the x axis,
# snow year as the fill - so the panels are directly comparable.
#
# percent = TRUE plots percent bias instead of mm. Use it wherever a region
# spans a wide range of snow depths: in mm a handful of deep sites dominate
# every region-year mean, so the boxes separate mainly because the snowpack is
# bigger, not because ERA5-Land is relatively further off. The Peace is the
# clear case - its alpine sites run about -65% in both low and high snow years,
# so the mm figure shows a strong effect and the percent figure shows almost
# none. The percent one is the correct reading there.
plot_bias_by_tercile <- function(rt, drop_normal = FALSE, percent = FALSE) {
  d <- dplyr::filter(region_year_snow, region_type == rt)
  # all three terciles by default, so nothing is hidden. drop_normal = TRUE
  # keeps only the Low / High contrast, which is tidier for a final figure.
  if (drop_normal) d <- dplyr::filter(d, snow_class != "Normal")
  d <- dplyr::mutate(d, snow_class = droplevels(snow_class))

  yv   <- if (percent) "percent_bias" else "mean_bias"
  ylab <- if (percent) {
    "ERA5-Land bias (% of manual survey SWE)"
  } else {
    "ERA5-Land - manual survey SWE (mm)"
  }

  # basins use the same short labels as the correlation and mean SWE figures
  xlab_fun <- if (identical(rt, "Basin")) short_basin else ggplot2::waiver()
  xlabs    <- if (identical(rt, "Basin")) short_basin(d$region) else d$region

  ggplot2::ggplot(d, ggplot2::aes(x = region, y = .data[[yv]], fill = snow_class)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.6) +
    ggplot2::geom_boxplot(outlier.alpha = 0.2) +
    ggplot2::scale_x_discrete(labels = xlab_fun) +
    theme_box_er(
      angle    = label_angle_for(rt),
      margin_l = if (label_angle_for(rt) == 45) left_margin_for(xlabs) else 5.5
    ) +
    ggplot2::labs(x = rt, y = ylab, fill = NULL)
}

# how strongly each region's bias responds to snow amount, with 95% CI
plot_snow_sensitivity <- function(rt, stats_tbl = site_level_snow_stats) {
  d <- stats_tbl %>%
    dplyr::filter(region_type == rt) %>%
    dplyr::mutate(sig = ifelse(slope_p < 0.05, "p < 0.05", "n.s."))

  ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(region, slope_mm_per_sd),
                                  y = slope_mm_per_sd, colour = sig)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.6) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = slope_lwr, ymax = slope_upr),
                             size = 0.7) +
    ggplot2::scale_colour_manual(values = c("p < 0.05" = "dodgerblue4",
                                            "n.s." = "grey60")) +
    theme_box_er() +
    ggplot2::labs(x = rt,
                  y = "Change in bias per 1 SD of snow (mm SWE)",
                  colour = NULL)
}

# per-site high minus low snow year bias difference, one point per site.
# Above zero means that site's bias runs more positive in its snowy years.
plot_site_highlow <- function(rt) {
  d <- dplyr::filter(site_highlow_long, region_type == rt)

  ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(region, diff,
                                                     FUN = stats::median),
                                  y = diff, fill = region)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.6) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    ggplot2::geom_jitter(width = 0.15, size = 1.8, alpha = 0.7) +
    theme_box_er(
      angle    = label_angle_for(rt),
      margin_l = if (label_angle_for(rt) == 45) left_margin_for(d$region) else 5.5
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = rt,
                  y = "High minus low snow year bias (mm SWE, per site)",
                  fill = NULL)
}

# year-by-year site-level bias against site-level snow magnitude, in mm.
# One point per site-year. Colour carries the year, so a relationship driven by
# a handful of particular winters shows up as colour clustering rather than an
# even spread along the line.
plot_site_year_mag <- function(rt, colour_by_year = TRUE) {
  d <- dplyr::filter(site_year_mag_long, region_type == rt)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = mag_anom, y = bias)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey80", linetype = "dashed")

  p <- if (colour_by_year) {
    p + ggplot2::geom_point(ggplot2::aes(colour = year), size = 1.8, alpha = 0.7) +
      ggplot2::scale_colour_viridis_c(option = "viridis")
  } else {
    p + ggplot2::geom_point(size = 1.8, alpha = 0.5, colour = "grey30")
  }

  p +
    ggplot2::geom_smooth(method = "lm", se = TRUE, colour = "black",
                         fill = "lightgrey", linewidth = 0.8, alpha = 0.5) +
    ggplot2::facet_wrap(~ region) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "Site snow anomaly (mm from that site's normal)",
                  y = "ERA5-Land - manual survey SWE (mm)",
                  colour = "Year")
}

p_snow_basin_scatter <- plot_bias_vs_snow("Basin")
p_snow_eco_scatter   <- plot_bias_vs_snow("Ecozone")
p_snow_ep_scatter    <- plot_bias_vs_snow("Ecoprovince")
p_snow_er_scatter    <- plot_bias_vs_snow("Ecoregion")

p_snow_basin_box <- plot_bias_by_tercile("Basin", percent = T)
p_snow_eco_box   <- plot_bias_by_tercile("Ecozone")
p_snow_ep_box    <- plot_bias_by_tercile("Ecoprovince")
p_snow_er_box    <- plot_bias_by_tercile("Ecoregion")

# percent versions - the honest reading where a region spans a wide SWE range
p_snow_basin_box_pct <- plot_bias_by_tercile("Basin",     percent = TRUE)
p_snow_eco_box_pct   <- plot_bias_by_tercile("Ecozone",   percent = TRUE)
p_snow_ep_box_pct    <- plot_bias_by_tercile("Ecoprovince", percent = TRUE)
p_snow_er_box_pct    <- plot_bias_by_tercile("Ecoregion", percent = TRUE)

p_snow_basin_sens <- plot_snow_sensitivity("Basin")
p_snow_eco_sens   <- plot_snow_sensitivity("Ecozone")
p_snow_ep_sens    <- plot_snow_sensitivity("Ecoprovince")
p_snow_er_sens    <- plot_snow_sensitivity("Ecoregion")

p_site_year_basin <- plot_site_year_mag("Basin")
p_site_year_eco   <- plot_site_year_mag("Ecozone")
p_site_year_ep    <- plot_site_year_mag("Ecoprovince")
p_site_year_er    <- plot_site_year_mag("Ecoregion")

p_site_highlow_basin <- plot_site_highlow("Basin")
p_site_highlow_eco   <- plot_site_highlow("Ecozone")
p_site_highlow_ep    <- plot_site_highlow("Ecoprovince")
p_site_highlow_er    <- plot_site_highlow("Ecoregion")

p_snow_basin_scatter
p_snow_basin_box
p_snow_basin_box_pct
p_snow_basin_sens
p_site_highlow_basin
p_site_year_basin

# site-year cloud behind the regional slopes
p_snow_sites <- ggplot2::ggplot(
  dplyr::filter(snow_long, region_type == "Basin"),
  ggplot2::aes(x = z_ref, y = bias_anom)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.6) +
  ggplot2::geom_point(ggplot2::aes(colour = region), size = 1.5, alpha = 0.4) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, colour = "black",
                       fill = "lightgrey", linewidth = 0.8, alpha = 0.5) +
  ggplot2::facet_wrap(~ region) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(x = "Site snow anomaly (SD from the site's normal)",
                y = "Bias anomaly (mm SWE, site means removed)",
                title = "Within-site bias response to snow year magnitude")
p_snow_sites

 write.csv(tercile_summary,       paste0(savepath, "bias_by_snow_tercile", scope, ".csv"), row.names = FALSE)
 write.csv(snow_year_stats,       paste0(savepath, "bias_snow_year_stats", scope, ".csv"), row.names = FALSE)
 write.csv(site_level_snow_stats, paste0(savepath, "bias_snow_year_sitelevel", scope, ".csv"), row.names = FALSE)
 write.csv(site_highlow,          paste0(savepath, "bias_site_highlow", scope, ".csv"), row.names = FALSE)
 write.csv(site_highlow_stats,    paste0(savepath, "bias_site_highlow_stats", scope, ".csv"), row.names = FALSE)
 write.csv(site_year_mag_stats,   paste0(savepath, "bias_site_year_magnitude", scope, ".csv"), row.names = FALSE)
 write.csv(snow_class_anova,      paste0(savepath, "bias_snow_class_anova", scope, ".csv"), row.names = FALSE)
 write.csv(snow_class_timing,     paste0(savepath, "bias_snow_class_timing", scope, ".csv"), row.names = FALSE)
  
# ggplot2::ggsave("p_snow_basin_scatter.png", p_snow_basin_scatter, dpi = 900, path = savepath)
 # --- grouped snow-tercile box plots, all four groupings, both metrics -------
 # Region on the x axis, snow year as the fill. ONE set only - no scope suffix,
 # so a run overwrites the previous one. The manuscript uses the MRB-scope
 # version; re-running under scope "NWT" will replace these files.
 fig_path <- paste0(user, "/Documents/NT_Hydrology/Figures")
 dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)

 for (rt in c("Basin", "Ecozone", "Ecoprovince", "Ecoregion")) {
   tag <- tolower(rt)
   # vertical ecoregion labels eat a lot of height; without extra canvas the
   # panel shrinks until the rotated y-axis title no longer fits beside it
   h <- if (label_angle_for(rt) == 90) 9 else 6
   ggplot2::ggsave(paste0("p_bias_tercile_mm_",  tag, ".png"),
                   plot_bias_by_tercile(rt),                  dpi = 900,
                   path = fig_path, width = 9, height = h)
   ggplot2::ggsave(paste0("p_bias_tercile_pct_", tag, ".png"),
                   plot_bias_by_tercile(rt, percent = TRUE),  dpi = 900,
                   path = fig_path, width = 9, height = h)
 }
 cat("\nWrote 8 grouped tercile box plots (scope", scope, ") to", fig_path, "\n")

 # --- combined basin figure for the main text: (a) mm bias, (b) percent bias ---
 # One shared legend, x axis labels on the bottom panel only.
 p_tercile_basin <- patchwork::wrap_plots(
     plot_bias_by_tercile("Basin") +
       ggplot2::theme(axis.text.x  = ggplot2::element_blank(),
                      axis.title.x = ggplot2::element_blank()),
     plot_bias_by_tercile("Basin", percent = TRUE),
     ncol = 1, guides = "collect") +
   patchwork::plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
   ggplot2::theme(plot.tag = ggplot2::element_text(size = 14, face = "bold"))
 ggplot2::ggsave(paste0("p_bias_tercile_basin_ab_", scope, ".png"), p_tercile_basin,
                 dpi = 900, path = fig_path, width = 9, height = 10)
# ggplot2::ggsave("p_snow_basin_sens.png",    p_snow_basin_sens,    dpi = 900, path = savepath)
# ggplot2::ggsave("p_snow_sites.png",         p_snow_sites,         dpi = 900, path = savepath)
