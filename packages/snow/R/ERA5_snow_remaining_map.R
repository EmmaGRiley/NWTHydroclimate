# ERA5 Snow Remaining Analysis
# Downloads ERA5-Land snow depth (m water equivalent) for a user-specified date
# and compares it against the April 1 max (from snow_depth_April_max_2026.nc) as
# a percentage. Maps the percent of snow remaining relative to the April 1 max.
# See README_snow_scripts.md in the snow package folder before running.

`%>%` <- magrittr::`%>%`
`%!in%` <- Negate(`%in%`)

# CARTO background tiles using your own API key from .Renviron (same function as in ERA5_snowtrends_map_hourly_functions.R)
carto_tiles <- function(map, style = "light_all", group = "CartoDB") {
  key <- Sys.getenv("CARTO_API_KEY")          # match your .Renviron name
  if (!nzchar(key)) stop("CARTO_API_KEY not set in .Renviron")

  leaflet::addTiles(
    map,
    urlTemplate = sprintf(
      "https://{s}.basemaps.cartocdn.com/rastertiles/%s/{z}/{x}/{y}.png?key=%s",
      style, key),
    attribution = paste0(
      '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, ',
      '&copy; <a href="https://carto.com/attributions">CARTO</a>'),
    group = group,
    options = leaflet::tileOptions(subdomains = "abcd", maxZoom = 20)
  )
}

# adds the background map chosen in the basemap setting
add_basemap <- function(map) {
  if (basemap == "carto_key") {
    carto_tiles(map, style = "light_nolabels") # CARTO light background, no labels
  } else {
    leaflet::addProviderTiles(map, basemap, group = "Basemap")
  }
}

# Define user
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME"))) # PC version

#set up file directory
folder <- paste0(user, "/Documents/R_scripts/Packages/snow/data/")
era5_dir <- paste0(folder, "SWEmax_dataset/") # ERA5 .nc files are downloaded to / read from here
shapefile_path <- paste0(user, "/Documents/Shapefiles/")

# FIRST-TIME SETUP ONLY - run these lines once on a new computer (highlight and Ctrl+Enter), not every time:
# install.packages(c("ecmwfr", "ncdf4", "automap", "foreach", "doSNOW", "pbapply", "cowplot"),
#                  dependencies = TRUE)
# #note - workflow does not work with terra versions > 1.8-60
# install.packages("terra",
#   repos = "https://packagemanager.posit.co/cran/2024-12-01",
#   type = "win.binary"
# )
# install.packages(c('progress', 'viridis', 'usethis'))
# install.packages(paste0(user, "/Documents/Github/KrigR"), repos = NULL, type = "source")

# Required libraries
library(KrigR)
library(terra)
library(dplyr)
library(sf)
library(leaflet)
library(ncdf4)

# ---- User inputs -------------------------------------------------------------

current_year  <- 2026
start_date <- "2026-06-09"
target_date   <- "2026-06-09"   # date to compare against April 1 max (YYYY-MM-DD). Check to see whats available on the climate store
baseline_date <- "2026-05-01"   # diff mode only: label for the baseline_file date (used in legend)

# Path to April 1 max baseline .nc (produced by ERA5_spring_snow_analysis.R)
baseline_file <- paste0(user,
                        "/Documents/R_Scripts/Packages/snow/data/SWEmax_dataset/",
                        "snow_depth_April_max_", current_year, ".nc")

#option to compare to a specific file
# baseline_file <- paste0(user,
#                         "/Documents/R_Scripts/Packages/snow/data/SWEmax_dataset/",
#                         "snow_depth_target_20260609.nc")

# Spatial extent (must match the baseline .nc)
lat_min  <- 51
lat_max  <- 80
long_min <- -140
long_max <- -95
interval <- 0.1

# ECMWF API credentials (stored in your .Renviron file - see README)
api_user <- Sys.getenv("CDS_USER")
api_key  <- Sys.getenv("CDS_KEY")
if (api_user == "" || api_key == "") {
  stop("CDS_USER / CDS_KEY not found. Add them to your .Renviron file and restart R (see README).", call. = FALSE)
}
if (!file.exists(baseline_file)) {
  stop("Baseline file not found: ", baseline_file,
       "\nRun ERA5_spring_snow_analysis.R for ", current_year, " first (see README).", call. = FALSE)
}

# Mapping options
map_mode           <- "swe"  # "percent" = % of April 1 max remaining; "swe" = absolute SWE (mm w.e.) "diff" = compare target date to baseline file
clip_ERA5          <- TRUE
mack_basin_outline <- TRUE
add_legend         <- TRUE
save               <- TRUE
add_basin          <- TRUE
basemap            <- "carto_key" # background map. "carto_key" = CARTO light, no labels, using your own CARTO_API_KEY in .Renviron
# (how to get one: README section 6d). Never use someone else's key. No key? use another leaflet provider,
# e.g. "Esri.WorldGrayCanvas" (full list: names(leaflet::providers))

# check the background map setting
if (basemap == "carto_key" && !nzchar(Sys.getenv("CARTO_API_KEY"))) {
  stop("basemap = \"carto_key\" but there is no CARTO_API_KEY in your .Renviron.\n",
       "Add your own key (see README section 6d) and restart R, or use a background that doesn't need a key, e.g. basemap = \"Esri.WorldGrayCanvas\".", call. = FALSE)
}
if (basemap != "carto_key" && !(basemap %in% names(leaflet::providers))) {
  stop("basemap = \"", basemap, "\" is not a leaflet background map. Check the spelling, or run names(leaflet::providers) to see the options.", call. = FALSE)
}

# Set webshot path (added to the end of PATH rather than replacing it)
Sys.setenv(PATH = paste(Sys.getenv("PATH"), paste0(user, "/Documents/Modelling/phantomjs/phantomjs/bin"), sep = ";"))

# ---- Download ERA5-Land SWE for target date ----------------------------------

study_extent <- terra::ext(c(long_min, long_max, lat_min, lat_max))

date_start <- paste0(target_date, " 00:00")
date_stop  <- paste0(target_date, " 23:00")

target_fname <- paste0("snow_depth_target_", gsub("-", "", target_date))
target_nc    <- paste0(era5_dir, target_fname, ".nc")

if (!file.exists(target_nc)) {
  cat(sprintf("\nDownloading ERA5-Land SWE for %s...\n", target_date))
  CDownloadS(
    Variable      = "snow_depth_water_equivalent",
    DataSet       = "reanalysis-era5-land",
    Type          = NA,
    DateStart     = date_start,
    DateStop      = date_stop,
    TZone         = "UTC",
    TResolution   = "hour",
    TStep         = 1,
    FUN           = "none",
    Extent        = study_extent,
    Dir           = era5_dir,
    FileName      = target_fname,
    FileExtension = ".nc",
    API_User      = api_user,
    API_Key       = api_key,
    verbose       = TRUE
  )
} else {
  cat(sprintf("Target .nc already exists: %s\n", target_nc))
}

# ---- Helper: extract per-point max SWE across time layers --------------------

extract_point_max <- function(nc_path,
                              lat_min, lat_max,
                              long_min, long_max,
                              interval) {

  if (!file.exists(nc_path)) stop("NetCDF file not found: ", nc_path)

  snow_data <- terra::rast(nc_path)
  cat(sprintf("  %s: %d layers\n", basename(nc_path), terra::nlyr(snow_data)))

  # Compute per-cell max directly in raster space — avoids terra::extract
  # coordinate-alignment failures introduced by newer terra versions when cell
  # centres from a manually-assigned extent don't exactly match a seq() grid.
  snow_max <- terra::app(snow_data, fun = function(x) {
    x[x <= -1e30]              <- NA   # fill value
    x[x == 10]                 <- NA   # glacier flag
    x[x < 0 & abs(x) < 1e-12] <- 0    # clamp underflow
    x[x < 0 | x > 10]         <- NA   # physical bounds
    m <- max(x, na.rm = TRUE)
    if (is.infinite(m)) NA_real_ else m
  })

  # Read cell centres from the raster itself, then round to grid precision so
  # old-CDS and new-CDS files join on identical coordinates.
  df <- terra::as.data.frame(snow_max, xy = TRUE, na.rm = FALSE)
  names(df) <- c("Longitude", "Latitude", "sd")
  df$Longitude <- round(df$Longitude, 1)
  df$Latitude  <- round(df$Latitude,  1)

  dplyr::tibble(
    Longitude = df$Longitude,
    Latitude  = df$Latitude,
    sd        = df$sd
  ) %>%
    dplyr::filter(Longitude >= long_min, Longitude <= long_max,
                  Latitude  >= lat_min,  Latitude  <= lat_max)
}

# ---- Extract snow depth for April 1 max and target date ----------------------

cat("\nExtracting April 1 max baseline...\n")
april_df <- extract_point_max(baseline_file,
                              lat_min, lat_max, long_min, long_max, interval) %>%
  dplyr::rename(sd_april = sd)

cat("\nExtracting target date snow depth...\n")
target_df <- extract_point_max(target_nc,
                               lat_min, lat_max, long_min, long_max, interval) %>%
  dplyr::rename(sd_target = sd)

# ---- Compute metric to map: percent remaining or absolute SWE ----------------

joined <- dplyr::left_join(april_df, target_df,
                           by = c("Longitude", "Latitude")) %>%
  dplyr::mutate(
    per_remaining = dplyr::case_when(
      is.na(sd_april) | is.na(sd_target) ~ NA_real_,
      sd_april <= 0                      ~ NA_real_,   # avoid divide-by-zero
      TRUE                               ~ (sd_target / sd_april) * 100
    ),
    swe_mm = sd_target * 1000,  # ERA5-Land SWE is in m water equivalent -> mm
    diff_mm = (sd_target - sd_april) * 1000  # target minus baseline (diff mode)
  )

if (map_mode == "percent") {
  plot_df      <- joined %>%
    dplyr::filter(!is.na(per_remaining)) %>%
    dplyr::rename(value = per_remaining)
  bin_breaks   <- c(0, 10, 25, 50, 75, 90, 100, 1000)
  bin_labels   <- c("< 10%", "10 - 25%", "26 - 50%", "51 - 75%",
                    "76 - 90%", "91 - 100%", "> 100%")
  pal_name     <- "RdYlBu"   # red = little snow remaining, blue = still snowy
  legend_title <- paste0("Snow Remaining<br>", target_date,
                         "<br>vs April 1 max ", current_year)
} else if (map_mode == "swe") {
  plot_df      <- joined %>%
    dplyr::filter(!is.na(swe_mm)) %>%
    dplyr::rename(value = swe_mm)
  bin_breaks   <- c(0, 10, 25, 50, 100, 200, 400, Inf)
  bin_labels   <- c("< 10", "10 - 25", "25 - 50", "50 - 100",
                    "100 - 200", "200 - 400", "> 400")
  pal_name     <- "YlGnBu"   # light = low SWE, dark = high SWE
  legend_title <- paste0("SWE (mm w.e.)<br>", target_date)
} else if (map_mode == "diff") {
  # Difference of two single-day snapshots: target_date minus baseline_file.
  # NOTE: baseline_file must point to the OTHER date's single-day SWE .nc,
  # and both files must be single-day (not a multi-day max) for a fair diff.
  plot_df      <- joined %>%
    dplyr::filter(!is.na(diff_mm)) %>%
    dplyr::rename(value = diff_mm)
  bin_breaks   <- c(-Inf, -200, -100, -50, -10, 10, 50, 100, 200, Inf)
  bin_labels   <- c("< -200", "-200 to -100", "-100 to -50", "-50 to -10",
                    "-10 to 10", "10 to 50", "50 to 100", "100 to 200", "> 200")
  pal_name     <- "RdBu"     # red = target drier, white = no change, blue = target snowier
  legend_title <- paste0("SWE difference (mm)<br>", target_date, " &minus; ", baseline_date)
} else {
  stop("map_mode must be 'percent', 'swe', or 'diff'")
}

cat(sprintf("\nValue summary (mode = %s):\n", map_mode))
print(summary(plot_df$value))

# ---- Convert to sf and clip to Mackenzie Basin -------------------------------

proj <- '+proj=longlat +datum=WGS84'
sd_values_sf <- sf::st_as_sf(plot_df,
                             coords = c("Longitude", "Latitude"),
                             crs = proj)

Mack <- sf::st_read(paste0(user, "/Documents/Shapefiles/MackenzieRiverBasin_FDA.shp"),
                    layer = "MackenzieRiverBasin_FDA")
Mack <- sf::st_transform(Mack, sp::CRS(proj))
Mack <- sf::st_zm(Mack)

if (clip_ERA5) {
  sd_values_sf <- sf::st_intersection(sd_values_sf, Mack)
}

# ---- Bin the mapped value ----------------------------------------------------

sd_values_sf <- sd_values_sf %>%
  dplyr::mutate(
    Bin = cut(value,
              breaks = bin_breaks,
              include.lowest = TRUE,
              labels = bin_labels)
  )

PerCol_gridded <- leaflet::colorFactor(palette = pal_name,
                                       domain  = sd_values_sf$Bin)
# Derive legend colours from the palette so they always match the map
legend_colors <- PerCol_gridded(levels(sd_values_sf$Bin))

# ---- Build leaflet map -------------------------------------------------------

map <- leaflet::leaflet() %>%
  add_basemap() %>%
  leaflet::addCircleMarkers(
    data        = sd_values_sf,
    fillColor   = ~PerCol_gridded(Bin),
    fillOpacity = 1,
    color       = ~PerCol_gridded(Bin),
    weight      = 0,
    opacity     = 1,
    radius      = 1
  ) %>%
  leaflet::addScaleBar()

if (mack_basin_outline) {
  map <- map %>%
    leaflet::addPolygons(data = Mack, color = "grey", weight = 2,
                         opacity = 0.5, fillOpacity = 0,
                         group = "Mackenzie Basin")
}

if (add_legend) {
  map <- map %>%
    leaflet::addLegend(
      position = "bottomleft",
      colors   = legend_colors,
      labels   = bin_labels,
      title    = legend_title,
      opacity  = 1
    )
}

if (add_basin == TRUE){
  
 Liard <- sf::st_read(dsn = paste0(shapefile_path, 
                                               substr("10ED002", start = 1, stop = 2), "/",
                                               "10ED002", "/",
                                               "10ED002", 
                                               "_DrainageBasin_BassinDeDrainage.shp"), 
                                  layer = paste0("10ED002", 
                                                 "_DrainageBasin_BassinDeDrainage"))
  Liard <- sf::st_transform(Liard, sp::CRS(proj))
  Liard <- sf::st_zm(Liard)
  
  Nahanni <- sf::st_read(dsn = paste0(shapefile_path, 
                                    substr("10EB001", start = 1, stop = 2), "/",
                                    "10EB001", "/",
                                    "10EB001", 
                                    "_DrainageBasin_BassinDeDrainage.shp"), 
                       layer = paste0("10EB001", 
                                      "_DrainageBasin_BassinDeDrainage"))
  Nahanni <- sf::st_transform(Nahanni, sp::CRS(proj))
  Nahanni <- sf::st_zm(Nahanni)
  
  
  # map <- map %>%
  #   leaflet::addPolygons(data = Nahanni, color = "black", weight = 2,
  #                        fill = FALSE, opacity = 1, group = "basin outline") %>%
  #   leaflet::addPolygons(data = Liard, color = "red", weight = 2,
  #                        fill = FALSE, opacity = 1, group = "basin outline")
  
  if (mack_basin_outline == TRUE){
    map <- map %>%
      leaflet::addPolygons(data = Mack, color = "grey", weight = 2, opacity = 0.5, fillOpacity = 0, group = "Mackenzie Basin")
  }
  
}

map

# ---- Save -------------------------------------------------------------------

if (save) {
  output_dir <- paste0(user, "/Documents/NT_Hydrology/Figures/") # maps are saved here
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  out_base <- paste0(output_dir, "map_snow_", map_mode, "_", gsub("-", "", target_date))
  htmlwidgets::saveWidget(map, file = paste0(out_base, ".html"),
                          selfcontained = TRUE)
  webshot::webshot(paste0(out_base, ".html"),
                   paste0(out_base, ".png"),
                   delay = 20, vwidth = 1000, vheight = 800, zoom = 1)
  message("Map saved to ", out_base, ".html / .png")
}

# ---- Basin-averaged value (mean SWE in "swe" mode) ---------------------------
# `value` is whatever the active map_mode produced: swe_mm, per_remaining, or diff_mm.
# Requires add_basin == TRUE so Nahanni / Liard exist.

basin_mean <- function(points_sf, basin_sf) {
  basin_sf <- sf::st_make_valid(basin_sf)
  pts      <- sf::st_filter(points_sf, basin_sf, .predicate = sf::st_within)
  # cos(latitude) weight: 0.1-deg grid cells shrink poleward, so weight by area
  lat <- sf::st_coordinates(pts)[, "Y"]
  w   <- cos(lat * pi / 180)
  data.frame(
    n           = nrow(pts),
    mean_simple = mean(pts$value, na.rm = TRUE),
    mean_areawt = stats::weighted.mean(pts$value, w, na.rm = TRUE)
  )
}

if (add_basin) {
  cat(sprintf("\nBasin-averaged value (mode = %s):\n", map_mode))
  cat("Nahanni:\n"); print(basin_mean(sd_values_sf, Nahanni))
  cat("Liard:\n");   print(basin_mean(sd_values_sf, Liard))
}

