# Plot 2020 max SWE over the Yamba area from ERA5-Land
#
# - Reads ERA5-Land spring max snow depth (m w.e.) from the combined CSV
# - Clips to the Yamba bounding box (matches 03_clip_swe.R)
# - Excludes grid cells over water bodies (SWE deficits due to lakes)
# - Converts to SWE (mm w.e.) and renders a leaflet map with a continuous palette

library(dplyr)
library(sf)
library(leaflet)
library(htmlwidgets)

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

user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")))

swe_csv <- paste0(user,
  "/Documents/R_Scripts/Packages/snow/data/SWEmax_dataset/spring_max_snow_depth_combined.csv")
basin_shp <- paste0(user,
  "/Documents/PhD/Yamba/basinmaker_project/basinmaker_out/finalcat_info.shp")

basemap <- "carto_key" # background map. "carto_key" = CARTO light, no labels, using your own CARTO_API_KEY in .Renviron
# (how to get one: README section 6d). No key? use e.g. "Esri.WorldGrayCanvas" (full list: names(leaflet::providers))

# check the background map setting
if (basemap == "carto_key" && !nzchar(Sys.getenv("CARTO_API_KEY"))) {
  stop("basemap = \"carto_key\" but there is no CARTO_API_KEY in your .Renviron.\n",
       "Add your own key (see README section 6d) and restart R, or use a background that doesn't need a key, e.g. basemap = \"Esri.WorldGrayCanvas\".", call. = FALSE)
}
if (basemap != "carto_key" && !(basemap %in% names(leaflet::providers))) {
  stop("basemap = \"", basemap, "\" is not a leaflet background map. Check the spelling, or run names(leaflet::providers) to see the options.", call. = FALSE)
}

# Yamba bounding box (WGS84) - matches 03_clip_swe.R
LAT_MIN <- 64.5; LAT_MAX <- 65.5
LON_MIN <- -112.0; LON_MAX <- -110.0

# m w.e. -> mm w.e.
SWE_CONVERSION <- 1000.0

YEAR <- 2020
year_col <- paste0("Snow_", YEAR)

# Grid cells masked because they sit over lakes (artificially low SWE)
excluded_cells <- data.frame(
  lat = c(64.9, 64.9, 64.9, 64.9,
          65.0, 65.0, 65.0, 65.0, 65.0, 65.0),
  lon = c(-111.4, -111.3, -111.2, -111.1,
          -111.1, -111.2, -111.3, -111.4, -111.5, -111.6)
)

# Load + clip
swe_raw <- read.csv(swe_csv)
if ("X" %in% names(swe_raw)) swe_raw$X <- NULL

swe_yamba <- swe_raw %>%
  dplyr::filter(lat >= LAT_MIN, lat <= LAT_MAX,
                lon >= LON_MIN, lon <= LON_MAX) %>%
  dplyr::select(lon, lat, dplyr::all_of(year_col)) %>%
  dplyr::rename(SnowDepth_m = !!year_col) %>%
  dplyr::mutate(SWE_mm = SnowDepth_m * SWE_CONVERSION) %>%
  dplyr::filter(!is.na(SWE_mm))

# Drop lake-affected cells (match on rounded lat/lon to avoid float issues)
swe_yamba <- swe_yamba %>%
  dplyr::anti_join(
    excluded_cells %>% dplyr::mutate(lat = round(lat, 2), lon = round(lon, 2)),
    by = c("lat" = "lat", "lon" = "lon")
  )

cat(sprintf("Plotting %d grid cells. SWE range: %.1f-%.1f mm\n",
            nrow(swe_yamba), min(swe_yamba$SWE_mm), max(swe_yamba$SWE_mm)))

# To sf for mapping
proj <- "+proj=longlat +datum=WGS84"
swe_sf <- sf::st_as_sf(swe_yamba, coords = c("lon", "lat"), crs = proj)

# Yamba basin outline - dissolve subbasins to a single polygon for clipping
basin <- sf::st_read(basin_shp, quiet = TRUE) %>%
  sf::st_transform(proj) %>%
  sf::st_zm() %>%
  sf::st_make_valid()
basin_outline <- sf::st_union(basin)

swe_sf <- sf::st_intersection(swe_sf, basin_outline)
cat(sprintf("After basin clip: %d grid cells\n", nrow(swe_sf)))

swe_bins <- c(130, 134, 138, 142, 146, 150, Inf)
pal <- leaflet::colorBin(palette = "RdYlBu", domain = swe_sf$SWE_mm,
                         bins = swe_bins, reverse = FALSE)

map <- leaflet::leaflet(swe_sf) %>%
  add_basemap() %>%
  leaflet::addPolygons(data = basin_outline, fill = FALSE,
                       color = "grey", weight = 2, opacity = 1) %>%
  leaflet::addCircleMarkers(
    fillColor = ~pal(SWE_mm),
    fillOpacity = 0.95,
    color = "black",
    weight = 1,
    opacity = 1,
    radius = 7,
    popup = ~sprintf("SWE: %.1f mm<br>lat: %.2f<br>lon: %.2f",
                     SWE_mm, sf::st_coordinates(swe_sf)[, 2],
                     sf::st_coordinates(swe_sf)[, 1])
  ) %>%
  leaflet::addLegend(
    position = "bottomright",
    pal = pal,
    values = swe_sf$SWE_mm,
    title = paste0("SWE (mm w.e.)<br>", YEAR, " spring max"),
    opacity = 1
  ) %>%
  leaflet::addScaleBar()

map

Sys.setenv(PATH = paste0(user, "/Documents/Modelling/Phantomjs/phantomjs/bin"))
htmlwidgets::saveWidget(map, file = paste0(save_path, "/map.html"), selfcontained = TRUE)
webshot::webshot(paste0(save_path, "/map.html"),
                 paste0(save_path, "/yamba_swe_map", ".png"),
                 delay = 5, vwidth = 1000, vheight = 800, zoom = 2)

out_html <- paste0(user,
  "/Documents/R_Scripts/Packages/snow/data/yamba_swe_", YEAR, ".html")
htmlwidgets::saveWidget(map, file = out_html, selfcontained = TRUE)
cat(sprintf("Saved map to %s\n", out_html))
