#ERA5 snow mapping
#for mapping ERA5-land swe only

library(leaflet)

#variables
file <- "Jan1_max_"
current_year <- 2026

#mapping variables
selected_layer = "all"
clip_ERA5 = T
add_legend = T
mack_basin_outline = T
save = T

#set web shot path
Sys.setenv(PATH = paste0(user, "/Documents/Modelling/phantomjs/phantomjs/bin")) #can install phantomjs executable here: webshot::install_phantomjs()

ERA5_snow <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/ERA5/data/SWEmax_dataset/", file, "_combined_max_snow.csv")) %>%
  dplyr::select(-c(X)) 

#pivot dataframe
ERA5_snow <- ERA5_snow %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("Snow_"),
    names_to = "sd_year",
    names_prefix = "Snow_",
    values_to = "sd"
  ) %>%
  dplyr::rename(
    Latitude = lat,
    Longitude = lon
  )

#filter out glaciers and negative values
ERA5_snow <- ERA5_snow %>%
  dplyr::select(Latitude, Longitude, sd, sd_year)

#create average from 1995-2025 data (rolling average)
ERA5_snow <- ERA5_snow %>%
  dplyr::group_by(Longitude, Latitude) %>%
  dplyr::mutate(Snow_Avg = mean(sd[sd_year != current_year], na.rm=T))

per_norm_snow_values <- ERA5_snow %>%
  dplyr::group_by(Longitude, Latitude) %>%
  dplyr::filter(sd_year == current_year) %>%
  dplyr::mutate(per_norm = (sd/Snow_Avg)*100) %>% # calculate percent normal for given year
  dplyr::filter(is.na(per_norm)==FALSE) #filters NA values

sd_values <- per_norm_snow_values
# Convert to sf object
proj <- '+proj=longlat +datum=WGS84'
sd_values_sf <- sf::st_as_sf(sd_values, coords = c("Longitude", "Latitude"), crs = proj)

#bring in mack basin
Mack <- sf::st_read(paste0(user, "/Documents/Shapefiles/MackenzieRiverBasin_FDA.shp"),
                    layer = "MackenzieRiverBasin_FDA")
Mack <- sf::st_transform(Mack, sp::CRS(proj))
Mack <- sf::st_zm(Mack)

if(clip_ERA5 == T) {
  sd_values_sf <- sf::st_intersection(sd_values_sf, Mack)
  
  # Clip data
  if (selected_layer != "all") {
    selected_sf <- switch(selected_layer,
                          "Mackenzie Basin" = Mack_clip,
                          "Hay Basin" = Hay_valid,
                          "Liard Basin" = Liard_valid,
                          "Great Bear Basin" = GB_valid,
                          "Great Slave Lake Basin" = GSL_clip,
                          "Peel Basin" = Peel_valid,
                          "Taltson Basin" = Taltson_valid,
                          "Slave River Basin" = Slave_valid,
                          "NWT Border" = NWT,
                          NULL)
    
    if (!is.null(selected_sf)) {
      sd_values_sf <- sd_values_sf %>% dplyr::filter(sf::st_within(geometry, sf::st_geometry(selected_sf), sparse = FALSE))
    }
  }
} 

sd_values_sf <- sd_values_sf %>%
  dplyr::mutate(
    Bin = cut(
      per_norm,
      breaks = c(0, 50, 70, 90, 110, 130, 150, 500), 
      include.lowest = T,
      labels = c("< 50%", "51 - 70%", "71 - 90%", "91 - 110%", 
                 "111 - 130%", "131 - 150%", "> 151%"))
  )

PerCol_gridded <- leaflet::colorFactor(palette = "RdYlBu", sd_values_sf$Bin)

map <- leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "CartoDB")

map <- map %>%
  leaflet::addCircleMarkers(data = sd_values_sf,
                            fillColor = ~PerCol_gridded(Bin),
                            fillOpacity = 1,
                            color = ~PerCol_gridded(Bin),
                            weight = 0,
                            opacity = 1,
                            radius = 1) %>%

  leaflet::addScaleBar()

if (mack_basin_outline == TRUE){
  map <- map %>%
    leaflet::addPolygons(data = Mack, color = "grey", weight = 2, opacity = 0.5, fillOpacity = 0, group = "Mackenzie Basin")
}

  
  if(add_legend == TRUE){
    map <- map %>%
      addLegend(
        position = "bottomleft",
        colors = c("#D73027", "#FC8D59", "#FEE090","#FFFFBF","#E0F3F8","#91BFDB", "#4575B4"),
        labels = c("< 50%", "51 - 70%", "71 - 90%", "91 - 110%", 
                   "111 - 130%", "131 - 150%", "> 151%"),
        title = paste0("Snow Water Equivalent", "<br>", file, year,  "<br>", "Relative to Average"),
        opacity = 1
      )
  }

map

if(save == T){
  htmlwidgets::saveWidget(map, file = paste0("map_MRB_", file, ".html"), selfcontained = TRUE)
  webshot::webshot(paste0("map_MRB_", file, ".html"), paste0("map_MRB_", file, ".png"), delay = 20,  vwidth = 1000, vheight = 800, zoom = 1)
}
   
