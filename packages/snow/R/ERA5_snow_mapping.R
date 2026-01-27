#ERA5 snow mapping
#for mapping ERA5-land swe only

library(leaflet)

#opposite of %in%:
`%!in%` = Negate(`%in%`)

#bring in manual snow survey data
source(paste0(user, "/Documents/R_Scripts/Packages/snow/R/R_Oracle_Connect.R"))

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

#mapping and analysis variables
file <- "April_max_"
current_year <- 2024
selected_layer = "all"
clip_ERA5 = T
add_legend = T
mack_basin_outline = T
save = T
manual_sites = T
NWT_border = TRUE
act = c("IA", "A")
surface = c("upland")
flags = c("Y", "Sk", "P", "Sk_2")
hdensity = 0.34
ldensity = 0.1
data = md_3
exclude_sites = NA
start_year = 1995
end_year = 2025
min_year = 24

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

map <- leaflet::leaflet() %>%
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

if (manual_sites == TRUE){
  
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
      dplyr::filter(dplyr::n_distinct(year)>min_year)%>%
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
    
    table <- rbind(table, TA)
    
  }
  
    canswe <- canswe %>%
      dplyr::group_by(site) %>%
      dplyr::filter(year >= start_year,
                    year <= end_year) %>%
      dplyr::mutate(no_years = dplyr::n_distinct(year)) %>%
      dplyr::filter(no_years > min_year) %>%
      dplyr::select(site, year, swe_cm, no_years, elevation) %>%
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
      table_canswe <- rbind(table_canswe, TA_canswe)
    }
  
    #calculate percent normals
    
    table_canswe <- table_canswe %>%
      dplyr::group_by(site) %>%
      dplyr::filter(any(year == current_year)) %>%
      dplyr::mutate(pernorm = (swe_cm[year == current_year]/sitemeanswe) * 100)%>%
      dplyr::filter(year == current_year)
    
    table <- table %>%
      dplyr::group_by(Site_name) %>%
      dplyr::filter(any(year == current_year)) %>%
      dplyr::mutate(pernorm = (meanswe[year == current_year]/sitemeanswe) * 100) %>%
      dplyr::filter(year == current_year)
    
    longitude = appendvar(var1=table_canswe$site, var2 = coordinates_high_res$Site_name, var3 = coordinates_high_res$Longitude)
    latitude = appendvar(var1=table_canswe$site, var2 = coordinates_high_res$Site_name, var3 = coordinates_high_res$Latitude)
    elevation = appendvar(var1=table_canswe$site, var2 = canswe$site, var3 = canswe$elevation)
    MapTable_canswe<-cbind(table_canswe, longitude, latitude, elevation) 
    
    MapTable_canswe <- MapTable_canswe %>%
      dplyr::rename("Site_name" = "site",
                    "Longitude" = "...8",
                    "Latitude" = "...9",
                    "elevation2" = "...10")
    
    longitude = appendvar(var1=table$Site_name, var2 = coordinates_high_res$Site_name, var3 = coordinates_high_res$Longitude)
    latitude = appendvar(var1=table$Site_name, var2 = coordinates_high_res$Site_name, var3 = coordinates_high_res$Latitude)
    elevation = appendvar(var1 = table$Site_name, var2 = nwt_elevation_data$site, nwt_elevation_data$elevation)
    
    MapTable<-cbind(table, longitude, latitude, elevation)%>%
      dplyr::rename("Longitude" = "...9",
                    "Latitude" = "...10",
                    "elevation2" = "...11")
    
    MT = rbind(MapTable, MapTable_canswe) %>%
      dplyr::filter(is.na(Longitude)==F)
    
    # Create bins for mapping
    MT$Percent_Normal_Bin <- cut(MT$pernorm, 
                                 breaks = c(0, 50, 70, 90, 110, 130, 150, 500), 
                                 include.lowest = T,
                                 labels = c("<50%", "51 - 70%", "71 - 90%", "91 - 110%", 
                                            "111 - 130%", "131 - 150%", "> 151%"))
    
    
    PerCol <- leaflet::colorFactor(palette = "RdYlBu", MT$Percent_Normal_Bin, na.color = NA)
    
    map = map %>%
      leaflet::addCircleMarkers(data = MT, 
                                fillColor = ~PerCol(Percent_Normal_Bin), #
                                fillOpacity = 1,
                                label = MT$Site_name,
                                radius = 7,
                                weight = 1,
                                color = "black",
                                popup =~ paste0("Site name: ", MT$Site_name, "<br>")) #%>%
    
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
   
