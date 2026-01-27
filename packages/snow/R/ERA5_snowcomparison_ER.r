#ERA5 and manual snow survey trend comparison and correlation

#req packages
library(dplyr)
library(trend)
library(spearmanCI)
library(lwgeom)
library(sf)
library(ggplot2)
library(broom)
library(tidyr)

#specify user
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")))

# Define `%>%` operator in current environment
`%>%` <- magrittr::`%>%`

# Call in source functions from other scripts
source(paste0(user, "/Documents/R_Scripts/Packages/snow/R/ERA5_snowtrends_map_hourly_functions.R"))

#opposite of %in%:
`%!in%` = Negate(`%in%`)

#set save path for figures
savepath <- paste0(user, "/Documents/NT_Hydrology/Figures")

#bring in data
#ERA5-Land
era5data <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/trendresults_serialautocorr_hourly_95_24_MRB.csv"))

# Load NWT manual survey data (sites)
# Can only use this as a GNWT employee
# source(paste0(user, "/Documents/R_Scripts/Packages/snow/R/R_Oracle_Connect.R"))

# bring in sites data from the /data folder of this package
sites <- readRDS(paste0(user,"/Documents/R_Scripts/Packages/snow/data/sites.rds"))

#data and plotting variables
proj <- '+proj=longlat +datum=WGS84'
crs_proj <- 3347  # NAD83 / Statistics Canada Lambert (projected, meters)

#bring in MT_tbl from ERA5_snowtrends_map_hourly.R
MT <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/MRB_snowsurvey_trends.csv")) 
MT <- MT %>%
  dplyr::select(-X)

era5data <- era5data %>%
  dplyr::select(-X) %>%
  dplyr::rename(lat = Latitude,
                lon = Longitude)

# Match each point in data1 to the closest point in data2
matched_data <- era5data %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    closest_point = list(
      MT %>%
        dplyr::mutate(
          distance = haversine_dist(
            lat, lon,
            dplyr::cur_data()$Latitude, dplyr::cur_data()$Longitude
          )
        ) %>%
        dplyr::slice_min(distance, n = 1, with_ties = FALSE) %>%
        dplyr::rename(
          MT_Latitude  = Latitude,
          MT_Longitude = Longitude
        )
    )
  ) %>%
  tidyr::unnest(closest_point)

#optional write to csv
#write.csv(matched_data, paste0(user, "/Documents/R_Scripts/Packages/snow/data/era5_hourly_manual_corr_adjswe_canswe.csv")) 
#matched_data <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/era5_hourly_manual_corr_adjswe_canswe.csv"))

#plot relationship
data_table <- data.frame()

for (i in unique(matched_data$Site_name)){
  data <- matched_data %>%
    dplyr::filter(Site_name == i) %>%
    dplyr::mutate(rank = rank(distance, na.last = NA)) %>%  # Add rank column
    dplyr::filter(rank == min(rank, na.rm=T)) 
  
  if(length(data$lon)>1){
  best_trend <- data %>%
    dplyr::filter(P_value == min(P_value, na.rm = TRUE))
  
  if(length(best_trend$lon)>1){
    best_trend <- best_trend %>%
      slice(1)
  }
  }else{
    best_trend <- data
  }
  
  data_table <- rbind(data_table, best_trend)
}


Data_table <- unique(data_table) %>%
  dplyr::mutate(
    Sen_slope = Sen_slope * 10000,  # unit adjustment
    NormSen_slope = (Sen_slope / (meanmaxswe * 1000)) * 100,
    NormMagnitude = (Magnitude / (sitemeanswe * 10)) * 100
  )

#optional write out
#write.csv(Data_table, paste0(user, "/Documents/R_Scripts/Packages/snow/data/ERA5_manual_comparison.csv"), row.names = F)
#Data_table <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/ERA5_manual_comparison.csv"))

NWTsites <- unique(sites$site_name)

Data_table <- Data_table %>%
  dplyr::mutate(
    group = dplyr::if_else(Site_name %in% NWTsites, "NWT", "Other")
  )

# Shared axes (computed from all data)
shared_limits <- range(
  c(Data_table$NormSen_slope, Data_table$NormMagnitude),
  na.rm = TRUE
)

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

#Lower slave

delta_slave <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/07NC003_DrainageBasin_BassinDeDrainage.shp"),
                           layer = "07NC003_DrainageBasin_BassinDeDrainage")
delta_slave <- sf::st_transform(delta_slave, crs_proj) # Change the projection
delta_slave <- sf::st_zm(delta_slave)
delta_slave <- sf::st_make_valid(delta_slave)

#join peace and athabasca

peace_AB <- sf::st_union(Peace, AB)
peace_AB <- sf::st_transform(peace_AB, crs_proj)
peace_AB <- sf::st_make_valid(peace_AB) 

# Make sure both shapefiles are valid, same CRS, polygon-only

delta2 <- st_make_valid(st_zm(delta_slave, drop=TRUE, what="ZM")) |>
  st_collection_extract("POLYGON") |>
  st_cast("MULTIPOLYGON")

cutter2 <- st_make_valid(st_zm(peace_AB, drop=TRUE, what="ZM")) |>
  st_collection_extract("POLYGON") |>
  st_cast("MULTIPOLYGON")

stopifnot(st_crs(delta2) == st_crs(cutter2))

# Reduce numerical noise 

delta2  <- st_set_precision(delta2, 1)  |> st_make_valid()
cutter2 <- st_set_precision(cutter2, 1) |> st_make_valid()

# Snap with small tolerance (5m)

tol <- 5
cutter_snap <- sf::st_snap(cutter2, delta2, tolerance = tol) |> st_make_valid()

# Union the cutter to avoid multipart boundary conflicts

cutter_u <- st_union(cutter_snap) |> st_make_valid()

# Difference

lower_slave <- st_difference(delta2, cutter_u) |> st_make_valid()

parts <- st_cast(lower_slave, "POLYGON")
parts$area_m2 <- st_area(parts)

# Look at the smallest polygons (these are almost always slivers)
parts |> 
  sf::st_drop_geometry() |>
  dplyr::arrange(area_m2) |>
  head(10)

thresh_m2 <- units::set_units(1e4, "m^2")  # 1 km^2 (adjust if needed)

keep <- parts[parts$area_m2 >= thresh_m2, ]
lower_slave_clean <- sf::st_union(keep) |> sf::st_make_valid()

lower_slave_clean <- lower_slave_clean |>
  sf::st_zm(drop = TRUE, what = "ZM") |>
  sf::st_make_valid() |>
  sf::st_cast("MULTIPOLYGON") |>
  sf::st_transform(proj)

#write out lower slave data
sf::st_write(
  lower_slave_clean,
  "lower_slave_clean.shp",
  delete_dsn = TRUE
)

lower_slave_clean <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/lower_slave_clean.shp"),
                                 layer = "lower_slave_clean")

# transform for leaflet

delta_ll <- sf::st_transform(delta2, proj)
lower_ll <- sf::st_transform(lower_slave_clean, proj)

#Assign each lat/long pair in Data_table to basin
pts <- sf::st_as_sf(
  Data_table,
  coords = c("MT_Longitude", "MT_Latitude"),
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


if (sf::st_crs(basins_sf) != sf::st_crs(pts)) {
  pts <- sf::st_transform(pts, sf::st_crs(basins_sf))
}

pts_basin <- sf::st_join(pts, basins_sf, join = sf::st_within, left = TRUE)

# If any points fall just outside boundaries (common), assign nearest basin
missing <- is.na(pts_basin$basin)
max_km <- 50

# Ensure diagnostics columns exist
pts_basin$dist_to_basin_m <- NA_real_
pts_basin$was_missing     <- FALSE
pts_basin$assigned_nearest <- FALSE

if (any(missing)) {
  pts_miss <- pts[missing, , drop = FALSE]
  
  # nearest polygon index (planar selection; OK because we verify with geodesic distance)
  nn <- sf::st_nearest_feature(pts_miss, basins_sf)
  
  # geodesic distance (meters) in lon/lat CRS
  d <- sf::st_distance(pts_miss, basins_sf[nn, ], by_element = TRUE)
  
  ok <- as.numeric(d) <= (max_km * 1000)
  
  pts_basin$basin[missing] <- ifelse(ok, basins_sf$basin[nn], NA_character_)
  pts_basin$dist_to_basin_m[missing] <- as.numeric(d)
  pts_basin$was_missing[missing] <- TRUE
  pts_basin$assigned_nearest[missing] <- ok
  
  # keep indices for mapping
  pts_basin$nn_basin_i <- NA_integer_
  pts_basin$nn_basin_i[missing] <- nn
} else {
  pts_basin$nn_basin_i <- NA_integer_
}

#assign missing points to more general groupings
#Mackenzie River main stem
MRBsites <- c("Tulita", "NORMAN WELLS A", "Norman Wells", "Fort Good Hope", "Caribou Creek")
GSL_othsites <- c("Nyarling River", "Kakisa River", "Pine Point", "Thubun Lake")
pts_basin$basin <- ifelse(is.na(pts_basin$basin == T)&pts_basin$Site_name %in%MRBsites, "Mackenzie River main stem", pts_basin$basin)
pts_basin$basin <- ifelse(is.na(pts_basin$basin == T)&pts_basin$Site_name %in%GSL_othsites, "Great Slave Lake - other", pts_basin$basin)

#compute absolute difference between trends

pts_basin <- pts_basin %>%
  dplyr::mutate(
    abs_err = abs(Sen_slope - Magnitude),
    abs_err_norm = abs(NormSen_slope - NormMagnitude)
  )

#plot differences based on basin

ggplot(pts_basin, aes(x = basin, y = abs_err_norm)) +
  geom_boxplot(outlier.alpha = 0.4) +
  geom_jitter(width = 0.15, alpha = 0.35) +
  labs(
    x = "Basin",
    y = "|% change in SWE (ERA5) − % change in SWE (manual)|",
    title = ""
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pts_basin_tbl <- pts_basin %>%
  sf::st_drop_geometry()

#write.csv(pts_basin_tbl, paste0(getwd(), "/data/pts_basin.csv"), row.names = F)

#re-create correlation plot with colours for different basins

# -----------------------------
# Correlation labels
# -----------------------------
corr_all <- stats::cor.test(
  Data_table$NormSen_slope,
  Data_table$NormMagnitude,
  method = "spearman"
)

corr_nwt <- stats::cor.test(
  Data_table$NormSen_slope[Data_table$group == "NWT"],
  Data_table$NormMagnitude[Data_table$group == "NWT"],
  method = "spearman"
)

fmt_p <- function(p) {
  if (is.na(p)) {
    "NA"
  } else if (p < 0.01) {
    "< 0.01"
  } else {
    paste0("= ", signif(p, 2))
  }
}

label_all <- paste0(
  "All sites: Spearman's \u03c1 = ",
  round(corr_all$estimate, 2),
  ", p ",
  fmt_p(corr_all$p.value)
)

label_nwt <- paste0(
  "NWT sites: Spearman's \u03c1 = ",
  round(corr_nwt$estimate, 2),
  ", p ",
  fmt_p(corr_nwt$p.value)
)

# Dynamic label placement
x_lab <- shared_limits[1] + 0.03 * diff(shared_limits)
y_top <- shared_limits[2] - 0.03 * diff(shared_limits)
y_2   <- shared_limits[2] - 0.10 * diff(shared_limits)

dt_plot <- pts_basin %>% sf::st_drop_geometry()

plot <- ggplot2::ggplot(dt_plot, ggplot2::aes(x = NormSen_slope, y = NormMagnitude)) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, linetype = "solid",
                       color = "black", linewidth = 1, fill = "lightgrey", alpha = 0.6) +
  ggplot2::geom_smooth(data = dplyr::filter(dt_plot, group == "NWT"), method = "lm", se = TRUE,
                      color = "dodgerblue4", fill = "dodgerblue3",
                      linewidth = 1.2, alpha = 0.35, size = 7) +
  ggplot2::geom_point(data = dt_plot, ggplot2::aes(fill = basin, color = basin),
                      size = 7, shape = 21, 
                      stroke =  1, 
                      alpha = 0.5) +
  ggplot2::geom_point(data = dplyr::filter(dt_plot, group == "NWT"), ggplot2::aes(fill = basin),
                      size = 7, shape = 21, color = "black", 
                      stroke =  1, 
                      alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::labs(
    title = "",
    x = "% change in ERA5-Land SWE/decade",
    y = "% change in manual snow survey SWE/decade",
    fill = "Basin"
  ) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted",
                       color = "black", linewidth = 1) +
  ggplot2::scale_x_continuous(limits = c(-15, 20)) +
  ggplot2::scale_y_continuous(limits = shared_limits) +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 14),
    axis.title = ggplot2::element_text(size = 18)
  ) +
  
  # Correlation annotations
  ggplot2::annotate(
    "text",
    x = -14,
    y = y_top,
    label = label_all,
    hjust = 0,
    vjust = 1,
    size = 5.5,
    color = "black"
  ) +
  ggplot2::annotate(
    "text",
    x = -14,
    y = y_2,
    label = label_nwt,
    hjust = 0,
    vjust = 1,
    size = 5.5,
    color = "black"
  ) 

plot

ggplot2::ggsave(filename = "corrplot_highres.png", dpi = 900, path = savepath)

#basin stats

basin_stats <- dt_plot %>%
  dplyr::filter(!is.na(basin), !is.na(NormSen_slope), !is.na(NormMagnitude)) %>%
  dplyr::group_by(basin) %>%
  dplyr::group_modify(~{
    d <- .x
    
    fit <- lm(NormMagnitude ~ NormSen_slope, data = d)
    sm  <- summary(fit)
    
    slope_row <- broom::tidy(fit) %>% dplyr::filter(term == "NormSen_slope")
    gl <- broom::glance(fit)
    
    ct <- cor.test(d$NormSen_slope, d$NormMagnitude, method = "spearman", adj_r2 = gl$adj.r.squared)
    
    dplyr::tibble(
      n              = nrow(d),
      slope          = slope_row$estimate,
      slope_p_value  = slope_row$p.value,
      r2             = gl$r.squared,
      rho_pearson    = unname(ct$estimate),
      rho_p_value    = ct$p.value
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(n))

basin_stats

#boxplots

dt <- dt_plot %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    # SWE in mm
    ERA5_meanmax_mm   = meanmaxswe * 1000,
    Manual_meanmax_mm = sitemeanswe * 10,
    
    # Trends (keep as-is unless you have a known unit conversion)
    ERA5_trend   = Sen_slope,
    Manual_trend = Magnitude
  )


swe_long <- dt %>%
  dplyr::select(basin, ERA5_meanmax_mm, Manual_meanmax_mm) %>%
  tidyr::pivot_longer(
    cols = c(ERA5_meanmax_mm, Manual_meanmax_mm),
    names_to = "source",
    values_to = "meanmax_swe_mm"
  ) %>%
  dplyr::mutate(
    source = dplyr::recode(
      source,
      ERA5_meanmax_mm   = "ERA5-Land",
      Manual_meanmax_mm = "Manual surveys"
    )
  )

p_swe <- ggplot2::ggplot(swe_long, ggplot2::aes(x = basin, y = meanmax_swe_mm, fill = source)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  ggplot2::theme_classic() +
  #ggplot2::geom_jitter() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(
    x = "Basin",
    y = "Mean end-of-season SWE (mm)",
    fill = NULL
  ) +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12),
    legend.text =  ggplot2::element_text(size = 12) )

p_swe

p_swe_basin <- p_swe

ggplot2::ggsave(filename = "p_swe_basin.png", dpi = 900, path = savepath)

trend_long <- dt %>%
  dplyr::select(basin, ERA5_trend, Manual_trend) %>%
  tidyr::pivot_longer(
    cols = c(ERA5_trend, Manual_trend),
    names_to = "source",
    values_to = "trend"
  ) %>%
  dplyr::mutate(
    source = dplyr::recode(
      source,
      ERA5_trend   = "ERA5-Land",
      Manual_trend = "Manual surveys"
    )
  )

p_trend <- ggplot2::ggplot(trend_long, ggplot2::aes(x = basin, y = trend, fill = source)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  ggplot2::theme_classic() +
  #ggplot2::geom_jitter() +
  ggplot2::geom_abline(slope = 0, linetype = "dashed") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(
    x = "Basin",
    y = "SWE trend (mm/decade)",
    fill = NULL
  )+
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12),
    legend.text =  ggplot2::element_text(size = 12) )

p_trend

p_trend_basin <- p_trend

ggplot2::ggsave(filename = "p_trend_basin.png", dpi = 900, path = savepath)

p_error <- ggplot2::ggplot(dt, ggplot2::aes(x = basin, y = abs_err_norm, fill = basin)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  #ggplot2::geom_jitter() +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(
    x = "Basin",
    y = "|% change in SWE (ERA5) − % change in SWE (manual)|",
    fill = NULL
  )+
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12),
    legend.text =  ggplot2::element_text(size = 12) )

p_error

p_error_basin <- p_error

ggplot2::ggsave(filename = "p_error_basin.png", dpi = 900, path = savepath)

means_swe <- swe_long %>%
  dplyr::group_by(basin, source) %>%
  dplyr::summarise(
    n = sum(!is.na(meanmax_swe_mm)),
    mean_meanmax_swe_mm = mean(meanmax_swe_mm, na.rm = TRUE),
    .groups = "drop"
  )

means_trend <- trend_long %>%
  dplyr::group_by(basin, source) %>%
  dplyr::summarise(
    n = sum(!is.na(trend)),
    mean_trend = mean(trend, na.rm = TRUE),
    .groups = "drop"
  )

means_error <- dt %>%
  dplyr::group_by(basin) %>%
  dplyr::summarise(
    n = sum(!is.na(abs_err_norm)),
    mean_error = mean(abs_err_norm, na.rm = TRUE),
    .groups = "drop"
  )

means_swe
means_trend
means_error

#differences between ERA5-Land and SS when grouped by basin

basin <- unique(dt$basin)

wilcox.test.results <- data.frame(
  test_results_p = numeric(),
  basin = character()
)

for (i in basin){
  dt_basin <- dt[dt$basin == i, ]
  
  test <- wilcox.test(dt_basin$Manual_meanmax_mm, dt_basin$ERA5_meanmax_mm, alternative = "two.sided")
  
  wilcox.test.results <- wilcox.test.results %>%
    dplyr::add_row(
      basin = i,
      test_results_p = test$p.value
    )
  
}

# #Similar analysis as above but with eco/region/zone/province

# 1) read in table
dt0 <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/ERA5_manual_comparison.csv")) 
dt0 <- dt0 %>%
  dplyr::mutate(
    ERA5_meanmax_mm   = meanmaxswe *1000,
    Manual_meanmax_mm = sitemeanswe * 10
  )

# 2) Convert to points
pts <- as_points_sf(dt0, lon_col = "MT_Longitude", lat_col = "MT_Latitude", crs = proj)

# 3) Join to a classification polygon layer - choose "mackenzie_ecozonesv2"(layer) & "ECOZONE_NA" (layer_col) or
#"mackenzie_ecoprovinces" (layer) & "ECOPROVI_1" (layer_col) or "mackenzie_ecoregions" & "ECOREGION1"
layer <- "mackenzie_ecozonesv2"
layer_col <- "ECOZONE_NA"
ecozones <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/", layer, ".shp")) %>%
  sf::st_transform(proj)

pts_ecoz <- attach_region(
  pts_sf = pts,
  regions_sf = ecozones,
  region_name_col = layer_col,
  max_km = 50,
  region_out_col = layer_col
)

# 4) Add error metrics and drop geometry for plotting/stats
dt_ecoz <- pts_ecoz %>%
  add_error_metrics() %>%
  sf::st_drop_geometry()

#boxplots

dt <- dt_ecoz %>%
  dplyr::mutate(
    # SWE in mm
    ERA5_meanmax_mm   = meanmaxswe * 1000,
    Manual_meanmax_mm = sitemeanswe * 10,
    
    # Trends (keep as-is unless you have a known unit conversion)
    ERA5_trend   = Sen_slope,
    Manual_trend = Magnitude
  )


swe_long <- dt %>%
  dplyr::select(ECOZONE_NA, ERA5_meanmax_mm, Manual_meanmax_mm) %>%
  tidyr::pivot_longer(
    cols = c(ERA5_meanmax_mm, Manual_meanmax_mm),
    names_to = "source",
    values_to = "meanmax_swe_mm"
  ) %>%
  dplyr::mutate(
    source = dplyr::recode(
      source,
      ERA5_meanmax_mm   = "ERA5-Land",
      Manual_meanmax_mm = "Manual surveys"
    )
  )

p_swe <- ggplot2::ggplot(swe_long, ggplot2::aes(x = ECOZONE_NA, y = meanmax_swe_mm, fill = source)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  ggplot2::theme_classic() +
  #gplot2::geom_jitter() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(
    x = "Ecozone",
    y = "Mean end-of-season SWE (mm)",
    fill = NULL
  ) +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12),
    legend.text =  ggplot2::element_text(size = 12) )

p_swe

ggplot2::ggsave(filename = "p_swe.png", dpi = 900, path = savepath)

trend_long <- dt %>%
  dplyr::select(ECOZONE_NA, ERA5_trend, Manual_trend) %>%
  tidyr::pivot_longer(
    cols = c(ERA5_trend, Manual_trend),
    names_to = "source",
    values_to = "trend"
  ) %>%
  dplyr::mutate(
    source = dplyr::recode(
      source,
      ERA5_trend   = "ERA5-Land",
      Manual_trend = "Manual surveys"
    )
  )

p_trend <- ggplot2::ggplot(trend_long, ggplot2::aes(x = ECOZONE_NA, y = trend, fill = source)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  ggplot2::theme_classic() +
  #ggplot2::geom_jitter() +
  ggplot2::geom_abline(slope = 0, linetype = "dashed") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(
    x = "Ecozone",
    y = "SWE trend (mm/decade)",
    fill = NULL
  )+
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12),
    legend.text =  ggplot2::element_text(size = 12) )

p_trend

ggplot2::ggsave(filename = "p_trend.png", dpi = 900, path = savepath)

p_error <- ggplot2::ggplot(dt, ggplot2::aes(x = ECOZONE_NA, y = abs_err_norm, fill = ECOZONE_NA)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  #ggplot2::geom_jitter() +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(
    x = "Ecozone",
    y = "|% change in SWE (ERA5) − % change in SWE (manual)|",
    fill = NULL
  )+
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12),
    legend.text =  ggplot2::element_text(size = 12) )

p_error

ggplot2::ggsave(filename = "p_error.png", dpi = 900, path = savepath)

means_swe <- swe_long %>%
  dplyr::group_by(ECOZONE_NA, source) %>%
  dplyr::summarise(
    n = sum(!is.na(meanmax_swe_mm)),
    mean_meanmax_swe_mm = mean(meanmax_swe_mm, na.rm = TRUE),
    .groups = "drop"
  )

means_trend <- trend_long %>%
  dplyr::group_by(ECOZONE_NA, source) %>%
  dplyr::summarise(
    n = sum(!is.na(trend)),
    mean_trend = mean(trend, na.rm = TRUE),
    .groups = "drop"
  )

means_error <- dt %>%
  dplyr::group_by(ECOZONE_NA) %>%
  dplyr::summarise(
    n = sum(!is.na(abs_err_norm)),
    mean_error = mean(abs_err_norm, na.rm = TRUE),
    .groups = "drop"
  )

means_swe
means_trend
means_error

#differences between ERA5-Land and SS when grouped by ecozone

ecozone <- unique(dt_ecoz$ECOZONE_NA)

wilcox.test.results <- data.frame(
  test_results_p = numeric(),
  ecozone = character()
)

for (i in ecozone){
  dt_eco <- dt[dt_ecoz$ECOZONE_NA == i, ]
  
  test <- wilcox.test(dt_eco$Manual_meanmax_mm, dt_eco$ERA5_meanmax_mm, alternative = "two.sided")
  
  wilcox.test.results <- wilcox.test.results %>%
    dplyr::add_row(
      ecozone = i,
      test_results_p = test$p.value
    )
  
}

# #Similar analysis as above but with eco/region/zone/province

# 1) read in table
dt0 <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/ERA5_manual_comparison.csv")) 
  dt0 <- dt0 %>%
  dplyr::mutate(
    ERA5_meanmax_mm   = meanmaxswe *1000,
    Manual_meanmax_mm = sitemeanswe * 10
  )

# 2) Convert to points
pts <- as_points_sf(dt0, lon_col = "MT_Longitude", lat_col = "MT_Latitude", crs = proj)

# 3) Join to a classification polygon layer - choose "mackenzie_ecozonesv2"(layer) & "ECOZONE_NA" (layer_col) or
#"mackenzie_ecoprovinces" (layer) & "ECOPROVI_1" (layer_col) or "mackenzie_ecoregions" & "ECOREGION1"
layer <- "mackenzie_ecoregions"
layer_col <- "ECOREGION1"
ecozones <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/", layer, ".shp")) %>%
  sf::st_transform(proj)

pts_ecoz <- attach_region(
  pts_sf = pts,
  regions_sf = ecozones,
  region_name_col = layer_col,
  max_km = 50,
  region_out_col = layer_col
)

# 4) Add error metrics and drop geometry for plotting/stats
dt_ecoz <- pts_ecoz %>%
  add_error_metrics() %>%
  sf::st_drop_geometry()

#boxplots

dt <- dt_ecoz %>%
  dplyr::mutate(
    # SWE in mm
    ERA5_meanmax_mm   = meanmaxswe * 1000,
    Manual_meanmax_mm = sitemeanswe * 10,
    
    # Trends (keep as-is unless you have a known unit conversion)
    ERA5_trend   = Sen_slope,
    Manual_trend = Magnitude
  )


swe_long <- dt %>%
  dplyr::select(ECOREGION1, ERA5_meanmax_mm, Manual_meanmax_mm) %>%
  tidyr::pivot_longer(
    cols = c(ERA5_meanmax_mm, Manual_meanmax_mm),
    names_to = "source",
    values_to = "meanmax_swe_mm"
  ) %>%
  dplyr::mutate(
    source = dplyr::recode(
      source,
      ERA5_meanmax_mm   = "ERA5-Land",
      Manual_meanmax_mm = "Manual surveys"
    )
  )

p_swe <- ggplot2::ggplot(swe_long, ggplot2::aes(x = ECOREGION1, y = meanmax_swe_mm, fill = source)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  ggplot2::theme_classic() +
  #ggplot2::geom_jitter() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
  ggplot2::labs(
    x = "Ecoregion",
    y = "Mean end-of-season SWE (mm)",
    fill = NULL
  ) +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12),
    legend.text =  ggplot2::element_text(size = 12) )

p_swe_region <- p_swe

ggplot2::ggsave(filename = "p_swe_region.png", dpi = 900, path = savepath)

trend_long <- dt %>%
  dplyr::select(ECOREGION1, ERA5_trend, Manual_trend) %>%
  tidyr::pivot_longer(
    cols = c(ERA5_trend, Manual_trend),
    names_to = "source",
    values_to = "trend"
  ) %>%
  dplyr::mutate(
    source = dplyr::recode(
      source,
      ERA5_trend   = "ERA5-Land",
      Manual_trend = "Manual surveys"
    )
  )

p_trend <- ggplot2::ggplot(trend_long, ggplot2::aes(x = ECOREGION1, y = trend, fill = source)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::geom_abline(slope = 0, linetype = "dashed") +
  #ggplot2::geom_jitter() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
  ggplot2::labs(
    x = "Ecoregion",
    y = "SWE trend (mm/decade)",
    fill = NULL
  )+
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12),
    legend.text =  ggplot2::element_text(size = 12) )

p_trend_region <- p_trend

ggplot2::ggsave(filename = "p_trend_region.png", dpi = 900, path = savepath)

p_error <- ggplot2::ggplot(dt, ggplot2::aes(x = ECOREGION1, y = abs_err_norm, fill = ECOREGION1)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  #ggplot2::geom_jitter() +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
  ggplot2::labs(
    x = "Ecoregion",
    y = "|% change in SWE (ERA5) − % change in SWE (manual)|",
    fill = NULL
  )+
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12) )
    #,
    #legend.text =  ggplot2::element_text(size = 12) )

p_error_region <- p_error

ggplot2::ggsave(filename = "p_error_region.png", dpi = 900, path = savepath)

means_swe <- swe_long %>%
  dplyr::group_by(ECOREGION1, source) %>%
  dplyr::summarise(
    n = sum(!is.na(meanmax_swe_mm)),
    mean_meanmax_swe_mm = mean(meanmax_swe_mm, na.rm = TRUE),
    .groups = "drop"
  )

means_trend <- trend_long %>%
  dplyr::group_by(ECOREGION1, source) %>%
  dplyr::summarise(
    n = sum(!is.na(trend)),
    mean_trend = mean(trend, na.rm = TRUE),
    .groups = "drop"
  )

means_error <- dt %>%
  dplyr::group_by(ECOREGION1) %>%
  dplyr::summarise(
    n = sum(!is.na(abs_err_norm)),
    mean_error = mean(abs_err_norm, na.rm = TRUE),
    .groups = "drop"
  )

means_swe
means_trend
means_error

#differences between ERA5-Land and SS when grouped by ecoregion

ecoregion <- unique(dt_ecoz$ECOREGION1)

wilcox.test.results <- data.frame(
  test_results_p = numeric(),
  ecoregion = character()
)

for (i in ecoregion){
  dt_eco <- dt[dt_ecoz$ECOREGION1 == i, ]
  
  if(dplyr::n_distinct(dt_eco$Manual_meanmax_mm) <= 2) next
  
  test <- wilcox.test(dt_eco$Manual_meanmax_mm, dt_eco$ERA5_meanmax_mm, alternative = "two.sided")
  
  wilcox.test.results <- wilcox.test.results %>%
    dplyr::add_row(
      ecoregion = i,
      test_results_p = test$p.value
    )
  
}

# #Similar analysis as above but with eco/region/zone/province

# 1) read in table
dt0 <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/ERA5_manual_comparison.csv")) 
dt0 <- dt0 %>%
  dplyr::mutate(
    ERA5_meanmax_mm   = meanmaxswe *1000,
    Manual_meanmax_mm = sitemeanswe * 10
  )

# 2) Convert to points
pts <- as_points_sf(dt0, lon_col = "MT_Longitude", lat_col = "MT_Latitude", crs = proj)

# 3) Join to a classification polygon layer - choose "mackenzie_ecozonesv2"(layer) & "ECOZONE_NA" (layer_col) or
#"mackenzie_ecoprovinces" (layer) & "ECOPROVI_1" (layer_col) or "mackenzie_ecoregions" & "ECOREGION1"
layer <- "mackenzie_ecoprovinces"
layer_col <- "ECOPROVI_1"
ecozones <- sf::st_read(paste0(user, "/Documents/R_Scripts/Packages/snow/data/Shapefiles/", layer, ".shp")) %>%
  sf::st_transform(proj)

pts_ecoz <- attach_region(
  pts_sf = pts,
  regions_sf = ecozones,
  region_name_col = layer_col,
  max_km = 50,
  region_out_col = layer_col
)

# 4) Add error metrics and drop geometry for plotting/stats
dt_ecoz <- pts_ecoz %>%
  add_error_metrics() %>%
  sf::st_drop_geometry()

#boxplots

dt <- dt_ecoz %>%
  dplyr::mutate(
    # SWE in mm
    ERA5_meanmax_mm   = meanmaxswe * 1000,
    Manual_meanmax_mm = sitemeanswe * 10,
    
    # Trends (keep as-is unless you have a known unit conversion)
    ERA5_trend   = Sen_slope,
    Manual_trend = Magnitude
  )


swe_long <- dt %>%
  dplyr::select(ECOPROVI_1, ERA5_meanmax_mm, Manual_meanmax_mm) %>%
  tidyr::pivot_longer(
    cols = c(ERA5_meanmax_mm, Manual_meanmax_mm),
    names_to = "source",
    values_to = "meanmax_swe_mm"
  ) %>%
  dplyr::mutate(
    source = dplyr::recode(
      source,
      ERA5_meanmax_mm   = "ERA5-Land",
      Manual_meanmax_mm = "Manual surveys"
    )
  )

p_swe <- ggplot2::ggplot(swe_long, ggplot2::aes(x = ECOPROVI_1, y = meanmax_swe_mm, fill = source)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  ggplot2::theme_classic() +
  #ggplot2::geom_jitter() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(
    x = "Ecoprovince",
    y = "Mean end-of-season SWE (mm)",
    fill = NULL
  ) +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12),
    legend.text =  ggplot2::element_text(size = 12) )

p_swe_ecoprov <- p_swe

p_swe_ecoprov

ggplot2::ggsave(filename = "p_swe_ecoprov.png", dpi = 900, path = savepath)

trend_long <- dt %>%
  dplyr::select(ECOPROVI_1, ERA5_trend, Manual_trend) %>%
  tidyr::pivot_longer(
    cols = c(ERA5_trend, Manual_trend),
    names_to = "source",
    values_to = "trend"
  ) %>%
  dplyr::mutate(
    source = dplyr::recode(
      source,
      ERA5_trend   = "ERA5-Land",
      Manual_trend = "Manual surveys"
    )
  )

p_trend <- ggplot2::ggplot(trend_long, ggplot2::aes(x = ECOPROVI_1, y = trend, fill = source)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::geom_abline(slope = 0, linetype = "dashed") +
  #ggplot2::geom_jitter() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(
    x = "Ecoprovince",
    y = "SWE trend (mm/decade)",
    fill = NULL
  )+
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12),
    legend.text =  ggplot2::element_text(size = 12) )

p_trend_ecoprov <- p_trend

p_trend_ecoprov

ggplot2::ggsave(filename = "p_trend_ecoprov.png", dpi = 900, path = savepath)

p_error <- ggplot2::ggplot(dt, ggplot2::aes(x = ECOPROVI_1, y = abs_err_norm, fill = ECOPROVI_1)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  #ggplot2::geom_jitter() +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(
    x = "Ecoprovince",
    y = "|% change in SWE (ERA5) − % change in SWE (manual)|",
    fill = NULL
  )+
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12),
    legend.text =  ggplot2::element_text(size = 12) )

p_error_ecoprov <- p_error

p_error_ecoprov 

ggplot2::ggsave(filename = "p_error_ecoprov.png", dpi = 900, path = savepath)

means_swe <- swe_long %>%
  dplyr::group_by(ECOPROVI_1, source) %>%
  dplyr::summarise(
    n = sum(!is.na(meanmax_swe_mm)),
    mean_meanmax_swe_mm = mean(meanmax_swe_mm, na.rm = TRUE),
    .groups = "drop"
  )

means_trend <- trend_long %>%
  dplyr::group_by(ECOPROVI_1, source) %>%
  dplyr::summarise(
    n = sum(!is.na(trend)),
    mean_trend = mean(trend, na.rm = TRUE),
    .groups = "drop"
  )

means_error <- dt %>%
  dplyr::group_by(ECOPROVI_1) %>%
  dplyr::summarise(
    n = sum(!is.na(abs_err_norm)),
    mean_error = mean(abs_err_norm, na.rm = TRUE),
    .groups = "drop"
  )

means_swe
means_trend
means_error

#differences between ERA5-Land and SS when grouped by ecoregion

ecoprovince <- unique(dt_ecoz$ECOPROVI_1)

wilcox.test.results <- data.frame(
  test_results_p = numeric(),
  ecoprovince = character()
)

for (i in ecoprovince){
  dt_eco <- dt[dt_ecoz$ECOPROVI_1 == i, ]
  
  if(dplyr::n_distinct(dt_eco$Manual_meanmax_mm) <= 2) next
  
  test <- wilcox.test(dt_eco$Manual_meanmax_mm, dt_eco$ERA5_meanmax_mm, alternative = "two.sided")
  
  wilcox.test.results <- wilcox.test.results %>%
    dplyr::add_row(
      ecoprovince = i,
      test_results_p = test$p.value
    )
  
}
