#ERA5 and manual snow survey trend comparison and correlation

#req packages
library(dplyr)
library(trend)
library(spearmanCI)

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

# Snow survey data
# Load NWT manual survey data (returns md_3, sites, depths, snow, swe)
# Can only use this as a GNWT employee
# source(paste0(user, "/Documents/R_Scripts/Packages/snow/R/R_Oracle_Connect.R"))

# for non-GNWT users, download data (point_data.csv and sites.csv) from https://doi.org/10.46887/2025-005
# or use data from the /data folder of this package
md_3 <- readRDS(paste0(user,"/Documents/R_Scripts/Packages/snow/data/md_3.rds"))
sites <- readRDS(paste0(user,"/Documents/R_Scripts/Packages/snow/data/sites.rds"))

#data and plotting variables
data <- md_3
exclude_sites <- NA 
variable <- "SWE"
start_year <- 1995
end_year <- 2024
p.value <- 0.05
min_year = (end_year - start_year)*0.75 
act = c("IA", "A")
surface = c("upland")
flags = c("Y", "Sk", "P", "Sk_2")
hdensity = 0.34
ldensity = 0.1
significantonly = F

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
                   meandepth = mean(snow_depth_cm, na.rm=T))
  
  #adjust md_3 values -> using csv from SWE_adjust_extra_depths
  
  adjust_swe <- read.csv("C:/Users/Emma_gregory/Documents/NT_Hydrology/snowfiles/adjusted_swe_values.csv") %>%
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
  
  if(variable == "SWE"){
    if (length(TA$meanswe) == 0) {} else {
      test.mk <- Kendall::MannKendall(TA$meanswe)
      test.ss <- trend::sens.slope(na.omit(TA$meanswe), conf.level = 0.95)
      p.value.function(p.value, test.mk)
      
      TA["p-value"] =  round(as.numeric(test.mk[2]), 5)
      TA["Magnitude"] = round(as.numeric(test.ss[1]), 5)
      TA["Significance"] =  significance = if (test.mk[2]< p.value) {"yes"} else {"no"}
      TA["Years of Record"] = dplyr::n_distinct(TA$year)
      
      table <- rbind (table, TA)
    }} 
  
} 

Table<-table %>%
  dplyr::group_by(Site_name)%>%
  dplyr::reframe(Magnitude = sample(Magnitude*100),
                 `p-value` = sample(`p-value`),
                 Significance= sample(Significance),
                 `Years of Record` = sample(`Years of Record`)) %>%
  dplyr::filter(`Years of Record` > min_year)
SiteTable = unique(Table)

longitude = appendvar(var1=SiteTable$Site_name, var2 = sites$site_name, var3 = sites$lng)
latitude = appendvar(var1=SiteTable$Site_name, var2 = sites$site_name, var3 = sites$lat)

MapTable<-cbind(SiteTable, longitude, latitude)
MT = MapTable

#Compare column sen_slope from ERA5 trends and Magnitude from "MT" 
#based on their closest lat/long matches

MT <- dplyr::rename(MT, 
              latMT = latitude,
              longMT = longitude)
#####################################################################################################
#option to bring in MT from ERA5_snowtrends_map_hourly.R
MT <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/MRB_snowsurvey_trends.csv")) 
MT <- MT %>%
  dplyr::select(-X)

era5data <- dplyr::rename(era5data, 
                             lat = Latitude,
                             long = Longitude)

era5data <- era5data %>%
  dplyr::select(-X)

# Define a function to calculate the Haversine distance (for closest lat/long match)
haversine_dist <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 # Earth radius in kilometers
  delta_lat <- (lat2 - lat1) * pi / 180
  delta_lon <- (lon2 - lon1) * pi / 180
  a <- sin(delta_lat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(delta_lon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c # Distance in km
}

# Match each point in data1 to the closest point in data2
matched_data <- era5data %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    closest_point = list(
      MT %>%
        dplyr::mutate(distance = haversine_dist(lat, long, dplyr::cur_data()$Latitude, dplyr::cur_data()$Longitude)) %>%
        dplyr::filter(distance == min(distance)) %>%
        dplyr::slice(1)
    )
  ) %>%
  tidyr::unnest(closest_point)

#optional write to csv
#write.csv(matched_data, paste0(user, "/Documents/R_Scripts/Packages/snow/data/era5_hourly_manual_corr_adjswe_canswe.csv")) 
#matched_data <- read.csv(paste0(user, "/Documents/R_Scripts/Packages/snow/data/era5_hourly_manual_corr_adjswe_canswe.csv"))

matched_data <- matched_data %>%
  dplyr::filter(Site_name %!in% exclude_sites)

#plot relationship
data_table <- data.frame()

for (i in unique(matched_data$Site_name)){
  data <- matched_data %>%
    dplyr::filter(Site_name == i) %>%
    dplyr::mutate(rank = rank(distance, na.last = NA)) %>%  # Add rank column
    dplyr::filter(rank == min(rank, na.rm=T)) 
  
  if(length(data$long)>1){
  best_trend <- data %>%
    dplyr::filter(P_value == min(P_value, na.rm = TRUE))
  
  if(length(best_trend$long)>1){
    best_trend <- best_trend %>%
      slice(1)
  }
  }else{
    best_trend <- data
  }
  
  data_table <- rbind(data_table, best_trend)
}

#write.csv(data_table, paste0(user, "/Documents/R_Scripts/Packages/snow/data/ERA5_manual_comparison.csv"))

Data_table <- unique(data_table) %>%
  dplyr::mutate(Sen_slope = Sen_slope*10000) %>% #era4 magnitude unit adjustment
  dplyr::mutate(NormSen_slope = ((Sen_slope/(meanmaxswe * 1000)) * 100),
                NormMagnitude = ((Magnitude/(sitemeanswe * 10)) * 100))

#plot data - trends normalized by site mean swe
plot(Data_table$NormSen_slope, Data_table$NormMagnitude)

corr <- cor.test(Data_table$NormSen_slope, Data_table$NormMagnitude, method = "spearman")  

corr$p.value <- ifelse(corr$p.value < 0.01, "< 0.01", "> 0.01")

cor_label <- paste0(
  "Spearman's ρ = ", round(corr$estimate, 2), 
  ", p ", corr$p.value)

#determine x and y axes scale
shared_limits <- range(c(Data_table$NormSen_slope, Data_table$NormMagnitude), na.rm = TRUE)

#fix for normalized trend plot
x_scale <- c(-1200, 1760)
y_scale <- c(-30, 30)

plot <- ggplot2::ggplot(data = Data_table, ggplot2::aes(x=NormSen_slope, y=NormMagnitude)) +
  ggplot2::geom_smooth(method = "lm", se = T, linetype = "solid", color = "black", size = 1, fill = "lightgrey", alpha = 0.6) +
  ggplot2::geom_point(size = 7, shape = 21, color="black", fill = NA, stroke = 1) +
  ggplot2::theme_classic() +
  ggplot2::annotate("text", x = 6, y = max(Data_table$NormMagnitude), 
                    label = cor_label, hjust = 1.3, vjust = 1, size = 7, color = "black") +
  ggplot2::labs(title = "", x = "% change in ERA5-Land SWE/decade", y = "% change in manual snow survey SWE/decade") +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "black", size = 1) +  # 1:1 line
  ggplot2::scale_x_continuous(limits = shared_limits) + 
  ggplot2::scale_y_continuous(limits = shared_limits) + 
  ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, size = 1, color = "black"),
                 axis.text = ggplot2::element_text(size = 14),      # Adjust numeric axis labels
                 axis.title = ggplot2::element_text(size = 18))

plot

