#Calculate mean April 1 SWE for the Yukon snow courses used on the April 1 spring outlook map
#Output goes into the Mean_SWE_mm column of YT_Data_<year>.csv - see README_snow_scripts.md (Yukon data)
#Based on snow_local/R/Calc_yukon_historical_swe_means.R

select_year <- 2027 #year you are mapping. The mean uses all April surveys BEFORE this year

user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")))
path <- paste0(user, "/Documents/R_Scripts/Packages/snow/data/YT_Data/")

YT_snow <- read.csv(paste0(path, "Snow_Samples_YT.csv"))

#snow courses in the Mackenzie River Basin used on the map
select_snow_courses <- c("10AA-SC01", #Watson Lake Airport
                         "10AA-SC02", #Tintina Airstrip
                         "10AA-SC03", #Pine Lake Airstrip
                         "10AA-SC04", #Ford Lake
                         "10AB-SC01", #Frances River
                         "10AD-SC01", #Hyland River (composite) - see README, Hyland needs checking
                         "10AD-SC01B", #Hyland River B
                         "10MA-SC01", #Blackstone River
                         "10MA-SC02", #Ogilvie River
                         "10MB-SC01") #Bonnet Plume Lake

if (max(lubridate::year(as.Date(lubridate::parse_date_time(YT_snow$Target_Date, c("mdy", "ymd")))), na.rm = TRUE) < select_year - 1) {
  warning("Snow_Samples_YT.csv has no surveys from ", select_year - 1, ". Download an updated copy first (see README).")
}

snow <- YT_snow %>%
  dplyr::select(Snow_Course_ID, Snow_Depth_cm, SWE_mm, Target_Date, Survey_Date)

snow <- snow %>%
  dplyr::mutate(target_month = lubridate::month(as.Date(lubridate::parse_date_time(Target_Date, c("mdy", "ymd"))))) %>%
  dplyr::mutate(target_year = lubridate::year(as.Date(lubridate::parse_date_time(Target_Date, c("mdy", "ymd"))))) %>%
  dplyr::filter(target_month == 4,
                target_year < select_year,
                Snow_Course_ID %in% select_snow_courses) %>%
  dplyr::group_by(Snow_Course_ID) %>%
  dplyr::summarise(meanSWE = round(mean(SWE_mm, na.rm=T), 1),
                   n_years = dplyr::n_distinct(target_year))

print(snow, n = Inf)
