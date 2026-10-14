#Calculate mean April 1 SWE for the Yukon snow courses used on the April 1 spring outlook map
#Output goes into the Mean_SWE_mm column of YT_Data_<year>.csv - see README_snow_scripts.md (Yukon data)
#Based on snow_local/R/Calc_yukon_historical_swe_means.R

select_year <- 2027 #year you are mapping. The mean uses all April surveys BEFORE this year

user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")))
path <- paste0(user, "/Documents/R_Scripts/Packages/snow/data/YT_Data/")

YT_snow <- read.csv(paste0(path, "Snow_Samples_YT.csv"), na.strings = c("NA", "", "-")) #"-" = no measurement

#snow courses in the Mackenzie River Basin used on the map
#(Hyland River is calculated separately below, from the combined Hyland A + B record)
select_snow_courses <- c("10AA-SC01", #Watson Lake Airport
                         "10AA-SC02", #Tintina Airstrip
                         "10AA-SC03", #Pine Lake Airstrip
                         "10AA-SC04", #Ford Lake
                         "10AB-SC01", #Frances River
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
                   n_years = dplyr::n_distinct(target_year),
                   last_year = max(target_year))

#Hyland River: combined record (see README_snow_scripts.md, Yukon data)
#Hyland A (10AD-SC01) was replaced by Hyland B (10AD-SC01B) in 2018. B reads about 9% higher than A,
#so years with only an A survey are scaled by the published B/A correction factor; years with a B survey use B.
#Source: YT_Data/2025_Hyland_River_B_Corrections.xlsx, sheet Hyland_HistAdj (cell E25, column P)
hyland_factor <- 1.0916 #April B/A ratio from the surveys where both sites were measured (2018-2022)

hyland <- YT_snow %>%
  dplyr::mutate(target_date = as.Date(lubridate::parse_date_time(Target_Date, c("mdy", "ymd")))) %>%
  dplyr::filter(lubridate::month(target_date) == 4,
                lubridate::year(target_date) < select_year,
                Snow_Course_ID %in% c("10AD-SC01", "10AD-SC01B"),
                !is.na(SWE_mm)) %>%
  dplyr::group_by(target_year = lubridate::year(target_date)) %>%
  dplyr::summarise(SWE_combined = if (any(Snow_Course_ID == "10AD-SC01B")) {
    mean(SWE_mm[Snow_Course_ID == "10AD-SC01B"])
  } else {
    mean(SWE_mm[Snow_Course_ID == "10AD-SC01"]) * hyland_factor
  })

snow <- snow %>%
  dplyr::bind_rows(data.frame(Snow_Course_ID = "10AD-SC01B", #Hyland River B, combined record
                              meanSWE = round(mean(hyland$SWE_combined), 1),
                              n_years = nrow(hyland),
                              last_year = max(hyland$target_year)))

print(snow, n = Inf)

#Warn if a site has no April survey from last year (the mean would quietly leave that year out)
missing_last <- snow$Snow_Course_ID[snow$last_year < select_year - 1]
if (length(missing_last) > 0) {
  warning("No April ", select_year - 1, " survey in Snow_Samples_YT.csv for: ", paste(missing_last, collapse = ", "),
          ". Check for an updated copy of the file, or add the survey before using these means (see README).", call. = FALSE)
}
