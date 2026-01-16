#Script for connecting R to Oracle database
#clear environment before running and working with SWE data
library(DBI)
library(magrittr)
library(odbc)
# Setting up connection (note that you must disconnect from one before connecting to another):
#These lines connect to the database in development

user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")), "/Documents/")
snow_package_path <- paste0(user, "R_Scripts/Packages/snow/")

#SnowDB connection
connection <- odbc::dbConnect(drv = odbc::odbc(), dsn = "",
                              uid = "", pwd = "")

#######################################################################
#req functions:
appendvar = function (var1, var2, var3){
  mtch = match(var1, var2)
  var4<-var3[match(var1, var2)]
  return(var4)
}

#######################################################################
#list tables in connection
odbc::dbListTables(connection, schema = "ECC_WMSD_SNOW") 

#assigning combined swe-depths view to a dataframe
snow <- DBI::dbReadTable(connection, DBI::Id(schema = "ECC_WMSD_SNOW", table = "SNOW_MV"), data.frame())

#assigning swe view to a dataframe
swe <- DBI::dbReadTable(connection, DBI::Id(schema = "ECC_WMSD_SNOW", table = "SNOW_SWE_MV"), data.frame())

#assigning depths view to a dataframe
depths <- DBI::dbReadTable(connection, DBI::Id(schema = "ECC_WMSD_SNOW", table = "SNOW_DEPTHS_MV"), data.frame())

#assigning sites view to a dataframe
sites <- DBI::dbReadTable(connection, DBI::Id(schema = "ECC_WMSD_SNOW", table = "SNOW_SITES_V"), data.frame())

#renaming columns in dataframe to match previously established functions
df <- snow %>%
  dplyr::select(COMMON_SITE_ID, COMMON_SITE_NAME, SWE_DATE, COMMON_YEAR, COMMON_MONTH, COMMON_DAY, COMMON_POINT,
                COMMON_EXTRA_DEPTH_NUMBER, COMMON_SURFACE_TYPE, SWE_INSTRUMENT, SWE_KIT, SWE_SURFACE_DIRECTION, SWE_WEIGHT_EMPTY_G, SWE_WEIGHT_FULL_G, SWE_CM, SNOW_DEPTH_CM, DENSITY,
                SWE_SURVEYOR_1, SWE_SURVEYOR_2, SWE_SURVEYOR_3, SWE_DATA_FLAG_1, SWE_DATA_FLAG_2, SWE_NOTES) %>%
  dplyr::filter(COMMON_EXTRA_DEPTH_NUMBER == "-99",
                is.na(SWE_CM) == "FALSE") %>%
  dplyr::rename(site_id = COMMON_SITE_ID,
                site = COMMON_SITE_NAME,
                date_time = SWE_DATE,
                year = COMMON_YEAR,
                month = COMMON_MONTH,
                day = COMMON_DAY,
                point = COMMON_POINT,
                surface_type = COMMON_SURFACE_TYPE,
                surface_direction = SWE_SURFACE_DIRECTION,
                Kit = SWE_KIT,
                instrument = SWE_INSTRUMENT,
                weight_empty_g = SWE_WEIGHT_EMPTY_G,
                weight_full_g = SWE_WEIGHT_FULL_G,
                swe_cm = SWE_CM,
                surveyor_1 = SWE_SURVEYOR_1,
                surveyor_2 = SWE_SURVEYOR_2,
                surveyor_3 = SWE_SURVEYOR_3,
                data_flag = SWE_DATA_FLAG_1,
                dataflag_2 = SWE_DATA_FLAG_2,
                density = DENSITY,
                snow_depth_cm = SNOW_DEPTH_CM)

#renaming columns in depths dataframe to match previously established functions
depths <- depths %>%
  dplyr::select(SITE_ID, SITE_NAME, DEPTH_DATE, DEPTH_YEAR, DEPTH_MONTH, DEPTH_DAY, POINT,
                EXTRA_DEPTH_NUMBER, SURFACE_TYPE, INSTRUMENT_ID, SURFACE_DIRECTION, SNOW_DEPTH_CM, 
                SURVEYOR_1, SURVEYOR_2, SURVEYOR_3, DATA_FLAG_1, DATA_FLAG_2, DEPTH_NOTES) %>%
  # dplyr::filter(EXTRA_DEPTH_NUMBER == "-99",
  #               is.na(SWE_CM) == "FALSE") %>%
  dplyr::rename(site_id = SITE_ID,
                site = SITE_NAME,
                date_time = DEPTH_DATE,
                year = DEPTH_YEAR,
                month = DEPTH_MONTH,
                day = DEPTH_DAY,
                point = POINT,
                surface_type = SURFACE_TYPE,
                surface_direction = SURFACE_DIRECTION,
                instrument = INSTRUMENT_ID,
                snow_depth_cm = SNOW_DEPTH_CM,
                surveyor_1 = SURVEYOR_1,
                surveyor_2 = SURVEYOR_2,
                surveyor_3 = SURVEYOR_3,
                data_flag = DATA_FLAG_1,
                dataflag_2 = DATA_FLAG_2,
                snow_depth_notes = DEPTH_NOTES)

#renaming columns in dataframe to match previously established functions 
sites <- sites %>%
  dplyr::rename(site_name = SITE_NAME,
                site_id = SITE_ID,
                lng = LONGITUDE,
                lat = LATITUDE,
                nwt_region = NWT_REGION,
                watershed = WATERSHED,
                catchment_reference = CATCHMENT,
                ecozone = ECOZONE,
                activity = ACTIVITY)

#adjust coordinates based on higher resolution values

coordinates_highres <- read.csv(paste0(snow_package_path, "data/site_coordinates_highres.csv")) %>%
  dplyr::rename("site_name"="Site_name",
                "lng" = "Longitude",
                "lat" = "Latitude")

sites <- sites %>%
  dplyr::rows_update(coordinates_highres, by = "site_name", unmatched = "ignore")

#creating dataframe with swe-depths and site metadata 
region = appendvar(var1=df$site, var2 = sites$site_name, var3 = sites$nwt_region)
activity = appendvar(var1=df$site, var2 = sites$site_name, var3 = sites$activity)
cf = appendvar(var1=df$site, var2 = sites$site_name, var3 = sites$catchment_reference)
lng = appendvar(var1=df$site, var2 = sites$site_name, var3 = sites$lng)
lt = appendvar(var1=df$site, var2 = sites$site_name, var3 = sites$lat)
md_3<-cbind(df, region, activity, cf, lng, lt)

#replace default -99 values in swe or snow depth values

md_3 <- md_3 %>%
  dplyr::mutate(swe_cm = dplyr::case_when(swe_cm == -99 ~ NA_real_, TRUE  ~ swe_cm),
                snow_depth_cm = dplyr::case_when(snow_depth_cm == -99 ~ NA_real_, TRUE  ~ snow_depth_cm))

#replace NTPC and ST4 instrument as ESC30 and include notes instead
md_3 <- md_3 %>%
  dplyr::mutate(SWE_NOTES = dplyr::case_when(instrument == "NTPC" ~ "NTPC", TRUE ~ SWE_NOTES),
                instrument = dplyr::case_when(instrument == "NTPC" ~ "ESC30", TRUE ~ instrument),
                SWE_NOTES = dplyr::case_when(instrument == "ST4" ~ "ST4", TRUE ~ SWE_NOTES),
                instrument = dplyr::case_when(instrument == "ST4" ~ "ESC30", TRUE ~ instrument))

#To prepare data for external use - in R Shiny app or other scripts
#remove specific columns
md_3 <- md_3 %>%
  dplyr::select(-c(surveyor_1, surveyor_2, surveyor_3, COMMON_EXTRA_DEPTH_NUMBER, SWE_NOTES)) %>%
  dplyr::rename("data_flag_1" = "data_flag",
                "data_flag_2" = "dataflag_2",
                "longitude" = "lng",
                "latitude" = "lt") %>%
  dplyr::select(-c(cf))



