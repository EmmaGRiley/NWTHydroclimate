#Creating summarized CanSWE from multi-point snow surveys

# Define `%>%` operator in current environment
`%>%` <- magrittr::`%>%`

#additional negative function
'%!in%' = Negate('%in%')

user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME"))) 
file_path <- paste0(user, "/Documents/R_Scripts/Packages/snow/data/")

#bring in canswe, downloaded version 7 from: https://zenodo.org/records/14901399
canswe_24 <- read.csv(paste0(file_path, "CanSWE-CanEEN_1928-2024_v7.csv")) %>%
  dplyr::rename("Latitude"="lat",
                "Longitude"="lon")

#filter time and flags
data_flags <- c("b'M'", "b'W'", "b'M'", "b'B'", "b'E'", "b'C'", "b'G'", "b'A'", "b'D'")

canswe_24_ <- canswe_24 %>%
  dplyr::select(station_id, station_name, time, Latitude, Longitude, elevation, source, station_name, type_mes, snw, snd, den, data_flag_snw, data_flag_snd, qc_flag_snd, qc_flag_snw) %>%
  dplyr::mutate(time = lubridate::ymd(time),
                year = lubridate::year(time)) %>%
  dplyr::filter(year >= 1995 & year < 2025,
                type_mes == 0,
                data_flag_snw %!in% data_flags, #filtering out agency data flags
                qc_flag_snw %!in% data_flags) %>% #filtering out canswe data flags
  dplyr::group_by(station_name) %>%
  dplyr::mutate(YearsofRecord = n_distinct(year)) %>%
  dplyr::filter(YearsofRecord >= 21.75) %>%
  dplyr::rename("id" = "station_id") %>%
  dplyr::select(id, station_name, source, time, snw, snd, den, Latitude, Longitude, type_mes, elevation, year) %>%
  dplyr::filter(is.na(snw)==FALSE)

#append missing BC values - 2015 march/april/may values
missingBC <- read.csv(paste0(file_path, "missingBCdata.csv")) %>%
  dplyr::select(station_id, station_name, time, Latitude, Longitude, elevation, source, station_name, type_mes, snw, snd, den, data_flag_snw, data_flag_snd, qc_flag_snd, qc_flag_snw) %>%
  dplyr::mutate(time = lubridate::mdy(time),
                year = lubridate::year(time)) %>%
  dplyr::filter(type_mes == 0,
                data_flag_snw %!in% data_flags, #filtering out agency data flags
                qc_flag_snw %!in% data_flags) %>% #filtering out canswe data flags
  dplyr::rename("id" = "station_id") %>%
  dplyr::select(id, station_name, source, time, snw, snd, den, Latitude, Longitude, type_mes, elevation, year) %>%
  dplyr::filter(is.na(snw)==FALSE)

CANswe <- rbind(canswe_24_, missingBC)

CANswe <- CANswe %>%
  dplyr::group_by(station_name, year) %>%
  dplyr::mutate(swe_cm = (max(na.omit(snw), na.rm=T)/10)) %>% #changing units to cm of swe
  dplyr::select(-time, -snw, -den, -snd) 

CANswe <- unique(CANswe)

#write out results
write.csv(CANswe, paste0(file_path, "CanSWE_v7_V2.csv"))

