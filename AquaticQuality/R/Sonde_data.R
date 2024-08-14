#Sonde data file conversion
#Emma Riley
#August 13, 2024

#required packages:
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(stringr)
library(magrittr)

#Convert sonde data file function

#' Sonde data convert
#' 
#' Function that renames column headings, manipulates date-time fields and re-formats column order
#' @param filename The name of the file, saved as an xlsx file. Don't include the file extension. Use quotation marks.
#' @param folder sub-folder where this file is saved. Sub-folder must be within "Documents." Use quotation marks. For ex: "/NT_Hydrology/Quality/"
#' @param latitude latitude of the station. Input number only, no quotation marks.
#' @param longitude longitude of the station. Input number only, no quotation marks.
#' @param station station abbreviation. Use quotation marks. For ex: "AKL-PCH"
#' @export

sonde_file_convert = function(filename,
                              folder,
                              station,
                              latitude,
                              longitude){

user <- tolower(Sys.getenv("USERNAME"))
file <- filename
folder <- folder 
path <- paste0("C:/Users/", user, "/Documents", folder)

data <- readxl::read_excel(paste0(path, paste0(file, ".xlsx")), 
                                        col_types = c("text", "date", "numeric", "text", 
                                                      rep("numeric", 22)), skip = 8)

#This line converts the character class 06/20/2023 to a date time field. 
#If date is not in this format (MM/DD/YYYY) this line won't work
data$`Date (MM/DD/YYYY)` <- as.Date.character(data$`Date (MM/DD/YYYY)`, format = ("%m/%d/%y"))

data$`Time (HH:mm:ss)` <- sub("1899-12-31 ", "", data$`Time (HH:mm:ss)` )
data$`Time (HH:mm:ss)` <- sub("1899-12-31", "00:00:00", data$`Time (HH:mm:ss)`)

data["year"] <- lubridate::year(data$`Date (MM/DD/YYYY)`)
data["month"] <- lubridate::month(data$`Date (MM/DD/YYYY)`)
data["day"] <- lubridate::yday(data$`Date (MM/DD/YYYY)`)
data["latitude"] <- latitude
data["longitude"] <- longitude
data["station"] <- station
                                        
data <- dplyr::rename(data, date = `Date (MM/DD/YYYY)`,
                time = `Time (HH:mm:ss)`,
                `site name` = `Site Name`,
                chlorophyll = `Chlorophyll RFU`,
                 d_oxygen = `ODO mg/L`,
                 s_cond = `Cond µS/cm`,
                 turbidity = `Turbidity FNU`,
                 w_temp = `Temp °C`)
data <- dplyr::select(data, `site name`, station, latitude, longitude, date, time, chlorophyll, d_oxygen, pH, s_cond, turbidity,
                w_temp, year, month, day)

write.csv(data, paste0(path, filename, "_clean.csv"), row.names = F)

}

# sonde_file_convert(filename = "Aklavik_PeelCh2023_092923_110538",
#                    folder = "/NT_Hydrology/Quality/",
#                    latitude = 68.1391,
#                    longitude = -135.2135,
#                    station = "AKL-PCH")
