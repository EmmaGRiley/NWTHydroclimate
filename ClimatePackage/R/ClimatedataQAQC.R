#############################################################
# Author: Emma Riley
# Affiliation: Water Resources, Government of the Northwest Territories
# E: emma_riley@gov.nt.ca
# T: 867-767-9234 x 53120

#Script with functions for climate data QAQC

library(dplyr)
library(writexl)
library(readxl)
library(xlsx)

#############################################################
#bring in hourly data sheets below
colomac = read_excel("~/NT_Hydrology/Abiotic Monitoring/Historic/Colomac_Station_ID_97CM01.xlsx", sheet = "Hourly")

#############################################################
#Required functions

#' Not a number search, for dataframes
#' 
#' Function to find nan values in a dataframe
#' @param x The dataframe itself
#' @examples 
#' a <- c(NaN, 1)
#' b<- c(2, 3)
#' df<-cbind.data.frame(a, b)
#' is.nan.data.frame(df) #returns:
#'          a     b
#'[1,]  TRUE  FALSE
#'[2,] FALSE  FALSE
#' @export
is.nan.data.frame <- function (x)
{do.call(cbind, lapply(x, is.nan))}

#'Sum not a number values
#'
#'Sums values, returns NA if all values are NA. Builds on the sum() function, which returns 0 if all values are NA.
#'
#'@param x A list of values
#'@examples
#'a<-c(NA, NA, NA, NA)
#'b<-c(1, NA, NA, 2)
#'sumna(a) #returns:
#'[1] NA
#'sumna(b) #returns:
#'[1] 3
#'@export
sumna <- function(x) {
  sumna <- NULL
  return(ifelse(all(is.na(x)), NA, sum(na.omit(x))))
}

#'Hourly to Daily
#'
#'Function to convert hourly to daily values for raw met station data
#'
#'Writes an excel file to the working directory with newly calculated daily values for 
#'Air Temperature, Relative Humidity, Rainfall, Wind speed and
#'Net Radiation.
#'
#'Hourly variables in dataframe must be written exactly as above.
#'
#'Calculates daily means only if there are at least 20 hourly values recorded in a day.
#'
#'@param x The hourly dataframe
#'@examples
#'`Air Temperature`<-c(21, 22, 23, 23, 23, 21, 21, 22, 23, 23, 23, 21, 21, 22, 23, 23, 23, NA, NA, NA, NA, 21, 18, 29)
#'`Relative Humidity`<-c(78, 79, 80, 81, 82, 83, 88, 90, 92, 95, 98, NA, NA, NA, NA, NA, 98, 95, 82, 80, 80, 79, 70, 71)
#'`Rainfall`<-c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.4, 9.2, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.4, 9.2, 0, 0)
#'`Wind Speed`<-c(1.2, 1.4, 0.8, 0.7, 0.2, 0.5, 1.2, 1.4, 0.8, 0.7, 0.2, 0.5, 1.2, 1.4, 0.8, 0.7, 0.2, 0.5, 1.2, 1.4, 0.8, 0.7, 0.2, 0.5)
#'`Net Radiation`<-c(54, 50, 50, 60, 80, 100, 150, 120, 180, 200, 180, 160, 140, 120, 100, 80, 60, 50, 50, 30, 20, 20, 10, 10)
#'Year<-c(1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999)
#'JD<-c(287, 287, 287, 287, 287, 287,  287, 287, 287, 287, 287, 287, 287, 287, 287, 287, 287, 287, 287, 287, 287, 287, 287, 287)
#'x<-cbind.data.frame(Year, JD, `Air Temperature`, `Relative Humidity`, `Rainfall`, `Wind Speed`, `Net Radiation`)
#'hourlytodaily(x) #returns
#'#writes an excel file "hourlymeansx.xlsx" to the working directory
#'#should contain Year	JD	dailyairT	dailyRH	dailyRain	dailyWS	dailyQ
#'#                1999	287	     22.3	   #N/A	     19.8	   0.8	86.42
#'@export
temporalmeans = function(x)
{dailyvars = x %>%                                   
  dplyr::group_by(Year, JD) %>%
  dplyr::summarize(dailyairT = round(mean(`Air Temperature`[sum(!is.na(`Air Temperature`))>19], na.rm=TRUE), 4),
                   dailyRH = round(mean(`Relative Humidity`[sum(!is.na(`Relative Humidity`))>19], na.rm=TRUE), 4),
                   dailyRain = sumna(`Rainfall`[sum(!is.na(`Rainfall`))>19]),
                   dailyWS = round(mean(`Wind Speed`[sum(!is.na(`Wind Speed`))>19], na.rm = TRUE), 2),
                   dailyQ = round(mean(`Net Radiation`[sum(!is.na(`Net Radiation`))>19], na.rm = TRUE), 2)) #specifies that there must be at least 
#20 non-Na values to create a daily mean
dailyvars[is.nan.data.frame(dailyvars)] <- NA #replaces NaN values with Na

namedata = c("~/dailyvars", deparse(substitute(x)),".xlsx") #will show up as dailyvars"nameofhourlysheet".xlsx in this directory
filename = paste(namedata[1], namedata[2], namedata[3], sep="")

xlsx::write.xlsx(as.data.frame(dailyvars), file=filename, row.names = FALSE)

dailyvars<<- dailyvars

monthlyvars = dailyvars %>%
  dplyr::group_by(Year, Month) %>%
  dplyr::summarize(monthlyairT = round(mean(dailyairT[sum(!is.na(dailyairT))>24], na.rm=TRUE), 4),
                   monthlyRH = round(mean(dailyRH[sum(!is.na(dailyRH))>24], na.rm=TRUE), 4),
                   monthlyRain = sumna(dailyRain[sum(!is.na(dailyRain))>24]),
                   monthlyWS = round(mean(dailyWS[sum(!is.na(dailyWS))>24], na.rm = TRUE), 2),
                   monthlyQ = round(mean(dailyQ[sum(!is.na(dailyQ))>24], na.rm = TRUE), 2)) #specifies that there must be at least 
#25 non-Na values to create a monthly mean}
monthlyvars[is.nan.data.frame(monthlyvars)] <- NA #replaces NaN values with Na

namedata = c("~/monthlyvars", deparse(substitute(x)),".xlsx") #will show up as monthlyvars"nameofhourlysheet".xlsx in this directory
filename = paste(namedata[1], namedata[2], namedata[3], sep="")

xlsx::write.xlsx(as.data.frame(monthlyvars), file=filename, row.names = FALSE)}
}

#############################################################

