# Author: Emma Riley
# Affiliation: Water Resources, Government of the Northwest Territories
# E: emma_riley@gov.nt.ca
# T: 867-767-9234 x 53120

############################################################################

#Snow functions

#function that writes summary table for spring outlook

#'Spring outlook summary table
#'
#'Takes snow survey values from the SnowDB database and generates a summary table for the current year.
#'Output is an excel file written as "filepath" argument.
#'swe, depth and site variables must be named "SWE", "depth" and "site" in the input dataframe. 
#'@param x Snow survey dataframe - measurement sheet
#'@param y Snow survey dataframe - metadata sheet
#'@param surface Filters for surface type. Specify "upland", "lake" or NULL (all surface types). 
#'@param minyear Minimum calculation year (minus 1) for the normal period. For example, for a 2001-2020 normal period minyear is set to 2000.
#'@param maxyear Maximum calculation year (plus 1) for the normal period. For example, for a 2001-2020 normal period maxyear is set to 2021.
#'@param curmaxyear Current calendar year.
#'@param act Set to either "A" or "IA." Filters for active snow survey sites.
#'@param filepath Specify filepath for output excel file including desired file name. For example "~/SummaryTableSO.xlsx"
#'@export
#'
SummaryTableSO <- function(x, y, surface, minyear, maxyear, curmaxyear, act, filepath)
  
{appendvar = function (var1, var2, var3){
  mtch = match(var1, var2)
  var4<-var3[match(var1, var2)]
  return(var4)}
 
  region = appendvar(var1=x$site, var2 = y$Site_name, var3 = y$region)
  activity = appendvar(var1=x$site, var2 = y$Site_name, 
                     var3 = y$`Active/Inactive`)
  cf = appendvar(var1=x$site, var2 = y$Site_name, 
               var3 = y$`catchment reference`)
  lng<-appendvar(x$site, y$Site_name, y$Long)
  lt<-appendvar(x$site, y$Site_name, y$Lat)
  x_2<-cbind(x, region, activity, cf, lng, lt) 
 
  ST_means = x_2 %>%
  dplyr::mutate(x_2, isY = dplyr::case_when(dataflags=="Y" ~ 'Y')) %>%
  dplyr::filter(surf==surface, 
                is.na(isY),
                activity==act) %>%
  dplyr::group_by(site, year) %>%
  dplyr::summarize(yearlySWE = round(mean(SWE, na.rm=TRUE), 4),
                   yearlydepth = round(mean(depth, na.rm=TRUE), 2),
                   long = mean(lng),
                   lat = mean(lt))

ST = ST_means %>%
  dplyr::group_by(site) %>%
  dplyr::summarize(yrs = dplyr::n_distinct(year, na.rm=TRUE),
                   yrs2 = dplyr::n_distinct(year[year>minyear & year<maxyear], na.rm=TRUE),
                   meanSWE01_20 = round(mean(yearlySWE[year>minyear & year<maxyear], na.rm=TRUE), 4), 
                   meandepth_cur = round(mean(yearlydepth[year==curmaxyear], na.rm=TRUE), 4),
                   meanSWE_cur = round(mean(yearlySWE[year==curmaxyear], na.rm=TRUE), 4),
                   meanSWE01_cur = round(mean(yearlySWE[year>minyear & year<(curmaxyear+1)], na.rm=TRUE), 2),
                   pernorm = ((meanSWE_cur/meanSWE01_20)*100))


xlsx::write.xlsx(ST, file=filepath)}

