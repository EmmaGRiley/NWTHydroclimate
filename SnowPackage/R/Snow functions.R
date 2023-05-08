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
#'@param x Snow survey dataframe
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

#functions for SWE plots

#'Regional SWE plot
#'
#'Creates boxplot for SWE based on location; South Slave, North Slave, Inuvialuit/Gwich'in, Sahtu and Dehcho regions.
#'@param x The dataframe containing snow survey data.
#'@param y The site metadata sheet. 
#'@param currentyear The current year, will be highlighted in red
#'@param act The activity of the snow survey site. Options are "A" for active and "IA" for inactive.
#'@param surface The surface type for the snow survey. Options are "upland" or "lake." 
#'@param plot_width Default set to 20
#'@param plot_height Default set to 12
#'@param plot_name Saves a png called "Regional SWE" in the working directory
#'@export

regional_SWE <- function(x,
                         y,
                         currentyear, 
                         act, 
                         surface,
                         plot_width = 20, 
                         plot_height = 12, 
                         plot_name = "Regional SWE")
  
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
  
  df <- x_2 %>%
  dplyr::mutate(x_2, isY = dplyr::case_when(dataflags=="Y" ~ 'Y')) %>%
  dplyr::select(surf, site, year, month, day, point, depth, SWE, dataflags, cf, region, activity, isY) %>% # Choose columns from master datasheet
  dplyr::filter(activity == act, surf == surface, is.na(isY)) %>%
  dplyr::group_by(region, year) %>% # These are the parameters you are grouping by
  dplyr::summarize(Annual_Mean = mean(SWE, na.rm=TRUE)) # Perform desired operation

plot <- ggplot2::ggplot(data = df, ggplot2::aes(x = region, y = Annual_Mean)) +
  ggplot2::geom_boxplot(outlier.shape = NA) +
  ggplot2::geom_jitter(ggplot2::aes(color = df$year == currentyear)) +
  ggplot2::scale_color_manual(labels=c("all other years", currentyear), values = c("TRUE" = "red", "FALSE" = "black")) +
  ggplot2::ylim(0, max(df$Annual_Mean)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = c(.85, .95)) +
  ggplot2::theme(legend.title = ggplot2::element_blank()) +
  ggplot2::labs(title = paste(""),
                x = "Region",
                y = "Annual (SWE)")
ggplot2::ggsave(paste0(plot_name, ".png"), plot = plot, device = "png", 
                path = "~", 
                scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = 900)

plot
}

