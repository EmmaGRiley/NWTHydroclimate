#Summary Snow Survey values

#bring in ss data (as GNWT employee)
source(paste0(user, "Documents/R_Scripts/Packages/snow/R/R_oracle_connect.R"))

# for non-GNWT users, download data (point_data.csv and sites.csv) from https://doi.org/10.46887/2025-005
# or use data from the /data folder of this package
md_3 <- readRDS(paste0(user,"/Documents/R_Scripts/Packages/snow/data/md_3.rds"))
sites <- readRDS(paste0(user,"/Documents/R_Scripts/Packages/snow/data/sites.rds"))

#other req functions:
#opposite of %in%:
`%!in%` = Negate(`%in%`)

#Summary Table function

SWEsummary <- function(data,
                       surface,
                       minyear, #2000 here
                       flags, #("Y")
                       maxyear, #2021 here
                       curmaxyear, #current year
                       act,
                       write,
                       hdensity,
                       ldensity) #"A"
     
   {densityrange = data %>%
     dplyr::filter(surface_type==surface, 
                   data_flag_1%!in% flags,
                   data_flag_2%!in% flags) %>%
     dplyr::select(density)
   
   meanD <- mean(densityrange$density, na.rm=T)
   SDD<-sd(densityrange$density, na.rm=T)
   
   lower_bound <- meanD - (2*SDD) # 0.096 -> round to 0.1
   upper_bound <- meanD + (3*SDD) # 0.334 (in 2025)
  
    df = data %>%
       dplyr::filter(surface_type==surface, 
                     data_flag_1%!in% flags,
                     data_flag_2%!in% flags,
                     is.na(density)|density < upper_bound,
                     is.na(density)|density > ldensity,
                     activity==act) %>%
       dplyr::group_by(region, site, year) %>%
       dplyr::summarize(yearlySWE = mean(swe_cm, na.rm=TRUE),
                     yearlydepth = mean(snow_depth_cm, na.rm=TRUE),
                     long = mean(longitude),
                     lat = mean(latitude)) %>%
     dplyr::mutate(rank = dplyr::dense_rank(dplyr::desc(yearlySWE)))
   
     Table = df %>%
         dplyr::group_by(region, site) %>%
         dplyr::summarize(yrs = dplyr::n_distinct(year, na.rm=TRUE),
                          yrs2 = dplyr::n_distinct(year[year>minyear & year<maxyear], na.rm=TRUE),
                          meanSWE01_20 = round(mean(yearlySWE[year>minyear & year<maxyear], na.rm=TRUE)*10, 4), 
                          meandepth_cur = round(mean(yearlydepth[year==curmaxyear], na.rm=TRUE), 4),
                          meanSWE_cur = round(mean(yearlySWE[year==curmaxyear], na.rm=TRUE)*10, 6),
                          meanSWE01_cur = round(mean(yearlySWE[year>minyear], na.rm=TRUE)*10, 2),
                          meanSWE_allyears = round(mean(yearlySWE[year<curmaxyear], na.rm=TRUE)*10, 2),
                          pernorm = ((meanSWE_cur/meanSWE01_20)*100),
                          pernorm_allyrs = ((meanSWE_cur/meanSWE_allyears)*100),
                          rank2024 = (rank[year==curmaxyear]),
                          Long = mean(long),
                          Lat = mean(lat))
     
     if(write == T){
       write.csv(Table, file=paste0(paste0(user, "/Documents/SummaryTable", curmaxyear, ".csv")))}
     
     return(Table)
     }


SWEsummary(data = md_3,
           surface="upland", 
           minyear=2000, 
           maxyear=2021,
           curmaxyear=2025,
           flags = c("Y", "HS", "Sk", "P", "Sk_2"),
           ldensity = 0.1,
           act="A",
           write = T)

#boxplot function - yearly mean SWE separated by region, with current year highlighted in blue
regional_boxplot = function (surface,
                             flags, #("Y", "HS")
                             curyear, #current year
                             act,
                             save_path,
                             plot_name,
                             select_year_point_size = 2,
                             historic_point_size = 1,
                             legend_position = c(0.1, 0.95),
                             save = FALSE,
                             plot_width = 16,
                             plot_height = 10,
                             y_min = NA,
                             y_max = NA, 
                             numbersites)
{ df = md_3 %>%
  dplyr::filter(surface_type==surface, 
                data_flag_1%!in% flags,
                data_flag_2%!in% flags,
                activity==act) %>%
  dplyr::group_by(region, year) %>%
  dplyr::summarize(yearlySWE = mean(swe_cm*10, na.rm=TRUE),
                   yearlydepth = mean(snow_depth_cm, na.rm=TRUE),
                   no_sites = dplyr::n_distinct(site)) %>%
  dplyr::filter(no_sites>=numbersites)

df$region[df$region == "Gwich'in"] <- "Inuvialuit-Gwich'in"


plot <-  ggplot2::ggplot(df, ggplot2::aes(x = region, y = yearlySWE, group = region)) + #editing: insert Year=Year in aes()
  ggplot2::geom_boxplot(notch = F, outlier.shape = NA, outlier.colour = NA) +
  ggplot2::geom_jitter(data = df[df$year!=curyear, ], colour = "grey", alpha = 0.75, size = historic_point_size) +
  ggplot2::geom_point(data = df[df$year==curyear, ], ggplot2::aes(colour="blue"), alpha = 1, size = select_year_point_size) +  #ER - added "x = MonthName, y = Value," inside aes(), removed colour = "red" and put outside aes() as colour = point_colour
  ggplot2::theme_classic() +
  ggplot2::scale_colour_manual("", labels=paste(curyear), values = "blue") +
  ggplot2::theme(legend.position = legend_position) +
  ggplot2::labs(title = paste0(""),
                x = "Region",
                y = "Mean annual SWE (mm)")

if(!is.na(y_min) == T) {
  plot <- plot +
    ggplot2::ylim(y_min, y_max)
}

if(save == TRUE) {
  ggplot2::ggsave(paste0(plot_name, ".", "png"), plot = plot, device = "png",
                  path = ifelse(exists("save_path"), save_path, getwd()),
                  scale = 1, width = plot_width, height = plot_height, units = c("cm"))
}

plot

}

regional_boxplot(surface = "upland",
                 flags = c("Y", "HS"), #("Y", "HS")
                 curyear = 2024, #current year
                 act = "A",
                 save_path = paste0(user, "/Documents/NT_Hydrology/Figures"),
                 plot_name = "Regional_boxplot",
                 select_year_point_size = 2,
                 historic_point_size = 1.3,
                 legend_position = c(0.5, 0.95),
                 save = TRUE,
                 plot_width = 18,
                 plot_height = 11,
                 y_min = NA,
                 y_max = NA,
                 numbersites = 3)






