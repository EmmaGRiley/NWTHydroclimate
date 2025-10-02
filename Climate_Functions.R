# Call all functions from other worksheets into active environment

library(dplyr)

# Define file paths

user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")), "/Documents/")
wf_path <- paste0(user, "R_Scripts/Functions/Workflows/")

# Call in source functions

source(paste0(wf_path, "nwtclimate_functions.R"))

###################################################################################################

# Run function to update climate data

source(paste0(wf_path, "update_climate.R"))

# Added by MA : ----------------------------------------------------
clim_plot_timeseries(
  site = c("Peace River"),
  parameter = "rain",
  start_year = "2003",
  end_year = "2010",
  select_year = NA,
  water_year_start = 1,
  zoom_x = c(as.Date("2008-10-01"), as.Date("2008-12-01")), #c(as.Date("yyyy-mm-dd"), as.Date("yyyy-mm-dd")) or NULL
  zoom_y = NULL # c(min y value, max y value) or NULL
)


###################################################################################################

hydro_plot_dayofyear(
  station_number = "10PA002",
  parameter = "flow",
  select_years = c(2022:2025), #2001, 2006, 2009, 2020, 2021 #1998:2000, 2002:2005, 2007:2008,
  after_bennett = T,
  historic_min = NA,
  historic_max = 2024,
  water_year_start = 1,
  historic = TRUE,
  log_scale = FALSE,
  start_month = 01,
  start_day = 01,
  end_month = 12,
  end_day = 31,
  line_colours = c("dodgerblue",
                   "blue4",
                   "green4",
                   "red4",
                   "purple4",
                   "yellow4"),
  legend_position = "top",
  line_size = 0.5,
  point_size = 0,
  legend_text_size = 8,
  y_min = NA,
  y_max = NA,
  save = T,
  plot_width = 20,
  plot_height = 11,
  dpi = 900,
  file_name = "Buffalo_24_2025",
  extension = "jpg")
  #horizontal_line = F,
  #hline_height = NA,
  #interactive = T,
  #y_axis_title = "Discharge (m3 s-1)",
  #title.size = 14)

###################################################################################################

hydro_plot_dayofyear(
  station_number = "10EB001",
  parameter = "level",
  select_years = c(2012, 2024),
  after_bennett = F,
  historic_min = NA,
  historic_max = 2011,
  water_year_start = 1,
  historic = TRUE,
  log_scale = FALSE,
  start_month = 01,
  start_day = 01,
  end_month = 12,
  end_day = 31,
  line_colours = c("dodgerblue",
                   "blue4",
                   "green4",
                   "red4",
                   "purple4",
                   "yellow4"),
  legend_position = "top",
  line_size = 0.5,
  point_size = 0,
  legend_text_size = 8,
  y_min = NA,
  y_max = NA,
  save = T,
  plot_width = 20,
  plot_height = 11,
  dpi = 900,
  file_name = "LiardFL_2025",
  extension = "png")


###################################################################################################

interactive <- hydro_plot_dayofyear(
  station_number = "10GC001",
  parameter = "level",
  select_years = c(2024, 2025),
  after_bennett = F,
  historic_min = NA,
  historic_max = 2023,
  water_year_start = 1,
  historic = TRUE,
  log_scale = FALSE,
  start_month = 01,
  start_day = 01,
  end_month = 12,
  end_day = 31,
  line_colours = c("dodgerblue",
                   #"blue4",
                   "green4",
                   "red4",
                   "purple4",
                   "yellow4"),
  legend_position = "top",
  line_size = 0.5,
  point_size = 0,
  legend_text_size = 8,
  y_min = NA,
  y_max = NA,
  save = TRUE,
  plot_width = 18,
  plot_height = 11,
  dpi = 900,
  file_name = "mack_at_simpson_2025",
  extension = "png")

plot<-plotly::ggplotly(interactive, tooltip = c("y", "x", "colour", "ymin", "ymax", "yintercept"))

plot <- plotly::plotly_build(plot)

plot

###################################################################################################
interactive <- hydro_plot_dayofyear_working(
  station_number = "07NB001",
  parameter = "flow",
  select_years = c(2021, 2022, 2023, 2024),
  after_bennett = T,
  historic_min = NA,
  historic_max = 2023,
  water_year_start = 1,
  historic = TRUE,
  log_scale = FALSE,
  start_month = 01,
  start_day = 01,
  end_month = 12,
  end_day = 31,
  line_colours = c("dodgerblue",
                   #"blue4",
                   "green4",
                   "red4",
                   "purple4",
                   "yellow4"),
  legend_position = "top",
  line_size = 0.5,
  point_size = 0,
  legend_text_size = 8,
  y_min = NA,
  y_max = NA,
  save = TRUE,
  plot_width = 18,
  plot_height = 11,
  dpi = 900,
  file_name = "Athabasca at Embarras flows",
  extension = "png",
  horizontal_line = F,
  hline_height = NA,
  title.size = 14,
  interactive = T,
  y_axis_title = "Discharge (m3 s-1)" #take this out for non-plotly plots = "Discharge (m3 s-1)",
)

plot<-plotly::ggplotly(interactive, tooltip = c("y", "x", "colour", "ymin", "ymax", "yintercept"))

plot <- plotly::plotly_build(plot)

plot

###################################################################################################

interactive <- hydro_plot_dayofyear_cumulative(
  station_number = "10PA002",
  parameter = "flow",
  select_year = c(2024), #this can only be one year for the cumulative function
  after_bennett = TRUE,
  historic_min = NA,
  historic_max = 2019,
  water_year_start = 1,
  historic = TRUE,
  log_scale = FALSE,
  start_month = 01,
  start_day = 01,
  end_month = 12,
  end_day = 30,
  line_colours = c("dodgerblue",
                   #"blue4",
                   "green4",
                   "red4",
                   "purple4",
                   "yellow4"),
  legend_position = "top",
  line_size = 0.5,
  point_size = 0,
  legend_text_size = 8,
  y_min = NA,
  y_max = NA,
  save = TRUE,
  plot_width = 18,
  plot_height = 11,
  dpi = 900,
  file_name = "cumulative slave flows 2025",
  extension = "png",
  #horizontal_line = F,
  #hline_height = NA,
  #title.size = 14,
  interactive = T,
  y_axis_title = "Cumulative Discharge (m3 s-1)" #take this out for non-plotly plots = "Discharge (m3 s-1)",
)

plot<-plotly::ggplotly(interactive, tooltip = c("y", "x", "colour", "ymin", "ymax", "yintercept"))

plot <- plotly::plotly_build(plot)

plot

###################################################################################################

interactive <- hydro_plot_dayofyear_working(
  station_number = "07SB001",
  parameter = "level",
  select_years = c(2024, 2025),
  after_bennett = T,
  historic_min = NA,
  historic_max = 2023,
  water_year_start = 1,
  historic = TRUE,
  log_scale = FALSE,
  start_month = 01,
  start_day = 01,
  end_month = 12,
  end_day = 31,
  line_colours = c("dodgerblue",
                   #"blue4",
                   "green4",
                   "red4",
                   "purple4",
                   "yellow4"),
  legend_position = "top",
  line_size = 0.5,
  point_size = 0,
  legend_text_size = 8,
  y_min = NA,
  y_max = NA,
  save = TRUE,
  plot_width = 18,
  plot_height = 11,
  dpi = 900,
  file_name = "Default hydrometric plot",
  extension = "png",
  horizontal_line = F,
  hline_height = NA,
  interactive = T,
  y_axis_title = "Water Level (m)" ,
  title.size = 14)

plot<-plotly::ggplotly(interactive, tooltip = c("y", "x", "colour", "ymin", "ymax", "yintercept"))

plot <- plotly::plotly_build(plot)

plot

###################################################################################################

test<-clim_plot_monthly_working(
  site = "Fort Smith",
  parameter = "temp",
  select_year = 2025,
  water_year_start = 4,
  water_year_end = 9,
  start_year = 1950,
  end_year = 2025,
  max_missing_days = 3,
  y_min = NA,
  y_max = NA,
  select_year_point_size = 2,
  historic_point_size = 1,
  legend_position = c(0.5, 0.95),
  save = TRUE,
  plot_width = 18,
  plot_height = 11,
  dpi = 900,
  file_name = "FS Temp",
  extension = "png",
  water_year = FALSE
)


plotly::ggplotly(test) %>%
  plotly::ggplotly(tooltip = c("y", "x", "Year"))

p <- plotly::plotly_build(test)

p$x$data[1] <- lapply(p$x$data[1], FUN = function(x){
  x$marker = list(opacity = 0)
  return(x)
})

p
###################################################################################################


test<-clim_plot_to_date_working(
  #site = c("Hay River", "Fort St John", "Fort Nelson", "Peace River", "High Level", "Fort Simpson"),
  #site = c("Fort Simpson", "Norman Wells", "Fort Good Hope", "Fort McPherson", "Inuvik"),
  #site = c("Fort Smith",  "Hay River", "Fort Simpson", "Yellowknife", "Norman Wells", "Inuvik"),
  #site = c("Fort Nelson", "High Level", "Hay River"),
  #site = c("Mackenzie", "Fort St John", "Peace River", "High Level",  "Fort McMurray", "Fort Chipewyan", "Fort Smith"),  
  site = c("Fort Liard", "Fort Nelson", "Fort Simpson", "Watson Lake"),
  #site = c("Fort Good Hope", "Norman Wells", "Inuvik"),
  parameter = "precip",
  select_year = 2025, 
  water_year_start = 4,
  end_date = "2025-06-04", 
  max_missing_days = 10, 
  y_min = 0,
  y_max = 250,
  legend_position = c(0.5, 0.99),
  save = T,
  file_name = paste0("precip spring 2025 liard", report_date), 
  extension = "png",
  water_year = F,
  plot_width = 20, #usually 16
  plot_height = 10, #usually 10
)

plotly::ggplotly(test) %>%
  plotly::ggplotly(tooltip = c("y", "x", "Year"))

p <- plotly::plotly_build(test)

p$x$data[1] <- lapply(p$x$data[1], FUN = function(x){
  x$marker = list(opacity = 0)
  return(x)
})

p
###################################################################################################


test<-clim_plot_to_date_working(
  #site = c("Hay River", "Fort St John", "Fort Nelson", "Peace River", "High Level", "Fort Simpson"),
  site = c("Fort Smith", "Hay River",  "Fort Simpson","Yellowknife","Norman Wells", "Inuvik"),
  #site = c("Fort Nelson", "High Level", "Hay River"),
  #site = c("Mackenzie", "Fort St John", "Peace River", "High Level", "Fort McMurray", "Fort Chipewyan", "Fort Smith"),
  #site = c("Fort Liard", "Fort Nelson", "Fort Simpson", "Watson Lake"),
  parameter = "precip",
  select_year = 2025,
  water_year_start = 4,
  end_date = "2025-05-14",
  max_missing_days = 10,
  y_min = 0,
  y_max = 150,
  legend_position = c(0.5, 0.99),
  save = F,
  file_name = paste0("All Total precip ", report_date),
  extension = "png",
  water_year = F,
  plot_width = 20, #usually 16
  plot_height = 10, #usually 10
)


plotly::ggplotly(test) %>%
  plotly::ggplotly(tooltip = c("y", "x", "Year"))

p <- plotly::plotly_build(test)

p$x$data[1] <- lapply(p$x$data[1], FUN = function(x){
  x$marker = list(opacity = 0)
  return(x)
})

p
###################################################################################################

test <- clim_plot_annual(
  site = c("Hay River", "Fort Smith", "Yellowknife"),
  parameter = "swe",
  select_year = 2023,
  water_year_start = 10,
  start_year = 2021,
  end_year = 2024,
  max_missing_days = 5,
  y_min = NA,
  y_max = NA,
  select_year_point_size = 2,
  historic_point_size = 1,
  legend_position = c(0.1, 0.95),
  save = FALSE,
  plot_width = 18,
  plot_height = 11,
  dpi = 900,
  file_name = "FC Precip",
  extension = "png"
)

plotly::ggplotly(test) %>%
  plotly::ggplotly(tooltip = c("y", "x", "Year"))

###################################################################################################

clim_plot_to_date(
    site = c("Fort Smith", "Hay River", "Yellowknife", "Norman Wells", "Fort Simpson", "Inuvik"),
    parameter = "SWE",
    select_year = lubridate::year(Sys.Date()),
    water_year_start = 10,
    start_year = 1950,
    end_year = 2024,
    end_date = Sys.Date(),
    max_missing_days = 10,
    y_min = 0,
    y_max = 350,
    select_year_point_size = 2,
    historic_point_size = 1,
    legend_position = c(0.1, 0.95),
    save = T,
    plot_width = 16,
    plot_height = 10,
    dpi = 900,
    file_name = "SWE 2024",
    extension = "png"
)

###################################################################################################

clim_calc_annual(
    site = "Fort Smith",
    parameter = "rain",
    start_year = 1950,
    end_year = 2024,
    #select_year = lubridate::year(Sys.Date()),
    water_year_start = 10
)

clim_calc_monthly(
  site = "Fort Smith",
  parameter = "rain",
  start_year = 1950,
  end_year = 2024,
  #select_year = lubridate::year(Sys.Date()),
  water_year_start = 1
)

###################################################################################################
#Generate hydro maps - can edit to plot cumulative precip, communities and/or gauges

hydro_map_basin(
  station = c("07NB001"),
  zoom = 2,
  Mack_basin = T,
  NWT_border = T,
  save = T,
  sub_basin_delineate = F,
  communities = c(  "Mackenzie",
                  "Peace River",
                  "Fort St John",
                  "High Level",
                  "Fort Chipewyan",
                  "Fort Smith",
                  "Hay River",
                  "Yellowknife",
                  "Fort Simpson",
                  "Norman Wells",
                  "Inuvik",
                  "Fort Liard",
                  "Fort Nelson",
                  "Watson Lake",
                  "Athabasca",
                  "Fort McMurray",
                  "Fort Good Hope"),
  plot_communities = F,
  plot_gauges = F,
  cum_precip = T, #plots cumulative precipitation
  save_path = paste0(user, "NT_Hydrology/Figures"),
  select_year = 2025,
  water_year_start = 4,
  water_year = F,
  end_date = "2025-10-01",
  plot_legend = T,
  adjust_manual_cum_precip = T, #change this to true if you are manually adjusting precip values based on gapfilling from FTS stns
  phantomjspath = "C:/Users/emma_riley/Documents/Modelling/Phantomjs/phantomjs/bin" #get phantomjs executable and save in similar directory
)




