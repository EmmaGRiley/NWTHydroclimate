#TB Hydroclimate plots
#Run tb_source_functions.R and the setup lines at the top of TB_Functions.R first. See README.md.

report_year <- 2025 # year the technical report is for - change this each year

save_path <- paste0(file_path, "Figures and Tables/")

#Hay River
#Discharge plots
hydro_plot_dayofyear(
  station_number = "07OB001",
  parameter = "flow",
  select_years = c(report_year),
  after_bennett = FALSE,
  historic_min = NA,
  historic_max = report_year - 1,
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
  save = TRUE,
  plot_width = 18,
  plot_height = 11,
  dpi = 900,
  file_name = "Default hydrometric plot",
  extension = "png")

#Cumulative precip/rain plots - example usage for 2021 report
clim_plot_dayofyear(
    site = "Hay River",
    parameter = "rain",
    start_year = 1950,
    end_year = report_year - 1,
    select_year = report_year,
    cumulative = T,
    plot_width = 18,
    plot_height = 11,
    y_min = NA,
    y_max = 600
)

clim_plot_dayofyear(
  site = "High Level",
  parameter = "rain",
  start_year = 1967,
  end_year = report_year - 1,
  select_year = report_year,
  cumulative = T,
  plot_width = 12,
  plot_height = 11,
  y_min = NA,
  y_max = 600
)

clim_plot_dayofyear(
  site = "Fort Nelson",
  parameter = "rain",
  start_year = 1950,
  end_year = report_year - 1,
  select_year = report_year,
  cumulative = T,
  plot_width = 12,
  plot_height = 11,
  y_min = NA,
  y_max = 600
)

#Air temperature plots

#daily values
clim_plot_dayofyear(
  site = "Hay River",
  parameter = "temp",
  start_year = 1950,
  end_year = report_year - 1,
  select_year = report_year,
  cumulative = F,
  plot_width = 18,
  plot_height = 11
)

clim_plot_dayofyear(
  site = "High Level",
  parameter = "temp",
  start_year = 1950,
  end_year = report_year - 1,
  select_year = report_year,
  cumulative = F,
  plot_width = 18,
  plot_height = 11
)

clim_plot_dayofyear(
  site = "Fort Nelson",
  parameter = "temp",
  start_year = 1950,
  end_year = report_year - 1,
  select_year = report_year,
  cumulative = F,
  plot_width = 18,
  plot_height = 11
)

#Average monthly values

Communities <- c(
  "Fort Smith",
  "Hay River",
  "High Level",
  "Fort Nelson",
  "Peace River",
  "Fort Chipewyan",
  "Mackenzie"
)


for(i in Communities) {
  clim_plot_monthly(
    site = i,
    parameter = "mean_temp",
    start_year = 1950,
    select_year = report_year,
    water_year_start = 1,
    max_missing_days = 10,
    water_year_end = 12,
    legend_position = c(0.5, 0.5),
    save = T,
    file_name = paste0(i, "_temp_wy"),
    water_year = F,
    y_max = 15,
    plot_width = 18
  )
}

#Runoff and runoff trends - I'm pretty sure we are not doing these for the annual technical reports
tb_watersheds <- c("07OB001",
                   "07NB001")
YieldsTB = YieldsTable(start_year = 1960,
                     end_year = report_year,
                     data = tb_watersheds,
                     DOR = 350,
                     exclude_years = NA)

SOEfigs_bystn(data = YieldsTB, station_number = "07NB001", start_year = 1972, p.value = 0.05, 
              exclude_years = c(2023:2025))

SOEfigs_bystn(data = YieldsTB, station_number = "07OB001", start_year = 1963, p.value = 0.05, 
              exclude_years = c(2023:2025))

#Slave River

hydro_plot_dayofyear(
  station_number = "07NB001",
  parameter = "flow",
  select_years = c(report_year),
  after_bennett = FALSE,
  historic_min = NA,
  historic_max = report_year - 1,
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
  save = TRUE,
  plot_width = 18,
  plot_height = 11,
  dpi = 900,
  file_name = "Default hydrometric plot",
  extension = "png")

#Cumulative precip/rain plots
clim_plot_dayofyear(
  site = "Fort Smith",
  parameter = "rain",
  start_year = 1950,
  end_year = report_year - 1,
  select_year = report_year,
  cumulative = T,
  plot_width = 12,
  plot_height = 11,
  y_min = NA,
  y_max = 600
)

clim_plot_dayofyear(
  site = "Fort Chipewyan",
  parameter = "rain",
  start_year = 1950,
  end_year = report_year - 1,
  select_year = report_year,
  cumulative = T,
  plot_width = 12,
  plot_height = 11,
  y_min = NA,
  y_max = 600
)

clim_plot_dayofyear(
  site = "Mackenzie",
  parameter = "rain",
  start_year = 1973,
  end_year = report_year - 1,
  select_year = report_year,
  cumulative = T,
  plot_width = 12,
  plot_height = 11,
  y_min = NA,
  y_max = 600
)

clim_plot_dayofyear(
  site = "Peace River",
  parameter = "rain",
  start_year = 1950,
  end_year = report_year - 1,
  select_year = report_year,
  cumulative = T,
  plot_width = 12,
  plot_height = 11,
  y_min = NA,
  y_max = 600
)

#Total annual streamflow 

#Hay River
df <- hydro_calc_daily(station = "07OB001",
                       parameter = "flow",
                       water_year_start = 1,
                       start_date = "1964-01-01",
                       end_date = paste0(report_year, "-12-31"))

df <- df %>%
  # dplyr::filter(Month > 4,
  #               Month < 8) %>%
  dplyr::group_by(CalendarYear) %>%
  dplyr::mutate(Count = ifelse(is.na(Value), 1, 0))


df <- df %>%
  dplyr::group_by(CalendarYear) %>%
  dplyr::summarise(cum_value = sum(Value, na.rm=T),
                   missing_days = sum(Count, na.rm=T))

df$cum_value <- ifelse(df$missing_days > 10, NA, df$cum_value)
df$cum_value <- df$cum_value * 86400 #look at df here to get total discharge. Discharge values are in m^3/s, so they must be multiplied by seconds in a day (86 400 or 60*60*24)
#total discharge for 2022 -> 7 868 671 774 (roughly 8 billion)

#What is considered normal ?
quantile(df$cum_value, 0.50, na.rm=T) #This gives you what a normal year would be, based on the median

#what is the percent of normal of the current year's cumulative discharge?
Cumulative_flows_current <- df$cum_value[df$CalendarYear == report_year] 
Cumulative_flows_mean <- mean(df$cum_value, na.rm=T)
Cumulative_flows_current/Cumulative_flows_mean * 100

plot(df$cum_value~df$CalendarYear) #this shows you total annual flows over time

#look at peak values

df <- hydro_calc_daily(station = "07OB001",
                       parameter = "flow",
                       water_year_start = 1,
                       start_date = "1900-01-01",
                       end_date = paste0(report_year, "-12-31"))

df <- df %>%
  # dplyr::filter(Month > 4,
  #               Month < 8) %>%
  dplyr::group_by(CalendarYear) %>%
  dplyr::mutate(peak = max((Value), na.rm=T)) %>%
  dplyr::select(CalendarYear, peak) %>%
  dplyr::filter(CalendarYear > 1963)

df <- unique (df)

#look at mean values

df <- hydro_calc_daily(station = "07OB001",
                       parameter = "flow",
                       water_year_start = 1,
                       start_date = "1964-01-01",
                       end_date = paste0(report_year, "-12-31"))

df <- df %>%
  # dplyr::filter(Month > 4,
  #               Month < 8) %>%
  dplyr::group_by(CalendarYear) %>%
  dplyr::mutate(Count = ifelse(is.na(Value), 1, 0))


df <- df %>%
  dplyr::group_by(CalendarYear) %>%
  dplyr::summarise(mean_value = mean(Value, na.rm=T),
                   missing_days = sum(Count, na.rm=T))

df$mean_value <- ifelse(df$missing_days > 10, NA, df$mean_value)

mean_annual_flow <- mean(df$mean_value, na.rm=T)

##########################################################

plot <- hydro_plot_dayofyear_cumulative(station_number = "07OB001",
                                parameter = "flow",
                                select_year = c(report_year),
                                after_bennett = FALSE,
                                historic_min = NA,
                                historic_max = report_year - 1,
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
                                save = TRUE,
                                plot_width = 18,
                                plot_height = 11,
                                dpi = 900,
                                file_name = "Default hydrometric plot",
                                extension = "png",
                                interactive = T,
                                y_axis_title = "Discharge (m3 s-1)")

plot <- hydro_plot_dayofyear(station_number = "07NB001",
                                        parameter = "flow",
                                        select_year = c(report_year),
                                        after_bennett = TRUE,
                                        historic_min = NA,
                                        historic_max = report_year - 1,
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
                                        save = TRUE,
                                        plot_width = 18,
                                        plot_height = 11,
                                        dpi = 900,
                                        file_name = "Default hydrometric plot",
                                        extension = "png")
plot <- plot +
  ggplot2::labs(
    x = "Day of Year",
    y = "Discharge (m\u00b3/s)"   # m³/s as a single string
  )

plot<-plotly::ggplotly(plot, tooltip = c("y", "x", "colour", "ymin", "ymax", "yintercept"))

plot <- plotly::plotly_build(plot)

plot

#Slave River
df <- hydro_calc_daily(station = "07NB001",
                       parameter = "flow",
                       water_year_start = 1,
                       start_date = "1972-01-01",
                       end_date = paste0(report_year, "-12-31"))

df <- df %>%
  # dplyr::filter(Month > 4,
  #               Month < 8) %>%
  dplyr::group_by(CalendarYear) %>%
  dplyr::mutate(Count = ifelse(is.na(Value), 1, 0))


df <- df %>%
  dplyr::group_by(CalendarYear) %>%
  dplyr::summarise(cum_value = sum(Value, na.rm=T),
                   missing_days = sum(Count, na.rm=T))


#df$cum_value <- ifelse(df$missing_days > 10, NA, df$cum_value)

df$cum_value <- df$cum_value * 86400
df$cum_value[df$CalendarYear == 2020] <- 142109496749 #gap-filling using techniques from the 2020-2021 report

#look at df here to get total discharge. Discharge values are in m^3/s, so they must be multiplied by seconds in a day (86 400 or 60*60*24)
#total discharge for 2022 ->115 337 088 000 (115 billion)

#What is considered normal ?
quantile(df$cum_value, 0.50, na.rm=T) #This gives you what a normal year would be

plot(df$cum_value~df$CalendarYear) #this shows you total annual flows over time

#mean cumulative flows from 1972 onwards is 105 billion m3 in 2019. 

mean_annual_flow <- mean(df$cum_value, na.rm=T)

current_annual_flow <- df$cum_value[df$CalendarYear == report_year] 

#percent of normal of cumulative flows
current_annual_flow/mean_annual_flow * 100

#Determining slave river allocations
#consumptive use threshold = 2
2/(mean_annual_flow/1000000000) # 1.9%
#The consumptive use threshold should remain at 1.9% unless the long term mean annual flow changes significantly

############################################################################
plot <- hydro_plot_dayofyear_cumulative(station_number = "07NB001",
                                parameter = "flow",
                                select_year = c(report_year),
                                after_bennett = TRUE,
                                historic_min = NA,
                                historic_max = report_year - 1, #historic max should always be select_year - 1
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
                                save = TRUE,
                                plot_width = 18,
                                plot_height = 11,
                                dpi = 900,
                                file_name = "Default hydrometric plot",
                                extension = "png",
                                interactive = T,
                                y_axis_title = "Discharge (m3 s-1)")

plot<-plotly::ggplotly(plot, tooltip = c("y", "x", "colour", "ymin", "ymax", "yintercept"))

plot <- plotly::plotly_build(plot)

plot

##########################################################################################
# Other interactive plots

interactive <- clim_plot_dayofyear(
  site = "Aklavik",
  parameter = "temp",
  start_year = 1953,
  end_year = 2024,
  select_year = 2026,
  cumulative = T,
  plot_width = 12,
  plot_height = 11,
  y_min = NA,
  y_max = 600,
  water_year = TRUE,
  water_year_start = 10,
)

plot<-plotly::ggplotly(interactive, tooltip = c("y", "x", "colour", "ymin", "ymax", "yintercept"))

plot <- plotly::plotly_build(plot)

plot

###########################################################################################

