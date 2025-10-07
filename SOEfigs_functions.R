#SOE figures
#
#Required functions:

`%!in%` = Negate(`%in%`)

appendvar = function (var1, var2, var3){
  mtch = match(var1, var2)
  var4<-var3[match(var1, var2)]
  return(var4)
}

`%>%` <- magrittr::`%>%`

# Function for Sen slope

sen <- function(...,weights = NULL) {
  mblm::mblm(...)
}

# p.value.function

p.value.function <- function(significance, mk.value)
  
{
  if (significance < 0.25 && significance > 0.1) { 
    
    if (mk.value[2] < 0.01) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray20", size = 2)  
    } else if (mk.value[2] < 0.05) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray40", size = 1.5)
    } else if (mk.value[2] < 0.1) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray60", size = 1)
    } else if (mk.value[2] < 0.25) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray80", size = 1)
    }
    
  } else if (significance >= 0.1) {
    
    if (mk.value[2] < 0.01) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray20", size = 2)  
    } else if (mk.value[2] < 0.05) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray40", size = 1.5)
    } else if (mk.value[2] < 0.1) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray60", size = 1)
    }
    
  } else if (significance >= 0.05) {
    
    if (mk.value[2] < 0.01) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray20", size = 2)  
    } else if (mk.value[2] < 0.05) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray40", size = 1.5)
    }
    
  } else if (significance <= 0.01) {
    
    if (mk.value[2] < 0.01) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray20", size = 2)  
    }
  }
}

#Creating Yields Table:

YieldsTable <- function(n,
                        start_year,
                        end_year,
                        DOR,
                        exclude_years,
                        data)
  
{
  
  df <- data.frame()
  
  for(i in data) {
    
    station_info <- tidyhydat::hy_stations(station_number = i)
    
    flow.df <- tidyhydat::hy_daily_flows(station_number = i) %>%
      dplyr::mutate(Year = lubridate::year(Date), 
                    jday = lubridate::yday(Date),
                    date = Date) %>%
      dplyr::group_by(Year) %>%
      dplyr::reframe(Flow = sum(Value, na.rm = T)*60*60*24/station_info$DRAINAGE_AREA_GROSS/1000000*1000,
                       days_of_record = length(Value),
                       last_date = max(Date, na.rm=TRUE)) %>%
      dplyr::mutate(Station_ID = station_info$STATION_NUMBER,
                    drainage = station_info$DRAINAGE_AREA_GROSS) %>%
      dplyr::mutate(size = ifelse(drainage >= 100000, "large",
                                  ifelse(drainage <= 10000, "small", 
                                         "medium"))) %>%
      subset(Year >= start_year) %>%
      subset(Year <= end_year) %>%
      subset(days_of_record >= DOR) 
    
    if (is.na(exclude_years) == TRUE) {
      flow.df <- flow.df
    } else {
      flow.df <- flow.df[!(flow.df$Year %in% exclude_years),]
    }
    
    df <- dplyr::bind_rows(df, flow.df)
    
  }
  
  df
  
}

#Creating Flows Table:

FlowsTable <- function(n,
                        exclude_years,
                        data = NTstns)
  
{
  
  df <- data.frame()
  
  
  for(i in data) {
    
    
    station_info <- tidyhydat::hy_stations(station_number = i)
    
    flow.df <- tidyhydat::hy_daily_flows(station_number = i) %>%
      dplyr::mutate(Year = lubridate::year(Date), 
                    jday = lubridate::yday(Date),
                    date = Date) %>%
      dplyr::group_by(STATION_NUMBER) %>%
      dplyr::reframe(last_date = max(Date, na.rm=TRUE))
    
    if (is.na(exclude_years) == TRUE) {
      flow.df <- flow.df
    } else {
      flow.df <- flow.df[!(flow.df$Year %in% exclude_years),]
    }
    
    df <- dplyr::bind_rows(df, flow.df)
    
  }
  
  df
  
}

#Creating max date Table:

Table_maxdate <- function(n,
                        start_year,
                        end_year,
                        DOR,
                        exclude_years,
                        data)
  
{
  
  df <- data.frame()
  
  for(i in data) {
    
    station_info <- tidyhydat::hy_stations(station_number = i)
    
    flow.df <- tidyhydat::hy_daily_flows(station_number = i) %>%
      dplyr::mutate(Year = lubridate::year(Date), 
                    jday = lubridate::yday(Date),
                    date = Date) %>%
      dplyr::group_by(STATION_NUMBER) %>%
      dplyr::reframe(maxyear = max(Year, na.rm=TRUE))
    
    if (is.na(exclude_years) == TRUE) {
      flow.df <- flow.df
    } else {
      flow.df <- flow.df[!(flow.df$Year %in% exclude_years),]
    }
    
    df <- dplyr::bind_rows(df, flow.df)
    
  }
}

#Creating Levels Table:

LevelsTable <- function(n,
                        start_year,
                        end_year,
                        DOR,
                        exclude_years,
                        data)
  
{
  
  df <- data.frame()
  
  for(i in data) {
    
    station_info <- tidyhydat::hy_stations(station_number = i)
    
    flow.df <- tidyhydat::hy_daily_levels(station_number = i) %>%
      dplyr::mutate(Year = lubridate::year(Date), 
                    jday = lubridate::yday(Date)) %>%
      dplyr::group_by(Year) %>%
      dplyr::reframe(Level = mean(Value, na.rm = T),
                       days_of_record = length(Value)) %>%
      dplyr::mutate(Station_ID = i,
                    drainage = station_info$DRAINAGE_AREA_GROSS) %>%
      dplyr::mutate(size = ifelse(drainage >= 100000, "large",
                                  ifelse(drainage <= 10000, "small", 
                                         "medium"))) %>%
      subset(Year >= start_year) %>%
      subset(Year <= end_year) %>%
      subset(days_of_record >= DOR) 
    
    if (is.na(exclude_years) == TRUE) {
      flow.df <- flow.df
    } else {
      flow.df <- flow.df[!(flow.df$Year %in% exclude_years),]
    }
    
    df <- dplyr::bind_rows(df, flow.df)
    
  }
  
  df
  
}

#function producing annual river yields based on station
SOEfigs_bystn = function(data, station_number, start_year, p.value, exclude_years)
  
{plot <- data %>%
  dplyr::filter(Station_ID == station_number) %>%
  dplyr::filter(Year >= start_year) %>%
  dplyr::filter(Year %!in% exclude_years) %>%
  dplyr::group_by(Year) %>%
  dplyr::reframe(meanflow = (mean(Flow, na.rm=TRUE)))

test.mk <- Kendall::MannKendall(plot$meanflow)
test.ss <- trend::sens.slope(plot$meanflow, conf.level = 0.95)
p.value.function(p.value, as.numeric(test.mk))

plot_ = ggplot2::ggplot(plot, ggplot2::aes(x = Year, y = meanflow)) +
  ggplot2::geom_point(shape = 19) +
  ggplot2::geom_smooth(method="lm", se=FALSE) +
  ggplot2::theme_classic() +
  ggplot2::ylim(min(plot$meanflow), max(plot$meanflow)) +
  ggplot2::expand_limits(y=0) +
  ggplot2::ylab("Mean annual river yield (mm)") +
  ggplot2::labs(title = paste(station_number)) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
  ggplot2::labs(title = paste(station_number),
                subtitle = paste("",
                                 "\n\np-value = ", 
                                 round(as.numeric(test.mk[2]), digits = 5),
                                 "\nSen's slope = ",
                                 paste(round(as.numeric(test.ss[1]), digits = 2)),
                                 "\nTau = ",
                                 round(as.numeric(test.mk[1]), digits = 2),
                                 sep = ""))
plot(plot_)
}

#The following function is the same as above but for lake levels:
SOEfigs_bystn_level = function(data, 
                               station_number, 
                               start_year,
                               p.value, 
                               exclude_years)
  
{plot <- data %>%
  dplyr::filter(Station_ID == station_number) %>%
  dplyr::filter(Year >= start_year) %>%
  dplyr::filter(Year %!in% exclude_years) %>%
  dplyr::group_by(Year) %>%
  dplyr::reframe(meanlevel = (mean(Level, na.rm=TRUE)))

test.mk <- Kendall::MannKendall(plot$meanlevel)
test.ss <- trend::sens.slope(plot$meanlevel, conf.level = 0.95)
p.value.function(p.value, as.numeric(test.mk))

plot_ = ggplot2::ggplot(plot, ggplot2::aes(x = Year, y = meanlevel)) +
  ggplot2::geom_point(shape = 19) +
  ggplot2::geom_smooth(method="lm", se=FALSE) +
  ggplot2::theme_classic() +
  ggplot2::ylim(min(plot$meanlevel), max(plot$meanlevel)) +
  ggplot2::expand_limits(y=0) +
  ggplot2::ylab("Mean annual Water level (m)") +
  ggplot2::labs(title = paste(station_number)) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
  ggplot2::labs(title = paste(station_number),
                subtitle = paste("",
                                 "\n\np-value = ", 
                                 round(as.numeric(test.mk[2]), digits = 5),
                                 "\nSen's slope = ",
                                 paste(round(as.numeric(test.ss[1]), digits = 2)),
                                 "\nTau = ",
                                 round(as.numeric(test.mk[1]), digits = 2),
                                 sep = ""))
plot(plot_)
}

#The following function plots yields over time based on the size of the drainage basin
SOEfigs_drainage = function(data, drainage_size, start_year, p.value, exclude_years){
  foo = function(x){
    paste(toupper(substring(x, 1, 1)),
          tolower(substring(x, 2, nchar(x))),
          sep = "")}
  
  plot <- data %>%
    dplyr::filter(size == drainage_size) %>%
    dplyr::filter(Year >= start_year) %>%
    dplyr::filter(Year %!in% exclude_years) %>%
    dplyr::group_by(Year) %>%
    dplyr::reframe(meanflow = (mean(Flow, na.rm=TRUE)))
  
  test.mk <- Kendall::MannKendall(plot$meanflow)
  test.ss <- trend::sens.slope(plot$meanflow, conf.level = 0.95)
  p.value.function(p.value, as.numeric(test.mk))
  
  plot_ = ggplot2::ggplot(plot, ggplot2::aes(x = Year, y = meanflow)) +
    ggplot2::geom_point(shape = 19) +
    ggplot2::geom_smooth(method="lm", se=FALSE) +
    ggplot2::theme_classic() +
    ggplot2::ylim(min(plot$meanflow), max(plot$meanflow)) +
    ggplot2::expand_limits(y=0) +
    ggplot2::ylab("Mean annual river yield (mm)") +
    ggplot2::labs(title = paste(drainage_size, "watersheds")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(title = paste(foo(drainage_size), "watersheds"),
                  subtitle = paste("",
                                   "\n\np-value = ", 
                                   round(as.numeric(test.mk[2]), digits = 5),
                                   "\nSen's slope = ",
                                   paste(round(as.numeric(test.ss[1]), digits = 2)),
                                   "\nTau = ",
                                   round(as.numeric(test.mk[1]), digits = 2),
                                   sep = ""))
  plot(plot_)
}

#Flows function - to plot mean seasonal flow over ten year periods for a) Rengleng
#River, b) Peel River, c) Hay River and d) Mackenzie River


SeasonalMeanFlows <- function(station_number,
                              exclude_years,
                              timeperiod_1,
                              timeperiod_2)
  
{foo = function(x){
  paste(toupper(substring(x, 1, 1)),
        tolower(substring(x, 2, nchar(x))),
        sep = "")}

station_info <- tidyhydat::hy_stations(station_number = station_number)

flows1 <- tidyhydat::hy_daily_flows(station_number = station_number) %>%
  dplyr::mutate(Year = lubridate::year(Date), 
                jday = lubridate::yday(Date)) %>%
  dplyr::filter(Year %in% timeperiod_1) %>%
  dplyr::filter(Year %!in% exclude_years) %>%
  dplyr::group_by(jday) %>%
  dplyr::reframe(meanflow = mean(Value, na.rm=TRUE),
                   drainage = station_info$DRAINAGE_AREA_GROSS,
                   station_name = foo(station_info$STATION_NAME),
                   size = ifelse(station_info$DRAINAGE_AREA_GROSS >= 100000, "Large",
                                 ifelse(station_info$DRAINAGE_AREA_GROSS <= 10000, "Small", 
                                        "Medium")),
                   month = lubridate::month(Date))

flows2 <- tidyhydat::hy_daily_flows(station_number = station_number) %>%
  dplyr::mutate(Year = lubridate::year(Date), 
                jday = lubridate::yday(Date)) %>%
  dplyr::filter(Year %in% timeperiod_2) %>%
  dplyr::filter(Year %!in% exclude_years) %>%
  dplyr::group_by(jday) %>%
  dplyr::reframe(meanflow = mean(Value, na.rm=TRUE),
                   drainage = station_info$DRAINAGE_AREA_GROSS,
                   station_name = foo(station_info$STATION_NAME),
                   size = ifelse(station_info$DRAINAGE_AREA_GROSS >= 100000, "Large",
                                 ifelse(station_info$DRAINAGE_AREA_GROSS <= 10000, "Small", 
                                        "Medium")),
                   month = lubridate::month(Date))

plot = ggplot2::ggplot() +
  ggplot2::geom_line(flows1, mapping = ggplot2::aes(jday, meanflow, color = paste(min(timeperiod_1), "-", max(timeperiod_1))))+
  ggplot2::geom_line(flows2, mapping = ggplot2::aes(jday, meanflow, color = paste(min(timeperiod_2), "-", max(timeperiod_2))))+
  ggplot2::theme_classic() +
  ggplot2::ylim(min(c(flows1$meanflow, flows2$meanflow)), max(c(flows1$meanflow, flows2$meanflow))) +
  ggplot2::expand_limits(y=0) +
  ggplot2::ylab(label = bquote(Mean~Flow~(m ^2~s^-1))) +
  ggplot2::xlab("Julian Day") +
  ggplot2::theme(plot.title = ggplot2::element_text(size =18, face = "bold", hjust = 0.91)) +
  ggplot2::theme(plot.subtitle = ggplot2::element_text(size =15, face = "plain", hjust = 0.75)) +
  ggplot2::labs(title = paste(flows2$size, "watershed"),
                subtitle = bquote(.(flows2$station_name)~Drainage:~.(flows2$drainage)~km^2)) +
  ggplot2::scale_colour_manual(name="",
                               values=c("1981 - 2000"="lightgreen", "2001 - 2020"="darkblue"))+
  ggplot2::theme(legend.position = c(.88, .9))

plot}

#Flows function - to plot mean seasonal flow over multiple years

station_number = "10KB001"

SeasonalMeanFlows_eachyr <- function(station_number,
                              exclude_years,
                              min_year = NA,
                              max_year = NA)
  
{foo = function(x){
  paste(toupper(substring(x, 1, 1)),
        tolower(substring(x, 2, nchar(x))),
        sep = "")}

station_info <- tidyhydat::hy_stations(station_number = station_number)

flows1 <- tidyhydat::hy_daily_flows(station_number = station_number) %>%
  dplyr::mutate(Year = lubridate::year(Date), 
                jday = lubridate::yday(Date)) %>%
  dplyr::filter(Year >= min_year,
                Year <= max_year) %>%
  dplyr::group_by(Year, jday) %>%
  dplyr::reframe(meanflow = mean(Value, na.rm=TRUE),
                 drainage = station_info$DRAINAGE_AREA_GROSS,
                 station_name = foo(station_info$STATION_NAME),
                 size = ifelse(station_info$DRAINAGE_AREA_GROSS >= 100000, "Large",
                               ifelse(station_info$DRAINAGE_AREA_GROSS <= 10000, "Small", 
                                      "Medium")),
                 month = lubridate::month(Date))


plot = ggplot2::ggplot() +
  ggplot2::geom_line(flows1, mapping = ggplot2::aes(jday, meanflow, group = Year, color = Year))+
  ggplot2::theme_classic() +
  ggplot2::ylim(min(flows1$meanflow), max(flows1$meanflow)) +
  ggplot2::expand_limits(y=0) +
  ggplot2::ylab(label = bquote(Mean~Flow~(m ^2~s^-1))) +
  ggplot2::xlab("Julian Day") +
  ggplot2::theme(plot.title = ggplot2::element_text(size =18, face = "bold", hjust = 0.91)) +
  ggplot2::theme(plot.subtitle = ggplot2::element_text(size =15, face = "plain", hjust = 0.75)) +
  ggplot2::labs(title = paste(flows1$size, "watershed"),
                subtitle = bquote(.(flows1$station_name)~Drainage:~.(flows1$drainage)~km^2)) +
  ggplot2::theme(legend.position = c(.88, .9))

plot}

#Flows function - to plot flows for a station

station_number = "07OC001"
year = 2020

SeasonalFlows_individualyrs <- function(station_number,
                                        year)
  
{foo = function(x){
  paste(toupper(substring(x, 1, 1)),
        tolower(substring(x, 2, nchar(x))),
        sep = "")}

station_info <- tidyhydat::hy_stations(station_number = station_number)

flows1 <- tidyhydat::hy_daily_flows(station_number = station_number) %>%
  dplyr::mutate(Year = lubridate::year(Date), 
                jday = lubridate::yday(Date)) %>%
  dplyr::filter(Year == year) %>%
  dplyr::mutate(size = ifelse(station_info$DRAINAGE_AREA_GROSS >= 100000, "Large",
                              ifelse(station_info$DRAINAGE_AREA_GROSS <= 10000, "Small", 
                                     "Medium")),
                station_name = foo(station_info$STATION_NAME),
                drainage = station_info$DRAINAGE_AREA_GROSS)

plot = ggplot2::ggplot() +
  ggplot2::geom_line(flows1, mapping = ggplot2::aes(jday, Value))+
  ggplot2::theme_classic() +
  ggplot2::ylim(min(flows1$Value), max(flows1$Value)) +
  ggplot2::expand_limits(y=0) +
  ggplot2::ylab(label = bquote(Flow~(m ^2~s^-1))) +
  ggplot2::xlab("Julian Day") +
  ggplot2::theme(plot.title = ggplot2::element_text(size =18, face = "bold", hjust = 0.91)) +
  ggplot2::theme(plot.subtitle = ggplot2::element_text(size =15, face = "plain", hjust = 0.75)) +
  ggplot2::labs(title = paste(flows1$size, "watershed", "Year:", year),
                subtitle = bquote(.(flows1$station_name)~Drainage:~.(flows1$drainage)~km^2)) +
  ggplot2::theme(legend.position = c(.88, .9))

plot

}

