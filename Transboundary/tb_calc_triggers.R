# Notes:
# 1) Enter correct file path for "file_location" argument
# 2) Create separate folder for each year of data (e.g. 2017 to present)
# 3) Ensure all necessary files are in folder (.dbf for Licences and .csv for Usage)
# 4) Ensure prefixes are correct as per code below

# Nomenclature for objects in script:
# ind_report      = individual boolean value to determine which licence reported data
# ind_allo_report = individual allocation value for each licence which reported data
# ind_allo_total  = individual allocation value for all licences
# n_report        = number of licences with reported data
# n_total         = total number of all licences
# allo_report     = allocation volume (m3) of licences with reported data
# allo_total      = total allocation volume (m3) of all licences
# prct_report     = percentage of licences with reported data
# flow_cmm        = flow at gauge in cubic metres per month
# flow_border_cmd = flow at border in cubic metres per month

tb_calc_triggers <- function(
    select_year = 2022,
    basin = "Hay",
    station_number = "07OB001"
)
{
  # Identify prefix of licence detail file. This should be consistent for each year.
  licence_file_name <- paste0("_LicenceDetail_", select_year)

  # Identify prefix of usage detail file. This should be consistent for each year.
  usage_file_name <- paste0(basin, "_UsageDetail_", select_year)

  # Import usage detail file
  if(file.exists(paste0(file_path,
                        select_year,
                        "/",
                        usage_file_name,
                        ".csv"))) {
    usage_detail <- read.csv(paste0(file_path,
                               select_year,
                               "/",
                               usage_file_name,
                               ".csv"))
  } else {
    usage_detail <- NULL
  }

  # Create data frame for surface water allocation data

  class_list <- c("SW", "GW")

  allo_data <- data.frame()

  for(i in class_list) {

    if(file.exists(paste0(file_path,
                          select_year,
                          "/",
                          basin,
                          "_",
                          i,
                          licence_file_name,
                          ".dbf"))) {

    # Import licence data for selected year
    df <- foreign::read.dbf(paste0(file_path,
                                   select_year,
                                   "/",
                                   basin,
                                   "_",
                                   i,
                                   licence_file_name,
                                   ".dbf"))
    } else { # If no allocation data are available, 2022 data will be used instead. Update to most recent available data.
      df <- foreign::read.dbf(paste0(file_path,
                                     "2022/", # Hardcode in most recent usage file
                                     basin,
                                     "_",
                                     i,
                                     "_LicenceDetail_2022",
                                     ".dbf"))
      print(paste0("Warning: No ",
                   i,
                   " allocation data exists for selected year. Data are generated using 2022 allocations as a proxy"))
    }

      df <- df %>%
      dplyr::mutate(ind_report = Water_Allo %in% usage_detail$Water.Allocation.Id) %>%
      dplyr::mutate(ind_allo_report = as.numeric(ifelse(ind_report == TRUE,
                                                      (CONSUMPTIV + LOSSES),
                                                      0)),
                    ind_allo_total = CONSUMPTIV + LOSSES) %>%
      dplyr::rename(sector = SubClassif) %>%
      dplyr::group_by(sector) %>%
      dplyr::summarize(n_report = length(which(ind_report == TRUE)),
                       n_total = length(ind_report),
                       allo_report = sum(ind_allo_report, na.rm = T),
                       allo_total = sum(ind_allo_total)) %>%
      dplyr::mutate(prct_report = allo_report / allo_total * 100) %>%
      dplyr::mutate(class = paste0(i))

    # Write .csv document to folder for selected year
    write.csv(df,
              paste0(file_path,
                     select_year,
                     "/",
                     basin,
                     "_",
                     i,
                     "_",
                     select_year,
                     ".csv"))

    allo_data <- dplyr::bind_rows(allo_data, df)

  }

    # Extract Monthly Use values

  if(!is.null(usage_detail)) {
    usage_detail$Measurement.Period.Date <- as.Date(usage_detail$Measurement.Period.Date, format = "%d-%b-%y") # Changes class to Date

    Use <- usage_detail %>%
      dplyr::group_by(Month = lubridate::month(Measurement.Period.Date),
                      Type = Water.Allocation.Type) %>%
      dplyr::summarize(Use = sum(Measurement.Value..m3., na.rm = T), .groups = "drop")

    SW_Use <- Use %>%
      dplyr::filter(Type == "SW") %>%
      dplyr::rename(SW_Use = Use)

    GW_Use <- Use %>%
      dplyr::filter(Type == "GW") %>%
      dplyr::rename(GW_Use = Use)

  }

  # Create dataframe for months and days
  month.days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  month.df <- data.frame(month.abb, month.days)
  colnames(month.df) <- c("Month", "Days")

    # Hard code Hutch Lake evaporation data
  allo_hutch <- allo_data %>%
    dplyr::filter(class == "SW" & sector == "ENVIRONMENTAL")
  allo_hutch <- as.numeric(allo_hutch$allo_total)

  # Percentage of annual evaporation by month. Calculated using Morton model, mean shallow water evaporation rates at High Level
  evap_values <- c(0,0,15,59,106,131,132,96,41,11,0,0)
  evap_function <- function(x){x/(sum(evap_values))}
  evap_high_level <- round(sapply(evap_values, evap_function),2)

  # Add High Level evapotranspiration (ET) estimate to monthly dataframe
  month.df$ET <- evap_high_level

  allo_data <- allo_data %>%
    dplyr::group_by(class) %>%
    dplyr::summarize(allo_report_m3 = sum(allo_total), # Generate a sum of Total Allocations (m3)
                     allo_no_report_m3 = sum(allo_total) - sum(allo_report)) # Generate a sum of Total Allocations (km3)

  allo_no_report <- sum(allo_data$allo_no_report_m3)

  month.df$allo_m3_SW <- (allo_data$allo_report_m3[2] * month.df$Days) / 365
  month.df$allo_m3_GW <- (allo_data$allo_report_m3[1] * month.df$Days) / 365
  month.df$allo_m3 <- month.df$allo_m3_SW + month.df$allo_m3_GW

  # This next line requires the "hydro_calc_daily" function, which should be called using source
  flow_data <- hydro_calc_daily(
    parameter = "Flow",
    station_number = station_number,
    start_date = as.Date(paste0(select_year, "-01-01")),
    end_date = if(select_year < lubridate::year(Sys.Date())) {
      as.Date(paste0(select_year, "-12-31"))
    } else {
      Sys.Date()
    }
    )

  flow_data <- flow_data %>%
    dplyr::select(Date, Value) %>%
    dplyr::filter(lubridate::year(Date) == select_year) %>%
    dplyr::group_by(Month = lubridate::month(Date, label = TRUE)) %>%
    dplyr::summarize(flow_cmm = sum(Value * 86400),
                     flow_border_cmm = (sum(Value * 86400)) * 0.94)

  # Code to add NA to months with missing data
  # if(nrow(SW_Use) < 12){
  #   SW_Use <- berryFunctions::insertRows(SW_Use, c(nrow(SW_Use)+1:(12-nrow(SW_Use)-1)), new = NA)
  #   }
  #
  # if(nrow(GW_Use) < 12){
  #   GW_Use <- berryFunctions::insertRows(GW_Use, c(nrow(GW_Use)+1:(12-nrow(GW_Use)-1)), new = NA)
  #   }

  if(nrow(SW_Use) < 12) {
    month.df <- month.df[1:nrow(SW_Use),]
    flow_data <- flow_data[1:nrow(SW_Use),]
  }


  if(!is.null(usage_detail)) {
    plot_data <- dplyr::bind_cols(dplyr::select(month.df, Month, Days, ET, allo_m3),
                                  dplyr::select(flow_data, flow_cmm, flow_border_cmm),
                                  dplyr::select(SW_Use, SW_Use),
                                  dplyr::select(GW_Use, GW_Use)) %>%
      dplyr::mutate(flow_natural_cmm_allo = flow_border_cmm + allo_m3) %>%
      dplyr::mutate(flow_natural_cmm_use = flow_border_cmm + (SW_Use + GW_Use)) %>%
      dplyr::mutate(allo_prct = allo_m3 / flow_natural_cmm_allo) %>%
      dplyr::mutate(use = ifelse(is.null(usage_detail), as.numeric(NA), SW_Use + GW_Use))

    allo_prct_used <- sum(plot_data$use / sum(plot_data$allo_m3)) # Important value for annual report
    print(paste0("Percentage of allocation to use for non reports: ",
                 round(allo_prct_used * 2 * 100, 1),
                 "%"))

  } else {
    plot_data <- dplyr::bind_cols(dplyr::select(month.df, Month, Days, ET, allo_m3),
                                  dplyr::select(flow_data, flow_cmm, flow_border_cmm)) %>%
      dplyr::mutate(flow_natural_cmm = flow_border_cmm + allo_m3) %>%
      dplyr::mutate(allo_prct = allo_m3 / flow_natural_cmm) %>%
      dplyr::mutate(use = ifelse(is.null(usage_detail), as.numeric(NA), NA))
  }

  prct_no_report <- allo_no_report / sum(plot_data$allo_m3)

  if(!is.null(usage_detail)) {
    plot_data <- plot_data %>%
      dplyr::mutate(et_hutch = ET * allo_hutch) %>%
      dplyr::mutate(use_no_report = ((allo_prct_used * 2) * (allo_no_report * Days / 365)) + use + et_hutch) %>%
      dplyr::mutate(use_prct = use_no_report / flow_natural_cmm_use)  %>%
      dplyr::select(Month, allo_prct, use_prct)
  } else {
    plot_data <- plot_data %>%
      dplyr::mutate(et_hutch = ET * allo_hutch) %>%
      dplyr::select(Month, allo_prct)
  }

  # Insert NAs if plot_data does not have values for all months
  if(nrow(plot_data) < 12) {
    plot_data[c((nrow(plot_data)+1):12),] <- NA
    plot_data[,1] <- month.abb
  }

  dplyr::as_tibble(plot_data)

}

#Determining use vs allocation values for SW and GW
# b <- sum(month.df$allo_m3_GW)
# a <- sum(GW_Use$GW_Use)
# 
# round((a/b)*100, 1)
# 
# b <- sum(month.df$allo_m3_SW)
# a <- sum(SW_Use$SW_Use)
# 
# round((a/b)*100, 1)

