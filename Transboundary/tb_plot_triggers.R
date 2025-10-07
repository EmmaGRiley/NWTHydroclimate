# Notes:
# 1) Enter correct file path for "file_location" argument
# 2) Create separate folder for each year of data (e.g. 2017 to present)
# 3) Ensure all necessary files are in folder (.dbf for Licences and .csv for Usage)
# 4) Ensure prefixes are correct as per code below

tb_plot_triggers <- function(
    select_year = 2022, # year to run analysis
    basin = "Hay", # name of basin to run analysis. Note: code is currently only written for Hay
    station_number = "07OB001", # hydrometric station to calculate flows
    y_max = NA, # maximum value on y-axis
    num_labels = TRUE, # numbered values above each bar
    num_offset = 0.0025, # offset for where numbers are positioned above each bar
    save_plot = TRUE, # saves plot(s) to selected folder
    plot_width = 20, # width (cm) of output plot(s)
    plot_height = 12, # height (cm) of output plot(s)
    dpi = 300 # resolution of output plot(s)
)
{

  plot_data <- tb_calc_triggers(
    select_year = select_year,
    basin = "Hay",
    station_number = "07OB001"
  )

  # Convert Months column to factor for plotting
plot_data$Month <- factor(plot_data$Month,
                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Import usage detail file - ER added lines 32 to 52

# Identify prefix of licence detail file. This should be consistent for each year.
licence_file_name <- paste0("_LicenceDetail_", select_year)

# Identify prefix of usage detail file. This should be consistent for each year.
usage_file_name <- paste0(basin, "_UsageDetail_", select_year)

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

# Fix scale on y-axis if y_max argument is defined
if(is.na(y_max) & max(plot_data$allo_prct, na.rm = T) > 0.1) {
  y_max <- max(plot_data$allo_prct, na.rm = T)
} else if (is.na(y_max)) {
  y_max = 0.1
}

# Plot monthly allocation data as perentage of natural flow
plot_allo <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Month, y = allo_prct)) +
  ggplot2::geom_col() +
  ggplot2::theme_classic() +
  ggplot2::geom_hline(yintercept = 0.025, linetype = "dashed") +
  ggplot2::labs(title = paste0("Hay River ", select_year, " monthly allocation as percentage of natural flow"),
                x = "Month", y = "Percent of Natural Border Flow") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                              limits = c(0, y_max)) +
  ggplot2::theme(text = ggplot2::element_text(size = 12, family = "serif"))

# Add labels to bars if num_labels argument is selected
if(num_labels == T) {
  plot_allo <- plot_allo +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(allo_prct, accuracy = .1),
                                    y = allo_prct + num_offset),
                       position = ggplot2::position_dodge(0.9),
                       vjust = 0)
}

# Save plot to folder is save_plot argument is selected
if(save_plot == T) {
  ggplot2::ggsave(paste(basin, "_Trigger1_", select_year, ".png"), plot = plot_allo, device = "png",
                  path = paste0(file_path, select_year),
                  scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
}

# Plot monthly use estimate as percentage of natural flow is usage data are available
if(!is.null(usage_detail)) {
  plot_use <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Month, y = use_prct)) +
    ggplot2::geom_col() +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(yintercept = 0.04, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0.05) +
    ggplot2::labs(title = paste0("Hay River ", select_year, " monthly use estimate as percentage of natural flow"),
                  x = "Month", y = "Percent of Natural Border Flow") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                                limits = c(0, y_max)) +
    ggplot2::theme(text = ggplot2::element_text(size = 12, family = "serif"))

  # Add labels to bars if num_labels argument is selected
  if(num_labels == T) {
    plot_use <- plot_use +
      ggplot2::geom_text(ggplot2::aes(label = scales::percent(use_prct, accuracy = .1),
                                      y = use_prct + num_offset),
                         position = ggplot2::position_dodge(0.9),
                         vjust = 0)
  }

  # Save plot to folder is save_plot argument is selected
  if(save_plot == T) {
    ggplot2::ggsave(paste(basin, "_Trigger2_", select_year, ".png"), plot = plot_use, device = "png",
                    paste0(file_path, select_year),
                    scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
  }

  # Output plots
  list(plot_allo, plot_use)

} else {

  plot_allo

}

}


