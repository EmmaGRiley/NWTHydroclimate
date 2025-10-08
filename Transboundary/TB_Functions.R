###################################################################################################

# Call all functions from other worksheets into active environment

# Define file paths

user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME"))) # Extract system username
tb_path <- paste0(package_path, "Transboundary/") # Filepath for transboundary scripts
file_path <- paste0(tb_path, "data/") # path for data files

###################################################################################################

# Function for generating plots for triggers and objectives

tb_plot_triggers(
  select_year = 2021, # year to run analysis
  basin = "Hay", # name of basin to run analysis. Note: code is currently only written for Hay
  station_number = "07OB001", # hydrometric station to calculate flows
  y_max = NA, # maximum value on y-axis
  num_labels = TRUE, # numbered values above each bar
  num_offset = 0.002, # offset for where numbers are positioned above each bar
  save_plot = TRUE, # saves plot(s) to selected folder
  plot_width = 20, # width (cm) of output plot(s)
  plot_height = 15, # height (cm) of output plot(s)
  dpi = 300 # resolution of output plot(s)
)

tb_calc_triggers(
  select_year = 2021,
  basin = "Hay",
  station_number = "07OB001"
)

