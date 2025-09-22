###################################################################################################

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

##################################################################################################

# Identify date of report

report_date <- Sys.Date() - lubridate::day(Sys.Date())

# Create a list of hydrometric gauges to include in the report

Hydro_sites <- c(
  
  "06KB003", # DUBAWNT RIVER ABOVE DUBAWNT LAKE
  "07NB001", # SLAVE RIVER AT FITZGERALD
  "07OB001", # HAY RIVER NEAR HAY RIVER
  "07OB002", # GREAT SLAVE LAKE AT HAY RIVER
  "07QD007", # TALTSON RIVER BELOW HYDRO DAM
  "07RD001", # LOCKHART RIVER AT OUTLET OF ARTILLERY LAKE
  "07SA002", # SNARE RIVER BELOW GHOST RIVER
  "07SB001", # GREAT SLAVE LAKE AT YELLOWKNIFE BAY
  "07SB010", # CAMERON RIVER BELOW REID LAKE
  "07SB014", #PROSPEROUS LAKE NEAR MCKEEKAN BAY
  "07SB017", # PRELUDE LAKE NEAR YELLOWKNIFE
  "07TA001", # LA MATRE RIVER BELOW OUTLET OF LAC LA MARTRE
  "10EB001", # SOUTH NAHANNI RIVER ABOVE VIRGINIA FALLS
  "10ED002", # LIARD RIVER NEAR THE MOUTH
  "10GC001", # MACKENZIE RIVER AT FORT SIMPSON
  "10JC003", # GREAT BEAR RIVER AT OUTLET OF GREAT BEAR LAKE
  "10JE002", # GREAT BEAR LAKE AT HORNBY BAY
  "10KA001", # MACKENZIE RIVER AT NORMAN WELLS
  "10LA002", # ARCTIC RED RIVER NEAR THE MOUTH
  "10LC002", # MACKENZIE RIVER (EAST CHANNEL) AT INUVIK
  "10LC014", # MACKENZIE RIVER AT ARCTIC RED RIVER
  "10MC002", # PEEL RIVER ABOVE FORT MCPHERSON
  "10MC003", # MACKENZIE RIVER (PEEL CHANNEL) ABOVE AKLAVIK
  "10PA001"  # COPPERMINE RIVER BELOW DESTEFFANY LAKE
  
)

# Identify which hydrometric gauges to display historic period after the 
# filling of the Bennett Dam

Post_Bennett <- c(
  "07NB001", # SLAVE RIVER AT FITZGERALD
  "07OB002", # GREAT SLAVE LAKE AT HAY RIVER
  "07SB001" # GREAT SLAVE LAKE AT YELLOWKNIFE BAY
  
)

# Identify communities to include climate figures

Communities <- c(
  "Fort Smith",
  "Hay River",
  "Yellowknife",
  "Fort Simpson",
  "Norman Wells",
  "Inuvik"
)

# Create a folder to save all figures
path <- paste0(user, "NT_Hydrology/Figures/Monthly_Bulletins/")
year <- paste0(lubridate::year(Sys.Date()), "/")
dir.create(paste0(path, Sys.Date()))
save_path <- paste0(path, Sys.Date())

# Run the Hydro_plot functions to generate Hydrographs

for(i in Hydro_sites) {
  tryCatch({
    
    hydro_plot_dayofyear(
      station_number = i,
      parameter = "Level",
      select_year = c(2024, 2025),
      line_colours = c("dodgerblue",
                       "blue4"),
      save = T,
      file_name = paste0(i, "_Level")
    )
    
    if(i %in% Post_Bennett) {
      hydro_plot_dayofyear(
        station_number = i,
        parameter = "Level",
        select_year = c(2024, 2025),
        line_colours = c("dodgerblue",
                         "blue4"),
        after_bennett = T,
        save = T,
        file_name = paste0(i, "_Level")
      )
    }
    
    hydro_plot_dayofyear(
      station_number = i,
      parameter = "flow",
      select_year = c(2024, 2025),
      line_colours = c("dodgerblue",
                       "blue4"),
      save = T,
      file_name = paste0(i, "_Flow")
    )
    
    if(i %in% Post_Bennett) {
      hydro_plot_dayofyear(
        station_number = i,
        parameter = "Flow",
        select_year = c(2024, 2025),
        line_colours = c("dodgerblue",
                         "blue4"),
        after_bennett = T,
        save = T,
        file_name = paste0(i, "_Flow")
      )
    }
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Call in updated climate dataset

Master <- readRDS(paste0(data_path, merged_data, extension))

# Run boxplot functions to create precipitation and temperature figures

Communities <- c(
  "Fort Smith", 
  "Hay River", 
  "Yellowknife", 
  "Fort Simpson", 
  "Norman Wells", 
  "Inuvik",
  "Mackenzie", "Fort St John", "Peace River", "Athabasca", "High Level",  "Fort McMurray", "Fort Chipewyan"
)

# for(i in Communities) {
#   clim_plot_monthly(
#     site = i,
#     parameter = "rain", 
#     select_year = 2025,
#     save = T,
#     file_name = paste0(i, "_rain")
#   )
# }
# 
# for(i in Communities) {
#   clim_plot_monthly(
#     site = i,
#     parameter = "mean_temp", 
#     select_year = 2025,
#     save = T,
#     file_name = paste0(i, "_meantemp")
#   )
# }

for(i in Communities) {
  clim_plot_monthly(
    site = i,
    start_year = 1950,
    select_year = 2025,
    water_year_start = 4,
    water_year_end = 9,
    max_missing_days = 3, #default is 3
    legend_position = c(0.5, 0.99),
    parameter = "Precipitation",
    save = T,
    file_name = paste0(i, "_total_precip"),
    water_year = FALSE
  )
}


for(i in Communities) {
  clim_plot_monthly(
    site = i,
    parameter = "mean_temp",
    start_year = 1950,
    select_year = 2025,
    water_year_start = 4,
    max_missing_days = 3,
    water_year_end = 9,
    legend_position = c(0.1, 0.99),
    save = T,
    file_name = paste0(i, "_temp_wy"),
    water_year = FALSE,
    y_max = 15
  )
}

clim_plot_to_date_working(
  #site = c("Hay River", "Fort St John", "Fort Nelson", "Peace River", "High Level", "Fort Simpson"),
  #site = c("Fort Simpson", "Norman Wells", "Fort Good Hope", "Fort McPherson", "Inuvik"),
  site = c("Fort Smith",  "Hay River", "Fort Simpson", "Yellowknife", "Norman Wells", "Inuvik"),
  #site = c("Fort Nelson", "High Level", "Hay River"),
  #site = c("Mackenzie", "Fort St John", "Peace River", "High Level",  "Fort McMurray", "Fort Chipewyan", "Fort Smith"),  
  #site = c("Fort Liard", "Fort Nelson", "Fort Simpson", "Watson Lake"),
  #site = c("Fort Good Hope", "Norman Wells", "Inuvik"),
  parameter = "precip",
  select_year = 2025, 
  water_year_start = 4,
  end_date = "2025-09-22", 
  max_missing_days = 12, #default is 10
  y_min = 0,
  y_max = 500,
  legend_position = c(0.5, 0.99),
  save = T,
  file_name = paste0("precip 2025 nwt", report_date), 
  extension = "png",
  water_year = F,
  plot_width = 20, #usually 16
  plot_height = 10, #usually 10
)


#################################


