###################################################################################################

user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")), "/Documents/")
wf_path <- paste0(user, "R_Scripts/Functions/Workflows/")
# Call functions from other worksheets into active environment

# Define file paths

package_path <- paste0(user, "R_Scripts/Packages/")
nwtclimate_path <- paste0(package_path, "nwtclimate/")
hydroclim_path <- paste0(package_path, "hydroclim/")
R_path <- paste0("R/")
data_path <- paste0(nwtclimate_path, "data/")

# Define file names
original_data = "ECCC_Climate_Data"
updated_data = paste0(original_data, "_Updated")
filtered_data = paste0(original_data, "_Filtered")
stitched_data = paste0(original_data, "_Stitched")
merged_data = paste0(original_data, "_Merged")
merged_data_clean = paste0("Climate_data_clean_merged") # newly added MA

# Define file extension
extension <- ".rds"

source(paste0(nwtclimate_path, R_path, "dependencies_data.R"))
source(paste0(nwtclimate_path, R_path, "dependencies_functions.R"))
source(paste0(nwtclimate_path, R_path, "update.R"))
source(paste0(nwtclimate_path, R_path, "filter.R"))
source(paste0(nwtclimate_path, R_path, "stitch.R"))
source(paste0(nwtclimate_path, R_path, "nt_merge.R"))
source(paste0(nwtclimate_path, R_path, "replace_coordinates.R"))  #Maude added
source(paste0(nwtclimate_path, R_path, "split_sites.R"))  #Maude added
source(paste0(hydroclim_path, R_path, "hydroclim_load.R"))
source(paste0(hydroclim_path, R_path, "hydro_filter.R")) # - ER added.
source(paste0(hydroclim_path, R_path, "clim_compile_daily.R"))
source(paste0(hydroclim_path, R_path, "hydro_compile_daily.R"))
source(paste0(hydroclim_path, R_path, "hydro_calc_daily.R"))
source(paste0(hydroclim_path, R_path, "hydro_calc_dayofyear.R"))
source(paste0(hydroclim_path, R_path, "hydro_plot_dayofyear.R"))
source(paste0(hydroclim_path, R_path, "hydro_map_basins.R")) # ER added
source(paste0(hydroclim_path, R_path, "hydro_plot_dayofyear_cumulative.R")) # - ER added
# source(paste0(nwtclimate_path, R_path, "merge_ECC_ECCC.R")) ## newly added - MA
source(paste0(hydroclim_path, R_path, "clim_compile_daily.R"))
source(paste0(hydroclim_path, R_path, "clim_calc_daily.R"))
source(paste0(hydroclim_path, R_path, "clim_calc_monthly.R"))
source(paste0(hydroclim_path, R_path, "clim_calc_annual.R"))
source(paste0(hydroclim_path, R_path, "clim_plot_monthly.R"))
source(paste0(hydroclim_path, R_path, "clim_plot_annual.R"))
source(paste0(hydroclim_path, R_path, "clim_plot_dayofyear.R"))
source(paste0(hydroclim_path, R_path, "clim_plot_to_date.R"))
source(paste0(hydroclim_path, R_path, "clim_plot_to_date_working.R")) # - ER added
source(paste0(hydroclim_path, R_path, "clim_plot_timeseries.R"))
# source(paste0(wf_path, "INF_plots.R"))

# Directory where plots are saved

save_path <- paste0(user, "NT_Hydrology/Figures")


###########################################################################################################




