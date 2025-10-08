###################################################################################################
# Define '%>%' operator in current environment

`%>%` <- magrittr::`%>%`

#Define workflow path
 user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")))
 wf_path <- paste0(user, "R_Scripts/Functions/Workflows/")

#Path to additional R scripts (optional)
 add_R_path <- paste0(user, "/Documents/NT_Hydrology/R/")

# Define file paths
wf_path <- paste0(user, "/Documents/R_Scripts/Functions/Workflows/")
package_path <- paste0(user, "/Documents/R_Scripts/Packages/")
nwtclimate_path <- paste0(package_path, "nwtclimate/")
hydroclim_path <- paste0(package_path, "hydroclim/")
tb_path <- paste0(package_path, "Transboundary/")
R_path <- paste0("R/")
data_path <- paste0(nwtclimate_path, "data/")
snow_path <- paste0(package_path, "snow/",R_path)

# Define file names
original_data = "ECCC_Climate_Data"
updated_data = paste0(original_data, "_Updated")
filtered_data = paste0(original_data, "_Filtered")
stitched_data = paste0(original_data, "_Stitched")
merged_data = paste0(original_data, "_Merged")

# Define file extension
#extension <- ".rds"

# Call functions from other worksheets into active environment
source(paste0(nwtclimate_path, R_path, "dependencies_data.R"))
source(paste0(nwtclimate_path, R_path, "dependencies_functions.R"))
source(paste0(nwtclimate_path, R_path, "update.R"))
source(paste0(nwtclimate_path, R_path, "filter.R"))
source(paste0(nwtclimate_path, R_path, "stitch.R"))
source(paste0(nwtclimate_path, R_path, "nt_merge.R"))
source(paste0(hydroclim_path, R_path, "hydroclim_load.R"))
source(paste0(hydroclim_path, R_path, "hydro_filter.R"))
source(paste0(hydroclim_path, R_path, "hydro_compile_daily.R"))
source(paste0(hydroclim_path, R_path, "hydro_calc_daily.R"))
source(paste0(hydroclim_path, R_path, "hydro_calc_dayofyear.R"))
#source(paste0(hydroclim_path, R_path, "hydro_calc_dayofyear_working.R"))
source(paste0(hydroclim_path, R_path, "hydro_calc_dayofyear_cumulative.R"))
source(paste0(hydroclim_path, R_path, "hydro_plot_dayofyear.R"))
#source(paste0(hydroclim_path, R_path, "hydro_plot_dayofyear_working.R"))
source(paste0(hydroclim_path, R_path, "hydro_plot_dayofyear_cumulative.R"))
source(paste0(hydroclim_path, R_path, "clim_compile_daily.R"))
source(paste0(hydroclim_path, R_path, "clim_calc_daily.R"))
source(paste0(hydroclim_path, R_path, "clim_calc_monthly.R"))
#source(paste0(hydroclim_path, R_path, "clim_calc_monthly_working.R"))
#source(paste0(hydroclim_path, R_path, "clim_calc_annual.R"))
source(paste0(hydroclim_path, R_path, "clim_plot_monthly.R"))
#source(paste0(hydroclim_path, R_path, "clim_plot_monthly_working.R"))
#source(paste0(hydroclim_path, R_path, "clim_plot_annual.R"))
source(paste0(hydroclim_path, R_path, "clim_plot_dayofyear.R"))
#source(paste0(hydroclim_path, R_path, "clim_plot_to_date.R"))
#source(paste0(wf_path, "Climate_Boxplots.R"))
#source(paste0(wf_path, "INF_plots.R"))
source(paste0(tb_path, "tb_calc_triggers.R"))
source(paste0(tb_path, "tb_plot_triggers.R"))
source(paste0(wf_path, "SOEfigs_functions.R"))

###########################################################################################################





