###################################################################################################
# Loads all functions needed for transboundary reporting. Run this first (Ctrl+Shift+S, or the Source button).
# See README.md in this folder for how to set up the folders this script expects.

# Define '%>%' operator in current environment
`%>%` <- magrittr::`%>%`

# Define file paths
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")))

wf_path <- paste0(user, "/Documents/R_Scripts/Functions/Workflows/") # SOEfigs_functions.R and update_climate.R
package_path <- paste0(user, "/Documents/R_Scripts/Packages/")
nwtclimate_path <- paste0(package_path, "nwtclimate/")
hydroclim_path <- paste0(package_path, "hydroclim/")
tb_path <- paste0(package_path, "Transboundary/")
R_path <- paste0("R/")
data_path <- paste0(nwtclimate_path, "data/")

# Define file names
original_data = "ECCC_Climate_Data"
updated_data = paste0(original_data, "_Updated")
filtered_data = paste0(original_data, "_Filtered")
stitched_data = paste0(original_data, "_Stitched")
merged_data = paste0(original_data, "_Merged")

# Scripts to load
tb_scripts <- c(
  paste0(nwtclimate_path, R_path, "dependencies_functions.R"),
  paste0(hydroclim_path, R_path, c("hydroclim_load.R",
                                   "hydro_filter.R",
                                   "hydro_compile_daily.R",
                                   "hydro_calc_daily.R",
                                   "hydro_calc_dayofyear.R",
                                   "hydro_calc_dayofyear_cumulative.R",
                                   "hydro_plot_dayofyear.R",
                                   "hydro_plot_dayofyear_cumulative.R",
                                   "clim_compile_daily.R",
                                   "clim_calc_daily.R",
                                   "clim_calc_monthly.R",
                                   "clim_plot_monthly.R",
                                   "clim_plot_dayofyear.R")),
  paste0(tb_path, c("tb_calc_triggers.R", "tb_plot_triggers.R")),
  paste0(wf_path, "SOEfigs_functions.R")
)

# Stop with a clear message if anything is missing, rather than failing part way through
missing_scripts <- tb_scripts[!file.exists(tb_scripts)]
if (length(missing_scripts) > 0) {
  stop("These scripts could not be found:\n", paste0("  - ", missing_scripts, collapse = "\n"),
       "\nnwtclimate and hydroclim must be the 'gnwt-operational' branch versions - see README.md (Setup).", call. = FALSE)
}

# Call functions from other worksheets into active environment
for (f in tb_scripts) source(f)

###########################################################################################################
