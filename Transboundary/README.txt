This folder contains scripts for AB-NT transboundary reporting for the Hay River and the Slave River. 

File descriptions
/data - this folder contains water use and allocation data, where available, for recent years
TB_Functions.R - this script is used as the workflow, or example script, for functions tb_plot_triggers.R and tb_calc_triggers.R. File paths must be set according to your local directory.
TB_hydroclimate_plots.R - this script is used as a workflow, or example script, for hydroclimate plots and calculations used in technical reports. This script is dependent on various functions sourced using tb_source_functions.R
tb_calc_triggers.R - this function calculated monthly use or allocation data as a percentage of natural flows
tb_plot_triggers.R - this function is used for plotting monthly use or allocation as a percentage of natural flows
tb_source_functions.R - this script sources in functions from hydroclim and nwtclimate, which can be found here: https://github.com/M-Auclair/hydroclim/tree/main/R and here: https://github.com/M-Auclair/nwtclimate/tree/main/R

Dependencies descriptions
update_climate.R - this script runs through functions update.R, filter.R, stitch.R and nt_merge.R in order to update the ECCC merged and homogenized dataset. It's found here: https://github.com/EmmaGRiley/NWTHydroclimate/blob/main/update_climate.R
hydroclim package - functions in this package compile, calculate and plot climate and hydrometric data
nwtclimate package - functions in this package compile a filtered, stitched and merged ECCC climate dataset
SOEfigs_functions.R - this function is used for trend analysis. It's found here: https://github.com/EmmaGRiley/NWTHydroclimate/blob/main/SOEfigs_functions.R 

To use:
First download the latest tidyhydat SQL file (contains finalized hydrometric data) by running: tidyhydat::download_hydat()
To run the scripts TB_Functions.R and TB_hydroclimate_plots.R, which produce the end results for the report(s) (calculations and figures), you must first clone nwtclimate (https://github.com/M-Auclair/nwtclimate/tree/main/R) and hydroclim (https://github.com/M-Auclair/hydroclim/tree/main/R) to your local computer (including all data files!). 
Download SOE_figsfunctions.R, and update_climate.R.
Once all data and files are set up and respective directories point to the files on your local computer, you must run update_climate.R. Verify that all rds files are up-to-date in your local directory. 
You must also clone all the scripts in this folder (Transboundary) and in the nested /data folder to your local computer, and set up your file paths to point to theses files on your computer.
Now you are ready to run the following scripts, in this order:
tb_source_functions.R - note you must edit the file pathways
tb_calc_triggers.R
tb_plot_triggers.R
TB_Functions & TB_hydroclimate_plots.R - note you must edit the file pathways





