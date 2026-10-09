# Snow scripts: how to run them

Operational guide for the snow survey summary, the ERA5-Land snow maps and the **April 1 spring outlook map**. The main `README.md` describes the package as a whole. This file covers how to run these scripts day to day.

| Script | What it makes | When |
|---|---|---|
| `R/SWE_summary.R` | Table of NWT snow survey SWE vs average, plus a regional boxplot | After spring surveys are loaded into the snow database |
| `R/ERA5_snow_mapping.R` | Map of ERA5-Land SWE as % of average (no survey points) | Spring outlook / on request |
| `R/ERA5_snow_remaining_map.R` | Map of SWE remaining on a chosen date (% of April 1 max, mm SWE, or change between two dates) | Late spring / melt-out questions |
| `R/Snow_Mapping_April1_2027.R` | **April 1 spring outlook map**: NT, YT, AB and BC survey sites over ERA5-Land % of average | Early–mid April |
| `R/ERA5_spring_snow_analysis.R` | Downloads ERA5-Land data and builds the files the maps above read | Before any ERA5 map, each year |

> **Before deciding a script is broken**, check [Troubleshooting](#troubleshooting). The 2027 mapping script checks its inputs first and stops with a message naming the missing file. Read that message, because it usually tells you exactly what to fix.

---

## 1. One-time setup

### Folder layout
Replace `<you>` with your Windows username (the folder under `C:/Users/`). All scripts build paths from this, so **the folders must be exactly here**:

```
C:/Users/<you>/Documents/
├── R_Scripts/Packages/snow/          ← this package (from GitHub: EmmaGRiley/NWTHydroclimate → packages/snow)
│   ├── R/                            ← scripts
│   └── data/
│       ├── site_coordinates_highres.csv   (on GitHub)
│       ├── CanSWE_v7.csv                  (on GitHub)
│       ├── YT_Data/  AB_Data/  BC_Data/   ← NOT on GitHub: copy from the H: drive (see below)
│       └── SWEmax_dataset/                ← NOT on GitHub: copy from the H: drive (see below)
├── Shapefiles/                       ← copy from H: drive (Active – Emma Gregory)
│   ├── MackenzieRiverBasin_FDA.shp
│   ├── NWT_Boundary/NWT_ENR_BND_FND.shp
│   ├── 07/07OB001/…  07/07SB002/…  07/07SA002/…  07/07NC003/…  07/07QA001/…
│   └── 10/10ED002/…  10/10EB001/…  10/10JC003/…  10/10FB004/…  10/10MC022/…
├── Modelling/phantomjs/phantomjs/bin/phantomjs.exe   ← for saving maps as .png
├── Github/KrigR/                     ← only for ERA5 downloads
└── NT_Hydrology/Figures/             ← maps and plots are saved here
```

**Data not on GitHub.** Copy these from the H: drive (GNWT network only):

`H:\Waters\001 - Water Resources - Staff Working Files\001a - Water Resources - Staff Working Files\Active - Emma Gregory`

Keep the same folder names, so each one goes straight into your `snow/data/` folder:

| Copy into `snow/data/` | Size | Needed for |
|---|---|---|
| `YT_Data/` (whole folder) | <1 MB | April 1 map, Yukon historical means (`Snow_Samples_YT.csv`) |
| `AB_Data/` (whole folder, **including** `AB_Data/Shapefiles/`) | 4 MB | April 1 map (`snow_stations.shp` locates the AB sites) |
| `BC_Data/` (whole folder) | 14 MB | April 1 map (`BC_Locations.csv` is needed even though the survey data download automatically) |
| `SWEmax_dataset/April_max_combined_max_snow.csv` | 55 MB | April 1 map, ERA5 map |
| `SWEmax_dataset/May_max_combined_max_snow.csv` | 55 MB | ERA5 map (`file <- "May_max"`) |
| `SWEmax_dataset/May_14max_combined_max_snow.csv` | 53 MB | ERA5 map (`file <- "May_14max"`) |
| `SWEmax_dataset/snow_depth_April_max_<latest year>.nc` | 24 MB | `ERA5_snow_remaining_map.R` |

Optional (research scripts only): `SWEmax_dataset/ERA5_updated1960_2025NWT.csv` and `ERA5_updatedfeb_mayNWT.csv` (for `ERA5_manual_SWE_bias.R` / `ERA5_manual_SWE_variability.R`), and the earlier `snow_depth_April_max_<year>.nc` files (only needed to rebuild the April max file from scratch). The rest of `SWEmax_dataset/` (44 GB in full) isn't needed. The shapefiles for `Documents/Shapefiles/` are in the same H: drive folder.

> If your Documents folder is redirected (e.g. to OneDrive), none of these paths will work. In R, run `path.expand("~")`. It should print `C:/Users/<you>/Documents`. If it doesn't, talk to IT before going further.

### Database connection (NWT snow survey data)
1. Set up the ODBC data source **ECC_WMSD_SNOW** (TNS service `PDB3PRD1`, user `SNOW_VIEWER`). See *Instructions – connecting to database.docx* and *R coding session – instructions.docx* in `NT_Hydrology/Abiotic Monitoring/ClimateDB/`.
2. Put the username and password in your **`.Renviron`** file. **Don't** type them into the scripts. In R:
   ```r
   usethis::edit_r_environ()
   ```
   Add these lines, save, then **restart R** (Session → Restart R):
   ```
   ORACLE_SNOW_USER=SNOW_VIEWER
   ORACLE_SNOW_PWD=<password from ISSS>
   ```
   > *R coding session – instructions.docx* says to type the password into `R_oracle_connect.R`. That's out of date: the script now reads it from `.Renviron`.
3. Test it: `source(".../snow/R/R_oracle_connect.R")` should create `md_3`, `sites`, `depths` and `snow` without errors.

### ERA5-Land downloads (only needed for ERA5 maps)
1. Create a free account at https://cds.climate.copernicus.eu and accept the **ERA5-Land** licence (on the dataset page, under *Download → Terms of use*).
2. Add your credentials to `.Renviron` the same way:
   ```
   CDS_USER=<your CDS user ID or email>
   CDS_KEY=<your API key from your CDS profile page>
   ```
3. Install packages. These are commented out at the top of `ERA5_snow_remaining_map.R`, so run them **once**:
   ```r
   install.packages(c("ecmwfr", "ncdf4", "automap", "foreach", "doSNOW", "pbapply", "cowplot", "progress", "viridis", "usethis"))
   # terra must be version 1.8-60 or older - newer versions break the workflow
   install.packages("terra", repos = "https://packagemanager.posit.co/cran/2024-12-01", type = "win.binary")
   # KrigR: download https://github.com/ErikKusch/KrigR as a ZIP, unzip to Documents/Github/KrigR, then:
   install.packages("C:/Users/<you>/Documents/Github/KrigR", repos = NULL, type = "source")
   ```

### Other packages
```r
install.packages(c("dplyr", "sf", "sp", "leaflet", "magrittr", "readxl", "lubridate", "tidyr", "stringr",
                   "foreign", "htmlwidgets", "webshot", "ggplot2", "odbc", "DBI"))
webshot::install_phantomjs()   # or unzip phantomjs to Documents/Modelling/phantomjs/
```

---

## 2. `SWE_summary.R`: snow survey summary table

**Needs:** database connection (above). NWT surveys for the year must already be loaded into the snow database.

**Run:** open it and click **Source**. `summary_year` at the top defaults to the current calendar year. Type a year (e.g. `summary_year <- 2027`) to summarize a different year.

**Outputs:**
- `Documents/SummaryTable<year>.csv`: one row per site
- `Documents/NT_Hydrology/Figures/Regional_boxplot.png`: mean SWE by region, current year in blue

**What the table columns mean:**

| Column | Meaning |
|---|---|
| `yrs` | Number of years with data |
| `yrs2` | Years with data in the 2001–2020 normal period |
| `meanSWE01_20` | Mean SWE (mm), 2001–2020 |
| `meanSWE_cur`, `meandepth_cur` | This year's SWE (mm) and depth (cm) |
| `meanSWE_allyears` | Mean SWE (mm) of **all** years before this one |
| `pernorm` | This year as % of the 2001–2020 mean |
| `pernorm_allyrs` | This year as % of the all-years mean. **This is what the maps use.** |
| `rank_current` | This year's rank (1 = most snow) |

**Data rules applied:** upland sites only, active (`A`) sites only, surveys with flags `Y`, `HS`, `Sk`, `P`, `Sk_2` excluded, and densities below 0.1 or above (mean + 3 SD) excluded. The `hdensity` argument isn't used, because the upper limit is calculated from the data.

The 2027 April map sources this script to get the `SWEsummary()` function. It switches off the table and boxplot when it does, so running the map won't overwrite your summary files.

---

## 3. Preparing ERA5-Land data each year: `ERA5_spring_snow_analysis.R`

The ERA5 maps don't download their own baseline. They read files this script makes. **Do this first each spring.** ERA5-Land runs about 5 days behind, so April 1 data are available around April 6.

1. In RStudio, set the working directory to the ERA5 folder. The script saves downloads into the working directory:
   ```r
   setwd("C:/Users/<you>/Documents/R_Scripts/Packages/snow/data/SWEmax_dataset")
   ```
2. Open `ERA5_spring_snow_analysis.R` and set:
   - `file <- "April_max_"` (for the April 1 map. Use `"May_max_"` for May 1, and so on)
   - `date_start` / `date_stop` inside `get_spring_snow()`: the window the maximum is taken over. **[EMMA: confirm the April 1 window, e.g. `-02-01 00:00` to `-04-01 00:00`]**
   - `for(year in 2027)` in the download loop
   - `for (year in 1995:2027)` in the combine loop
3. Skip the `install.packages()` lines if already installed. Run down to and including the `write.csv(combined_max_snow, …)` line. Stop there, because the rest of the script is separate analysis.
4. Check that `SWEmax_dataset/April_max_combined_max_snow.csv` now has a `Snow_2027` column.

The download takes from minutes to hours, depending on the CDS queue.

---

## 4. `ERA5_snow_mapping.R`: ERA5-Land % of average map

**Needs:** step 3 done for the year. Database connection (it sources `R_oracle_connect.R`).

**Settings (top of script):**

| Setting | Meaning |
|---|---|
| `file` | Which ERA5 file: `"April_max_"`, `"May_max_"`, `"May_14max_"`, or `"max_spring"` (Feb–May max). It reads `SWEmax_dataset/<file>_combined_max_snow.csv` (e.g. `April_max_combined_max_snow.csv`; a trailing `_` in `file` is ignored) |
| `current_year` | Year to map. % of average = this year ÷ mean of every other year in the file |
| `clip_ERA5` | `T` = only show the Mackenzie River Basin |
| `mack_basin_outline` | Draw the MRB outline |
| `manual_sites` | `T` = also plot survey sites (NT from database + CanSWE). Usually `F` |
| `add_legend` | Add legend to map |
| `save` | Save `.html` and `.png` |
| `basemap` | Background map. `"carto_key"` (default) needs **your own** CARTO API key; no key → e.g. `"Esri.WorldGrayCanvas"`. See the `basemap` bullet in §6d |

**Output:** `Documents/NT_Hydrology/Figures/map_MRB_mo<file><year>.html/.png`

---

## 5. `ERA5_snow_remaining_map.R`: snow remaining on a date

Downloads ERA5-Land SWE for one day (`target_date`) and maps it.

**Needs:** CDS credentials. The April max `.nc` for the year (from step 3) at `SWEmax_dataset/snow_depth_April_max_<year>.nc`.

**Settings:**

| Setting | Meaning |
|---|---|
| `current_year` | Year of the April 1 baseline |
| `target_date` | Date to map (`"YYYY-MM-DD"`). Must be at least ~5 days ago |
| `map_mode` | `"percent"` = % of April 1 max remaining. `"swe"` = SWE in mm on `target_date`. `"diff"` = change between `baseline_file` and `target_date` |
| `baseline_file` | Normally the April max `.nc`. For `"diff"`, point it at another single-day file (e.g. `snow_depth_target_20270601.nc`) and set `baseline_date` to that date |
| `clip_ERA5`, `mack_basin_outline`, `add_legend`, `save`, `basemap` | As above |
| `add_basin` | Load Liard and Nahanni basins and print their basin-average values |

**Outputs:**
- `snow_depth_target_<date>.nc` in `SWEmax_dataset/` (re-used if you run it again)
- `NT_Hydrology/Figures/map_snow_<mode>_<date>.html/.png`
- In the console: value summary, and Liard and Nahanni basin means (simple and area-weighted)

---

## 6. April 1 spring outlook map: `Snow_Mapping_April1_2027.R`

Combines April 1 snow survey results from four jurisdictions, each as **% of that site's own historical average**, over ERA5-Land % of average.

| Jurisdiction | Where it comes from | What you do |
|---|---|---|
| **NT** | Snow database, via `SWE_summary.R` | Nothing, once surveys are loaded |
| **BC** | BC government website | **Nothing.** Downloaded automatically (`download_BC_data = T`) |
| **YT** | Yukon April 1 bulletin | Build `YT_Data_2027.csv` (below) |
| **AB** | Alberta WISKI snow course report | Build `AB_Data_2027.csv` (below) |
| ERA5 | Step 3 above | Run `ERA5_spring_snow_analysis.R` |

### 6a. Yukon: `data/YT_Data/YT_Data_2027.csv`
YT data come as a spreadsheet with the April 1 bulletin results (2026 example: `YT_Data/YT_data_2026.xlsx`). **[EMMA: confirm who sends it / where to download]**

1. **Update the historical record.** Replace `YT_Data/Snow_Samples_YT.csv` with the latest version (it must include last year's surveys). **[EMMA: confirm source]**
2. **Calculate historical means.** Open `R/Calc_yukon_historical_swe_means.R`, set `select_year <- 2027`, and run it. It prints `meanSWE` (mean of every April survey before 2027) for each snow course.
3. **Make the CSV.** Copy `YT_Data_2026.csv` to `YT_Data_2027.csv`. Open it in Excel and, for each of the 9 sites, update these columns. Leave the others (Jurisdiction, Region, Site, Lat, Long, SNOW_COURSE_ID, ELEVATION, Length_Trimmed = NA) as they are.

   | Column | From |
   |---|---|
   | `Length_Total` | YT spreadsheet: *Years of record* |
   | `Mean_SWE_mm` | Step 2: `meanSWE` |
   | `Current_Depth` | YT spreadsheet: *Snow Depth (cm)* |
   | `Current_SWE` | YT spreadsheet: *Water content (SWE) (mm)* |
   | `Percent_Normal` | `= Current_SWE / Mean_SWE_mm * 100` |
   | `Date` | Survey target date, e.g. `4/1/2027` |
   | `SWE_LAST` | YT spreadsheet: *Previous Year SWE (mm)* |
   | `MedianSWE` | YT spreadsheet: *Median Historical SWE (mm)* |

   Sites: Watson Lake Airport (10AA-SC01), Tintina Airstrip (10AA-SC02), Pine Lake Airstrip (10AA-SC03), Ford Lake (10AA-SC04), Frances River (10AB-SC01), Hyland River B (10AD-SC01 composite), Blackstone River (10MA-SC01), Ogilvie River (10MA-SC02), Bonnet Plume Lake (10MB-SC01).

   If a site wasn't surveyed, delete its row, or leave `Current_SWE` blank (blank rows are dropped).

   > **Hyland River:** the 2026 mean (199 mm) doesn't match the plain mean from `Snow_Samples_YT.csv` (183.5 mm), because of the A→B site change (`YT_Data/2025_Hyland_River_B_Corrections.xlsx`). **[EMMA: explain how the Hyland mean is calculated]**

4. Save as CSV, keeping the name `YT_Data_2027.csv`.

### 6b. Alberta: `data/AB_Data/AB_Data_2027.csv`
AB data come as a WISKI report, `rptSnowCourseMonthly…xlsx` (Water Tree: Arctic Ocean; Report Type: 1st of Month; Plains/Mountains; Report Date April 1). The 2026 example is `AB_Data/rptSnowCourseMonthly_for NWT_April_2.xlsx`. **[EMMA: confirm who sends it / how to request it]**

1. Open the report in Excel and **delete the report-header rows above the table**, so row 1 is the column names.
2. Check the columns are in this order. The script reads them **by position**:
   `Basin/SubBasin, Station Name, Station ID, Snow Parameter, Survey/Reading Date, Survey/Reading Value, (last year's value), Historical Minimum, Hist. Min Year, Historical Maximum, Hist. Max Year, Historical Average, Years of Data, % of Avg`
   Compare with `AB_Data/AB_Data_2026.xlsx` if unsure.
3. Save As **CSV** as `AB_Data/AB_Data_2027.csv`.
4. Each station has two rows: `SD-SS` (depth) and `SW-SS` (SWE). Both are needed.

When you run the map, the console lists any **AB sites with no location match**. Those sites have new or changed names and won't be mapped. If one matters, add its coordinates to `AB_Data/Shapefiles/snow_stations.dbf` or to the "fix Alberta coordinates" block in the script.

*(For March/May maps AB sometimes only sends PDFs. `snow_local/R/AB_Data_PDF_convert.R` converts those, but April normally comes as Excel.)*

### 6c. BC: automatic
With `download_BC_data = T`, the script downloads `allmss_archive.csv` and `allmss_current.csv` from https://www.env.gov.bc.ca/wsd/data_searches/snow/asws/data/, combines them and saves `BC_Data/BC_Data_2027.csv`. % of average = this year's April 1 SWE ÷ mean of all earlier April 1 surveys at that course.

If you get a warning that BC has no April 1 data, BC hasn't published yet (usually around April 8–10). Wait and re-run. If the BC website is down, set `download_BC_data = F` and use a saved `BC_Data_2027.csv`.

### 6d. Run the map
1. Open `R/Snow_Mapping_April1_2027.R`.
2. Check the **SETTINGS** block:
   - `selected_layer`: `"all"` for the whole Mackenzie River Basin, or one basin (e.g. `"Peel Basin"`, `"Liard Basin"`). Valid names are listed in the script.
   - `site_clip_exceptions`: sites just outside the selected basin that should still be shown (2025 Liard map used `c("Fort Simpson", "Checkpoint")`)
   - `mrb_clip_exceptions`: sites outside the MRB to keep (Colville Lake, Pine Lake Airstrip)
   - `exclude_sites`: BC sites that also appear in the AB data (avoids duplicates)
   - `basemap`: the background map. The default `"carto_key"` is the CARTO light background with no labels, and it needs **your own** CARTO API key: create a CARTO account at https://carto.com, make an API access token (Developers → Credentials), and add `CARTO_API_KEY=<your key>` to your `.Renviron`. Never use someone else's key. No key? Use a background that doesn't need one, e.g. `"Esri.WorldGrayCanvas"` (run `names(leaflet::providers)` for the full list; the `CartoDB.*` ones need a key too). Step 5 at the top of the script has the details.
3. Click **Source**. The script first checks every input file and stops with a list of anything missing.
4. Check the console:
   - The table of sites per jurisdiction (`AB`, `BC`, `NT`, `YT`). If one is missing or has very few sites, check that jurisdiction's file.
   - When mapping a single basin: basin-averaged % of normal for survey sites and for ERA5.
5. Outputs in `Documents/NT_Hydrology/Figures/`:
   - `map_<MRB or basin>_April1_2027.html/.png`
   - for single basins, an inset `map_inset_<basin>_2027.html/.png`
6. Finish the map in **CorelDraw** (legend, labels, layout). Step-by-step instructions are on DIIMS:
   - End of season maps: https://diims.gov.nt.ca/gnwt/llisapi.dll/link/245398288
   - Mid-season maps: https://diims.gov.nt.ca/gnwt/llisapi.dll?func=ll&objaction=overview&objid=245772669

**To make a 2028 version:** copy this file to `Snow_Mapping_April1_2028.R` and change `year = 2028`. Everything else follows from `year`.

**Things that may need a look** (unchanged from 2026, so 2027 maps are comparable):
- **Great Slave Lake basin clip:** only the Taltson basin is removed from GSL. Each `st_difference` line starts again from `GSL`, so the Slave and Hay lines have no effect.
- **Fairview (AB):** the coordinate fix sets its *longitude* to 56.03, which is a latitude. Fairview probably isn't being mapped.

---

## Troubleshooting

| Message / symptom | Cause | Fix |
|---|---|---|
| `These input files could not be found: …` | File missing or wrongly named (year in the name, `_Data_` spelling, `.csv` vs `.xlsx`) | Fix the file named in the message |
| `…has no Snow_2027 column` | ERA5 data not prepared for this year | Section 3, or set `ERA5_Land = F` to map without ERA5 |
| `CDS_USER / CDS_KEY not found` | `.Renviron` not set up, or R not restarted afterwards | Setup → ERA5. Then Session → Restart R |
| `nanodbc… Data source name not found` | ODBC source `ECC_WMSD_SNOW` not set up, or set up under a different name | Setup → Database connection |
| `ORA-01017: invalid username/password` | Wrong or missing `ORACLE_SNOW_USER` / `ORACLE_SNOW_PWD`, or R not restarted after editing `.Renviron` | Check `.Renviron`, restart R |
| `ORA-28000: account is locked` | Too many wrong passwords | ISSS ticket to unlock SNOW_VIEWER |
| Warning: no NT snow survey data for 2027 | Surveys not yet loaded to the snow database | Wait for the load. **[EMMA: who loads surveys?]** |
| BC sites missing | BC hasn't published April 1 data yet | Wait and re-run |
| AB sites listed as "no location match" | AB renamed or added a station | See 6b |
| `.html` saved but no `.png` | phantomjs not installed or in the wrong folder | `webshot::install_phantomjs()` |
| `KrigR` / `terra` errors in ERA5 scripts | terra newer than 1.8-60 | Reinstall terra (Setup → ERA5, step 3) |
| Every path fails | Documents folder is redirected (OneDrive), or username differs from folder name | `path.expand("~")`. See Setup |
| Map looks wrong for one basin | Check `selected_layer` spelling and the `*_clip_exceptions` settings | Section 6d |

---

*Last updated October 2026 (Emma Gregory, before leave).*
