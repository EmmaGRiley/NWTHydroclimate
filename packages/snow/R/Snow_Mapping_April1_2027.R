# Snow map for spring outlook - April 1 version (2027)
# Based on Snow_Mapping_April1_2026.R (local version only). Too complex to run as a function - easier to run as a script.
#
# READ FIRST: README_snow_scripts.md (section "April 1 spring outlook map") in the snow package folder.
#
# What this script does:
#   Combines April 1 snow survey results from NT (snow database), YT, AB and BC, converts them to
#   percent of average, and maps them over ERA5-Land percent-of-average SWE for the Mackenzie River Basin
#   (or one sub-basin).
#
# BEFORE YOU RUN - the "preflight check" section below will stop with a plain-English message if any of
# these are missing. Work through them in order:
#
#   1. DATABASE CONNECTION (NT snow survey data)
#      a. The ODBC connection ECC_WMSD_SNOW must be set up on your computer (see "Instructions - connecting to
#         database.docx"). To check, run this in the console - ECC_WMSD_SNOW should be in the list:
#           odbc::odbcListDataSources()$name
#      b. Your snow database username and password must be in your .Renviron file. Get the password from ISSS.
#         - In the console, run:   usethis::edit_r_environ()
#           (if usethis isn't installed, run install.packages("usethis") first)
#         - A file called .Renviron opens in RStudio. Add these two lines exactly as shown, with your password
#           after the = sign. No quotes, no spaces around the =, and nothing else on the line:
#             ORACLE_SNOW_USER=SNOW_VIEWER
#             ORACLE_SNOW_PWD=yourpasswordhere
#         - Make sure there is an empty line at the very end of the file (press Enter after the last line).
#         - Save the file (Ctrl+S), then restart R: Session > Restart R.
#         - To check it worked, run the lines below. Both should say TRUE. (Don't print the password itself.)
#             nzchar(Sys.getenv("ORACLE_SNOW_USER"))
#             nzchar(Sys.getenv("ORACLE_SNOW_PWD"))
#         - Never type passwords into this script, and never email them or put them on GitHub.
#
#   2. THIS YEAR'S YUKON AND ALBERTA DATA, saved as snow/data/YT_Data/YT_Data_<year>.csv and
#      snow/data/AB_Data/AB_Data_<year>.csv (how to build each file: README_snow_scripts.md, section 6).
#      It is often easiest to wait until each agency publishes its April 1 snow data on its website / data
#      portal. If you need the data sooner, or something looks wrong, contact:
#        - Alberta: RFC.Ice@gov.ab.ca and/or nadia.kovachis@gov.ab.ca
#        - Yukon:   Emilie-Jeanne.Bercier@yukon.ca, or the Yukon snow survey team generally
#        - BC data downloads automatically (download_BC_data = T below) from
#          https://www.env.gov.bc.ca/wsd/data_searches/snow/asws/data/. If the download fails or this year's
#          surveys aren't there yet, contact Snow.Survey@gov.bc.ca and/or Jonathan.Boyd@gov.bc.ca and/or
#          Jon.Goetz@gov.bc.ca
#
#   3. ERA5-LAND APRIL MAX FILE updated to include this year: run ERA5_spring_snow_analysis.R first, with
#      file <- "April_max_". In that script, set date_stop to the LATEST DATE AVAILABLE this year at the time
#      you run it - not the default end of May, which won't exist yet. ERA5-Land runs about 5 days behind,
#      so e.g. for an April 1 map run on April 8, the last full day available is around April 3.
#      Then check that SWEmax_dataset/April_max_combined_max_snow.csv has a
#      Snow_<year> column.
#
#   4. SHAPEFILES in Documents/Shapefiles/ (copied from the H: drive, or separated yourself from
#      https://open.canada.ca/data/en/dataset/a4b190fe-e090-4e6d-881e-b87956c07977/resource/c993b360-f86e-4c1d-92c2-e0b4c9268671)
#
#   5. BACKGROUND MAP (basemap setting below). The default, basemap = "carto_key", is the CARTO light background
#      with no labels - what I use. It needs a CARTO API key. Use YOUR OWN key, never someone else's:
#        - Create a CARTO account at https://carto.com with your work email.
#        - In CARTO, go to Developers > Credentials and create an API access token. (CARTO renames its menus
#          from time to time - if these steps don't match, search CARTO's help for "API access token".)
#        - Add it to your .Renviron the same way as step 1b, on its own line:
#            CARTO_API_KEY=yourkeyhere
#          then save and restart R. The carto_tiles() function in this script reads it from there.
#      No key? Use a background that doesn't need one, e.g. basemap = "Esri.WorldGrayCanvas"
#      (to see every option, run names(leaflet::providers) in the console - the CartoDB.* ones need a key too).
#
# AFTER YOU RUN - finishing the map in CorelDraw:
#   The .png in Documents/NT_Hydrology/Figures/ is the base map. Legends, labels and layout are finished in
#   CorelDraw as map templates on DIIMS:
#     - End of season maps: https://diims.gov.nt.ca/gnwt/llisapi.dll/link/245398288
#     - Mid-season maps:    https://diims.gov.nt.ca/gnwt/llisapi.dll?func=ll&objaction=overview&objid=245772669

# required packages
library(dplyr)
library(sf)
library(leaflet)
library(magrittr)
library(readxl)

# ---- SETTINGS: change these ------------------------------------------------------------------------

year = 2027 # year you are mapping
exclude_sites = c("FORT NELSON AIRPORT - BC", "FORT ST. JOHN - BC", "PINE PASS - BC") # exclude BC sites if specified in both AB and BC datasets
save = T
add_legend = F
mack_basin_outline = T # set to F if mapping an individual basin. See selected_layer argument below
ERA5_Land = T
clip_ERA5 = T
clip_stations = T
selected_layer = "all" # basin you are mapping, options are: "all", "Mackenzie Basin",
#"Hay Basin",
#"Liard Basin",
#"Great Bear Basin",
#"Great Slave Lake Basin",
#"Peel Basin",
#"Taltson Basin",
#"Slave River Basin",
#"Snare Basin",
#"Yellowknife Basin",
#"NWT Border"
site_clip_exceptions = NA # sites that are representative of a given basin but are just outside basin borders, e.g. c("Fort Simpson", "Checkpoint") for Liard
mrb_clip_exceptions = c("Colville Lake", "Pine Lake Airstrip") # sites that you want to map but are outside the MRB
download_BC_data = T # T = download BC snow survey data straight from the BC website and save it as BC_Data_<year>.csv. F = use the file already saved
basemap = "carto_key" # background map. "carto_key" = CARTO light, no labels, using your own CARTO_API_KEY (see BEFORE YOU RUN step 5).
#No key? use another leaflet provider, e.g. "Esri.WorldGrayCanvas" (full list: names(leaflet::providers))

# ---- File locations (should not need changing if folders were set up as in the README) ----------

# Define user
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")), "/")

folder <- paste0(user, "Documents/R_Scripts/Packages/snow/data/")
shapefile_path <- paste0(user, "Documents/Shapefiles/")
output_dir <- paste0(user, "Documents/NT_Hydrology/Figures/") # maps are saved here
phantomjs_path <- paste0(user, "Documents/Modelling/phantomjs/phantomjs/bin")

current_year <- year

# Define `%>%` operator in current environment
`%>%` <- magrittr::`%>%`

# negate in function
'%!in%' = Negate('%in%')

# reads dates written as either 2027-04-01 or 4/1/2027 (Excel changes the format when it saves a csv)
parse_any_date <- function(x) {
  as.Date(lubridate::parse_date_time(as.character(x), orders = c("Ymd", "mdY", "Ymd HMS", "mdY HMS"), quiet = TRUE))
}

# CARTO background tiles using your own API key from .Renviron (same function as in ERA5_snowtrends_map_hourly_functions.R)
carto_tiles <- function(map, style = "light_all", group = "CartoDB") {
  key <- Sys.getenv("CARTO_API_KEY")          # match your .Renviron name
  if (!nzchar(key)) stop("CARTO_API_KEY not set in .Renviron")

  leaflet::addTiles(
    map,
    urlTemplate = sprintf(
      "https://{s}.basemaps.cartocdn.com/rastertiles/%s/{z}/{x}/{y}.png?key=%s",
      style, key),
    attribution = paste0(
      '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, ',
      '&copy; <a href="https://carto.com/attributions">CARTO</a>'),
    group = group,
    options = leaflet::tileOptions(subdomains = "abcd", maxZoom = 20)
  )
}

# adds the background map chosen in the basemap setting
add_basemap <- function(map) {
  if (basemap == "carto_key") {
    carto_tiles(map, style = "light_nolabels") # CARTO light background, no labels
  } else {
    leaflet::addProviderTiles(map, basemap, group = "Basemap")
  }
}

# ---- Preflight check: stops early with a clear message instead of failing halfway -----------------

basin_ids <- c(Hay = "07OB001", Liard = "10ED002", Yellowknife = "07SB002", Snare = "07SA002",
               Slave = "07NC003", Taltson = "07QA001", GB = "10JC003", GSL = "10FB004", Peel = "10MC022")
basin_file <- function(id) paste0(shapefile_path, substr(id, 1, 2), "/", id, "/", id, "_DrainageBasin_BassinDeDrainage.shp")

required_files <- c(
  "YT data"           = paste0(folder, "YT_Data/YT_Data_", year, ".csv"),
  "AB data"           = paste0(folder, "AB_Data/AB_Data_", year, ".csv"),
  "AB station points" = paste0(folder, "AB_Data/Shapefiles/snow_stations.dbf"),
  "BC locations"      = paste0(folder, "BC_Data/BC_Locations.csv"),
  "NT coordinates"    = paste0(folder, "site_coordinates_highres.csv"),
  "Mackenzie basin"   = paste0(shapefile_path, "MackenzieRiverBasin_FDA.shp"),
  "NWT border"        = paste0(shapefile_path, "NWT_Boundary/NWT_ENR_BND_FND.shp"),
  setNames(vapply(basin_ids, basin_file, ""), paste(names(basin_ids), "basin"))
)
if (!download_BC_data) required_files["BC data"] <- paste0(folder, "BC_Data/BC_Data_", year, ".csv")
if (ERA5_Land) required_files["ERA5 April max"] <- paste0(folder, "SWEmax_dataset/April_max_combined_max_snow.csv")

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("These input files could not be found:\n",
       paste0("  - ", names(missing_files), ": ", missing_files, collapse = "\n"),
       "\nCheck the file name (including the year) and folder. See README_snow_scripts.md.", call. = FALSE)
}

if (ERA5_Land) {
  era5_header <- names(read.csv(required_files[["ERA5 April max"]], nrows = 1))
  if (paste0("Snow_", year) %!in% era5_header) {
    stop("April_max_combined_max_snow.csv has no Snow_", year, " column.\n",
         "Run ERA5_spring_snow_analysis.R for ", year, " first (see README), or set ERA5_Land = F.", call. = FALSE)
  }
}

if (basemap == "carto_key" && !nzchar(Sys.getenv("CARTO_API_KEY"))) {
  stop("basemap = \"carto_key\" but there is no CARTO_API_KEY in your .Renviron.\n",
       "Add your own key (see BEFORE YOU RUN step 5) and restart R, or use a background that doesn't need a key, e.g. basemap = \"Esri.WorldGrayCanvas\".", call. = FALSE)
}
if (basemap != "carto_key" && basemap %!in% names(leaflet::providers)) {
  stop("basemap = \"", basemap, "\" is not a leaflet background map. Check the spelling, or run names(leaflet::providers) to see the options.", call. = FALSE)
}

if (!file.exists(file.path(phantomjs_path, "phantomjs.exe")) && save) {
  warning("phantomjs not found in ", phantomjs_path, " - the .html map will save but the .png will not. ",
          "Install with webshot::install_phantomjs() or see README.")
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# set web shot path (added to the end of PATH rather than replacing it)
Sys.setenv(PATH = paste(Sys.getenv("PATH"), phantomjs_path, sep = ";"))

# ---- Load NT data ----------------------------------------------------------------------------------

# source in SWE summary function (this also connects to the snow database and creates md_3).
# The option stops SWE_summary.R from writing its own summary table and boxplot.
old_opts <- options(swe_summary.functions_only = TRUE)
source(paste0(user, "Documents/R_Scripts/Packages/snow/R/SWE_summary.R"))
options(old_opts)

# SWE_summary.R and R_oracle_connect.R redefine `user` without the trailing slash - reset it
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")), "/")

NT_Data <- SWEsummary(data = md_3,
                      surface="upland",
                      minyear=2000,
                      maxyear=2021,
                      curmaxyear=year,
                      flags = c("Y", "HS", "Sk", "P", "Sk_2"),
                      ldensity = 0.1,
                      act="A",
                      write = FALSE) %>%
  mutate(Jurisdiction = "NT") %>%
  rename("Region"="region", "Site"="site", "Length_Total"="yrs", "Length_Trimmed"="yrs2",
         "Mean_SWE_mm"="meanSWE_allyears", "Current_Depth"="meandepth_cur",
         "Current_SWE"="meanSWE_cur", "Percent_Normal"="pernorm_allyrs") %>%
  mutate(Date = NA)

if (all(is.na(NT_Data$Current_SWE))) {
  warning("No NT snow survey data found for ", year, " in the snow database. Have this year's surveys been loaded?")
}

# ---- Load YT data ----------------------------------------------------------------------------------

YT_Data <- read.csv(required_files[["YT data"]])

YT_Data <- YT_Data %>%
  dplyr::select(1:16) %>%
  dplyr::filter(!is.na(Current_SWE)) %>%
  dplyr::mutate(Date = parse_any_date(Date))

# ---- Load AB data ----------------------------------------------------------------------------------

Data <- read.csv(required_files[["AB data"]], check.names = FALSE)
if (ncol(Data) < 14) {
  stop("AB_Data_", year, ".csv has ", ncol(Data), " columns but should have at least 14 ",
       "(Basin, Station Name, Station ID, Snow Parameter, Date, Value, ..., Historical Average, Years, % of Avg). See README.",
       call. = FALSE)
}
Data <- Data %>%
  dplyr::select(c(1,2, 4:6, 12:14)) %>%
  dplyr::mutate(Length_Trimmed = NA)

# Rename columns
colnames(Data) <- c("Region", "Site", "Parameter", "Date", "Value", "Mean", "Length_Total", "Percent_Normal",
                    "Length_Trimmed")

Data <- Data %>%
  dplyr::mutate(Length_Total = suppressWarnings(as.numeric(Length_Total)),
                Percent_Normal = suppressWarnings(as.numeric(Percent_Normal)),
                Value = suppressWarnings(as.numeric(Value)),
                Mean = suppressWarnings(as.numeric(Mean)))

# AB data is organized so that all values (depth and SWE) are coded differently. Need to create individual columns for these values

Depth <- Data %>%
  dplyr::filter(Parameter == "SD-SS") %>%
  dplyr::select(Site, Value) %>%
  dplyr::rename(Current_Depth = Value)

SWE <- Data %>%
  dplyr::filter(Parameter == "SW-SS") %>%
  dplyr::rename(Current_SWE = Value)

Data <- base::merge(SWE, Depth, by = "Site")

Data$Site <- toupper(Data$Site)

# Add Lat/Longs

Locations <- foreign::read.dbf(required_files[["AB station points"]]) %>%
  dplyr::select(c(NAME, LAT, LONG)) %>%
  dplyr::rename(Site = NAME, Lat = LAT, Long = LONG) %>%
  dplyr::mutate(Site = case_when(Site == "MARMOT-JAPER" ~ "MARMOT-JASPER", TRUE ~ Site))

# fix Alberta coordinates

Locations <- Locations %>%
  dplyr::mutate(Lat = ifelse(Site == "BARRHEAD NORTH", 54.283611111, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "BARRHEAD NORTH", -114.370278, Long)) %>%
  dplyr::mutate(Lat = ifelse(Site == "BARRHEAD WEST", 54.181111111, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "BARRHEAD WEST", -114.786389, Long)) %>%
  dplyr::mutate(Lat = ifelse(Site == "EUREKA RIVER", 56.516666667, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "FAIRVIEW", 56.033333333, Long)) %>% # NOTE: 56.03 looks like a latitude, not a longitude - check (see README)
  dplyr::mutate(Lat = ifelse(Site == "FORT CHIPEWYAN", 58.711111, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "FORT CHIPEWYAN", -111.147222, Long)) %>%
  dplyr::mutate(Lat = ifelse(Site == "HINTON", 53.553888889, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "HINTON", -117.9225, Long)) %>%
  dplyr::mutate(Lat = ifelse(Site == "KINUSO", 55.2275, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "KINUSO", -115.383611, Long)) %>%
  dplyr::mutate(Lat = ifelse(Site == "LODGEPOLE", 53.493333333, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "LODGEPOLE", -115.426111, Long)) %>%
  dplyr::mutate(Lat = ifelse(Site == "MARMOT-JASPER", 52.806666667, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "MARMOT-JASPER", -118.089444, Long)) %>%
  dplyr::mutate(Lat = ifelse(Site == "MEADOWVIEW", 54.003333333, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "MEADOWVIEW", -114.674722, Long)) %>%
  dplyr::mutate(Lat = ifelse(Site == "SPRING CREEK#1", 54.951944444, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "SPRING CREEK#1", -117.750556, Long)) %>%
  dplyr::mutate(Lat = ifelse(Site == "SUNWAPTA FALLS", 52.543333333, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "SUNWAPTA FALLS", -117.648056, Long)) %>%
  dplyr::mutate(Lat = ifelse(Site == "TWIN LAKES", 54.058888889, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "TWIN LAKES", -114.794722, Long)) %>%
  dplyr::mutate(Lat = ifelse(Site == "WESTLOCK", 54.006944444, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "WESTLOCK", -113.989722, Long)) %>%
  dplyr::mutate(Lat = ifelse(Site == "WHITECOURT", 54.070277778, Lat)) %>%
  dplyr::mutate(Long = ifelse(Site == "WHITECOURT", -115.554444, Long))

# Merge Location and Data files
# there can be differences between site names for these two dataframes

two_word_sites <- c("BARRHEAD", "FORT", "HIGH", "MOUNT", "WILKINSON", "ST", "SOUTH", "NORTH", "LOST", "LITTLE", "LEE", "BOW")

# Add a simplified site name column for matching
Locations <- Locations %>%
  dplyr::mutate(
    Site_Match = case_when(
      # For sites that start with any of the special words, get first two words
      stringr::str_detect(Site, paste0("^(", paste(two_word_sites, collapse = "|"), ")")) ~
        stringr::str_extract(Site, "^[A-Za-z]+ [A-Za-z]+"),
      # For all other sites, get first word or hyphenated pair
      TRUE ~ stringr::str_extract(Site, "^[A-Za-z]+-[A-Za-z]+|^[A-Za-z]+")
    )
  )

Data <- Data %>%
  dplyr::mutate(
    Site_Match = case_when(
      stringr::str_detect(Site, paste0("^(", paste(two_word_sites, collapse = "|"), ")")) ~
        stringr::str_extract(Site, "^[A-Za-z]+ [A-Za-z]+"),
      TRUE ~ stringr::str_extract(Site, "^[A-Za-z]+-[A-Za-z]+|^[A-Za-z]+")
    )
  )

# AB sites that will be dropped because they don't match a station location
unmatched_AB <- setdiff(Data$Site_Match, Locations$Site_Match)
if (length(unmatched_AB) > 0) {
  message("AB sites with no location match (not mapped): ", paste(unmatched_AB, collapse = ", "))
}

# Merge using the simplified names
AB_Data <- base::merge(Data, Locations, by = "Site_Match") %>%
  dplyr::select(-Site_Match) %>%  # Remove the matching column
  dplyr::group_by(Site.x) %>%     # Use original site name from Data
  dplyr::mutate(Jurisdiction = "AB") %>%
  dplyr::rename("Site" = "Site.x")

AB_Data$Date <- parse_any_date(AB_Data$Date)

# ---- Load BC data ----------------------------------------------------------------------------------
# Import data and rearrange datasets appropriately
Locations <- read.csv(required_files[["BC locations"]])
Locations <- Locations[,c(3,6,7)]
colnames(Locations) <- c("Site", "Lat", "Long")

# Convert Site names to same case
Locations$Site <- toupper(Locations$Site)

bc_file <- paste0(folder, "BC_Data/BC_Data_", year, ".csv")

if (download_BC_data) {
  # archive = all past years, current = this year. Saved together so the map can be re-made later
  bc_url <- "https://www.env.gov.bc.ca/wsd/data_searches/snow/asws/data/"
  bc_archive <- read.csv(paste0(bc_url, "allmss_archive.csv"))
  bc_current <- read.csv(paste0(bc_url, "allmss_current.csv"))
  # the two files overlap - keep the current-file version of any survey that is in both
  bc_archive <- bc_archive[!paste(bc_archive[[1]], bc_archive[[4]]) %in% paste(bc_current[[1]], bc_current[[4]]), ]
  Data <- rbind(bc_archive, bc_current)
  write.csv(Data, bc_file, row.names = FALSE)
  message("BC data downloaded and saved to ", bc_file)
}

Data <- read.csv(bc_file)

Data <- Data %>%
  dplyr::filter(Survey.Code != "PROBLEM") # filters SWE flagged for problem

Data <- Data[,c(1,2,4:6,10)]
colnames(Data) <- c("Site", "Region", "Date", "Current_Depth", "Current_SWE", "Period")

Data["Per_Norm"] <- NA

Data$Date <- parse_any_date(Data$Date)
Data$Period <- sub("^0", "", trimws(Data$Period)) # BC website writes "01-Apr", Excel changes it to "1-Apr"

if (!any(lubridate::year(Data$Date) == year & Data$Period == "1-Apr", na.rm = TRUE)) {
  warning("BC_Data_", year, ".csv has no April 1 (", year, ") surveys - BC sites will not be mapped. ",
          "Has BC published this year's April 1 data yet?")
}

historic <- Data %>%
  dplyr::filter(Period == "1-Apr") %>%
  dplyr::mutate(Year = lubridate::year(Date)) %>%
  dplyr::filter(Year < year) %>%
  dplyr::group_by(Site) %>%
  dplyr::summarize(Mean_SWE_mm = round(mean(Current_SWE, na.rm = T), 1),
                   Mean_Depth = round(mean(Current_Depth), 1),
                   Length_Total = length(!is.na(Current_SWE)))

current <- Data %>%
  dplyr::group_by(Site) %>%
  dplyr::slice(which.max(Date)) %>%
  dplyr::filter(lubridate::year(Date) == year,
                Period == "1-Apr")

# Merge historic and current data with sites from 'Locations' data frame which are in Mackenzie basin
BC_Data <- base::merge(base::merge(historic, current, by = "Site"), Locations, by = "Site") %>%
  dplyr::group_by(Site) %>%
  dplyr::slice(which.max(Date)) %>%
  dplyr::mutate(Jurisdiction = "BC",
                Length_Trimmed = NA,
                Percent_Normal = round((Current_SWE / Mean_SWE_mm) * 100),
                Per_Norm = Percent_Normal) %>%
  dplyr::select(Site, Mean_SWE_mm, Length_Total, Region, Date, Current_Depth, Current_SWE, Per_Norm,
                Lat, Long, Jurisdiction, Length_Trimmed, Percent_Normal)

# ---- Combine jurisdictions -------------------------------------------------------------------------

# Combine data from each jurisdiction in common data frame
df <- dplyr::bind_rows(YT_Data,
                       NT_Data, AB_Data, BC_Data)

# Remove any rows without data for current year
df <- df[!is.na(df$Percent_Normal), ]

print(table(df$Jurisdiction)) # number of sites per jurisdiction - check none are missing

# Convert to sf object
proj <- '+proj=longlat +datum=WGS84'
df <- sf::st_as_sf(df, coords = c("Long", "Lat"), crs = proj)

# ---- Load shapefiles -------------------------------------------------------------------------------

read_basin <- function(path) {
  shp <- sf::st_read(path, quiet = TRUE)
  shp <- sf::st_transform(shp, sp::CRS(proj))
  sf::st_zm(shp)
}

Mack <- read_basin(required_files[["Mackenzie basin"]])

if(clip_stations == T){
  df_2 <- df %>% dplyr::filter(Site %in% mrb_clip_exceptions)
  df <- sf::st_intersection(df, Mack)
  newcols <- setdiff(names(df), names(df_2))
  df_2[newcols] <- NA
  df <- rbind(df, df_2)
}

Hay         <- read_basin(basin_file(basin_ids[["Hay"]]))
Liard       <- read_basin(basin_file(basin_ids[["Liard"]]))
Yellowknife <- read_basin(basin_file(basin_ids[["Yellowknife"]]))
Snare       <- read_basin(basin_file(basin_ids[["Snare"]]))
Slave       <- read_basin(basin_file(basin_ids[["Slave"]]))
Taltson     <- read_basin(basin_file(basin_ids[["Taltson"]]))
GB          <- read_basin(basin_file(basin_ids[["GB"]]))
GSL         <- read_basin(basin_file(basin_ids[["GSL"]]))
Peel        <- read_basin(basin_file(basin_ids[["Peel"]]))
NWT         <- read_basin(required_files[["NWT border"]])

# Perform clipping operations
Peel_valid <- sf::st_make_valid(Peel)
GB_valid <-  sf::st_make_valid(GB)
Liard_valid <-  sf::st_make_valid(Liard)
GSL_valid <-  sf::st_make_valid(GSL)
Slave_valid <-  sf::st_make_valid(Slave)
Taltson_valid <-  sf::st_make_valid(Taltson)
Hay_valid <-  sf::st_make_valid(Hay)
Snare_valid <- sf::st_make_valid(Snare)
Yellowknife_valid <- sf::st_make_valid(Yellowknife)

Mack_clip <-  sf::st_difference(Mack, Peel_valid)
Mack_clip <-  sf::st_difference(Mack_clip, GB_valid)
Mack_clip <-  sf::st_difference(Mack_clip, Liard_valid)

# NOTE: kept as in 2026 - each line starts again from GSL, so only Taltson is actually removed (see README)
GSL_clip <-  sf::st_difference(GSL, Slave)
GSL_clip <-  sf::st_difference(GSL, Hay_valid)
GSL_clip <-  sf::st_difference(GSL, Taltson_valid)

basin_layers <- list("Mackenzie Basin" = Mack_clip,
                     "Hay Basin" = Hay_valid,
                     "Liard Basin" = Liard_valid,
                     "Great Bear Basin" = GB_valid,
                     "Great Slave Lake Basin" = GSL_clip,
                     "Peel Basin" = Peel_valid,
                     "Taltson Basin" = Taltson_valid,
                     "Slave River Basin" = Slave_valid,
                     "NWT Border" = NWT,
                     "Snare Basin" = Snare_valid,
                     "Yellowknife Basin" = Yellowknife_valid)

if (selected_layer != "all" && selected_layer %!in% names(basin_layers)) {
  stop("selected_layer = \"", selected_layer, "\" is not one of: \"all\", ",
       paste0("\"", names(basin_layers), "\"", collapse = ", "), call. = FALSE)
}
selected_sf <- if (selected_layer != "all") basin_layers[[selected_layer]] else NULL

# ---- Map stations ----------------------------------------------------------------------------------

# Create bins for mapping
df$Percent_Normal_Bin <- cut(df$Percent_Normal,
                             breaks = c(0, 50, 70, 90, 110, 130, 150, 250, 350, 2000),
                             include.lowest = T,
                             labels = c("< 50%", "51 - 70%", "71 - 90%", "91 - 110%",
                                        "111 - 130%", "131 - 150%", "151 - 250%", "251 - 350%", "> 350%"))

customPalette = c("#D73027","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#043678") #or use "RdYlBu"

PerCol <- leaflet::colorFactor(palette = customPalette, df$Percent_Normal_Bin)

# Clip data
if (!is.null(selected_sf)) {

  df_2 <- df %>%
    dplyr::filter(Site %in% site_clip_exceptions) # add back in station data that are representative of basin but not directly in basin

  df <- df %>% dplyr::filter(sf::st_within(geometry, sf::st_geometry(selected_sf), sparse = FALSE))

  df <- rbind(df_2, df)
}

if(is.na(exclude_sites[1])==F){
  df <- df %>%
    dplyr::filter(Site %!in% exclude_sites)
}

# Create leaflet map
map <- leaflet() %>%
  add_basemap()

if (mack_basin_outline == TRUE){
  map <- map %>%
    leaflet::addPolygons(data = Mack, color = "grey", weight = 2, opacity = 0.5, fillOpacity = 0, group = "Mackenzie Basin")
}

if(!is.null(selected_sf)) {
  map <- map %>%
    leaflet::addPolygons(data = selected_sf, color = "black", weight = 2, opacity = 1, fillOpacity = 0, group = "basin outline")
}

if(add_legend == TRUE){
  map <- map %>%
    addLegend(
      position = "bottomleft",
      colors = customPalette,
      labels = c("< 50%", "51 - 70%", "71 - 90%", "91 - 110%",
                 "111 - 130%", "131 - 150%", "151 - 250%", "251 - 350%", "> 350%"),
      title = paste0("Snow Water Equivalent", "<br>", "Spring ", year,  "<br>", "Relative to Average"),
      opacity = 1
    )
}

# ---- ERA5-Land gridded background ------------------------------------------------------------------

if(ERA5_Land == TRUE){
  ERA5_snow <- read.csv(required_files[["ERA5 April max"]])

  # pivot dataframe
  ERA5_snow <- ERA5_snow %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("Snow_"),
      names_to = "sd_year",
      names_prefix = "Snow_",
      values_to = "sd"
    ) %>%
    dplyr::rename(
      Latitude = lat,
      Longitude = lon
    )

  # select certain columns
  ERA5_snow <- ERA5_snow %>%
    dplyr::select(Latitude, Longitude, sd, sd_year)

  # create average from all years except the current one
  ERA5_snow <- ERA5_snow %>%
    dplyr::group_by(Longitude, Latitude) %>%
    dplyr::mutate(Snow_Avg = mean(sd[sd_year != current_year], na.rm=T))

  per_norm_snow_values <- ERA5_snow %>%
    dplyr::group_by(Longitude, Latitude) %>%
    dplyr::filter(sd_year == current_year) %>%
    dplyr::mutate(per_norm = (sd/Snow_Avg)*100) %>% # calculate percent normal for given year
    dplyr::filter(is.na(per_norm)==FALSE) # filters NA values

  sd_values <- per_norm_snow_values
  # Convert to sf object
  sd_values_sf <- sf::st_as_sf(sd_values, coords = c("Longitude", "Latitude"), crs = proj)

  if(clip_ERA5 == T) {
    sd_values_sf <- sf::st_intersection(sd_values_sf, Mack)

    if (!is.null(selected_sf)) {
      sd_values_sf <- sd_values_sf %>% dplyr::filter(sf::st_within(geometry, sf::st_geometry(selected_sf), sparse = FALSE))
    }
  }
  sd_values_sf <- sd_values_sf %>%
    dplyr::mutate(
      Bin = cut(
        per_norm,
        breaks = c(0, 50, 70, 90, 110, 130, 150, 250, 350, 2000),
        include.lowest = T,
        labels = c("< 50%", "51 - 70%", "71 - 90%", "91 - 110%",
                   "111 - 130%", "131 - 150%", "151 - 250%", "251 - 350%", "> 350%"))
    )

  # filter negative values
  sd_values_sf <- sd_values_sf %>%
    dplyr::filter(Snow_Avg > 0)

  PerCol_gridded <- leaflet::colorFactor(palette = customPalette, sd_values_sf$Bin)

  map <- map %>%
    leaflet::addCircleMarkers(data = sd_values_sf,
                              fillColor = ~PerCol_gridded(Bin),
                              fillOpacity = 1,
                              color = ~PerCol_gridded(Bin),
                              weight = 0,
                              opacity = 1,
                              radius = 1)
}

map <- map %>%
  leaflet::addCircleMarkers(data = df,
                            color = "black",
                            fillColor = ~PerCol(Percent_Normal_Bin),
                            radius = 7,
                            label = ~Site,
                            weight = 1,
                            opacity = 0.8,
                            fillOpacity = 0.8,
                            popup =~ paste0("Percent of Average (%): ", round(Percent_Normal, 1), "<br>",
                                            "SWE (mm): ",  round(Current_SWE, 1), "<br>",
                                            "Snow Depth (cm): ", round(Current_Depth, 1), "<br>",
                                            "Years of Record: ", Length_Total, "<br>",
                                            "Geometry", geometry)) %>%
  leaflet::addScaleBar()

map

# ---- Save ------------------------------------------------------------------------------------------

layer_tag <- if (selected_layer == "all") "MRB" else gsub(" ", "_", selected_layer)
map_file <- paste0(output_dir, "map_", layer_tag, "_April1_", year)

if(save == T){
  htmlwidgets::saveWidget(map, file = paste0(map_file, ".html"), selfcontained = TRUE)
  webshot::webshot(paste0(map_file, ".html"), paste0(map_file, ".png"), delay = 20, vwidth = 1100, vheight = 900, zoom = 1)
  message("Map saved to ", map_file, ".html / .png")
  message("To finish it in CorelDraw, see the AFTER YOU RUN links at the top of this script.")
}

# convert df back from sf object
Table <- df %>%
  dplyr::mutate(Latitude = geometry[2],
                Longitude = geometry[1])

if(selected_layer != "all"){
  print(paste0("Basin-averaged per cent of normal (snow survey data) for ", selected_layer, " is: ",  round(mean(Table$Percent_Normal))))
}

if(selected_layer != "all" && ERA5_Land == TRUE){
  Table_ERA5 <- sd_values_sf %>%
    dplyr::mutate(Latitude = geometry[2],
                  Longitude = geometry[1])
  print(paste0("Basin-averaged per cent of normal (ERA5-Land data) for ", selected_layer, " is: ",  round(mean(Table_ERA5$per_norm))))
}

################
# make separate map for inset (only needed when mapping one basin)
if(selected_layer != "all"){
  map_inset <- leaflet() %>%
    add_basemap() %>%
    leaflet::addPolygons(data = NWT, color = "black", weight = 1, opacity = 1, fillOpacity = 0, group = "NWT border") %>%
    leaflet::addPolygons(data = selected_sf, color = "grey", weight = 1)

  map_inset

  if(save == T){
    inset_file <- paste0(output_dir, "map_inset_", layer_tag, "_", year)
    htmlwidgets::saveWidget(map_inset, file = paste0(inset_file, ".html"), selfcontained = TRUE)
    webshot::webshot(paste0(inset_file, ".html"), paste0(inset_file, ".png"), delay = 20, vwidth = 900, vheight = 700, zoom = 3)
  }
}

######################################################################################################
