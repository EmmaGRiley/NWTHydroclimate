#Updating State of the Environment figures
#required packages
library(dplyr)
library(tidyhydat)

#Mean annual river yields based on watershed size

#Make sure hydat files are up to date:
tidyhydat::download_hydat(dl_hydat_here = NULL, ask = TRUE)
#The line above takes ~10 minutes to download a new sqlite file

#All NT stations available in the sqlite file:

# NT_flowstations = tidyhydat::allstations %>%
#   dplyr::filter(PROV_TERR_STATE_LOC == "NT",
#                 HYD_STATUS == "ACTIVE",
#                 STATION_NUMBER %!in% levelstations,
#                 STATION_NUMBER %!in% c("10NB001",
#                                        "10MC023",
#                                        "10MC022",
#                                        "10MC010",
#                                        "10MC011",
#                                        "10LC019",
#                                        "10LD001",
#                                        "10LC021",
#                                        "10KD001",
#                                        "07SB020",
#                                        "07RB001",
#                                        "07PA003"))
# 
# levelstations = c("07OB002",
#                   "07QD002",
#                   "07SB001",
#                   "07SB012",
#                   "07SB014",
#                   "07SB015",
#                   "07SB017",
#                   "10JE002",
#                   "10LC020",
#                   "10PB003")
###########################################################
#The following watersheds are chosen based on data availability
#The ones below have data until 2020-12-31 or later


small_watersheds = c("07SB013",
                     "07SA004", "07SB010", "10KB001",
                     "10ED009",
                     "10ED003",
                     "10GC003", 
                     "10FB005",
                     "10PA002",
                     "07SC002",
                     "10FA002")
medium_watersheds = c("07TA001", "07RD001", "10JA002",
                      "07OB001", "10PB001", "10HA004",
                      "07SB003")
large_watersheds = c("10ED002", "07NB001", "10JC003", "10LC014")

all_watersheds = c(small_watersheds, medium_watersheds, large_watersheds)

all_lakes = c("07SB001", "10JE002")

#I removed 07SA002 and 07SB002 from the medium watersheds analysis as n was already 7, 
#and the flow is regulated. I removed 10KA001 as it is redundant.

#The watersheds below are the ones used in the 2022 SOE report

small_watersheds2017 = c("10ND002", 
                         "07SB013", 
                         "10ED009", 
                         "10ED003", 
                         "10LC007",
                         "10LC003", 
                         "10FB005",
                         "07SA004",
                         "10GC003", 
                         "07SB010", 
                         "06JB001")
medium_watersheds2017 = c("07TA001", 
                          "10LA002", 
                          "10PB001",
                          "07RD001",
                          "10JA002",
                          "07OB001",
                          "10MC002")
large_watersheds2017 = c("10ED002", "10LC014", "07NB001")

all_watersheds2017 = c(small_watersheds2017, medium_watersheds2017, large_watersheds2017)

#Creating tables with annual yields for the above stations
Yields = YieldsTable(start_year = 1960,
                     end_year = 2024,
                     data = all_watersheds,
                     DOR = 350,
                     exclude_years = NA)

Yields_CB = YieldsTable(start_year = 1960,
                        end_year = 2023,
                        data = all_watersheds2017,
                        DOR = 350,
                        exclude_years = NA)

Table_Md_2017 = Table_maxdate(start_year = 1960,
                         end_year = 2024,
                         data = all_watersheds2017,
                         DOR = 350,
                         exclude_years = NA)

Table_Md = Table_maxdate(start_year = 1930,
                         end_year = 2024,
                         data = all_watersheds,
                         DOR = 350,
                         exclude_years = NA)

write.csv(Table_Md_2017, "C:/Users/Emma_Riley/Documents/NT_Hydrology/Hydrometric/Table_Md_2017.csv")

write.csv(Table_Md, "C:/Users/Emma_Riley/Documents/NT_Hydrology/Hydrometric/Table_Md.csv")

################################################################
#The following figures are for mean annual river yield plotted over a 
#time period from 1959-current, separated by individual stations / rivers

#Slave River at fitzgerald

SOEfigs_bystn(data = Yields_CB, station_number = "07NB001", start_year = 1959, p.value = 0.05, 
              exclude_years = c(1968, 1969, 1970, 1971, 1996, 1997, 2018:2021))

SOEfigs_bystn(data = Yields, station_number = "07NB001", start_year = 1959, p.value = 0.05, 
              exclude_years = c(1968, 1969, 1970, 1971, 1996, 1997))

#For Slave R, annual yields have significantly decreased 
#at a rate of 0.6 mm per year from 1960-2021. In the last SOE report,
#this rate was 0.4mm per year. I don't understand where Casey got that 
#value from as I got as sens slope of -0.75 for that time period.

#Liard River near the mouth
SOEfigs_bystn(data = Yields, station_number = "10ED002", start_year = 1959, p.value = 0.05, exclude_years = NA)

#For Liard River, there have been no significant changes in 
#annual yields over the period of record (1973-2021). The observed
#trend is positive with a rate of 0.39 mm/year. This is different
#than the last SOE where it was interpreted as a decreasing 
#trend for the Liard and Slave.

#Great Bear at outlet
SOEfigs_bystn(data = yields, station_number = "10JC003", start_year = 1959, p.value = 0.05, exclude_years = NA)

#For Great Bear River at outlet of GB, annual yields have not significantly
#changed over the period of record (from 1963-2020). The 'observed' trend
#is positive, with a rate of 0.36 mm a year.

#Mack at ARR
SOEfigs_bystn(data = yields, station_number = "10LC014", start_year = 1959, p.value = 0.05, exclude_years = c(2018, 2019))

#For Mackenzie River ARR, there have been no significant changes in 
#annual yields over the period of record (1973-2019). The 'observed' trend
#is negative with a magnitude of 0.07 mm a year. The trend
#was positive up until 2017 (when the last SOE was put out).
#You can see this by excluding 2018 and 2019 in the function.

#Let's run the trend on Great Bear Lake on Hornby Bay,
#and Great Slave Lake at Yellowknife Bay (but I think maybe Ryan was going to do this/has done this)

Levels = LevelsTable(start_year = 1930,
                     end_year = 2023,
                     data = all_lakes,
                     DOR = 350,
                     exclude_years = NA)

SOEfigs_bystn_level(data = Levels,
                    station_number = "10JE002",
                    p.value = 0.05,
                    start_year = 1930,
                    exclude_years = NA)

#The above plot shows an observed increased trend in Levels
#on Great Bear on Hornby Bay, though not significant.

SOEfigs_bystn_level(data = Levels,
                    station_number = "07SB001",
                    p.value = 0.05,
                    start_year = 1930,
                    exclude_years = c(1968:1971, 1996:1997, 2020:2022))

#The above plot shows the level figured generated in the last SOE,
#confirming the interpretation of no trend at the time (up until 2019)

SOEfigs_bystn_level(data = Levels,
                    station_number = "07SB001",
                    p.value = 0.05,
                    start_year = 1930,
                    exclude_years = c(1968:1971, 1996:1997))

#When rerun up until 2022, still no trend is found (Sen's slope = 0).

#Looking at Hay River @ HR: 
SOEfigs_bystn(data = yields, station_number = "07OB001", start_year = 1959, p.value = 0.05, 
              exclude_years = NA)

#Observed increasing trend, not significant.

#Looking at a particular station in a small watershed  - Cameron
#River below Reid Lake
SOEfigs_bystn(data = Yields, station_number = "10FA002", start_year = 1959, p.value = 0.05, 
exclude_years = c(2020:2022))

#Looking at a particular station in a small watershed - Baker
#Creek at Outlet of lower Martin
SOEfigs_bystn(data = yields, station_number = "07SB013", start_year = 1959, p.value = 0.05, 
              exclude_years = c(2020:2022))

#Looking at a particular station in a small watershed  - Indin River
#above Chalco Lake
SOEfigs_bystn(data = yields, station_number = "07SA004", start_year = 1959, p.value = 0.05, 
              exclude_years = c(2020:2022))

#Looking at a particular station in a small watershed  - Carcajou
#River below Imperial River
SOEfigs_bystn(data = yields, station_number = "10KB001", start_year = 1959, p.value = 0.05, 
              exclude_years = c(2020:2022))

#Looking at a particular station in a small watershed - Scotty Creek
#at Highway 7
SOEfigs_bystn(data = yields, station_number = "10ED009", start_year = 1959, p.value = 0.05, 
              exclude_years = c(2020:2022))

#Looking at a particular station in a small watershed - Martin River at Highway 1
SOEfigs_bystn(data = yields, station_number = "10GC003", start_year = 1959, p.value = 0.05, 
              exclude_years = c(2020:2022))

#Looking at a particular station in a small watershed - Hanbury River above Hoare Lake

SOEfigs_bystn(data = yields, station_number = "06JB001", start_year = 1978, p.value = 0.05, 
              exclude_years = c(2020:2022))

#
SOEfigs_bystn(data = yields, station_number = "10FA002", start_year = 1978, p.value = 0.05, 
              exclude_years = c(2020:2022))

#Looking at specific stations - medium watersheds
# medium_watersheds = c("07TA001", "07SB002", "07RD001", "10JA002",
#                       "07OB001", "10PB001", "10HA004")

yields = YieldsTable(start_year = 1960,
                     end_year = 2023,
                     data = NTstns$STATION_NUMBER[is.na(NTstns$DRAINAGE_AREA_GROSS)==FALSE],
                     DOR = 350,
                     exclude_years = NA)
smallyields = yields %>%
  dplyr::filter(size == "small") %>%
  dplyr::filter(Station_ID %!in% Yields$Station_ID)

NTstns$STATION_NUMBER[is.na(NTstns$DRAINAGE_AREA_GROSS)==TRUE]

NorthSlavestns = c("07SB010",
                   "07SB013",
                   "07SA004",
                   "07SB002",
                   "07SB003",
                   "07TA001")

NorthSlaveyields = YieldsTable(start_year = 1960,
                               end_year = 2023,
                               data = ,
                               DOR = 350,
                               exclude_years = NA)

#The two stations below, 10LC003 and 10LC007, have larger sig. increasing trends than other 
#small watersheds:
SOEfigs_bystn(data = yields, station_number = "10LC003", start_year = 1978, p.value = 0.05, 
              exclude_years = NA)

SOEfigs_bystn(data = yields, station_number = "10LC007", start_year = 2010, p.value = 0.05, 
              exclude_years = NA)

yields_cariboucreek = yields %>%
  dplyr::filter(Station_ID == "10LC007",
                Year>2009)

plot(yields_cariboucreek$Flow~Peel_rain$rain)

SOEfigs_bystn(data = yields, station_number = "06JB001", start_year = 1978, p.value = 0.05, 
              exclude_years = NA) 

SOEfigs_bystn(data = yields, station_number = "10GC003", start_year = 1980, p.value = 0.05, 
              exclude_years = NA)

a = tidyhydat::hy_stations("10GA001")
View(a)

###############################################################
#The following figures are for mean annual river yields,
#separated by small and medium basins


SOEfigs_drainage(data= Yields_CB,
                 drainage_size = "small", 
                 start_year = 1973, 
                 exclude_years = c(2018:2022), 
                 p.value = 0.05)

#The above figure shows the trend that was talked about in the last
#SOE, where small basins show an increasing trend of yields.
#The rate is slightly different than reported in the last
#SOE, I think this just has to do with data availability
#differences between now and then - It's hard to know
#exactly which data was "released" by ECCC at that time.

SOEfigs_drainage(data= Yields,
                 drainage_size = "small", 
                 start_year = 1978, 
                 exclude_years = c(2020:2022), 
                 p.value = 0.05)

#The trend above does persist with the data I'm using, but the rate
#is lower and is not significant.
#I have fewer stations than when Casey performed this analysis.
#N=7 for small watershed stations, when including stations that had 
#data up until 2019. No other "small watershed" stations 
#have been updated up until 2019.

SOEfigs_drainage(data = Yields_CB,
                 drainage_size = "medium", 
                 start_year = 1978, 
                 exclude_years = c(2018:2022),
                 p.value = 0.05)

#The above figure shows the trend that was talked about in the last
#SOE, where medium basins show an increasing trend of yields
#at a rate of 1mm/year.

SOEfigs_drainage(data = Yields,
                 drainage_size = "medium", 
                 start_year = 1978, 
                 exclude_years = c(2020:2022),
                 p.value = 0.05)

#The above figure shows a very similar trend for the period of 1968-2020,
#with an increasing rate of 1mm/year. n=7 for number of stations.

#Creating tables to inspect differences between the data
#I used and that which Casey used:
small17watersheds = Yields_CB %>%
  dplyr::filter(size == "small") %>%
  dplyr::group_by(Station_ID, Year) %>%
  dplyr::summarise(meanyield = mean(Flow, na.rm=TRUE))

smallwatersheds = yields %>%
  dplyr::filter(size == "small") %>%
  dplyr::group_by(Station_ID, Year) %>%
  dplyr::summarise(meanyield = mean(Flow, na.rm=TRUE))

med17watersheds = Yields_CB %>%
  dplyr::filter(size == "medium") %>%
  dplyr::group_by(Station_ID, Year) %>%
  dplyr::summarise(meanyield = mean(Flow, na.rm=TRUE))

medwatersheds = yields %>%
  dplyr::filter(size == "medium") %>%
  dplyr::group_by(Station_ID, Year) %>%
  dplyr::summarise(meanyield = mean(Flow, na.rm=TRUE))

##############################################################
#Mack at ARR -> the above shows trend of 'more flow over winter' as
#described in the previous SOE
SeasonalMeanFlows(station_number = "10LC014",
                  timeperiod_1=c(1981:2000),
                  timeperiod_2 = c(2001:2020),
                  exclude_years = NA)

#Liard River -> more flow over winter?
SeasonalMeanFlows(station_number = "10ED002",
                  timeperiod_1=c(1981:2000),
                  timeperiod_2 = c(2001:2020),
                  exclude_years = NA)

#Slave River -> more flow over winter?
SeasonalMeanFlows(station_number = "07NB001",
                  timeperiod_1=c(1981:2000),
                  timeperiod_2 = c(2001:2020),
                  exclude_years = c(1996, 1997))

#Peel above Fort Mcpherson -> This shows more flow in early winter/fall,
#as stated in previous SOE.. also shows earlier
#spring melt?
SeasonalMeanFlows(station_number = "10MC002",
                  timeperiod_1=c(1981:2000),
                  timeperiod_2 = c(2001:2020),
                  exclude_years = NA)

#Rengleng River -> This shows earlier spring melt and more flow in
#early fall/winter, similar to previous SOE
SeasonalMeanFlows(station_number = "10LC003",
                  timeperiod_1=c(1981:2000),
                  timeperiod_2 = c(2001:2020),
                  exclude_years = NA)
#These observations can also be seen using Cameron River below Reid Lake (07SB010):
SeasonalMeanFlows(station_number = "07SB010",
                  timeperiod_1=c(1981:2000),
                  timeperiod_2 = c(2001:2020),
                  exclude_years = NA)

#Hay River -> Little change in timing between 
#these two periods, similar to previous SOE
SeasonalMeanFlows(station_number = "07OB001",
                  timeperiod_1=c(1981:2000),
                  timeperiod_2 = c(2001:2020),
                  exclude_years = NA)
 
#Checking out Lower Martin - flow over winter, similar to Cameron River below Reid
SeasonalMeanFlows(station_number = "07SB013",
                  timeperiod_1=c(1981:2000),
                  timeperiod_2 = c(2001:2020),
                  exclude_years = NA)

SeasonalMeanFlows_eachyr(station_number = "07SB013", 
                         min_year = 1970,
                         max_year = 2023)

#Trying to find stations to add for the "small watersheds" and "medium watersheds"
#trend analyses

yieldscheck = YieldsTable(start_year = 1960,
                     end_year = 2023,
                     data = c("10ND002"),
                     DOR = 350,
                     exclude_years = NA)

#checking to see if Carcajou and Keele data are appropriate:

#Carcajou
SeasonalMeanFlows_eachyr(station_number = "10KB001", 
                         min_year = 1970,
                         max_year = 2023)

#Keele
SeasonalMeanFlows_eachyr(station_number = "10HA004",
                         min_year = 1970,
                         max_year = 2023)

# small_watersheds = c("10ND002", "07SB013", "10ED009", "10ED003", "10LC003", "10FB005",
#                      "07SA004", "10GC003", "07SB010", "06JB001")
# medium_watersheds = c("07TA001", "10LA002", "10PB001", "07RD001", "10JA002",
#                       "07OB001")
# large_watersheds = c("10ED002", "10LC014", "07NB001")

#Looking at last date (final data) for each station:

# Table_Maxdate = Table_maxdate(start_year = 1960,
#                          end_year = 2023,
#                          data = "10MC007",
#                          DOR = 350,
#                          exclude_years = NA)
# 
# yields = YieldsTable(start_year = 1960,
#                      end_year = 2023,
#                      data = NTstns$STATION_NUMBER,
#                      DOR = 100,
#                      exclude_years = NA)
# 
# NTstns = tidyhydat::hy_stations(prov_terr_state_loc = "NT")
# 
# NTstns = NTstns %>%
#   dplyr::filter(HYD_STATUS == "ACTIVE",
#                 STATION_NUMBER %!in% levelstations,
#                 STATION_NUMBER %!in% c("10NB001",
#                         "10MC023",
#                         "10MC022",
#                         "10MC010",
#                         "10MC011",
#                         "10LC019",
#                         "10LD001",
#                         "10LC012",
#                         "10LC021",
#                         "10KD001",
#                         "07RB001",
#                         "07PA003",
#                         "10LC013",
#                         "10LC015"))
# 
# station = "10PB003"
# 
# tidyhydat::hy_stations(station_number = station)
# 
# flows = FlowsTable(data = station,
#                    exclude_years = NA)
# 
# tidyhydat::hy_daily_flows(station_number = station, start_date = "2019-12-01")
# 
# 
# levels = LevelsTable(start_year = 1930,
#                     end_year = 2023,
#                     data = station,
#                     DOR = 100,
#                     exclude_years = NA)

#looking at stations without drainage area:
a = NTstns$STATION_NUMBER[is.na(NTstns$DRAINAGE_AREA_GROSS)==TRUE]
stnlist = tidyhydat::hy_stations(a)
write.csv(stnlist, "C:/Users/Emma_Riley/Documents/NT_Hydrology/Emails/stnlist.csv")

#Looking into gauge data - Hay River near Meander 07OB003 and Chinchaga River 07OC001
#

#Hay River near Meander 
#When the gauge is operationalized for the season?

#between 1975 and 1996, it operated either between March 1st - October 31st,
#Feb 1st to Nov 30th, or Feb 1st to Nov 1st.
#From 1997-2021, it was operated between March 1st and October 31st.

#Chinchaga River near High level
#Does the gauge stop operating during spring breakup?

years = c(1969:2021)

SeasonalMeanFlows_eachyr(station_number = "07SB013",
                         min_year = 1970,
                         max_year = 2023)

SeasonalFlows_individualyrs(station_number = "07OC001",
                            year = 1969)

flows = tidyhydat::realtime_ws(station_number = "07OC001", parameters = NULL)

############################################################################
#checking tazin lake
LevelsTable

SOEfigs_bystn_level(data = Levels,
                    station_number = "07QC002",
                    p.value = 0.05,
                    start_year = 1930,
                    exclude_years = NA)

