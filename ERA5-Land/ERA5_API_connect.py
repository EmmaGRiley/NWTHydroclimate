#ERA5 API connect

import cdsapi

dataset = "reanalysis-era5-land"
request = {
  "variable": ["snow_depth_water_equivalent"],
  "year": "2013",
  "month": "02",
  "day": ["15"],
  "time": ["00:00"],
  "data_format": "netcdf",
  "download_format": "zip",
  "area": [80, -140, 51, -95]
}

client = cdsapi.Client()
