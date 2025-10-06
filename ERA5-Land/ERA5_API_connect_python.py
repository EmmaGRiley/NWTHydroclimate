import cdsapi
import os
from datetime import datetime

# Initialize the CDS API client
client = cdsapi.Client()

# Define the dataset
dataset = "reanalysis-era5-land"

# Define year ranges (6-month chunks to avoid cost limits)
year_ranges = [
    # 2000
    ["2000"], ["2000"],  # Jan-Jun, Jul-Dec
    # 2001
    ["2001"], ["2001"],  # Jan-Jun, Jul-Dec
    # 2002
    ["2002"], ["2002"],  # Jan-Jun, Jul-Dec
    # 2003
    ["2003"], ["2003"],  # Jan-Jun, Jul-Dec
    # 2004
    ["2004"], ["2004"],  # Jan-Jun, Jul-Dec
    # 2005
    ["2005"], ["2005"],  # Jan-Jun, Jul-Dec
    # 2006
    ["2006"], ["2006"],  # Jan-Jun, Jul-Dec
    # 2007
    ["2007"], ["2007"],  # Jan-Jun, Jul-Dec
    # 2008
    ["2008"], ["2008"],  # Jan-Jun, Jul-Dec
    # 2009
    ["2009"], ["2009"],  # Jan-Jun, Jul-Dec
    # 2010
    ["2010"], ["2010"],  # Jan-Jun, Jul-Dec
    # 2011
    ["2011"], ["2011"],  # Jan-Jun, Jul-Dec
    # 2012
    ["2012"], ["2012"],  # Jan-Jun, Jul-Dec
    # 2013
    ["2013"], ["2013"],  # Jan-Jun, Jul-Dec
    # 2014
    ["2014"], ["2014"],  # Jan-Jun, Jul-Dec
    # 2015
    ["2015"], ["2015"],  # Jan-Jun, Jul-Dec
    # 2016
    ["2016"], ["2016"],  # Jan-Jun, Jul-Dec
    # 2017
    ["2017"], ["2017"],  # Jan-Jun, Jul-Dec
    # 2018
    ["2018"], ["2018"],  # Jan-Jun, Jul-Dec
    # 2019
    ["2019"], ["2019"],  # Jan-Jun, Jul-Dec
    # 2020
    ["2020"], ["2020"],  # Jan-Jun, Jul-Dec
    # 2021
    ["2021"], ["2021"],  # Jan-Jun, Jul-Dec
    # 2022
    ["2022"], ["2022"],  # Jan-Jun, Jul-Dec
    # 2023
    ["2023"], ["2023"],  # Jan-Jun, Jul-Dec
    # 2024
    ["2024"], ["2024"]   # Jan-Jun, Jul-Dec
]

# Define month ranges for each chunk
month_ranges = [
    ["1", "2", "3", "4", "5", "6"],    # Jan-Jun
    ["7", "8", "9", "10", "11", "12"], # Jul-Dec
] * 25  # Repeat for each year

# Define the request parameters (same for all chunks)
base_request = {
    "variable": ["total_precipitation"],
    "month": ["12", "11", "10", "9", "8", "7", "6", "5", "4", "3", "2", "1"],
    "day": ["31", "30", "29", "28", "27", "26", "25", "24", "23", "22", "21", "20", "19", "18", "17", "16", "15", "14", "13", "12", "11", "10", "9", "8", "7", "6", "5", "4", "3", "2", "1"],
    "time": [
        "00:00", "01:00", "02:00",
        "03:00", "04:00", "05:00",
        "06:00", "07:00", "08:00",
        "09:00", "10:00", "11:00",
        "12:00", "13:00", "14:00",
        "15:00", "16:00", "17:00",
        "18:00", "19:00", "20:00",
        "21:00", "22:00", "23:00"
    ],
    "area": [65.5, -112, 64.5, -110.5],
    "data_format": "netcdf"
}

print(f"Downloading {dataset} data in 5-year chunks...")
print(f"Variable: {base_request['variable']}")
print(f"Area: {base_request['area']}")
print(f"Total chunks: {len(year_ranges)}")

# Download each chunk
for i, (years, months) in enumerate(zip(year_ranges, month_ranges)):
    year = years[0]  # Single year
    month_start = months[0]
    month_end = months[-1]
    
    # Create descriptive filename
    if month_start == "1":
        period = "JanJun"
    else:
        period = "JulDec"
    
    output_file = f"total_precipitation_{year}_{period}.nc"
    
    print(f"\n📥 Downloading chunk {i+1}/{len(year_ranges)}: {year} {period}")
    print(f"Output file: {output_file}")
    
    # Create request for this 6-month chunk
    request = base_request.copy()
    request["year"] = years
    request["month"] = months
    
    try:
        # Execute the download
        print(f"🚀 Starting download for {year} {period}...")
        start_time = datetime.now()
        
        client.retrieve(dataset, request, output_file)
        
        end_time = datetime.now()
        duration = (end_time - start_time).total_seconds() / 60  # minutes
        
        print(f"✅ Download completed successfully: {output_file}")
        print(f"⏱️  Duration: {duration:.1f} minutes")
        
        # Check if file was created
        if os.path.exists(output_file):
            file_size = os.path.getsize(output_file) / (1024 * 1024)  # Convert to MB
            print(f"📁 File size: {file_size:.2f} MB")
        else:
            print("⚠️  Warning: Output file not found")
            
    except Exception as e:
        print(f"❌ Download failed for {year} {period}: {str(e)}")
        print("💡 Check your .cdsapirc file and internet connection")
        continue
    
    # Wait between chunks to be nice to the API
    if i < len(year_ranges) - 1:  # Don't wait after the last chunk
        print("⏳ Waiting 1 minute before next chunk...")
        import time
        time.sleep(60)

print(f"\n🎉 All downloads completed! Check your directory for {len(year_ranges)} files.")
