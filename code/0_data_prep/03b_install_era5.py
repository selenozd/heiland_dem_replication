
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City

## Download ERA5 (Temperature, Dew Point Temperature and Precipitation)
## Author: Selen Ozdogan

## This script and the following R script are adapted from the
## replication package for LoPalo (2023).
## https://www.openicpsr.org/openicpsr/project/141721/version/V1/view

# Setup
import os
import cdsapi
import time
from pathlib import Path

c = cdsapi.Client()

# Set working directory: !!! CHANGE TO YOUR PROJECT DIRECTORY !!!
os.chdir("/your/project/directory")

# Define and create the output directory 
output_dir = Path("data/raw_data/era5") 
output_dir.mkdir(parents = True, exist_ok = True)
print(f"Data will be saved in: {output_dir.resolve()}")

# Define the month groups (will download in chunks of 4 months)
months_groups = [
    ['01', '02', '03', '04'],
    ['05', '06', '07', '08'],
    ['09', '10', '11', '12']
]

# Loop through each year
for year in range(1976, 2023):

    # Loop through each group of months (chunk)
    for group_index, months in enumerate(months_groups):
    
        # Define the file name based on the year and chunk index
        file_name = f"era5_{year}_{group_index + 1}.zip"
        target_file = output_dir / file_name
        
        print(f"\nSubmitting request for {year}, months: {months}...")

        try:
            c.retrieve(
                'reanalysis-era5-land',
                {
                    # Select the variables to download
                    'variable': [
                        '2m_dewpoint_temperature', '2m_temperature', 
                        'total_precipitation',
                    ],
                    'year': str(year),
                    'month': months,
                    # Select all days of the month
                    'day': [
                        '01', '02', '03', '04', '05', '06',
                        '07', '08', '09', '10', '11', '12',
                        '13', '14', '15', '16', '17', '18',
                        '19', '20', '21', '22', '23', '24',
                        '25', '26', '27', '28', '29', '30', 
                        '31',
                    ],
                    # Select all times of the day
                    'time': [
                        '00:00', '01:00', '02:00', '03:00', '04:00', '05:00',
                        '06:00', '07:00', '08:00', '09:00', '10:00', '11:00',
                        '12:00', '13:00', '14:00', '15:00', '16:00', '17:00',
                        '18:00', '19:00', '20:00', '21:00', '22:00', '23:00',
                    ],
                    # Define the area of interest (New York City)
                    'area': [
                        41.2, -74.5, 40.2, -73,
                    ],
                    # Define the format of the output file
                    "data_format": "netcdf",
                    "download_format": "zip"
                },
                target_file
            )
            print(f"Successfully downloaded to {target_file}")

        except Exception as e:
            print(f"FAILED request for {year}, months: {months}.")
            print(f"Error: {e}")

        # Wait 5 seconds before next request...
        time.sleep(5)

print("All download attempts are complete.")
