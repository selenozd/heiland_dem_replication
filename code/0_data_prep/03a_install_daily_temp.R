
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Download and Process Daily Temperature Data for New York City
### Author: Selen Ozdogan

# 0 # Setup 
rm(list = ls())
gc()

# Run the libraries script to load (and install if necessary) 
# all required packages
source("code/00_libraries.R")

# 1 # Load and Process GHCHd Weather Station Data

# Define the station IDs for NYC (Central Park, JFK, LaGuardia)
station_ids <- c("USW00094728", "USW00094789", "USW00014732")

# A full list of all stations is available from the NCEI
# https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/
# doc/ghcnd-stations.txt

# Define the base URL for data access
base_url <- "https://www.ncei.noaa.gov/data/daily-summaries/access/"

# Loop over the stations, read the csv file from the URL, and combine all data
all_station_data <- station_ids %>%
  map_dfr(~read_csv(
    paste0(base_url, .x, ".csv"),
    show_col_types = FALSE 
  ))

# Clean the data
ghchd_clean <- all_station_data %>%
  # Create a clean date variable and separate variables
  mutate(date = ymd(DATE),
         year = year(date),
         month = month(date),
         day_of_year = yday(date)) %>%
  # Drop the original date variable
  dplyr::select(-DATE) %>%
  # Keep only 1976-2022, our period of interest
  filter(year <= 2022 & year >= 1976) %>%
  # Keep daily max and min temperature, and precipitation variables
  dplyr::select(date, TMAX, TMIN, PRCP) %>%
  # Group by date (to aggregate variables citywide using the three stations)
  group_by(date) %>%
  # Calculate mean maximum and minimum temperatures (tmax, tmin) across stations
  # Data in tenths of degrees Celsius, so divide by 10 to convert to Celsius
  # Get average precip across stations and convert from tenths of mm to inches
  summarise(tmax_c = mean(TMAX / 10, na.rm = TRUE),
            tmin_c = mean(TMIN / 10, na.rm = TRUE),
            precip_in = mean(PRCP / 254, na.rm = TRUE)) %>%
  # Create temperatures in Fahrenheit
  mutate(tmax_f = (tmax_c * 9/5) + 32,
         tmin_f = (tmin_c * 9/5) + 32)
  
# Check out the first and last  few rows of the cleaned data
print(head(ghchd_clean))
print(tail(ghchd_clean))
  

# 2 # Download and Clean ERA5 Data 

# ERA5 data can be downloaded using a Python script that calls the Climate Data
# Store (CDS) API. This requires a one-time setup of the CDS API on your 
# machine, with instructions available here:
# https://cds.climate.copernicus.eu/how-to-api)

# Once your API is configured, run the accompanying Python script, 
# 03b_install_era5.py, to download the data into a local directory. 

# Unzip the Downloaded ERA5 Data

# Define years and chunks
years_to_unzip <- 1976:2022
chunks <- 1:3

# Define the directory
climate_path <- here("data", "raw_data", "era5")

# Unzip and rename files
expand_grid(year = years_to_unzip, chunk = chunks) %>%
  pwalk(~{
    zip_file <- here(climate_path, paste0("era5_", .x, "_", .y, ".zip"))
    
    unzip(zipfile = zip_file, exdir = climate_path)
    
    current_file <- here(climate_path, "data_0.nc")
    new_file     <- here(climate_path, paste0("era5_", .x, "_", .y, ".nc"))
    
    file.rename(from = current_file, to = new_file)
  })

# Download NYC Boundary Shapefile

# Define the full path to the shapefile
shp_path <- file.path("data/raw_data", "nybb_25b/nybb.shp")

# Define the URL and the local destination for the zip file
nyc_url1 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/"
nyc_url2 <- "bytes/borough-boundaries/nybb_25b.zip"
zip_dest <- file.path("data/raw_data", "nybb.zip")

# Download the zipped data
download.file(paste0(nyc_url1, nyc_url2), destfile = zip_dest)

# Unzip the downloaded file into our local directory
unzip(zip_dest, exdir = "data/raw_data")

# Read in the shapefile
nyc_boundary <- st_read(shp_path) %>%
  # Project to US National Atlas Equal Area for accurate area calculations
  st_transform(crs = st_crs(2163)) %>%
  # Merge all borough polygons into a single feature
  st_union()

# Plot to confirm the boundary was loaded correctly
plot(st_geometry(nyc_boundary), main = "NYC Boundary")


# 3 # Define Functions to Calculate Relative Humidity and Wet Bulb Temperature

# Define a function to calculate relative humidity (RH)
calc_rh <- function(temp_k, dewp_k) {
  
  # The original ERA5 data is in Kelvin. The calculations require Celsius.
  # Convert Kelvin to Celsius
  temp_c <- temp_k - 273.15
  dewp_c <- dewp_k - 273.15
  
  # Define constants for the formula
  a <- 17.625
  b <- 243.04
  
  # Calculate numerator and denominator of the RH formula
  numer <- exp((a * dewp_c) / (b + dewp_c))
  denom <- exp((a * temp_c) / (b + temp_c))
  
  # Calculate relative humidity
  rh <- 100 * (numer / denom)
  return(rh)
}

# Define a function to calculate wet bulb temperature, using relative humidity
calc_wetbtemp <- function(rh, temp_c) {
  # define constants for the formula
  k <- 0.151977
  l <- 8.313659
  m <- 1.676331
  n <- 0.00391838
  p <- 0.023101
  s <- 4.686035
  
  # calculate wet bulb temperature
  wetbtemp <- temp_c * atan((k*(rh + l))^(1/2)) +
    atan(temp_c + rh) - atan(rh - m) +
    (n*(rh^(3/2))) * atan(p*rh) - s
  
  return(wetbtemp)
}

# 4 # Define a Function to Process Data for a Single Year

process_yearly_data <- function(year, climate_path, nyc_boundary) {
  
  print(paste0("Working with year: ", year))
  
  # a # Read Raster Data
  
  # Define the variables to extract from the raw files
  vars <- list("d2m", "t2m") # d2m: dew point, t2m: air temp
  
  # Loop through the year chunks & variables, stack, and reproject data
  for (i in 1:3) {
    for (v in vars) {
      name <- brick(file.path(climate_path,
                              paste0("era5_", year, "_", i, ".nc")), 
                    varname = v) %>%
              projectRaster(crs = crs(nyc_boundary))
      assign(paste0(v, i), name)
    }
  }
  rm(name)
  
  # b # Append chunks and get one brick for each year and variable
  d2m <- stack(d2m1, d2m2, d2m3)
  t2m <- stack(t2m1, t2m2, t2m3)
  rm(d2m1, d2m2, d2m3, t2m1, t2m2, t2m3)
  
  # c # Calculate Wet Bulb Temperature
  
  # Calculate relative humidity
  rh <- calc_rh(t2m, d2m)
  
  # Convert air temperature to Celsius and calculate wet bulb temperature
  t2m_c <- t2m - 273.15
  wetbtemp_c <- calc_wetbtemp(rh, t2m_c)
  
  # Convert final wet bulb temperature from Celsius to Fahrenheit
  wetbtemp_f <- (wetbtemp_c * 1.8) + 32
  
  # d # Aggregate Hourly Data to Daily Summaries
  
  # Get the number of layers in the wet bulb temperature brick
  num_lyrs <- nlayers(wetbtemp_f)
  
  # Initialize empty lists to store the new day layers
  daily_lyrs_mean <- list()
  daily_lyrs_min <- list()
  daily_lyrs_max <- list()
  
  # Initialize iteration
  iter <- 0
  
  # Loop through the layers in groups of 24 (hours of the day)
  for (i in seq(1, num_lyrs, by = 24)) {
    
    # Subset the brick to get the hours of a particular day
    subset <- wetbtemp_f[[i:(i + 24 - 1)]]
    
    # Calculate the daily summaries
    daily_mean <- calc(subset, mean)
    daily_min <- calc(subset, min)
    daily_max <- calc(subset, max)
    
    # Increase the iteration
    iter <- iter + 1
    
    # Add the day layer to the list
    daily_lyrs_mean[[iter]] <- daily_mean
    daily_lyrs_min[[iter]] <- daily_min
    daily_lyrs_max[[iter]] <- daily_max
  }
  
  # Create a brick from the list of layers
  daily_wetb_mean <- brick(daily_lyrs_mean)
  daily_wetb_min <- brick(daily_lyrs_min)
  daily_wetb_max <- brick(daily_lyrs_max)
  
  # e # Spatially Aggregate to NYC Boundary 
  
  # Get spatial max and weighted mean of maximum temperature
  df_max <- exact_extract(daily_wetb_max, nyc_boundary, 
                          c("max", "weighted_mean"),
                          weights = "area",
                          force_df = TRUE,
                          stack_apply = TRUE)
                             
  # Get spatial min and weighted mean of minimum temperature
  df_min <- exact_extract(daily_wetb_min, nyc_boundary, 
                          fun = c("min", "weighted_mean"),
                          weights = "area",
                          stack_apply = TRUE,
                          force_df = TRUE)
  
  # Get spatial weighted mean of mean wet bulb temperature
  df_mean <- exact_extract(daily_wetb_mean, nyc_boundary, 
                           fun = "weighted_mean", 
                           weights = "area",
                           stack_apply = TRUE,
                           force_df = TRUE)

  # f # Clean and Combine Data Frames
  df_max %<>%
      pivot_longer(everything(), 
               names_to = c(".value", "day"), 
               names_pattern = "(\\w+)\\.layer\\.(\\d+)") %>%
    rename(wetb_max_citymax = max, wetb_max_citymean = weighted_mean)
    
  df_min %<>%
      pivot_longer(everything(), 
               names_to = c(".value", "day"), 
               names_pattern = "(\\w+)\\.layer\\.(\\d+)") %>%
    rename(wetb_min_citymin = min, wetb_min_citymean = weighted_mean)
  
  df_mean %<>%
      pivot_longer(everything(), 
               names_to = c(".value", "day"), 
               names_pattern = "(\\w+)\\.layer\\.(\\d+)") %>%
    rename(wetb_mean_citymean = weighted_mean)
    
  # Join all data frames into one, create a date variable
  yearly_df <- list(df_max, df_min, df_mean) %>%
    reduce(full_join, by = "day") %>%
    mutate(
      year = year,
      day = as.integer(day),
      date = make_date(year, 1, 1) + days(day - 1)
    ) %>%
    dplyr::select(date, year, day, everything())
    
  return(yearly_df)
}

# 5 # Process All Years and Combine Data

# Define the full range of years we want to process
years_to_process <- 1976:2022

# Use map_dfr() to iterate through each year and combine the results
era5_clean <- map_dfr(years_to_process, ~process_yearly_data(
  year = .x,
  climate_path = here("data", "raw_data", "era5"),
  nyc_boundary = nyc_boundary
))

# Arrange the final data frame by date
era5_clean %<>% 
  arrange(date)

# Let's inspect the first and last few rows of our complete dataset
print(head(era5_clean))
print(tail(era5_clean))

# 6 # Merge GHCDd (Air Temp) and ERA5 (Wet Bulb Temp) Data and Create Lagged 
# Measures

# Merge GHCDd and ERA5 Data

# Merge the GHCDd and ERA5 summaries by date
daily_climate_data <- full_join(ghchd_clean, era5_clean, by = "date") %>%
  # Sort by date
  arrange(date) %>%
  # Drop variable we don't need
  dplyr::select(-c("tmax_c", "tmin_c"))


# Generate Lagged Temperature Exposure Variables

# Define the variables to process
vars_for_max <- c("tmax_f", "wetb_max_citymax", "wetb_max_citymean")
vars_for_min <- c("tmin_f", "wetb_min_citymin", "wetb_min_citymean")
vars_for_mean <- c("wetb_mean_citymean")
all_vars_for_lags <- c(vars_for_max, vars_for_min)

# Create rolling statistics for temperatures
daily_climate_data <- daily_climate_data %>%
  mutate(
    
    across(
      .cols = all_of(vars_for_max),
      .fns = list(
        "3d" = ~ slide_dbl(.x, max, .before = 2, .complete = FALSE),
        "7d" = ~ slide_dbl(.x, max, .before = 6, .complete = FALSE),
        "10d" = ~ slide_dbl(.x, max, .before = 9, .complete = FALSE)
      ),
      .names = "{.col}_{.fn}"
    ),
    
    across(
      .cols = all_of(vars_for_min),
      .fns = list(
        "3d" = ~ slide_dbl(.x, min, .before = 2, .complete = FALSE),
        "7d" = ~ slide_dbl(.x, min, .before = 6, .complete = FALSE),
        "10d" = ~ slide_dbl(.x, min, .before = 9, .complete = FALSE)
      ),
      .names = "{.col}_{.fn}"
    ),
    
    across(
      .cols = all_of(vars_for_mean),
      .fns = list(
        "3d" = ~ slide_dbl(.x, mean, .before = 2, .complete = FALSE),
        "7d" = ~ slide_dbl(.x, mean, .before = 6, .complete = FALSE),
        "10d" = ~ slide_dbl(.x, mean, .before = 9, .complete = FALSE)
      ),
      .names = "{.col}_{.fn}"
    ),
    
  )

# View the resulting dataset
glimpse(daily_climate_data)

# Export as csv
write.csv(daily_climate_data, "data/nyc_1976_2022_climate_data.csv")


