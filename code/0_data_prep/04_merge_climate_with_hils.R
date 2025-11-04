
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

# 1 # Read in death counts data
a_death_count <- read_csv("data/death_counts.csv")

#1# get noaa data
noaa <- read.csv(file.path(climatepath, noaapath))

# keep only city means
noaa %<>% subset(NAME == "City means") %>%
  # drop 2023
  filter(Year < 2023) %>%
  # keep relevant variables
  subset(select = c(DATE, TMAX, TMIN, Year, Month, 
                    Day, Day_of_year, Week)) %>%
  # rename variables
  rename_all(tolower)
  
# temperatures need to be divided by 10 to represent celcius
noaa %<>% mutate(tmin = tmin/10, tmax = tmax/10)

# also generate fahrenheit temperatures
noaa %<>% mutate(tmin_f = ((tmin * (9/5)) + 32),
                 tmax_f = ((tmax * (9/5)) + 32))

# get the noaa precip data
noaa_precip <- read.csv(file.path(climatepath, "nyc_temp_precip_1950.csv"))

# get city means (average of all three stations)
noaa_precip %<>% group_by(DATE) %>%
  summarize(precip = mean(PRCP, na.rm = TRUE)) %>%
  # precip is in tenths of mm, convert to inches
  mutate(precip = precip / 254) %>%
  # rename variables to lower case
  rename_all(tolower) 

# change the date formats to match 
noaa$date <- ymd(noaa$date)
noaa_precip$date <- ymd(noaa_precip$date)
a_death_count$death_date <- ymd(a_death_count$death_date)

# restrict precip years to 1976-2022
noaa_precip <- noaa_precip %>%
  filter(year(date) < 2023 & year(date) > 1975)

# merge noaa with adult data using date
a_merged <- merge(a_death_count, noaa, by.x = "death_date",
                  by.y = "date", all = TRUE) %>%
  # drop 2023
  filter(year < 2023 & year > 1975) %>% 
  dplyr::select(-death_year)

# replace NA death count values as zero b/c it didn't match
a_merged %<>% mutate(death_count = ifelse(is.na(death_count), 0, death_count))

# merge the precip variable
a_merged <- merge(a_merged, noaa_precip, by.x = "death_date",
                  by.y = "date", all = TRUE) %>%
  # drop 2023
  filter(year < 2023 & year > 1975) %>% 
  dplyr::select(-year)

# # bin tmin and tmax data
# bins <- seq(-10, 110, by = 10)
# a_merged %<>% mutate(tmin_f_bin = cut(tmin_f, breaks = bins),
#                      tmax_f_bin = cut(tmax_f, breaks = bins),
#                      tmin_f_bin = str_sub(tmin_f_bin, start = 2, end = -2),
#                      tmin_f_bin = str_replace_all(tmin_f_bin, ",", "-"),
#                      tmax_f_bin = str_sub(tmax_f_bin, start = 2, end = -2),
#                      tmax_f_bin = str_replace_all(tmax_f_bin, ",", "-"))

# save merged data
write_csv(a_merged, "use_data/noaa_hils_adults.csv")

#2# read in era5 wet bulb and precip data
era5 <- read_excel(file.path(climatepath, "era5_dailyaggs.xlsx")) %>%
  # drop precip variable
  dplyr::select(-precip)

# change the date formats to match
era5$date <- make_date(era5$year, month = 1, day = 1) + days(era5$day - 1)

# merge era5 with adult data using date
a_merged <- merge(a_death_count, era5, by.x = "death_date",
                  by.y = "date", all = TRUE) %>%
  # drop 2023
  filter(year < 2023 & year > 1975) %>% 
  dplyr::select(-death_year)

# replace NA death count values as zero b/c it didn't match
a_merged %<>% mutate(death_count = ifelse(is.na(death_count), 0, death_count))

# merge the precip variable
a_merged <- merge(a_merged, noaa_precip, by.x = "death_date",
                  by.y = "date", all = TRUE) %>%
  # drop 2023
  filter(year < 2023 & year > 1975)

# save merged data
write_csv(a_merged, "use_data/era5_hils_adults.csv")

