
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

# 1 # Read in death counts and climate data
a_death_count <- read_csv("data/death_counts.csv")
climate_data <- read_csv("data/nyc_1976_2022_climate_data.csv") %>%
  # drop row names column
  dplyr::select(-1)


# 2 # Merge the two using date
use_data <- merge(a_death_count, climate_data, by.x = "death_date",
                  by.y = "date", all = TRUE) %>%
  # Replace NA death count values as zero b/c it didn't match with death records
  mutate(death_count = ifelse(is.na(death_count), 0, death_count),
         female_death_count = ifelse(is.na(female_death_count), 0, 
                                     female_death_count),
         male_death_count = ifelse(is.na(male_death_count), 0, 
                                     male_death_count),
         unknown_sex_death_count = ifelse(is.na(unknown_sex_death_count), 0, 
                                          unknown_sex_death_count),
         under65_death_count = ifelse(is.na(under65_death_count), 0, 
                                        under65_death_count),
         over65_death_count = ifelse(is.na(over65_death_count), 0,
                                       over65_death_count),
         unknown_age_death_count = ifelse(is.na(unknown_age_death_count), 0,
                                          unknown_age_death_count)) %>%
  # Keep 1977-2022
  filter(year >= 1977 & year <= 2022)

# Again, check death counts to be sure
sum(use_data$death_count)
sum(use_data$female_death_count) + sum(use_data$male_death_count) +
  sum(use_data$unknown_sex_death_count)
sum(use_data$under65_death_count) + sum(use_data$over65_death_count) +
  sum(use_data$unknown_age_death_count)

# Create some extra time variables
use_data <- use_data %>%
  mutate(month = month(death_date),
         decade = as.numeric(substring(year, 1, 3)) * 10,
         day_of_week = weekdays(death_date))
         
# Create dummies for bank holidays that are always the same date
use_data <- use_data %>%
  mutate(newyear = ifelse(month == 1 & day == 1, 1, 0),
         july4 = ifelse(month == 7 & day == 4, 1, 0),
         veterans = ifelse(month == 11 & day == 11, 1, 0),
         christmas = ifelse(month == 12 & day == 25, 1, 0),
         # Now, create dummies for holidays that change date
         thanksgiving = as.integer(month == 11 & 
                                     day >= 22 &
                                     day <= 28 &
                                     day_of_week == "Thursday"),
         memorial = as.integer(month == 5 &
                                 day >= 25 &
                                 day_of_week == "Monday"),
         presidents = as.integer(month == 2 &
                                   day >= 15 &
                                   day <= 21 &
                                   day_of_week == "Monday"),
         labor = as.integer(month == 9 &
                              day <= 7 &
                              day_of_week == "Monday"),
         columbus = as.integer(month == 10 &
                                 day >= 8 &
                                 day <= 14 &
                                 day_of_week == "Monday"),
         # Create dummies for 4 seasons
         spring = ifelse(month %in% c(3, 4, 5), 1, 0),
         summer = ifelse(month %in% c(6, 7, 8), 1, 0),
         fall = ifelse(month %in% c(9, 10, 11), 1, 0),
         winter = ifelse(month %in% c(12, 1, 2), 1, 0),
         season = case_when(
           summer == 1 ~ "Summer",
           winter == 1 ~ "Winter",
           fall == 1 ~ "Fall",
           spring == 1 ~ "Spring"))
    
# Save use data
write_csv(use_data, "data/use_data.csv")

