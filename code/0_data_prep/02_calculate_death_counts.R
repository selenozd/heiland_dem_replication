
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Calculate Daily Death Counts Using the Hart Island Look-up Service Data
### Author: Selen Ozdogan

### Note: To protect data privacy, we share only the variables used in our 
### analysis and have removed all personal identifiers (including names). 
### To clean the raw scraped data, we dropped observations with ages below 1 
### and 110 or above. We also dropped duplicate records and recoded values as
### missing when they indicated missing information. See the commented out code 
### chunk at the end of this script for our procedure to impute missing 
### sex information from first names in the original data.

## Setup 
rm(list = ls())
gc()

# Run the libraries script to load (and install if necessary) 
# all required packages
source("code/00_libraries.R")

# Read in the cleaned adult death records data
hils <- read.csv("data/raw_data/death_records.csv") %>%
  # Calculate death year, month, day and week
  mutate(death_year = year(death_date),
         death_month = month(death_date),
         death_day = day(death_date),
         death_week = week(death_date)) %>%
  # Keep years 1976-2022
  filter(death_year < 2023 & death_year > 1975) %>%
  # Create a categorical age variable
  mutate(age_grp = ifelse((age < 65), "under65", "over65"),
         age_grp = ifelse(is.na(age), "unknown", age_grp))
  
# Calculate daily death counts (all) and then by sub-populations
a_death_count <- hils %>%
  group_by(death_date) %>%
  summarize(
    # Total
    death_count = n(),
    # By sex
    female_death_count = sum(imputed_sex == "female", na.rm = TRUE),
    male_death_count = sum(imputed_sex == "male", na.rm = TRUE),
    unknown_sex_death_count = sum(imputed_sex == "unknown", na.rm = TRUE),
    # By age group
    under65_death_count = sum(age_grp == "under65", na.rm = TRUE),
    over65_death_count = sum(age_grp == "over65", na.rm = TRUE),
    unknown_age_death_count = sum(age_grp == "unknown", na.rm = TRUE),
    .groups = 'drop'
  )

# Check the totals
sum(a_death_count$death_count)
sum(a_death_count$female_death_count) + sum(a_death_count$male_death_count) +
  sum(a_death_count$unknown_sex_death_count)
sum(a_death_count$under65_death_count) + sum(a_death_count$over65_death_count) +
  sum(a_death_count$unknown_age_death_count)
nrow(hils) # matches, great!

# Save
write_csv(a_death_count, "data/death_counts.csv")


### Impute sex based on first names ############################################

# imputed sex: Possible values are "male" and "female" for proportion above 0.5, 
# "either" for proportions that are exactly 0.5, and NA for combinations of 
# names and years for which sex cannot be predicted using the given method.

# # impute gender based on first name
# names <- gender(
#   hils_raw$first_name,
#   years = c(1977, 2022),
#   method = c("ssa", "ipums", "napp", "kantrowitz", "genderize", "demo"),
#   countries = c("United States")
# )

# # clean the names data
# names <- names %>%
#   # drop duplicates
#   distinct(name, .keep_all = TRUE) %>%
#   # keep necessary variables
#   dplyr::select(name, gender) %>%
#   # rename variables
#   dplyr::rename(first_name = name, imputed_sex = gender)

# # merge back with death records  data
# hils_raw <- hils_raw %>%
#   left_join(names, by = "first_name")

# # flag when imputed sex is different from the recorded (except unknowns)
# hils_raw %<>% 
#   mutate(gender_mismatch = case_when(
#     imputed_sex != sex & 
#       imputed_sex != "unknown" & 
#       sex != "unknown" ~ 1,
#     TRUE ~ 0
#   ))
# 
# table(hils_raw$gender_mismatch) # only 1.3 percent mismatch. great!

# # generate imputed sex = sex if recorded, = imputed if sex is unknown
# hils_raw %<>%
#   mutate(imputed_sex = case_when(
#     sex == "unknown" ~ imputed_sex,
#     TRUE ~ sex
#   ))
################################################################################
