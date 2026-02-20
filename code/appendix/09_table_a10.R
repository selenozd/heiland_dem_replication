
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Table A10
### Author: Selen Ozdogan

## Setup 
rm(list = ls())
gc()

# run the libraries script to load (and install if necessary) 
# all required packages
source("code/00_libraries.R")


## Read in the use data
use_data <- read_csv("data/use_data.csv") %>%
  # keep only 1977-2019
  filter(year <= 2019)


# initialize an empty list to store the regression models
noaa_list <- list()
era5_list <- list()

# run negative binomial regression models with season and temp interactions
# and fixed effects
noaa_1d <- glm.nb(death_count ~ tmin_f + tmin_f:summer + tmin_f:spring +
                    tmin_f:fall + tmax_f + tmax_f:summer + tmax_f:spring +
                    tmax_f:fall + summer + fall + spring + newyear + july4 + 
                    veterans + christmas + thanksgiving + memorial + 
                    presidents + labor + columbus + precip_in + 
                    relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

noaa_3d <- glm.nb(death_count ~ tmin_f_3d + tmin_f_3d:summer + 
                    tmin_f_3d:spring + tmin_f_3d:fall + tmax_f_3d + 
                    tmax_f_3d:summer + tmax_f_3d:spring + tmax_f_3d:fall + 
                    summer + fall + spring + newyear + july4 + veterans + 
                    christmas + thanksgiving + memorial + presidents + labor + 
                    columbus + precip_in + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

noaa_7d <- glm.nb(death_count ~ tmin_f_7d + tmin_f_7d:summer + 
                    tmin_f_7d:spring + tmin_f_7d:fall + tmax_f_7d + 
                    tmax_f_7d:summer + tmax_f_7d:spring +
                    tmax_f_7d:fall + summer + fall + spring + 
                    newyear + july4 + veterans + christmas + thanksgiving + 
                    memorial + presidents + labor + columbus + 
                    precip_in + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

era5_1d <- glm.nb(death_count ~ wetb_min_citymin + wetb_min_citymin:summer + 
                    wetb_min_citymin:spring + wetb_min_citymin:fall + 
                    wetb_max_citymax + wetb_max_citymax:summer + 
                    wetb_max_citymax:spring + wetb_max_citymax:fall + 
                    summer + spring + fall + newyear + july4 + veterans + 
                    christmas + thanksgiving + memorial + presidents + labor + 
                    columbus + precip_in + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

era5_3d <- glm.nb(death_count ~ wetb_min_citymin_3d + 
                    wetb_min_citymin_3d:summer + wetb_min_citymin_3d:spring + 
                    wetb_min_citymin_3d:fall + wetb_max_citymax_3d + 
                    wetb_max_citymax_3d:summer + wetb_max_citymax_3d:spring + 
                    wetb_max_citymax_3d:fall + summer + spring + fall + 
                    newyear + july4 + veterans + christmas + 
                    thanksgiving + memorial + presidents + labor + columbus + 
                    precip_in + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

era5_7d <- glm.nb(death_count ~ wetb_min_citymin_7d + 
                    wetb_min_citymin_7d:summer + wetb_min_citymin_7d:spring + 
                    wetb_min_citymin_7d:fall + wetb_max_citymax_7d + 
                    wetb_max_citymax_7d:summer + wetb_max_citymax_7d:spring + 
                    wetb_max_citymax_7d:fall + summer + spring + fall + 
                    newyear + july4 + veterans + christmas + 
                    thanksgiving + memorial + presidents + labor + columbus + 
                    precip_in + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

# store regression results in lists
noaa_list <- list(noaa_1d, noaa_3d, noaa_7d)
era5_list <- list(era5_1d, era5_3d, era5_7d)

# export the regression output
modelsummary(c("Air temperature (NOAA)" = noaa_list, 
               "Wet bulb temperature (ERA5)" = era5_list),
             coef_omit = paste0(
               "relevel|Intercept|newyear|july4|veterans|christmas|",
               "thanksgiving|memorial|presidents|labor|columbus"),
             coef_map = c("tmax_f" = "Maximum temperature",
                          "tmax_f_3d" = "Maximum temperature",
                          "tmax_f_7d" = "Maximum temperature",
                          "wetb_max_citymax" = "Maximum temperature",
                          "wetb_max_citymax_3d" = "Maximum temperature",
                          "wetb_max_citymax_7d" = "Maximum temperature",
                          "tmin_f" = "Minimum temperature",
                          "tmin_f_3d" = "Minimum temperature",
                          "tmin_f_7d" = "Minimum temperature",
                          "wetb_min_citymin" = "Minimum temperature",
                          "wetb_min_citymin_3d" = "Minimum temperature",
                          "wetb_min_citymin_7d" = "Minimum temperature",
                          "precip_in" = "Total precipitation",
                          "summer" = "Summer", 
                          "spring" = "Spring", 
                          "fall" = "Fall",
                          "summer:tmax_f" = "Max. temp. x Summer",
                          "spring:tmax_f" = "Max. temp. x Spring",
                          "fall:tmax_f" = "Max. temp. x Fall",
                          "summer:tmax_f_3d" = "Max. temp. x Summer",
                          "summer:tmax_f_7d" = "Max. temp. x Summer",
                          "spring:tmax_f_3d" = "Max. temp. x Spring",
                          "spring:tmax_f_7d" = "Max. temp. x Spring",
                          "fall:tmax_f_3d" = "Max. temp. x Fall",
                          "fall:tmax_f_7d" = "Max. temp. x Fall",
                          "summer:wetb_max_citymax" = "Max. temp. x Summer",
                          "spring:wetb_max_citymax" = "Max. temp. x Spring",
                          "fall:wetb_max_citymax" = "Max. temp. x Fall",
                          "summer:wetb_max_citymax_3d" = "Max. temp. x Summer",
                          "summer:wetb_max_citymax_7d" = "Max. temp. x Summer",
                          "spring:wetb_max_citymax_3d" = "Max. temp. x Spring",
                          "spring:wetb_max_citymax_7d" = "Max. temp. x Spring",
                          "fall:wetb_max_citymax_3d" = "Max. temp. x Fall",
                          "fall:wetb_max_citymax_7d" = "Max. temp. x Fall",
                          "tmin_f:summer" = "Min. temp. x Summer",
                          "tmin_f:spring" = "Min. temp. x Spring",
                          "tmin_f:fall" = "Min. temp. x Fall",
                          "tmin_f_3d:summer" = "Min. temp. x Summer",
                          "tmin_f_7d:summer" = "Min. temp. x Summer",
                          "tmin_f_3d:spring" = "Min. temp. x Spring",
                          "tmin_f_7d:spring" = "Min. temp. x Spring",
                          "tmin_f_3d:fall" = "Min. temp. x Fall",
                          "tmin_f_7d:fall" = "Min. temp. x Fall",
                          "wetb_min_citymin:summer" = "Min. temp. x Summer",
                          "wetb_min_citymin:spring" = "Min. temp. x Spring",
                          "wetb_min_citymin:fall" = "Min. temp. x Fall",
                          "wetb_min_citymin_3d:summer" = "Min. temp. x Summer",
                          "wetb_min_citymin_7d:summer" = "Min. temp. x Summer",
                          "wetb_min_citymin_3d:spring" = "Min. temp. x Spring",
                          "wetb_min_citymin_7d:spring" = "Min. temp. x Spring",
                          "wetb_min_citymin_3d:fall" = "Min. temp. x Fall",
                          "wetb_min_citymin_7d:fall" = "Min. temp. x Fall"),
             vcov = "HC3",
             gof_omit = "^(?!.*Num)",
             stars = TRUE,
             output = "output/appendix/tablea10.docx")
