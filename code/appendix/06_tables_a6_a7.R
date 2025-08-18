
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Tables A6 and A7
### Author: Selen Ozdogan

## Setup 
rm(list = ls())
gc()

# run the libraries script to load (and install if necessaary) 
# all required packages
source("code/00_libraries.R")


## Read in the use data
use_data <- read_csv("data/use_data.csv")


## Table A6

# initialize an empty list to store the regression models
noaa_tmax_list <- list()
era5_tmax_list <- list()

# run negative binomial regression models with season and temp interactions
# and fixed effects
noaa_tmax_1d <- glm.nb(death_count ~ tmax_f + tmax_f:summer + tmax_f:spring +
                    tmax_f:fall + summer + fall + spring + newyear + july4 + 
                    veterans + christmas + thanksgiving + memorial + 
                    presidents + labor + columbus + precip + 
                    relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

noaa_tmax_3d <- glm.nb(death_count ~ tmax_f_3d + tmax_f_3d:summer + 
                    tmax_f_3d:spring + tmax_f_3d:fall + 
                    summer + fall + spring + newyear + july4 + veterans + 
                    christmas + thanksgiving + memorial + presidents + labor + 
                    columbus + precip + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

noaa_tmax_7d <- glm.nb(death_count ~ tmax_f_7d + tmax_f_7d:summer + 
                    tmax_f_7d:spring + tmax_f_7d:fall + summer + fall + spring + 
                    newyear + july4 + veterans + christmas + thanksgiving + 
                    memorial + presidents + labor + columbus + 
                    precip + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

era5_tmax_1d <- glm.nb(death_count ~ wetb_maxmax + wetb_maxmax:summer + 
                    wetb_maxmax:spring + wetb_maxmax:fall + summer + spring + 
                    fall + newyear + july4 + veterans + christmas + 
                    thanksgiving + memorial + presidents + labor + columbus + 
                    precip + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

era5_tmax_3d <- glm.nb(death_count ~ wetb_maxmax_3d + wetb_maxmax_3d:summer + 
                    wetb_maxmax_3d:spring + wetb_maxmax_3d:fall + summer + 
                    spring + fall + newyear + july4 + veterans + christmas + 
                    thanksgiving + memorial + presidents + labor + columbus + 
                    precip + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

era5_tmax_7d <- glm.nb(death_count ~ wetb_maxmax_7d + wetb_maxmax_7d:summer + 
                    wetb_maxmax_7d:spring + wetb_maxmax_7d:fall + summer + 
                    spring + fall + newyear + july4 + veterans + christmas + 
                    thanksgiving + memorial + presidents + labor + columbus + 
                    precip + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

# store regression results in lists
noaa_tmax_list <- list(noaa_tmax_1d, noaa_tmax_3d, noaa_tmax_7d)
era5_tmax_list <- list(era5_tmax_1d, era5_tmax_3d, era5_tmax_7d)

# export the regression output
modelsummary(c("Air temperature (NOAA)" = noaa_tmax_list, 
               "Wet bulb temperature (ERA5)" = era5_tmax_list),
             coef_omit = paste0(
               "relevel|Intercept|newyear|july4|veterans|christmas|",
               "thanksgiving|memorial|presidents|labor|columbus"),
             coef_map = c("tmax_f" = "Maximum temperature",
                          "tmax_f_3d" = "Maximum temperature",
                          "tmax_f_7d" = "Maximum temperature",
                          "wetb_maxmax" = "Maximum temperature",
                          "wetb_maxmax_3d" = "Maximum temperature",
                          "wetb_maxmax_7d" = "Maximum temperature",
                          "precip" = "Total precipitation",
                          "summer" = "Summer", 
                          "spring" = "Spring", 
                          "fall" = "Fall",
                          "tmax_f:summer" = "Max. temp. x Summer",
                          "tmax_f:spring" = "Max. temp. x Spring",
                          "tmax_f:fall" = "Max. temp. x Fall",
                          "tmax_f_3d:summer" = "Max. temp. x Summer",
                          "tmax_f_7d:summer" = "Max. temp. x Summer",
                          "tmax_f_3d:spring" = "Max. temp. x Spring",
                          "tmax_f_7d:spring" = "Max. temp. x Spring",
                          "tmax_f_3d:fall" = "Max. temp. x Fall",
                          "tmax_f_7d:fall" = "Max. temp. x Fall",
                          "wetb_maxmax:summer" = "Max. temp. x Summer",
                          "wetb_maxmax:spring" = "Max. temp. x Spring",
                          "wetb_maxmax:fall" = "Max. temp. x Fall",
                          "wetb_maxmax_3d:summer" = "Max. temp. x Summer",
                          "wetb_maxmax_7d:summer" = "Max. temp. x Summer",
                          "wetb_maxmax_3d:spring" = "Max. temp. x Spring",
                          "wetb_maxmax_7d:spring" = "Max. temp. x Spring",
                          "wetb_maxmax_3d:fall" = "Max. temp. x Fall",
                          "wetb_maxmax_7d:fall" = "Max. temp. x Fall"),
             vcov = "HC3",
             gof_omit = "^(?!.*Num)",
             stars = TRUE,
             output = "output/appendix/tablea6.docx")


## Table A7

# initialize an empty list to store the regression models
noaa_tmin_list <- list()
era5_tmin_list <- list()

# run negative binomial regression models with season and temp interactions
# and fixed effects
noaa_tmin_1d <- glm.nb(death_count ~ tmin_f + tmin_f:summer + tmin_f:spring +
                    tmin_f:fall + summer + fall + spring + newyear + july4 + 
                    veterans + christmas + thanksgiving + memorial + 
                    presidents + labor + columbus + precip + 
                    relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

noaa_tmin_3d <- glm.nb(death_count ~ tmin_f_3d + tmin_f_3d:summer + 
                    tmin_f_3d:spring + tmin_f_3d:fall + 
                    summer + fall + spring + newyear + july4 + veterans + 
                    christmas + thanksgiving + memorial + presidents + labor + 
                    columbus + precip + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

noaa_tmin_7d <- glm.nb(death_count ~ tmin_f_7d + tmin_f_7d:summer + 
                    tmin_f_7d:spring + tmin_f_7d:fall + summer + fall + spring + 
                    newyear + july4 + veterans + christmas + thanksgiving + 
                    memorial + presidents + labor + columbus + 
                    precip + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

era5_tmin_1d <- glm.nb(death_count ~ wetb_minmin + wetb_minmin:summer + 
                    wetb_minmin:spring + wetb_minmin:fall + summer + spring + 
                    fall + newyear + july4 + veterans + christmas + 
                    thanksgiving + memorial + presidents + labor + columbus + 
                    precip + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

era5_tmin_3d <- glm.nb(death_count ~ wetb_minmin_3d + wetb_minmin_3d:summer + 
                    wetb_minmin_3d:spring + wetb_minmin_3d:fall + summer + 
                    spring + fall + newyear + july4 + veterans + christmas + 
                    thanksgiving + memorial + presidents + labor + columbus + 
                    precip + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

era5_tmin_7d <- glm.nb(death_count ~ wetb_minmin_7d + wetb_minmin_7d:summer + 
                    wetb_minmin_7d:spring + wetb_minmin_7d:fall + summer + 
                    spring + fall + newyear + july4 + veterans + christmas + 
                    thanksgiving + memorial + presidents + labor + columbus + 
                    precip + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

# store regression results in lists
noaa_tmin_list <- list(noaa_tmin_1d, noaa_tmin_3d, noaa_tmin_7d)
era5_tmin_list <- list(era5_tmin_1d, era5_tmin_3d, era5_tmin_7d)

# export the regression output
modelsummary(c("Air temperature (NOAA)" = noaa_tmin_list, 
               "Wet bulb temperature (ERA5)" = era5_tmin_list),
             coef_omit = paste0(
               "relevel|Intercept|newyear|july4|veterans|christmas|",
               "thanksgiving|memorial|presidents|labor|columbus"),
             coef_map = c("tmin_f" = "Minimum temperature",
                          "tmin_f_3d" = "Minimum temperature",
                          "tmin_f_7d" = "Minimum temperature",
                          "wetb_minmin" = "Minimum temperature",
                          "wetb_minmin_3d" = "Minimum temperature",
                          "wetb_minmin_7d" = "Minimum temperature",
                          "precip" = "Total precipitation",
                          "summer" = "Summer", 
                          "spring" = "Spring", 
                          "fall" = "Fall",
                          "tmin_f:summer" = "Min. temp. x Summer",
                          "tmin_f:spring" = "Min. temp. x Spring",
                          "tmin_f:fall" = "Min. temp. x Fall",
                          "tmin_f_3d:summer" = "Min. temp. x Summer",
                          "tmin_f_7d:summer" = "Min. temp. x Summer",
                          "tmin_f_3d:spring" = "Min. temp. x Spring",
                          "tmin_f_7d:spring" = "Min. temp. x Spring",
                          "tmin_f_3d:fall" = "Min. temp. x Fall",
                          "tmin_f_7d:fall" = "Min. temp. x Fall",
                          "wetb_minmin:summer" = "Min. temp. x Summer",
                          "wetb_minmin:spring" = "Min. temp. x Spring",
                          "wetb_minmin:fall" = "Min. temp. x Fall",
                          "wetb_minmin_3d:summer" = "Min. temp. x Summer",
                          "wetb_minmin_7d:summer" = "Min. temp. x Summer",
                          "wetb_minmin_3d:spring" = "Min. temp. x Spring",
                          "wetb_minmin_7d:spring" = "Min. temp. x Spring",
                          "wetb_minmin_3d:fall" = "Min. temp. x Fall",
                          "wetb_minmin_7d:fall" = "Min. temp. x Fall"),
             vcov = "HC3",
             gof_omit = "^(?!.*Num)",
             stars = TRUE,
             output = "output/appendix/tablea7.docx")
