
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Tables A3 and A4
### Author: Selen Ozdogan

## Setup 
rm(list = ls())
gc()

# run the libraries script to load (and install if necessaary) 
# all required packages
source("code/00_libraries.R")


## Read in the use data
use_data <- read_csv("data/use_data.csv")


## Table A3

# initialize an empty list to store the regression models
noaa_under65_list <- list()
noaa_over65_list <- list()

# run negative binomial regression models with season and temp interactions
# and fixed effects
noaa_1d_under65 <- glm.nb(under65_death_count ~ tmin_f + tmin_f:summer +
                         tmin_f:spring + tmin_f:fall + tmax_f + tmax_f:summer +
                         tmax_f:spring + tmax_f:fall + summer + fall + spring + 
                         newyear + july4 + veterans + christmas + thanksgiving + 
                         memorial + presidents + labor + columbus + precip +
                         relevel(factor(decade), ref = 5) +
                         relevel(factor(year), ref = 34),
                       data = use_data)
  
noaa_3d_under65 <- glm.nb(under65_death_count ~ tmin_f_3d + tmin_f_3d:summer + 
                         tmin_f_3d:spring + tmin_f_3d:fall + tmax_f_3d + 
                         tmax_f_3d:summer + tmax_f_3d:spring +
                         tmax_f_3d:fall + summer + fall + spring +
                         newyear + july4 + veterans + christmas + thanksgiving + 
                         memorial + presidents + labor + columbus + precip +
                         relevel(factor(decade), ref = 5) +
                         relevel(factor(year), ref = 34),
                       data = use_data)
  
noaa_7d_under65 <- glm.nb(under65_death_count ~ tmin_f_7d + tmin_f_7d:summer + 
                         tmin_f_7d:spring + tmin_f_7d:fall + tmax_f_7d + 
                         tmax_f_7d:summer + tmax_f_7d:spring +
                         tmax_f_7d:fall + summer + fall + spring + 
                         newyear + july4 + veterans + christmas + thanksgiving + 
                         precip + relevel(factor(decade), ref = 5) +
                         relevel(factor(year), ref = 34),
                       data = use_data)

noaa_1d_over65 <- glm.nb(over65_death_count ~ tmin_f + tmin_f:summer +
                         tmin_f:spring + tmin_f:fall + tmax_f + tmax_f:summer +
                         tmax_f:spring + tmax_f:fall + summer + fall + spring + 
                         newyear + july4 + veterans + christmas + thanksgiving + 
                         memorial + presidents + labor + columbus + precip +
                         relevel(factor(decade), ref = 5) +
                         relevel(factor(year), ref = 34),
                       data = use_data)

noaa_3d_over65 <- glm.nb(over65_death_count ~ tmin_f_3d + tmin_f_3d:summer + 
                         tmin_f_3d:spring + tmin_f_3d:fall + tmax_f_3d + 
                         tmax_f_3d:summer + tmax_f_3d:spring +
                         tmax_f_3d:fall + summer + fall + spring +
                         newyear + july4 + veterans + christmas + thanksgiving + 
                         memorial + presidents + labor + columbus + precip +
                         relevel(factor(decade), ref = 5) +
                         relevel(factor(year), ref = 34),
                       data = use_data)

noaa_7d_over65 <- glm.nb(over65_death_count ~ tmin_f_7d + tmin_f_7d:summer + 
                         tmin_f_7d:spring + tmin_f_7d:fall + tmax_f_7d + 
                         tmax_f_7d:summer + tmax_f_7d:spring +
                         tmax_f_7d:fall + summer + fall + spring + 
                         newyear + july4 + veterans + christmas + thanksgiving + 
                         precip + relevel(factor(decade), ref = 5) +
                         relevel(factor(year), ref = 34),
                       data = use_data)
  
# store the results
noaa_under65_list <- list(noaa_1d_under65, noaa_3d_under65, noaa_7d_under65)
noaa_over65_list <- list(noaa_1d_over65, noaa_3d_over65, noaa_7d_over65)
 
# export regression results
modelsummary(c("65 and over" = noaa_over65_list,
               "Under 65" = noaa_under65_list),
             coef_omit = paste0(
               "relevel|Intercept|newyear|july4|veterans|christmas|",
               "thanksgiving|memorial|presidents|labor|columbus"),
             coef_map = c("tmax_f" = "Maximum temperature",
                          "tmax_f_3d" = "Maximum temperature",
                          "tmax_f_7d" = "Maximum temperature",
                          "wetb_maxmax" = "Maximum temperature",
                          "wetb_maxmax_3d" = "Maximum temperature",
                          "wetb_maxmax_7d" = "Maximum temperature",
                          "tmin_f" = "Minimum temperature",
                          "tmin_f_3d" = "Minimum temperature",
                          "tmin_f_7d" = "Minimum temperature",
                          "wetb_minmin" = "Minimum temperature",
                          "wetb_minmin_3d" = "Minimum temperature",
                          "wetb_minmin_7d" = "Minimum temperature",
                          "precip" = "Total precipitation",
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
                          "summer:wetb_maxmax" = "Max. temp. x Summer",
                          "spring:wetb_maxmax" = "Max. temp. x Spring",
                          "fall:wetb_maxmax" = "Max. temp. x Fall",
                          "summer:wetb_maxmax_3d" = "Max. temp. x Summer",
                          "summer:wetb_maxmax_7d" = "Max. temp. x Summer",
                          "spring:wetb_maxmax_3d" = "Max. temp. x Spring",
                          "spring:wetb_maxmax_7d" = "Max. temp. x Spring",
                          "fall:wetb_maxmax_3d" = "Max. temp. x Fall",
                          "fall:wetb_maxmax_7d" = "Max. temp. x Fall",
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
             output = "output/appendix/tablea3.docx")

## Table A4

# initialize an empty list to store regression results 
era5_under65_list <- list()
era5_over65_list <- list()

# run negative binomial regression models with season and temp interactions
# and fixed effects
era5_under65_1d <- glm.nb(under65_death_count ~ wetb_minmin + wetb_minmin:summer + 
                         wetb_minmin:spring + wetb_minmin:fall + wetb_maxmax + 
                         wetb_maxmax:summer + wetb_maxmax:spring + 
                         wetb_maxmax:fall + summer + spring + fall + 
                         newyear + july4 + veterans + christmas + thanksgiving + 
                         memorial + presidents + labor + columbus + 
                         precip + relevel(factor(decade), ref = 5) +
                         relevel(factor(year), ref = 34), 
                       data = use_data)
  
era5_under65_3d <- glm.nb(under65_death_count ~ wetb_minmin_3d + wetb_minmin_3d:summer + 
                         wetb_minmin_3d:spring + wetb_minmin_3d:fall + 
                         wetb_maxmax_3d + wetb_maxmax_3d:summer + 
                         wetb_maxmax_3d:spring + wetb_maxmax_3d:fall + summer + 
                         spring + fall + newyear + july4 + veterans + christmas + 
                         thanksgiving + memorial + presidents + labor + columbus + 
                         precip + relevel(factor(decade), ref = 5) +
                         relevel(factor(year), ref = 34), 
                       data = use_data)
  
era5_under65_7d <- glm.nb(under65_death_count ~ wetb_minmin_7d + wetb_minmin_7d:summer + 
                         wetb_minmin_7d:spring + wetb_minmin_7d:fall + 
                         wetb_maxmax_7d + wetb_maxmax_7d:summer + 
                         wetb_maxmax_7d:spring + wetb_maxmax_7d:fall + summer + 
                         spring + fall + newyear + july4 + veterans + christmas + 
                         thanksgiving + memorial + presidents + labor + columbus + 
                         precip + relevel(factor(decade), ref = 5) +
                         relevel(factor(year), ref = 34), 
                       data = use_data)

era5_over65_1d <- glm.nb(over65_death_count ~ wetb_minmin + wetb_minmin:summer + 
                         wetb_minmin:spring + wetb_minmin:fall + wetb_maxmax + 
                         wetb_maxmax:summer + wetb_maxmax:spring + 
                         wetb_maxmax:fall + summer + spring + fall + 
                         newyear + july4 + veterans + christmas + thanksgiving + 
                         memorial + presidents + labor + columbus + 
                         precip + relevel(factor(decade), ref = 5) +
                         relevel(factor(year), ref = 34), 
                       data = use_data)

era5_over65_3d <- glm.nb(over65_death_count ~ wetb_minmin_3d + wetb_minmin_3d:summer + 
                         wetb_minmin_3d:spring + wetb_minmin_3d:fall + 
                         wetb_maxmax_3d + wetb_maxmax_3d:summer + 
                         wetb_maxmax_3d:spring + wetb_maxmax_3d:fall + summer + 
                         spring + fall + newyear + july4 + veterans + christmas + 
                         thanksgiving + memorial + presidents + labor + columbus + 
                         precip + relevel(factor(decade), ref = 5) +
                         relevel(factor(year), ref = 34), 
                       data = use_data)

era5_over65_7d <- glm.nb(over65_death_count ~ wetb_minmin_7d + wetb_minmin_7d:summer + 
                         wetb_minmin_7d:spring + wetb_minmin_7d:fall + 
                         wetb_maxmax_7d + wetb_maxmax_7d:summer + 
                         wetb_maxmax_7d:spring + wetb_maxmax_7d:fall + summer + 
                         spring + fall + newyear + july4 + veterans + christmas + 
                         thanksgiving + memorial + presidents + labor + columbus + 
                         precip + relevel(factor(decade), ref = 5) +
                         relevel(factor(year), ref = 34), 
                       data = use_data)
  
# store the results
era5_under65_list <- list(era5_under65_1d, era5_under65_3d, era5_under65_7d)
era5_over65_list <- list(era5_over65_1d, era5_over65_3d, era5_over65_7d)

# export regression results
modelsummary(c("65 and over" = era5_over65_list, 
               "Under 65" = era5_under65_list),
             coef_omit = paste0(
               "relevel|Intercept|newyear|july4|veterans|christmas|",
               "thanksgiving|memorial|presidents|labor|columbus"),
             coef_map = c("tmax_f" = "Maximum temperature",
                          "tmax_f_3d" = "Maximum temperature",
                          "tmax_f_7d" = "Maximum temperature",
                          "wetb_maxmax" = "Maximum temperature",
                          "wetb_maxmax_3d" = "Maximum temperature",
                          "wetb_maxmax_7d" = "Maximum temperature",
                          "tmin_f" = "Minimum temperature",
                          "tmin_f_3d" = "Minimum temperature",
                          "tmin_f_7d" = "Minimum temperature",
                          "wetb_minmin" = "Minimum temperature",
                          "wetb_minmin_3d" = "Minimum temperature",
                          "wetb_minmin_7d" = "Minimum temperature",
                          "precip" = "Total precipitation",
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
                          "summer:wetb_maxmax" = "Max. temp. x Summer",
                          "spring:wetb_maxmax" = "Max. temp. x Spring",
                          "fall:wetb_maxmax" = "Max. temp. x Fall",
                          "summer:wetb_maxmax_3d" = "Max. temp. x Summer",
                          "summer:wetb_maxmax_7d" = "Max. temp. x Summer",
                          "spring:wetb_maxmax_3d" = "Max. temp. x Spring",
                          "spring:wetb_maxmax_7d" = "Max. temp. x Spring",
                          "fall:wetb_maxmax_3d" = "Max. temp. x Fall",
                          "fall:wetb_maxmax_7d" = "Max. temp. x Fall",
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
             output = "output/appendix/tablea4.docx")

