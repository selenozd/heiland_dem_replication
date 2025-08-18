
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Table A5
### Author: Selen Ozdogan

## Setup 
rm(list = ls())
gc()

# run the libraries script to load (and install if necessaary) 
# all required packages
source("code/00_libraries.R")


## Read in the use data
use_data <- read_csv("data/use_data.csv")


## Table A5

# initialize an empty list to store regression results 
noaa_list <- list()
era5_list <- list()

# Run negative binomial regression models with season and temp interactions
# and fixed effects

noaa_3d_ma <- glm.nb(death_count ~ tminmean_f_3d + tminmean_f_3d:summer + 
                       tminmean_f_3d:spring + tminmean_f_3d:fall + tmaxmean_f_3d +
                       tmaxmean_f_3d:summer + tmaxmean_f_3d:spring +
                       tmaxmean_f_3d:fall + summer + fall + spring +
                       newyear + july4 + veterans + christmas + thanksgiving + 
                       memorial + presidents + labor + columbus + precip +
                       relevel(factor(decade), ref = 5) +
                       relevel(factor(year), ref = 34),
                     data = use_data)
 
noaa_7d_ma <- glm.nb(death_count ~ tminmean_f_7d + tminmean_f_7d:summer + 
                       tminmean_f_7d:spring + tminmean_f_7d:fall + tmaxmean_f_7d +
                       tmaxmean_f_7d:summer + tmaxmean_f_7d:spring +
                       tmaxmean_f_7d:fall + summer + fall + spring +
                       newyear + july4 + veterans + christmas + thanksgiving + 
                       memorial + presidents + labor + columbus + precip +
                       relevel(factor(decade), ref = 5) +
                       relevel(factor(year), ref = 34),
                     data = use_data)

noaa_10d_ma <- glm.nb(death_count ~ tminmean_f_10d + tminmean_f_10d:summer + 
                        tminmean_f_10d:spring + tminmean_f_10d:fall + 
                        tmaxmean_f_10d + tmaxmean_f_10d:summer + 
                        tmaxmean_f_10d:spring + tmaxmean_f_10d:fall + summer + 
                        fall + spring + newyear + july4 + veterans + christmas +
                        thanksgiving + memorial + presidents + labor + 
                        columbus + precip + relevel(factor(decade), ref = 5) +
                        relevel(factor(year), ref = 34), 
                      data = use_data)

noaa_10d <- glm.nb(death_count ~ tmin_f_10d + tmin_f_10d:summer + 
                     tmin_f_10d:spring + tmin_f_10d:fall + tmax_f_10d + 
                     tmax_f_10d:summer + tmax_f_10d:spring +
                     tmax_f_10d:fall + summer + fall + spring + 
                     newyear + july4 + veterans + christmas + thanksgiving + 
                     memorial + presidents + labor + columbus + precip +
                     relevel(factor(decade), ref = 5) +
                     relevel(factor(year), ref = 34), 
                   data = use_data)

era5_3d_ma <- glm.nb(death_count ~ wetb_maxmaxmean_3d + 
                       wetb_maxmaxmean_3d:summer +
                       wetb_maxmaxmean_3d:spring + wetb_maxmaxmean_3d:fall +
                       wetb_minminmean_3d + wetb_minminmean_3d:summer +
                       wetb_minminmean_3d:spring + wetb_minminmean_3d:fall +
                       summer + spring + fall + newyear + july4 + veterans + 
                       christmas + thanksgiving + memorial + presidents + 
                       labor + columbus + precip + 
                       relevel(factor(decade), ref = 5) +
                       relevel(factor(year), ref = 34),
                     data = use_data)

era5_7d_ma <- glm.nb(death_count ~ wetb_maxmaxmean_7d + 
                       wetb_maxmaxmean_7d:summer +
                       wetb_maxmaxmean_7d:spring + wetb_maxmaxmean_7d:fall +
                       wetb_minminmean_7d + wetb_minminmean_7d:summer +
                       wetb_minminmean_7d:spring + wetb_minminmean_7d:fall +
                       summer + spring + fall + newyear + july4 + veterans + 
                       christmas + thanksgiving + memorial + presidents + 
                       labor + columbus + precip + 
                       relevel(factor(decade), ref = 5) +
                       relevel(factor(year), ref = 34),
                     data = use_data)

era5_10d_ma <- glm.nb(death_count ~ wetb_minminmean_10d + 
                        wetb_minminmean_10d:summer + 
                        wetb_minminmean_10d:spring + wetb_minminmean_10d:fall + 
                        wetb_maxmaxmean_10d + wetb_maxmaxmean_10d:summer + 
                        wetb_maxmaxmean_10d:spring + wetb_maxmaxmean_10d:fall + 
                        summer + spring + fall + newyear + july4 + veterans + 
                        christmas + thanksgiving + memorial + presidents + 
                        labor + columbus + precip + 
                        relevel(factor(decade), ref = 5) +
                        relevel(factor(year), ref = 34), 
                      data = use_data)

era5_10d <- glm.nb(death_count ~ wetb_minmin_10d + wetb_minmin_10d:summer + 
                     wetb_minmin_10d:spring + wetb_minmin_10d:fall + 
                     wetb_maxmax_10d + wetb_maxmax_10d:summer + 
                     wetb_maxmax_10d:spring + wetb_maxmax_10d:fall + 
                     summer + spring + fall + newyear + july4 + veterans + 
                     christmas + thanksgiving + memorial + presidents + labor + 
                     columbus + precip + relevel(factor(decade), ref = 5) +
                     relevel(factor(year), ref = 34), 
                   data = use_data)

# store the results
noaa_list <- list(noaa_3d_ma, noaa_7d_ma, noaa_10d_ma, noaa_10d)
era5_list <- list(era5_3d_ma, era5_7d_ma, era5_10d_ma, era5_10d)

# export regression results

modelsummary(c("Air temperature (NOAA)" = noaa_list, 
               "Wet bulb temperature (ERA5)" = era5_list),
             coef_omit = paste0(
               "relevel|Intercept|newyear|july4|veterans|christmas|",
               "thanksgiving|memorial|presidents|labor|columbus"),
             coef_map = c("tmax_f_10d" = "Maximum temperature",
                          "tmaxmean_f_3d" = "Maximum temperature",
                          "tmaxmean_f_7d" = "Maximum temperature",
                          "tmaxmean_f_10d" = "Maximum temperature",
                          "wetb_maxmax_10d" = "Maximum temperature",
                          "wetb_maxmaxmean_3d" = "Maximum temperature",
                          "wetb_maxmaxmean_7d" = "Maximum temperature",
                          "wetb_maxmaxmean_10d" = "Maximum temperature",
                          "tmin_f_10d" = "Minimum temperature",
                          "tminmean_f_3d" = "Minimum temperature",
                          "tminmean_f_7d" = "Minimum temperature",
                          "tminmean_f_10d" = "Minimum temperature",
                          "wetb_minmin_10d" = "Minimum temperature",
                          "wetb_minminmean_3d" = "Minimum temperature",
                          "wetb_minminmean_7d" = "Minimum temperature",
                          "wetb_minminmean_10d" = "Minimum temperature",
                          "precip" = "Total precipitation",
                          "summer" = "Summer", 
                          "spring" = "Spring", 
                          "fall" = "Fall",
                          "summer:tmax_f_10d" = "Max. temp. x Summer",
                          "spring:tmax_f_10d" = "Max. temp. x Spring",
                          "fall:tmax_f_10d" = "Max. temp. x Fall",
                          "summer:tmaxmean_f_3d" = "Max. temp. x Summer",
                          "summer:tmaxmean_f_7d" = "Max. temp. x Summer",
                          "summer:tmaxmean_f_10d" = "Max. temp. x Summer",
                          "spring:tmaxmean_f_3d" = "Max. temp. x Spring",
                          "spring:tmaxmean_f_7d" = "Max. temp. x Spring",
                          "spring:tmaxmean_f_10d" = "Max. temp. x Spring",
                          "fall:tmaxmean_f_3d" = "Max. temp. x Fall",
                          "fall:tmaxmean_f_7d" = "Max. temp. x Fall",
                          "fall:tmaxmean_f_10d" = "Max. temp. x Fall",
                          "summer:wetb_maxmax_10d" = "Max. temp. x Summer",
                          "spring:wetb_maxmax_10d" = "Max. temp. x Spring",
                          "fall:wetb_maxmax_10d" = "Max. temp. x Fall",
                          "wetb_maxmaxmean_3d:summer" = "Max. temp. x Summer",
                          "wetb_maxmaxmean_7d:summer" = "Max. temp. x Summer",
                          "summer:wetb_maxmaxmean_10d" = "Max. temp. x Summer",
                          "wetb_maxmaxmean_3d:spring" = "Max. temp. x Spring",
                          "wetb_maxmaxmean_7d:spring" = "Max. temp. x Spring",
                          "spring:wetb_maxmaxmean_10d" = "Max. temp. x Spring",
                          "wetb_maxmaxmean_3d:fall" = "Max. temp. x Fall",
                          "wetb_maxmaxmean_7d:fall" = "Max. temp. x Fall",
                          "fall:wetb_maxmaxmean_10d" = "Max. temp. x Fall",
                          "tmin_f_10d:summer" = "Min. temp. x Summer",
                          "tmin_f_10d:spring" = "Min. temp. x Spring",
                          "tmin_f_10d:fall" = "Min. temp. x Fall",
                          "tminmean_f_3d:summer" = "Min. temp. x Summer",
                          "tminmean_f_7d:summer" = "Min. temp. x Summer",
                          "tminmean_f_10d:summer" = "Min. temp. x Summer",
                          "tminmean_f_3d:spring" = "Min. temp. x Spring",
                          "tminmean_f_7d:spring" = "Min. temp. x Spring",
                          "tminmean_f_10d:spring" = "Min. temp. x Spring",
                          "tminmean_f_3d:fall" = "Min. temp. x Fall",
                          "tminmean_f_7d:fall" = "Min. temp. x Fall",
                          "tminmean_f_10d:fall" = "Min. temp. x Fall",
                          "wetb_minmin_10d:summer" = "Min. temp. x Summer",
                          "wetb_minmin_10d:spring" = "Min. temp. x Spring",
                          "wetb_minmin_10d:fall" = "Min. temp. x Fall",
                          "summer:wetb_minminmean_3d" = "Min. temp. x Summer",
                          "summer:wetb_minminmean_7d" = "Min. temp. x Summer",
                          "wetb_minminmean_10d:summer" = "Min. temp. x Summer",
                          "spring:wetb_minminmean_3d" = "Min. temp. x Spring",
                          "spring:wetb_minminmean_7d" = "Min. temp. x Spring",
                          "wetb_minminmean_10d:spring" = "Min. temp. x Spring",
                          "fall:wetb_minminmean_3d" = "Min. temp. x Fall",
                          "wetb_minminmean_10d:fall" = "Min. temp. x Fall",
                          "fall:wetb_minminmean_7d" = "Min. temp. x Fall"),
             vcov = "HC3",
             gof_omit = "^(?!.*Num)",
             stars = TRUE,
             output = "output/appendix/tablea5.docx")
