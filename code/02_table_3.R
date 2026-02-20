
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Table 3
### Authors: Selen Ozdogan

## Setup 
rm(list = ls())
gc()

# run the libraries script to load (and install if necessary) 
# all required packages
source("code/00_libraries.R")


## Read in the use data
use_data <- read_csv("data/use_data.csv")


## Prepare data to estimate heat-exacerbated deaths (following NYC Dept. of
## Health and Mental Hygiene methodology)
table3_df <- use_data %>%
  # create temp variables in celsius
  mutate(tmax = (tmax_f - 32) * 5/9,
         tmin = (tmin_f - 32) * 5/9,
         # create relative humidity variable and calculate heat index
         e1 = (17.625*tmin) / (243.04-tmin),
         e2 = (17.625*tmax)/ (243.04+tmax),
         rh = 100 * ((10^e1)/(10^e2)),
         rh = ifelse(rh>100, 100, rh),
         # create heat index variable using maximum temperature and rh
         hi_max = -42.379 + 2.04901523*tmax_f + 10.14333127*rh -
           0.22475541*tmax_f*rh - .00683783*tmax_f^2 -
           0.05481717*rh^2 + .00122874*tmax_f^2*rh +
           0.00085282*tmax_f*rh^2 - 
           0.00000199*tmax_f^2*rh^2,
         # heat index is only for values > 80 and rh < 40, adjust
         hi = ifelse(tmax_f < 80 | rh < 40, tmax_f, hi_max),
         # hi values above 130 are not real
         hi = ifelse(hi > 130, 130, hi),
         # always use highest temperature value
         hi = ifelse(tmax_f > hi, tmax_f, hi),
         # create lagged hi variable
         hi_lag1 = lag(hi),
         # identify 2 day extreme heat days
         d2_above_95 = ifelse(hi > 95 & hi_lag1 > 95, 1, 0),
         # identify days over 100 extreme heat days
         d2_above_95 = ifelse(hi > 100, 1, d2_above_95),
         # calculate extreme heat event day: any day during or in the 
         # three days following extreme heat) 
         ehe = ifelse(d2_above_95 == 1, 1, 0),
         ehe = ifelse(lag(d2_above_95, 1) == 1, 1, ehe),
         ehe = ifelse(lag(d2_above_95, 2) == 1, 1, ehe),
         ehe = ifelse(lag(d2_above_95, 3) == 1, 1, ehe)) %>%
  # limit the data to may-september to match dohmh methodology
  filter( month %in% c(5,6,7,8,9))


## Run negative binomial regression models 

# initialize an empty list to store the regression models
t3_list <- list()

# run the regressions
model1 <- glm.nb(death_count ~ ehe + precip_in, 
                 data = table3_df)

model2 <- glm.nb(death_count ~ ehe + precip_in + factor(decade), 
                 data = table3_df)

model3 <- glm.nb(death_count ~ ehe + precip_in + 
                   factor(decade) + factor(year), 
                 data = table3_df)

model4 <- glm.nb(death_count ~ ehe + precip_in + 
                   factor(decade) + factor(year) + factor(month),
                 data = table3_df)

model5 <- glm.nb(death_count ~ ehe + precip_in + 
                   factor(decade) + factor(year) + factor(month) +
                   + newyear + july4 + veterans + christmas + thanksgiving + 
                   memorial + presidents + labor + columbus,
                 data = table3_df)

# store results in a list
t3_list <- list("Model 1" = model1, 
                "Model 2" = model2, 
                "Model 3" = model3, 
                "Model 4" = model4,
                "Model 5" = model5)

# add table notes
fe_rows <- tibble::tribble(
  ~x, ~model1, ~model2, ~model3, ~model4, ~model5,
  "Decade FE", "No", "Yes", "Yes", "Yes", "Yes",
  "Year FE", "No", "No", "Yes", "Yes", "Yes",
  "Month FE", "No", "No", "No", "Yes", "Yes",
  "Holiday FE", "No", "No", "No", "No", "Yes"
)

attr(fe_rows, "position") <- c(3, 4, 5, 6)

# export the regression output as table 3
modelsummary(t3_list,
             coef_omit = paste0(
               "factor|Intercept|newyear|july4|veterans|christmas|",
               "thanksgiving|memorial|presidents|labor|columbus|precip"),
             vcov = "HC3",
             gof_omit = "^(?!.*Num)", 
             gof_map = c("nobs"),
             stars = TRUE,
             add_rows = fe_rows,
             output = "output/table3.docx")
