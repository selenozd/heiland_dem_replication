
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Table A9
### Author: Selen Ozdogan

## Setup 
rm(list = ls())
gc()

# run the libraries script to load (and install if necessary) 
# all required packages
source("code/00_libraries.R")


## Read in the use data
use_data <- read_csv("data/use_data.csv") %>%
  # add month variable
  mutate(month = month(death_date))

# add month dummies
use_data <- cbind(use_data, 
                   model.matrix(~factor(month) - 1, data = use_data)) %>%
  setNames(c(names(use_data), paste0("month", 1:12)))


## Define function to run negative binomial regression with month fixed effects
run_temp_reg <- function(data, temp_vars, ma = "") {
  
  # construct tmin / tmax &  month fe interaction term strings
  temp_terms <- lapply(temp_vars, function(var) {
    base_var <- if (ma == "") var else paste0(var, "_", ma, "d")
    c(base_var, paste0(base_var, ":month", c(2:12)))
  })
  
  # combine all temperature terms
  temp_formula <- paste(unlist(temp_terms), collapse = " + ")
  
  # fixed effects and controls
  controls <- paste(c("newyear", "july4", "veterans", 
                      "christmas", "thanksgiving", "memorial", "presidents",
                      "labor", "columbus", "precip_in", "month2",
                      "month3", "month4", "month5", "month6", "month7",
                      "month8", "month9", "month10", "month11", "month12",
                      "relevel(factor(decade), ref = 5)",
                      "relevel(factor(year), ref = 34)",
                      "relevel(factor(month), ref = 1)"), 
                    collapse = " + ")
  
  # construct full formula
  formula_str <- paste("death_count ~", temp_formula, "+", controls)
  
  # run regression
  glm.nb(as.formula(formula_str), data = data)
}

# run all models and store in list
run_all_models <- function(use_data) {
  
  # initialize the list
  models_list <- list()
  
  # NOAA model: both tmin and tmax
  models_list[["both"]] <- list(
    run_temp_reg(use_data, c("tmin_f", "tmax_f")),
    run_temp_reg(use_data, c("tmin_f", "tmax_f"), "3"),
    run_temp_reg(use_data, c("tmin_f", "tmax_f"), "7")
  )
  
  # ERA5 model: both wetbmax and wetbmin
  models_list[["wetb_both"]] <- list(
    run_temp_reg(use_data, c("wetb_min_citymin", "wetb_max_citymax")),
    run_temp_reg(use_data, c("wetb_min_citymin", "wetb_max_citymax"), "3"),
    run_temp_reg(use_data, c("wetb_min_citymin", "wetb_max_citymax"), "7")
  )
  
  return(models_list)
}

models_list <- run_all_models(use_data)

## Export regression results
modelsummary(c("Air temperature (NOAA)" = models_list[["both"]], 
               "Wet bulb temperature (ERA5)" = models_list[["wetb_both"]]),
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
                          # month fixed effects
                          "month2" = "February",
                          "month3" = "March",
                          "month4" = "April",
                          "month5" = "May",
                          "month6" = "June",
                          "month7" = "July",
                          "month8" = "August",
                          "month9" = "September",
                          "month10" = "October",
                          "month11" = "November",
                          "month12" = "December",
                          # tmax interactions with months
                          "month2:tmax_f" = "Max. temp. x Feb.",
                          "month3:tmax_f" = "Max. temp. x Mar.",
                          "month4:tmax_f" = "Max. temp. x Apr.",
                          "month5:tmax_f" = "Max. temp. x May",
                          "month6:tmax_f" = "Max. temp. x Jun.",
                          "month7:tmax_f" = "Max. temp. x Jul.",
                          "month8:tmax_f" = "Max. temp. x Aug.",
                          "month9:tmax_f" = "Max. temp. x Sep.",
                          "month10:tmax_f" = "Max. temp. x Oct.",
                          "month11:tmax_f" = "Max. temp. x Nov.",
                          "month12:tmax_f" = "Max. temp. x Dec.",
                          "month2:tmax_f_3d" = "Max. temp. x Feb.",
                          "month3:tmax_f_3d" = "Max. temp. x Mar.",
                          "month4:tmax_f_3d" = "Max. temp. x Apr.",
                          "month5:tmax_f_3d" = "Max. temp. x May",
                          "month6:tmax_f_3d" = "Max. temp. x Jun.",
                          "month7:tmax_f_3d" = "Max. temp. x Jul.",
                          "month8:tmax_f_3d" = "Max. temp. x Aug.",
                          "month9:tmax_f_3d" = "Max. temp. x Sep.",
                          "month10:tmax_f_3d" = "Max. temp. x Oct.",
                          "month11:tmax_f_3d" = "Max. temp. x Nov.",
                          "month12:tmax_f_3d" = "Max. temp. x Dec.",
                          "month2:tmax_f_7d" = "Max. temp. x Feb.",
                          "month3:tmax_f_7d" = "Max. temp. x Mar.",
                          "month4:tmax_f_7d" = "Max. temp. x Apr.",
                          "month5:tmax_f_7d" = "Max. temp. x May",
                          "month6:tmax_f_7d" = "Max. temp. x Jun.",
                          "month7:tmax_f_7d" = "Max. temp. x Jul.",
                          "month8:tmax_f_7d" = "Max. temp. x Aug.",
                          "month9:tmax_f_7d" = "Max. temp. x Sep.",
                          "month10:tmax_f_7d" = "Max. temp. x Oct.",
                          "month11:tmax_f_7d" = "Max. temp. x Nov.",
                          "month12:tmax_f_7d" = "Max. temp. x Dec.",
                          "month2:wetb_max_citymax" = "Max. temp. x Feb.",
                          "month3:wetb_max_citymax" = "Max. temp. x Mar.",
                          "month4:wetb_max_citymax" = "Max. temp. x Apr.",
                          "month5:wetb_max_citymax" = "Max. temp. x May",
                          "month6:wetb_max_citymax" = "Max. temp. x Jun.",
                          "month7:wetb_max_citymax" = "Max. temp. x Jul.",
                          "month8:wetb_max_citymax" = "Max. temp. x Aug.",
                          "month9:wetb_max_citymax" = "Max. temp. x Sep.",
                          "month10:wetb_max_citymax" = "Max. temp. x Oct.",
                          "month11:wetb_max_citymax" = "Max. temp. x Nov.",
                          "month12:wetb_max_citymax" = "Max. temp. x Dec.",
                          "month2:wetb_max_citymax_3d" = "Max. temp. x Feb.",
                          "month3:wetb_max_citymax_3d" = "Max. temp. x Mar.",
                          "month4:wetb_max_citymax_3d" = "Max. temp. x Apr.",
                          "month5:wetb_max_citymax_3d" = "Max. temp. x May",
                          "month6:wetb_max_citymax_3d" = "Max. temp. x Jun.",
                          "month7:wetb_max_citymax_3d" = "Max. temp. x Jul.",
                          "month8:wetb_max_citymax_3d" = "Max. temp. x Aug.",
                          "month9:wetb_max_citymax_3d" = "Max. temp. x Sep.",
                          "month10:wetb_max_citymax_3d" = "Max. temp. x Oct.",
                          "month11:wetb_max_citymax_3d" = "Max. temp. x Nov.",
                          "month12:wetb_max_citymax_3d" = "Max. temp. x Dec.",
                          "month2:wetb_max_citymax_7d" = "Max. temp. x Feb.",
                          "month3:wetb_max_citymax_7d" = "Max. temp. x Mar.",
                          "month4:wetb_max_citymax_7d" = "Max. temp. x Apr.",
                          "month5:wetb_max_citymax_7d" = "Max. temp. x May",
                          "month6:wetb_max_citymax_7d" = "Max. temp. x Jun.",
                          "month7:wetb_max_citymax_7d" = "Max. temp. x Jul.",
                          "month8:wetb_max_citymax_7d" = "Max. temp. x Aug.",
                          "month9:wetb_max_citymax_7d" = "Max. temp. x Sep.",
                          "month10:wetb_max_citymax_7d" = "Max. temp. x Oct.",
                          "month11:wetb_max_citymax_7d" = "Max. temp. x Nov.",
                          "month12:wetb_max_citymax_7d" = "Max. temp. x Dec.",
                          # tmin interactions with months
                          "tmin_f:month2" = "Min. temp. x Feb.",
                          "tmin_f:month3" = "Min. temp. x Mar.",
                          "tmin_f:month4" = "Min. temp. x Apr.",
                          "tmin_f:month5" = "Min. temp. x May",
                          "tmin_f:month6" = "Min. temp. x Jun.",
                          "tmin_f:month7" = "Min. temp. x Jul.",
                          "tmin_f:month8" = "Min. temp. x Aug.",
                          "tmin_f:month9" = "Min. temp. x Sep.",
                          "tmin_f:month10" = "Min. temp. x Oct.",
                          "tmin_f:month11" = "Min. temp. x Nov.",
                          "tmin_f:month12" = "Min. temp. x Dec.",
                          "tmin_f_3d:month2" = "Min. temp. x Feb.",
                          "tmin_f_3d:month3" = "Min. temp. x Mar.",
                          "tmin_f_3d:month4" = "Min. temp. x Apr.",
                          "tmin_f_3d:month5" = "Min. temp. x May",
                          "tmin_f_3d:month6" = "Min. temp. x Jun.",
                          "tmin_f_3d:month7" = "Min. temp. x Jul.",
                          "tmin_f_3d:month8" = "Min. temp. x Aug.",
                          "tmin_f_3d:month9" = "Min. temp. x Sep.",
                          "tmin_f_3d:month10" = "Min. temp. x Oct.",
                          "tmin_f_3d:month11" = "Min. temp. x Nov.",
                          "tmin_f_3d:month12" = "Min. temp. x Dec.",
                          "tmin_f_7d:month2" = "Min. temp. x Feb.",
                          "tmin_f_7d:month3" = "Min. temp. x Mar.",
                          "tmin_f_7d:month4" = "Min. temp. x Apr.",
                          "tmin_f_7d:month5" = "Min. temp. x May",
                          "tmin_f_7d:month6" = "Min. temp. x Jun.",
                          "tmin_f_7d:month7" = "Min. temp. x Jul.",
                          "tmin_f_7d:month8" = "Min. temp. x Aug.",
                          "tmin_f_7d:month9" = "Min. temp. x Sep.",
                          "tmin_f_7d:month10" = "Min. temp. x Oct.",
                          "tmin_f_7d:month11" = "Min. temp. x Nov.",
                          "tmin_f_7d:month12" = "Min. temp. x Dec.",
                          "wetb_min_citymin:month2" = "Min. temp. x Feb.",
                          "wetb_min_citymin:month3" = "Min. temp. x Mar.",
                          "wetb_min_citymin:month4" = "Min. temp. x Apr.",
                          "wetb_min_citymin:month5" = "Min. temp. x May",
                          "wetb_min_citymin:month6" = "Min. temp. x Jun.",
                          "wetb_min_citymin:month7" = "Min. temp. x Jul.",
                          "wetb_min_citymin:month8" = "Min. temp. x Aug.",
                          "wetb_min_citymin:month9" = "Min. temp. x Sep.",
                          "wetb_min_citymin:month10" = "Min. temp. x Oct.",
                          "wetb_min_citymin:month11" = "Min. temp. x Nov.",
                          "wetb_min_citymin:month12" = "Min. temp. x Dec.",
                          "wetb_min_citymin_3d:month2" = "Min. temp. x Feb.",
                          "wetb_min_citymin_3d:month3" = "Min. temp. x Mar.",
                          "wetb_min_citymin_3d:month4" = "Min. temp. x Apr.",
                          "wetb_min_citymin_3d:month5" = "Min. temp. x May",
                          "wetb_min_citymin_3d:month6" = "Min. temp. x Jun.",
                          "wetb_min_citymin_3d:month7" = "Min. temp. x Jul.",
                          "wetb_min_citymin_3d:month8" = "Min. temp. x Aug.",
                          "wetb_min_citymin_3d:month9" = "Min. temp. x Sep.",
                          "wetb_min_citymin_3d:month10" = "Min. temp. x Oct.",
                          "wetb_min_citymin_3d:month11" = "Min. temp. x Nov.",
                          "wetb_min_citymin_3d:month12" = "Min. temp. x Dec.",
                          "wetb_min_citymin_7d:month2" = "Min. temp. x Feb.",
                          "wetb_min_citymin_7d:month3" = "Min. temp. x Mar.",
                          "wetb_min_citymin_7d:month4" = "Min. temp. x Apr.",
                          "wetb_min_citymin_7d:month5" = "Min. temp. x May",
                          "wetb_min_citymin_7d:month6" = "Min. temp. x Jun.",
                          "wetb_min_citymin_7d:month7" = "Min. temp. x Jul.",
                          "wetb_min_citymin_7d:month8" = "Min. temp. x Aug.",
                          "wetb_min_citymin_7d:month9" = "Min. temp. x Sep.",
                          "wetb_min_citymin_7d:month10" = "Min. temp. x Oct.",
                          "wetb_min_citymin_7d:month11" = "Min. temp. x Nov.",
                          "wetb_min_citymin_7d:month12" = "Min. temp. x Dec."),
             vcov = "HC3",
             gof_omit = "^(?!.*Num)",
             stars = TRUE,
             output = "output/appendix/tablea9.docx")
