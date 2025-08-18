
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Table A9
### Author: Selen Ozdogan

## Setup 
rm(list = ls())
gc()

# run the libraries script to load (and install if necessaary) 
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

# Define function to run negative binomial regression with month fe
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
                      "labor", "columbus", "precip", "month2",
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
    run_temp_reg(noaa_data, c("tmin_f", "tmax_f")),
    run_temp_reg(noaa_data, c("tmin_f", "tmax_f"), "3"),
    run_temp_reg(noaa_data, c("tmin_f", "tmax_f"), "7")
  )
  
  # ERA5 model: both wetbmax and wetbmin
  models_list[["wetb_both"]] <- list(
    run_temp_reg(era5_data, c("wetb_minmin", "wetb_maxmax")),
    run_temp_reg(era5_data, c("wetb_minmin", "wetb_maxmax"), "3"),
    run_temp_reg(era5_data, c("wetb_minmin", "wetb_maxmax"), "7")
  )
  
  return(models_list)
}


#3# run all models and export results ##########################################
models_list <- run_all_models(noaa_hils, era5_hils)

# export both tmax & tmin models
modelsummary(c("Air temperature (NOAA)" = models_list[["both"]], 
               "Wet bulb temperature (ERA5)" = models_list[["wetb_both"]]),
             coef_omit = "relevel|Intercept|newyear|july4|veterans|christmas|thanksgiving|memorial|presidents|labor|columbus",
             vcov = "HC3",
             gof_omit = "^(?!.*Num)",
             stars = TRUE,
             output = "tables/r&r/month_fe_int_main_models.docx")

#4# run wald tests for joint significance ######################################

# function to run wald test
run_wald_tests <- function(model, prefix) {  
  
  # define which months go into which season
  seasons <- list(
    Winter = c(1, 2, 12),
    Spring = c(3, 4, 5),
    Summer = c(6, 7, 8),
    Fall = c(9, 10, 11)
  )
  
  # helper function to create hypothesis strings
  create_hyp <- function(prefix, months) {
    if (1 %in% months) {
      hyp <- c(paste0(prefix, " = 0"))
      months <- months[months != 1]
    } else {
      hyp <- c()
    }
    
    # create both possible interaction term formats
    format1 <- paste0(prefix, ":month", months, " = 0") 
    format2 <- paste0("month", months, ":", prefix, " = 0") 
    
    # check which format exists in the model
    coef_names <- names(coef(model))
    use_format1 <- any(gsub(" = 0", "", format1) %in% coef_names)
    use_format2 <- any(gsub(" = 0", "", format2) %in% coef_names)
    
    if (use_format1) {
      return(c(hyp, format1))
    } else if (use_format2) {
      return(c(hyp, format2))
    }
    return(hyp)
  }
  
  # run tests for each season
  results <- list()
  for (season_name in names(seasons)) {
    months <- seasons[[season_name]]
    hypotheses <- create_hyp(prefix, months)
    
    # run the test
    test_result <- linearHypothesis(model, hypotheses, singular.ok = TRUE, 
                                    vcov = hccm(model, type = "hc3"))
    results[[season_name]] <- test_result
    cat("\n", season_name, " Test Results:\n")
    print(test_result)
  }
  
  invisible(results)
}

# function to test both temperature variables together
run_full_temp_tests <- function(model, min_var = "tmin_f", max_var = "tmax_f") {
  # run tests for minimum temperature
  cat(sprintf("=== %s Seasonal Tests ===\n", min_var))
  tmin_results <- run_wald_tests(model, prefix = min_var)
  
  # run tests for maximum temperature
  cat(sprintf("\n=== %s Seasonal Tests ===\n", max_var))
  tmax_results <- run_wald_tests(model, prefix = max_var)
  
  # return both sets of results invisibly
  invisible(list(tmin = tmin_results, tmax = tmax_results))
}

# run the wald tests for noaa models
wald_noaa_both <- run_full_temp_tests(models_list[["both"]][[1]], 
                                      "tmin_f", "tmax_f")
wald_noaa_both3d <- run_full_temp_tests(models_list[["both"]][[2]],
                                        "tmin_f_3d", "tmax_f_3d")
wald_noaa_both7d <- run_full_temp_tests(models_list[["both"]][[3]], 
                                        "tmin_f_7d", "tmax_f_7d")

# run the wald tests for era5 models
wald_era5_both <- run_full_temp_tests(models_list[["wetb_both"]][[1]], 
                                      "wetb_minmin", "wetb_maxmax")
wald_era5_both3d <- run_full_temp_tests(models_list[["wetb_both"]][[2]],
                                        "wetb_minmin_3d", "wetb_maxmax_3d")
wald_era5_both7d <- run_full_temp_tests(models_list[["wetb_both"]][[3]], 
                                        "wetb_minmin_7d", "wetb_maxmax_7d")

# clean and export wald test results
clean_results <- function(wald_object, source, temp_type) {
  do.call(rbind, lapply(names(wald_object), function(season) {
    res <- wald_object[[season]]
    data.frame(
      source = source,
      temp_type = temp_type,
      season = season,
      df = res$Df[2],
      chisq = res$Chisq[2],
      p_value = res$`Pr(>Chisq)`[2],
      significant_95 = res$`Pr(>Chisq)`[2] < 0.05,
      significant_90 = res$`Pr(>Chisq)`[2] < 0.10,
      stringsAsFactors = FALSE
    )
  }))
}

# combine all results
results_df <- rbind(
  clean_results(wald_noaa_both$tmin, "NOAA", "tmin"),
  clean_results(wald_noaa_both$tmax, "NOAA", "tmax"),
  clean_results(wald_era5_both$tmin, "ERA5", "wetb_min"),
  clean_results(wald_era5_both$tmax, "ERA5", "wetb_max")
)

# export
write.csv(results_df, "tables/r&r/month_fe_wald_test_summary.csv", 
          row.names = FALSE)
