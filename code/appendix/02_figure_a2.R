
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Figure A2
### Author: Selen Ozdogan

## Setup 
rm(list = ls())
gc()

# run the libraries script to load (and install if necessaary) 
# all required packages
source("code/00_libraries.R")


## Read in the use data
use_data <- read_csv("data/use_data.csv")


## Run the 7-day regression models again but don't specify reference levels
era5_7d <- glm.nb(death_count ~ wetb_minmin_7d + wetb_minmin_7d:summer + 
                    wetb_minmin_7d:spring + wetb_minmin_7d:fall + 
                    wetb_maxmax_7d + wetb_maxmax_7d:summer + 
                    wetb_maxmax_7d:spring + wetb_maxmax_7d:fall + summer + 
                    spring + fall + newyear + july4 + veterans + christmas + 
                    thanksgiving + memorial + presidents + labor + columbus + 
                    precip + factor(decade) + factor(year), 
                  data = use_data)

noaa_7d <- glm.nb(death_count ~ tmin_f_7d + tmin_f_7d:summer + 
                    tmin_f_7d:spring + tmin_f_7d:fall + tmax_f_7d + 
                    tmax_f_7d:summer + tmax_f_7d:spring + tmax_f_7d:fall + 
                    summer + fall + spring + newyear + july4 +
                    veterans + christmas + thanksgiving + memorial + 
                    presidents + labor + columbus + precip + factor(decade) +
                    factor(year), 
                  data = use_data)


## Figure A2 (a): Prediction plots for NOAA

# calculate the mean difference between tmax and tmin (difference is 29.15)
diff_temp <- mean(use_data$tmax_f_7d) - mean(use_data$tmin_f_7d)
diff_temp

# create a data frame with the range maximum temperatures based on real range
temp_range <- seq(24, 104, length.out = 100)
pred_data <- data.frame(tmax_f_7d = rep(temp_range, 4),
                        summer = c(rep(0, 100), rep(1, 100), rep(0, 200)),
                        spring = c(rep(0, 200), rep(1, 100), rep(0, 100)),
                        fall = c(rep(0, 300), rep(1, 100)),
                        july4 = 0, labor = 0, christmas = 0, thanksgiving = 0, 
                        newyear = 0, veterans = 0, memorial = 0, presidents = 0,
                        columbus = 0, precip = mean(use_data$precip))

# add tmin to the prediction data frame based on the mean difference of 29.15
pred_data$tmin_f_7d <- pred_data$tmax_f_7d - diff_temp

# get real temp ranges by season
tapply(use_data$tmax_f_7d, use_data$season, summary)

# drop unrealistic values
pred_data %<>% filter((tmax_f_7d > 70 & tmax_f_7d < 104 & summer == 1) |
                        (tmax_f_7d < 75 & tmax_f_7d > 24 & summer == 0 & 
                           spring == 0 & fall == 0) |
                        (tmax_f_7d < 96 & tmax_f_7d > 33 & spring == 1) |
                        (tmax_f_7d < 97 & tmax_f_7d > 44 & fall == 1))

# add decade and year variables to the prediction data frame
pred_data$decade <- use_data$decade[1]
pred_data$year <- use_data$year[1]

# change the season interaction coefficients to combined coefficients manually
noaa_7d$coefficients[c(67, 68, 69, 70, 71, 72)] <- 
  c(-0.001095821, 0.004086315, -0.003439481,
    0.011878866, -0.003652598, -0.000619311)

# generate predictions for each combination of min and max temps and season
pred_data$predicted_count <- predict(noaa_7d, 
                                     newdata = pred_data,
                                     type = "response")

# add lower and upper confidence intervals
pred_data$lower_ci <- predict(noaa_7d, 
                              newdata = pred_data, 
                              type = "response", 
      se.fit = TRUE)$fit - 1.96 * predict(noaa_7d, newdata = pred_data, 
                                      type = "response", se.fit = TRUE)$se.fit

pred_data$upper_ci <- predict(noaa_7d, 
                              newdata = pred_data, 
                              type = "response", 
      se.fit = TRUE)$fit + 1.96 * predict(noaa_7d, newdata = pred_data, 
                                      type = "response", se.fit = TRUE)$se.fit

# create Figure A2 a
figa2a <- ggplot(pred_data, aes(x = tmax_f_7d, y = predicted_count, 
                                color = factor(2*spring + 3*summer + 4*fall))) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("#2E86C1", "#229954", "#CB4335","#D68910"),
                     labels = c("Winter", "Spring", "Summer", "Fall")) +
  geom_ribbon(aes(x = tmax_f_7d, ymin = lower_ci, ymax = upper_ci),
              fill = "gray", alpha = 0.2, linetype = "blank") +
  scale_x_continuous(breaks = seq(20, 110, 10), limits = c(20, 110)) +
  scale_y_continuous(breaks = seq(0.75, 1.75, 0.25), limits = c(0.75, 1.75)) +
  labs(x = "Maximum temperature",
       y = "",
       color = "",
       fill = "") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        plot.title = element_text(size = 18),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

# display and save plot
figa2a
ggsave("output/appendix/figa2_a.png", width = 8, height = 6, dpi = 600)


## Figure A2 (b): Prediction plots for ERA5

# calculate the mean difference between maxmax and minmin wet bulb temp
diff_wetb_temp <- mean(use_data$wetb_maxmax_7d) - mean(use_data$wetb_minmin_7d)
diff_wetb_temp

# create a data frame with range of maximum temperatures for each season
# based on the actual observed max temp
temp_range <- seq(16.36, 111.03, length.out = 100)
pred_data_era5 <- data.frame(wetb_maxmax_7d = rep(temp_range, 4),
                        summer = c(rep(0, 100), rep(1, 100), rep(0, 200)),
                        spring = c(rep(0, 200), rep(1, 100), rep(0, 100)),
                        fall = c(rep(0, 300), rep(1, 100)), july4 = 0, 
                        labor = 0, christmas = 0, thanksgiving = 0, newyear = 0, 
                        veterans = 0, memorial = 0, presidents = 0,
                        columbus = 0, precip = mean(use_data$precip))

# add wetb_minmin to the prediction data frame based on the mean difference of 38.17
pred_data_era5$wetb_minmin_7d <- pred_data_era5$wetb_maxmax_7d - diff_wetb_temp

# add decade and year variables to the prediction data frame
pred_data_era5$decade <- use_data$decade[1]
pred_data_era5$year <- use_data$year[1]

# get real temp ranges by season
tapply(use_data$wetb_maxmax_7d, use_data$season, summary)
tapply(use_data$wetb_minmin_7d, use_data$season, summary)

# drop unrealistic values for each season
pred_data_era5 %<>% filter((wetb_maxmax_7d > 72 & wetb_maxmax_7d < 111 & 
                              summer == 1) |
                        (wetb_maxmax_7d < 83 & wetb_maxmax_7d > 16 & 
                           summer == 0 & spring == 0 & fall == 0) |
                        (wetb_maxmax_7d < 100 & wetb_maxmax_7d > 31 & 
                           spring == 1) |
                          (wetb_maxmax_7d < 105 & wetb_maxmax_7d > 40 & 
                             fall == 1))

# change the season interaction coefficients to combined coefs manually
era5_7d$coefficients[c(67, 68, 69, 70, 71, 72)] <- 
  c(-0.000658517, 0.003857502, -0.00114067,
    0.008573337, -0.004472797, -0.002084064)

# generate predictions for each combination of min and max temps and season
pred_data_era5$predicted_count <- predict(era5_7d, 
                                          newdata = pred_data_era5, 
                                          type = "response")

# generate lower and upper confidence intervals
pred_data_era5$lower_ci <- predict(era5_7d, 
                                   newdata = pred_data_era5,
                                   type = "response", 
      se.fit = TRUE)$fit - 1.96 * predict(era5_7d, newdata = pred_data_era5, 
                                      type = "response", se.fit = TRUE)$se.fit
pred_data_era5$upper_ci <- predict(era5_7d, newdata = pred_data_era5,
                                   type = "response", 
      se.fit = TRUE)$fit + 1.96 * predict(era5_7d, newdata = pred_data_era5, 
                                      type = "response", se.fit = TRUE)$se.fit

# create figure a2 b
figa2b <- ggplot(pred_data_era5, aes(x = wetb_maxmax_7d, y = predicted_count, 
                                color = factor(2*spring + 3*summer + 4*fall))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_color_manual(values = c("#2E86C1", "#229954", "#CB4335","#D68910"),
                     labels = c("Winter", "Spring", "Summer", "Fall")) +
  geom_ribbon(aes(x = wetb_maxmax_7d, ymin = lower_ci, ymax = upper_ci),
              fill = "gray", alpha = 0.2, linetype = "blank") +
  scale_x_continuous(breaks = seq(20, 110, 10), limits = c(20, 110)) +
  scale_y_continuous(breaks = seq(0.75, 1.75, 0.25), limits = c(0.75, 1.75)) +
  labs(x = "Maximum temperature",
       y = "",
       color = "",
       fill = "") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        plot.title = element_text(size = 18),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

# display and save the plot
figa2b
ggsave("output/appendix/figa2_b.png", width = 8, height = 6, dpi = 600)
