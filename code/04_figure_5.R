
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Figure 5
### Author: Selen Ozdogan

## Setup 
rm(list = ls())
gc()

# run the libraries script to load (and install if necessaary) 
# all required packages
source("code/00_libraries.R")


## Read in the use data
use_data <- read_csv("data/use_data.csv")


## Run the 7-day regression models again
era5_7d <- glm.nb(death_count ~ wetb_minmin_7d + wetb_minmin_7d:summer + 
                    wetb_minmin_7d:spring + wetb_minmin_7d:fall + 
                    wetb_maxmax_7d + wetb_maxmax_7d:summer + 
                    wetb_maxmax_7d:spring + wetb_maxmax_7d:fall + summer + 
                    spring + fall + newyear + july4 + veterans + christmas + 
                    thanksgiving + memorial + presidents + labor + columbus + 
                    precip + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)

noaa_7d <- glm.nb(death_count ~ tmin_f_7d + tmin_f_7d:summer + 
                    tmin_f_7d:spring + tmin_f_7d:fall + tmax_f_7d + 
                    tmax_f_7d:summer + tmax_f_7d:spring +
                    tmax_f_7d:fall + summer + fall + spring + 
                    newyear + july4 + veterans + christmas + thanksgiving + 
                    memorial + presidents + labor + columbus + 
                    precip + relevel(factor(decade), ref = 5) +
                    relevel(factor(year), ref = 34), 
                  data = use_data)


## Figure 5 (a): Coefficient plots for NOAA

# extract coefficients and variance-covariance matrix
coef_noaa <- coef(noaa_7d)
vcov_noaa <- vcov(noaa_7d)

# calculate combined (absolute) effect coefficients
combined_coefs_noaa <- c(
  coef_noaa["tmax_f_7d"] + coef_noaa["summer:tmax_f_7d"],
  coef_noaa["tmin_f_7d"] + coef_noaa["tmin_f_7d:summer"],
  coef_noaa["tmax_f_7d"] + coef_noaa["spring:tmax_f_7d"],
  coef_noaa["tmin_f_7d"] + coef_noaa["tmin_f_7d:spring"],
  coef_noaa["tmax_f_7d"] + coef_noaa["fall:tmax_f_7d"],
  coef_noaa["tmin_f_7d"] + coef_noaa["tmin_f_7d:fall"],
  coef_noaa["tmax_f_7d"],
  coef_noaa["tmin_f_7d"]
)

# calculate combined standard errors
combined_ses_noaa <- c(
  sqrt(vcov_noaa["tmax_f_7d", "tmax_f_7d"] + 
         vcov_noaa["summer:tmax_f_7d", "summer:tmax_f_7d"] + 
         2*vcov_noaa["tmax_f_7d", "summer:tmax_f_7d"]),
  sqrt(vcov_noaa["tmin_f_7d", "tmin_f_7d"] + 
         vcov_noaa["tmin_f_7d:summer", "tmin_f_7d:summer"] + 
         2*vcov_noaa["tmin_f_7d", "tmin_f_7d:summer"]),
  sqrt(vcov_noaa["tmax_f_7d", "tmax_f_7d"] + 
         vcov_noaa["spring:tmax_f_7d", "spring:tmax_f_7d"] + 
         2*vcov_noaa["tmax_f_7d", "spring:tmax_f_7d"]),
  sqrt(vcov_noaa["tmin_f_7d", "tmin_f_7d"] + 
         vcov_noaa["tmin_f_7d:spring", "tmin_f_7d:spring"] + 
         2*vcov_noaa["tmin_f_7d", "tmin_f_7d:spring"]),
  sqrt(vcov_noaa["tmax_f_7d", "tmax_f_7d"] + 
         vcov_noaa["fall:tmax_f_7d", "fall:tmax_f_7d"] + 
         2*vcov_noaa["tmax_f_7d", "fall:tmax_f_7d"]),
  sqrt(vcov_noaa["tmin_f_7d", "tmin_f_7d"] + 
         vcov_noaa["tmin_f_7d:fall", "tmin_f_7d:fall"] + 
         2*vcov_noaa["tmin_f_7d", "tmin_f_7d:fall"]),
  sqrt(vcov_noaa["tmax_f_7d", "tmax_f_7d"]),
  sqrt(vcov_noaa["tmin_f_7d", "tmin_f_7d"])
)

# assign clean coefficient names
noaa_coefnames = c(
  "summer:tmax_f_7d" = "Max. temp, Summer",
  "tmin_f_7d:summer" = "Min. temp, Summer",
  "spring:tmax_f_7d" = "Max. temp, Spring",
  "tmin_f_7d:spring" = "Min. temp, Spring",
  "fall:tmax_f_7d" = "Max. temp, Fall",
  "tmin_f_7d:fall" = "Min. temp, Fall",
  "tmax_f_7d" = "Max. temp, Winter",
  "tmin_f_7d" = "Min. temp, Winter"
)

# create a data frame for plotting
fig5a_df <- data.frame(
  term = noaa_coefnames,
  estimate = combined_coefs_noaa,
  std.error = combined_ses_noaa,
  order = seq_along(noaa_coefnames)
)

# define a function that creates groups for coefficients for coloring
get_coef_group <- function(coef_names) {
  coef_groups <- numeric(length(coef_names))  
  
  # loop through each coefficient name and assign it to a group
  for (i in seq_along(coef_names)) {
    name <- coef_names[i]
    if (grepl("Winter", name)) {
      coef_groups[i] <- 1
    } else if (grepl("Fall", name)) {
      coef_groups[i] <- 2
    } else if (grepl("Spring", name)) {
      coef_groups[i] <- 3
    } else if (grepl("Summer", name)) {
      coef_groups[i] <- 4
    } else {
      coef_groups[i] <- NA  
    }
  }
  
  return(coef_groups)
}

# apply the function to get coefficient groups for noaa
coef_groups <- get_coef_group(noaa_coefnames)
print(coef_groups)

# define stylistic elements for the plot
linetype <- rep(c("dashed", "solid"), 4)

season_colors <- c( "#2E86C1","#D68910","#229954", "#CB4335")

## plot figure 5a
fig5a <- ggplot(fig5a_df, 
                aes(x = estimate, y = fct_reorder(term, -order),
                    xmin = estimate - 1.96 * std.error, 
                    xmax = estimate + 1.96 * std.error)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray45") +
  geom_pointrange(aes(color = factor(coef_groups), linetype = linetype), 
                  size = 1.5, fatten = 3, linewidth = 1) + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = c(-0.005, 0, 0.005, 0.01, 0.015)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_color_manual(values = season_colors) +
  theme_bw() + 
  xlab("") + 
  ylab("") +
  guides(color = FALSE, linetype = FALSE) +
  ggtitle("") +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 18, hjust = 14.2),
        plot.subtitle = element_text(size = 15),
        axis.title = element_blank(),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(subtitle = "Dependent variable: Daily unclaimed death count")

print(fig5a)

# save
png("output/fig5a.png", width = 8, height = 6, units = "in", res = 600)
grid::grid.draw(fig5a)
dev.off()


## Figure 5 (b): Coefficient plots for ERA5

# extract coefficients and variance-covariance matrix
coef_era5 <- coef(era5_7d)
vcov_era5 <- vcov(era5_7d)

# calculate combined coefficients
combined_coefs <- c(
  coef_era5["wetb_maxmax_7d"] + coef_era5["summer:wetb_maxmax_7d"],
  coef_era5["wetb_minmin_7d"] + coef_era5["wetb_minmin_7d:summer"],
  coef_era5["wetb_maxmax_7d"] + coef_era5["spring:wetb_maxmax_7d"],
  coef_era5["wetb_minmin_7d"] + coef_era5["wetb_minmin_7d:spring"],
  coef_era5["wetb_maxmax_7d"] + coef_era5["fall:wetb_maxmax_7d"],
  coef_era5["wetb_minmin_7d"] + coef_era5["wetb_minmin_7d:fall"],
  coef_era5["wetb_maxmax_7d"],
  coef_era5["wetb_minmin_7d"]
)

# calculate combined standard errors
combined_ses <- c(
  sqrt(vcov_era5["wetb_maxmax_7d", "wetb_maxmax_7d"] + 
         vcov_era5["summer:wetb_maxmax_7d", "summer:wetb_maxmax_7d"] +
         2*vcov_era5["wetb_maxmax_7d", "summer:wetb_maxmax_7d"]),
  sqrt(vcov_era5["wetb_minmin_7d", "wetb_minmin_7d"] + 
         vcov_era5["wetb_minmin_7d:summer", "wetb_minmin_7d:summer"] + 
         2*vcov_era5["wetb_minmin_7d", "wetb_minmin_7d:summer"]),
  sqrt(vcov_era5["wetb_maxmax_7d", "wetb_maxmax_7d"] + 
         vcov_era5["spring:wetb_maxmax_7d", "spring:wetb_maxmax_7d"] + 
         2*vcov_era5["wetb_maxmax_7d", "spring:wetb_maxmax_7d"]),
  sqrt(vcov_era5["wetb_minmin_7d", "wetb_minmin_7d"] + 
         vcov_era5["wetb_minmin_7d:spring", "wetb_minmin_7d:spring"] + 
         2*vcov_era5["wetb_minmin_7d", "wetb_minmin_7d:spring"]),
  sqrt(vcov_era5["wetb_maxmax_7d", "wetb_maxmax_7d"] + 
         vcov_era5["fall:wetb_maxmax_7d", "fall:wetb_maxmax_7d"] + 
         2*vcov_era5["wetb_maxmax_7d", "fall:wetb_maxmax_7d"]),
  sqrt(vcov_era5["wetb_minmin_7d", "wetb_minmin_7d"] + 
         vcov_era5["wetb_minmin_7d:fall", "wetb_minmin_7d:fall"] +
         2*vcov_era5["wetb_minmin_7d", "wetb_minmin_7d:fall"]),
  sqrt(vcov_era5["wetb_maxmax_7d", "wetb_maxmax_7d"]),
  sqrt(vcov_era5["wetb_minmin_7d", "wetb_minmin_7d"])
)

# assign clean coefficient names
era5_coefnames = c(
  "summer:wetb_maxmax_7d" = "Max. temp, Summer",
  "wetb_minmin_7d:summer" = "Min. temp, Summer",
  "spring:wetb_maxmax_7d" = "Max. temp, Spring",
  "wetb_minmin_7d:spring" = "Min. temp, Spring",
  "fall:wetb_maxmax_7d" = "Max. temp, Fall",
  "wetb_minmin_7d:fall" = "Min. temp, Fall",
  "wetb_maxmax_7d" = "Max. temp, Winter",
  "wetb_minmin_7d" = "Min. temp, Winter"
)

# create a data frame for plotting
fig5b_df <- data.frame(
  term = era5_coefnames,
  estimate = combined_coefs,
  std.error = combined_ses,
  order = seq_along(era5_coefnames)
)

# get coefficient groups
coef_groups <- get_coef_group(era5_coefnames)
print(coef_groups)

# create figure 5 b
fig5b <- ggplot(fig5b_df, 
                aes(x = estimate, y = fct_reorder(term, -order),
                    xmin = estimate - 1.96 * std.error, 
                    xmax = estimate + 1.96 * std.error)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray45") +
  geom_pointrange(aes(color = factor(coef_groups), linetype = linetype), 
                  size = 1.5, fatten = 3, linewidth = 1) + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = c(-0.005, 0, 0.005, 0.01, 0.015), 
                     limits = c(min(fig5b_df$estimate - 1.96 * 
                                      fig5b_df$std.error), 0.017)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_color_manual(values = season_colors) +
  theme_bw() + 
  xlab("") + 
  ylab("") +
  guides(color = FALSE, linetype = FALSE) +
  ggtitle("") +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 18, hjust = 14.2),
        plot.subtitle = element_text(size = 15),
        axis.title = element_blank(),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(subtitle = "Dependent variable: Daily unclaimed death count")

print(fig5b)

# save
png("output/fig5b.png", width = 8, height = 6, units = "in", res = 600)
grid::grid.draw(fig5b)
dev.off()



