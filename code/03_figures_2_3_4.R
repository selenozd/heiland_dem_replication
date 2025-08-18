
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Figures 2, 3, and 4
### Author: Selen Ozdogan

## Setup 
rm(list = ls())
gc()

# run the libraries script to load (and install if necessaary) 
# all required packages
source("code/00_libraries.R")


## Read in the use data
use_data <- read_csv("data/use_data.csv")


## Figure 2: Average daily air temperature minimum and maximum trends by season

# calculate seasonal averages of maximum and minimum air temperatures
fig2_df <- use_data %>%
  mutate(season = case_when(
    summer == 1 ~ "Summer",
    winter == 1 ~ "Winter",
    fall == 1 ~ "Fall",
    spring == 1 ~ "Spring")) %>%
  group_by(year, season) %>%
  summarize(mean_tmax = mean(tmax_f), mean_tmin = mean(tmin_f))

fig2 <- ggplot(fig2_df, aes(x = year)) +
  geom_line(aes(y = mean_tmax, color = "Average daily maximum")) +
  geom_line(aes(y = mean_tmin, color = "Average daily minimum")) +
  # add dashed regression lines
  geom_smooth(aes(y = mean_tmax, color = "Average daily maximum"),
              method = "lm", se = FALSE, linetype = "dashed",
              linewidth = 0.5) +
  geom_smooth(aes(y = mean_tmin, color = "Average daily minimum"),
              method = "lm", se = FALSE, linetype = "dashed",
              linewidth = 0.5) +
  facet_wrap(~ season, scales = "free_x") +
  theme_bw() +
  scale_fill_manual(values = c("#CB4335", "#2E86C1")) +
  scale_color_manual(values = c("#CB4335", "#2E86C1")) +
  ggtitle("") +
  xlab("") + ylab(" ") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        axis.title = element_blank(),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18)) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10),
                     sec.axis = sec_axis(~ ., name = "",
                     breaks = seq(from = 0, to = 100, by = 10)))

# display and save the plot
fig2
ggsave("output/fig2.png", plot = fig2, width = 8, height = 6, dpi = 600)


## Figure 3: Daily maximum and minimum air temperature (Fahrenheit) in 2015

# Keep only 2015 data
fig3_df <- use_data %>%
  filter(year == 2015)

fig3 <- ggplot(fig3_df, aes(x = day_of_year)) +
  geom_line(aes(y = tmax_f, color = "Daily maximum")) +
  geom_line(aes(y = tmin_f, color = "Daily minimum")) +
  theme_bw() +
  scale_color_manual(values = c("#CB4335", "#2E86C1")) +
  # add vertical lines for seasons
  geom_vline(xintercept = c(60, 150, 240, 330), linetype = "solid",
             linewidth = 0.4) +
  # label seasons
  annotate("text", x = c(22, 105, 195, 285, 355), y = 100, 
           label = c("Winter", "Spring", "Summer", "Fall", "Winter"), 
           color = "black", size = 6) +
  ggtitle("") +
  theme(plot.title = element_text(size = 18)) +
  xlab("Day of year") + ylab(" ") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(from = 0, to = 90, by = 10)) + 
  scale_x_continuous(breaks = seq(from = 0, to = 365, by = 30)) +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18)) 

# display and save the plot
fig3
ggsave("output/fig3.png", plot = fig3, width = 8, height = 6, dpi = 600)


## Figure 4: Boxplots of average daily maximum and minimum wet bulb 
## temperatures, averaged over seasons

# calculate daily averages for each season
fig4_df <- use_data %>%
  group_by(day_of_year, season) %>%
  summarise(wetb_minmin = mean(wetb_minmin),
            wetb_maxmax = mean(wetb_maxmax))

# create separate boxplots for each season distribution
fig4a <- ggplot(fig4_df,
                aes(fill = factor(season,
                             levels = c("Winter","Spring","Summer", "Fall")),
                    y = wetb_minmin)) +
  geom_boxplot() +
  theme_light() + 
  scale_fill_manual(values = c("#2E86C1", "#229954", "#CB4335", "#D68910")) +
  theme(plot.title = element_text(size = 12), legend.position = "none",
        axis.text.x = element_blank()) +
  labs(subtitle = "Minimum wet bulb temperature", fill = NULL) + 
  ylab("") + xlab("") +
  coord_cartesian(ylim = c(10,100)) +
  # label seasons
  annotate("text", x = c(-0.3, -0.1, 0.1, 0.3), y = 100, 
           label = c("Winter", "Spring", "Summer", "Fall"), 
           color = "black", size = 5) +
  scale_y_continuous(breaks = seq(from = 10, to = 100, by = 10)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        plot.subtitle = element_text(size = 15),
        axis.title = element_blank(),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15))

fig4a

fig4b <- ggplot(fig4_df, 
                aes(fill = factor(season,
                              levels = c("Winter", "Spring", "Summer", "Fall")),
                    y = wetb_maxmax)) +
  geom_boxplot() +
  theme_light() + 
  scale_fill_manual(values = c("#2E86C1", "#229954", "#CB4335", "#D68910")) +
  theme(plot.title = element_text(size = 12), legend.position = "none",
        axis.text.x = element_blank()) +
  labs(subtitle = "Maximum wet bulb temperature", fill = NULL) + 
  ylab("") + xlab("") +
  coord_cartesian(ylim = c(10,100)) +
  # label seasons
  annotate("text", x = c(-0.3, -0.1, 0.1, 0.3), y = 100, 
           label = c("Winter", "Spring", "Summer", "Fall"), 
           color = "black", size = 5) +
  scale_y_continuous(breaks = seq(from = 10, to = 100, by = 10)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.subtitle = element_text(size = 15),
        axis.title = element_blank(),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15))

fig4b

fig4 <- plot_grid(
  fig4a, fig4b, 
  ncol = 2,
  align = 'v',
  axis = 'l',
  rel_heights = c(1, 1),  
  vjust = 1, 
  scale = 0.9 
)

# display and save the plot
fig4
ggsave("output/fig4.png", plot = fig4, width = 8, height = 6, dpi = 600)

