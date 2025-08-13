
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Figure 1
### Author: Selen Ozdogan

## Setup 
rm(list = ls())
gc()

# run the libraries script to load (and install if necessaary) 
# all required packages
source("code/00_libraries.R")


## Read in the use data
use_data <- read_csv("data/use_data.csv")


## Figure 1 (a): Total number of unclaimed deaths by year

# reshape the data from wide to long format
fig1a_df <- use_data %>%
  pivot_longer(
    cols = c(female_death_count, male_death_count, unknown_sex_death_count),
    names_to = "sex",
    values_to = "death_count_total"
  ) %>%
  mutate(sex = str_remove(sex, "_death_count")) %>%
  mutate(sex = str_remove(sex, "_sex")) %>%
  filter(death_count_total > 0) %>%
  group_by(year, sex) %>%
  summarize(death_count_total = sum(death_count_total), .groups = "drop")

# create the stacked bar chart
fig1a <- ggplot(fig1a_df, 
                aes(fill = factor(sex, levels = c("unknown", "female", "male")),
                    x = year, y = death_count_total)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#0072B2")) +
  labs(fill = NULL) +
  theme_bw() +
  ggtitle("") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        axis.title = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = NA))

# display and save the plot
fig1a
ggsave("output/fig1a.png", plot = fig1a, width = 8, height = 6, dpi = 600)


## Figure 1 (b): Total number of unclaimed deaths by month

# reshape the data from wide to long format
fig1b_df <- use_data %>%
  pivot_longer(
    cols = c(female_death_count, male_death_count, unknown_sex_death_count),
    names_to = "sex",
    values_to = "death_count_total"
  ) %>%
  mutate(sex = str_remove(sex, "_death_count")) %>%
  mutate(sex = str_remove(sex, "_sex")) %>%
  filter(death_count_total > 0) %>%
  mutate(month = month(death_date)) %>%
  group_by(month, sex) %>%
  summarize(death_count_total = sum(death_count_total), .groups = "drop")

# create a list of month abbreviations
months_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

fig1b <- ggplot(fig1b_df, 
                aes(fill = factor(sex, levels = c("unknown", "female", "male")),
                    x = factor(month, levels = 1:12), y = death_count_total)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  labs(fill = NULL) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#0072B2")) +
  ggtitle("") +
  ylab("") + guides(fill = FALSE) +
  theme_bw() +
  scale_x_discrete(labels = months_labels) +
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        plot.title = element_text(size = 20),
        axis.title = element_blank())

# display and save the plot
fig1b
ggsave("output/fig1b.png", plot = fig1b, width = 8, height = 6, dpi = 600)


## Figure 1 (c): Distribution of unclaimed deaths by age at death

# read in death records data
death_records <- read_csv("data/death_records.csv")

# drop records with missing age and add 0.5 to age to center the histogram
fig1c_df <- death_records %>%
  filter(!is.na(age)) %>%
  mutate(age = age + 0.5)

# create a histogram of age distribution by gender
fig1c <- ggplot(fig1c_df, 
                aes(fill = factor(imputed_sex, 
                                  levels = c("unknown", "female", "male")),
                    x = age)) +
  geom_histogram() +
  scale_fill_manual(values = c("#999999", "#E69F00", "#0072B2")) +
  ggtitle("") +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
  xlab("") + ylab("") +
  theme_bw() + guides(fill = FALSE) +
  scale_x_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
  labs(fill = NULL) +
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        plot.title = element_text(size = 20))

# display and save the plot
fig1c 
ggsave("output/fig1c.png", plot = fig1c, width = 8, height = 6, dpi = 600)

## Figure 1 (d): Distribution of daily number of unclaimed deaths

# top code death count at 10
fig1d_df <- use_data %>%
  mutate(death_count = ifelse(death_count > 10, 10, death_count))

fig1d <- ggplot(fig1d_df, aes(x = death_count)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 binwidth = 1, fill = "#00008B") +
  ggtitle("") +
  theme_bw() +
  xlab("") + ylab("") +
  # x label 10+ instead of 10
  scale_x_continuous(breaks = seq(from = 0, to = 10, by = 1),
                     labels = c(0:9, "10+")) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 20))

# display and save the plot
fig1d
ggsave("output/fig1d.png", plot = fig1d, width = 8, height = 6, dpi = 600)
