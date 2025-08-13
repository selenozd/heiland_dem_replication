
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Figure A1
### Author: Selen Ozdogan

## Setup 
rm(list = ls())
gc()

# run the libraries script to load (and install if necessaary) 
# all required packages
source("code/00_libraries.R")


## Read in the death records data
death_records <- read_csv("data/death_records.csv")

# Get death counts by sex
figa1a_df <- death_records %>% 
  mutate(death_year = year(death_date)) %>%
  group_by(death_year, sex) %>%
  summarize(death_count_total = n(),
            year = mean(death_year))


## Figure A1 (a): Total number of unclaimed deaths by year
figa1a <- ggplot(figa1a_df, 
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
figa1a
ggsave("output/appendix/figa1_a.png", plot = figa1a,
       width = 8, height = 6, dpi = 600)


## Figure A1 (b): Total number of unclaimed deaths by month

# Get death counts by month and sex
figa1b_df <- death_records %>% 
  mutate(month = month(death_date)) %>%
  group_by(month, sex) %>%
  summarize(death_count_total = n())

# create a list of month abbreviations
months_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

figa1b <- ggplot(figa1b_df, 
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
figa1b
ggsave("output/appendix/figa1_b.png", plot = figa1b, 
       width = 8, height = 6, dpi = 600)


## Figure A1 (c): Distribution of unclaimed deaths by age at death

# drop records with missing age and add 0.5 to age to center the histogram
figa1c_df <- death_records %>%
  filter(!is.na(age)) %>%
  mutate(age = age + 0.5)

# create a histogram of age distribution by gender
figa1c <- ggplot(figa1c_df, 
                aes(fill = factor(sex, 
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
figa1c 
ggsave("output/appendix/figa1_c.png", plot = figa1c, 
       width = 8, height = 6, dpi = 600)
