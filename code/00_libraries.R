
### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

### Load (or Install) Required R Packages
### Author: Selen Ozdogan

# check for and install pacman if not already installed
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

# define and load standard libraries
packages <- c("tidyverse", "devtools", "furrr", "readr", "tibble", "dplyr",
             "tictoc", "glue", "lubridate", "magrittr", "janitor", "units", 
             "sjmisc", "fixest", "foreign", "haven", "readxl", "stringdist", 
             "ggpubr", "ggExtra", "sandwich", "modelsummary", "jtools", 
             "huxtable", "here", "stargazer", "MASS", "terra", "rgdal", 
             "raster", "gdalUtilities", "exactextractr", "sf", "tmap", 
             "tmaptools", "stars", "rmapshaper", "animation",
             "mapview", "ggmap", "geosphere", "cowplot", "fastDummies", 
             "patchwork", "forcats")

p_load(char = packages)
