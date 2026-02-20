
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
             "raster", "gdalUtilities", "exactextractr", "sf", "tmap", "pandoc", 
             "tmaptools", "stars", "rmapshaper", "animation", "slider",
             "mapview", "ggmap", "geosphere", "cowplot", "fastDummies", 
             "patchwork", "forcats", "zoo", "gender", "openxlsx", "fixest")

# load or install packages using pacman
pacman::p_load(char = packages, install = TRUE, 
               update = getOption("pac_update"), character.only = FALSE)

