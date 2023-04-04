#---------------------------------
####Libraries####
#---------------------------------

library(here)
library(tidyverse)
library(lubridate)
library(readxl)

#----------------------------------
#####Load data###
#----------------------------------

clearing_data <- read_excel(here("data","ClearingEfficiency_v1.xlsx")) %>% 
  mutate(Capture = if_else(Capture == "Y", true = 1,
                           false = if_else(Capture == "N", true = 0,
                                         false = NA_real_)))


