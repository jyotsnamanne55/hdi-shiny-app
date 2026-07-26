#loading data
files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
files

library(tidyverse)

hdi_data <- files |> 
#results
glimpse(hdi_data)
#countries
unique(hdi_data$country_name)
