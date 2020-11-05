## Nils Skript for SISAL data extraction:

library(plyr)
library(dplyr)
library(tidyverse)

source("Functions/SISAL_extracting.R")

data <- load_sisal_data(year_start = 73500, year_stop = 14700, min_period = 3000, min_dating = 3, min_d18O = 30)

#This contains all information on the entity: site_id, entity_id, latitude, longitude, elevation, geology, cover_thickness, distance_entrance
entity_info <- data[[1]]
#This includes all d18O samples including the original dating (interp_age). You can link the sample_info to the entity_info via the entity_id
sample_info <- data[[2]]
#This includes the ensemble means of all age-model realizations availabe from Carla and published with the SISALv2 database
dating_info <- data[[3]]


#You link this together as follows: e.g. say you want entity_id 30 with 

entity = 30

data_rec_sample <- sample_info%>% filter(entity_id == entity)
data_rec_dating <- dating_info %>% filter(entity_id == entity)

# example TS for a different dating than original chronology

example_TS <- zoo::zoo(x = data_rec_sample$d18O_measurement, order.by = data_rec_dating$Bchron_age)

#Here you can compare your age models to the original
plot(example_TS)
lines(data_rec_sample$interp_age, data_rec_sample$d18O_measurement, col = "red")


