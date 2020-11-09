#################################################
## NETWORK ANALYSIS ENSELBLES ###################
#################################################

library(plyr)
library(dplyr)
library(tidyverse)
library(zoo)


#################################################
# 1) Get list with chronology - d18O for all
#################################################

#falls du andere ensembles brauchst, musst du einfach hier die Suche der entities anpassen. Bitte year start und year stop bei analysis_year_* ändern, 
#weil das unten noch gebraucht wird. Dann einfach das Skript ausführen. Das sollte automatisch laufen. Allerdings 

analysis_year_start = 73500
analysis_year_stop = 14700

data <- load_sisal_data(year_start = analysis_year_start, year_stop = analysis_year_stop, min_period = 3000, min_dating = 3, min_d18O = 30)

ENSEMBLE <- list()

ENSEMBLE$CAVES_RAW <- data[[2]]
ENSEMBLE$ensembles <- list()
ENSEMBLE$names <- read_csv("/stacywork/ginnyweasley/02_SISAL/SISAL_v2/entity.csv") %>% select(entity_id, entity_name) %>%
  filter(entity_id %in% data[[1]]$entity_id)

rm_list = list()

print(".. read in ensembles")

counter <- list(entity_id = list(), noEnsemble = list())
counter_E <- 0

#for(entity in DATA_past1000$CAVES$entity_info$entity_id){
for(entity in data[[1]]$entity_id){
  print(entity)
  name = ENSEMBLE$names$entity_name[ENSEMBLE$names$entity_id == entity]
  sample <- read.csv("/stacywork/ginnyweasley/02_SISAL/SISAL_v2/sample.csv") %>% filter(entity_id == entity)
  org_chronology <- read.csv("/stacywork/ginnyweasley/02_SISAL/SISAL_v2/original_chronology.csv") %>% filter(sample_id %in% sample$sample_id) %>%
    filter(interp_age > analysis_year_stop & interp_age < analysis_year_start)# %>% filter(!sample_id %in% c(286696,286697))
  sample <- sample %>% filter(sample_id %in% org_chronology$sample_id)
  d18O <- read.csv("/stacywork/ginnyweasley/02_SISAL/SISAL_v2/d18o.csv") %>% filter(sample_id %in% sample$sample_id)
  d13C <- read.csv("/stacywork/ginnyweasley/02_SISAL/SISAL_v2/d13c.csv") %>% filter(sample_id %in% sample$sample_id)
  ens_all = list()
  for(chronology in c("Bacon", "Bchron", "copRa", "linInterp", "linReg")){
    if(file.exists(file = paste0("/stacywork/ariana/SISALv2_ensembles/",entity,"-",name,"-", chronology,".RData"))){
      if(entity == 1 && chronology == "linReg"){next}
      if(entity == 91 && chronology == "copRa"){next}
      
      rm_list  = c(rm_list, paste0(entity,"-",name,"-", chronology))
      ens_1000 = get(load(paste0("/stacywork/ariana/SISALv2_ensembles/",entity,"-",name,"-", chronology,".RData"))) %>% 
        filter(sample_id %in% sample$sample_id) %>% as.data.frame()
      if(is_empty(ens_all)){
        ens_all = ens_1000[c(1,3:dim(ens_1000)[2])]
      }else{
        if(dim(ens_1000)[1] == dim(ens_all)[2]){
          ens_all = cbind(ens_all, ens_1000[3:dim(ens_1000)[2]])
        }else if(dim(ens_1000)[1]<dim(ens_all)[1]){
          ens_1000 <- ens_1000 %>% filter(sample_id %in% ens_all$sample_id)
          ens_all <- ens_all %>% filter(sample_id %in% ens_1000$sample_id)
        }
        
      }
    }
  }
  if(is_empty(ens_all)){
    ENSEMBLE$ensembles[[paste0("ENTITY", entity, "_age")]] = org_chronology
  }else{
    org_chronology <- org_chronology %>% filter(sample_id %in% ens_all$sample_id)
    ENSEMBLE$ensembles[[paste0("ENTITY", entity, "_age")]] = left_join(ens_all,org_chronology, by = 'sample_id')
    ENSEMBLE$ensembles[[paste0("ENTITY", entity, "_samples")]] = left_join(d18O, d13C, by = 'sample_id') %>% 
      select(sample_id, d18O_measurement, d13C_measurement) %>% filter(sample_id %in% ens_all$sample_id)
    counter$entity_id = c(counter$entity_id, entity)
    counter$noEnsemble = c(counter$noEnsemble, dim(ens_all)[2])
  }
  
}

rm(list = as.character(rm_list))
rm(counter, d18O, d13C, ens_1000, ens_all, org_chronology, rm_list, sample)

save(ENSEMBLE, file = "ENSEMBLE.RData")


#################################################
## How to access the data:

entity = 30

data_ensembles  <- ENSEMBLE$ensembles[[paste0("ENTITY",entity,"_age")]]
data_sample <- ENSEMBLE$ensembles[[paste0("ENTITY",entity,"_samples")]]

plot(data_ensembles$interp_age, data_sample$d18O_measurement, type = "l", col = "red")
lines(data_ensembles$'346', data_sample$d18O_measurement)
lines(data_ensembles$'236', data_sample$d18O_measurement)
