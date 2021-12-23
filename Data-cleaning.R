# Setting environment
library(tidyverse)
library(readxl)

setwd("D:/CHU 2.0/VegLab/Master/Data/Species")

# Load data and data pre-processing
TBN_raw = read_excel("./TBN_raw/TBN_plant_raw.xlsx")  # Command takes time to run, separate it 

TBN = TBN_raw %>% 
  ## Select specific columns I need from datasets
  select(eventDate, scientificName, vernacularName, 
         decimalLatitude, decimalLongitude, datasetName) %>% 
  ## Remove data without basic information
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude) & 
           !is.na(scientificName) & !is.na(datasetName) & !is.na(eventDate)) %>% 
  ## Remove duplicated datasets and those collected in human-disturbance environment
  filter(datasetName == "iNaturalist Research-grade Observations")

## Remove data which are not identify to species level
splt_name = TBN$scientificName %>% str_split(pattern = " ")
name_length = lapply(splt_name, FUN = function(x) length(unlist(x)))
TBN = TBN[-which(name_length <= 2),]

## Remove people's names in scientific name
for (n in 1:nrow(TBN)){
  name = TBN$scientificName[n] %>% str_split(pattern = " ") %>% unlist()
  
  if ("var." %in% name){
    TBN$scientificName[n] = paste(name[1], name[2], "var.", name[which(name == "var.") + 1], sep = " ")
  }else{
    TBN$scientificName[n] = paste(name[1], name[2], sep = " ")
  }
}

write.csv(TBN, "D:/CHU 2.0/Forest/110-1 Space Time data Viz/Term Project/TBN.csv")

