#Wrangling SMFS 
library(tidyverse)
library(readr)
SMFS_05202023 <- read_csv("Data/SMFS_05202023.csv")
str(SMFS_05202023)
library(lubridate)
SMFS_05202023$Year<-year(SMFS_05202023$SampleDate)
library(lfstat)
SMFS_05202023$WaterYear<-water_year(SMFS_05202023$SampleDate, as.POSIX = FALSE, origin="usgs")

#Now I just want otter trawl data
SMFS_OTR_05202023<-SMFS_05202023 %>% filter(MethodCode %in% "OTR") #filters otter trawl samples from table

#Now I want to constrain the data to the years of interest (2011 to now)
str(SMFS_OTR_05202023)
SMFS_OTR_05202023$WaterYear<-as.character(SMFS_OTR_05202023$WaterYear)#when converting the factor to a number, need to first make it a character and THEN numeric. Otherwise, the number gets changed.
SMFS_OTR_05202023$WaterYear<-as.numeric(SMFS_OTR_05202023$WaterYear) #need to make water year numeric so I can filter out years easily.
SMFS_OTR_Thesis<-SMFS_OTR_05202023 %>% filter(WaterYear > 2010) #Filtering out years which overlap with South Bay Otter Trawl Survey for my thesis work.
FishCountTable<-SMFS_OTR_Thesis %>% group_by(OrganismCode) %>% summarise(Total=sum(Count))

FishCountTable<-SMFS_OTR_Thesis %>% group_by(OrganismCode, WaterYear) %>% summarise(Total=sum(Count))
FishCountTable %>% spread(WaterYear)
