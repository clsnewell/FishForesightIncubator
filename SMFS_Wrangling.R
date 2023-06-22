#Wrangling SMFS 
library(tidyverse)
library(rlang)
library(cowplot)
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
SMFS_OTR_05202023$WaterYear<-as.character(SMFS_OTR_05202023$WaterYear)#when converting the factor to a number, need to first make it a character and THEN numeric. Otherwise, the number gets changedfor some reason.
SMFS_OTR_05202023$WaterYear<-as.numeric(SMFS_OTR_05202023$WaterYear) #need to make water year numeric so I can filter out years easily.
SMFS_OTR_Thesis<-SMFS_OTR_05202023 %>% filter(WaterYear > 2010) #Filtering out water years which overlap with South Bay Otter Trawl Survey for my thesis work.
FishCountTable<-SMFS_OTR_Thesis %>% group_by(OrganismCode) %>% summarise(Total=sum(Count))

FishCountTable<-SMFS_OTR_Thesis %>% group_by(OrganismCode, WaterYear) %>% summarise(Total=sum(Count))
FishCountTable %>% spread(WaterYear)

#Now I want to look at fish and their frequencies of catch over water quality parameters. 

which(is.na(SMFS_OTR_Thesis$WaterTemperature)) #THIS IS A PROBLEM (for later). Use this link to fix: https://www.tutorialspoint.com/dealing-with-missing-data-in-r#:~:text=Finding%20Missing%20Data%20in%20R&text=We%20can%20use%20the%20is,otherwise%20it%20should%20be%20False.

# Create the catch frequency graph
#WaterTemp Graph for Tule Perch 2011-2023
SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "TP") %>% 
  ggplot(aes(x = WaterTemperature, y = Count)) +
  geom_col(position = "dodge") +
  labs(x = "Water Temp", y = "Catch Frequency", title = "Tule Perch Catch Frequency by Water Temp") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()



CatchFrequency <- function(data, x, y, title) {
  ggplot(data) +
    aes({{x}}, {{y}}) +
    geom_col(position = "dodge") +
    labs(x = deparse(substitute(x)), y = deparse(substitute(y)), title = title) +
    theme_minimal()
} #deparse automatically pulls the name from x and y to label the axes. This function is to make plotting more streamlined :)
#Water Temp graph for TP 2011-2023
WT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "TP", !is.na(WaterTemperature), !is.na(Count)) %>%
  CatchFrequency(WaterTemperature, Count, title = "TP & Water Temp SMFS 2011-2023")
#DO Graph for TP  2011-2023
DO.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "TP", !is.na(DO), !is.na(Count)) %>%
  CatchFrequency(DO, Count, title = "TP & DO SMFS 2011-2023")
#Salinity Graph for TP 2011-2023
PPT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "TP", !is.na(Salinity), !is.na(Count)) %>%
  CatchFrequency(Salinity, Count, title = "TP & Salinity SMFS 2011-2023")
#Secchi Graph for TP 2011-2023
Secchi.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "TP", !is.na(Secchi), !is.na(Count)) %>%
  CatchFrequency(Secchi, Count, title = "TP & Secchi SMFS 2011-2023")
# Arrange plots into a single image
plot_grid(WT.Plot, DO.Plot, PPT.Plot, Secchi.Plot, ncol = 2, labels = "AUTO")

#Water Temp graph for Striped Bass 2011-2023
WT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "SB", !is.na(WaterTemperature), !is.na(Count)) %>%
  CatchFrequency(WaterTemperature, Count, title = "SB & Water Temp SMFS 2011-2023")
#DO Graph for SB  2011-2023
DO.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "SB", !is.na(DO), !is.na(Count)) %>%
  CatchFrequency(DO, Count, title = "SB & DO SMFS 2011-2023")
#Salinity Graph for SB 2011-2023
PPT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "SB", !is.na(Salinity), !is.na(Count)) %>%
  CatchFrequency(Salinity, Count, title = "SB & Salinity SMFS 2011-2023")
#Secchi Graph for SB 2011-2023
Secchi.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "SB", !is.na(Secchi), !is.na(Count)) %>%
  CatchFrequency(Secchi, Count, title = "SB & Secchi SMFS 2011-2023")
# Arrange plots into a single image
plot_grid(WT.Plot, DO.Plot, PPT.Plot, Secchi.Plot, ncol = 2, labels = "AUTO")

#Water Temp graph for Prickly Sculpin 2011-2023
WT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "SCP", !is.na(WaterTemperature), !is.na(Count)) %>%
  CatchFrequency(WaterTemperature, Count, title = "SCP & Water Temp SMFS 2011-2023")
#DO Graph for SCP  2011-2023
DO.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "SCP", !is.na(DO), !is.na(Count)) %>%
  CatchFrequency(DO, Count, title = "SCP & DO SMFS 2011-2023")
#Salinity Graph for SCP 2011-2023
PPT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "SCP", !is.na(Salinity), !is.na(Count)) %>%
  CatchFrequency(Salinity, Count, title = "SCP & Salinity SMFS 2011-2023")
#Secchi Graph for SCP 2011-2023
Secchi.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "SCP", !is.na(Secchi), !is.na(Count)) %>%
  CatchFrequency(Secchi, Count, title = "SCP & Secchi SMFS 2011-2023")
# Arrange plots into a single image
plot_grid(WT.Plot, DO.Plot, PPT.Plot, Secchi.Plot, ncol = 2, labels = "AUTO")

#Water Temp graph for Longfin Smelt 2011-2023
WT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "LFS", !is.na(WaterTemperature), !is.na(Count)) %>%
  CatchFrequency(WaterTemperature, Count, title = "LFS & Water Temp SMFS 2011-2023")
#DO Graph for LFS  2011-2023
DO.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "LFS", !is.na(DO), !is.na(Count)) %>%
  CatchFrequency(DO, Count, title = "LFS & DO SMFS 2011-2023")
#Salinity Graph for LFS 2011-2023
PPT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "LFS", !is.na(Salinity), !is.na(Count)) %>%
  CatchFrequency(Salinity, Count, title = "LFS & Salinity SMFS 2011-2023")
#Secchi Graph for LFS 2011-2023
Secchi.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "LFS", !is.na(Secchi), !is.na(Count)) %>%
  CatchFrequency(Secchi, Count, title = "LFS & Secchi SMFS 2011-2023")
# Arrange plots into a single image
plot_grid(WT.Plot, DO.Plot, PPT.Plot, Secchi.Plot, ncol = 2, labels = "AUTO")

#Water Temp graph for Yellowfin Goby 2011-2023
WT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "YFG", !is.na(WaterTemperature), !is.na(Count)) %>%
  CatchFrequency(WaterTemperature, Count, title = "YFG & Water Temp SMFS 2011-2023")
#DO Graph for YFG  2011-2023
DO.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "YFG", !is.na(DO), !is.na(Count)) %>%
  CatchFrequency(DO, Count, title = "YFG & DO SMFS 2011-2023")
#Salinity Graph for YFG 2011-2023
PPT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "YFG", !is.na(Salinity), !is.na(Count)) %>%
  CatchFrequency(Salinity, Count, title = "YFG & Salinity SMFS 2011-2023")
#Secchi Graph for YFG 2011-2023
Secchi.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "YFG", !is.na(Secchi), !is.na(Count)) %>%
  CatchFrequency(Secchi, Count, title = "YFG & Secchi SMFS 2011-2023")
# Arrange plots into a single image
plot_grid(WT.Plot, DO.Plot, PPT.Plot, Secchi.Plot, ncol = 2, labels = "AUTO")

#Water Temp graph for American Shad 2011-2023
WT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "ASH", !is.na(WaterTemperature), !is.na(Count)) %>%
  CatchFrequency(WaterTemperature, Count, title = "ASH & Water Temp SMFS 2011-2023")
#DO Graph for ASH  2011-2023
DO.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "ASH", !is.na(DO), !is.na(Count)) %>%
  CatchFrequency(DO, Count, title = "ASH & DO SMFS 2011-2023")
#Salinity Graph for ASH 2011-2023
PPT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "ASH", !is.na(Salinity), !is.na(Count)) %>%
  CatchFrequency(Salinity, Count, title = "ASH & Salinity SMFS 2011-2023")
#Secchi Graph for ASH 2011-2023
Secchi.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "ASH", !is.na(Secchi), !is.na(Count)) %>%
  CatchFrequency(Secchi, Count, title = "ASH & Secchi SMFS 2011-2023")
# Arrange plots into a single image
plot_grid(WT.Plot, DO.Plot, PPT.Plot, Secchi.Plot, ncol = 2, labels = "AUTO")

#Water Temp graph for Threadfin Shad 2011-2023
WT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "TFS", !is.na(WaterTemperature), !is.na(Count)) %>%
  CatchFrequency(WaterTemperature, Count, title = "TFS & Water Temp SMFS 2011-2023")
#DO Graph for TFS  2011-2023
DO.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "TFS", !is.na(DO), !is.na(Count)) %>%
  CatchFrequency(DO, Count, title = "TFS & DO SMFS 2011-2023")
#Salinity Graph for TFS 2011-2023
PPT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "TFS", !is.na(Salinity), !is.na(Count)) %>%
  CatchFrequency(Salinity, Count, title = "TFS & Salinity SMFS 2011-2023")
#Secchi Graph for TFS 2011-2023
Secchi.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "TFS", !is.na(Secchi), !is.na(Count)) %>%
  CatchFrequency(Secchi, Count, title = "TFS & Secchi SMFS 2011-2023")
# Arrange plots into a single image
plot_grid(WT.Plot, DO.Plot, PPT.Plot, Secchi.Plot, ncol = 2, labels = "AUTO")

#Water Temp graph for Starry Flounder 2011-2023
WT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "SF", !is.na(WaterTemperature), !is.na(Count)) %>%
  CatchFrequency(WaterTemperature, Count, title = "SF & Water Temp SMFS 2011-2023")
#DO Graph for SF  2011-2023
DO.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "SF", !is.na(DO), !is.na(Count)) %>%
  CatchFrequency(DO, Count, title = "SF & DO SMFS 2011-2023")
#Salinity Graph for SF 2011-2023
PPT.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "SF", !is.na(Salinity), !is.na(Count)) %>%
  CatchFrequency(Salinity, Count, title = "SF & Salinity SMFS 2011-2023")
#Secchi Graph for SF 2011-2023
Secchi.Plot<-SMFS_OTR_Thesis %>% 
  filter(OrganismCode %in% "SF", !is.na(Secchi), !is.na(Count)) %>%
  CatchFrequency(Secchi, Count, title = "SF & Secchi SMFS 2011-2023")
# Arrange plots into a single image
plot_grid(WT.Plot, DO.Plot, PPT.Plot, Secchi.Plot, ncol = 2, labels = "AUTO")

