#Wrangling SMFS 
library(tidyverse)
library(rlang)
library(cowplot)
library(readr)
SMFS_09192023 <- read_csv("C:/Users/cnewe/OneDrive/Documents/Incubator/Code/FishForesightIncubator/Data/SMFS_09192023.csv")
str(SMFS_09192023)
library(lubridate)
SMFS_09192023$Year<-year(SMFS_09192023$SampleDate)
library(lfstat)
SMFS_09192023$WaterYear<-water_year(SMFS_09192023$SampleDate, as.POSIX = FALSE, origin="usgs")

#Now I just want otter trawl data
SMFS_OTR_09192023<-SMFS_09192023 %>% filter(MethodCode %in% "OTR") #filters otter trawl samples from table

#Now I want to constrain the data to the years of interest (2011 to now)
str(SMFS_OTR_09192023)
SMFS_OTR_09192023$WaterYear<-as.character(SMFS_OTR_09192023$WaterYear)#when converting the factor to a number, need to first make it a character and THEN numeric. Otherwise, the number gets changedfor some reason.
SMFS_OTR_09192023$WaterYear<-as.numeric(SMFS_OTR_09192023$WaterYear) #need to make water year numeric so I can filter out years easily.
SMFS_OTR_09192023$Year<-as.numeric(SMFS_OTR_09192023$Year) #need to make water year numeric so I can filter out years easily.

SMFS_OTR_Thesis<-SMFS_OTR_09192023 %>% filter(Year > 2010) #Filtering out years which overlap with South Bay Otter Trawl Survey for my thesis work.

#Visualizing fish counts by year for each species
FishCountTable<-SMFS_OTR_Thesis %>% group_by(OrganismCode) %>% summarise(Total=sum(Count))

FishCountTable<-SMFS_OTR_Thesis %>% group_by(OrganismCode, Year) %>% summarise(Total=sum(Count))

FishCountTable %>% spread(key = Year, value = Total)

#Adding zeros for sampling events with no fish caught
#https://derekogle.com/fishR/2018-04-19-Adding-Zero-Catches
length(unique(SMFS_OTR_Thesis$SampleRowID)) 
length(unique(SMFS_OTR_Thesis$TrawlRowID)) 
length(unique(SMFS_OTR_Thesis$OrganismCode))
3702*79


Nas_<-SMFS_OTR_Thesis %>% filter(TowDuration %in% NA)
NA_SampleRowIDs<-unique(Nas_$SampleRowID)
NA_Tow_Info<-SMFS_09192023 %>% filter(SampleRowID %in% NA_SampleRowIDs)
unique(NA_Tow_Info$SampleRowID)
#Okay I am deducing that these samples are actually fake samples with incidental or rod and reel info? 
#Removing all NA Trawl Duration (below)


#Why NA DO?
NoDO<-SMFS_OTR_Thesis %>% filter(DO %in% NA)
#Reasons for missing DO: 
#Row 81, 232, 253,  - just didn't record DO or PctSat
#Is it missing for other organisms from the same trawl?

NoDOTrawlID<-unique(NoDO$TrawlRowID)
length(NoDOTrawlID) #35 trawls
NoDOTrawls<-SMFS_OTR_Thesis %>% filter(TrawlRowID %in% NoDOTrawlID) #nope. The trawl just doesn't have the info! Remove these rows.

#Now checking salinity
NoSal<-SMFS_OTR_Thesis %>% filter(Salinity %in% NA)
NoSalTrawlID<-unique(NoSal$TrawlRowID)
length(NoSalTrawlID) #6 trawls
NoSalTrawls<-SMFS_OTR_Thesis %>% filter(TrawlRowID %in% NoSalTrawlID) #These trawls just doesn't have the info! Remove these rows.

#Now checking Secchi
NoSecchi<-SMFS_OTR_Thesis %>% filter(Secchi %in% NA)
NoSecchiTrawlID<-unique(NoSecchi$TrawlRowID) 
length(NoSecchiTrawlID) #3 trawls
NoSecchiTrawls<-SMFS_OTR_Thesis %>% filter(TrawlRowID %in% NoSecchiTrawlID) #These trawls just doesn't have the info! Remove these rows.

#Now checking Water Temp
NoWT<-SMFS_OTR_Thesis %>% filter(WaterTemperature %in% NA)
NoWTTrawlID<-unique(NoWT$TrawlRowID) 
length(NoWTTrawlID) #5 trawls
NoWTTrawls<-SMFS_OTR_Thesis %>% filter(TrawlRowID %in% NoWTTrawlID) #These trawls just doesn't have the info! Also missing all other WQ. Remove these rows.

#NOTE: There is info for other wq variables. So I am losing some amount of good info here... 
SMFS_OTR_Thesis<-SMFS_OTR_Thesis %>% filter(!TowDuration %in% NA, !DO %in% NA, !Salinity %in% NA, !Secchi %in% NA, !WaterTemperature %in% NA, !TideCode %in% NA, !StationCode %in% NA) #Remove NA rows for Tow Duration, DO, Salinity, Secchi, and Water Temp (Important WQ). Also removed NA's for other variables of interest for good measure. 
which(is.na(SMFS_OTR_Thesis$TowDuration)) #Tow Duration NAs removed.


#Now we add zeros! For every sampling event (unique sample row ID) there should be catch data for each species (unique organism code). 

SMFS_OTR_Thesis_WithZeros<-SMFS_OTR_Thesis %>% 
  complete(nesting(SampleRowID, 
                   WaterTemperature, 
                   Secchi, 
                   DO, 
                   Salinity, 
                   MethodCode, 
                   StationCode, 
                   SampleDate, 
                   SampleTime, 
                   PctSaturation, 
                   SpecificConductance, 
                   TideCode, 
                   ElecCond, 
                   TowDuration, 
                   TrawlComments, 
                   Year, 
                   WaterYear), 
           OrganismCode, 
           fill=list(num=0)) %>% as.data.frame() #Didn't add 0 values...Also ended up with more rows than expected? I have more rows than expected because there are multiple SIZES which each get their own row as well. So if I care about size/age classes, will have to bin those first and then add zeros. For now, move forward here.

SMFS_OTR_Thesis_WithZeros$Count[is.na(SMFS_OTR_Thesis_WithZeros$Count)] <- 0
#NOW it has zeros

#Check to see if it worked by counting number of rows for each sampling event (unique tow ID) and make sure it has a minimum of 79 accounts (79 unique organism codes in SMFS_OTR_Thesis)
Check<-SMFS_OTR_Thesis_WithZeros %>% group_by(SampleDate, StationCode) %>% summarize(n=n())
unique(Check$n) #We have 3693 trawls! We have at least 79 accounts per trawl (but that also doesn't make sense if we have at least one row with non-zero counts yeah?)
#How to check that we didn't make up trawls?
CheckCheck<- SMFS_OTR_Thesis %>% group_by(SampleDate, StationCode) %>% summarize(n=n()) #Same number of rows so that is good.

#you can also use spread(..... fill = 0) to add zeros to every sample x spp combo that doesn't exist or have data and then gather() it back up.

#Now I want to look at fish and their frequencies of catch over water quality parameters. 

which(is.na(SMFS_OTR_Thesis$WaterTemperature)) #THIS IS A PROBLEM (for later). Use this link to fix: https://www.tutorialspoint.com/dealing-with-missing-data-in-r#:~:text=Finding%20Missing%20Data%20in%20R&text=We%20can%20use%20the%20is,otherwise%20it%20should%20be%20False.

#adding month real quick
SMFS_OTR_Thesis_WithZeros$Month<-month(SMFS_OTR_Thesis_WithZeros$SampleDate)

Tester<-SMFS_OTR_Thesis[SMFS_OTR_Thesis$SampleRowID %in% "{E4889AE1-DD10-4A96-A1C2-E0BFFEB6FCBF}",]
Tester2<-SMFS_OTR_Thesis_WithZeros[SMFS_OTR_Thesis_WithZeros$SampleRowID %in% "{E4889AE1-DD10-4A96-A1C2-E0BFFEB6FCBF}",]

SMFS_OTR_Thesis_WithZeros$CPUE<-SMFS_OTR_Thesis_WithZeros$Count / SMFS_OTR_Thesis_WithZeros$TowDuration

summary(SMFS_OTR_Thesis_WithZeros)

#Matching fish names
SMFS_OrganismsLookUp <- read_excel("Data/SMFS_OrganismsLookUp.xlsx")

SMFS<-left_join(SMFS_OTR_Thesis_WithZeros, SMFS_OrganismsLookUp, by = "OrganismCode")

SMFS<-mutate(SMFS, gensp = paste(Genus, Species, sep=" "))


#Save New DF
write.csv(SMFS, "C:/Users/cnewe/OneDrive/Documents/Incubator/Code/FishForesightIncubator/Data/SMFS_OTR_Thesis_WithZeros.csv")




  

