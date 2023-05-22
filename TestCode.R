#Testing!
library(readxl)
library(tidyverse)
SBOTS2019 <- read_excel("Data/SBOTS2019.xlsx")
SMFS2019 <- read_excel("Data/SMFS2019.xlsx")

#First let's compare the names of the columns
SMFS_Columns<-colnames(SMFS2019)
SBOTS_Columns<-colnames(SBOTS2019)
SMFS_Columns
SBOTS_Columns
#Matching Columns, Different Column names > New name:

#Now lets compare what is inside of the columns
SMFS_OrganismCodes<- unique(SMFS2019$OrganismCode)
SMFS_OrganismsLookUp <- read_excel("Data/SMFS_OrganismsLookUp.xlsx")

#I believe the codes from the 2019 match the codes in the organism look-up sheet. Let's double check by comparing the two lists.
identical(SMFS_OrganismsLookUp$OrganismCode, SMFS_OrganismCodes) #verified identical

str(SMFS_OrganismsLookUp)
SMFS_FishInfo<-SMFS_OrganismsLookUp %>% filter(Phylum %in% "Chordata") #Creating list of fish and retaining information about nativity, residence, taxonomy, common name
SMFS_NonFishInfo<-SMFS_OrganismsLookUp %>% filter(!(Phylum %in% "Chordata")) #Includes NO CATCH and WQ ONLY NO TRAWL and UNK which are useful for calculations later on! Also contains plants, bugs, shrimp, etc. UNK not certain a part of fish or something else.

SMFS_InvertsInfo<-SMFS_OrganismsLookUp %>% filter(Phylum %in% c("Arthropoda", "Mollusca", "Cnidaria"))

SMFS_PlantInfo<-SMFS_OrganismsLookUp %>% filter(Phylum %in% c("Chlorophyta", "Tracheophytes", "several"))

SMFS_DebrisInfo<-SMFS_OrganismsLookUp %>% filter(OrganismCode %in% c("DET", "MUD", "many", "CLAMSHELLS"))



SBOTS_FishCodes<-unique(SBOTS2019$code) #Oh dang this only has silversides in it.
#Will compare fish codes later!
#So I will pull out silversides from SMFS for the purposes of this joining effort.
SMFS_2019Silversides<-SMFS2019 %>% filter(OrganismCode%in%"ISS")

