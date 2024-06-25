#Merging SMFS dataframes (in r instead of database). Not included but needed later: Depth, Organism Lookup, Station Lookup, AgesBySizeMonth
#read in tables
library(readxl)
SMFS_Catch062424 <- read_excel("Data/SMFS_Catch062424.xlsx")
SMFS_Sample062424 <- read_excel("Data/SMFS_Sample062424.xlsx")
SMFS_TrawlEffort062424 <- read_excel("Data/SMFS_TrawlEffort062424.xlsx")

#Join them by SampleRowID
library(tidyverse)
Step1<-left_join(SMFS_Catch062424, SMFS_Sample062424, by="SampleRowID")
Step1
FullTable<-left_join(Step1, SMFS_TrawlEffort062424, by="SampleRowID")
write.csv(FullTable, "Data/SMFS_062424.csv")

