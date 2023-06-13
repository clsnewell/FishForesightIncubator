#Merging SMFS dataframes (in r instead of database)
#read in tables
library(readxl)
SMFS_Catch052023 <- read_excel("Data/SMFS_Catch052023.xlsx")
SMFS_Sample052023 <- read_excel("Data/SMFS_Sample052023.xlsx")
SMFS_TrawlEffort052023 <- read_excel("Data/SMFS_TrawlEffort052023.xlsx")

#Join them by SampleRowID
library(tidyverse)
Step1<-left_join(SMFS_Catch052023, SMFS_Sample052023, by="SampleRowID")
Step1
FullTable<-left_join(Step1, SMFS_TrawlEffort052023, by="SampleRowID")
write.csv(FullTable, "Data/SMFS_05202023.csv")

