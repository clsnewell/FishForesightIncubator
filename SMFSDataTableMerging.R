#Merging SMFS dataframes (in r instead of database)
#read in tables
library(readxl)
SMFS_Catch091923 <- read_excel("Data/SMFS_Catch091923.xlsx")
SMFS_Sample091923 <- read_excel("Data/SMFS_Sample091923.xlsx")
SMFS_TrawlEffort091923 <- read_excel("Data/SMFS_TrawlEffort091923.xlsx")

#Join them by SampleRowID
library(tidyverse)
Step1<-left_join(SMFS_Catch091923, SMFS_Sample091923, by="SampleRowID")
Step1
FullTable<-left_join(Step1, SMFS_TrawlEffort091923, by="SampleRowID")
write.csv(FullTable, "Data/SMFS_09192023.csv")

