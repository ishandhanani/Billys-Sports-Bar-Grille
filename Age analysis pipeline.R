##ECMT 673 Project 
##Analyzing age groups in selected counties
#This file contains references to Population Subset pipeline.R to pull relevant files
#It will not run unless that file is ran
#The source statement below setwd() should run the file if it exists in your wd

setwd("~/Desktop/OneDrive - Texas A&M University/Texas A&M Coursework/Year 3/Semester 1/ECMT 673 SAS/Project")
source("Population Subset pipeline.R",local = TRUE)

##-------Read in libraries---------
library(readxl)
library(tidyverse)
library(writexl)

##-------Add in age labels for the AGEGRP variable---------
ageVar = data.frame(AGEGRP = seq(4,10,by=1))
ageMap = data.frame(age = c("15-19",
                      "20-24",
                      "25-29",
                      "30-34",
                      "35-39",
                      "40-44",
                      "45-49"))
ageDF = bind_cols(ageVar,ageMap)
ageAnalysis = inner_join(popsubset_fulldata,ageDF)
ageAnalysis = ageAnalysis%>%relocate(age,.before = AGEGRP)

#export for analysis
write_xlsx(ageAnalysis, "ageAnalysis.xlsx")



