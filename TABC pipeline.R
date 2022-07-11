##ECMT 673 Project 
##TABC subset workflow
##This file is serves as a pipeline from the tabc_data.sas7bdat data
#This file contains references to Population Subset pipeline.R to pull relevant files
#It will not run unless that file is ran
#The source statement below setwd() should run the file if it exists in your wd

setwd("~/Desktop/OneDrive - Texas A&M University/Texas A&M Coursework/Year 3/Semester 1/ECMT 673 SAS/Project")
source("Population Subset pipeline.R",local = TRUE)

##-------Read in libraries---------
library(readxl)
library(tidyverse)
library(writexl)
library(lubridate) #to extract year from dates

##---------------------Read in TABC data-----------------------
tabc1 = read_sas("tabc_data.sas7bdat")
#using the tabcInfo created in the population workflow, merge to have the names included in the TABC data
tabc = inner_join(tabc1, tabcInfo)
#subset TABC for only the counties in our county subset
tabc = tabc%>%filter(CTYNAME %in% popsubset$CTYNAME)

#---------------------Create informative vars-----------------------------
#how long have they held beverage tax responsibility
#this gives rough estimate into the age of the bar
tabc$resp_age = ifelse(is.na((tabc$Responsibility_End_Date-tabc$Responsibility_Begin_Date)/365),
                       (Sys.Date()-tabc$Responsibility_Begin_Date)/365,
                       (tabc$Responsibility_End_Date-tabc$Responsibility_Begin_Date)/365)

#the bar is new if it started in 2001
tabc$new = ifelse(year(tabc$Responsibility_Begin_Date)==2021,"New","Old")

#the year the bars responsibility ended
#variable to show when it closed
tabc$end_year = year(tabc$Responsibility_End_Date)

#the bar is active if it does not have an end_year 
tabc$active = ifelse(is.na(tabc$Responsibility_End_Date),"Active","Expired")

#------------------------- Summaries on the establishments ------------------------
ageSummary = tabc%>%group_by(CTYNAME)%>%
  summarise(mean_responsib_age = mean(resp_age))

oldNewSummary = tabc%>%group_by(CTYNAME)%>%
  summarise(newEstabs = sum(new=="New"),
            oldEstabs = sum(new=="Old"))  

expirationSummary = tabc%>%group_by(CTYNAME)%>%
  count(end_year)

activeExpSummary = tabc%>%group_by(CTYNAME)%>%
  count(active)

cityLimitsSummary = tabc%>%group_by(CTYNAME)%>%
  count(Location_City, Inside_Outside_City_Limits)

#---------------------------- Summary on Alc Sales ---------------------------------------
alcSummary = tabc %>%group_by(CTYNAME)%>%summarise(meanLiquor = mean(liquor_receipts),
                                                   meanBeer = mean(beer_receipts),
                                                   meanWine = mean(wine_receipts),
                                                   meanTotal = mean(total_receipts))

#---------------------------- Number of Establishments per County -------------------------
tabcUnique <- tabc%>%distinct(Taxpayer_Number,.keep_all = TRUE)

numEstabsPerCounty = tabcUnique%>%group_by(CTYNAME)%>%
  count(CTYNAME)

