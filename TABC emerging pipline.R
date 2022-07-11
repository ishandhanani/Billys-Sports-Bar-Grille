##ECMT 673 Project 
##TABC subset EMERGING workflow
##This file is serves as a pipeline from the tabc_data.sas7bdat data
##Runs like the TABC pipeline but is specifically for emerging counties specified in the Population pipline
#This file contains references to Population Subset pipeline.R to pull relevant files
#It will not run unless that file is ran
#The source statement below setwd() should run the file if it exists in your wd

setwd("~/Desktop/OneDrive - Texas A&M University/Texas A&M Coursework/Year 3/Semester 1/ECMT 673 SAS/Project")
#source("Population Subset pipeline.R",local = TRUE)

##-------Read in libraries---------
library(readxl)
library(tidyverse)
library(writexl)
library(lubridate) #to extract year from dates

##---------------------Read in TABC data-----------------------
tabc1 = read_sas("tabc_data.sas7bdat")
#using the tabcInfo created in the population workflow, merge to have the names included in the TABC data
tabc1E = inner_join(tabc1, tabcInfo)
#subset TABC for only the counties in our county subset
tabcE = tabc%>%filter(CTYNAME %in% emergeSubset$CTYNAME)

#---------------------Create informative vars-----------------------------
#how long have they held beverage tax responsibility
#this gives rough estimate into the age of the bar
tabcE$resp_age = ifelse(is.na((tabcE$Responsibility_End_Date-tabcE$Responsibility_Begin_Date)/365),
                        (Sys.Date()-tabcE$Responsibility_Begin_Date)/365,
                        (tabcE$Responsibility_End_Date-tabcE$Responsibility_Begin_Date)/365)

#the bar is new if it started in 2001
tabcE$new = ifelse(year(tabcE$Responsibility_Begin_Date)==2021,"New","Old")

#the year the bars responsibility ended
#variable to show when it closed
tabcE$end_year = year(tabcE$Responsibility_End_Date)

#the bar is active if it does not have an end_year 
tabcE$active = ifelse(is.na(tabcE$Responsibility_End_Date),"Active","Expired")

#------------------------- Summaries on the establishments ------------------------
ageSummaryE = tabcE%>%group_by(CTYNAME)%>%
  summarise(mean_responsib_age = mean(resp_age))

oldNewSummaryE = tabcE%>%group_by(CTYNAME)%>%
  summarise(newEstabs = sum(new=="New"),
            oldEstabs = sum(new=="Old"))  

expirationSummaryE = tabcE%>%group_by(CTYNAME)%>%
  count(end_year)

activeExpSummary = tabcE%>%group_by(CTYNAME)%>%
  count(active)

cityLimitsSummaryE = tabcE%>%group_by(CTYNAME)%>%
  count(Location_City, Inside_Outside_City_Limits)

#---------------------------- Summary on Alc Sales ---------------------------------------
alcSummaryE = tabcE %>%group_by(CTYNAME)%>%summarise(meanLiquor = mean(liquor_receipts),
                                                     meanBeer = mean(beer_receipts),
                                                     meanWine = mean(wine_receipts),
                                                     meanTotal = mean(total_receipts))

#---------------------------- Number of Establishments per County -------------------------
tabcUniqueE <- tabcE%>%distinct(Taxpayer_Number,.keep_all = TRUE)

numEstabsPerCountyE = tabcUniqueE%>%group_by(CTYNAME)%>%
  count(CTYNAME)
