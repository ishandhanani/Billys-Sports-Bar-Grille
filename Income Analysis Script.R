##ECMT 673 Project 
##Analyzing PC income in selected counties
##This file is serves as a pipeline from the "CAINC30_TX_1969_2019.csv" data
#This file contains references to Population Subset pipeline.R to pull relevant files
#It will not run unless that file is ran
#The source statement below setwd() should run the file if it exists in your wd

setwd("~/Desktop/OneDrive - Texas A&M University/Texas A&M Coursework/Year 3/Semester 1/ECMT 673 SAS/Project")
source("Population Subset pipeline.R",local = TRUE)

##-------Read in libraries---------
library(readxl)
library(tidyverse)
library(writexl)
library(plotly)

#-----------------------Income data pre-processing------------------------------------------------
#Read in the data
income = read.csv("CAINC30_TX_1969_2019.csv")
#Subset to per capita personal income
income = income[income$Description==" Per capita personal income 4/",]

#Remove ",TX" and add " County"
income = income%>%separate(col = GeoName ,into = "GeoName", sep = ",")
CTYNAME = sub("$", " County", income$GeoName)
income$CTYNAME = CTYNAME
income = income%>%select(-GeoName)%>%relocate(CTYNAME, .before = Region)

#Subset income with selected counties
income = income%>%filter(CTYNAME %in% popsubset$CTYNAME)

#Drop unneccessary columns of years
income = income[,-c(9:49)]

#-----------------------Income Analysis------------------------------------------------
#Calculate Compound annual growth rate
income$CAGR = ((income$X2019-income$X2010)^(1/10))-1

#Get the most recent year
inc_summary = income[,c(2,18)]
#Create a variable predicting spending on alcohol
inc_summary$Spending_Amount = inc_summary$X2019*.01
#The 0.01 comes from BLS 

#-----------------------Merge income analysis with pop data------------------------------------------------
popIncMerge = inner_join(inc_summary, popsubsetInc)

#Create Predicted Spending variables
popIncMerge$Total_Spending = popIncMerge$Spending_Amount*popIncMerge$TOT_POP
popIncMerge$Male_Spending = popIncMerge$Spending_Amount*popIncMerge$TOT_MALE
popIncMerge$Female_Spending = popIncMerge$Spending_Amount*popIncMerge$TOT_FEMALE
#Rename second column
colnames(popIncMerge)[2]="Per Capita Personal Income 2019"



