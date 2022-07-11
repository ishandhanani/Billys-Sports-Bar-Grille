##ECMT 673 Project 
##Population subset workflow
##This file is serves as a pipline from the texas_population.sas7bdat data
#Run this file before the other piplines are ran in order to reference sets made in this file

setwd("~/Desktop/OneDrive - Texas A&M University/Texas A&M Coursework/Year 3/Semester 1/ECMT 673 SAS/Project")

##-------Read in libraries---------
rm(list)
library(readxl)
library(tidyverse)
library(writexl)
library(plotly)
library(haven)

##-------Read in data and create year variable---------
popdata = read_sas("texas_population.sas7bdat")

YEAR = seq(3,12,by=1)
yearnum = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
yearMap = data.frame(YEAR,yearnum)
popdata = inner_join(popdata,yearMap) 
names(popdata)[names(popdata) == 'yearnum'] <- 'year'
names(popdata)[names(popdata) == 'COUNTY'] <- 'FIPS_Code'

##-------Read in county code mapping for TABC workflow---------
countyMapping = read_excel("County Code Bridge.xlsx")
popdata = inner_join(popdata,countyMapping)
tabcInfo = popdata%>%select(CTYNAME,TABC_Code)%>%distinct(CTYNAME,.keep_all = TRUE) #used for TABC workflow
names(tabcInfo)[names(tabcInfo) == 'TABC_Code'] <- 'Taxpayer_County'
rm(yearMap,countyMapping)
##-------Subset counties between 100k-300k in 2019-------------
popsubset=popdata%>%select(CTYNAME,TOT_POP,TOT_MALE,year,AGEGRP)%>%
                    filter((TOT_POP>=100000 & TOT_POP<=300000) & 
                             AGEGRP == 0 & 
                             year==2019)

#the subset below is used for the income analysis file
popsubsetInc=popdata%>%select(CTYNAME,TOT_POP,TOT_MALE,year,AGEGRP,TOT_FEMALE)%>%
                    filter((TOT_POP>=100000 & TOT_POP<=300000) & 
                            AGEGRP == 0 & 
                            year==2019)


popsubset_fulldata = popdata%>%filter(CTYNAME %in% popsubset$CTYNAME)

##-------View interactive plot of selected counties population growth trends----
ggplotly(ggplot(data=popsubset_fulldata%>%filter(AGEGRP==0),
                aes(x=year,y=TOT_POP,col=CTYNAME))+geom_line())

#ggplot(data=popsubset_fulldata%>%filter(AGEGRP==0, (CTYNAME=="Guadalupe County" | CTYNAME=="Midland County" | CTYNAME=="Rockwall County")),
       #aes(x=year,y=TOT_POP,col=CTYNAME))+geom_line()


##-------Find percent growth from 2010 to 2019-------------
gRate=data.frame(popsubset$CTYNAME) 
#Gathering population data from 2010
pop2010 = popsubset_fulldata%>%select(CTYNAME,TOT_POP,year,AGEGRP)%>%
                               filter(year==2010 & AGEGRP==0)%>%
                               select(CTYNAME,TOT_POP,year)%>%
                               rename(pop2010 = TOT_POP)
#Gathering population data from 2019
pop2019 = popsubset_fulldata%>%select(CTYNAME,TOT_POP,year,AGEGRP)%>%
                               filter(year==2019 & AGEGRP==0)%>%
                               select(CTYNAME,TOT_POP,year)%>%
                               rename(pop2019 = TOT_POP)
#Bind data into 1 DF and calculate percent change(F-I)/I
gRate=bind_cols(pop2010,pop2019)
gRate = gRate%>%select(CTYNAME...1, pop2010, pop2019)
gRate=gRate%>%mutate(perChange=((pop2019-pop2010)/pop2010))
rm(pop2010,pop2019)
#To view in highest to lowest rate, run this line
gRate%>%arrange(desc(perChange))


##-------Potential emerging candidates for investment ---------
#This section will find counties that were at 90k+ in 2019
#Further research will be done on these to see if these have untapped potential

##-------Subset counties between 90k-100k in 2019-------------
emergeSubset=popdata%>%select(CTYNAME,TOT_POP,TOT_MALE,year,AGEGRP)%>%
                       filter((TOT_POP>=90000 & TOT_POP<=100000) & 
                               AGEGRP == 0 & 
                               year==2019)

emergesubset_fulldata = popdata%>%filter(CTYNAME %in% emergeSubset$CTYNAME)

##-------Find percent growth from 2010 to 2019 for emerging-------------
gRateEm=data.frame(emergeSubset$CTYNAME) 
#Gathering population data from 2010
pop2010 = emergesubset_fulldata%>%select(CTYNAME,TOT_POP,year,AGEGRP)%>%
                                  filter(year==2010 & AGEGRP==0)%>%
                                  select(CTYNAME,TOT_POP,year)%>%
                                  rename(pop2010 = TOT_POP)
#Gathering population data from 2019
pop2019 = emergesubset_fulldata%>%select(CTYNAME,TOT_POP,year,AGEGRP)%>%
                               filter(year==2019 & AGEGRP==0)%>%
                               select(CTYNAME,TOT_POP,year)%>%
                               rename(pop2019 = TOT_POP)
#Bind data into 1 DF and calculate percent change(F-I)/I
gRateEm = bind_cols(pop2010,pop2019)
gRateEm = gRateEm%>%select(CTYNAME...1, pop2010, pop2019)
gRateEm = gRateEm%>%mutate(perChange=((pop2019-pop2010)/pop2010))
rm(pop2010,pop2019)
#To view in highest to lowest rate, run this line
gRateEm%>%arrange(desc(perChange))











