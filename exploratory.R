#rm(list=ls())

setwd("~/Dropbox (MIT)/MIT/Analytics Edge/Project")

#Libraries

library(dplyr)
library(chron)


#Read data

scores <- read.csv("Data/scores.csv", as.is = T)
directory <- read.csv("Data/DOE_High_School_Directory_2014-2015.csv", as.is = T)
programs <- read.csv("Data/DOE_High_School_Programs_2014-2015.csv", as.is = T)


#Notes:

#1. Lat and long seem healthy (all values fall in the similar range)
#2. All observations are from NY and are informed in the City and Zip code information

#Clean data

scores <- scores %>%
    mutate(Percent.White = as.numeric(gsub("%","",Percent.White)),
           Percent.Black = as.numeric(gsub("%","",Percent.Black)),
           Percent.Hispanic = as.numeric(gsub("%","",Percent.Hispanic)),
           Percent.Asian = as.numeric(gsub("%","",Percent.Asian)),
           Percent.Tested = as.numeric(gsub("%","",Percent.Tested)),
           aux = Percent.White + Percent.Black + Percent.Hispanic + Percent.Asian,
           Percent.Other = pmax(100-aux,0),
           Start.Time = times(paste0(gsub(" AM", '', Start.Time),":00")),
           End.Time = times(paste0(gsub(" PM", '', End.Time),":00"))+.5,
           Length.Aux = End.Time-Start.Time,
           Length = round(hours(scores2$Length)+minutes(scores2$Length)/60,2)) %>%
    select(-c(Phone.Number, State, aux, Start.Time, End.Time, Length.Aux))

sum(complete.cases(scores))

#trial <- scores %>%
    #left_join(directory, by = c("School.ID" = "dbn"))
#table(trial$boro, trial$Borough)
dif <- subset(trial, School.Name != school_name ) %>%
    select(School.Name, school_name)


#borough has same information as Borough in scores
#school names are identical for 338 records, the 97 remaining differ in capitalization, using articles, or other minor differences
#zip is identical for all cases
#se_services equal for all observations -> "This school will provide students with disabilities the supports and services indicated on their IEPs."


directory2 <- directory %>%
    mutate(School.ID = dbn,
           accesible = ifelse(school_accessibility_description == "Functionally Accessible",1,0)
           ) %>%
    select(-c(boro, school_name, building_code, phone_number, fax_number,
              expgrade_span_min, expgrade_span_max, state_code, zip, city, school_accessibility_description,
              se_services))


#First model

m <- lm(Average.Score..SAT.Math. ~ Percent.Black + Percent.Hispanic + Percent.Asian + Percent.White + Length, data = scores)

#.6692 accuracy