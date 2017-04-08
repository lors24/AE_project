#rm(list=ls())

setwd("~/Dropbox (MIT)/MIT/Analytics Edge/Project")

#Libraries

library(dplyr)


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
           Percent.Other = pmax(100-aux,0)) %>%
    select(-c(Phone.Number, State, Start.Time, End.Time,aux))

sum(complete.cases(scores))

#trial <- scores %>%
    #left_join(directory, by = c("School.ID" = "dbn"))
