#rm(list=ls())

setwd("~/Dropbox (MIT)/MIT/Analytics Edge/Project")

#Libraries

library(dplyr) 
library(chron)
library(ggmap)
library(tm) #text mining
library(SnowballC)
library(tau)
library(tidyr)


#Read data

scores <- read.csv("Data/scores.csv", as.is = T, na.strings=c("","NA"))
programs <- read.csv("Data/DOE_High_School_Programs_2014-2015.csv", as.is = T, header = T)
directory <- read.csv("Data/DOE_High_School_Directory_2014-2015.csv", as.is = T)


#Notes:

#1. Lat and long seem healthy (all values fall in the similar range)
#2. All observations are from NY and are informed in the City and Zip code information

#Clean data

#1. Remove observations without score (60)

scores <- subset(scores, complete.cases(scores))

#2. Parse variables to correct format and create variable for length of school day

scores <- scores %>%
    mutate(Percent.White = as.numeric(gsub("%","",Percent.White)),
           Percent.Black = as.numeric(gsub("%","",Percent.Black)),
           Percent.Hispanic = as.numeric(gsub("%","",Percent.Hispanic)),
           Percent.Asian = as.numeric(gsub("%","",Percent.Asian)),
           Percent.Tested = as.numeric(gsub("%","",Percent.Tested)),
           aux = Percent.White + Percent.Black + Percent.Hispanic + Percent.Asian,
           Percent.Other = round(pmax(100-aux,0),2),
           Start.Time = times(paste0(gsub(" AM", '', Start.Time),":00")),
           End.Time = times(paste0(gsub(" PM", '', End.Time),":00"))+.5,
           Length.Aux = End.Time-Start.Time,
           Length = round(hours(Length.Aux)+minutes(Length.Aux)/60,2)) %>%
    select(-c(Phone.Number, State, aux, Length.Aux, Street.Address))

sum(complete.cases(scores))

#
trial <- scores %>%
    left_join(directory, by = c("School.ID" = "dbn"))

#table(trial$boro, trial$Borough)
dif <- subset(trial, School.Name != school_name ) %>%
    select(School.Name, school_name)


#borough has same information as Borough in scores
#school names are identical for 338 records, the 97 remaining differ in capitalization, using articles, or other minor differences
#zip is identical for all cases
#se_services equal for all observations -> "This school will provide students with disabilities the supports and services indicated on their IEPs."

# same-gender
# CTE
# Consortium
# New School
# International
# Specialized school
# P-Tech


directory2 <- directory %>%
    mutate(School.ID = dbn,
           accesible = ifelse(school_accessibility_description == "Functionally Accessible",1,0),
           bus = ifelse(bus == "N/A",0,1),
           subway = ifelse(subway == "N/A",0,1),
           CTE = as.numeric(grepl("CTE",school_type)),
           same_gender = as.numeric(grepl("All-",school_type)),
           New_school = as.numeric(grepl("New",school_type)),
           International_school = as.numeric(grepl("International",school_type)),
           Specialized_school = as.numeric(grepl("Specialized", school_type)),
           P_tech = as.numeric(grepl("P-Tech", school_type)),
           ESL_dual = as.numeric(grepl(pattern = "Dual",x = directory$ell_programs)),
           ESL_transitional = as.numeric(grepl(pattern = "Transitional",x = directory$ell_programs))
           ) %>%
    select(-c(dbn, boro, school_name, building_code, phone_number, fax_number,
              campus_name, 
              expgrade_span_min, expgrade_span_max, state_code, zip, city, school_accessibility_description,
              website, primary_address_line_1, school_type,
              se_services, start_time, end_time, ell_programs,
              priority01:priority10, Location.1))


#First model

m <- lm(Average.Score..SAT.Math. ~ Percent.Black + Percent.Hispanic + Percent.Asian + Percent.White + Length, data = scores)

#.6692 accuracy


#MAP

map <- get_map(location = c(min_lon,min_lat,max_lon,max_lat))

max_lat <- max(scores$Latitude)
min_lat <- min(scores$Latitude)
max_lon <- max(scores$Longitude)
min_lon <- min(scores$Longitude)

mapPoints <- ggmap(map) +
    geom_point(aes(x = scores$Longitude, y = scores$Latitude, color = (scores$Average.Score..SAT.Math.)), data = scores, alpha = .5)
mapPoints


#Text analytics

clean_text <- function(variable, words.remove = "school"){
    # Step 1: Convert our text to a corpus
    corpus = Corpus(VectorSource(variable))
    
    # Step 2: Change all the text to lower case.
    corpus = tm_map(corpus, tolower)
    
    # Step 3: Remove all punctuation.
    corpus = tm_map(corpus, removePunctuation)
    
    # Step 4: Remove stop words.  
    corpus = tm_map(corpus, removeWords, c(words.remove, stopwords("english")))
    
    # Step 5: Stem our document 
    corpus = tm_map(corpus, stemDocument)
    
    # Step 6: Create a word count matrix (rows are emails, columns are words)
    #dtm = DocumentTermMatrix(corpus)
    matrix <- DocumentTermMatrix(corpus,control=list(tokenize=tokenize_ngrams))
    
    return(matrix)
}

#Languages

directory$language_classes <- gsub("\\s*\\([^\\)]+\\)","",as.character(directory2$language_classes))
languages <- clean_text(directory$language_classes, c("language","arts")) 
findFreqTerms(languages, lowfreq=25)
s_languages = removeSparseTerms(languages, 0.90)
text = as.data.frame(as.matrix(s_languages))

#Extra curricular activites

#directory$extracurricular_activities <- gsub("\\s*\\([^\\)]+\\)","",as.character(directory2$language_classes))
extraCurricular <- clean_text(directory$extracurricular_activities)
findFreqTerms(extraCurricular, lowfreq=25)
s_extraCurricular = removeSparseTerms(extraCurricular, 0.75)
text = as.data.frame(as.matrix(s_extraCurricular))

tokenize_ngrams <- function(x, n=3) 
    return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))
matrix <- DocumentTermMatrix(corpus,control=list(tokenize=tokenize_ngrams))

#additional

addInfo <- clean_text(directory$overview_paragraph)
findFreqTerms(addInfo, lowfreq=50)
aux = removeSparseTerms(addInfo, .90)
text = as.data.frame(as.matrix(addInfo))


#### PROGRAMS ####

# Number of programs is accurate

program_number <- programs %>%
    filter(dbn != 'dbn') %>%
    group_by(dbn, interest) %>%
    summarise(number = n()) %>%
    spread(interest, number)

program_number[is.na(program_number)] <- 0
