#rm(list=ls())

setwd("~/Dropbox (Personal)/MIT/15.071AnalyticsEdge/")

#Libraries

library(dplyr) 
library(chron)
library(ggmap)
library(tm) #text mining
library(SnowballC)
library(tau)
library(tidyr)
library(stringr)


#Read data

scores <- read.csv("Data/scores.csv", as.is = T, na.strings=c("","NA"))
#programs <- read.csv("Data/DOE_High_School_Programs_2014-2015.csv", as.is = T, header = T)
directory <- read.csv("Data/DOE_High_School_Directory_2014-2015.csv", as.is = T)

#Functions: 

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
    matrix <- DocumentTermMatrix(corpus)
    
    return(matrix)
}

#Notes:

#1. Lat and long seem healthy (all values fall in the similar range)
#2. All observations are from NY and are informed in the City and Zip code information
#3. City is same as borough except for Queens which is subdivided

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
           before8 = as.numeric(Start.Time <= "8:00:00"),
           before830 = as.numeric(Start.Time > "8:00:00" & Start.Time <= "8:30:00"),
           after830 = as.numeric(Start.Time > "8:30:00"),
           Length.Aux = End.Time-Start.Time,
           Length = round(hours(Length.Aux)+minutes(Length.Aux)/60,2),
           SAT.Math = Average.Score..SAT.Math.,
           SAT.Reading = Average.Score..SAT.Reading.,
           SAT.Writing = Average.Score..SAT.Writing.,
           SAT.Score = SAT.Math + SAT.Writing + SAT.Reading
           ) %>%
    select(-c(Building.Code, City, Phone.Number, State, aux, Length.Aux, Street.Address,
              Average.Score..SAT.Math., Average.Score..SAT.Reading.,
              Average.Score..SAT.Writing.))

sum(complete.cases(scores))

#SAT Scores by Borough
ggplot(scores, aes(Borough, SAT.Score, fill = Borough))+geom_boxplot()+guides(fill = F)
#ggplot(scores, aes(Borough, SAT.Math, fill = Borough))+geom_boxplot()+guides(fill = F)
#ggplot(scores, aes(Borough, SAT.Writing, fill = Borough))+geom_boxplot()+guides(fill = F)
#ggplot(scores, aes(Borough, SAT.Reading, fill = Borough))+geom_boxplot()+guides(fill = F)

#Race

ggplot(scores, aes(Percent.White, SAT.Score, color = Borough))+geom_point()
ggplot(scores, aes(Percent.Black, SAT.Score, color = Borough))+geom_point()
ggplot(scores, aes(Percent.Hispanic, SAT.Score, color = Borough))+geom_point()
ggplot(scores, aes(Percent.Asian, SAT.Score, color = Borough))+geom_point()

#Time at school
ggplot(scores, aes(Length, SAT.Score))+geom_point()

#borough has same information as Borough in scores
#school names are identical for 338 records, the 97 remaining differ in capitalization, using articles, or other minor differences
#zip is identical for all cases
#se_services equal for all observations -> "This school will provide students with disabilities the supports and services indicated on their IEPs."

#Types of school:
# same-gender
# CTE
# Consortium
# New School
# International
# Specialized school
# P-Tech

# Remove variables that apepar in both tables and create new variables for type of school

directory <- directory %>%
    mutate(School.ID = dbn,
           accesible = ifelse(school_accessibility_description == "Functionally Accessible",1,0),
           bus = ifelse(bus == "N/A",0,1),
           subway = ifelse(subway == "N/A",0,1),
           school_years = ifelse(grade_span_min != "K", as.numeric(grade_span_max)-as.numeric(grade_span_min)+1, as.numeric(grade_span_max)),
           CTE = as.numeric(grepl("CTE",school_type)),
           same_gender = as.numeric(grepl("All-",school_type)),
           New_school = as.numeric(grepl("New",school_type)),
           International_school = as.numeric(grepl("International",school_type)),
           Specialized_school = as.numeric(grepl("Specialized", school_type)),
           P_tech = as.numeric(grepl("P-Tech", school_type)),
           ESL_dual = as.numeric(grepl(pattern = "Dual",x = directory$ell_programs)),
           ESL_transitional = as.numeric(grepl(pattern = "Transitional",x = directory$ell_programs)),
           partner_cbo = ifelse(nchar(partner_cbo)==0,0,1),
           partner_corporate = ifelse(nchar(partner_corporate)==0,0,1),
           partner_hospital = ifelse(nchar(partner_hospital)==0,0,1),
           partner_highered = ifelse(nchar(partner_highered)==0,0,1),
           partner_cultural = ifelse(nchar(partner_cultural)==0,0,1),
           partner_nonprofit = ifelse(nchar(partner_nonprofit)==0,0,1),
           partner_financial = ifelse(nchar(partner_financial)==0,0,1),
           partner_other = ifelse(nchar(partner_other)==0,0,1),
           partners = partner_corporate + partner_cbo+ partner_corporate +
                          partner_other+ partner_hospital+ partner_cultural+
                        partner_financial + partner_highered
           ) %>%
    select(-c(dbn, boro, school_name, building_code, phone_number, fax_number,
              campus_name, 
              expgrade_span_min, expgrade_span_max, state_code, zip, city, school_accessibility_description,
              website, primary_address_line_1, 
              se_services, start_time, end_time, ell_programs,
              priority01:priority10, Location.1))

#Extracurricular activites
aux <- clean_text(directory$extracurricular_activities) 
#findFreqTerms(aux, lowfreq=50)
aux = removeSparseTerms(aux, 0.90)
extraCurricular = as.data.frame(as.matrix(aux))

#Languages
directory$language_classes <- gsub("\\s*\\([^\\)]+\\)","",as.character(directory$language_classes))
aux <- clean_text(directory$language_classes, c("language","arts")) 
#findFreqTerms(aux, lowfreq=50)
aux = removeSparseTerms(aux, 0.90)
languages = as.data.frame(as.matrix(aux))

#Advanced placement courses
directory$advancedplacement_courses <- gsub("\\s*\\([^\\)]+\\)","",as.character(directory$advancedplacement_courses))
aux <- clean_text(directory$advancedplacement_courses, c("language","arts")) 
findFreqTerms(aux, lowfreq=50)
aux = removeSparseTerms(aux, 0.90)
ap_courses = as.data.frame(as.matrix(aux))

directory <- directory %>%
    mutate(
        apclass = tolower(gsub("\\s*\\([^\\)]+\\)","",as.character(advancedplacement_courses))),
        apclass= gsub("n/a","",apclass),
        ap_history = as.numeric(grepl("history",apclass)),
        ap_english = as.numeric(grepl("english",apclass)),
        ap_biology = as.numeric(grepl("biology",apclass)),
        ap_chemistry = as.numeric(grepl("chemistry",apclass)),
        ap_calculus = as.numeric(grepl("calculus",apclass)),
        ap_art = as.numeric(grepl("art",apclass)),
        ap_politics = as.numeric(grepl("politics", apclass)),
        ap_spanish = as.numeric(grepl("spanish",apclass)),
        ap_physics = as.numeric(grepl("physics",apclass)),
        ap_environmental = as.numeric(grepl("environmental",apclass)),
        ap_psychology = as.numeric(grepl("psychology",apclass)),
        ap_statistics = as.numeric(grepl("statistics",apclass)),
        num_ap = ifelse(nchar(apclass) != 0, str_count(apclass,',')+1,0)
    ) %>% select(-c(advancedplacement_courses,apclass))

#Sports male
aux <- clean_text(directory$psal_sports_boys) 
findFreqTerms(aux, lowfreq=50)
aux = removeSparseTerms(aux, 0.90)
sports_men = as.data.frame(as.matrix(aux))
sports_men$indoor <-NULL
sports_men$outdoor <- NULL
sports_men$countri <- NULL
names(sports_men) <- paste(names(sports_men),"men",sep =".")

#Sports female
aux <- clean_text(directory$psal_sports_girls) 
findFreqTerms(aux, lowfreq=50)
aux = removeSparseTerms(aux, 0.90)
sports_girls = as.data.frame(as.matrix(aux))
sports_girls$countri <-NULL
sports_girls$indoor <- NULL
sports_girls$outdoor <- NULL
names(sports_girls) <- paste(names(sports_girls),"girls",sep =".")

directory <- directory %>%
    mutate(sports = ifelse(nchar(psal_sports_boys) + nchar(psal_sports_girls) + nchar(psal_sports_coed)+nchar(school_sports)!= 0,1,0 ),
           male_sports = ifelse(nchar(psal_sports_boys)!=0, str_count(psal_sports_boys,",")+1,0),
           female_sports = ifelse(nchar(psal_sports_girls)!=0, str_count(psal_sports_girls,",")+1,0)
           ) %>%
    select(-c(psal_sports_boys, psal_sports_girls, psal_sports_coed))

directory <- cbind(directory, languages,sports_men,sports_girls)

data_clean <- scores %>%
    left_join(directory,by = "School.ID")

save(data_clean, file = "Data/data_clean.Rdata")

#####







######## MODELING ############
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





#New data
new.data <- read.csv("Data/data_additional_variables.csv",as.is = T)

new.data <- new.data %>%
    left_join()




data_additional_variables <- data_additional_variables[complete.cases(data_additional_variables$Average.Score..SAT.Math.), ]
data_additional_variables <- data_additional_variables %>% 
    rowwise() %>%
    mutate(average.sum.sat = sum(Average.Score..SAT.Math.,Average.Score..SAT.Reading.,Average.Score..SAT.Writing., na.rm=TRUE))

subset <- data_additional_variables %>%
    select(Start.Time, End.Time, Student.Enrollment, Percent.Asian, Percent.White, Percent.Black,
           Percent.Hispanic, Percent.Other, Percent.Tested, Average.Score..SAT.Math., Average.Score..SAT.Reading.,
           Average.Score..SAT.Writing., bus, subway, same_gender, New_school, International_school, P_tech, num_ap,
           ap_history, ap_calculus, ap_english, ap_spanish, ap_physics, ap_psych, ap_chem, ap_art, ap_politics, ap_bio,
           ap_econ, number_programs, num_boy_sport, num_girl_sport, num_coed_sport, basket_boy, basket_girl, baseball_boy, 
           softball_girl, track_boy, track_girl, xc_boy, soccer_boy, soccer_girl, volleyball_boy, volleyball_girl, tennis_boy,
           tennis_girl, fotball_boy, average.sum.sat)




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
aux = removeSparseTerms(addInfo, .75)
text = as.data.frame(as.matrix(aux))


#### PROGRAMS ####

# Number of programs is accurate

program_number <- programs %>%
    filter(dbn != 'dbn') %>%
    group_by(dbn, interest) %>%
    summarise(number = n()) %>%
    spread(interest, number)

program_number[is.na(program_number)] <- 0





######

aux <- clean_text(directory$overview_paragraph) 
aux = removeSparseTerms(aux, 0.85)
Aux = as.data.frame(as.matrix(aux))

