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
library(stringr)
library(readr)
library(caret)
library(class)
library(klaR)
library("RColorBrewer")
library(psych)
library(corrplot)


#Read data

scores <- read.csv("Data/scores.csv", as.is = T, na.strings=c("","NA"))
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


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
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

scores$start.hour <- 2
scores[scores$Start.Time<="8:00:00", "start.hour"] <- 1
scores[scores$Start.Time>"8:30:00","start.hour"] <- 3
scores$start.hour <- as.factor(scores$start.hour)

sum(complete.cases(scores))

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

#Program highlights
aux <- clean_text(directory$program_highlights,
                  c("student","students","now","school","include","includes","opportunity","program","programs",
                    "class","classes","classroom","grade")) 
aux = removeSparseTerms(aux, 0.85)
Highlights = as.data.frame(as.matrix(aux))


#Languages
directory$language_classes <- gsub("\\s*\\([^\\)]+\\)","",as.character(directory$language_classes))
aux <- clean_text(directory$language_classes, c("language","arts")) 
#findFreqTerms(aux, lowfreq=50)
aux = removeSparseTerms(aux, 0.90)
languages = as.data.frame(as.matrix(aux))

#Extracurricular activites
aux <- clean_text(directory$extracurricular_activities, c("student", "students")) 
#findFreqTerms(aux, lowfreq=50)
aux = removeSparseTerms(aux, 0.90)
extraCurricular = as.data.frame(as.matrix(aux))
names(extraCurricular) <- paste(names(extraCurricular),"extraC",sep =".")

#Sports male
aux <- clean_text(directory$psal_sports_boys) 
findFreqTerms(aux, lowfreq=50)
aux = removeSparseTerms(aux, 0.90)
sports_boys = as.data.frame(as.matrix(aux))
sports_boys$indoor <-NULL
sports_boys$outdoor <- NULL
sports_boys$countri <- NULL
names(sports_boys) <- paste(names(sports_boys),"boys",sep =".")

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
    mutate(School.ID = dbn,
           accesible = ifelse(school_accessibility_description == "Functionally Accessible",1,0),
           bus = ifelse(bus == "N/A",0,1),
           subway = ifelse(subway == "N/A",0,1),
           school_years = ifelse(grade_span_min != "K", as.numeric(grade_span_max)-as.numeric(grade_span_min)+1, as.numeric(grade_span_max)),
           #Type of school
           CTE = as.numeric(grepl("CTE",school_type)),
           same_gender = as.numeric(grepl("All-",school_type)),
           New_school = as.numeric(grepl("New",school_type)),
           International_school = as.numeric(grepl("International",school_type)),
           Specialized_school = as.numeric(grepl("Specialized", school_type)),
           P_tech = as.numeric(grepl("P-Tech", school_type)),
           ESL_dual = as.numeric(grepl(pattern = "Dual",x = directory$ell_programs)),
           ESL_transitional = as.numeric(grepl(pattern = "Transitional",x = directory$ell_programs)),
           #Ap classes
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
           num_ap = ifelse(nchar(apclass) != 0, str_count(apclass,',')+1,0),
           #sports
           sports = ifelse(nchar(psal_sports_boys) + nchar(psal_sports_girls) + nchar(psal_sports_coed)+nchar(school_sports)!= 0,1,0 ),
           male_sports = ifelse(nchar(psal_sports_boys)!=0, str_count(psal_sports_boys,",")+1,0),
           female_sports = ifelse(nchar(psal_sports_girls)!=0, str_count(psal_sports_girls,",")+1,0),
           #uniform
           uniform = as.numeric(grepl("uniform|dress|code", tolower(directory$addtl_info1))),
           #community service
           community.service = as.numeric(grepl("service", tolower(directory$addtl_info2))),
           #partners
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
              campus_name, overview_paragraph, online_ap_courses, online_language_courses,
              expgrade_span_min, expgrade_span_max, state_code, zip, city, school_accessibility_description,
              website, primary_address_line_1, school_sports, school_type,
              se_services, start_time, end_time, ell_programs,
              priority01:priority10, Location.1, total_students, grade_span_min, grade_span_max,
              advancedplacement_courses,apclass,
              psal_sports_boys, psal_sports_girls, psal_sports_coed,
              program_highlights, language_classes, extracurricular_activities, addtl_info1,
              addtl_info2))

directory <- cbind(directory,Highlights,languages,extraCurricular,sports_boys,sports_girls)

schools_NYC <- scores %>%
    left_join(directory,by = "School.ID")

save(schools_NYC, file = "Data/schools_NYC.Rdata")

#####   GRAPHS     ######

SAT.scores <- schools_NYC %>%
    select(School.ID,SAT.Math, SAT.Reading, SAT.Writing) %>%
    gather(SAT, Score, -School.ID)

ggplot(SAT.scores, aes(SAT, Score, fill = SAT))+geom_boxplot()+guides(fill = F)+scale_fill_brewer(palette="Paired")+
    ggtitle("SAT Scores distribution by section") + xlab("Section")

ggplot(schools_NYC, aes(1,SAT.Score))+geom_boxplot(fill= "light blue")+
    ggtitle("SAT Scores distribution") + xlab("")


#SAT Scores by Borough
ggplot(schools_NYC, aes(Borough, SAT.Score, fill = Borough))+geom_boxplot()+guides(fill = F)+scale_fill_brewer(palette="Paired")+
    ggtitle("SAT Scores distribution by Borough") + ylab("SAT Score")

#ggplot(scores, aes(Borough, SAT.Math, fill = Borough))+geom_boxplot()+guides(fill = F)
#ggplot(scores, aes(Borough, SAT.Writing, fill = Borough))+geom_boxplot()+guides(fill = F)
#ggplot(scores, aes(Borough, SAT.Reading, fill = Borough))+geom_boxplot()+guides(fill = F)



ggplot(schools_NYC, aes(Borough, Percent.Hispanic))+geom_boxplot()+guides(fill = F)

#Race

SAT.race <- schools_NYC %>%
    select(Borough, Percent.White, Percent.Black, Percent.Asian, Percent.Hispanic, SAT.Score) %>%
    gather(Race, Percent, -c(Borough,SAT.Score))

ggplot(SAT.race, aes(Percent, SAT.Score, color = Borough))+
    geom_point()+scale_colour_brewer(palette="Paired")+facet_wrap(SAT.Score~Race)

p1 <- ggplot(schools_NYC, aes(Percent.White, SAT.Score, color = Borough))+geom_point()+scale_colour_brewer(palette="Paired")+xlab("Percent White") + ylab("SAT Score") 
p2 <- ggplot(schools_NYC, aes(Percent.Black, SAT.Score, color = Borough))+geom_point()+scale_colour_brewer(palette="Paired")+xlab("Percent Black") + ylab("SAT Score") 
p3 <- ggplot(schools_NYC, aes(Percent.Hispanic, SAT.Score, color = Borough))+geom_point()+scale_colour_brewer(palette="Paired")+xlab("Percent Hispanic") + ylab("SAT Score") 
p4 <- ggplot(schools_NYC, aes(Percent.Asian, SAT.Score, color = Borough))+geom_point()+scale_colour_brewer(palette="Paired")+xlab("Percent Asian") + ylab("SAT Score") 

p1 <- ggplot(schools_NYC, aes(Percent.White, SAT.Score, color = Borough))+geom_point()+scale_colour_brewer(palette="Paired")+xlab("Percent White") + ylab("SAT Score") 



#Time at school
ggplot(schools_NYC, aes(Length, SAT.Score))+geom_point()

# Ap courses

ggplot(schools_NYC, aes(num_ap, SAT.Score))+geom_point()
ggplot(schools_NYC, aes(ap_calculus, SAT.Math))+geom_boxplot()
ggplot(schools_NYC, aes(as.factor(ap_physics), SAT.Math))+geom_boxplot()
ggplot(schools_NYC, aes(as.factor(ap_biology), SAT.Math))+geom_boxplot()


# PRELIMINARY ANALYSIS -------------------------------

hist.data <- schools_NYC %>% select(SAT.Score, SAT.Math, SAT.Reading, SAT.Writing,
                                    Student.Enrollment, Percent.White, Percent.Black, 
                                  Percent.Hispanic, Percent.Asian, Percent.Tested)
M <- cor(hist.data)
corrplot(M, type = "upper", tl.pos = "td",
         method = "square", tl.cex = 0.5, tl.col = 'black', diag = FALSE)                      

vars <- schools_NYC %>% select(Student.Enrollment, Percent.Tested, num_ap, Length)
multi.hist(vars)













