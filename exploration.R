library(readr)
library(dplyr)
library(stringr)
library(caret)
library(class)
library(klaR)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(psych)
library(corrplot)
set.seed(123)

clean_data <- read_csv("~/Dropbox (Personal)/MIT/15.071AnalyticsEdge/Data/cleaned_data.csv")

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
# CREATE VARIABLES FOR AP CLASSES -------------------------------
clean_data <- clean_data %>%
  mutate(num_ap = ifelse(is.na(str_count(advancedplacement_courses, ',')), 0,  
                               str_count(advancedplacement_courses, ',') + 1),
         ap_history = ifelse(is.na(str_count(advancedplacement_courses, "History")),0,
                             str_count(advancedplacement_courses, "History") + 1),
         ap_calculus = ifelse(is.na(str_count(advancedplacement_courses, "Calculus")),0,
                              str_count(advancedplacement_courses, "Calculus") + 1),
         ap_english = ifelse(is.na(str_count(advancedplacement_courses, "English")),0,
                             str_count(advancedplacement_courses, "English") + 1),
         ap_spanish = ifelse(is.na(str_count(advancedplacement_courses, "Spanish")),0,
                           str_count(advancedplacement_courses, "Spanish") + 1),
         ap_psych = ifelse(is.na(str_count(advancedplacement_courses, "Psychology")),0,
                           str_count(advancedplacement_courses, "Psychology") + 1),
         ap_chem = ifelse(is.na(str_count(advancedplacement_courses, "Chemistry")),0,
                           str_count(advancedplacement_courses, "Chemistry") + 1),
         ap_physics = ifelse(is.na(str_count(advancedplacement_courses, "Physics")),0,
                           str_count(advancedplacement_courses, "Physics") + 1),
         ap_art = ifelse(is.na(str_count(advancedplacement_courses, "Art")),0,
                             str_count(advancedplacement_courses, "Art") + 1),
         ap_politics = ifelse(is.na(str_count(advancedplacement_courses, "Politics")),0,
                         str_count(advancedplacement_courses, "Politics") + 1),
         ap_bio = ifelse(is.na(str_count(advancedplacement_courses, "Biology")),0,
                         str_count(advancedplacement_courses, "Biology") + 1),
         ap_econ = ifelse(is.na(str_count(advancedplacement_courses, "Economics")),0,
                         str_count(advancedplacement_courses, "Economics") + 1)
         
  )

# CREATE VARIABLES FOR SPORTS -------------------------------
clean_data <- clean_data %>%
  mutate(num_boy_sport = ifelse(is.na(str_count(psal_sports_boys, ',')), 0,  
                         str_count(psal_sports_boys, ',') + 1),
         num_girl_sport = ifelse(is.na(str_count(psal_sports_girls, ',')), 0,  
                                str_count(psal_sports_girls, ',') + 1),
         num_coed_sport = ifelse(is.na(str_count(psal_sports_coed, ',')), 0,  
                                str_count(psal_sports_coed, ',') + 1),
         num_total_sport = num_boy_sport + num_girl_sport + num_coed_sport,
         basket_boy = ifelse(is.na(str_count(psal_sports_boys, 'Basketball')), 0,  
                             str_count(psal_sports_boys, 'Basketball') + 1),
         baseball_boy = ifelse(is.na(str_count(psal_sports_boys, 'Baseball')), 0,  
                             str_count(psal_sports_boys, 'Baseball') + 1),
         track_boy = ifelse(is.na(str_count(psal_sports_boys, 'Track')), 0,  
                             str_count(psal_sports_boys, 'Track') + 1),
         xc_boy = ifelse(is.na(str_count(psal_sports_boys, 'Cross Country')), 0,  
                             str_count(psal_sports_boys, 'Cross Country') + 1),
         soccer_boy = ifelse(is.na(str_count(psal_sports_boys, 'Soccer')), 0,  
                             str_count(psal_sports_boys, 'Soccer') + 1),
         tennis_boy = ifelse(is.na(str_count(psal_sports_boys, 'Tennis')), 0,  
                             str_count(psal_sports_boys, 'Tennis') + 1),
         volleyball_boy = ifelse(is.na(str_count(psal_sports_boys, 'Volleyball')), 0,  
                             str_count(psal_sports_boys, 'Volleyball') + 1),
         fotball_boy = ifelse(is.na(str_count(psal_sports_boys, 'Football')), 0,  
                             str_count(psal_sports_boys, 'Football') + 1),
         basket_girl = ifelse(is.na(str_count(psal_sports_girls, 'Basketball')), 0,  
                              str_count(psal_sports_girls, 'Basketball') + 1),
         soccer_girl = ifelse(is.na(str_count(psal_sports_girls, 'Soccer')), 0,  
                              str_count(psal_sports_girls, 'Soccer') + 1),
         track_girl = ifelse(is.na(str_count(psal_sports_girls, 'Track')), 0,  
                              str_count(psal_sports_girls, 'Track') + 1),
         volleyball_girl = ifelse(is.na(str_count(psal_sports_girls, 'Volleyball')), 0,  
                              str_count(psal_sports_girls, 'Volleyball') + 1),
         tennis_girl = ifelse(is.na(str_count(psal_sports_girls, 'Tennis')), 0,  
                              str_count(psal_sports_girls, 'Tennis') + 1),
         softball_girl = ifelse(is.na(str_count(psal_sports_girls, 'Softball')), 0,  
                              str_count(psal_sports_girls, 'Softball') + 1)
         
  )

# CREATE VARIABLES FOR LANGUAGES -------------------------------
clean_data <- clean_data %>%
  mutate(num_language = ifelse(is.na(str_count(language_classes, ',')), 0,  
                                str_count(language_classes, ',') + 1),
         num_french = ifelse(is.na(str_count(psal_sports_boys, 'French')), 0,  
                             str_count(psal_sports_boys, 'French') + 1),
         num_spanish = ifelse(is.na(str_count(psal_sports_boys, 'Spanish')), 0,  
                               str_count(psal_sports_boys, 'Spanish') + 1),
         num_latin = ifelse(is.na(str_count(psal_sports_boys, 'Latin')), 0,  
                              str_count(psal_sports_boys, 'Latin') + 1)
  )

# JOIN WITH SCORES -------------------------------
data_additional_variables <- scores %>%
  left_join(clean_data, by = c("School.ID" = "dbn")) %>%
  mutate(total.score = sum(Average.Score..SAT.Math., Average.Score..SAT.Reading., Average.Score..SAT.Writing.))

write.csv(data_additional_variables, '~/Dropbox (Personal)/MIT/15.071AnalyticsEdge/Data/data_additional_variables.csv')
# PRELIMINARY ANALYSIS -------------------------------
hist.data <- subset %>%
  dplyr::select(Percent.Hispanic, Percent.Other, Percent.Tested, Average.Score..SAT.Math., Average.Score..SAT.Reading.,
                Average.Score..SAT.Writing., average.sum.sat)

multi.hist(hist.data[,sapply(hist.data, is.numeric)])

hist.data.1 <- subset %>%
  mutate( total.sport = num_boy_sport + num_girl_sport + num_coed_sport) %>%
  dplyr::select(num_ap, number_programs, num_boy_sport, num_girl_sport, num_coed_sport, total.sport)

multi.hist(hist.data.1[,sapply(hist.data.1, is.numeric)])

t <- subset %>%
  dplyr::select(-subway, -New_school)
         
M <- cor(t)
corrplot(M, type = "upper", tl.pos = "td",
         method = "square", tl.cex = 0.5, tl.col = 'black', diag = FALSE)
rm(t)

# CREATE TRAIN AND TEST -------------------------------
## 75% of the sample size
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
subset <- mutate(subset,
                 P_tech = as.numeric(P_tech),
                 New_school = as.numeric(New_school))

smp_size <- floor(0.60 * nrow(subset))
train_ind <- sample(seq_len(nrow(subset)), size = smp_size)
train <- subset[train_ind, ]
temp <- subset[-train_ind, ]
smp_size <- floor(0.5 * nrow(temp))
test_ind <- sample(seq_len(nrow(temp)), size = smp_size)
test <- temp[test_ind, ]
validate <- temp[-train_ind, ]

# CLUSTER -------------------------------
#normalize
normalized.train <- as.data.frame(lapply(train, normalize))
normalized.test <- as.data.frame(lapply(test, normalize))
normalized.validate <- as.data.frame(lapply(validate, normalize))


#find the correct number of clusters 
for(i in 1:length(withindiff)){
  c.r <-kmodes(train[,3:49], i, iter.max = 10, weighted = FALSE )
  withindiff[i] <- mean(c.r$withindiff)
}
plot(num.clus, withindiff, type="n", main=heading) 
lines(num.clus, withindiff, type='l') 

cluster.results.4 <-kmodes(train[,3:49], 4, iter.max = 10, weighted = FALSE )
train <- cbind(train, cluster = cluster.results.4$cluster )
#cluster on train and validation data as well 
c.r.4 <-kmodes(test[,3:49], 4, iter.max = 10, weighted = FALSE )
test <- cbind(test, cluster = c.r.4$cluster )
c.r.4 <-kmodes(validate[,3:49], 4, iter.max = 10, weighted = FALSE )
validate <- cbind(validate, cluster = c.r.4$cluster )

table(train$`cluster.results.4$cluster`)

write.csv(train, '~/Dropbox (Personal)/MIT/15.071AnalyticsEdge/Data/train_cluster.csv')

# GRAPHS  -------------------------------

###SCORES
graph.data <- dplyr::select(train, Average.Score..SAT.Reading., Average.Score..SAT.Math.,
                            Average.Score..SAT.Writing., average.sum.sat, cluster ) %>%
  group_by(cluster) %>%
  summarise(average.reading = mean(Average.Score..SAT.Reading.),
            average.math = mean(Average.Score..SAT.Math.),
            average.writing = mean(Average.Score..SAT.Writing.),
            average.total = mean(average.sum.sat)) %>%
  gather(variable, value, - cluster)

ggplot(graph.data, aes(variable, value)) +   
  geom_bar(aes(fill = as.factor(cluster)), position = "dodge", stat="identity") +
  labs(x = "Category", y = "Average Score") +
  ggtitle("Average Score by Cluster") + 
  guides(fill=guide_legend(title="Cluster")) + 
  scale_fill_brewer(palette="GnBu") +
  theme(axis.text=element_text(size=30, face="bold"),
        axis.title=element_text(size=22,face="bold"),
        plot.title = element_text(size = 40, face = "bold"),
        legend.text = element_text(size=24),
        legend.title =element_text(size=22,face="bold") ) 

### Ethnicity
graph.data <- dplyr::select(train, Percent.Hispanic, Percent.White,
                            Percent.Asian, Percent.Black, cluster ) %>%
  group_by(cluster) %>%
  summarise(percent.hispanic = mean(Percent.Hispanic),
            percent.white = mean(Percent.White),
            percent.asian = mean(Percent.Asian),
            percent.black = mean(Percent.Black)) %>%
  gather(variable, value, - cluster)

ggplot(graph.data, aes(variable, value)) +   
  geom_bar(aes(fill = as.factor(cluster)), position = "dodge", stat="identity") +
  labs(x = "Category", y = "Percentage") +
  ggtitle("Ethnicities by Cluster") + 
  guides(fill=guide_legend(title="Cluster")) + 
  scale_fill_brewer(palette="GnBu") +
  theme(axis.text=element_text(size=30, face="bold"),
        axis.title=element_text(size=22,face="bold"),
        plot.title = element_text(size = 40, face = "bold"),
        legend.text = element_text(size=24),
        legend.title =element_text(size=22,face="bold") )

### AP
graph.data <- dplyr::select(train, num_ap, ap_calculus,ap_spanish,
                            ap_calculus, ap_english, ap_physics, ap_history,
                            cluster ) %>%
  group_by(cluster) %>%
  summarise(total.ap = mean(num_ap),
            ap.calculus = mean(ap_calculus),
            ap.history = mean(ap_history),
            ap.english = mean(ap_english),
            ap.physics = mean(ap_physics),
            ap.spanish = mean(ap_spanish)) %>%
  gather(variable, value, - cluster)

ggplot(graph.data, aes(variable, value)) +   
  geom_bar(aes(fill = as.factor(cluster)), position = "dodge", stat="identity") +
  labs(x = "Category", y = "Average Number of AP Classes") +
  ggtitle("AP Classes by Cluster") + 
  guides(fill=guide_legend(title="Cluster")) + 
  scale_fill_brewer(palette="GnBu") +
  theme(axis.text=element_text(size=30, face="bold"),
        axis.title=element_text(size=22,face="bold"),
        plot.title = element_text(size = 40, face = "bold"),
        legend.text = element_text(size=24),
        legend.title =element_text(size=22,face="bold") )



# LOGISTIC REGRESSION -------------------------------
# RANDOM FORREST -------------------------------
# DECISION TREE -------------------------------

mean(subset$average.sum.sat)
mean(subset$Average.Score..SAT.Math.)
mean(subset$Average.Score..SAT.Reading.)
mean(subset$Average.Score..SAT.Writing.)