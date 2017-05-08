library(readr)
library(dplyr)
library(caret)
library(class)
library(glmnet)
set.seed(123)

load("/Users/kskatteboe/Dropbox (Personal)/MIT/15.071AnalyticsEdge/Data/data_clean.Rdata")

# CREATE TRAIN AND TEST -------------------------------
data_clean <- data_clean[complete.cases(data_clean$Average.Score..SAT.Math.), ]
data_clean <- data_clean %>% 
  rowwise() %>%
  mutate(average.sum.sat = sum(Average.Score..SAT.Math.,Average.Score..SAT.Reading.,Average.Score..SAT.Writing., na.rm=TRUE))

subset <- data_clean %>%
  dplyr::select(-School.ID, -School.Name, -Building.Code, -City, -Latitude, - Longitude, - grade_span_max, -grade_span_min, -Length,
         -overview_paragraph, -program_highlights, -language_classes, -online_ap_courses, -online_language_courses, - extracurricular_activities,
         -school_sports, -addtl_info1, -addtl_info2, -total_students, - Borough, - Zip.Code -Average.Score..SAT.Math., 
         -Average.Score..SAT.Reading.,-Average.Score..SAT.Writing.) %>%
  mutate(P_tech = as.numeric(P_tech),
         New_school = as.numeric(New_school))

smp_size <- floor(0.60 * nrow(subset))
train_ind <- sample(seq_len(nrow(subset)), size = smp_size)
train <- subset[train_ind, ]
temp <- subset[-train_ind, ]
smp_size <- floor(0.5 * nrow(temp))
test_ind <- sample(seq_len(nrow(temp)), size = smp_size)
test <- temp[test_ind, ]
validate <- temp[-train_ind, ]

# PENALIZED REGRESSION -------------------------------
formula <-  ~ 0 + Start.Time + End.Time + Student.Enrollment + Student.Enrollment + Percent.White +Percent.Black+Percent.Hispanic +
  Percent.Tested+Percent.Other+
  bus+subway+partner_cbo+partner_hospital+partner_highered+partner_cultural+partner_nonprofit+partner_corporate+
  partner_financial+partner_other+number_programs+accesible+school_years+CTE+same_gender+New_school+International_school+Specialized_school+
  P_tech+ESL_dual+ESL_transitional+partners+ap_history+ap_english+ap_biology+ap_chemistry+ap_calculus+ap_art+ap_politics+ap_psychology+ap_statistics+
  num_ap+sports+male_sports+female_sports+spanish+chines+french+basebal.men+basketbal.men+cross.men+soccer.men+track.men+volleybal.men+
  bowl.men+handbal.men+wrestl.men+footbal.men+golf.men+swim.men+tenni.men+basketbal.girls+cross.girls+soccer.girls+softbal.girls+track.girls+volleybal.girls+           
  bowl.girls+handbal.girls+tenni.girls+swim.girls

mm.train <- sparse.model.matrix(
  formula,
  data = train
)
mm.test <- sparse.model.matrix(
  formula,
  data = test
)

glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))
#using 10 fold croas validation to decide on lambda 
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_fit <- train(x = train[,1:75],
                    y = train$average.sum.sat,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid,
                    trControl = glmnet_ctrl,
                    family="gaussian")

glmnet_fit$bestTune

glmnet.model <- glmnet(
  x = mm.train,
  y = train$average.sum.sat,
  family = "gaussian",
  alpha = glmnet_fit$bestTune$alpha,
  lambda = glmnet_fit$bestTune$lambda 
)

coef(glmnet.model)


test$y.hat.reg <- as.vector(predict(glmnet.model, s = glmnet_fit$bestTune$lambda, newx = mm.test))
test$rmse.reg <- (test$y.hat.reg - test$average.sum.sat)^2
test$ss.tot <- (mean(test$average.sum.sat) - test$average.sum.sat)^2
mean(test$rmse.reg)
r.2.reg <- 1 - sum(test$rmse.reg)/sum(test$ss.tot)

