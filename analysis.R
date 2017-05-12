library(readr)
library(dplyr)
library(caret)
library(class)
library(glmnet)
library(rpart.plot)


load("/Users/kskatteboe/Dropbox (Personal)/MIT/15.071AnalyticsEdge/Data/data_clean.Rdata")
load("Data/data_clean.Rdata")


# CREATE TRAIN AND TEST -------------------------------
data_clean <- data_clean[complete.cases(data_clean$Average.Score..SAT.Math.), ]
data_clean <- data_clean %>% 
  rowwise() %>%
  mutate(average.sum.sat = sum(Average.Score..SAT.Math.,Average.Score..SAT.Reading.,Average.Score..SAT.Writing., na.rm=TRUE))


subset <- data_clean %>%
  dplyr::select(-School.ID, -School.Name, -Building.Code, -City, -Latitude, - Longitude, - grade_span_max, -grade_span_min, -Length,
         -overview_paragraph, -program_highlights, -language_classes, -online_ap_courses, -online_language_courses, - extracurricular_activities,
         -school_sports, -addtl_info1, -addtl_info2, -total_students, -Borough, -Zip.Code -Average.Score..SAT.Math., 
         -Average.Score..SAT.Reading.,-Average.Score..SAT.Writing.) %>%
  mutate(P_tech = as.numeric(P_tech),
         New_school = as.numeric(New_school))
subset$Average.Score..SAT.Math. <- NULL
subset$Zip.Code <- NULL
smp_size <- floor(0.60 * nrow(subset))

train_ind <- sample(seq_len(nrow(subset)), size = smp_size)
train <- subset[train_ind, ]
temp <- subset[-train_ind, ]
smp_size <- floor(0.5 * nrow(temp))
test_ind <- sample(seq_len(nrow(temp)), size = smp_size)
test <- temp[test_ind, ]
validate <- temp[-train_ind, ]

# PENALIZED REGRESSION -------------------------------

model <- lm(average.sum.sat ~Start.Time + End.Time + Student.Enrollment + Student.Enrollment + Percent.White +Percent.Black+Percent.Hispanic +
               Percent.Tested+Percent.Other+
               bus+subway+partner_cbo+partner_hospital+partner_highered+partner_cultural+partner_nonprofit+partner_corporate+
               partner_financial+partner_other+number_programs+accesible+school_years+CTE+same_gender+New_school+International_school+Specialized_school+
               P_tech+ESL_dual+ESL_transitional+partners+ap_history+ap_english+ap_biology+ap_chemistry+ap_calculus+ap_art+ap_politics+ap_psychology+ap_statistics+
               num_ap+sports+male_sports+female_sports+spanish+chines+french+basebal.men+basketbal.men+cross.men+soccer.men+track.men+volleybal.men+
               bowl.men+handbal.men+wrestl.men+footbal.men+golf.men+swim.men+tenni.men+basketbal.girls+cross.girls+soccer.girls+softbal.girls+track.girls+volleybal.girls+           
               bowl.girls+handbal.girls+tenni.girls+swim.girls, data = train)
summary(model)

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

mm.validate <- sparse.model.matrix(
  formula,
  data = validate
)

glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))
#using 10 fold croas validation to decide on lambda 
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_fit <- train(x = train[,1:73],
                    y = train$average.sum.sat,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid,
                    trControl = glmnet_ctrl,
                    family="gaussian")

plot(glmnet_fit)
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
r.2.reg

validate$y.hat.reg <- as.vector(predict(glmnet.model, s = glmnet_fit$bestTune$lambda, newx = mm.validate))
validate$rmse.reg <- (validate$y.hat.reg - validate$average.sum.sat)^2
validate$ss.tot <- (mean(validate$average.sum.sat) - validate$average.sum.sat)^2
mean(validate$rmse.reg)
r.2.reg.val <- 1 - sum(validate$rmse.reg)/sum(validate$ss.tot)
r.2.reg.val



# DECISION TREE -------------------------------
fit <- rpart(average.sum.sat ~ .,
             data=train,
             method="anova")
prp(fit)
prune.rpart.tree <- prune(fit, cp=0.02)
prp(prune.rpart.tree)

test$y.hat.tree <- as.vector(predict(fit, test, type="vector"))
test$rmse.tree <- (test$y.hat.tree - test$average.sum.sat)^2
mean(test$rmse.tree)
r.2.tree <- 1 - sum(test$rmse.tree)/sum(test$ss.tot)
r.2.tree

validate$y.hat.tree <- as.vector(predict(fit, validate, type="vector"))
validate$rmse.tree <- (validate$y.hat.tree - validate$average.sum.sat)^2
mean(validate$rmse.tree)
r.2.tree.val <- 1 - sum(validate$rmse.tree)/sum(validate$ss.tot)
r.2.tree.val

# RANDOM FORREST -------------------------------



# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(average.sum.sat ~ .,
                       data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
rf_gridsearch$bestTune$mtry

rf <- randomForest(average.sum.sat ~ .,
                        data=train, mtry = rf_gridsearch$bestTune$mtry, type = 'regression')
rf$importance

test$y.hat.rf <- as.vector(predict(rf, test))
test$rmse.rf <- (test$y.hat.rf - test$average.sum.sat)^2
mean(test$rmse.rf)
r.2.rf <- 1 - sum(test$rmse.rf)/sum(test$ss.tot)
r.2.rf

validate$y.hat.rf <- as.vector(predict(rf, validate))
validate$rmse.rf <- (validate$y.hat.rf - validate$average.sum.sat)^2
mean(validate$rmse.rf)
r.2.rf.val <- 1 - sum(validate$rmse.rf)/sum(validate$ss.tot)
r.2.rf.val

varImpPlot(rf,type=2)
