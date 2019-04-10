library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(mlr)
library(tidyverse)
library(magrittr)
set.seed(100)

income <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

## Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]
#########################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################
## The Classification Task
theTask <- makeClassifTask(id = "taskname", data = income.train, target = 'high.earner')
#########################################################################################################################################################################################################################################################################################################################################################
## The 3-Fold Cross-Validation Strategy
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)
#########################################################################################################################################################################################################################################################################################################################################################
tuneMethod <- makeTuneControlRandom(maxit = 10L)
#########################################################################################################################################################################################################################################################################################################################################################
predAlg1 <- makeLearner("classif.rpart",predict.type = "response")
predAlg2 <- makeLearner("classif.glmnet",predict.type = "response")
predAlg3 <- makeLearner("classif.nnet",predict.type = "response")
predAlg4 <- makeLearner("classif.naiveBayes",predict.type = "response")
predAlg5 <- makeLearner("classif.kknn",predict.type = "response")
predAlg6 <- makeLearner("classif.svm",predict.type = "response")
#########################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################
modelParams1 <- makeParamSet(makeIntegerParam("minsplit",lower=10,upper=50),makeIntegerParam("minbucket",lower=5,upper=50),makeNumericParam("cp",lower=0.001,upper=0.2))
modelParams2 <- makeParamSet(makeNumericParam("lambda",lower=0,upper=3),makeNumericParam("alpha",lower=0,upper=1))
modelParams3 <- makeParamSet(makeIntegerParam("size",lower=1,upper=10),makeNumericParam("decay",lower=.1,upper=.5),makeIntegerParam("maxit",lower=1000,upper=1000))
modelParams5 <- makeParamSet(makeIntegerParam("k",lower=1,upper=30))
modelParams6 <- makeParamSet(makeDiscreteParam("kernel",values=c("radial")),makeDiscreteParam("cost",values=c(2^-2,2^-1,2^0,2^1,2^2,2^10)),makeDiscreteParam("gamma",values=c(2^-2,2^-1,2^0,2^1,2^2,2^10)))
tunedModel1 <- tuneParams(learner = predAlg1,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = list(f1,gmean),
                         par.set = modelParams1,
                         control = tuneMethod,
                         show.info = TRUE)
tunedModel2 <- tuneParams(learner = predAlg2,
                          task = theTask,
                          resampling = resampleStrat,
                          measures = list(f1,gmean),
                          par.set = modelParams2,
                          control = tuneMethod,
                          show.info = TRUE)
tunedModel3 <- tuneParams(learner = predAlg3,
                          task = theTask,
                          resampling = resampleStrat,
                          measures = list(f1,gmean),
                          par.set = modelParams3,
                          control = tuneMethod,
                          show.info = TRUE)
tunedModel5 <- tuneParams(learner = predAlg5,
                          task = theTask,
                          resampling = resampleStrat,
                          measures = list(f1,gmean),
                          par.set = modelParams5,
                          control = tuneMethod,
                          show.info = TRUE)
tunedModel6 <- tuneParams(learner = predAlg6,
                          task = theTask,
                          resampling = resampleStrat,
                          measures = list(f1,gmean),
                          par.set = modelParams6,
                          control = tuneMethod,
                          show.info = TRUE)
#########################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################
## The Six Learner Algorithms
  # Trees
predAlg1 <- setHyperPars(learner=predAlg1, par.vals = tunedModel1$x)
# Verify on CV sample sets
resample(predAlg1,theTask,resampleStrat,measures=list(f1,gmean))
# Train the final model
finalModel <- train(learner = predAlg1, task = theTask)
# Predict in test set!
prediction <- predict(finalModel, newdata = income.test)
out.of.sample1 <- mean(as.numeric(prediction$data$truth)-as.numeric(prediction$data$response))
#########################################################################################################################################################################################################################################################################################################################################################
  # Logistic Regression
predAlg2 <- setHyperPars(learner=predAlg2, par.vals = tunedModel2$x)
# Verify on CV sample sets
resample(predAlg2,theTask,resampleStrat,measures=list(f1,gmean))
# Train the final model
finalModel <- train(learner = predAlg2, task = theTask)
# Predict in test set!
prediction <- predict(finalModel, newdata = income.test)
out.of.sample2 <- mean(as.numeric(prediction$data$truth)-as.numeric(prediction$data$response))
#########################################################################################################################################################################################################################################################################################################################################################
  # Neural Network
predAlg3 <- setHyperPars(learner=predAlg3, par.vals = tunedModel3$x)
# Verify on CV sample sets
resample(predAlg3,theTask,resampleStrat,measures=list(f1,gmean))
# Train the final model
finalModel <- train(learner = predAlg3, task = theTask)
# Predict in test set!
prediction <- predict(finalModel, newdata = income.test)
out.of.sample3 <- mean(as.numeric(prediction$data$truth)-as.numeric(prediction$data$response))
#########################################################################################################################################################################################################################################################################################################################################################
  # Naive Bayes
# Train the final model
finalModel <- train(learner = predAlg4, task = theTask)
# Predict in test set!
prediction <- predict(finalModel, newdata = income.test)
out.of.sample4 <- mean(as.numeric(prediction$data$truth)-as.numeric(prediction$data$response))
#########################################################################################################################################################################################################################################################################################################################################################
# KNN
predAlg5 <- setHyperPars(learner=predAlg5, par.vals = tunedModel5$x)
# Verify on CV sample sets
resample(predAlg5,theTask,resampleStrat,measures=list(f1,gmean))
# Train the final model
finalModel <- train(learner = predAlg5, task = theTask)
# Predict in test set!
prediction <- predict(finalModel, newdata = income.test)
out.of.sample5 <- mean(as.numeric(prediction$data$truth)-as.numeric(prediction$data$response))
#########################################################################################################################################################################################################################################################################################################################################################
  # SVM
predAlg6 <- setHyperPars(learner=predAlg6, par.vals = tunedModel6$x)
# Verify on CV sample sets
resample(predAlg6,theTask,resampleStrat,measures=list(f1,gmean))
# Train the final model
finalModel <- train(learner = predAlg6, task = theTask)
# Predict in test set!
prediction <- predict(finalModel, newdata = income.test)
out.of.sample6 <- mean(as.numeric(prediction$data$truth)-as.numeric(prediction$data$response))
#########################################################################################################################################################################################################################################################################################################################################################

out.of.sample1
out.of.sample2
out.of.sample3
out.of.sample4
out.of.sample5
out.of.sample6
