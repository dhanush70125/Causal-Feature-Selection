#importing dataset
clean_data = read.csv("D:/RT/Basecamp/CleanedDataset_SyS.csv")
selected_data = clean_data[-c(4:8,10:13)]

selected_data$localisation[selected_data$localisation == 3] <- 1
selected_data$localisation[selected_data$localisation == 9] <- 8
selected_data$localisation[selected_data$localisation == 10] <- 8

library(tidyverse)
library(dummies)

selected_data <- dummies::dummy.data.frame(
  as.data.frame(selected_data),
  names = c("localisation"),
  sep = "="
) %>%
  as_tibble()

#encoding categorical data
selected_data$RuptureState = as.factor(selected_data$RuptureState)
selected_data$MultipleAneurysm = as.factor(selected_data$MultipleAneurysm)
str(selected_data)

#splitting data
library(caTools)
set.seed(123)
split = sample.split(selected_data$RuptureState, SplitRatio = 0.7)
train_set = as.data.frame(subset(selected_data, split == TRUE))
test_set = as.data.frame(subset(selected_data, split == FALSE))

#feature scaling
train_set[,11] = scale(train_set[,11])
test_set[,11] = scale(test_set[,11])

#fitting logistic regression on train_set
logistic = glm(formula = RuptureState ~.,
               family = binomial,
               data = train_set)

#preidcting the test_set results
prob_pred = predict(logistic, type = 'response', newdata = test_set[-1]) #results in probabilites
y_pred = ifelse(prob_pred > 0.5, 1, 2) #predicted values

#making confusion matrix
cm = table(test_set[,1], y_pred)

#Applying k-fold cross validation
#install.packages('caret')
library(caret)
folds = createFolds(train_set$RuptureState, k = 10) #class list of 10 accuracies
cv = lapply(folds, function(x) {
  train_fold = train_set[-x,]
  test_fold = train_set[x,]
  logistic = glm(formula = RuptureState ~.,
                 family = binomial,
                 data = train_fold)
  prob_pred = predict(logistic, type = 'response', newdata = test_fold[-1]) #results in probabilites
  y_pred = ifelse(prob_pred > 0.5, 1, 0)
  cm = table(test_fold[,1], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[1,1] + cm[1,2] + cm[2,1])
  return(accuracy)
})
mean_accuracy = mean(as.numeric(cv))

