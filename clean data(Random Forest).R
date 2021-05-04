#importing dataset
clean_data = read.csv("D:/RT/Basecamp/CleanedDataset_SyS.csv")
selected_data = clean_data[-c(4:8,10:13)]

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
train_set[,14] = scale(train_set[,14])
test_set[,14] = scale(test_set[,14])

#fitting random forest on train_set
#install.packages('randomForest')
library(randomForest)
Random_classifier = randomForest(x = train_set[-1],
                          y = train_set$RuptureState,
                          ntree = 70)

#preidcting the test_set results
y_pred = predict(Random_classifier, newdata = test_set[-1])

#making confusion matrix
cm = table(test_set[,1], y_pred)

#Applying k-fold cross validation
#install.packages('caret')
library(caret)
folds = createFolds(train_set$RuptureState, k = 10) #class list of 10 accuracies
cv = lapply(folds, function(z) {
  train_fold = train_set[-z,]
  test_fold = train_set[z,]
  Random_classifier = randomForest(x = train_fold[-1],
                                   y = train_fold$RuptureState,
                                   ntree = 70)
  y_pred = predict(Random_classifier, newdata = test_fold[-1])
  cm = table(test_fold[,1], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[1,1] + cm[1,2] + cm[2,1])
  return(accuracy)
})
mean_accuracy = mean(as.numeric(cv))