
# Random example on test set

library(caret)
library(kernlab)
library(dplyr)
library(tidyverse)
library(rpart)
library(randomForest)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(gbm)
library(ranger)

training_set <- read.csv("Data/pml-training.csv")
validation_set <- read.csv("Data/pml-testing.csv") # We leave this to the end

# CLEAN THE DATA

# Before we start we need to convert the variable "classe" to a factor

training_set$classe <- as.factor(training_set$classe)

# Remove rows that do not have any data 

# Take the training_set and sum the records where the data is either NA or empty.

KeepThrow <- colSums(is.na(training_set) |training_set=="")

# Keep the records where we have fields that have observations

Observ <- which(KeepThrow == 0)

training_set <- training_set[, Observ]
validation_set <- validation_set[, Observ]
# Remove first 7 observations as they do not help us in predicting the outcome of our study.

training_set <- training_set[, -c(1:7)] 
validation_set <- validation_set[, -c(1:7)]

inTrain <- createDataPartition(training_set$classe, 
                               p = 0.7, list = FALSE)

training <- training_set[inTrain, ]
testing <- training_set[-inTrain, ]

# Ok let's start with a fit of the model following the example laid
# out in Week 2, lecture 1.

#=======================================================
# Model 1: Classification Trees
#=======================================================

rcontrol <- rpart.control(xval = 10)
modelFit_1 <- rpart(classe ~ ., data = training, method = "class", control = rcontrol)


modelFit_1 # Look at model

fancyRpartPlot(modelFit_1)

# Look at the final model

# We can then validate it against our testing dataset

predictTreeMod1 <- predict(modelFit_1, testing, type = "class")
confusionMatrix(predictTreeMod1, testing$classe)


modelFit_1$finalModel

# Prediction

prediction_modelFit_1 <- predict(modelFit_1, newdata = testing)
prediction_modelFit_1

# Confusion Matrix

confusionMatrix(prediction_modelFit_1, testing$classe)
# Now speak about in and out of sample errors

#=======================================================
# Model 2: Random Forest:
#=======================================================

modelFit_2 <- randomForest(classe~., data = training)

?randomForest

str(modelFit_2)

class(modelFit_2$y)

ok <- confusionMatrix(modelFit_2$y, training$classe)
ok

print(modelFit_2)

?randomForest

# MeanSquaredError: Out of bag error rate 0.58%
# Number of variables randomly selected at each split: 7



plot(modelFit_2, main = "RF Model")

# Error rate decreases as we add more and more trees and average them.




print(modelFit_2)





predict1 <- predict(modelFit_2, testing)
confusionMatrix(predict1, testing$classe)

postResample(modelFit_2, actual)



default_rmse <- sqrt(modelFit_2$prediction.error)

qplot(modelFit_2$num.trees, modelFit_2$prediction.error)

default_rmse


plot(modelFit_2, main = "Random Forest Errors")

predict_FINAL <- predict(modelFit_2, validation_set, type = "class")
print(predict_FINAL)



# Using prediction to test

Pred_modelFit_2 <- predict(modelFit_2, testing, type = "class")

# Confusion matrix

confusionMatrix(Pred_modelFit_2, testing$classe)
# Now speak about in and out of sample errors




























