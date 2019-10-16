
# Random example on test set

install.packages("rpart.plot")

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

training_set <- read.csv("Data/pml-training.csv")
validation_set <- read.csv("Data/pml-testing.csv") # We leave this to the end

# CLEAN THE DATA

# Before we start we need to convert the variable "classe" to a factor

training_set$classe <- as.factor(training_set$classe)

# Remove rows that do not have any data 

trainData<- training_set[, colSums(is.na(training_set)) == 0]
validData <- validation_set[, colSums(is.na(validation_set)) == 0]

# We do not need X1, user_name, raw_timestamp_part1, raw_timestamp_part2,
# cvtd_timestamp, new_window, num_window.

trainData <- trainData %>% select(-c("X","user_name","raw_timestamp_part_1",
                                     "raw_timestamp_part_2", "cvtd_timestamp",
                                     "new_window", "num_window"))
validData <- validData %>% select(-c("X","user_name","raw_timestamp_part_1",
                                     "raw_timestamp_part_2", "cvtd_timestamp",
                                     "new_window", "num_window"))

# Remove variables that are almost always NA

mostlyNA <- sapply(trainData, function(x) mean(is.na(x))) > 0.95
trainData <- trainData[, mostlyNA==F]


NZV <- nearZeroVar(trainData)
trainData <- trainData[, -NZV]

dim(trainData)



inTrain <- createDataPartition(trainData$classe, 
                               p = 0.7, list = FALSE)

training <- trainData[inTrain, ]
testing <- trainData[-inTrain, ]


# Ok let's start with a fit of the model following the example laid
# out in Week 2, lecture 1.

#=======================================================
# Model 1: Classification Trees
#=======================================================

modelFit_1 <- rpart(classe ~ ., data = training, method = "class")

modelFit_1 # Look at model

fancyRpartPlot(modelFit_1)

# Look at the final model

# We can then validate it against our testing dataset

predictTreeMod1 <- predict(modelFit_1, testing, type = "class")
cmtree <- confusionMatrix(predictTreeMod1, testing$classe)
cmtree

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

modelFit_2<- randomForest(classe ~. , data=training)

plot(modelFit_2, main = "Random Forest Errors")

# Using prediction to test

Pred_modelFit_2 <- predict(modelFit_2, testing, type = "class")

# Confusion matrix

confusionMatrix(Pred_modelFit_2, testing$classe)
# Now speak about in and out of sample errors
































