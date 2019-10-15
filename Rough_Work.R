
# Machine Learning Project

# Goa: To predict the "classe" variable

# install relevant packages

install.packages("tidyverse")
install.packages("corrplot")
library(corrplot)
library(dplyr)
library(tidyverse)
library(caret)

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

# Import data

training_set <- read.csv("Data/pml-training.csv")
validation_set <- read.csv("Data/pml-testing.csv")

# Clean the data

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


# Split into training and test datasets

set.seed(1234) 
inTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)  # Change this to .75
trainData <- trainData[inTrain, ]
testData <- trainData[-inTrain, ]

# Remove variables that are near zero variance

NZV <- nearZeroVar(trainData)
trainData <- trainData[, -NZV]
testData  <- testData[, -NZV]
dim(trainData)

cor_mat <- cor(trainData[, -53])
corrplot(cor_mat, order = "FPC", method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))

highlyCorrelated = findCorrelation(cor_mat, cutoff=0.75)

names(trainData)[highlyCorrelated]

# Prediction with classification trees

decisionTreeMod1 <- rpart(classe ~ ., data=trainData, method="class")
fancyRpartPlot(decisionTreeMod1)



