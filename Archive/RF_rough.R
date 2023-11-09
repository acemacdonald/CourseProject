# Random Forests
#=====================

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

training_set<- training_set[, colSums(is.na(training_set)) == 0]
validation_set <- validation_set[, colSums(is.na(validation_set)) == 0]

# We do not need X1, user_name, raw_timestamp_part1, raw_timestamp_part2,
# cvtd_timestamp, new_window, num_window.

training_set <- training_set %>% select(-c("X","user_name","raw_timestamp_part_1",
                                           "raw_timestamp_part_2", "cvtd_timestamp",
                                           "new_window", "num_window"))
validation_set <- validation_set %>% select(-c("X","user_name","raw_timestamp_part_1",
                                               "raw_timestamp_part_2", "cvtd_timestamp",
                                               "new_window", "num_window"))


inTrain <- createDataPartition(training_set$classe, 
                               p = 0.7, list = FALSE)

training <- training_set[inTrain, ]
testing <- training_set[-inTrain, ]

# Remove near zero variance
NZV <- nearZeroVar(training)
trainData <- training[, -NZV]
testData  <- testing[, -NZV]

model_forest <- randomForest(classe ~., data = training,
                             mtry = 30,
                             importance = TRUE)


#===================
## Model 2: Random Forests

```{r rf_model2, echo=TRUE, results=FALSE}
modelFit_2<- randomForest(classe ~. , data=training)
```

Using prediction to test.

```{r pred_model2, echo=TRUE, results=FALSE}
Pred_modelFit_2 <- predict(modelFit_2, testing, type = "class")
```

```{r confmat2, echo=TRUE, results=TRUE}
confusionMatrix(Pred_modelFit_2, testing$classe)
```

## Model 3: Boosting

Let's fit the model using the _train_ function and setting method to "gbm".

```{r model3, echo=TRUE, results=TRUE}
modelFit_3 <- train(classe ~., method = "gbm", data = training, verbose = FALSE)
print(modelFit_3)
```

