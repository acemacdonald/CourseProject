
training_set <- read.csv("Studying/OnlineStudying/MachineLearning/CourseProject/Data/pml-training.csv")

# validation_set <- read.csv("Data/pml-testing.csv")

validation_set <- read.csv("Studying/OnlineStudying/MachineLearning/CourseProject/Data/pml-testing.csv")

training_set$classe <- as.factor(training_set$classe)

# Take the training_set and sum the records where the data is either NA or empty.

KeepThrow <- colSums(is.na(training_set) |training_set=="")

# Keep the records where we have fields that have observations

Observ <- which(KeepThrow == 0)

training_set <- training_set[, Observ]

# Remove first 7 observations as they do not help us in predicting the outcome of our study.

training_set <- training_set[, -c(1:7)] 


inTrain <- createDataPartition(training_set$classe, 
                               p = 0.7, list = FALSE)

training <- training_set[inTrain, ]
testing <- training_set[-inTrain, ]



