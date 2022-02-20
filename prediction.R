#prediction
library("caret")
library("tidyr")
library("dplyr")

training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
test <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

set.seed(123456)

#replace NA values
colNames <- names(test)
quitar <- c("X")

for(i in 1:length(colNames)){
  if(sum(is.na(test[colNames[i]]))>0){
    quitar <-c(quitar, colNames[i])
  }
}

training <- training[ , !(names(training) %in% quitar)]
test <- test[ , !(names(test) %in% quitar)]

entrenamiento <- createDataPartition(training$classe, p=0.7, list = FALSE)
trainModel <- training[entrenamiento,]
trainTest <- training[-entrenamiento,]

modelo <- train(classe ~ ., data = trainModel, method = "rf", trControl = trainControl(method = "cv", 5), ntree = 10)

modelo

predTrain <- predict(modelo, trainTest)
confusionMatrix(trainTest$classe, predTrain)

exactitud <- postResample(predTrain, trainTest$classe)
exactitud

predTest <- predict(modelo, test)
predTest

