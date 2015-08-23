library(caret)

training <- read.csv(file="pml-training.csv", header=TRUE, as.is = TRUE, stringsAsFactors = FALSE, sep=',', na.strings=c('NA','','#DIV/0!'))
evaluationData <- read.csv(file="pml-testing.csv", header=TRUE, as.is = TRUE, stringsAsFactors = FALSE, sep=',', na.strings=c('NA','','#DIV/0!'))

training$classe <- as.factor(training$classe)

validFeatures  <- colnames(training[colSums(is.na(training)) == 0])[-(1:7)]
procTraining  <- training[validFeatures]

set.seed(123456789)
inTrain = createDataPartition(procTraining$classe, p = 0.8, list=FALSE)
trainData = procTraining[inTrain,]
cvData = procTraining[-inTrain,]

rfModel <- train(classe ~., method="rf", data=trainData, trControl=trainControl(method='cv'), number=5, allowParallel=TRUE )

predTrain <- predict(rf, newdata=trainData)
confusionMatrix(predTrain,trainData$classe)

predCV <- predict(rf, newdata=cvData)
confusionMatrix(predCV,cvData$classe)


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

answers <- predict(rfModel, procEvData)
pml_write_files(answers)
answers