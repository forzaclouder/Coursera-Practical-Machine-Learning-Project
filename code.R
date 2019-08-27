library(caret)


traindata<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", na.strings=c("","NA", "#DIV/0!"))
testdata<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", na.strings=c("","NA", "#DIV/0!"))

traindata<-traindata[,colSums(is.na(traindata))==0] 
testdata<-testdata[,colSums(is.na(testdata))==0] 

traindata<-traindata[,-c(1:7)]
testdata<-testdata[,-c(1:7)]

set.seed(2019) 
inTrain<-createDataPartition(traindata$classe,p=0.7,list=F)
trainingset<-traindata[inTrain,]
validationset<-traindata[-inTrain,]

control<-trainControl(method="cv", 5)
model<-train(classe~.,data=trainingset,method="rf",trControl=control,ntree=250)
vi <- varImp(model)$importance

predict<-predict(model,validationset)

accuracy<-postResample(predict,validationset$classe)

result<-predict(model,testdata[,-length(names(testdata))])

model
confusionMatrix(validationset$classe,predict)
vi[head(order(unlist(vi), decreasing = TRUE), 5L), , drop = FALSE]
accuracy
result
