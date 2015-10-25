library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
##Load data from file
setwd("~/R/predict")
datatrain0<-read.csv("pml-training (2).csv", na.strings = c(NA,"","DIV/0!"))
datatest<-read.csv("pml-testing (1).csv")
## Prelucrate data 
datatrain1<-datatrain0[,colSums(is.na(datatrain0))==0]
datatest1<-datatest[,colSums(is.na(datatest))==0]

datatrain1<-datatrain1[,-(1:6)]

#Preproces trainig test
intrain<-createDataPartition(y=datatrain1$classe,p=0.7,list=FALSE)
training2<-datatrain1[intrain,]
testing2<-datatrain1[-intrain,]
# Matrix for record performance
performance<-matrix(data=NA,nrow = 2,ncol=3)
colnames(performance)<-c("RandomForest(RandomForest)","RandomForest(Caret)","Tree(rpart)")
row.names(performance)<-c("Time/Speed", "Accuracy")

## Model1 Random Forest with package randomForest
procesortime<-proc.time()
model1<-randomForest(classe~.,data=training2,methods="class")
t<-proc.time()-procesortime
performance[1,1]<-t[1]
#summary(model1)
prediction1<-predict(model1,testing2,type="class")
Matrix1<-confusionMatrix(prediction1,testing2$classe)
performance[2,1]<-Matrix1$overall[1]
Matrix1
# Model 2 Random Forest with package caret 
procesortime<-proc.time()
model2<- train(classe~., method="rf", data=training2, 
                 trControl=trainControl(method='cv',5), ntree=250)
t<-proc.time()-procesortime
performance[1,2]<-t[1]
predict2<-predict(model2, testing2)
Matrix2<-confusionMatrix(predict2,testing2$classe)
performance[2,2]<-Matrix2$overall[1]
 #Model 3 Tree  package rpart
procesortime<-proc.time()
treeView<-rpart(classe~., data=training2 , method="class")
t<-proc.time()-procesortime
performance[1,3]<-t[1]
predict3<-predict(treeView, testing2,type="class")
Matrix3<-confusionMatrix(predict3,testing2$classe)
performance[2,3]<-Matrix3$overall[1]
prp(treeView)
#cf3




