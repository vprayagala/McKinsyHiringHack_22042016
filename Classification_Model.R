#Analytics Vidya - mcKinsy Exercise
rm(list=ls())
getwd()
setwd("/Users/Saatwik/Documents/avidya/McKinsy")

#Load the Required packages
require(DMwR)
require(C50)
require(vegan)
require(randomForest)
require(infotheo)
require(adabag)
#Read the training data and pre-process the data
train<-read.csv("Train_psolI3n.csv",header=T,sep=",")
dim(train)
str(train)
sum(is.na(train))
summary(train)

#Function to pre-process the data, so that we can apply the same to train/test
data_process<-function(data){
  #convert variables to factors
  data<-centralImputation(data)
  #data.num<-subset(data,select=c(Subject_Hotness_Score,
  #                               Total_Past_Communications,
  #                               Word_Count,
  #                               Total_Links,
  #                               Total_Images))
  #data.num<-discretize(data.num,disc="equalwidth",nbins=12)
  #data.num<-decostand(data.num,method="range")
  #data.num$Subject_Hotness_Score<-as.factor(data.num$Subject_Hotness_Score)
  #data.num$Total_Past_Communications<-as.factor(data.num$Total_Past_Communications)
  #data.num$Word_Count<-as.factor(data.num$Word_Count)
  #data.num$Total_Links<-as.factor(data.num$Total_Links)
  #data.num$Total_Images<-as.factor(data.num$Total_Images)
  
  #data.cat<-subset(data,select=-c(Subject_Hotness_Score,
  #                               Total_Past_Communications,
  #                               Word_Count,
  #                               Total_Links,
  #                               Total_Images))
  
  #data<-data.frame(data.cat,data.num)
  data$Email_Type<-as.factor(data$Email_Type)
  data$Email_Source_Type<-as.factor(data$Email_Source_Type)
  data$Email_Campaign_Type<-as.factor(data$Email_Campaign_Type)
  data$Time_Email_sent_Category<-as.factor(data$Time_Email_sent_Category)
  
  data$Customer_Location<-as.character(data$Customer_Location)
  data$Customer_Location[data$Customer_Location == ""]<-"NA"
  data$Customer_Location<-as.factor(data$Customer_Location)
  
  data<-subset(data,select=-c(1))
  return (data)
}

train<-data_process(train)
str(train)
train$Email_Status<-as.factor(train$Email_Status)
#Now Build the model using C50.. Column  is target variable
Model_C50 <-C5.0(train[,-11],train[,11],trials=100)
Model_C50
summary(Model_C50)

#Predicting on Train
P1_train=predict(Model_C50,train);
P1_train
train.conf<-table(train[,11],Predicted=P1_train)
train.accuracy<-sum(diag(train.conf))/sum(train.conf)
train.accuracy
#Build using rpart
#Model_rpart<-rpart(Email_Status~.,data=train,method="class")
#Predicting on Test
test<-read.csv("Test_09JmpYa.csv",header=T,sep=",")
test.EmailID<-as.character(test$Email_ID)
test<-data_process(test)
test$Customer_Location<-as.character(test$Customer_Location)
test$Customer_Location[test$Customer_Location == "H"]<-"NA"
test$Customer_Location<-as.factor(test$Customer_Location)

P1_test = predict(Model_C50,test);
P1_test
res<-data.frame(Email_ID=test.EmailID,Email_Status=P1_test)
write.csv(res,file="result.csv")
table(P1_test)

#Build using random forest
Model_rf<-randomForest(Email_Status~.,data=train,importance=T)
Model_rf
summary(Model_rf)

pred_rf<-predict(Model_rf,newdata=test,type="response",predict.all=T)
summary(pred_rf$aggregate)
res_rf<-data.frame(Email_ID=test.EmailID,Email_Status=pred_rf$aggregate)
write.csv(res_rf,file="result_rf.csv")

Model_rf$importance
#Apply boosting
Model_boost<-boosting(Email_Status~.,data=train)


