library(readxl) #Library to read spreadsheet based files
library(kknn) #Library to implement KNN algorithm
options(scipen=999) #To avoid scientific notations
RNGversion('3.5.1')
#Assignment 1
#Reading from excel file
data = read_excel("spambase.xlsx",sheet = "spambase_data")
#1. Splitting the data
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
#Converting target variable "Spam" to factor
data$Spam = as.factor(data$Spam)
train=data[id,]
test=data[-id,]


#2. Logistic Regression with P(Y|X)>0.5
logisticModel_1 = glm(Spam~.,family = binomial,data = train)
summary(logisticModel_1)

#Function to calculate misclassification rate
missclass=function(actualData,predictedData){
  n=length(actualData)
  return(1-sum(diag(table(actualData,predictedData)))/n)
}

#0.5 classification principle
predicted_training_prob_1 = predict(logisticModel_1,newdata = train,type="response")
predicted_training_1 = ifelse(predicted_training_prob_1>0.5,1,0)
table(as.factor(predicted_training_1),train$Spam)
missclass(train$Spam,as.factor(predicted_training_1))

predicted_test_prob_1 = predict(logisticModel_1,newdata = test,type = "response")
predicted_test_1 = ifelse(predicted_test_prob_1>0.5,1,0)
table(as.factor(predicted_test_1),test$Spam)
missclass(test$Spam,as.factor(predicted_test_1))

#0.8 classification principle
predicted_training_prob_2 = predict(logisticModel_1,newdata = train,type="response")
predicted_training_2 = ifelse(predicted_training_prob_2>0.8,1,0)
table(as.factor(predicted_training_2),train$Spam)
missclass(train$Spam,as.factor(predicted_training_2))

predicted_test_prob_2 = predict(logisticModel_1,newdata = test,type = "response")
predicted_test_2 = ifelse(predicted_test_prob_2>0.8,1,0)
table(as.factor(predicted_test_2),test$Spam)
missclass(test$Spam,as.factor(predicted_test_2))


#3. KNN

knnModel_1 = train.kknn(Spam~.,data = train,kmax = 30)

predicted_3_training_data = predict(knnModel_1,newdata = train)
table(as.factor(predicted_3_training_data),train$Spam)

predicted_3_testing_data = predict(knnModel_1,newdata = test)
table(as.factor(predicted_3_testing_data),test$Spam)

missclass(train$Spam,as.factor(predicted_3_training_data))
missclass(test$Spam,as.factor(predicted_3_testing_data))

knnModel_2 = train.kknn(Spam~.,data=train,kmax = 1)

predicted_4_training_data = predict(knnModel_2,newdata = train)
table(as.factor(predicted_4_training_data),train$Spam)

predicted_4_testing_data = predict(knnModel_2,newdata = test)
table(as.factor(predicted_4_testing_data),test$Spam)

missclass(train$Spam,as.factor(predicted_4_training_data))
missclass(test$Spam,as.factor(predicted_4_testing_data))

knn_1 = kknn::kknn(Spam~.,train=train,test = train,k = 30)
knn_2 = kknn::kknn(Spam~.,train=test,test = test,k = 30)

table(knn_1$fitted.values,train$Spam)
table(knn_2$fitted.values,test$Spam)
missclass(train$Spam,knn_1$fitted.values)
missclass(test$Spam,knn_2$fitted.values)


knn_3 = kknn::kknn(Spam~.,train=train,test = train,k = 1)
knn_4 = kknn::kknn(Spam~.,train=test,test = test,k = 1)
table(knn_3$fitted.values,train$Spam)
table(knn_4$fitted.values,test$Spam)
missclass(train$Spam,knn_3$fitted.values)
missclass(test$Spam,knn_4$fitted.values)



