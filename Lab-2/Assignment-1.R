library(ggplot2) #Library to visualize data
library(MASS) #Library for lda()
options(scipen=999) #To avoid scientific notations
set.seed(12345)
RNGversion('3.5.1')
#Loading the data
australianCrabs = read.csv("australian-crabs.csv",stringsAsFactors = FALSE)

#Function to calculate misclassification rate
missclass=function(actualData,predictedData){
  n=length(actualData)
  return(1-sum(diag(table(actualData,predictedData)))/n)
}

#1. 
ggplot(australianCrabs,aes(x=CL,y=RW,colour=sex,shape=sex))+geom_point()

#2. Performing LDA
ldaFit = lda(sex~CL+RW,data = australianCrabs[,-c(3)],prior=c(0.5,0.5))
ldaFit
ldaFit.values = predict(ldaFit,type="response")
newData_1 = data.frame(CL=australianCrabs[,6],RW=australianCrabs[,5],PredictedSex=ldaFit.values$class)
ggplot(newData_1)+
  geom_point(aes(CL,RW,colour=PredictedSex),size=2.5)

table("predicted_value"=as.factor(newData_1$PredictedSex),as.factor(australianCrabs$sex))
missclass(as.factor(australianCrabs$sex),as.factor(newData_1$PredictedSex))

#3. Performing LDA with different priors
ldaFit_2 = lda(sex~CL+RW,data = australianCrabs[,-c(3)],prior=c(0.1,0.9))
ldaFit_2
ldaFit_2.values = predict(ldaFit_2,type="response")
newData_2 = data.frame(CL=australianCrabs[,6],RW=australianCrabs[,5],PredictedSex=ldaFit_2.values$class)
ggplot(newData_2)+
  geom_point(aes(CL,RW,colour=PredictedSex),size=2.5)

table("Predicted" = as.factor(newData_2$PredictedSex),"Actual"=australianCrabs$sex)
missclass(australianCrabs$sex,newData_2$PredictedSex)

#4. Logistic Regression
australianCrabs['Sex'] = as.factor(ifelse(australianCrabs$sex=="Male",1,0))
logisticReg = glm(Sex~CL+RW,data = australianCrabs[,-c(3)],family = "binomial")
summary(logisticReg)
logisticReg_Predict = predict(logisticReg,australianCrabs,type="response")
logisticReg_Predict_Final = ifelse(logisticReg_Predict>0.5,"Male","Female")
table("Predicted"=as.factor(logisticReg_Predict_Final),"Actual"=ifelse(australianCrabs$Sex==1,"Male","Female"))
missclass(australianCrabs$Sex,as.factor(logisticReg_Predict_Final))

ggplot(australianCrabs)+
  geom_point(aes(CL,RW,colour=logisticReg_Predict_Final),size=2.5)+
  geom_abline(slope = coef(logisticReg)[2]/(-coef(logisticReg)[3]) ,intercept = coef(logisticReg)[1]/(-coef(logisticReg)[3]),col="red")
