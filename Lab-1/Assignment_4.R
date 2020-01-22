#Assignment - 3
library(readxl) #Library to read spreadsheet based files
library(ggplot2) #For visualisation
options(scipen=999) #To avoid scientific notations
set.seed(12345)
#Loading data
tecatorData = read_excel("tecator.xlsx",sheet = "data")

#1. plotting Moisture VS Protein
plot(tecatorData$Protein, tecatorData$Moisture, main="Moisture vs. Protein",xlab = "Protein",ylab = "Moisture",col="blue")
abline(lm(formula = Moisture ~ Protein, data=tecatorData))

#Subsetting data
requiredData = tecatorData[,103:104]
id = sample(1:nrow(requiredData),floor(nrow(requiredData)*0.5))
train = requiredData[id,]
test = requiredData[-id,]

#Linear Regression
  #M1
tecatorModel_1 = lm(formula = Moisture ~ Protein, data=train)
sm1 = summary(tecatorModel_1)
sm1

  #M2
tecatorModel_2 = lm(formula = Moisture ~ Protein+I(Protein^2), data=train)
sm2 = summary(tecatorModel_2)
sm2

  #M3
tecatorModel_3 = lm(formula = Moisture ~ Protein+I(Protein^2)+I(Protein^3), data=train)
sm3 = summary(tecatorModel_3)
sm3

  #M4
tecatorModel_4 = lm(formula = Moisture ~ Protein+I(Protein^2)+I(Protein^3)+I(Protein^4), data=train)
sm4 = summary(tecatorModel_4)
sm4

  #M5
tecatorModel_5 = lm(formula = Moisture ~ Protein+I(Protein^2)+I(Protein^3)+I(Protein^4)+I(Protein^5), data=train)
sm5 = summary(tecatorModel_5)
sm5

  #M6
tecatorModel_6 = lm(formula = Moisture ~ Protein+I(Protein^2)+I(Protein^3)+I(Protein^4)+I(Protein^5)+I(Protein^6), data=train)
sm6 = summary(tecatorModel_6)
sm6

MSE_train = numeric(6)
MSE_train[1] = mean((train$Moisture-predict.lm(tecatorModel_1,train[,1]))^2)
MSE_train[2] = mean((train$Moisture-predict.lm(tecatorModel_2,train[,1]))^2)
MSE_train[3] = mean((train$Moisture-predict.lm(tecatorModel_3,train[,1]))^2)
MSE_train[4] = mean((train$Moisture-predict.lm(tecatorModel_4,train[,1]))^2)
MSE_train[5] = mean((train$Moisture-predict.lm(tecatorModel_5,train[,1]))^2)
MSE_train[6] = mean((train$Moisture-predict.lm(tecatorModel_6,train[,1]))^2)

MSE_test = numeric(6)
MSE_test[1] = mean((test$Moisture-predict.lm(tecatorModel_1,test[,1]))^2)
MSE_test[2] = mean((test$Moisture-predict.lm(tecatorModel_2,test[,1]))^2)
MSE_test[3] = mean((test$Moisture-predict.lm(tecatorModel_3,test[,1]))^2)
MSE_test[4] = mean((test$Moisture-predict.lm(tecatorModel_4,test[,1]))^2)
MSE_test[5] = mean((test$Moisture-predict.lm(tecatorModel_5,test[,1]))^2)
MSE_test[6] = mean((test$Moisture-predict.lm(tecatorModel_6,test[,1]))^2)

plot(1:6,MSE_train,type="l",ylim = c(20,45),col="red",lwd=2.5,xlab = "Index",ylab = "MSE")
lines(1:6,MSE_test,col="blue",lwd=2.5)
legend(x="center",legend = c("MSE_Test","MSE_Train"),lwd=1,col=c("Blue","Red"),lty=1)


print(anova(tecatorModel_1,tecatorModel_2,tecatorModel_3,tecatorModel_4,tecatorModel_5,tecatorModel_6))

#4. Variable Selection
library(MASS)
aicData = tecatorData
aicData = aicData[,-c(103:104)]
aicData = aicData[,-c(1)]
stepaic = stepAIC(lm(Fat~.,data = aicData))
stepaic$anova

#5. Ridge Regression
library(glmnet)
covariates = scale(tecatorData[, 2:101])
response = scale(tecatorData$Fat)
ridge_model = glmnet(as.matrix(covariates),
                     response, alpha = 0, family="gaussian")
plot(ridge_model, xvar="lambda", label=TRUE, main="Ridge regression \n\n")

#6. Lasso Regression
lasso_model = glmnet(as.matrix(covariates),
                     response, alpha = 1, family="gaussian")
plot(lasso_model, xvar="lambda", label=TRUE, main="LASSO regression\n\n")

#7. Cross-Validation
lasso_model_cv = cv.glmnet(as.matrix(covariates), response, alpha=1, family="gaussian", lambda=seq(0,1,0.001))
plot(lasso_model_cv, xvar="lamdba", label=TRUE, main="LASSO Cross-validation\n\n")
coef(lasso_model_cv, s="lambda.min")
print(lasso_model_cv$lambda.min)



