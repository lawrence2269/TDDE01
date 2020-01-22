options(scipen=999) #To avoid scientific notations
RNGversion('3.5.1')
library(neuralnet)
library(ggplot2)

set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
ggplot(trva,aes(x=Var,y=Sin))+geom_line()
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31,-1,1)
#Since we are dealing with regression, best way to choose the model is to use MSE
mse_test = rep(0,10)
mse_train = rep(0,10)

for(i in 1:10)
{
  nn = neuralnet(Sin~Var,data=tr,linear.output = TRUE,threshold = i/1000,hidden = 10,startweights = winit)
  pred = predict(nn,va)
  mse_test[i] = sum((va$Sin-pred)^2)/nrow(va)
  mse_train[i] = sum((tr$Sin-pred)^2)/nrow(tr)
}

best_Threshold = which(mse_test == min(mse_test))
plot(1:10,mse_test,xlab="Threshold",ylab = "MSE",main="Selecting Threshold")
#1st model is having lowest test error when compared with other models.
nn = neuralnet(Sin~Var,data=tr,linear.output = TRUE,threshold = best_Threshold/1000,hidden = 10,startweights = winit)

plot(prediction(nn)$rep1,col="black",xlab = "",ylab = "")
points(tr,col="red")
legend(x="bottomleft",legend = c("NN Predictions","Train Data"),col=c("Black","Red"),pch = c(1,1))

plot(va$Var,predict(nn,va)[,1],col="black",xlab = "",ylab = "",main="Comparison of Validation data with predictions")
points(va,col="red")
legend(x="bottomleft",legend = c("NN Predictions","Validation Data"),col=c("Black","Red"),pch = c(1,1))

plot(nn,rep="best")




