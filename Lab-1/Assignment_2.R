#Assignment - 2
library(readxl) #Library to read spreadsheet based files
options(scipen=999) #To avoid scientific notations
set.seed(12345)
RNGversion('3.5.1')
#1. Reading the data
machineData = read_excel("machines.xlsx",sheet = "machines")

#Distribution
hist(machineData$Length,probability = TRUE,xlab = "Length", main = "Length Vs Density")
lines(density(machineData$Length),col="blue", lwd=2)

#Log-likelihood function
theta = seq(from=0,to=3,by=0.01)
logLikelihood = function(theta,dataVector)
{
  return(length(dataVector)*log(theta)-theta*sum(dataVector))
}

maximumLikelihood = function(theta,dataVector)
{
  return(length(dataVector)/sum(dataVector))
}

plot(theta, logLikelihood(theta,machineData$Length), ylim = c(-200,0), col = 'red', xlab="thetas", ylab = "",
     main="Log-likelihood, six machines vs all machines")
par(new = TRUE)
plot(theta, logLikelihood(theta,machineData[1:6,]), ylim = c(-200,0), col='blue', xlab="thetas", ylab = "",
     main="Log-likelihood, six machines vs all machines")

maximumLikelihood(theta,machineData$Length)
maximumLikelihood(theta,machineData$Length[1:6])

#4.
bayesian <- function(data, theta,lambda) {
  return( logLikelihood(theta, data) + log(lambda*exp(-lambda*theta)))
}

plot(theta,bayesian(machineData$Length,theta,10),col="red",xlab="theta",
     ylab = "Bayesian-Likelihood")
par(new=TRUE)
points(theta,bayesian(machineData$Length,theta,10),col="blue")
theta[which.max(bayesian(machineData$Length,theta,10))]
#5.
theta[which.max(logLikelihood(theta,machineData$Length))]
new_observation_exp = (rexp(50, theta[which.max(logLikelihood(theta,machineData$Length))]))
hist(new_observation_exp,main = "Histogram of New Data",xlab="New Data")
hist(machineData$Length, main = "Histogram of Actual Data",xlab = "Actual Data")
par(new = TRUE)
