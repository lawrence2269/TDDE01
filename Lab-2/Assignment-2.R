library(ggplot2) #Library to visualize data
options(scipen=999) #To avoid scientific notations
#set.seed(12345)
RNGversion('3.5.1')
library(readxl)
library(e1071)
library(tree)
library(MASS)

#Loading Data
creditScoring = read_excel("creditscoring.xls")

#Function to calculate misclassification rate
missclass=function(actualData,predictedData){
  n=length(actualData)
  return(1-sum(diag(table(actualData,predictedData)))/n)
}

creditScoring$good_bad = as.factor(creditScoring$good_bad)
n = dim(creditScoring)[1]
set.seed(12345)
id = sample(1:n,floor(n*0.5))
train = creditScoring[id,]

id1 = setdiff(1:n,id)
set.seed(12345)
id2 = sample(id1,floor(n*0.25))
valid = creditScoring[id2,]

id3 = setdiff(id1,id2)
test = creditScoring[id3,]

#2. Decision tree - measure of impurity
  #Deviance
deviance_Tree = tree(good_bad~.,data = train,split = c("deviance"))

deviance_Predicted_Test = predict(deviance_Tree,newdata=test,type="class")
missclass(as.factor(test$good_bad),deviance_Predicted_Test)

deviance_Predicted_Train = predict(deviance_Tree,newdata = train,type="class")
missclass(as.factor(train$good_bad),deviance_Predicted_Train)

  #gini
gini_Tree = tree(good_bad~.,data = train,split = c("gini"))

gini_Predicted_Test = predict(gini_Tree,newdata = test,type="class")
table(gini_Predicted_Test,test$good_bad)
missclass(test$good_bad,gini_Predicted_Test)

gini_Predicted_Train = predict(gini_Tree,newdata=train,type="class")
missclass(train$good_bad,gini_Predicted_Train)

summary(deviance_Tree)
summary(gini_Tree)

#3 Choose optimal tree depth
tree_1 = tree(good_bad~.,data = train,split = c("deviance"))
summary(tree_1)
terminalNodes = 15
trainScore = rep(0,terminalNodes)
validationScore = rep(0,terminalNodes)
missclassification_Rate = c()
for(i in 2:terminalNodes){
  tree_1_Prune = prune.tree(tree_1,best=i)
  print(i)
  pred = predict(tree_1_Prune,newdata = valid,type="tree")
  trainScore[i] = deviance(tree_1_Prune)
  validationScore[i] = deviance(pred)
}

plot(2:terminalNodes,trainScore[2:terminalNodes],type="l",ylim=c(270,600),col="red",pch = "o",ylab = "Train/Test Score",xlab="No. of Leaves")
points(2:terminalNodes,trainScore[2:terminalNodes],col="red")
par(new = TRUE)
plot(2:terminalNodes,validationScore[2:terminalNodes],type="l",ylim=c(270,600),col="blue",pch = "o",ylab = "Train/Test Score",yaxt="n",xlab="No. of Leaves")
points(2:terminalNodes,validationScore[2:terminalNodes],col="blue")
legend(x="top",legend = c("Test","Train"),lwd=1,col=c("Blue","Red"),lty=1)

final_Pruned_Tree = prune.tree(tree_1,best = 4)
summary(final_Pruned_Tree)
plot(final_Pruned_Tree)
text(final_Pruned_Tree,pretty = 0)

pred_train_using_pruned_tree = stats::predict(final_Pruned_Tree,train,type="class")
table(pred_train_using_pruned_tree,train$good_bad)
missclass(train$good_bad,pred_train_using_pruned_tree)

pred_test_using_pruned_tree = stats::predict(final_Pruned_Tree,test,type="class")
table(pred_test_using_pruned_tree,test$good_bad)
missclass(test$good_bad,pred_test_using_pruned_tree)

#4. Naive Bayes
naive_Bayes_Fit = naiveBayes(good_bad~.,data = train)
naive_Bayes_Fit

y_train_pred_naive_bayes = stats::predict(naive_Bayes_Fit,train)
table(y_train_pred_naive_bayes,train$good_bad)
missclass(train$good_bad,y_train_pred_naive_bayes)

y_pred_naive_bayes = stats::predict(naive_Bayes_Fit,test)
table(y_pred_naive_bayes,test$good_bad)
missclass(test$good_bad,y_pred_naive_bayes)

#5.
library(caret)
pi = seq(from=0.05,to=0.95,by=0.05)
#1. optimal tree
optimal_tree_vector = stats::predict(final_Pruned_Tree,test,type="vector")
naive_Bayes_raw = stats::predict(naive_Bayes_Fit,test,type="raw")
tree_tpr = c()
tree_fpr = c()
bayes_tpr = c()
bayes_fpr = c()
for(i in 1:length(pi))
{
  temp1 = confusionMatrix(as.factor(ifelse(optimal_tree_vector[,2]>pi[i],"good","bad")),test$good_bad)
  tree_tpr[i] = temp1$table[4]/(temp1$table[4]+temp1$table[3])
  tree_fpr[i] = temp1$table[2]/(temp1$table[2]+temp1$table[1])
  temp2 = confusionMatrix(as.factor(ifelse(naive_Bayes_raw[,2]>pi[i],"good","bad")),test$good_bad)
  bayes_tpr[i] = temp2$table[4]/(temp2$table[4]+temp2$table[3])
  bayes_fpr[i] = temp2$table[2]/(temp2$table[2]+temp2$table[1])
}

roc_Df = data.frame(tree_tpr = tree_tpr,tree_fpr = tree_fpr,bayes_tpr=bayes_tpr,bayes_fpr = bayes_fpr)

ggplot(roc_Df) + geom_line(aes(x=bayes_fpr, y = bayes_tpr, color = 'Bayesian')) + 
  geom_line(aes(x=tree_fpr, y = tree_tpr, color = 'Tree')) +
  geom_abline(slope=1,intercept = 0)+
  ggtitle("ROC Curves")+xlab("FPR")+ylab("TPR")


#6.
prob_Naive_Bayes_test = predict(naive_Bayes_Fit,newdata = test,type = c("raw"))
weighted_Naive_Bayes_Pred_Test_Data = replace(prob_Naive_Bayes_test,prob_Naive_Bayes_test[,1]*10>prob_Naive_Bayes_test[,2],"bad")
weighted_Naive_Bayes_Pred_Test_Data = replace(weighted_Naive_Bayes_Pred_Test_Data,prob_Naive_Bayes_test[,1]*10<=prob_Naive_Bayes_test[,2],"good")
table("Predicted"=as.factor(weighted_Naive_Bayes_Pred_Test_Data[,1]),"Actual"=test$good_bad)
missclass(test$good_bad,as.factor(weighted_Naive_Bayes_Pred_Test_Data[,1]))

prob_Naive_Bayes_train = predict(naive_Bayes_Fit,newdata = train,type = c("raw"))
weighted_Naive_Bayes_Pred_Train_Data = replace(prob_Naive_Bayes_train,prob_Naive_Bayes_train[,1]*10>prob_Naive_Bayes_train[,2],"bad")
weighted_Naive_Bayes_Pred_Train_Data = replace(weighted_Naive_Bayes_Pred_Train_Data,prob_Naive_Bayes_train[,1]*10<=prob_Naive_Bayes_train[,2],"good")
table("Predicted"=as.factor(weighted_Naive_Bayes_Pred_Train_Data[,1]),"Actual"=train$good_bad)
missclass(train$good_bad,as.factor(weighted_Naive_Bayes_Pred_Train_Data[,1]))


