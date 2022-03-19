library(MASS)
library(dplyr)
data("birthwt")
str(birthwt)
#make them factor :
birthwt$race<-as.factor(birthwt$race)
birthwt$smoke<-as.factor(birthwt$smoke)
birthwt$ht<-as.factor(birthwt$ht)
birthwt$ui<-as.factor(birthwt$ui)
birthwt$ptl<-as.factor(birthwt$ptl)

summary(birthwt)
table(birthwt$low)
mean(birthwt$low)


#Using smoke,lwt,race to fit model :
fit=glm(low~smoke+age+race+lwt,data = birthwt,family = "binomial")
summary(fit)
#predict:
birthwt$predicted<-predict(fit,newdata = birthwt,type = "response")
#calculate Odds ratio
exp(coef(fit))
#Calculate conf interval for Odds ratio
exp(confint(fit))

library(caret)
library(InformationValue)
library(pROC)
library(ROCR)
optimal<-optimalCutoff(birthwt$low,birthwt$predicted)
confusionMatrix(birthwt$low,birthwt$predicted)
#sensitivity :  TP/P
sensitivity(actuals = birthwt$low,birthwt$predicted)
#specificity : TN/N
specificity(actuals = birthwt$low,birthwt$predicted)
#+ve predictive value : TP/TP+FP


#calculate total misclassification error rate
misClassError(actuals = birthwt$low,birthwt$predicted, threshold=optimal)

#caret confusion matrix
caret::confusionMatrix(data = as.factor(birthwt$low),as.factor(if_else(birthwt$predicted>optimal,1,0)))

#Area under the curve:
roc(birthwt$low,(if_else(birthwt$predicted>optimal,1,0)))
predicted<-prediction(birthwt$predicted,birthwt$low)
perf<-performance(predicted,"tpr","fpr")
plot(perf)
