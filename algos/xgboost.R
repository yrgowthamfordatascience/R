library(xgboost)
library(pROC)
library(ROCR)
columna<-names(train)[c(1,3,4,5,6,7,8)]
train$survived<-as.numeric(train$survived)-1
test$survived<-as.numeric(test$survived)-1

dtrain<-xgb.DMatrix(data.matrix(train%>%dplyr::select(columna)),label=train$survived)
dtest<-xgb.DMatrix(data.matrix(test%>%dplyr::select(columna)),label=test$survived)
xgb_fit<-xgboost(data = dtrain,nrounds = 100,objective='binary:logistic',nthread=2,eta=0.01,eval_metric='auc',gamma=1)
result<-predict(xgb_fit,newdata=dtest,"probs")
roc_auc<-performance(prediction(result,test$survived),"auc")
auc<-roc_auc@y.values[1]
xgb.importance(xgb_fit$feature_names,xgb_fit)
#plot
plot(roc_auc<-performance(prediction(result,test$survived),"tpr","fpr"))
optimalCutoff(actuals = test$survived,predictedScores = result)

#confusion matrix:
caret::confusionMatrix(as.factor(if_else(result>0.4840617,1,0)),as.factor(test$survived))

z<-3*1:floor(20/3)
y<-5*1:floor(20/5)
d<-c(y,z)[!duplicated(c(y,z))]
sum(d)
