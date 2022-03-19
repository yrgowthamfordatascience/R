library(randomForest)
library(caret)

#model:
?randomForest
fit_rf<-randomForest(train$survived~train$pclass+train$sex+train$sex,mtry=2)
importance(fit_rf)
varImpPlot(fit_rf)

#hyper parameter tune ntree and mtry:
#ntree : 
plot(fit_rf$err.rate[,1],type="b",main="OBB")
plot(fit_rf$err.rate[,2],type="b",main="0")
plot(fit_rf$err.rate[,3],type="b",main="1")
fit_rf$err.rate[500]
#mtry :
obb_error<-c()
for(i in 1:3)
{
  print(i)
 fit_mtry=randomForest(train$survived~train$pclass+train$sex+train$pclass+train$sibsp,mtry=i)
 obb_error[i]=fit_mtry$err.rate[500]
}

print(obb_error)

#build final model with mtry=2, and ntree=500 :
           