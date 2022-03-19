library(dplyr)
library(rpart.plot)
set.seed(678)
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read.csv(path)
head(titanic)
str(titanic)
#remove some variables : 
titanic<-titanic%>%dplyr::select(-c(home.dest,cabin,name,ticket))
#convert int to factor :
titanic$pclass<-as.factor(titanic$pclass)
titanic$survived<-as.factor(titanic$survived)
titanic$sex<-as.factor(titanic$sex)
titanic$age<-as.integer(titanic$age)


#remove all rows that have NA :
sapply(titanic, function(x)sum(is.na(x)))
cleaned_titanic<-titanic%>%na.omit()
cleaned_titanic$survived
#divide the dataset to train and test :
train_prop=0.7
train_index<-sample.int(length(cleaned_titanic$survived),round(length(cleaned_titanic$survived)*train_prop))
train<-cleaned_titanic[train_index,]
test<-cleaned_titanic[-train_index,]
#prop:
prop.table(table(train$survived))
prop.table(table(test$survived))

names(train)
str(train)
#model :
library(rpart)
fit<-rpart(survived~pclass+sex+age,data=train,method='class')
rpart.plot(fit)
#predict :
unseen<-predict(fit,test,type = 'class')
table(test$survived,unseen)
caret::confusionMatrix(test$survived,unseen)

#hyper parameter :
control=rpart.control(minsplit= 20,minbucket =4 ,maxdepth = 30)
#fit using hyper parameter :
fit<-rpart(survived~pclass+sex+age,data=train,method='class',control = control)
unseen<-predict(fit,test,type = 'class')
caret::confusionMatrix(test$survived,unseen)

