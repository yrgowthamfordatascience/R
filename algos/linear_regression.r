library(caret)
#dataset
mouse.data=data.frame(
  weight=c(0.9,1.8,2.4,3.5,3.9,4.4,5.1,5.6,6.3),
  size=c(1.4,2.6,1.0,3.7,5.5,3.2,3.0,4.9,6.3))
#head
head(mouse.data)
plot(mouse.data)
#lienar regression : 
mouse.regression<-lm(size~weight,data=mouse.data)
summary(mouse.regression)
abline(mouse.regression,col='blue')
#predictions : 
predicted_value<-predict(object = mouse.regression,df = mouse.data)
mouse.data$predicted_size=predicted_value
summary(abs(mouse.data$predicted_size-mouse.data$size))


#complex example multi linear regression : 
library(readr)
Automobile_data <- read.csv("D:/R algos/Automobile_data.csv",header=TRUE,na.strings='?')
View(Automobile_data)

#datatype of each variable : 
str(Automobile_data)
#convert all character to factor
Automobile_data<-Automobile_data%>%mutate_if(is.character,as.factor)
summary(Automobile_data)
Automobile_data$normalized.losses
#impute missing values using mean : numeric : 
Automobile_data[is.na(Automobile_data$normalized.losses),]$normalized.losses<-mean(Automobile_data$normalized.losses,na.rm = TRUE)
Automobile_data[is.na(Automobile_data$city.mpg),]$city.mpg<-mean(Automobile_data$city.mpg,na.rm = TRUE)
Automobile_data[is.na(Automobile_data$horsepower),]$horsepower<-mean(Automobile_data$horsepower,na.rm = TRUE)
Automobile_data[is.na(Automobile_data$peak.rpm),]$peak.rpm<-mean(Automobile_data$peak.rpm,na.rm = TRUE)
Automobile_data[is.na(Automobile_data$price),]$price<-mean(Automobile_data$price,na.rm = TRUE)

selected<-subset(Automobile_data,select = c(horsepower,city.mpg,peak.rpm,curb.weight,num.of.doors,price))

#cor matrix : 
cor(selected[,c("horsepower","city.mpg","peak.rpm","curb.weight","price")])

#split data in to train and validation : 
set.seed(2021)
train.size=0.7
train.index=sample.int(length(selected$price),round(length(selected$price)*train.size))
train=selected[train.index,]
test=selected[-train.index,]             

fit=lm(price~horsepower+curb.weight,data = train)
summary(fit) #Adj.R2 0.68
plot(fit)

#add predictions : 
test$predicted_price=predict(object = fit,test%>%select(horsepower,city.mpg,curb.weight))

#check how good model is on validation dataset :
cor(test$predicted_price,test$price)
#RMSE:
sqrt(mean((test$predicted_price-test$price)^2))
#MAE:
abs(mean(test$predicted_price-test$price))

