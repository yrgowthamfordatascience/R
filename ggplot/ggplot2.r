#useful link : https://datascience.csuchico.edu/event/ggplot2_intro1/#one-numeric-and-one-categorical


#Basic data types and suitable plots :

#1) Numeric,Numeric -> Scaterplot
#2)Categorical,Numeric -> box plot , bar plot


#libs:
library(ggplot2)
library(caret)
library(tidyverse)

#Scatter plots : 

#ggplot 2 numeric variables : dot and line with X,Y :
head(BOD)
ggplot(data=BOD,
       mapping =aes(x = Time,
                    y=demand))+geom_point(size=5)+geom_line(colour='red')
#ggplot 2 numeric variables , colored by 3rd variable : scatter plot :
head(CO2)
str(CO2)
ggplot(CO2,aes(conc,uptake,colour=Treatment))+geom_point()
#ggplot2 above with linear line for trend and split by a variable Type(categorical) and heading :
ggplot(CO2,aes(conc,uptake,colour=Treatment))+geom_point(size=3)+
  geom_smooth(method=lm,se=F)+facet_wrap(~Type)+labs(title="Concentration of Co2")+theme_bw()

#ggplot2 with Treatment(categorical) vs uptake(numeric) using boxplot :

ggplot(CO2,aes(Treatment,uptake))+geom_boxplot()+
  geom_point(aes(size=conc,color=Plant))+coord_flip()+
  theme_bw()+facet_wrap(~Type)


#ggplot2  apply filter to data and same as above :
head(mpg)
ggplot(mpg%>%filter(cty<25),aes(x=displ,y=cty))+
  geom_point(aes(size=trans,color=drv),alpha=0.5)+geom_smooth(method=lm)+
  facet_wrap(~year)+labs(x="Engone size",y="MPG in city",title="Fuel efficiency")+theme_bw()
###################################################################################################################################
#Bar plots : 
names(msleep)
head(msleep)  

#Missing values in the dataset :
sapply(msleep,function(x) sum(is.na(x)))

#plot single categorical variable - bar chart x= cat ,y=count of obs , remove na's: 
ggplot(msleep%>%drop_na(vore),aes(x=vore))+geom_bar()+labs(x="Vore",y="Number of obs",title="Number of obs per vore type")

#plot single numeric variable - histogram  x=numeric value , y=count of obs:
ggplot(msleep%>%drop_na(awake),aes(x=awake))+geom_histogram(binwidth = 2)

#plot two categorical variables 1st as category and 2nd as color of bar side-by-side: 
head(starwars)
#method 1 using position='dodge':
ggplot(starwars%>%filter(hair_color %in% c('black','brown'))%>%drop_na(sex),aes(hair_color,fill=sex))+
  geom_bar(position='dodge',alpha=0.5)+theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x="hair color",y="count of obs",title = "Count of hair color by Sex")
#method 2 using facet_wrap:
ggplot(starwars%>%filter(hair_color %in% c('black','brown'))%>%drop_na(sex),aes(sex))+
  geom_bar(aes(fill=sex),alpha=0.5)+facet_wrap(~hair_color)

################################################################################################################################
#one numeric and a categorical : 
head(msleep)

#each vore and sleep they get : 
ggplot(msleep,aes(vore,sleep_total))+geom_boxplot()
ggplot(msleep%>%drop_na(vore),aes(sleep_total))+geom_density(aes(fill=vore),alpha=0.5)+facet_wrap(~vore)




################################################################################################################################
#Tell story with data : 

#single numeric data : histogram , density plot , box plot , violin plot(similar to density plot )
#single categorical variable : bar plot 


#two categorical and one numeric : split by facet wrap (category) and density plot(numeric) with color(category)
head(starwars)
ggplot(starwars%>%filter(hair_color %in% c('black','brown'))%>%drop_na(sex),aes(x=height))+geom_density(aes(fill=sex),alpha=0.5)+
  facet_wrap(~hair_color)

#two numeric and one categorical : scatter plot (2 numeric) and color of dot(categorical) and optional facet the categorical:
head(starwars)
ggplot(starwars%>%filter(hair_color %in% c('black','brown'))%>%drop_na(sex),aes(x=height,y=mass))+geom_point(aes(colour=sex,size=4))

#three numeric one categorical : scatter plot (2 numeric) and size of dot (numeric) and color of dot (categorical)
library(tidyverse)
install.packages("gapminder")
library(gapminder)
head(gapminder)
ggplot(gapminder%>%filter(continent %in% c('Africa','Europe')& gdpPercap<30000),
  aes(gdpPercap,lifeExp))+geom_point(aes(size=pop,colour=year),alpha=0.5)+facet_wrap(~continent)

###################################################################################################################################################

