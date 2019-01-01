#Reading the dataset
pacman::p_load(tidyverse,reshape2,readxl,jsonlite,corrplot,XLConnect,magrittr)
data<-as.data.frame(fromJSON(file.path('E:/NUS Stuff/C2-AA/FBA - Day 7/FBA - Day 7/data/titanic','titanic3.json')))
View(data)
#--------------------------------------------------------------------------------------------------
#Task 1 - Examine impact of:
#--------------------------------------------------------------------------------------------------

#Gender of a passenger on survival
data%>%
  group_by(survived,sex)%>%
  summarise(count_level = n(),percentage = n()/nrow(data))%>%
  ggplot(aes(x=as.factor(sex),y=count_level,fill=as.factor(survived))) + geom_bar(stat = "identity",position = position_dodge()) + geom_text(aes(label=round(percentage,2))) + labs(x="Sex",y="Number of passengers", title="Survival Rate", subtitle="by sex") + scale_fill_discrete("Survival",labels = c("Casualty","Survived"))
#There are a higher number of female survivors(26%) as compared to male survivors(12%)
#Also more male passengers died(52%) as compared to female passengers(10%)(percentage on absolute)

#Embarkation point on survival
data%>%
  group_by(embarked,survived)%>%
  summarise(number = n())%>%
  ggplot(aes(x=as.factor(embarked),y=number)) + geom_bar(stat = "identity") + geom_text(aes(label=number)) + facet_grid(.~survived,labeller = labeller(survived=c('0' = 'Casualty','1' = 'Survived'))) + labs(x="Embarkation points")
#There are a higher proportion of survivors in embarkation point C

#Cabin on survival
data%>%
  group_by(cabin)%>%
  summarise(number = n())%>%
  ggplot(aes(x=as.factor(cabin),y=number)) 
