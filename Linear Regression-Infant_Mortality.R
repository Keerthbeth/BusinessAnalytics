library(pacman)
data<-read.csv("D:/Linear Regression/Infant Mortality in North India.csv")
p_load(magrittr,tidyverse,dplyr)
data<-data%>%
  select(-one_of("State_District_Name"))

#Shape/Distribution of Y(% of Infant Mortality)
data%>%
  ggplot(aes(x=Infant_Mortality))+geom_histogram(binwidth = 7,colour = "white",fill="black")+xlab("% of Infant Mortality")
shapiro.test(data$Infant_Mortality)
#Y is normally distributed in population from which the sample is drawn.

#imapct of State levels on Infant Mortality
data%>%
  ggplot(aes(x=State_Name,y=Infant_Mortality)) + geom_boxplot() + xlab("State") + ylab("% Infant Mortality") + theme(axis.text.x = element_text(angle=90)) + labs(title="Infant Mortality prevalence in various states")
summary(aov(Infant_Mortality~State_Name,data))
#State levels are significant for Infant Mortality % rate

#Correlation Study
p_load(corrplot)
plot(select_if(data,is.numeric))
par(mfrow=c(1,1))
corrplot(cor(select_if(data,is.numeric)),type = "upper",method = "number")

#Test-Train Split 60-40 split
p_load(caTools)
set.seed(888)
split<-sample.split(data$Infant_Mortality,SplitRatio = 0.6)
train<-subset(data,split==TRUE)
test<-subset(data,split==FALSE)
rm(split,data)

p_load(car)

#1. Removing Antenatal and FA for Delivery as per correlation plot
lin_model<-lm(Infant_Mortality~.-Antenatal-FA_for_delivery,train)
summary(lin_model)

#2.
lin_model<-update(lin_model,.~.-Marriage_lessthan18yrs,train)
summary(lin_model)

#3.
lin_model<-update(lin_model,.~.-Awareness.on.HIV,train)
summary(lin_model)

#4.
lin_model<-update(lin_model,.~.-US_Taken,train)
summary(lin_model)

#5
lin_model<-update(lin_model,.~.-Delivery_At_Home,train)
summary(lin_model)
#Since excluding UltraSound Taken? decreases the proportion of variance explained by model
#we retain it

#5.
lin_model<-update(lin_model,.~.+Delivery_At_Home,train)
summary(lin_model)

#VIF
vif(lin_model)[,3]^2

lin_model<-update(lin_model,.~.-BP_Taken,train)
summary(lin_model)

vif(lin_model)[,3]^2

#Check Autocorrelation (shouldn't be significant due to non-timeseries data)
durbinWatsonTest(lin_model)
#No Autocorrelation found H0 holds

#Checking Residuals for normality
#1. Moments
p_load(moments)
mean(residuals(lin_model))
skewness(residuals(lin_model))
#both are Very close to zero which implies symmetry around 0
kurtosis(residuals(lin_model))
#Almost 3 which implies normal looking curve

#2.(a) QQ plot
par(mfrow=c(2,2))
plot(lin_model)
#Residuals vs Fitted shows no pattern with a near straight line and Normal QQ plot closely follows a straight line - 
#implies normality of residuals with homoskedasticity
#  (b) Breusch-Pagan test for heteroskedasticity
p_load(lmtest)
bptest(lin_model)
#H0:Homoskedasticity holds > 8%

#3.Shapiro-Wilk Normality test
shapiro.test(residuals(lin_model))
#Implies H0 holds i.e. residuals are normally distributed

#4. Histogram
df<-data.frame(x=residuals(lin_model))
df%>%
  ggplot(aes(x=x)) + geom_histogram(colour = 'White',binwidth = 7) + xlab("Residual Value") + ylab("Frequency") + labs(title="Histogram for Residuals of fit on train")

#Measurements using model on test dataset

#Fitted vs Actual
par(mfrow=c(1,1))
actual<-test$Infant_Mortality
test<-test%>%
  select(-one_of("Infant_Mortality"))
predictions=predict(lin_model,test)
rm(vif_model)
plot(predictions~actual,xlab="Actual",ylab="Fitted",main="Fitted vs Actual for test dataset")
abline(0,1)

#Correlation test
cor.test(actual,predictions)

#Confidence Interval
conf_interval<-predict(lin_model,newdata = test,interval = "confidence",level = 0.95)
yes=0
no=0
i=0
for (i in 1:length(actual)) {
  if(actual[i] >= conf_interval[i,2] & actual[i] <= conf_interval[i,3]){
    yes = yes + 1
  } else {
    no = no + 1
  }
}

df<-data.frame(outcome = c("yes","no"),values=c(yes,no))
rm(yes,no,i,conf_interval)
ggplot(df,aes(x=outcome,y=values)) + geom_bar(stat = "identity") + geom_text(aes(label=values),vjust=1.8,colour="white") + xlab("Actual Values within predicted 95% confidence intervals?") + ylab("Number of Values")

p_load(caret)
#R^2, MAE and RMSE measures on train and test datasets
actual_train<-train$Infant_Mortality
train<-train%>%
  select(-one_of("Infant_Mortality"))
predictions_train<-predict(lin_model,train)
#measures on train dataset
postResample(pred=predictions_train,obs=actual_train)
#measures on test dataset
postResample(pred=predictions,obs=actual)

#Out-of-sample Residual Standard Error
degrees_of_freedom=nrow(test) - 12 #no of variables including dummy variables
sqrt(sum((actual-predictions)^2)/degrees_of_freedom)
#Which is very near to 8.488 or the earlier residual standard error on train dataset. Thus we can say our model behaves consistently
#across seen and unseen data and we have not overfitted the model

#Coefficient of Variation
sd(predictions)/mean(predictions)

