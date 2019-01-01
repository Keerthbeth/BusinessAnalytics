library(ggplot2)
library(forecast)
library(zoo)
library(ggseas)
library(magrittr)
library(dplyr)
library(DescTools)
library(MASS)
data <- read.delim("E:/EBAC/6 Data Analytics/Assignment 3 ARIMA/GRPRating.csv")
str(data)
data$Date <- as.Date(data[,1],"%d-%b-%y")
head(data)
data$GRPRatingsDate <- NULL

#Initial plot
ggplot(data,aes(x=Date,y=GRP))+geom_line()+ylim(0,350)

##Decomposition method--------STL method
#Test and train split
train_data <- data$GRP[1:72]
test_data <- data$GRP[73:92]

data_GRP <- ts(train_data,frequency=26)

#Seasonal plots
seasonplot(data_GRP,col=rainbow(3), year.labels = TRUE, year.labels.left = TRUE)
ggseasonplot(data_GRP,polar=TRUE,year.labels = TRUE, col=rainbow(3))

#Taking log of data to convert from multiplicative to additive
l1 <- log(train_data)
l2 <- ts(l1,frequency=26)

#using stl function to decompose
stl_mul <- stl(l2,"per")
stl_mul%>% forecast(method="naive", h=20)%>%autoplot
fit_1 <- stl_mul%>% forecast(method="naive", h=20)
fit_1= as.data.frame(fit_1)

#plotting the decomposed time series
s2 <- as.matrix(stl_mul)
s2 <- stl_mul$time.series
s3 <- as.data.frame(s2)
decomposed_data <- exp(s3)
par(mfrow=c(3,1))
p1 <- plot(decomposed_data$seasonal,type="l",xlab = "Week",ylab= "seasonal")
p2 <- plot(decomposed_data$trend,type="l",xlab = "week",ylab="trend")
p3 <- plot(decomposed_data$remainder,type="l",xlab="week", ylab="remainder")

#Back transforming log data
fit.stldecomposition <- exp(fit_1$'Point Forecast')

#Accuracy measures
accuracy(fit.stldecomposition,test_data)

#Deriving predicted values from formula
df_3 <- as.data.frame(stl_mul$time.series)
df_3 <- exp(df_3)
df_3$new <- df_3$seasonal*df_3$trend
Predicted <-c((df_3$new),(fit.stldecomposition))
Actual <- data$GRP


#Plotting predicted Vs actual
autoplot.zoo(cbind.zoo(Predicted,Actual),facets = "FALSE")+theme_set(theme_minimal())+theme(legend.position = "bottom") +geom_line(size=1)+
ylim(0,350)+geom_vline(xintercept=73)+xlab("Week Number")+ylab("GRP") +ggtitle("Actual Vs Predicted by STL Decomposition Method")

##-------------------------------------------------------------------------------------------------------------------------##

#Time series regression
#Creating variables for regression

data_1 <- data

data_1$t <- seq(1:92)
data_1$t_sqrt <- (data_1$t)^(1/2)
data_1$t_cube <- (data_1$t)^(1/3)
data_1$t_log <- log(data_1$t)
data_1$logy <- log(data_1$GRP)
data_1$logx <- log(data_1$t)

#Square root model
train_sqrt <- data.frame(data_1$GRP[1:72],data_1$t_sqrt[1:72])
test_sqrt <- data.frame(data_1$GRP[73:92],data_1$t_sqrt[73:92])
total <- data.frame(data_1$GRP,data_1$t_sqrt)
colnames(train_sqrt)= c("GRP", "Input")
colnames(test_sqrt)= c("GRP","Input")
total
colnames(total)= c("GRP","Input")
fit.sqrt<- lm(GRP~ Input,train_sqrt)
summary(fit.sqrt)
pred.sqrt <- predict(fit.sqrt,test_sqrt)
pred.sqrt.total <- predict(fit.sqrt,total)
accuracy(pred.sqrt,data_1$GRP[73:92])

fit.sqrt.rlm<- rlm(GRP~ Input,train_sqrt,psi=psi.bisquare)
summary(fit.sqrt.rlm)
pred.sqrt.rlm <- predict(fit.sqrt.rlm,test_sqrt)
predicted.fit.rlm <- predict(fit.sqrt.rlm,total)
#pred.rlm.total
accuracy(pred.sqrt.rlm,data_1$GRP[73:92])

plot(predicted.fit.rlm,type="l",ylim=c(0,350),col="green",xlab = "Week",
     ylab="GRP", main="Predicted Vs Actual of Time Series Regression", cex.main=0.9)
points(data_1$GRP,type="l",col="red")
abline(v=73)
title("", cex=0.5)

#log model
train_log <- data.frame(data_1$logy[1:72],data_1$t[1:72])
test_log <- data.frame(data_1$logy[73:92],data_1$t[73:92])
colnames(train_log)= c("GRP", "Input")
colnames(test_log)= c("GRP","Input")
fit.log<- lm(GRP~ Input,train_log)
summary(fit.log)
pred <- exp(predict(fit.log,test_log))
accuracy(pred,data_1$GRP[73:92])

fit.log.rlm<- rlm(GRP~ Input,train_log,psi=psi.bisquare)
summary(fit.log.rlm)
pred.log.rlm <- exp(predict(fit.log.rlm,test_log))
accuracy(pred.log.rlm,data_1$GRP[73:92])

#cube root model
train_cuberoot <- data.frame(data_1$GRP[1:72],data_1$t_cube[1:72])
test_cuberoot <- data.frame(data_1$GRP[73:92],data_1$t_cube[73:92])
colnames(train_cuberoot)= c("GRP", "Input")
colnames(test_cuberoot)= c("GRP","Input")
fit.cuberoot<- lm(GRP~ Input,train_cuberoot)
summary(fit.cuberoot)
pred.cuberoot <- predict(fit.cuberoot,test_cuberoot)
accuracy(pred.cuberoot,data_1$GRP[73:92])

fit.cube.rlm<- rlm(GRP~ Input,train_cuberoot,psi=psi.bisquare)
summary(fit.cube.rlm)
pred.cube.rlm <- predict(fit.cube.rlm,test_cuberoot)
accuracy(pred.cube.rlm,data_1$GRP[73:92])

#logxlogy model
train_loglog <- data.frame(data_1$logy[1:72],data_1$logx[1:72])
test_loglog <- data.frame(data_1$logy[73:92],data_1$logx[73:92])
total <- data.frame(data_1$logy,data_1$logx)
colnames(train_loglog)= c("logy", "logx")
colnames(test_loglog)= c("logy","logx")
colnames(total)= c("logy","logx")
fit.loglog<- lm(logy~logx,train_loglog)
summary(fit.loglog)
pred.loglog <- exp(predict(fit.loglog,test_loglog))
pred <- exp(predict(fit.loglog,total))
plot(pred,type="l")

plot(pred, type="l", col="green", ylim=c(0,350))
points(data_1$GRP, type="l", col="red" )
accuracy(pred.loglog,data_1$GRP[73:92])

fit.loglog.rlm<- rlm(logy~logx,train_loglog,psi=psi.bisquare)
summary(fit.loglog.rlm)
pred.loglog.rlm <- exp(predict(fit.loglog.rlm,test_loglog))
accuracy(pred.loglog.rlm,data_1$GRP[73:92])

#Multinomial model
train_lm <- data.frame(data_1$GRP[1:72],data_1$t[1:72])
test_lm <- data.frame(data_1$GRP[73:92],data_1$t[73:92])
total <- data.frame(data_1$GRP,data_1$t)
colnames(train_lm)= c("GRP", "Input")
colnames(test_lm)= c("GRP","Input")
colnames(total)= c("GRP","Input")
lm.fit2=lm(GRP~Input+I(Input^2)+I(Input^3),train_lm)
summary(lm.fit2)
pred.linear <- predict(lm.fit2,test_lm)
accuracy(pred.linear,data_1$GRP[73:92])

fit.rlm.linear<- rlm(GRP~Input+I(Input^2)+I(Input^3),train_lm,psi=psi.bisquare)
summary(fit.rlm.linear)
pred.linear.rlm <- predict(fit.rlm.linear,test_lm)
accuracy(pred.linear.rlm,data_1$GRP[73:92])

#Linear model
lm.fit <- lm(GRP~Input,train_lm)
summary(lm.fit)
pred_linear <- predict(lm.fit,test_lm)
accuracy(pred_linear,data_1$GRP[73:92])

fit.rlm<- rlm(GRP~Input,train_lm,psi=psi.bisquare)
summary(fit.rlm)
pred.rlm <- predict(fit.rlm,test_lm)
pred.total <- predict(fit.rlm,total)
accuracy(pred.rlm,data_1$GRP[73:92])

#Shapiro test on residuals
shapiro.test(fit.sqrt.rlm$residuals)

