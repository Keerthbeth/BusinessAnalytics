library(ggplot2)
library(forecast)
library(dplyr)
library(colortools)
library(gridExtra)
library(RColorBrewer)
library(pacman)
library(zoo)
library(dplyr)
library(magrittr)
library(tidyverse)
library(tseries)
library(stringr)
library(dygraphs)
library(MASS)
library(ggplot2)
library(xgboost)
library(plyr) 
library(rattle)
library(caret)
library(neuralnet)
library(xlsx)

#####################Reading the data####################################
data <- read.csv("E:/EBAC/6 Data Analytics/Day 4/Folder_SLFiles(22)/day.csv")
data$dteday <- as.Date(data$dteday,format="%Y-%m-%d")
summary(data)
day=data

#Pre-processing data
data$instant<-NULL

data$weathersit<-as.factor(data$weathersit)
data$holiday<-as.factor(data$holiday)
data$season<-as.factor(data$season)
data$workingday<-as.factor(data$workingday)
data$weekday<-as.factor(data$weekday)
data$dteday<-as.Date(data$dteday,format="%Y-%m-%d")


#Exploratory data Analysis ---- Bivariate Analysis

#Temp Vs Count

ggplot(data,aes(temp,cnt))+geom_point(aes(color=temp),alpha=0.5)+scale_color_gradientn(colors =c('dark blue','blue','light blue','light green','yellow','orange','red'))+theme_classic()
t1 <- ggplot(data,aes(temp,registered))+
  geom_jitter(aes(color=temp),alpha=1.0)+
  theme_bw()+
  scale_color_gradientn(colors =c('dark blue','blue','light blue','light green','yellow','orange','red'))+
  ggtitle("Scatter plot of Registered Vs Temperature ")+
  labs(x="Temperature",y="Registered")+
  theme(plot.title = element_text(face="bold",size=10))+
  theme(axis.title = element_text(size=8))
print(t1) 

t2 <- ggplot(data,aes(temp,casual))+
  geom_jitter(aes(color=temp),alpha=1.0)+
  theme_bw()+scale_color_gradientn(colors =c('dark blue','blue','light blue','light green','yellow','orange','red'))+
  ggtitle("Scatter plot of Casual Vs Temperature ")+
  labs(x="Temperature",y="Casual")+
  theme(plot.title = element_text(face="bold",size=10))+
  theme(axis.title = element_text(size=8))
print(t2)

grid.arrange(t1,t2,nrow=1)

#Season Vs count


s1 <- ggplot(data,aes(season,registered))+
  geom_boxplot(aes(color=season),fill="grey",alpha=1.0)+
  theme_bw()+ggtitle("Boxplot of Season and Registered")+
  labs(x="Season",y="Registered")+
  theme(plot.title = element_text(face="bold",size=10))+
  theme(axis.title = element_text(size=8))
print(s1)

s2 <- ggplot(data,aes(season,casual))+
  geom_boxplot(aes(color=season),fill="grey",alpha=1.0)+
  theme_bw()+ggtitle("Boxplot of Season and Casual")+
  labs(x="Season",y="Casual")+
  theme(plot.title = element_text(face="bold",size=10))+
  theme(axis.title = element_text(size=8))
print(s2)
grid.arrange(s1,s2,nrow=1)

#Weekday Vs Count

w1 <- ggplot(data,aes(weekday,registered))+
  geom_boxplot(aes(color=weekday),alpha=0.7,fill="grey")+theme_bw()+
  ggtitle("Boxplot of Weekday and Registered")+
  labs(x="Weekday",y="Registered")+
  theme(plot.title = element_text(face="bold",size=10))+
  theme(axis.title = element_text(size=8))

w2 <- ggplot(data,aes(weekday,casual))+
  geom_boxplot(aes(color=weekday),alpha=0.7,fill="grey")+theme_bw()+
  ggtitle("Boxplot of Weekday and Casual")+
  labs(x="Weekday",y="Casual")+
  theme(plot.title = element_text(face="bold",size=10))+
  theme(axis.title = element_text(size=8))

grid.arrange(w1,w2,nrow=1)

#Identification and imputation of outliers --- count and humidity

#Calculate 7 day moving average for count, registered and casual
data[["cnt_mov_avg_7"]]<-c(rep(NA,times = 3),rollmean(zoo(data$cnt,order.by = data$dteday),k = 7),rep(NA,times = 3))


#Defining upper and lower bounds based on IQR foor outlier detection
cnt_upper<-data$cnt + 1.5*(quantile(data$cnt,0.25,0.75))
cnt_lower<-data$cnt - 1.5*(quantile(data$cnt,0.25,0.75))

cnt_th_up_mav<-c(rep(0,times = 10),rollmean(zoo(cnt_upper,order.by = data$dteday),k = 21),rep(0,times = 10))
cnt_th_lw_mav<-c(rep(0,times = 10),rollmean(zoo(cnt_lower,order.by = data$dteday),k = 21),rep(0,times = 10))


#Defining theme for the Plot

theme_example <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text = element_text(colour = "grey50"),
          axis.title.x = element_text(colour = "grey50"),
          axis.title.y = element_text(colour = "grey50", angle = 90),
          panel.background = element_rect(fill = "white"),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major = element_blank(),
          plot.background = element_blank()
    )
}

ggplot(data,aes(x = as.POSIXct(dteday),y = cnt)) + geom_line() + scale_x_datetime("%Y") +
  geom_line(aes(x=as.POSIXct(dteday),y=cnt_th_up_mav),color = 'blue') +
  geom_line(aes(x=as.POSIXct(dteday),y=cnt_th_lw_mav),color = "red") +theme_example()


out_index<-which((data$cnt < cnt_th_lw_mav | data$cnt > cnt_th_up_mav) & (cnt_th_lw_mav != 0 | cnt_th_up_mav != 0))
data$cnt[out_index]<-data$cnt_mov_avg_7[out_index]

ggplot(data,aes(x = as.POSIXct(dteday),y = cnt)) + geom_line() + scale_x_datetime("%Y") +
  geom_line(aes(x=as.POSIXct(dteday),y=cnt_th_up_mav),color = 'blue') +
  geom_line(aes(x=as.POSIXct(dteday),y=cnt_th_lw_mav),color = "red")+theme_example()

#Humidity outlier Detection
hum_out_index <- which(data$hum==0)
data[["hum_mov_avg_7"]]<-c(rep(NA,times = 3),rollmean(zoo(data$hum,order.by = data$dteday),k = 7),rep(NA,times = 3))
data$hum[hum_out_index] <- data$hum_mov_avg_7[hum_out_index]
data$hum_mov_avg_7 <- NULL
#Decomposing time series data
#Converting count data to ts object
cnt_ts <- ts(data[,c('cnt')],frequency=365)
head(cnt_ts)

#Taking log of data for multiplicative decomposition
cnt_stl <- log(cnt_ts)
s1 <- stl(cnt_stl,s.window = "periodic",robust="TRUE")
autoplot(s1)+theme_example()
s2 <- as.matrix(s1)
s2 <- s1$time.series
s3 <- as.data.frame(s2)
d1 <- exp(s3)

#Plotting decomposed time series
par(mfrow=c(4,1))
p0 <- plot(as.numeric(cnt_ts),type="l")
p1 <- plot(d1$seasonal,type="l")
p2 <- plot(d1$trend,type="l")
p3 <- plot(d1$remainder,type="l")


#Feature Engineering

#Creating lag variables for 7 days
for(i in 1:8){
  data[[str_trim(paste("cnt_lag_",i,sep = ""))]]=lag(data$cnt,i)
}


#Creating lead variables
data[["cnt_tom"]]=lead(data$cnt,1)

head(data[,18:ncol(data)],10)

#rollmean(zoo(data$cnt_lag_1,order.by = data$dteday),k = 7,align = "right")
data[["r_cnt_mov_avg_7"]]<-c(rep(NA,times = 6),rollapply(zoo(data$cnt_lag_1,order.by=data$dteday),7,mean,align = "right"))

summary(data)
#2 day moving average
data[["r_cnt_mov_avg_2"]]<-c(rep(NA,times = 1),rollapply(zoo(data$cnt_lag_1,order.by=data$dteday),2,mean,align = "right"))

#21 day moving average
data[["r_cnt_mov_avg_21"]]<-c(rep(NA,times = 20),rollapply(zoo(data$cnt_lag_1,order.by=data$dteday),21,mean,align = "right"))

#Weekly trend
data[["weekly_trend"]]<-(data$cnt_lag_8 - data$cnt_lag_1)/data$cnt_lag_8

summary(data)


#Splitting data into test and train
train_data_cnt_tom= data$cnt[10:365]
train_data_r_cnt_mov_avg_7= data$r_cnt_mov_avg_7[9:364]
dim(data)
y= cbind(train_data_cnt_tom, train_data_r_cnt_mov_avg_7)
head(y,20)
#Differencing
df1 <- train_data_cnt_tom -train_data_r_cnt_mov_avg_7

#ACF PACF tests
adf.test(df1,alternative = "stationary")
par( mfrow = c( 2, 1 ) )
Acf(df1)
Pacf(df1)

library(lmtest)
#Fitting ARIMA Model
fit.100= arima(df1, order=c(1,0,0))
coeftest(fit.100)
fit.100

tsdisplay(residuals(fit.100), lag.max = 20)

AR <- 14.506656+0.431250*lag(df1,1)
AR
avg_ar <- mean(na.omit(AR))
avg_ar

avg_df1 <- mean(na.omit(df1))
avg_df1
avg= avg_df1-avg_ar

#AR is now zero centered
AR <-14.506656+0.431250*lag(df1,1)-avg

#Applying the same to the entire dataset
diff= data$cnt_tom- data$r_cnt_mov_avg_7
data$AR <- data$r_cnt_mov_avg_7+14.058791+0.432722*lag(diff,1)-avg
data$AR= as.numeric(data$AR)


#Plotting the ARIMA plot
plot(data$cnt_tom, col='gray', type='l')
lines(data$r_cnt_mov_avg_7, lty=2)
lines(data$AR, lwd=2)
legend("topleft", legend=c("Actual", "Moving Average 7 days", "Derived ARIMA"),
       col=c("gray", "black", "black"), lty=1:3, cex=0.8)
str(data)

write.csv(data, "E:/EBAC/6 Data Analytics/Assignment 1 Barry/Data_final.csv")

lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
