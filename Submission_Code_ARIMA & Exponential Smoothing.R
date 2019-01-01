library(ggplot2)
library(forecast)
#install.packages('forecast')
library(zoo)
library(ggseas)
#install.packages('ggseas')
library(dygraphs)
library(ggplot2)
library(tidyverse)
library(fpp2)  
library(tseries)
library(lmtest)


data <- read.delim("E:/EBAC/6 Data Analytics/Assignment 3 ARIMA/GRPRating.csv")
str(data)
dates <- as.Date(data$Date,"%d-%b-%y")
head(dates)
head(data)
data$Date <- NULL
head(data)
t1= zoo(data, as.Date(dates, "%d-%b-%y"))
theme_set(theme_minimal())
autoplot.zoo(t1)+ 
  geom_line(color = "#00AFBB", size = 1)+
  scale_x_date(date_labels = "%b/%Y")+    
  ylim(0,350)+   xlab("Date") +ggtitle("GRP Rating")

################Spilt of data into test & train #########################
sta=as.Date("17-Jun-2007", "%d-%b-%Y")
mid1=as.Date("26-Oct-2008", "%d-%b-%Y")
mid2=as.Date("27-Oct-2008",  "%d-%b-%Y")
last= as.Date("15-Mar-2009", "%d-%b-%Y")

train_data= window(t1, start= sta, end= mid1)
test_data= window(t1, start=mid2, end=last)

autoplot.zoo(cbind(train_data, test_data), facets = FALSE)+
  ylim(0,350)+geom_line(size=2)+theme(legend.position = "bottom")+
  xlab("Date") +ggtitle("GRP Rating")

#######Simple Exponential Smoothing#####################################

sess.grp= ses(train_data, alpha=0.2, h=20)
accuracy(sess.grp, test_data)

sess.grp$model

#                 ME     RMSE      MAE        MPE     MAPE       MASE      ACF1
# Training set -4.86566388 20.37295 15.51838 -2.6716200 6.371149 0.06165073 0.4427124
# Test set     -0.02319758 12.47568 10.31064 -0.3819723 5.060223 0.04096165        NA

# identify optimal alpha parameter
alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  fit <- ses(train_data, alpha = alpha[i], h = 100)
  RMSE[i] <- accuracy(fit, test_data)[2,2]
}

# convert to a data frame and idenitify min alpha value
grp.fit <- data_frame(alpha, RMSE)
alpha.min <- filter(grp.fit, RMSE == min(RMSE))
alpha.min

#Alpha is already optimised for 0.2

plot(sess.grp, ylim=c(0,350))
lines(test_data)
# plot RMSE vs. alpha
ggplot(grp.fit, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = alpha.min, aes(alpha, RMSE), size = 2, color = "blue")+theme_bw()


###############Double Exponential Smoothing#############################

holt.grp <- holt(train_data, h = 20)
accuracy(holt.grp, test_data)
#                   ME     RMSE      MAE        MPE      MAPE       MASE       ACF1
# Training set -0.4665802 17.97437 12.72234 -0.6308713  5.143234 0.05054276 0.06329432
# Test set     30.0823284 33.91115 30.08233 14.5381426 14.538143 0.11950974         NA

#Model is overfitting, so changing value of alpha to optimal

holt.grp <- holt(train_data, h = 20, alpha=0.2)
accuracy(holt.grp, test_data)

#                   ME     RMSE      MAE        MPE     MAPE       MASE      ACF1
# Training set -0.03646521 19.70689 15.62065 -0.6874105 6.260175 0.06205704 0.4270719
# Test set     16.10291388 22.62657 17.73661  7.5758963 8.450196 0.07046322        NA

# identify optimal alpha parameter
alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  fit <- holt(train_data, alpha = alpha[i], h = 100)
  RMSE[i] <- accuracy(fit, test_data)[2,2]
}

# convert to a data frame and idenitify min alpha value
grp.fit <- data_frame(alpha, RMSE)
alpha.min <- filter(grp.fit, RMSE == min(RMSE))
alpha.min

# In our model we used the standard ??=0.20; however, we can tune our alpha parameter to 
# identify the value that reduces our forecasting error. Here we loop through alpha values
# from 0.01-0.99 and identify the level that minimizes our test RMSE. Turns out that ??=0.03
# minimizes our prediction error.

holt.grp <- holt(train_data, h = 20, alpha=0.03)
accuracy(holt.grp, test_data)

#                 ME     RMSE      MAE       MPE     MAPE       MASE     ACF1
# Training set -1.959689 23.49274 18.94206 -1.011598 7.707616 0.07525217 0.628052
# Test set     -3.521702 12.37805 10.60184 -2.098321 5.286582 0.04211852       NA



# plot RMSE vs. alpha
ggplot(grp.fit, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = alpha.min, aes(alpha, RMSE), size = 2, color = "blue")+theme_bw()

holt.grp$model

alpha.min

fc2 <- holt(train_data, damped=TRUE, phi = 0.9, h=20)
accuracy(fc2, test_data)

#                 ME     RMSE      MAE       MPE     MAPE       MASE       ACF1
# Training set -2.945385 17.61509 12.61052 -1.562995 5.141075 0.05009852 0.02802187
# Test set     19.204748 22.90191 19.20475  9.188852 9.188852 0.07629577         NA

#optimising test RMSE by iterating over both alpha & beta

beta <- seq(.0001, .5, by = .001)
alpha <- seq(.01, .99, by = .01)

RMSE <- matrix(nrow = length(alpha),ncol = length(beta))

for(i in 1:length(alpha)){
  for(j in 1:length(beta)) 
  {
    if(beta[j]<alpha[i])
    {
      fit<- holt(train_data, beta = beta[j],alpha= alpha[i], h=20)
      RMSE[i,j] <- accuracy(fit, test_data)[2,2]
    }
    else RMSE[i,j]= NA
  }
}

which(RMSE == min(RMSE), arr.ind = TRUE)
alpha[2] #0.02
beta[15] #0.0141
fit<- holt(train_data, beta = 0.0141,alpha= 0.02, h=20)
fit$model
fit_forecast= forecast(fit, test_data)
fitted_val=fit_forecast$mean
accuracy(fit, test_data)
plot(fit, ylim=c(0,350))
lines(test_data)

# ME     RMSE       MAE        MPE     MAPE       MASE   ACF1
# Training set -2.0728629 23.58072 19.141968 -1.0115371 7.771569 0.07604636 0.6339
# Test set     -0.4867839 11.85240  9.659485 -0.5860842 4.744687 0.03837477     NA


nrz= nrow(RMSE)
ncz= nrow(RMSE)
jet.colors= colorRampPalette(c("blue","green"))
nbcol= 100
color= jet.colors(nbcol)
zfacet= RMSE[-1,-1]+RMSE[-1, -ncz]+RMSE[-nrz,-1]+RMSE[-nrz, -ncz]
facetcol= cut(zfacet, nbcol)
persp(alpha, beta, RMSE, theta=30, col= color[facetcol],ticktype = "detailed", border = NA, expand=0.5)

################Triple Expononent Smoothing #######################################

data3= ts(data, frequency=26)
train_data3= window(data3, sta=c(1,1), end=c(3,20))
test_data3= window(data3, sta=c(3,21))

beta <- seq(.01, .5, by = .05)
alpha <- seq(.01, .99, by = .01)
gamma= seq(.01, .5, by = .05)

RMSE <- array(rep(1, length(alpha)*length(beta)*length(gamma)), dim=c(length(alpha), length(beta), length(gamma)))

for(i in 1:length(alpha))
{  for(j in 1:length(beta)) 
{ for(k in 1:length(gamma))
{
  fit1<- HoltWinters(train_data3, beta = beta[j],alpha= alpha[i], gamma = gamma[k], seasonal = c("multiplicative"))
  fit= forecast(fit1, h=20)
  RMSE[i,j,k] <- accuracy(fit, test_data)[2,2]
}
}
}

which(RMSE==min(RMSE), arr.ind = TRUE)

RMSE1= as.data.frame(RMSE)
which(RMSE==min(RMSE, na.rm = T), arr.ind = TRUE)

fit1<- HoltWinters(train_data3, beta = beta[6],alpha= alpha[5], gamma = gamma[10], seasonal = c("multiplicative"))
fit1
fit= forecast(fit1, h=20)
plot(fit, ylim=c(0,350))
lines(test_data3, col='red')
accuracy(fit, test_data)

################################ARIMA ##################################

adf.test(train_data)
adf.test(diff(train_data))
adf.test(diff(diff(train_data)))
diff.train_data=diff(train_data)

arima_fit=auto.arima(train_data)
arima_fit
fit= forecast(arima_fit,h=20)
coeftest(arima_fit)

accuracy(fit, test_data)

#                 ME     RMSE      MAE        MPE     MAPE       MASE       ACF1
# Training set -1.40217 17.80302 12.54064 -0.9994919 5.093436 0.04982090 0.04410197
# Test set     20.10704 23.66295 20.10704  9.6380142 9.638014 0.07988034         NA

plot(fit, ylim=c(0,350))
lines(test_data, col='red')

#Results from JMP
result_jmp= c('180.36139234','179.35652444','178.35165655','177.34678865','176.34192075','175.33705285','174.33218495','173.32731706','172.32244916','171.31758126','170.31271336','169.30784546','168.30297757','167.29810967','166.29324177','165.28837387','164.28350597','163.27863808','162.27377018','161.26890228')

value=as.data.frame(fit)

plot(result_jmp, type='l', ylim=c(0,350))
lines(as.numeric(test_data), col='red')
lines(value$`Point Forecast`, col='blue')

JMP_output= zoo( as.numeric(result_jmp),dates[73:92])
R_output= zoo(as.numeric(value$`Point Forecast`),dates[73:92])
output= cbind(test_data, JMP_output, R_output)
plot(output)
autoplot.zoo(output, facets = FALSE)+theme_minimal()+  labs(title="Comparison of JMP & R Outputs")+ 
  scale_y_continuous("GRP", limits = c(0,350))+theme(legend.position = "bottom")


as.numeric(fit$x)
tsdisplay(residuals(fit), lag.max=45, main='(0,1,1) Model Residuals')

arima_fit=arima(train_data, order=c(1,1,1))
coeftest(arima_fit)

fit= forecast(arima_fit,h=20)
accuracy(fit, test_data)

#                 ME     RMSE      MAE       MPE     MAPE       MASE       ACF1
# Training set -1.500029 17.70909 12.52898 -1.052491 5.090397 0.04977457 0.01717023
# Test set     17.129455 21.21543 17.12946  8.155040 8.155040 0.06805114         NA


plot(fit, ylim=c(0,350))
lines(test_data, col='red')
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')
library(lmtest)

arima_fit=arima(train_data, order=c(0,1,1))
coeftest(arima_fit)
fit= forecast(arima_fit,h=20)
accuracy(fit, test_data)
plot(fit, ylim=c(0,350))
lines(test_data, col='red')
tsdisplay(residuals(fit), lag.max=45, main='(0,1,1) Model Residuals')

############################SARIMA ############################################

arima_fit1=arima(train_data, order=c(0,1,1), seasonal = list(order = c(0,1,0), period =26))
arima_fit1
coeftest(arima_fit1)
fit1= forecast(arima_fit1,h=20)
accuracy(fit1, test_data)
plot(fit1, ylim=c(0,350))
lines(test_data, col='red')
tsdisplay(residuals(fit1), lag.max=45, main='(0,1,1)(0,1,0)26 Model Residuals')
checkresiduals(fit1)


########################## Analysis on the best Model Fitted #############################

t2= zoo(cbind(fitted_val, test_data), as.Date(dates[73:92], "%d-%b-%y"))

#The second order smoothing lead to the best value

autoplot.zoo(t2, facets = FALSE)+
  theme(legend.position = "bottom") +geom_line(size=1)+
  ylim(0,350)+xlab("Forecasted Week Number")+ylab("GRP") +ggtitle("Actual Vs Predicted")

Deviation= round((t2$GRP-t2$fitted_val)*100/t2$GRP,2)
Deviation

