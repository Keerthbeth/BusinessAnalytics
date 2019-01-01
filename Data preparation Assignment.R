
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1 Data Preparation and imputations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data1<-read.csv("D:/assignment_data_prep/assignment_data_prep/Assignment_Data.csv")
data2<-read.csv("D:/assignment_data_prep/assignment_data_prep/Assignment_Data2.csv")

library(pacman)
p_load(plyr)

#Join the datasets on full level
data<-join(data1,data2,type="full")
rm(data1,data2)

p_load(magrittr,tidyverse,dplyr)

#Sort by timestamp and unit id
data<-data%>%
  arrange(date_time)
#Sensor 36 may be the first operational sensor!

#Cross-check indices - manual
#which(data$unitid == 'SS0029')[1]
#write.csv(data,file="D:/assignment_data_prep/assignment_data_prep/Sorted_Data.csv")

#Minute impute the dataset to make it ready for time series imputation
data_imp<-data%>%
  mutate(date_time_hm=format(as.POSIXct(date_time,format="%Y-%m-%d %H:%M:%S"),'%Y-%m-%d %H:%M'))

#?complete

data_imp<-data_imp%>%
  mutate(date_time_hm=as.POSIXct(date_time_hm))%>%
  complete(date_time_hm=seq(min(as.POSIXct(date_time_hm)),max(as.POSIXct(date_time_hm)),by = '1 min'))%>%
  select(-one_of("date_time"))
#If above code gives error or takes time, please clear workspace environment and restart R with the script


#write.csv(data_imp,file="D:/assignment_data_prep/assignment_data_prep/Minute_imputed_Data.csv")
p_load(zoo)
p_load(imputeTS)

#Imputing TS as linear interpolation from endpoints via a zoo TS
for (i in 3:length(colnames(data_imp))) {
  if (i > length(colnames(data_imp))){
    break
  } else {
    x <- zoo(data_imp[colnames(data_imp)[i]],data_imp$date_time_hm)
    x <- na.interpolation(x, option = "linear")
    df<-as.data.frame(x)
    data_imp[colnames(data_imp)[i]]<-df[,1]
    
  }
}

#Manual check for imputations
#write.csv(data_imp,file="D:/assignment_data_prep/assignment_data_prep/Minute_imputed_Data.csv")

rm(df,i,x,data)

#Trying different imputations for unit id to verify with support in non imputed dataset
data_impute_1<-data_imp #Mirror preceding
data_impute_2<-data_imp #Linear Discriminant Analysis
data_impute_3<-data_imp #k Nearest Neighbours
data_impute_4<-data_imp #LOCF

#A. Impute unit id using mirror preceding(assuming the first entry for unit id is not empty)

for (i in 2:length(data_impute_1$unitid)){
  if(is.na(data_impute_1$unitid[i]) == TRUE){
    if(is.na(data_impute_1$unitid[i-1]) == FALSE){
        data_impute_1$unitid[i] = data_impute_1$unitid[i-1]
      }
    }
  }
  
rm(i)

#write.csv(data_impute_1,file = "D:/assignment_data_prep/assignment_data_prep/Minute_imputed_Data_mirror.csv")


#B. Impute unit id by using LDA
p_load(MASS)

lda_model<-lda(unitid ~ Temperature + Noise + Light + Co2 + VOC + Humidity,data = data_impute_2)

predictions<-predict(lda_model,newdata=data_impute_2[is.na(data_impute_2$unitid) == FALSE,c(3:8)])$class
table(predictions,data_impute_2[is.na(data_impute_2$unitid) == FALSE,]$unitid)

#heavy misclassifications on level SS0050

for (i in 1:length(data_impute_2$unitid)){
  if(is.na(data_impute_2$unitid[i]) == TRUE){
    data_impute_2$unitid[i]<-predict(lda_model,newdata = data_impute_2[i,c(3:8)])$class
  }
}

rm(i,lda_model,predictions)
#Manual Verification
#write.csv(data_impute_2,file = "D:/assignment_data_prep/assignment_data_prep/Minute_imputed_Data_lda.csv")
#Imputes wrongly! wont be used!

#C. Impute unitid using k-Nearest Neighbours - this algorith considers and compares entire row slices 
#(all columns) to make imputations on unitid(i.e using nearest non NA unitids with similar Temp,Noise,... patterns) 
p_load(VIM,ggplot2)
#Below code may take upto approx 3 mins to execute...please wait
data_impute_3<-kNN(data_impute_3,variable = "unitid",k=5)
#Manual Verification
#write.csv(data_impute_3,file = "D:/assignment_data_prep/assignment_data_prep/Minute_imputed_Data_knn.csv")
#Support for each unitid preserved in imputed dataset

#D. Impute unitid using LOCF
p_load(stringr)
ids<-as.numeric(str_extract(data_impute_4$unitid,"[0-9]+"))
ids_imp<-na.locf(ids)%>%
  sapply(function(x) paste('SS00',x,sep=''))

data_impute_4$unitid<-ids_imp
#Manual verification
#write.csv(data_impute_4,file = "D:/assignment_data_prep/assignment_data_prep/Minute_imputed_Data_locf.csv")

rm(ids,ids_imp)

#Delete 31st March 2017 entry - entire day imputed with 1440 datapoints!

data_impute_3<-data_impute_3[format(data_impute_3$date_time_hm,"%Y-%m-%d") != "2017-03-31",]#KNN - best imputation!
data_impute_4<-data_impute_4[format(data_impute_4$date_time_hm,"%Y-%m-%d") != "2017-03-31",]#LOCF
data_impute_1<-data_impute_1[format(data_impute_1$date_time_hm,"%Y-%m-%d") != "2017-03-31",]#Mirror

#We will be using only kNN from here forth
rm(data_impute_4,data_impute_1,data_impute_2)
#--------------------------------------------------------------------------------------------------------------
#Observations on dataset level
#--------------------------------------------------------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2. Check if variables measured are correlated!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p_load(corrplot)
corrplot(cor(data_impute_3[,3:8]),method = 'number',type = 'upper')

#As we can see VOC and Co2 are positively 'PERFECTLY CORRELATED'.
#Also temperature and humidity are positively correlated.

#Temperature and Light are negatively correlated and Light and Humidity are negatively correlated

#Remember to compare with WHO levels
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3. Comparing recording levels for each sensor
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Temperature
data_impute_3%>%
  group_by(unitid)%>%
  ggplot(aes(x=factor(unitid),y=Temperature)) + geom_boxplot() + labs(x="Sensor ID")

summary(aov(data_impute_3$Temperature~data_impute_3$unitid))

#Light
data_impute_3%>%
  group_by(unitid)%>%
  ggplot(aes(x=factor(unitid),y=Light)) + geom_boxplot() + labs(x="Sensor ID")

summary(aov(data_impute_3$Light~data_impute_3$unitid))

#Noise
data_impute_3%>%
  group_by(unitid)%>%
  ggplot(aes(x=factor(unitid),y=Noise)) + geom_boxplot() + labs(x="Sensor ID")

summary(aov(data_impute_3$Noise~data_impute_3$unitid))

data_impute_3%>%
  group_by(unitid)%>%
  ggplot(aes(x=factor(unitid),y=Co2)) + geom_boxplot() + labs(x="Sensor ID")
#Room 29 has highest CO2 levels and is a common office space

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4. Hourly Plot - To identify trends and habits on any given random day
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_impute_3<-data_impute_3%>%
  mutate(hour=format(date_time_hm,format = "%H"))

#Temperature
data_impute_3%>%
  group_by(hour)%>%
  summarise(aver_temp=mean(Temperature))%>%
  ggplot(aes(hour,aver_temp)) + geom_point(aes(size = 12)) + labs(y="Mean Temperature(C)") + theme(text = element_text(size = 20),legend.position = "none")

#CO2
data_impute_3%>%
  group_by(hour)%>%
  summarise(C=mean(Co2))%>%
  ggplot(aes(hour,C)) + geom_point(aes(size = 12)) + labs(y="Mean CO2(ppm)") + theme(text = element_text(size = 20),legend.position = "none")

#Noise
data_impute_3%>%
  group_by(hour)%>%
  summarise(N=mean(Noise))%>%
  ggplot(aes(hour,N)) + geom_point(aes(size = 12)) + labs(y="Mean Noise(dB)") + theme(text = element_text(size = 20),legend.position = "none")

#Light
data_impute_3%>%
  group_by(hour)%>%
  summarise(L=mean(Light))%>%
  ggplot(aes(hour,L)) + geom_point(aes(size = 12)) + labs(y="Mean Light(LUX)") + theme(text = element_text(size = 20),legend.position = "none")

#Humidity
data_impute_3%>%
  group_by(hour)%>%
  summarise(H=mean(Humidity))%>%
  ggplot(aes(hour,H)) + geom_point(aes(size = 12)) + labs(y="Mean Humidity(%)") + theme(text = element_text(size = 20),legend.position = "none")

#VOC
data_impute_3%>%
  group_by(hour)%>%
  summarise(V=mean(VOC))%>%
  ggplot(aes(hour,V)) + geom_point(aes(size = 12)) + labs(y="VOC(ppm)") + theme(text = element_text(size = 20),legend.position = "none")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5. Daily plot using time-series and moving average
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p_load(lubridate,scales)
#------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------
#Create moving average for Temperature,Noise,Light,CO2,VOC,Humidity with POSIXct Timestamp
p_load(zoo,tidyverse,magrittr,dplyr,scales,xts)
#--------------------------------------------------------------------------------------------------------------------------
l_zoo<-zoo(data_impute_3$Temperature,data_impute_3$date_time_hm)
m_av<-rollmean(l_zoo,1440,fill = list(NA,NULL,NA))
data_impute_3$mv_temp=coredata(m_av)
l_zoo<-zoo(data_impute_3$Noise,data_impute_3$date_time_hm)
m_av<-rollmean(l_zoo,1440,fill = list(NA,NULL,NA))
data_impute_3$mv_noise=coredata(m_av)
l_zoo<-zoo(data_impute_3$Light,data_impute_3$date_time_hm)
m_av<-rollmean(l_zoo,1440,fill = list(NA,NULL,NA))
data_impute_3$mv_light=coredata(m_av)
l_zoo<-zoo(data_impute_3$Co2,data_impute_3$date_time_hm)
m_av<-rollmean(l_zoo,1440,fill = list(NA,NULL,NA))
data_impute_3$mv_co2=coredata(m_av)
l_zoo<-zoo(data_impute_3$VOC,data_impute_3$date_time_hm)
m_av<-rollmean(l_zoo,1440,fill = list(NA,NULL,NA))
data_impute_3$mv_voc=coredata(m_av)
l_zoo<-zoo(data_impute_3$Humidity,data_impute_3$date_time_hm)
m_av<-rollmean(l_zoo,1440,fill = list(NA,NULL,NA))
data_impute_3$mv_hum=coredata(m_av)
rm(l_zoo,m_av)
#--------------------------------------------------------------------------------------------------------------------------
#Note: 28/Feb - 01/Mar is 00:00-23:59 of 01/Mar
#MARCH - Daily timeseries plot with trend-line using moving average rolled up to 1440 min => 1 day 
#--------------------------------------------------------------------------------------------------------------------------
data_impute_3[format(data_impute_3$date_time_hm,format = "%m") < "04",]%>%
  ggplot(aes(date_time_hm,Temperature)) + geom_line() + geom_line(aes(date_time_hm,mv_temp),color="blue") +
  labs(y="Temperature(C)",x="Day of March 2017") + 
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d")) + theme(axis.text.x = element_text(angle = 90))

data_impute_3[format(data_impute_3$date_time_hm,format = "%m") < "04",]%>%
  ggplot(aes(date_time_hm,Noise)) + geom_line() + geom_line(aes(date_time_hm,mv_noise),color="blue") + 
  labs(y="Noise(dB)",x="Day of March 2017") + 
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d")) + theme(axis.text.x = element_text(angle = 90))

data_impute_3[format(data_impute_3$date_time_hm,format = "%m") < "04",]%>%
  ggplot(aes(date_time_hm,Light)) + geom_line() + geom_line(aes(date_time_hm,mv_light),color="blue") +
  labs(y="Light(LUX)",x="Day of March 2017") + 
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d")) + theme(axis.text.x = element_text(angle = 90))

data_impute_3[format(data_impute_3$date_time_hm,format = "%m") < "04",]%>%
  ggplot(aes(date_time_hm,Co2)) + geom_line() + geom_line(aes(date_time_hm,mv_co2),color = "blue") +
  labs(y="CO2(ppm)",x="Day of March 2017") + 
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d")) + theme(axis.text.x = element_text(angle = 90))

data_impute_3[format(data_impute_3$date_time_hm,format = "%m") < "04",]%>%
  ggplot(aes(date_time_hm,VOC)) + geom_line() + geom_line(aes(date_time_hm,mv_voc),color = "blue") + 
  labs(y="VOC(ppm)",x="Day of March 2017") + 
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d")) + theme(axis.text.x = element_text(angle = 90))

data_impute_3[format(data_impute_3$date_time_hm,format = "%m") < "04",]%>%
  ggplot(aes(date_time_hm,Humidity)) + geom_line() + geom_line(aes(date_time_hm,mv_hum),color = "blue") + 
  labs(y="Humidity(%)",x="Day of March 2017") + 
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d")) + theme(axis.text.x = element_text(angle = 90))
#---------------------------------------------------------------------------------------------------------------------------
#APRIL - Daily timeseries plot with trend-line using moving average rolled up to 1440 minutes => 1 day
#---------------------------------------------------------------------------------------------------------------------------
data_impute_3[format(data_impute_3$date_time_hm,format = "%m") == "04",]%>%
  ggplot(aes(date_time_hm,Temperature)) + geom_line() + geom_line(aes(date_time_hm,mv_temp),color="blue") +
  labs(y="Temperature(C)",x="Day of April 2017") + 
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d")) + theme(axis.text.x = element_text(angle = 90))

data_impute_3[format(data_impute_3$date_time_hm,format = "%m") == "04",]%>%
  ggplot(aes(date_time_hm,Noise)) + geom_line() + geom_line(aes(date_time_hm,mv_noise),color="blue") + 
  labs(y="Noise(dB)",x="Day of April 2017") + 
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d")) + theme(axis.text.x = element_text(angle = 90))

data_impute_3[format(data_impute_3$date_time_hm,format = "%m") == "04",]%>%
  ggplot(aes(date_time_hm,Light)) + geom_line() + geom_line(aes(date_time_hm,mv_light),color="blue") +
  labs(y="Light(LUX)",x="Day of April 2017") + 
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d")) + theme(axis.text.x = element_text(angle = 90))

data_impute_3[format(data_impute_3$date_time_hm,format = "%m") == "04",]%>%
  ggplot(aes(date_time_hm,Co2)) + geom_line() + geom_line(aes(date_time_hm,mv_co2),color = "blue") +
  labs(y="CO2(ppm)",x="Day of April 2017") + 
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d")) + theme(axis.text.x = element_text(angle = 90))

data_impute_3[format(data_impute_3$date_time_hm,format = "%m") == "04",]%>%
  ggplot(aes(date_time_hm,VOC)) + geom_line() + geom_line(aes(date_time_hm,mv_voc),color = "blue") + 
  labs(y="VOC(ppm)",x="Day of April 2017") + 
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d")) + theme(axis.text.x = element_text(angle = 90))

data_impute_3[format(data_impute_3$date_time_hm,format = "%m") == "04",]%>%
  ggplot(aes(date_time_hm,Humidity)) + geom_line() + geom_line(aes(date_time_hm,mv_hum),color = "blue") + 
  labs(y="Humidity(%)",x="Day of April 2017") + 
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d")) + theme(axis.text.x = element_text(angle = 90))
#--------------------------------------------------------------------------------------------------------------
#Verification using plot.xts's moving average builtin
#--------------------------------------------------------------------------------------------------------------
#help("plot.zoo")  
#l_xts<-as.xts(l_zoo)
#plot(l_xts)
#lines(TTR::SMA(l_xts,n=1440),col = "blue")
#--------------------------------------------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Extra.Interactive Viewer for minute details
#We have not excluded outliers since these observations may record anomalous behaviour of indoor systems
#and our collection of data from these systems is intended to monitor them and act on such anomalies
#Excluding data points blindly without this consideration defeats the purpose of IoT monitoring!
#We will examine the data at minute detail in this section using dygraphs to report on such anomalies 
#if any found
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p_load(dygraphs)
#--------------------------------------------------------------------------------------------------------------
xts_temp<-xts(data_impute_3$Temperature, order.by = data_impute_3$date_time_hm)
xts_humi<-xts(data_impute_3$Humidity, order.by = data_impute_3$date_time_hm)
xts_data<-cbind(xts_temp, xts_humi)
rm(xts_temp,xts_humi)
dygraph(xts_data)%>%
  dyRangeSelector()
#There is an abnormally low value of humidity on 2nd March 05:36 am of 46.4 with a minor surge of temperature
#This phenomena started at 5:35 am and receded by 5:37 am. The humidity stabilized by 5:45 am
#This is infact the global minima for humidity!
data_impute_3[which(data_impute_3$Humidity == min(data_impute_3$Humidity)),"date_time_hm"]

#There is an erratic pattern of humidity fluctuations observed on 16th April from 6:13 am all the way upto 8:48 am.
#This is accompanied by small fluctuations in temperature also. Humidity seems to have adhered thereafter to cyclicity
#---------------------------------------------------------------------------------------------------------------------
xts_co2<-xts(data_impute_3$Co2,order.by = data_impute_3$date_time_hm)
xts_voc<-xts(data_impute_3$VOC,order.by = data_impute_3$date_time_hm)
xts_data<-cbind(xts_co2,xts_voc)
rm(xts_co2,xts_voc)
dygraph(xts_data)%>%
  dyRangeSelector()
#A comparison over two months reveals that the pattern gradually spreads out over the two months and hence 
#finding outliers here would eliminate important information such as the spike in both CO2 and VOC on 25th April
#at 4:29 pm which are the global maxima and indicative of a flash rise in emissions detected by system due to
#some event near the sensor SS0029. The maxima reached together along with the fact that their sample correlation is
#1 justifies the observation's non exclusion as an outlier observation
data_impute_3[which(data_impute_3$VOC == max(data_impute_3$VOC)),c("date_time_hm","unitid")]
#---------------------------------------------------------------------------------------------------------------------
#We will be comparing Light and Noise separately each of which may be indicative of activity
xts_light<-xts(data_impute_3$Light,order.by = data_impute_3$date_time_hm)
dygraph(xts_light)%>%
  dyRangeSelector()
#There is a sudden and random dimming at 10:44 pm followed by spiked brightening of light source upto
#11:02 pm on 10th March. This phenomenon may be indicative of issues with neutral wiring of the light source
#and may warrant inspection
#There was an anomalous usage of light on 11 & 12 March Sunday & Monday: at 5:41 to 6:10 pm on 11th and 11:05pm
#to 02:35 am on 11-12th. Similarly on 1st April 5:17pm to 6:10 pm, 14th April(Good Friday) 5:04pm to 8:10 pm and
#22nd April 9:23pm to 10:46 pm. More prominently, the intensity of usage of light seems to follow a decreasing trend
#---------------------------------------------------------------------------------------------------------------------
rm(xts_light)
xts_noise<-xts(data_impute_3$Noise,order.by = data_impute_3$date_time_hm)
dygraph(xts_noise)%>%
  dyRangeSelector()
#As discussed earlier there is a minute but sharp drop in background noise from 16th April onwards. This may be a deliberate
#calibration effort by the administration team to improve quality of noise recorded over background noise.
#A global maxima is reached on 2nd April at 5:38 pm
data_impute_3[which(data_impute_3$Noise == max(data_impute_3$Noise)),"date_time_hm"]

#Thanks :)