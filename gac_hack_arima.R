#Data cleaning and preparation done on excel
#Display Share and Feature Share na's replaced by 0
#Macro economic variables replaced by average
#Note to scale down the the macro economic variables for model
#convergence

#Input the file
inp_data<-read.csv("GAC Hackathon_Sales_Data.csv",header=TRUE)

#Checking correlation between numerical fields to intuitively #identify importance of attributes
work_data<-inp_data
library(dplyr)
library(corrplot)
x1<-work_data[,(9:55)]
x<-x1[,-c(2,3)]

corr_matrix<-cor(x)
corrplot(corr_matrix, method="circle",type="upper", tl.col="black", tl.srt=90)
write.csv(corr_matrix, "corr_matrix.csv")

#Find number of unique products
prods<-unique(work_data$Product)

#Analyze the time series
sales_ts<-work_data[which(work_data$Product=="BRAND140 24-PACK 11.2-13 OZ GLASS"),c("Week","Unit.Sales")]

#plot(sales_ts$Week,sales_ts$Unit.Sales)

#Time series plot
x_ts<-as.ts(sales_ts$Unit.Sales)
plot(x_ts,xlab="Time",ylab="Unit Sales",main="Unit Sales Time Series Plot")

#To decompose the TS
newts<-ts(x_ts,deltat = 1/52)
plot(decompose(newts))

#TS signature plots
acf(x_ts)
pacf(x_ts)

#Unit Test
library(tseries)
adf.test(x_ts)
adf.test(diff(x_ts,lag=52))
adf.test(diff(diff(x_ts,lag = 52)))

#Test for randomness
Box.test(diff(diff(x_ts,lag = 52)))

library(zoo)
library(lmtest)

grangertest(work_data$Average.of.Mean.TemperatureC~work_data$Unit.Sales)

#SARIMAX carried out for various levels of the product
work_data<-inp_data[which(inp_data$Product=="BRAND318 30-PACK 12 OZ CAN"),]


xreg_data<-work_data[,-c(1,2,3,4,5,6,7,8,9,10,11,13,16,19,20,21,23,24,25,26,27,28,30,31,33,36,37,39,43,50,52)]
a<-arima(work_data$Unit.Sales,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=52),xreg = xreg_data)
coeftest(a)
