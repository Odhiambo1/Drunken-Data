#**********************************
#Read the data into R
#**********************************
Brent<-read.csv("E:\\series\\MARX\\Data Science Projects\\Data\\Brentspot.csv",header=TRUE)
Brent
View(Brent)
class(Brent)

WTI<-read.csv("E:\\series\\MARX\\Data Science Projects\\Data\\WTI.csv",header=TRUE)
WTI
#extract the prices and merge
Brent.price<-Brent$Price
WTI.price<-WTI$Price
Price<-as.data.frame(cbind(Brent.price,WTI.price,difference))
names(Price)
class(Price)
nrow(Price)
#************************************************
                #Plots
#************************************************
#simple plot
B<-ts(Brent.price,frequency = 12,start = c(1987,5))
W<-ts(WTI.price,frequency = 12,start = c(1987,5))
plot(B,type='l',xlab="Time",ylab="Price($/barrel)",col=3, main="crude oil Prices",zoom=T)
lines(W,lty=2, col= "red")
 
OilPrice<-ts(Price,frequency = 12,start = c(1987,5))
OilPrice
time(OilPrice)
plot(OilPrice)
par(mfcol = c(2, 1))
summary(OilPrice)
difference<- Brent.price- WTI.price

#plotly plots

library(plotly)
library(tidyr)
library(plyr)
#Brent
p1 <- plot_ly( x=time(B), y = B,type = "scatter",mode="lines")
layout(p1,title= "Brent crude oil Prices",
       xaxis=list(title="Time"),
       yaxis=list("$/barrel"))
#WTI
p2 <- plot_ly( x=time(W), y = W,type = "scatter",mode="lines",color = "orange")
layout(p2,title= " WTI crude oil Prices",
       xaxis=list(title="Time"),
       yaxis=list("$/barrel"))

#Brent and WTI
Oil<-as.data.frame(OilPrice)%>%
  gather(index,price)%>%
  mutate(time=time(OilPrice))

Time<-Oil$time
PRICE<-Oil$price
INDEX<-Oil$index
plot_ly(Oil,x=Time,y=PRICE,color = INDEX,type = "scatter",mode="lines")%>%
layout(Oil,title="Brent and WTI Crude Oil Prices",
       xaxis=list(title="Time"),
       yaxis=list(title="Price in dolars/barrel"))




A<-c(35,40,45,50,55,60,65)
B<-c(-0.4,0,.4,1,.4,0,-.4)


Win<-plot_ly(x=A,y=B,type = "scatter",mode="lines")
layout(Win,title="Profit and loss ",xaxis=list(title="Price")
       ,yaxis=list(title="value of contract"))

#####write.csv(Price,file="CombinedOildata.csv",row.names = FALSE)####

#************************************************
#Forecasting
#************************************************
#convert to log price 
Decompose.Brent<-decompose(B)
Decompose.Brent
plot(Decompose.Brent)#gives out the components graph

#remove seasonality by subtracting the component
seasonality.Brent<-Decompose.Brent$seasonal
seasonality.Brent
Trend.Brent<-B-seasonality.Brent
Trend.Brent
plot(Trend.Brent)

#FORECASTING
Forecast1<-HoltWinters(Trend.Brent,beta=FALSE,gamma=FALSE)
Forecast1
Forecast1.fitted<-Forecast1$fitted
Forecast1.fitted
plot(Forcast1)#the forecast is plotted in red
Z<-Forecast1$SSE #test for accuracy of the 
Z
Trend.Brent[1]
Forecast2<-HoltWinters(Trend.Brent,beta=FALSE,gamma=FALSE,start= -0.6060844)
Forecast2
library(forecast)
Forecast3<-forecast.HoltWinters(Forecast1,h=48)
Forecast3 
plot.forecast(Forecast3)
plot(Forecast3)
Residuals1<-Forecast3$residuals
Residuals1
