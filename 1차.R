library(forecast)
library(ggplot2)
getwd()
setwd("C:/Users/lee/Desktop/data_prep/data_prep")

gangnam<-read.csv("gangnam_2012_2017_in.csv",header = T)

str(gangnam)

gangnam1<-ts(gangnam$합계,frequency=365,start=c(2012,01,01),end=c(2017,09,30))

plot(gangnam1)
acf(gangnam1)
pacf(gangnam1)
# season 7 / MA 2
#??arima
auto.arima(gangnam1)

# MA : 평균의 error 에 대한 고려

gangnam$day = as.Date(gangnam$날짜)
gangnam1<-gangnam[1460:1825,]

ggplot(gangnam, aes(day, 합계)) + geom_line()  + ylab("") + xlab("") + scale_x_date('month') 

plot(gangnam1$합계,type="l")

count_ts = ts(gangnam[, c('합계')])

#length(gangnam$total);length(count_ts)
#gangnam$total = tsclean(count_ts)

#ggplot() +
#  geom_line(data = gangnam, aes(x = day, y = total)) + ylab('Cleaned Bicycle Count')


gangnam$cnt_ma = ma(gangnam$합계, order=7) 
gangnam$cnt_ma30 = ma(gangnam$합계, order=30)
gangnam$cnt_ma365 = ma(gangnam$합계, order=365)

ggplot() +
  geom_line(data = gangnam, size = 1.5,aes(x = day, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = gangnam, size = 2,aes(x = day, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  geom_line(data = gangnam,colour="darkgray", aes(x = day, y = 합계, colour = "Counts")) +
  #geom_line(data = gangnam, aes(x = day, y = cnt_ma365, colour = "yearly Moving Average"))  +
  ylab('강남 승차량')

??ggplot
###

require(graphics)

arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          sd = sqrt(0.1796))

# mildly long-tailed
arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          rand.gen = function(n, ...) sqrt(0.1796) * rt(n, df = 5))

# An ARIMA simulation
ts.sim <- arima.sim(list(order = c(0,0,2), ma = c(0.2,0.8)), n = 200)

acf(ts.sim);pacf(ts.sim);
## ar acf 감소 / pacf 사라짐
## ma acf 사라 / pacf 감ㅅ

ts.plot(ts.sim)



