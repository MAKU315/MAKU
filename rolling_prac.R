install.packages("zoo")
library(zoo)
setwd("C:/Users/lee/Documents")
?read.csv
rawdata<-read.csv("Dt1.csv" ,header =T ,sep=',' , raw)
rawdata<-as.data.frame(rawdata)

as.yearmon(time(rawdata[,1]))
xdata<-rawdata[,1:3]


z <- zoo(11:15, as.Date(31:35))
rollapply(z, 2, mean)

zoo(rawdata)
z2 <- zoo(rnorm(6))
rollapply(z2, 3, mean, by = 3)
aggregate(z2, c(3,3,3,6,6,6), mean)

seat <- as.zoo(log(UKDriverDeaths))
time(seat) <- as.yearmon(time(seat))
seat <- merge(y = seat, y1 = lag(seat, k = -1),
              y12 = lag(seat, k = -12), all = FALSE)

rr <- rollapply(seat, width = 36,
                FUN = function(z) coef(lm(y ~ y1 + y12, data = as.data.frame(z))),
                by.column = FALSE, align = "right")
plot(rr)
