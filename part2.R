setwd("C:/Users/lee/Desktop/data_prep/data_prep")
getwd()
install.packages("ggplot2")
library(ggplot2)
library(forecast)
ite<-read.csv("part2_time.csv",header = F)
  plot(ite)

colnames(ite)
bp<-ggplot(data = ite, aes(x = V1, y = V2,fill=V1,color=V1)) +
  geom_boxplot()
bp + scale_fill_hue(l=40, c=35) + scale_color_grey(start=0.5,end=0.5)+ylab("승차량-하차량") + xlab("시간")



ggplot(data = ite, aes(x = V1, y = V2)) +   
  geom_boxplot(fill="darkgray",color="grey50") +  
  theme_minimal() + 
  geom_hline(yintercept = 0,color="darkgray")

plot(ts(ite$V2))
tsdisplay(ts(ite$V2))


theme_minimal()ggplot(data = ite, aes(x = V1, y = V2)) +   geom_boxplot(fill="darkgray",color="grey25")


bp + scale_fill_brewer(palette = "Set3")

bp + scale_fill_brewer(palette="Dark2")
str(ite)


bp+scale_color_grey()
