library(colorRamps)
library(forecast)
library(ggplot2)
library(RColorBrewer)
library(lattice)
library(reshape2)
library(reshape)

library(tidyr)
setwd("C:/Users/lee/Desktop/data_prep")


aic.mat <- read.csv("g_heatmap.csv")
aic.mat[aic.mat == 0] <- max(aic.mat)
aic.mat2 <- as.matrix(aic.mat)

RMSE.mat <- read.csv("g_RMSE_heatmap.csv")
RMSE.mat2 <- as.matrix(RMSE.mat)

RMSE.mat3<-melt(RMSE.mat2)
RMSE.mat3[(RMSE.mat3$value %in% boxplot.stats(RMSE.mat3$value)$out),3] <-0  # outlier
RMSE.mat3[RMSE.mat3$value == 0,3] <- max(RMSE.mat3$value)
boxplot(RMSE.mat3$value)
m1<-matrix(RMSE.mat3$value,nrow=75,ncol = 100,byrow =F) 

rownames(m1)<-c(paste(1:75,sep = ""))

colnames(m1)<-c(paste("V",1:100,sep = ""))


RMSE.mat4<-(m1-mean(m1))/sd(m1)
aic.mat2<-(aic.mat2-mean(aic.mat2))/sd(aic.mat2)
total<-aic.mat2+RMSE.mat4

length(boxplot.stats(RMSE.mat3$value)$out)

f <- function(m) t(m)[,nrow(m):1]


# heatmap(aic.mat2,Colv=NA,Rowv=NA,scale="none")

my_panel <- function(...) {
  panel.levelplot(...)
  panel.abline(h=seq(0,75.5,by=5)-4.5, v =seq(0,100.5,by=5)-4.5, lty = 2)
  panel.rug(x = (0:100)-1, y = (0:75)-1, 
            start = 1, end =2,
            col="black")
}

par(mfrow=c(1,2))

#aic.mat2;RMSE.mat4;total
levelplot(f(aic.mat2),xlab = "", ylab = "",scales=list(x=list(rot=90, alternating=3)), panel = my_panel,
          main=list('Subway ARIMA model grid search',side=1,line=0.5), region=T,colorkey=T, col.regions= colorRampPalette(brewer.pal(11, "RdBu"))(25))

levelplot(f(RMSE.mat4),xlab = "", ylab = "",scales=list(x=list(rot=90, alternating=3)), panel = my_panel,
          main=list('Subway ARIMA model grid search',side=1,line=0.5), region=T,colorkey=T, col.regions= colorRampPalette(brewer.pal(11, "RdBu"))(25))

levelplot(f(total),xlab = "", ylab = "",scales=list(x=list(rot=90, alternating=3)), panel = my_panel,
          main=list('Subway ARIMA model grid search',side=1,line=0.5), region=T,colorkey=T, col.regions= colorRampPalette(brewer.pal(11, "RdBu"))(25))

## ggplot

#aic.mat_melt <- melt(aic.mat2)


#aic.mat_melt$small_i[aic.mat_melt$Var1 <= 25] <- 0
#aic.mat_melt$small_i[aic.mat_melt$Var1 <= 50 & aic.mat_melt$Var1 >= 26] <- 1
#aic.mat_melt$small_i[aic.mat_melt$Var1 >= 51] <- 2

#aic.mat_melt$drift[1:3750] <- 1
#aic.mat_melt$drift[3750:7500] <- 2

#aic.mat_melt$drift[1:3750] <- 1
#aic.mat_melt$drift[3750:7500] <- 2

#library(ggExtra)
# 

#p <- ggplot(aic.mat_melt, aes(Var2, Var1, fill = value)) + geom_tile() 
#p <- p + theme_minimal() + scale_y_reverse(breaks =c(0, 25, 50, 75)) + labs(x="",y="")
# p <- p + scale_fill_gradient(low = "black", high = "white")
#p <- p + scale_colour_manual(values = brewer.pal(3,"BuPu"))
#p <- p + labs(title = "Subway ARIMA model Grid search")
#p <- p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))

# p <- p + scale_y_continuous(breaks =c(0,5,10,15, 20, 25))
# p <- p + scale_x_continuous(breaks =c(0,5,10,15, 20, 25))


#p


data(french_fries)
ffm <- melt(french_fries, id = 1:4, na.rm = TRUE)

# Aggregate examples - all 3 yield the same result
dcast(ffm, treatment ~ .)
dcast(ffm, treatment ~ ., function(x) length(x))
dcast(ffm, treatment ~ ., length) 

# Passing further arguments through ...
dcast(ffm, treatment ~ ., sum)
dcast(ffm, treatment ~ ., sum, trim = 0.1)
