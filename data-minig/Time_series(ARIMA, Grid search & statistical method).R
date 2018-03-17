# remove all of things in the workspace before analysis
rm(list=ls())

# plot library 
library(ggplot2)

# basic time series model ARIMA ..
library(tseries)
# time series model Auto.ARIMA ..
library(forecast)

# add color in R
library(RColorBrewer)
library(lattice)


# set the work directory
setwd("C:/Users/user/Desktop")

# load time series data
si<-read.csv("____.csv",header = T)

# set the training set 
train_obs<-1827
h_train<-si[1:train_obs,]

# set the  testing set 
h_test<-si[train_obs:dim(si)[2],]

# transform data type -> ts : time-series
ts_h_tr<-ts(h_train$target_var)
ts_h_ts<-ts(h_test$target_var,start=train_obs+1)

# create space to record RMSE(Root Mean Square Error).
RMSE<-matrix(0,nrow=150, ncol=200)
# create space to record AIC(Akaike information criterion)
aic.mat<-matrix(0,nrow=150, ncol=200)

### ARIMA : grid search to find optimal hyperparameter combination
## non-seasonality ar order: p / ma order : q / I : degree of differencing 
## seasonality ar order: P / ma order : Q / I : degree of differencing 
## drift : Linear regression with ARIMA errors is fitted / FALSE(defualt) or TRUE

for(drift in 0:1)
{
  for(P in 0:4)
  {
    for(Q in 0:4)
    {
      for(I in 0:1)
      {
        
        for(p in 0:4)
        {
          for(q in 0:4)
          {
            for(i in 0:2)
            {              
              
              #p;i;q;P;I;Q;drift
              ifelse(drift==0,
                     ifelse((P+Q+I)==0,
                            
                            # Check the error
                            arim<-tryCatch(Arima(ts_h_tr,order=c(p,i,q),method="CSS-ML"),error=function(err) FALSE ),
                            arim<-tryCatch(Arima(ts_h_tr,order=c(p,i,q),method="CSS-ML", seasonal = list(order=c(P,I,Q),period=7)),error=function(err) FALSE )
                     ),
                     ifelse((P+Q+I)==0,
                            
                            # Check the error
                            arim<-tryCatch(Arima(ts_h_tr,order=c(p,i,q),method="CSS-ML",xreg = 1:length(ts_h_tr)),error=function(err) FALSE ),
                            arim<-tryCatch(arima(ts_h_tr,order=c(p,i,q),xreg = 1:length(ts_h_tr),method="CSS-ML", seasonal = list(order=c(P,I,Q),period=7)),error=function(err) FALSE )
                     )
              )
              
              ifelse(!is.logical(arim),
                     aic.mat[p+i*10+P*30+1,q+Q*10+I*50+100*drift+1]<-arim$aic
                     ,next)
              ifelse(!is.logical(arim),
                     ifelse(!is.logical(tryCatch(forecast(arim,h=273),error=function(err) FALSE )),
                            RMSE[p+P*5+i*25+1,q+Q*10+I*50+100*drift+1]<-sqrt((sum((ts_h_ts-forecast(arim,h=273)$mean)^2))/(273)),
                            RMSE[p+P*5+i*25+1,q+Q*10+I*50+100*drift+1]<-sqrt((sum((ts_h_ts-(predict(arim,newxreg = 1828:2100)$pred))^2))/(273))
                     )
                     ,next)
              
            }
            print(q)
          }
        }
      }
    }
  }
  print(P)   
}



write.table(aic.mat,file="hap_heatmap.csv",row.names=T,col.names=T, sep=",")
write.table(RMSE,file="hap_RMSE_heatmap.csv",row.names=T,col.names=T, sep=",")





### Check and visualize RMSE &  AIC per grid 
plot(c(ts_g_tr,ts_g_ts),type="l")
lines((predict(arim,newxreg = (train_obs+1):(train_obs+length(h_test)))$pred),col="blue",lwd=10)
lines((predict(arim,newxreg = 1:length(h_test))$pred),col="red",lwd=10)
lines((arim$fitted),col="orange",lwd=10)

plot(predict(arim,newxreg = 1:length(h_test)))
predict(arim,newxreg = (train_obs+1):(train_obs+length(h_test)))
p=1;q=1;drift=0


predict(arim,n.ahead=length(h_test))
plot(c(ts_g_tr,ts_g_ts),type="l")
lines((forecast(arim,h=length(h_test))$mean),col="blue",lwd=10)



si$hour<-c(1:(train_obs+length(h_test)))

# "#0072B2" Deep Blue
p<-ggplot(si, aes(hour, V2)) + geom_line(color="skyblue4" ,size=1) +  
  xlab("")+ylab("target variable") +theme_minimal() + ggtitle("MAIN_NAME")
p
train<-ts(si[1:train_obs,2])
test<-ts(si[(train_obs+1):(train_obs+length(h_test)),2],start = (train_obs+1) )

 
 # check the seasonality: example seasonality 40th
 decomp<-stl(ts(si[1:train_obs,2],frequency = 40),s.window = "periodic")
 plot(decomp)
 
 plot(decomp)
 
 deseasonal<-seasadj(decomp)
 
 # stationary test null hypothesis : non-stationary
 adf.test(train, alternative = "stationary")

 par(mfrow=c(1,1))
 
 acf(train); pacf(train)
 
 
 # 1 diff
 acf(diff(train))
 pacf(diff(train))
 
 # 10 diff
 acf(diff(train,lag=10))
 pacf(diff(train,lag=10))
 
 
 # 20 diff
 acf(diff(train,lag=20))
 pacf(diff(train,lag=20))
 
  # 40 diff
 acf(diff(train,lag=40))
 pacf(diff(train,lag=40))
 
 # use auto.arima function in forecast library
 forecast::auto.arima((train))
 forecast::auto.arima(diff(train))
 forecast::auto.arima(diff(train,lag=40))
 
 
 
 # Find subjective Arima models order
 # method CSS : minimises the sum of squared residuals - Fastest and Least accurate
 # method ML : maximises the log-likelihood function - Slowest, but most accurate
 # method CSS-ML : mixed both method  - medium
 Arima(train,order=c(3,0,4),method="CSS-ML")
 Arima(train,order=c(3,1,0),method="CSS-ML")
 Arima(train,order=c(0,0,0),method="CSS-ML", 
       seasonal = list(order=c(1,1,2),period=40))
 Arima(train,order=c(3,0,4),method="CSS-ML", 
       seasonal = list(order=c(1,1,2),period=40)) 
 Arima(train,order=c(3,1,0),method="CSS-ML", 
       seasonal = list(order=c(1,1,2),period=40)) 

 arim<-Arima(train,order=c(3,0,4),method="CSS-ML", 
       seasonal = list(order=c(1,1,2),period=40))
 
 
 # Step 7: Evaluate and Iterate
 tsdisplay(residuals(arim), main='(3,0,4)x(1,1,2)[40] Model Residuals')
 
 # Does the time series data independent지(null hypothesis)
 #Compute the Box–Pierce or Ljung–Box test statistic for examining the null hypothesis 
 #of independence in a given time series. 
 #These are sometimes known as ‘portmanteau’ tests.
 
 #H0: The data are independently distributed 
 #(i.e. the correlations in the population from which the sample is taken are 0, 
 #so that any observed correlations in the data result from randomness of the sampling process).
 #Ha: The data are not independently distributed; they exhibit serial correlation.
 
 # 채택 : 모델의 잔차는 자기 상관이 없다.
 # accept the hyphthesis :  No auto correlation in this model
 Box.test(residuals(arim),type="Ljung-Box")
 


# plot the original timeseries data and Predict value
 colnames(fore)<-c("Point_Forecast","Lo80","Hi80","Lo95","Hi95")
 ggplot() +
   geom_line(data = (si_train),size=1, color = "skyblue4", aes(x = hour, y = V2, colour="The Number of Getting off" ))  +  
   geom_line(data = si_test, size=1, color = "skyblue3", aes(x = hour, y = V2, colour="The Number of Getting off" ))+
   #geom_line(data=fore, aes(x=hour, y = Hi95)) +
   #geom_line(data=fore,aes(x=hour,y = Lo95))+
   geom_ribbon(data=fore,aes(x=hour,ymin=Lo95, ymax=Hi95),fill="darkorange3",alpha=0.5) +
   geom_ribbon(data=fore,aes(x=hour,ymin=Lo80, ymax=Hi80),fill="darkorange4",alpha=0.5) +
   theme_minimal() 