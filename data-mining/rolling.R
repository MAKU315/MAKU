install.packages("zoo")
library(zoo)

# 원하는 경로 설정 필요
setwd("C:/Users/lee/Documents/rolling")
raw<-read.csv("raw4.csv",header = T,sep=',')

head(raw,4)

#데이터의 차원 확인/ 나중에 점검할때도 필ㅇ
dim(raw) # 393 93


#width는 설정하면됨 원하는 보고 싶은 만큼 넣어
# 나는 5 10 20 30 해봤음

for(width in c(5,10,20,30))
{
  #raw 523 col 4 개
  # (dim(Dt1)[1]-(width)+1) 는 raw가 100개 이면 width를 10으로 설정 했을때, beta는 총 91개 나옴
  # 만약 변수가 많아지면 ncol 을 늘어난 변수 만큼 4에서 더하면 됨
  int<-matrix(0,nrow=(dim(raw)[1]-(width)+1) ,ncol = 90)
  beta<-matrix(0,nrow=(dim(raw)[1]-(width)+1) ,ncol = 90)
  delta<-matrix(0,nrow=(dim(raw)[1]-(width)+1) ,ncol = 90)
  
  for(var in 1:90)
    {
    
    y<-raw[,(var+1)]
    x.mat<-raw[,92:93]
    select<-cbind(y,x.mat)
  # (dim(Dt1)[1]-(width)+1) 위와 같은 이유
    for(i in 1:(dim(raw)[1]-(width)+1))
    {
    
  
      # width 만큼 뽑아서 data로 추출 한다음 회귀 분석을 진행할 것
      data<-select[i:(i+width-1),]
    
    fit<-lm(data[,1]~data[,2]+data[,3],data=data)
    
    int[i,var]<-fit$coefficients[1]
    beta[i,var]<-fit$coefficients[2]
    delta[i,var]<-fit$coefficients[3]
    # as.data.frame 하는 이유는 Date 변수 행을 넣기 위해서야, matrix form에서는 Date가 들어가 지 못해
  }
  }
  # 해당 파일 저장 
  # 만약 오류가 난다면 두가지 이유 : 1. 경로 설정 잘못 2. beta 메트릭스 오류
  file_name=paste("width_",width,"_int.csv", sep="")
  write.table(int,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")
  
  file_name=paste("width_",width,"_beta.csv", sep="")
  write.table(beta,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")
  
  file_name=paste("width_",width,"delta.csv", sep="")
  write.table(delta,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")
  
  
  # 몇 번째 까지 돌아갔는지 확인 
  
  print(width)
}

