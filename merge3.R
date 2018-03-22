install.packages("tibble")
install.packages("dplyr")
library(dplyr)

setwd("C:/Users/lee/Downloads") # 경로설정

x<-read.csv("C160026_쿼리_XLSX_eCRF_입력별_화면표시명_코드값_20170804.csv",header=T,sep=",")
names<-colnames(x)
id<-as.vector(unique(x[,4])) # 4 는 "연구대상자ID" 의 위치

length(id) #4257

y<-as.data.frame(matrix(nrow=length(id),ncol=dim(x)[2]))


for(id.num in 1:length(id)){
  for(i in 1:dim(x)[1])
  { 
  if(id[id.num]==x[i,4]) # 4 는 "연구대상자ID" 의 위치
  {
    raw<-as.vector(x[i,])
    
    for(j in 1:11) #dim(x)[2]
    {
      if(!is.na(raw[j]))
      {
        y[id.num,(j)]<-as.vector(raw[(j)][[1]][1])
  
      }
    }
  }
  }
  print(id.num)
}

??aggregate
colnames(y)<-names
write.csv(y,file = "Final.csv",row.names = F)


# "과제번호", "기관코드", "기관명", "연구대상자ID", "연구대상자명", "서면동의일", "성별_1", "생년월일", "나이", "연구대상자상태"


## Compute the averages for the variables in 'state.x77', grouped
## according to the region (Northeast, South, North Central, West) that
## each state belongs to.
aggregate(state.x77, list(Region = state.region), mean)

## Compute the averages according to region and the occurrence of more
## than 130 days of frost.
aggregate(state.x77,
          list(Region = state.region,
               Cold = state.x77[,"Frost"] > 130),
          mean)
## (Note that no state in 'South' is THAT cold.)


## example with character variables and NAs
testDF <- data.frame(v1 = c(1,3,5,7,8,3,5,NA,4,5,7,9),
                     v2 = c(11,33,55,77,88,33,55,NA,44,55,77,99) )
by1 <- c("red", "blue", 1, 2, NA, "big", 1, 2, "red", 1, NA, 12)
by2 <- c("wet", "dry", 99, 95, NA, "damp", 95, 99, "red", 99, NA, NA)
aggregate(x = testDF, by = list(by1, by2), FUN = "mean")

# and if you want to treat NAs as a group
fby1 <- factor(by1, exclude = "")
fby2 <- factor(by2, exclude = "")
aggregate(x = testDF, by = list(fby1, fby2), FUN = "mean")


## Formulas, one ~ one, one ~ many, many ~ one, and many ~ many:
aggregate(weight ~ feed, data = chickwts, mean)
aggregate(breaks ~ wool + tension, data = warpbreaks, mean)
aggregate(cbind(Ozone, Temp) ~ Month, data = airquality, mean)
aggregate(cbind(ncases, ncontrols) ~ alcgp + tobgp, data = esoph, sum)

## Dot notation:
aggregate(. ~ Species, data = iris, mean)
aggregate(len ~ ., data = ToothGrowth, mean)

## Often followed by xtabs():
ag <- aggregate(len ~ ., data = ToothGrowth, mean)
xtabs(len ~ ., data = ag)


## Compute the average annual approval ratings for American presidents.
aggregate(presidents, nfrequency = 1, FUN = mean)
## Give the summer less weight.
aggregate(presidents, nfrequency = 1,
          FUN = weighted.mean, w = c(1, 1, 0.5, 1))




cc <- read.csv('C160026_쿼리_XLSX_eCRF_입력별_화면표시명_코드값_20170804.csv',header=T)
names<-colnames(cc)
colnames(cc) <- paste("V",1:1065,sep="")

cc2 <- aggregate(cc, by=list(name=cc$V4),
                 FUN=function(x) ifelse(is.numeric(x),
                                        mean,paste(names(table(x)[rank(-1*table(x),ties.method="min")==1]),collapse=" ")))


cc <- read.csv('C160026_쿼리_XLSX_eCRF_입력별_화면표시명_코드값_20170804.csv',header=T)
names<-colnames(cc)
colnames(cc) <- paste("V",1:1065,sep="")
cc1 <- cc
cc1 <- as.data.frame(lapply(cc1,as.integer))
cc2 <- aggregate(cc1, by=list(name=cc$V4),FUN=mean,na.rm =FALSE)
str(cc2)

numtolevel <- function(oldx,newx){
  ifelse(!is.na(newx),levels(oldx)[newx],newx)
}
numtolevel(cc[,11],cc2[,11])










