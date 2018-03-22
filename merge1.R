install.packages("tibble")
install.packages("dplyr")
library(dplyr)

setwd("C:/Users/lee/Downloads")

x<-read.csv("C160026_쿼리_XLSX_eCRF_입력별_화면표시명_코드값_20170804.csv",header=T,sep=",")

id<-as.vector(unique(x[,4]))

length(id) #4257

y<-as.data.frame(matrix(0,nrow=4257,ncol=1064))

for(id.num in 1:length(id)){
  for(i in 1:4295)
  { 
  if(id[id.num]==x[i,4])
  {
    raw<-as.vector(x[i,])
    
    for(j in 1:1053)
    {
      if(!is.na(raw[j+11]))
      {
        y[id.num,(j+11)]<-raw[(j+11)]
      }
    }
  }
  }
}


dim(y)
# "과제번호", "기관코드", "기관명", "연구대상자ID", "연구대상자명", "서면동의일", "성별_1", "생년월일", "나이", "연구대상자상태"
z12<-z1%>% full_join(z2, by=c("연구대상자ID","과제번호", "기관코드", "기관명", "연구대상자ID", "연구대상자명", "서면동의일", "성별_1", "생년월일", "나이", "연구대상자상태"))

z123<-z12 %>% full_join(z3, by=c("연구대상자ID","과제번호", "기관코드", "기관명", "연구대상자ID", "연구대상자명", "서면동의일", "성별_1", "생년월일", "나이", "연구대상자상태"))

write.csv(z123,file = "z123.csv",row.names = F)




