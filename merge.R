install.packages("tibble")
install.packages("dplyr")
library(dplyr)


setwd("C:/Users/lee/Downloads")

x<-read.csv("C160026_쿼리_XLSX_eCRF_입력별_화면표시명_코드값_20170804.csv",header=T,sep=",")

dim(x)[1]/3
head(x)
colnames(x)

x1<-x[,1:468]
x2<-x[,c(1:10,469:916)]
x3<-x[,c(1:10,917:1065)]
write.csv(x3,file = "x2.csv",row.names = F)
write.csv(x2,file = "x3.csv",row.names = F)
write.csv(x1,file = "x1.csv",row.names = F)

z1<-x1[!is.na(x1[,246]),]
z2<-x2[!is.na(x2[,11]),]
z3<-x3[!is.na(x3[,11]),]


# "과제번호", "기관코드", "기관명", "연구대상자ID", "연구대상자명", "서면동의일", "성별_1", "생년월일", "나이", "연구대상자상태"
z12<-z1%>% full_join(z2, by=c("연구대상자ID","과제번호", "기관코드", "기관명", "연구대상자ID", "연구대상자명", "서면동의일", "성별_1", "생년월일", "나이", "연구대상자상태"))

z123<-z12 %>% full_join(z3, by=c("연구대상자ID","과제번호", "기관코드", "기관명", "연구대상자ID", "연구대상자명", "서면동의일", "성별_1", "생년월일", "나이", "연구대상자상태"))
dim(z123)

write.csv(z123,file = "z123.csv",row.names = F)

?aggregate
