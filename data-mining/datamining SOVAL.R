rm(list=ls())

set.seed(1234)

library(glmnet)
library(e1071)

place.df <- read.csv("C:\\Users\\GaHee\\Documents\\2016 µ¥ÀÌÅÍ¸¶ÀÌ´×\\csv\\adolecent placement study.csv")
colnames(place.df)<-c("id",'place','place3','age','race','gender','neuro','emot','danger','elope','los','behav','custd','viol')

colnames(place.df)
##############################################################

place.df$los<-log(place.df$los)

##############################################################

y.vec <- place.df$place3
y.vec <- as.numeric(y.vec)

temp1 <- (1*(place.df$neuro==1))
temp2 <- (1*(place.df$neuro==2))
temp3 <- (1*(place.df$neuro==3))
temp4 <- (1*(place.df$danger==1))
temp5 <- (1*(place.df$danger==2))
temp6 <- (1*(place.df$danger==3))
emot<-place.df$emot


x.mat<- as.matrix(cbind(place.df[,-c(1,2,3,7,9)],temp1,temp2,temp3,temp4,temp5,temp6)) #dummy
#x.mat<- as.matrix(cbind(place.df[,-c(1,2,3,7,9)],temp1,temp2,temp3,temp4,temp5,temp6,temp1*emot,temp2*emot,temp3*emot)) #dummy+interaction

##############################################################

r.num=4; s.num=500
n.num=dim(x.mat)[1]
e.mat=matrix(0,nrow=2,ncol=s.num)

for(s.pos in 1:s.num){
  
  idx.0<-split(sample((1:n.num)[y.vec==0]),1:r.num)
  idx.1<-split(sample((1:n.num)[y.vec==1]),1:r.num)
  idx.2<-split(sample((1:n.num)[y.vec==2]),1:r.num)
  
  ## fit (training ,learing)
  tr.x.mat<-x.mat[-c(idx.0[[1]],idx.1[[1]],idx.2[[1]]),]
  tr.y.vec<-y.vec[-c(idx.0[[1]],idx.1[[1]],idx.2[[1]])]
  
  ## obtain error |split((1:n.num)[y.vec==0],1:r.num)
  ts.x.mat<-x.mat[c(idx.0[[1]],idx.1[[1]],idx.2[[1]]),]
  ts.y.vec<-y.vec[c(idx.0[[1]],idx.1[[1]],idx.2[[1]])]
  
  ############################################################## Make 3 binary output
  
  df.tr0<-as.data.frame(cbind(tr.y.vec==0,tr.x.mat))
  df.tr1<-as.data.frame(cbind(tr.y.vec==1,tr.x.mat))
  df.tr2<-as.data.frame(cbind(tr.y.vec==2,tr.x.mat))
  head(df.tr0)
  ############################################################### Ordinary logistic Regression
  
  b.vec.0 <- coef(glm(V1~.,data=df.tr0,family="binomial"))
  f.vec.0 <- exp(cbind(1,ts.x.mat)%*%b.vec.0) 
  
  b.vec.1 <- coef(glm(V1~.,data=df.tr1,family="binomial"))
  f.vec.1 <- exp(cbind(1,ts.x.mat)%*%b.vec.1) 
  
  b.vec.2 <- coef(glm(V1~.,data=df.tr2,family="binomial"))
  f.vec.2 <- exp(cbind(1,ts.x.mat)%*%b.vec.2) 
  
  f.mat <- cbind(f.vec.0,f.vec.1,f.vec.2)
  
  p.mat <- f.mat/(1+f.mat)
  y.hat <- apply(p.mat,1,which.max)-1
  
  e.mat[1,s.pos]<- mean(ts.y.vec != y.hat) 
  
  ############################################################### lasso
  cv.glm.fit.0 <- cv.glmnet(x=as.matrix(df.tr0[,-1]),y=df.tr0$V1,family="binomial",type.measure="class") 
  lam.vec.0 <- seq(10*cv.glm.fit.0$lambda.1se,cv.glm.fit.0$lambda.1se,length=100)
  glm.fit.0 <- glmnet(x=as.matrix(df.tr0[,-1]),y=df.tr0$V1,family="binomial",alpha=1,lambda=lam.vec.0)
  b.vec.0 <- c(glm.fit.0$a0[100],glm.fit.0$beta[,100])
  f.vec.0 <- exp(cbind(1,ts.x.mat)%*%b.vec.0) 
  
  cv.glm.fit.1 <- cv.glmnet(x=as.matrix(df.tr1[,-1]),y=df.tr1$V1,family="binomial",type.measure="class") 
  lam.vec.1 <- seq(10*cv.glm.fit.1$lambda.1se,cv.glm.fit.1$lambda.1se,length=100)
  glm.fit.1 <- glmnet(x=as.matrix(df.tr1[,-1]),y=df.tr1$V1,family="binomial",alpha=1,lambda=lam.vec.1)
  b.vec.1 <- c(glm.fit.1$a0[100],glm.fit.1$beta[,100])
  f.vec.1 <- exp(cbind(1,ts.x.mat)%*%b.vec.1) 
  
  cv.glm.fit.2 <- cv.glmnet(x=as.matrix(df.tr2[,-1]),y=df.tr2$V1,family="binomial",type.measure="class") 
  lam.vec.2 <- seq(10*cv.glm.fit.2$lambda.1se,cv.glm.fit.2$lambda.1se,length=100)
  glm.fit.2 <- glmnet(x=as.matrix(df.tr2[,-1]),y=df.tr2$V1,family="binomial",alpha=1,lambda=lam.vec.2)
  b.vec.2 <- c(glm.fit.2$a0[100],glm.fit.2$beta[,100])
  f.vec.2 <- exp(cbind(1,ts.x.mat)%*%b.vec.2) 
  
  f.mat <- cbind(f.vec.0,f.vec.1,f.vec.2)
  p.mat <- f.mat/(1+f.mat)
  
  y.hat <- apply(p.mat,1,which.max)-1
  
  e.mat[2,s.pos]<- mean(ts.y.vec != y.hat) 
  
  
  print(s.pos)
  
}


e.mat
rowMeans(e.mat)


df0<-as.data.frame(cbind(y.vec==0,x.mat))
df1<-as.data.frame(cbind(y.vec==1,x.mat))
df2<-as.data.frame(cbind(y.vec==2,x.mat))

############################################################### conclusion_ordinary

b.vec.0 <- coef(glm(V1~.,data=df0,family="binomial"))
f.vec.0 <- exp(cbind(1,x.mat)%*%b.vec.0) 

b.vec.1 <- coef(glm(V1~.,data=df1,family="binomial"))
f.vec.1 <- exp(cbind(1,x.mat)%*%b.vec.1) 

b.vec.2 <- coef(glm(V1~.,data=df2,family="binomial"))
f.vec.2 <- exp(cbind(1,x.mat)%*%b.vec.2) 

f.mat <- cbind(f.vec.0,f.vec.1,f.vec.2)
p.mat <- f.mat/(1+f.mat)

y.hat <- apply(p.mat,1,which.max)-1

cbind(b.vec.0,b.vec.1,b.vec.2)

table(y.hat, y.vec)
mean(y.vec != y.hat) 

############################################################### conclusion_lasso
cv.glm.fit.0 <- cv.glmnet(x=as.matrix(df0[,-1]), y=df0$V1,family="binomial", type.measure="class") 
lam.vec.0 <- seq(10*cv.glm.fit.0$lambda.1se, cv.glm.fit.0$lambda.1se, length=100)
glm.fit.0 <- glmnet(x=as.matrix(df0[,-1]), y=df0$V1, family="binomial", alpha=1, lambda=lam.vec.0)
b.vec.0 <- c(glm.fit.0$a0[100], glm.fit.0$beta[,100])
f.vec.0 <- exp(cbind(1,x.mat)%*%b.vec.0) 

cv.glm.fit.1 <- cv.glmnet(x=as.matrix(df1[,-1]),y=df1$V1,family="binomial",type.measure="class") 
lam.vec.1 <- seq(10*cv.glm.fit.1$lambda.1se,cv.glm.fit.1$lambda.1se,length=100)
glm.fit.1 <- glmnet(x=as.matrix(df1[,-1]),y=df1$V1,family="binomial",alpha=1,lambda=lam.vec.1)
b.vec.1 <- c(glm.fit.1$a0[100],glm.fit.1$beta[,100])
f.vec.1 <- exp(cbind(1,x.mat)%*%b.vec.1) 

cv.glm.fit.2 <- cv.glmnet(x=as.matrix(df2[,-1]),y=df2$V1,family="binomial",type.measure="class") 
lam.vec.2 <- seq(10*cv.glm.fit.2$lambda.1se,cv.glm.fit.2$lambda.1se,length=100)
glm.fit.2 <- glmnet(x=as.matrix(df2[,-1]),y=df2$V1,family="binomial",alpha=1,lambda=lam.vec.2)
b.vec.2 <- c(glm.fit.2$a0[100],glm.fit.2$beta[,100])
f.vec.2 <- exp(cbind(1,x.mat)%*%b.vec.2) 


f.mat <- cbind(f.vec.0,f.vec.1,f.vec.2)
p.mat <- f.mat/(1+f.mat)

y.hat <- apply(p.mat,1,which.max)-1

cbind(b.vec.0,b.vec.1,b.vec.2)
table(y.hat, y.vec)
mean( y.vec != y.hat) 

####################################################################################
