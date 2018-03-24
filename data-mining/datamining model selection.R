###########################3######

rm(list=ls())

set.seed(1234)

library(glmnet)
library(e1071)

place.df.f<- read.csv("C:\\Users\\GaHee\\Documents\\2016 데이터마이닝\\csv\\adolecent placement study.csv")
place.df <- read.csv("C:\\Users\\GaHee\\Documents\\2016 데이터마이닝\\csv\\adolecent placement study.csv")
colnames(place.df)<-c("id",'place','place3','age','race','gender','neuro','emot','danger','elope','los','behav','custd','viol')
colnames(place.df.f)<-c("id",'place','place3','age','race','gender','neuro','emot','danger','elope','los','behav','custd','viol')

################################ log변환
place.df$los<-log(place.df$los)
place.df.f$los<-log(place.df.f$los)

################################ as.factor
place.df.f[,9] <- factor(x=place.df.f[,9],levels = c(0:3))
place.df.f[,7] <- factor(x=place.df.f[,7],levels = c(0:3))
place.df.f[,5] <- factor(x=place.df.f[,5],levels = c(0:1))
place.df.f[,6] <- factor(x=place.df.f[,6],levels = c(0:1))
place.df.f[,8] <-factor(x=place.df.f[,8],levels = c(0:1))
place.df.f[,10] <- factor(x=place.df.f[,10],levels = c(0:1))
place.df.f[,13] <-factor(x=place.df.f[,13],levels = c(0:1))
place.df.f[,14] <- factor(x=place.df.f[,14],levels = c(0:1))
place.df.f[,3]<- factor(x=place.df.f[,3],levels = c(0:2))
#########################################

y.vec <- place.df$place3
y.vec <- as.numeric(y.vec)

temp1 <- (1*(place.df$neuro==1))
temp2 <- (1*(place.df$neuro==2))
temp3 <- (1*(place.df$neuro==3))
temp4 <- (1*(place.df$danger==1))
temp5 <- (1*(place.df$danger==2))
temp6 <- (1*(place.df$danger==3))
emot<-place.df$emot


#x.mat<- as.matrix(cbind(place.df[,-c(1,2,3,7,9)],temp1,temp2,temp3,temp4,temp5,temp6)) #dummy
x.mat<- as.matrix(cbind(place.df[,-c(1,2,3,7,9)],temp1,temp2,temp3,temp4,temp5,temp6,temp1*emot,temp2*emot,temp3*emot)) #dummy+interaction

########################################
y.vec_ <- place.df.f$place3

temp1 <- (1*(place.df.f$neuro=="1"))
temp2 <- (1*(place.df.f$neuro=="2"))
temp3 <- (1*(place.df.f$neuro=="3"))
temp4 <- (1*(place.df.f$danger=="1"))
temp5 <- (1*(place.df.f$danger=="2"))
temp6 <- (1*(place.df.f$danger=="3"))
emot<-place.df.f$emot

#x.mat_ <- cbind(place.df.f[,-c(1,2,3,7,9)],temp1,temp2,temp3,temp4,temp5,temp6)
x.mat_ <- cbind(place.df[,-c(1,2,3,7,9)],temp1,temp2,temp3,temp4,temp5,temp6,temp1*emot,temp2*emot,temp3*emot) #dummy+interaction

########################################

r.num <- 4; s.num <- 100

e.mat <- matrix(0,nrow=5,ncol=s.num)

n.num <- dim(x.mat)[1]



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
  
  
  
  ### ordinary multinomial regression 
  
  lam.vec <- exp(seq(log(10),log(1e-10),length.out=100))
  
  glm.fit <- glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",lambda=lam.vec)
  
  a.vec <- glm.fit$a0[,100]
  
  b.mat <- cbind(glm.fit$beta[[1]][,100],glm.fit$beta[[2]][,100],glm.fit$beta[[3]][,100])
  
  b.mat <- rbind(a.vec,b.mat)
  
  f.mat <- exp(cbind(1,ts.x.mat)%*%b.mat) 
  
  e.mat[1,s.pos] <- mean(ts.y.vec!=(apply(f.mat/rowSums(f.mat),1,which.max)-1))
  
  
  
  ### penalized logistic regression: LASSO 
  
  cv.glm.fit <- cv.glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=1,type.measure="class") 
  
  lam.vec <- seq(10*cv.glm.fit$lambda.1se,cv.glm.fit$lambda.1se,length=100)
  
  glm.fit <- glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=1,lambda=lam.vec)
  
  
  
  
  a.vec <- glm.fit$a0[,100]
  
  b.mat <- cbind(glm.fit$beta[[1]][,100],glm.fit$beta[[2]][,100],glm.fit$beta[[3]][,100])
  
  b.mat <- rbind(a.vec,b.mat)
  
  f.mat <- exp(cbind(1,ts.x.mat)%*%b.mat) 
  
  e.mat[2,s.pos] <- mean(ts.y.vec!=(apply(f.mat/rowSums(f.mat),1,which.max)-1))
  
  
  
  ### penalized logistic regression: Ridge 
  
  cv.glm.fit <- cv.glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=0,type.measure="class") 
  
  lam.vec <- seq(10*cv.glm.fit$lambda.1se,cv.glm.fit$lambda.1se,length=100)
  
  glm.fit <- glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=0,lambda=lam.vec)
  
  
  
  a.vec <- glm.fit$a0[,100]
  
  b.mat <- cbind(glm.fit$beta[[1]][,100],glm.fit$beta[[2]][,100],glm.fit$beta[[3]][,100])
  
  b.mat <- rbind(a.vec,b.mat)
  
  f.mat <- exp(cbind(1,ts.x.mat)%*%b.mat) 
  
  e.mat[3,s.pos] <- mean(ts.y.vec!=(apply(f.mat/rowSums(f.mat),1,which.max)-1))
  
  
  
  
  
  
  
  ### penalized logistic regression: Enet  
  
  alp.vec <- seq(1,0,length.out=10)
  
  lam.vec <- c() 
  
  for(alp.pos in 1:10){
    
    glm.fit <- glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=alp.vec[alp.pos])
    
    lam.vec <- c(lam.vec,glm.fit$lambda)
    
  }
  
  lam.vec <- exp(seq(log(max(lam.vec)),log(min(lam.vec)),length.out=100))
  
  
  cv.e.mat <- matrix(0,nrow=10,ncol=100)
  
  for(alp.pos in 1:10){
    
    cv.glm.fit <- cv.glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=alp.vec[alp.pos],lambda=lam.vec,type.measure="class") 
    
    cv.e.mat[alp.pos,] <- cv.glm.fit$cvm 
    
  }
  
  pos.opt <- which(cv.e.mat==min(cv.e.mat),arr.ind = TRUE)[1,]
  
  alp.opt <- alp.vec[pos.opt[1]]
  
  lam.opt <- lam.vec[pos.opt[2]]
  
  
  
  lam.vec <- seq(10*lam.opt,lam.opt,length=100)
  
  glm.fit <- glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=alp.opt,lambda=lam.vec)
  
  
  a.vec <- glm.fit$a0[,100]
  
  b.mat <- cbind(glm.fit$beta[[1]][,100],glm.fit$beta[[2]][,100],glm.fit$beta[[3]][,100])
  
  b.mat <- rbind(a.vec,b.mat)

  f.mat <- exp(cbind(1,ts.x.mat)%*%b.mat) 
  
  e.mat[4,s.pos] <- mean(ts.y.vec!=(apply(f.mat/rowSums(f.mat),1,which.max)-1))
  
  ################################ NaiveBayes
  idx.00<-split(sample((1:n.num)[y.vec_==0]),1:r.num) 
  idx.11<-split(sample((1:n.num)[y.vec_==1]),1:r.num) 
  idx.22<-split(sample((1:n.num)[y.vec_==2]),1:r.num)
  
  tr.x.mat_<-x.mat_[-c(idx.00[[1]],idx.11[[1]],idx.22[[1]]),]
  tr.y.vec_<-y.vec_[-c(idx.00[[1]],idx.11[[1]],idx.22[[1]])]
  
  ts.x.mat_<-x.mat_[c(idx.00[[1]],idx.11[[1]],idx.22[[1]]),]
  ts.y.vec_<-y.vec_[c(idx.00[[1]],idx.11[[1]],idx.22[[1]])]
  
  nb.fit<-e1071::naiveBayes(tr.y.vec_~.,data = tr.x.mat_, laplace = 0)
  
  y.table<-table(predict(nb.fit,ts.x.mat_), ts.y.vec_)
  
  e.mat[5,s.pos] <-1-(sum(y.table[1,1],y.table[2,2],y.table[3,3])/sum(y.table))
  
  print(s.pos)
  }

e.mat

rowMeans(e.mat[,c(1:30)])



#################################################### Result- 

### penalized logistic regression: Enet  

alp.vec <- seq(1,0,length.out=10)

lam.vec <- c() 
alp.opt <- rep(0,100)
lam.opt <- rep(0,100)

for(num in 1:100){
  
for(alp.pos in 1:10){
  
  glm.fit <- glmnet(x=x.mat,y=y.vec,family="multinomial",alpha=alp.vec[alp.pos])
  lam.vec <- c(lam.vec,glm.fit$lambda)
  
}

lam.vec <- exp(seq(log(max(lam.vec)),log(min(lam.vec)),length.out=100))
cv.e.mat <- matrix(0,nrow=10,ncol=100)


for(alp.pos in 1:10){
  
  cv.glm.fit <- cv.glmnet(x=x.mat,y=y.vec,family="multinomial",alpha=alp.vec[alp.pos],lambda=lam.vec,type.measure="class") 
  cv.e.mat[alp.pos,] <- cv.glm.fit$cvm 
  
}
pos.opt <- which(cv.e.mat==min(cv.e.mat),arr.ind = TRUE)[1,]

alp.opt[num] <- alp.vec[pos.opt[1]]
lam.opt[num] <- lam.vec[pos.opt[2]]

print(num)

}

alp.opt<-mean(alp.opt)
lam.opt<-mean(lam.opt)


lam.vec <- seq(10*lam.opt,lam.opt,length=100)

glm.fit <- glmnet(x=x.mat,y=y.vec,family="multinomial",alpha=alp.opt,lambda=lam.vec)


a.vec <- glm.fit$a0[,100]

b.mat <- cbind(glm.fit$beta[[1]][,100],glm.fit$beta[[2]][,100],glm.fit$beta[[3]][,100])

b.mat <- rbind(a.vec,b.mat)

f.mat <- exp(cbind(1,x.mat)%*%b.mat) 

mean(y.vec!=(apply(f.mat/rowSums(f.mat),1,which.max)-1))





