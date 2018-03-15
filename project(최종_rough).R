library(ggplot2)
library(glmnet)
library(e1071)
library("rpart")
library("rpart.plot")
library("party")
library("caret")
library("C50")
library("CHAID")
library("tree")
library("randomForest")
library(factoextra)
library(class)
library(klaR)
library("MASS")
library("ROCR")
library("rpart")
library("rpart.plot")
library("party")
library("caret")
library("e1071")
library("C50")
library("CHAID")
library("tree")
library(pROC)
require(Epi)
require(ztable)
require(moonBook)
library(kknn)
library(FNN)
library("cluster")
library("knnGarden")
library(mlbench)
library("hydroGOF")
getwd()
setwd("C:/Users/user/Desktop")
gj<-read.csv("gj_modify.csv",header = T)
gj<-na.omit(gj)
dim(gj)
colnames(gj)<-tolower(colnames(gj))
set.seed(1)
idx.00<-split(sample((1:dim(gj)[1])[gj[,34]==1]),1:10) 

idx.01<-split(sample((1:dim(gj)[1])[gj[,34]==2]),1:10) 
idx.02<-split(sample((1:dim(gj)[1])[gj[,34]==3]),1:10)
gj<-gj[c(idx.00[[1]],idx.01[[1]],idx.02[[1]]),]


##### ???? : ????, ??ȣ, log ??
y.vec <- gj$target
y.vec_cat <- gj$target
x.mat<-gj[,-c(1,8,34)]
x.mat_cat<-gj[,-c(1,8,34)]
str(x.mat)



############# dummy
x.mat$smk_stat1<-((x.mat$smk_stat==2)*1)
x.mat$smk_stat2<-((x.mat$smk_stat==3)*2)
x.mat$region1<-(x.mat$region==26)*1
x.mat$region2<-(x.mat$region==27)*1
x.mat$region3<-(x.mat$region==28)*1
x.mat$region4<-(x.mat$region==29)*1
x.mat$region5<-(x.mat$region==30)*1
x.mat$region6<-(x.mat$region==31)*1
x.mat$region7<-(x.mat$region==41)*1
x.mat$region8<-(x.mat$region==42)*1
x.mat$region9<-(x.mat$region==43)*1
x.mat$region10<-(x.mat$region==44)*1
x.mat$region11<-(x.mat$region==45)*1
x.mat$region12<-(x.mat$region==46)*1
x.mat$region13<-(x.mat$region==47)*1
x.mat$region14<-(x.mat$region==48)*1

x.mat<-x.mat[,-c(3,26)]

############# factor -> ???߿? ?礷
x.mat_cat$sex<-as.factor(x.mat_cat$sex)
x.mat_cat$region<-as.factor(x.mat_cat$region)
x.mat_cat$income<-as.factor(x.mat_cat$income)
x.mat_cat$dfab_level<-as.factor(x.mat_cat$dfab_level)
x.mat_cat$dfab<-as.factor(x.mat_cat$dfab)
x.mat_cat$gly<-as.factor(x.mat_cat$gly)
x.mat_cat$olig_occu<-as.factor(x.mat_cat$olig_occu)
x.mat_cat$olig_prote<-as.factor(x.mat_cat$olig_prote)

x.mat_cat$fmly_liver<-as.factor(x.mat_cat$fmly_liver)
x.mat_cat$fmly_hprts<-as.factor(x.mat_cat$fmly_hprts)
x.mat_cat$fmly_apop<-as.factor(x.mat_cat$fmly_apop)
x.mat_cat$fmly_hdise<-as.factor(x.mat_cat$fmly_hdise)
x.mat_cat$fmly_diabml<-as.factor(x.mat_cat$fmly_diabml)
x.mat_cat$fmly_cancer<-as.factor(x.mat_cat$fmly_cancer)

x.mat_cat$smk_stat1<-((x.mat_cat$smk_stat==2)*1)
x.mat_cat$smk_stat2<-((x.mat_cat$smk_stat==3)*1)

x.mat_cat$smk_term<-as.factor(x.mat_cat$smk_term)
x.mat_cat$smk_pres<-as.factor(x.mat_cat$smk_pres)

x.mat_cat$drnk_habit<-as.factor(x.mat_cat$drnk_habit)
x.mat_cat$drk_fq<-as.factor(x.mat_cat$drk_fq)
x.mat_cat$exerci_freq<-as.factor(x.mat_cat$exerci_freq)

x.mat_cat$hbp<-as.factor(x.mat_cat$hbp)
x.mat_cat$liver<-as.factor(x.mat_cat$liver)
x.mat_cat$tuber<-as.factor(x.mat_cat$tuber)
x.mat_cat$hepa<-as.factor(x.mat_cat$hepa)

x.mat_cat$region1<-(x.mat_cat$region==26)*1
x.mat_cat$region2<-(x.mat_cat$region==27)*1
x.mat_cat$region3<-(x.mat_cat$region==28)*1
x.mat_cat$region4<-(x.mat_cat$region==29)*1
x.mat_cat$region5<-(x.mat_cat$region==30)*1
x.mat_cat$region6<-(x.mat_cat$region==31)*1
x.mat_cat$region7<-(x.mat_cat$region==41)*1
x.mat_cat$region8<-(x.mat_cat$region==42)*1
x.mat_cat$region9<-(x.mat_cat$region==43)*1
x.mat_cat$region10<-(x.mat_cat$region==44)*1
x.mat_cat$region11<-(x.mat_cat$region==45)*1
x.mat_cat$region12<-(x.mat_cat$region==46)*1
x.mat_cat$region13<-(x.mat_cat$region==47)*1
x.mat_cat$region14<-(x.mat_cat$region==48)*1


######### ???? ��ġ Ȯ??
which(colnames(x.mat_cat)=='drk_fq')
which(colnames(x.mat_cat)=='hepa')
which(colnames(x.mat_cat)=='region')
which(colnames(x.mat_cat)=='smk_stat')


#####log ??ȯ 
x.mat$bmi<-log(x.mat$bmi)
x.mat$bp_high<-log(x.mat$bp_high)
x.mat$bp_lwst<-log(x.mat$bp_lwst)
x.mat$blds<-log(x.mat$blds)
x.mat$tot_chole<-log(x.mat$tot_chole)
x.mat$sgot_ast<-log(x.mat$sgot_ast)
x.mat$sgpt_alt<-log(x.mat$sgpt_alt)


x.mat_cat$bmi<-log(x.mat_cat$bmi)
x.mat_cat$bp_high<-log(x.mat_cat$bp_high)
x.mat_cat$bp_lwst<-log(x.mat_cat$bp_lwst)
x.mat_cat$blds<-log(x.mat_cat$blds)
x.mat_cat$tot_chole<-log(x.mat_cat$tot_chole)
x.mat_cat$sgot_ast<-log(x.mat_cat$sgot_ast)
x.mat_cat$sgpt_alt<-log(x.mat_cat$sgpt_alt)

str(x.mat_cat)

# fmly_liver3571 fmly_hprts10921 fmly_apop6850 fmly_hdise3067 fmly_diabml7009 fmly_cance16538 ??

#hbp 8651 liver 1447 tuber 3138

x.mat_cat<-x.mat_cat[,-c(3,26)]
str(x.mat_cat)
summary(x.mat_cat)

y.vec_cat<-as.factor(y.vec_cat)

####

## scale
x.mat$bmi<-scale(x.mat$bmi,center=T,scale=T )
x.mat$bp_high<-scale(x.mat$bp_high,center=T,scale=T )
x.mat$bp_lwst<-scale(x.mat$bp_lwst,center=T,scale=T )
x.mat$blds<-scale(x.mat$blds,center=T,scale=T )
x.mat$tot_chole<-scale(x.mat$tot_chole,center=T,scale=T )
x.mat$hmg<-scale(x.mat$hmg,center=T,scale=T )
x.mat$olig_ph<-scale(x.mat$olig_ph,center=T,scale=T )
x.mat$sgot_ast<-scale(x.mat$sgot_ast,center=T,scale=T )
x.mat$sgpt_alt<-scale(x.mat$sgpt_alt,center=T,scale=T )
x.mat$gamma_gtp<-scale(x.mat$gamma_gtp,center=T,scale=T )


x.mat_cat$bmi<-scale(x.mat_cat$bmi,center=T,scale=T )
x.mat_cat$bp_high<-scale(x.mat_cat$bp_high,center=T,scale=T )
x.mat_cat$bp_lwst<-scale(x.mat_cat$bp_lwst,center=T,scale=T )
x.mat_cat$blds<-scale(x.mat_cat$blds,center=T,scale=T )
x.mat_cat$tot_chole<-scale(x.mat_cat$tot_chole,center=T,scale=T )
x.mat_cat$hmg<-scale(x.mat_cat$hmg,center=T,scale=T )
x.mat_cat$olig_ph<-scale(x.mat_cat$olig_ph,center=T,scale=T )
x.mat_cat$sgot_ast<-scale(x.mat_cat$sgot_ast,center=T,scale=T )
x.mat_cat$sgpt_alt<-scale(x.mat_cat$sgpt_alt,center=T,scale=T )
x.mat_cat$gamma_gtp<-scale(x.mat_cat$gamma_gtp,center=T,scale=T )


memory.limit(memory.size(max=T)*100)
x.mat<-as.matrix(x.mat)

# 1:3 test:train # simulation 100ȸ
r.num <- 4; s.num <- 100
e.mat <- matrix(0,nrow=10,ncol=s.num)
n.num <- dim(x.mat)[1]

time.mat<- matrix(0,nrow=10,ncol=s.num)
sp.mat<-matrix(0,nrow=10,ncol=s.num)

## simulation
for(s.pos in 1:s.num){
  
  set.seed(s.pos*12)
  
  ## class ?? ?յ? ä??
  idx.0<-split(sample((1:n.num)[y.vec==1]),1:r.num) 
  
  idx.1<-split(sample((1:n.num)[y.vec==2]),1:r.num) 
  
  idx.2<-split(sample((1:n.num)[y.vec==3]),1:r.num)
  
 
  ## fit (training ,learing)
  
  tr.x.mat<-x.mat[-c(idx.0[[1]],idx.1[[1]],idx.2[[1]]),]
  
  tr.y.vec<-y.vec[-c(idx.0[[1]],idx.1[[1]],idx.2[[1]])]
  
  
  ## test set | obtain error |split((1:n.num)[y.vec==0],1:r.num) 
  
  
  ts.x.mat<-x.mat[c(idx.0[[1]],idx.1[[1]],idx.2[[1]]),]
  
  ts.y.vec<-y.vec[c(idx.0[[1]],idx.1[[1]],idx.2[[1]])]
  
  
  tr.x.mat_cat<-x.mat_cat[-c(idx.0[[1]],idx.1[[1]],idx.2[[1]]),]
  tr.y.vec_cat<-y.vec_cat[-c(idx.0[[1]],idx.1[[1]],idx.2[[1]])]
  
  ts.x.mat_cat<-x.mat_cat[c(idx.0[[1]],idx.1[[1]],idx.2[[1]]),]
  ts.y.vec_cat<-y.vec_cat[c(idx.0[[1]],idx.1[[1]],idx.2[[1]])]
  
  
  train<-cbind(tr.y.vec_cat,tr.x.mat_cat)
  test<-cbind(ts.y.vec_cat,ts.x.mat_cat)
  colnames(train)[1]<-"target"
  colnames(test)[1]<-"target"
  
  ### ordinary multinomial regression 
  
  
  glm.time <- proc.time()
  glm.fit <- glmnet(x= tr.x.mat,y=tr.y.vec,family="multinomial",lambda=0)
  
  a.vec <- as.vector(glm.fit$a0)
  
  b.mat <- cbind(glm.fit$beta[[1]][,1],glm.fit$beta[[2]][,1],glm.fit$beta[[3]][,1])
  
  b.mat <- rbind(a.vec,b.mat)
  
  f.mat <- exp(cbind(1,ts.x.mat)%*%b.mat) 
  
  
  # accuracy
  e.mat[1,s.pos] <- mean(ts.y.vec==(apply(f.mat/rowSums(f.mat),1,which.max)))
  t<-table(ts.y.vec,apply(f.mat/rowSums(f.mat),1,which.max))
  if( dim(t)[2] == 2){
  sp.mat[1,s.pos] <-0/sum(t[3,])}else{  sp.mat[1,s.pos] <-sum(t[3,3])/sum(t[3,])}
  
  
  # table(ts.y.vec,apply(f.mat/rowSums(f.mat),1,which.max)) ?????? ??�� ?߻?(3 ?? ????)
  print(  time.mat[1,s.pos] )
  ### penalized logistic regression: LASSO 
  
  cv.glm.fit <- cv.glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=1,type.measure="class") 
  
  
  lasso.time <- proc.time()
  
  glm.fit <- glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=1,lambda=cv.glm.fit$lambda.1se)
  
  a.vec <- as.vector(glm.fit$a0)
  
  b.mat <- cbind(glm.fit$beta[[1]][,1],glm.fit$beta[[2]][,1],glm.fit$beta[[3]][,1])
  
  b.mat <- rbind(a.vec,b.mat)
  
  f.mat <- exp(cbind(1,ts.x.mat)%*%b.mat) 
  
  # accuracy
  time.mat[2,s.pos] <-(proc.time()-lasso.time)[2]
  e.mat[2,s.pos] <- mean(ts.y.vec==(apply(f.mat/rowSums(f.mat),1,which.max)))
  
  
  t<-table(ts.y.vec,apply(f.mat/rowSums(f.mat),1,which.max))
  if( dim(t)[2] == 2){
    sp.mat[2,s.pos] <-0/sum(t[3,])}else{  sp.mat[2,s.pos] <-sum(t[3,3])/sum(t[3,])}
  
  print(  time.mat[2,s.pos] ) 
  
  
  ### penalized logistic regression: Ridge 

  cv.glm.fit <- cv.glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=0,type.measure="class") 

  ridge.time<-proc.time()  
  
  glm.fit <- glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=0,lambda=cv.glm.fit$lambda.1se)
  
  a.vec <- as.vector(glm.fit$a0)
  
  b.mat <- cbind(glm.fit$beta[[1]][,1],glm.fit$beta[[2]][,1],glm.fit$beta[[3]][,1])
  
  b.mat <- rbind(a.vec,b.mat)
  
  f.mat <- exp(cbind(1,ts.x.mat)%*%b.mat) 
  
  
  time.mat[3,s.pos] <-(proc.time()-ridge.time)[2]
  e.mat[3,s.pos] <- mean(ts.y.vec==(apply(f.mat/rowSums(f.mat),1,which.max)))
  
  
  t<-table(ts.y.vec,apply(f.mat/rowSums(f.mat),1,which.max))
  if( dim(t)[2] == 2){
    sp.mat[3,s.pos] <-0/sum(t[3,])}else{  sp.mat[3,s.pos] <-sum(t[3,3])/sum(t[3,])}
  
  print(  time.mat[3,s.pos] )
  
  
  ### penalized logistic regression: Enet  
  
  alp.vec <- seq(0.99,0.01,length.out=10)
  
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
  
  enet.time<-proc.time()
  
  alp.opt <- alp.vec[pos.opt[1]]
  
  lam.opt <- lam.vec[pos.opt[2]]
  
  glm.fit <- glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=alp.opt,lambda=lam.opt)
  
  a.vec <- as.vector(glm.fit$a0)
  
  b.mat <- cbind(glm.fit$beta[[1]][,1],glm.fit$beta[[2]][,1],glm.fit$beta[[3]][,1])
  
  b.mat <- rbind(a.vec,b.mat)
  f.mat <- exp(cbind(1,ts.x.mat)%*%b.mat) 
  t<-table(ts.y.vec,apply(f.mat/rowSums(f.mat),1,which.max))
  if( dim(t)[2] == 2){
    sp.mat[4,s.pos] <-0/sum(t[3,])}else{  sp.mat[4,s.pos] <-sum(t[3,3])/sum(t[3,])}
  
  e.mat[4,s.pos] <- mean(ts.y.vec==(apply(f.mat/rowSums(f.mat),1,which.max)))
  time.mat[4,s.pos] <-(proc.time()-enet.time)[2]
  print(  time.mat[4,s.pos] )
  ################################ NaiveBayes ## factor ȭ ?ʿ?
  
  nv.time<-proc.time()
  
  nb.fit<-e1071::naiveBayes(tr.x.mat_cat,tr.y.vec_cat)
  
  y.table<-table( ts.y.vec,predict(nb.fit,ts.x.mat,type = "class"))
  
  e.mat[5,s.pos] <-(sum(y.table[1,1],y.table[2,2],y.table[3,3])/sum(y.table))
  t<-y.table
  if( dim(t)[2] == 2){
    sp.mat[5,s.pos] <-0/sum(t[3,])}else{  sp.mat[5,s.pos] <-sum(t[3,3])/sum(t[3,])}
  
  time.mat[5,s.pos] <-(proc.time()-nv.time)[2]
  print(  time.mat[5,s.pos] )
  
  
  ################################ dicision tree rpart(cart ?˰���??)
  rpart.time<-proc.time()
  
  rpart.model <- rpart::rpart(target~.,data=train)
  rpart.model$cptable[which.min(rpart.model$cptable[,"rel error"]),1]
  opt.cp <- rpart.model$cptable[which.min(rpart.model$cptable[,"xerror"]),1]
  fit3 <- rpart(target ~ ., data = train,control = rpart.control(cp = opt.cp))
  
  p.test<-predict(fit3,newdata = test[,-which(colnames(test)=="target")],type="class")
  t<-caret::confusionMatrix(test[,which(colnames(test)=="target")],p.test)$table
  if( dim(t)[2] == 2){
    sp.mat[6,s.pos] <-0/sum(t[3,])}else{  sp.mat[6,s.pos] <-sum(t[3,3])/sum(t[3,])}
  
  e.mat[6,s.pos] <-caret::confusionMatrix(test[,which(colnames(test)=="target")],p.test)$overall[[1]]
  
  time.mat[6,s.pos] <-(proc.time()-rpart.time)[2]
  print(  time.mat[6,s.pos] )
  ################################ dicision tree C.50(cart ?˰���??)
  
  cfvec <- as.data.frame(seq(from = 0,to = 1,length.out = 20))
  ind <- split(1:nrow(train),1:10)
  error <- matrix(0,nrow =10 , ncol=20)
  for (j in 1:20){
    for(i in 1:10){
      test1<-train[ind[[i]],]
      train1<-train[-ind[[i]],]
      target <- test1$target[ind[[i]]]
      model <- C50::C5.0(target~., data=train1,CF=cfvec[j])
      pred <-predict(model, newdata=test1, type="class")
      table <- table(target, pred)
      error[i,j] <- 1-sum(diag(table))/sum(table)
    }
  }
  
  opt.cf <- cfvec[which.min(colMeans(error)),]
  
  # ??????�� ?ٿ??? ????
  c50.time<-proc.time()
  
  cv.c50.model <- C50::C5.0(target~.,data=train,CF =opt.cf,winnow=T)
  c50.pred <- predict(cv.c50.model,newdata=ts.x.mat_cat,type="class")
  y.table<-table(ts.y.vec_cat,c50.pred)
  t<-y.table
  if( dim(t)[2] == 2){
    sp.mat[7,s.pos] <-0/sum(t[3,])}else{  sp.mat[7,s.pos] <-sum(t[3,3])/sum(t[3,])}
  
  
  e.mat[7,s.pos] <-(sum(y.table[1,1],y.table[2,2],y.table[3,3])/sum(y.table))
  time.mat[7,s.pos] <-(proc.time()-c50.time)[2]
  print(  time.mat[7,s.pos] )
  
  
  
  ################################ random Forest
  ntree<-c(200,300,500)# default 500 / sqrt(49) = 7
  mtry<-c(6,7,8)
  err<-matrix(0,nrow =3,ncol = 3)
  re<-matrix(0,nrow=2,ncol=4)
  ??randomForest::randomForest
  ## class ?? ?յ? ä??
  id11<-split(sample((1:dim(train)[1])[train$target==1]),1:4) 
  
  id12<-split(sample((1:dim(train)[1])[train$target==2]),1:4) 
  
  id13<-split(sample((1:dim(train)[1])[train$target==3]),1:4)
  
  # 4-fold cross 
  for(z in 1:4)
  {
    
    rf.train1<-train[-c(id11[[z]],id12[[z]],id13[[z]]),]
    rf.test1<-train[c(id11[[z]],id12[[z]],id13[[z]]),]
    #rf.train1<-as.matrix(rf.train1);rf.test1<-as.matrix(rf.test1)
    
    for (i in 1:length(ntree) ){
      for( j in 1:length(mtry)){
        randF<-randomForest::randomForest(target~.,data=rf.train1,ntree=ntree[i],mtry=mtry[j])
        pred<-predict(randF,newdata= rf.test1[,-which(colnames(rf.test1)=="target")])
        err[i,j]<-caret::confusionMatrix(rf.test1[,which(colnames(rf.test1)=="target")],pred)$overall[[1]]
        
        re[1,z]<-which(err==max(err),arr.ind = TRUE)[1];re[2,z]<-which(err==max(err),arr.ind = TRUE)[2]
      }
    }
    
  }
  
  nt<-ntree[which.max(table(re[1,]))[[1]]];mt<-mtry[which.max(table(re[2,]))[[1]]]
  
  
  rf.time<-proc.time()
  randF<-randomForest::randomForest(target~.,data=train,importance=T,ntree=nt,mtry=mt)
  
  p.test1<-predict(randF,newdata= test[,-which(colnames(test)=="target")])
  e.mat[8,s.pos]<-caret::confusionMatrix(test[,which(colnames(test)=="target")],p.test1)$overall[[1]]
  
  t<-caret::confusionMatrix(test[,which(colnames(test)=="target")],p.test1)$table
  if( dim(t)[2] == 2){
    sp.mat[8,s.pos] <-0/sum(t[3,])}else{  sp.mat[8,s.pos] <-sum(t[3,3])/sum(t[3,])}
  
  time.mat[8,s.pos] <-(proc.time()-rf.time)[2]
  print(  time.mat[8,s.pos] )
  
  
  ################################ knn
  
  cls<-13
  clust<-matrix(0,nrow =4,ncol=cls )
  id11<-split(sample((1:dim(train)[1])[train$target==1]),1:4) 
  
  id12<-split(sample((1:dim(train)[1])[train$target==2]),1:4) 
  
  id13<-split(sample((1:dim(train)[1])[train$target==3]),1:4)
  for(i in 1:4)
  {
    
    knn.train1<-train[-c(id11[[i]],id12[[i]],id13[[i]]),]
    knn.test1<-train[c(id11[[i]],id12[[i]],id13[[i]]),]
    
    for(j in 2:cls){
      
      knn.train_x.mat<-knn.train1[,-1]
      knn.train_y.vec<-knn.train1[,1]
      
      knn.test_x.mat<-knn.test1[,-1]
      knn.test_y.vec<-knn.test1[,1]
      
      cl<-knn.train_y.vec
      knn<-class::knn(knn.train_x.mat,knn.test_x.mat, cl=cl,k=j,prob = T)
      clust[i,j]<-mean(knn==knn.test_y.vec)
      
    }
  }
  
  best.clust<-which.max(colMeans(clust))
  
  knn.time<-proc.time()
  
  final.knn<-class::knn(train[,-1],test[,-1], cl=train[,1],k=best.clust)
  e.mat[9,s.pos]<-caret::confusionMatrix(test[,which(colnames(test)=="target")],final.knn)$overall[[1]]
  t<-caret::confusionMatrix(test[,which(colnames(test)=="target")],final.knn)$table
  if( dim(t)[2] == 2){
    sp.mat[9,s.pos] <-0/sum(t[3,])}else{  sp.mat[9,s.pos] <-sum(t[3,3])/sum(t[3,])}
  
  time.mat[9,s.pos] <-(proc.time()-knn.time)[2]
  print(  time.mat[9,s.pos] )
  
 
  
  print(s.pos)
  
}

tr.e.mat<-t(e.mat)
names<-c("logit","LASSO","ridge","E.net","NB","Tree","tree(C50)")

boxplot(tr.e.mat[,1],tr.e.mat[,2],tr.e.mat[,3],tr.e.mat[,4],tr.e.mat[,5],tr.e.mat[,6],tr.e.mat[,7],tr.e.mat[,8],tr.e.mat[,9],tr.e.mat[,10],names=c("logit","LASSO","ridge","E.net","NB","Tree","Tree(C50)","Random Forest","KNN","SVM"))

length(names)
?boxplot

tr.time.mat<-t(time.mat)

file_name=paste("C:/Users/user/Desktop/????/????/?��?��Ʈ/emat_01.csv", sep="")
write.table(e.mat,file=file_name,sep=",")

file_name=paste("C:/Users/user/Desktop/????/????/?��?��Ʈ/time_01.csv", sep="")
write.table(time.mat,file=file_name,sep=",")


file_name=paste("C:/Users/user/Desktop/????/????/?��?��Ʈ/semat_01.csv", sep="")
write.table(sp.mat,file=file_name,sep=",")




################################ svm

#result<-tune(svm, target~., data = train,unecontrol = tune.control(sampling = "cross")) # ranges=list(gamma = 2^(-1:1), cost = 2^(0:2))

#result$best.model

#gamma<-result$best.model$gamma;cost<-result$best.model$cost

svm.time<-proc.time()
# gamma 1/2 1 2  cost  1 2 4
svm.model<-e1071::svm(target~.,data = train,kernel="radial",cost=4,gamma= 1)

pred.svm<-predict(svm.model,test[,-1])
e.mat[10,s.pos]<-caret::confusionMatrix(test[,which(colnames(test)=="target")],pred.svm)$overall[[1]]

t<-caret::confusionMatrix(test[,which(colnames(test)=="target")],pred.svm)$table
if( dim(t)[2] == 2){
  sp.mat[10,s.pos] <-0/sum(t[3,])}else{  sp.mat[10,s.pos] <-sum(t[3,3])/sum(t[3,])}


time.mat[10,s.pos] <-(proc.time()-svm.time)[2]
print(  time.mat[10,s.pos] )







################ compare model 
emat_tot<-read.csv("emat_tot.csv",header = T)
tr.e.mat<-t(emat_tot)
boxplot(tr.e.mat[,1],tr.e.mat[,2],tr.e.mat[,3],tr.e.mat[,4],tr.e.mat[,6],tr.e.mat[,7],tr.e.mat[,8],tr.e.mat[,9],tr.e.mat[,10],names=c("logit","LASSO","ridge","E.net","Tree","Tree(C50)","Random Forest","KNN","SVM"),main="ACCURACY")
boxplot(tr.e.mat[,5],xlab=c("NB"),main="ACCURACY",ylim=c(0, 0.8))

emat_tot<-read.csv("semat_tot.csv",header = T)
tr.e.mat<-t(emat_tot)
boxplot(tr.e.mat[,1],tr.e.mat[,2],tr.e.mat[,3],tr.e.mat[,4],tr.e.mat[,6],tr.e.mat[,7],tr.e.mat[,8],tr.e.mat[,9],tr.e.mat[,10],names=c("logit","LASSO","ridge","E.net","Tree","Tree(C50)","Random Forest","KNN","SVM"),main="True Class3")
boxplot(tr.e.mat[,5],xlab=c("NB"),main="True Class3",ylim=c(0, 0.8))

emat_tot<-read.csv("time_tot.csv",header = T)
tr.e.mat<-t(emat_tot)
boxplot(tr.e.mat[,1],tr.e.mat[,2],tr.e.mat[,3],tr.e.mat[,4],tr.e.mat[,6],tr.e.mat[,7],tr.e.mat[,8],tr.e.mat[,9],tr.e.mat[,10],names=c("logit","LASSO","ridge","E.net","Tree","Tree(C50)","Random Forest","KNN","SVM"),main="Time")
