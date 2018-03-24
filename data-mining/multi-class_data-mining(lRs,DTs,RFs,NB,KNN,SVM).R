# plot library 
library(ggplot2)

# penalty regression library
library(glmnet)

# Naive Bayesian classifer & SVM library
library(e1071)

# decision tree library
library("rpart") 
library("rpart.plot")
library("party")
library("C50")
library("CHAID")
library("tree")
library("rpart")
library("rpart.plot")

# decision tree library
library("randomForest")

# classiﬁcation and regression training http://topepo.github.io/caret/index.html
library("caret")

# PCA
library("factoextra")

library("klaR")
library("MASS")

# make ROCurve
library("ROCR")
library("pROC")

require(Epi)

# add color in R
require(ztable)
require(moonBook)

# knn
library("kknn")
library("FNN")
library("class")

# hierarchical, CLARA..  clustering.. 
library("cluster")

# Data.set .. 
library("mlbench")

# Goodness-of-Fit Functions
library("hydroGOF")


# set the work directory
setwd("C:/Users/user/Desktop")

# data set 

# skip pro-processing dataset

# increase the memory limit
memory.limit(memory.size(max=T)*100)


x.mat<-as.matrix(x.mat)

# 1:3 test:train
# random sampling simulation 100 
r.num <- 4; s.num <- 100

# create space to record accuracy.
e.mat <- matrix(0,nrow=10,ncol=s.num)
n.num <- dim(x.mat)[1]

# create space to record calculation time.
time.mat<- matrix(0,nrow=10,ncol=s.num)
sp.mat<-matrix(0,nrow=10,ncol=s.num)

## simulation
for(s.pos in 1:s.num){
  
  set.seed(s.pos*12)
  
  ## divide the class equally
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
  
  
  # calculate the accuracy
  e.mat[1,s.pos] <- mean(ts.y.vec==(apply(f.mat/rowSums(f.mat),1,which.max)))
  t<-table(ts.y.vec,apply(f.mat/rowSums(f.mat),1,which.max))
  if(dim(t)[2] == 2){
  sp.mat[1,s.pos] <-0/sum(t[3,])}else{  sp.mat[1,s.pos] <-sum(t[3,3])/sum(t[3,])}
  
  
  # table(ts.y.vec,apply(f.mat/rowSums(f.mat),1,which.max))
  print(time.mat[1,s.pos])
  
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
  
  # Find the appropriate hyper-parameter(Enet) Alpha - Lasso/Ridge ratio : 10-fold cross-validation 
  for(alp.pos in 1:10){
    
    cv.glm.fit <- cv.glmnet(x=tr.x.mat,y=tr.y.vec,family="multinomial",alpha=alp.vec[alp.pos],lambda=lam.vec,type.measure="class") 
    
    cv.e.mat[alp.pos,] <- cv.glm.fit$cvm 
    
  }
  
  pos.opt <- which(cv.e.mat==min(cv.e.mat),arr.ind = TRUE)[1,]
  
  enet.time<-proc.time()
  
  alp.opt <- alp.vec[pos.opt[1]]
  
  # Find the appropriate hyper-parameter(Enet) Lambda - Degree of penalty parameter at the same Alpha
  
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
  
  
  
  ################################ NaiveBayes 
  
  nv.time<-proc.time()
  
  nb.fit<-e1071::naiveBayes(tr.x.mat_cat,tr.y.vec_cat)
  
  y.table<-table( ts.y.vec,predict(nb.fit,ts.x.mat,type = "class"))
  
  e.mat[5,s.pos] <-(sum(y.table[1,1],y.table[2,2],y.table[3,3])/sum(y.table))
  t<-y.table
  if( dim(t)[2] == 2){
    sp.mat[5,s.pos] <-0/sum(t[3,])}else{  sp.mat[5,s.pos] <-sum(t[3,3])/sum(t[3,])}
  
  time.mat[5,s.pos] <-(proc.time()-nv.time)[2]
  print(  time.mat[5,s.pos] )
  
  
  ################################ dicision tree rpart(cart package)
  
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
  
  ################################ dicision tree C.50 (have boosting)
  
  # find the confidence factor(Complexity) - Hyper parameter 
  ### use cross-validation to find optimal Hyper parameter  CF
  
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
  #### mtry Number of variables randomly sampled 
  ## default  target variable is category : (sqrt(ncol(x))
  ## default  target variable is numeric : (ncol(x)/3)
  
  # example : grid for two hyper-parameter(Number of trees to grow, Number of variables randomly sampled)
  ntree<-c(200,300,500)
  mtry<-c(6,7,8)
  err<-matrix(0,nrow =3,ncol = 3)
  re<-matrix(0,nrow=2,ncol=4)
  
  ## class3 test: train = 1: 4
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
  #### mtry Number of variables randomly sampled 
  ## default  target variable is category : (sqrt(ncol(x))
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
      #FNN::knn cl : factor of true classifications of training set
      # k : hyper-paramter the number of neighbours considered
      knn<-class::knn(knn.train_x.mat,knn.test_x.mat, cl=cl,k=j,prob = T)
      clust[i,j]<-mean(knn==knn.test_y.vec)
      
    }
  }
  
  # the best number of neighbours 
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

# Draw the boxplot related to accuracy
boxplot(tr.e.mat[,1],tr.e.mat[,2],tr.e.mat[,3],tr.e.mat[,4],tr.e.mat[,5],tr.e.mat[,6],tr.e.mat[,7],tr.e.mat[,8],tr.e.mat[,9],tr.e.mat[,10],names=c("logit","LASSO","ridge","E.net","NB","Tree","Tree(C50)","Random Forest","KNN","SVM"))

tr.time.mat<-t(time.mat)

file_name=paste("C:/Users/user/Desktop/at_01.csv", sep="")
write.table(e.mat,file=file_name,sep=",")

file_name=paste("C:/Users/user/Desktop/time_01.csv", sep="")
write.table(time.mat,file=file_name,sep=",")


file_name=paste("C:/Users/user/Desktop/semat_01.csv", sep="")
write.table(sp.mat,file=file_name,sep=",")


################################ SVM

#result<-tune(svm, target~., data = train,unecontrol = tune.control(sampling = "cross")) # ranges=list(gamma = 2^(-1:1), cost = 2^(0:2))

#result$best.model

#gamma<-result$best.model$gamma;cost<-result$best.model$cost

svm.time<-proc.time()
# gamma 1/2 1 2  cost  1 2 4
# hyper paramter : gaussian, cost, gamma
svm.model<-e1071::svm(target~.,data = train,kernel="radial",cost=4,gamma= 1)

pred.svm<-predict(svm.model,test[,-1])
e.mat[10,s.pos]<-caret::confusionMatrix(test[,which(colnames(test)=="target")],pred.svm)$overall[[1]]

t<-caret::confusionMatrix(test[,which(colnames(test)=="target")],pred.svm)$table
if( dim(t)[2] == 2){
  sp.mat[10,s.pos] <-0/sum(t[3,])}else{  sp.mat[10,s.pos] <-sum(t[3,3])/sum(t[3,])}


time.mat[10,s.pos] <-(proc.time()-svm.time)[2]
print(  time.mat[10,s.pos] )




################ compare model test accuracy
emat_tot<-read.csv("emat_tot.csv",header = T)
ts.e.mat<-t(emat_tot)
boxplot(ts.e.mat[,1],ts.e.mat[,2],ts.e.mat[,3],ts.e.mat[,4],ts.e.mat[,6],ts.e.mat[,7],ts.e.mat[,8],ts.e.mat[,9],ts.e.mat[,10],names=c("logit","LASSO","ridge","E.net","Tree","Tree(C50)","Random Forest","KNN","SVM"),main="ACCURACY")
boxplot(ts.e.mat[,5],xlab=c("NB"),main="ACCURACY",ylim=c(0, 0.8))

################ compare model test sensitivity
emat_tot<-read.csv("semat_tot.csv",header = T)
sensitivity<-t(emat_tot)
boxplot(sensitivity[,1],sensitivity[,2],sensitivity[,3],sensitivity[,4],sensitivity[,6],
        sensitivity[,7],sensitivity[,8],sensitivity[,9],sensitivity[,10],names=c("logit","LASSO","ridge","E.net","Tree","Tree(C50)","Random Forest","KNN","SVM"),main="True Class3")
boxplot(sensitivity[,5],xlab=c("NB"),main="True Class3",ylim=c(0, 0.8))

################ compare model computation time
emat_tot<-read.csv("time_tot.csv",header = T)
com_time<-t(emat_tot)
boxplot(com_time[,1],com_time[,2],com_time[,3],com_time[,4],com_time[,6],com_time[,7],
        com_time[,8],com_time[,9],com_time[,10],names=c("logit","LASSO","ridge","E.net","Tree","Tree(C50)","Random Forest","KNN","SVM"),main="Time")
