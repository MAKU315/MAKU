rm(list=ls())
memory.limit(30000)

install.packages("KoNLP")
install.packages("plyr")
install.packages("XML")
install.packages("httr")
install.packages("tm") 

install.packages("clValid") 
install.packages("proxy") 
install.packages("kohonen") 
install.packages("FSelector") 


install.packages("stringi") 
install.packages("data.table")
install.packages("plyr")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("Rtextrankr")
install.packages("RWeka") 
install.packages("glmnet")


library(KoNLP)
library(plyr)
library(XML)
library(httr)
library(tm) 

library(clValid) 
library(proxy) 
library(kohonen) 
library(FSelector) 


library(stringi) 
library(data.table)
library(plyr)
library(RColorBrewer)
library(wordcloud)
library(Rtextrankr)
library(RWeka) 
library(glmnet)


getwd()
setwd("C://Users/korea/Desktop/수업/1-1/텍스트마이닝")
tr_com <- read.csv("stemmed_eurang_fin.csv", header=T, stringsAsFactors = FALSE) 

# Stopword 설정
myStopwords<-read.csv("stopwords.csv", header=F , sep=",",stringsAsFactors = FALSE)  
myStopwords<-as.character(myStopwords$V1)


# Corpus화로 변환
myCorpus <- Corpus(VectorSource(tr_com$Review)) 
# 구두점 제거
PreCorpus <- tm_map(myCorpus, content_transformer(removePunctuation)) 
PreCorpus[[2]][1]

PreCorpus <- tm_map(PreCorpus, removeWords, myStopwords) 


# Term document Matrix 생성
myTDM <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 2)) 

# TF-IDF : word weigting
# TF 1: Natural 
natural_tf <- as.matrix(myTDM) 
# TF 2: Logarithm 
log_tf <- log(1+natural_tf) 

# TF 3: augmented 
max_tf <- apply(natural_tf, 2, max)
augmented_tf <- 0.5+0.5*t(t(natural_tf)/max_tf) 

# (Inverse) Document Frequency 
N_total <- dim(natural_tf)[2] 

n_doc <- function(x) length(which(x > 0)) 


idf_vec <- log(N_total/apply(natural_tf, 1, n_doc)) 
idf_mat <- replicate(N_total, idf_vec) 


# TF-IDF 2: Log TF * IDF (cosine normalized) 
TFIDF_2 <- log_tf*idf_mat 

# LSI(Latent Semantic Indexing) - 
#cos_normalize <- function(x) x/sqrt(sum(x^2)) 
#TFIDF_2 <- apply(TFIDF_2, 2, cos_normalize) 

# Feature Selection 2: LSI 
# SVD singular Value Decomposition 
SVD.Mat <- svd(TFIDF_2) 
LSI.D <- SVD.Mat$d 
LSI.U <- SVD.Mat$u 
LSI.V <- SVD.Mat$v 

LSI.Mat <- as.data.frame(t((diag(100)*LSI.D[1:100]) %*% t(LSI.V[,1:100]))) 

# With LSI Features 
LSI.dist <- dist(LSI.Mat, method = "cosine", diag = TRUE) 

# 단점 메모리용량이 많이 필요


# pos 1 non-pos 0
#for(i in 1:dim(LSI.Mat)[1]){
#if (tr_com$Cat[i]>=5){ tr_com$Cat[i]=1}else {tr_com$cat[i]=0}
#}

tTFIDF_2<-t(as.matrix(TFIDF_2))

# Label
#tr_com$cat<-as.numeric(tr_com$cat)



### Perform hierarchical clustering 
LSI.hr <- hclust(LSI.dist, method = "single", members=NULL) 

LSI.hr <- hclust(LSI.dist, method = "complete") 
plot(LSI.hr, main = 'Cluster Dendrogram complete') 

rect.hclust(LSI.hr, k=20 , border="red")
# Find the clusters 

kn<-c(1,5,15,20,50)
LSI.hr.Clusters <- cutree(LSI.hr, k=kn) 
# Find the clusters 


# cluster 마다의 감성분석(Sentiment Analysis)
i<-20
##############i#####
for(i in 1:kn){
  #length(tr_com$Review[(LSI.hr.Clusters==i)])
  
  assign(paste0("y.clust",i),tr_com$Cat[(LSI.hr.Clusters==i)])
  assign(paste0("clust",i),tr_com[(LSI.hr.Clusters==i),])
  
  myCorpus <- Corpus(VectorSource(assign(paste0("clust",i),tr_com[(LSI.hr.Clusters==i),])$Review)) 
  myTDM <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1)) 
  
  # TF-IDF Variants 
  # TF 1: Natural 
  natural_tf <- as.matrix(myTDM) 
  # TF 2: Logarithm 
  log_tf <- log(1+natural_tf) 
  
  # TF 3: augmented 
  max_tf <- apply(natural_tf, 2, max)
  augmented_tf <- 0.5+0.5*t(t(natural_tf)/max_tf) 
  
  # (Inverse) Document Frequency 
  N_total <- dim(natural_tf)[2] 
  
  
  n_doc <- function(x) length(which(x > 0)) 
  
  
  idf_vec <- log(N_total/apply(natural_tf, 1, n_doc)) 
  
  idf_mat <- replicate(N_total, idf_vec) 
  
  # TF-IDF 2: Log TF * IDF (cosine normalized) 
  TFIDF_2 <- log_tf*idf_mat 
  
  cos_normalize <- function(x) x/sqrt(sum(x^2)) 
  
  TFIDF_2 <- apply(TFIDF_2, 2, cos_normalize) 
  
  tTFIDF_2<-t(TFIDF_2)
  
  
  # 1clust TDIDF.LASSO & Elastic net
  
  alp.vec <- seq(0.95,0.75,length.out=10)
  lam.vec <- c() 
  for(alp.pos in 1:10){
  
    glm.fit <- glmnet(x=tTFIDF_2 ,y=assign(paste0("y.clust",i),tr_com$Cat[(LSI.hr.Clusters==i)]),family="binomial",alpha=alp.vec[alp.pos])
  
    lam.vec <- c(lam.vec,glm.fit$lambda)
  
   }
  
  lam.vec <- exp(seq(log(max(lam.vec)),log(min(lam.vec)),length.out=50))
  
  cv.e.mat <- matrix(0,nrow=10,ncol=50)
  
  for(alp.pos in 1:10){
  
    cv.glm.fit <- cv.glmnet(x=tTFIDF_2 ,y=(assign(paste0("y.clust",i),tr_com$Cat[(LSI.hr.Clusters==i)])),lambda=lam.vec,  
                           alpha=alp.vec[alp.pos],type.measure = "mse")
  
    cv.e.mat[alp.pos,] <- cv.glm.fit$cvm 
  
  } 
  
  pos.opt <- which(cv.e.mat==min(cv.e.mat),arr.ind = TRUE)[1,]
  
  alp.opt <- alp.vec[pos.opt[1]]
  
  lam.opt <- lam.vec[pos.opt[2]]
  
  
  glm.fit<-glmnet(x=tTFIDF_2,y=assign(paste0("y.clust",i),tr_com$Cat[(LSI.hr.Clusters==i)])
                  ,family="binomial",lambda = (lam.opt-0.01),alpha=alp.vec[alp.pos])
  
  # ACC
  print(mean(assign(paste0("y.clust",i),tr_com$Cat[(LSI.hr.Clusters==i)])==predict(glm.fit,newx = tTFIDF_2,type="class")))
  
  
  coef<-(predict(glm.fit,type="coef"))
  coef<-as.data.frame(as.matrix(coef))
  # coef
  file_name=paste("C:/Users/user/Desktop/coef.csv", sep="")
  write.table(coef,file=file_name)
  
  
  file_name=paste("C:/Users/user/Desktop/Location.csv", sep="")
  Location<-tr_com$Location[(LSI.hr.Clusters==i)]
  write.table(Location,file=file_name)
  
  
  file_name=paste("C:/Users/user/Desktop/Cat.csv", sep="")
  Cat<-tr_com$Cat[(LSI.hr.Clusters==i)]
  write.table(Cat,file=file_name)
  
}

for(i in 6:20)
{
  k <- cbind(tr_com$Location[(LSI.hr.Clusters==i)],tr_com$Cat[(LSI.hr.Clusters==i)],tr_com$Review[(LSI.hr.Clusters==i)])
  file_name=paste("C:/Users/user/Desktop/cluster",i,".csv",sep="")
  write.table(k,file=file_name)
}


