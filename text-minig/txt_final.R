install.packages("clValid") 
install.packages("proxy") 
install.packages("kohonen") 
install.packages("FSelector") 
rm(list=ls())
memory.limit(30000)
library(glmnet)
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
library(KoNLP)
library(data.table)
library(plyr)
library(RColorBrewer)
library(wordcloud)
library(Rtextrankr)
library(RWeka) 

getwd()
setwd("C:/Users/user/Desktop/텍스트마이닝")
tr_com <- read.csv("stemmed_eurang_fin.csv", header=T, stringsAsFactors = FALSE) 

#myStopwords<-read.csv("stopwords.csv", header=F , sep=",",stringsAsFactors = FALSE)  
#myStopwords<-as.character(myStopwords$V1)



myCorpus <- Corpus(VectorSource(tr_com$Review)) 
#PreCorpus <- tm_map(myCorpus, content_transformer(removePunctuation)) 
#PreCorpus[[2]][1]

#PreCorpus <- tm_map(PreCorpus, removeWords, myStopwords) 

myTDM <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 2)) 

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

# LSI 안됨
#cos_normalize <- function(x) x/sqrt(sum(x^2)) 
#TFIDF_2 <- apply(TFIDF_2, 2, cos_normalize) 

# Feature Selection 2: LSI 
SVD.Mat <- svd(TFIDF_2) 
LSI.D <- SVD.Mat$d 
LSI.U <- SVD.Mat$u 
LSI.V <- SVD.Mat$v 

LSI.Mat <- as.data.frame(t((diag(100)*LSI.D[1:100]) %*% t(LSI.V[,1:100]))) 

# With LSI Features 
LSI.dist <- dist(LSI.Mat, method = "cosine", diag = TRUE) 



# pos 1 non-pos 0
#for(i in 1:dim(LSI.Mat)[1]){
#if (tr_com$Cat[i]>=5){ tr_com$Cat[i]=1}else {tr_com$cat[i]=0}
#}

tTFIDF_2<-t(as.matrix(TFIDF_2))

# Label
#tr_com$cat<-as.numeric(tr_com$cat)


# Perform hierarchical clustering 
LSI.hr <- hclust(LSI.dist, method = "average", members=NULL) 
LSI.hr <- hclust(LSI.dist, method = "complete") 
plot(LSI.hr) 

rect.hclust(LSI.hr, k=20
            , border="red")
# Find the clusters 

kn<-20
LSI.hr.Clusters <- cutree(LSI.hr, k=kn) 
select.kn<-c()
for(i in 1:kn){
select.kn<-c(select.kn,(length(tr_com$Review[(LSI.hr.Clusters==i)])>10)*i)
}
select.kn<-select.kn[select.kn>0]
tr_com_mod<-data.frame()
for(i in 1:length(select.kn)){
  tr_com_mod<-rbind(tr_com_mod,tr_com[(LSI.hr.Clusters==i),])
}

myCorpus <- Corpus(VectorSource(tr_com_mod$Review)) 
#PreCorpus <- tm_map(myCorpus, content_transformer(removePunctuation)) 
#PreCorpus[[2]][1]

#PreCorpus <- tm_map(PreCorpus, removeWords, myStopwords) 

myTDM <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 2)) 

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

# LSI 안됨
#cos_normalize <- function(x) x/sqrt(sum(x^2)) 
#TFIDF_2 <- apply(TFIDF_2, 2, cos_normalize) 

# Feature Selection 2: LSI 
SVD.Mat <- svd(TFIDF_2) 
LSI.D <- SVD.Mat$d 
LSI.U <- SVD.Mat$u 
LSI.V <- SVD.Mat$v 

LSI.Mat <- as.data.frame(t((diag(100)*LSI.D[1:100]) %*% t(LSI.V[,1:100]))) 

# With LSI Features 
LSI.dist <- dist(LSI.Mat, method = "cosine", diag = TRUE) 

tTFIDF_2<-t(as.matrix(TFIDF_2))

# Label
#tr_com$cat<-as.numeric(tr_com$cat)


# Perform hierarchical clustering 
LSI.hr <- hclust(LSI.dist, method = "average", members=NULL) 
LSI.hr <- hclust(LSI.dist, method = "complete") 
plot(LSI.hr) 

rect.hclust(LSI.hr, k=20
            , border="red")
# Find the clusters 

kn<-20
LSI.hr.Clusters <- cutree(LSI.hr, k=kn) 


###################
for(i in 1:kn){
#length(tr_com$Review[(LSI.hr.Clusters==i)])

assign(paste0("y.clust",i),tr_com_mod$Cat[(LSI.hr.Clusters==i)])
assign(paste0("clust",i),tr_com_mod[(LSI.hr.Clusters==i),])

myCorpus <- Corpus(VectorSource(assign(paste0("clust",i),tr_com_mod[(LSI.hr.Clusters==i),])$Review)) 
myTDM <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1)) 
myTDM1 <- removeSparseTerms(myTDM, 0.999)
# TF-IDF Variants 
# TF 1: Natural 
natural_tf <- as.matrix(myTDM1) 
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


# 1clust TDIDF.LASSO
 
#alp.vec <- seq(0.95,0.75,length.out=10)
#lam.vec <- c() 
#for(alp.pos in 1:10){
  
#  glm.fit <- glmnet(x=tTFIDF_2 ,y=assign(paste0("y.clust",i),tr_com$Cat[(LSI.hr.Clusters==i)]),family="binomial",alpha=alp.vec[alp.pos])
  
#  lam.vec <- c(lam.vec,glm.fit$lambda)
  
# }

#lam.vec <- exp(seq(log(max(lam.vec)),log(min(lam.vec+0.001)),length.out=50))

#cv.e.mat <- matrix(0,nrow=10,ncol=50)

#for(alp.pos in 1:10){
  
#  cv.glm.fit <- cv.glmnet(x=tTFIDF_2 ,y=(assign(paste0("y.clust",i),tr_com$Cat[(LSI.hr.Clusters==i)])),lambda=lam.vec,  
 #                         alpha=alp.vec[alp.pos],type.measure = "mse")
  
#  cv.e.mat[alp.pos,] <- cv.glm.fit$cvm 
  
#} 

#pos.opt <- which(cv.e.mat==min(cv.e.mat),arr.ind = TRUE)[1,]

#alp.opt <- alp.vec[pos.opt[1]]

#lam.opt <- lam.vec[pos.opt[2]]
#print(alp.opt)

  cv.glm.fit <- cv.glmnet(x=tTFIDF_2 ,y=(assign(paste0("y.clust",i),tr_com_mod$Cat[(LSI.hr.Clusters==i)])),  
                         alpha=0.9,type.measure = "mse")

glm.fit<-glmnet(x=tTFIDF_2,y=assign(paste0("y.clust",i),tr_com_mod$Cat[(LSI.hr.Clusters==i)])
                ,family="binomial",lambda = cv.glm.fit$lambda.1se,alpha=0.9)
# ACC
print(mean(assign(paste0("y.clust",i),tr_com_mod$Cat[(LSI.hr.Clusters==i)])==predict(glm.fit,newx = tTFIDF_2,type="class")))


coef<-(predict(glm.fit,type="coef"))
coef<-as.data.frame(as.matrix(coef))
# coef
file_name=paste("C:/Users/user/Desktop/텍스트마이닝/coef",i,".csv", sep="")
write.table(coef,file=file_name)


file_name=paste("C:/Users/user/Desktop/텍스트마이닝/Location",i,".csv", sep="")
Location<-tr_com_mod$Location[(LSI.hr.Clusters==i)]
write.table(Location,file=file_name)


file_name=paste("C:/Users/user/Desktop/텍스트마이닝/Cat",i,".csv", sep="")
Cat<-tr_com_mod$Cat[(LSI.hr.Clusters==i)]
write.table(Cat,file=file_name)



}





