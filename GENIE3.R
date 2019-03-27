getwd()
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("GENIE3")
library(GENIE3)
library(ggplot2)
library(network)
library(sna)
library("ggnetwork")
network::network.adjacency(emon[[1]])
rgraph(10, tprob = 0.2)
n <- network(rgraph(10, tprob = 0.2), directed = FALSE)
igraph::get.adjacency(emon[[1]])
data(emon)
View(as.matrix(emon[[1]]))

n2<-ggnetwork(emon[[1]], weights = "Frequency")
ggplot(n2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed")) +
  geom_nodetext(aes(label = vertex.names, size = 0.5))+ 
  geom_nodes(color = "tomato", size = 4) +
  theme_blank()

######################
exprMatr <- matrix(sample(1:10, 100, replace=TRUE), nrow=20)
rownames(exprMatr) <- paste("Gene", 1:20, sep="")
colnames(exprMatr) <- paste("Sample", 1:5, sep="")
head(exprMatr)
weightMat <- GENIE3(exprMatr)

regulators <- c("Gene2", "Gene4", "Gene7")
weightMat <- GENIE3(exprMatr, regulators=regulators)

weightMat[weightMat<0.5] <- 0
linkList <- getLinkList(weightMat)
linkList <- linkList[linkList[,3]!=0,]

net <- igraph::graph_from_edgelist(el =as.matrix(linkList[,1:2] ))
igraph::as_adjacency_matrix(net)

install.packages("ggplot2")
install.packages("network")
install.packages("igraph")

library(igraph)
linkList$weight <- linkList$weight *100
net <- igraph::graph_from_edgelist(el =as.matrix(linkList[,1:2] ))
plot(net, edge.arrow.size=.4, vertex.label.color="black", vertex.label.dist=1,
     vertex.size=7,vertex.label.font=2,vertex.color="skyblue")

#######
library("glmnet")
Samples_stand <- as.matrix(t(exprMatr))
nV <- dim(Samples_stand)[2]

est_MB<-matrix(0,nrow=nV,ncol=nV)

for(index in c(1:1))
{
  ### not in factor
  #if(colnames()[index] %in% fac_lst){
    yy<-Samples_stand[,index]                   # Decide yy (child) and xx (parents) in the LASSO type penalized LR
    xx<-Samples_stand[,-index]
    cv_lm <- cv.glmnet(xx, yy,family=c("gaussian"))
    glmnet_fit<-glmnet(xx, yy,family=c("gaussian"),lambda=cv_lm$lambda.1se)
    inter_coef_list<-coef(glmnet_fit)
    coef_list<-inter_coef_list[-1]
    
    est_MB[index,-index]<-coef_list
    
    
  #}
  
}

est_MB_short<-matrix(NA,nrow=1,ncol=3)
for(i in 1:nV)
{
  for(j in 1:nV)
  {
    if(est_MB[j,i]!=0)
    {
      est_MB_short<-rbind(est_MB_short,c(j,i,est_MB[j,i]))
    }
  }
}

colnames(est_MB_short)<-c("from","to","coeff")
est_MB_short<-est_MB_short[!is.na(est_MB_short[,1]),]
