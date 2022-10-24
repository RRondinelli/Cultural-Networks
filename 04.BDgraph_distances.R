# ====================================================================
#             Cultures as networks of cultural traits: 
#  A unifying framework for measuring culture and cultural distances
#
#     Luca De Benedictis - Roberto Rondinelli - Veronica Vinciotti
#
# 		              Script 04 - BDgraph distances
# ====================================================================



# Load the results of our (or your) elaborations - extracted matrices
data <- readRDS("data")
load("bdgraph_2million.RData")
#load("bdgraph_estimates") #switch to load your elaborations

#--------------------------- Standardized precision matrices and partial correlation

country.names <- names(data)

names(bdkhat) <- country.names
names(bdprob) <- country.names
names(bdadj) <- country.names

# Transformed Precision matrices (K) in terms of correlation
bdkhat_corr <- list()
for (k in 1:length(bdkhat)){
  a <- solve(bdkhat[[k]])
  b <- diag(1/sqrt(diag(a))) %*% a %*% diag(1/sqrt(diag(a)))
  bdkhat_corr[[k]] <- solve(b)
}
names(bdkhat_corr) <- country.names

# Partial correlation matrices
bdpar_corr <- list()
for (k in 1:length(bdkhat)){
  a <- bdkhat_corr[[k]]
  bdpar_corr[[k]] <- -diag(1/sqrt(diag(a))) %*% a %*% diag(1/sqrt(diag(a)))
  diag(bdpar_corr[[k]])<-1
}
names(bdpar_corr) <- country.names

# Symmetrize posterior probability matrices
bdprob_symm <- list()
for (k in 1:length(bdprob)){
  bdprob_symm[[k]] <- bdprob[[k]] + t(bdprob[[k]])
  rownames(bdprob_symm[[k]]) <- colnames(data[[1]])
  colnames(bdprob_symm[[k]]) <- colnames(data[[1]])
}
names(bdprob_symm) <- country.names


library(igraph)

# Signed (partial correlation) matrices
sign <- list()
sign_graph <- list()
for (k in 1:length(bdpar_corr)){
  sign[[k]] <- bdpar_corr[[k]]/abs(bdpar_corr[[k]])
  sign_graph[[k]] <- graph_from_adjacency_matrix(sign[[k]], 
                                                 mode = "undirected", 
                                                 diag = FALSE,
                                                 weighted = TRUE)
}
names(sign) <- country.names
names(sign_graph) <- country.names


save(bdkhat_corr,bdpar_corr,bdprob,bdprob_symm,sign,sign_graph, file = "bdgraph_estimates_new.RData")








#--------------------------- Marginal distributions

# set categories for each cultural trait
n.ct<-ncol(data[[1]])
cat.ct<-list()
for(k in 1:n.ct)
{
  k.val<-as.numeric(lapply(data,function(x)length(table(x[,k]))))
  cat.ct[[k]]<-sort(unique(data[[which.max(k.val)]][,k]))
}

# marginal distributions
f.marginals<-list()
mean.ct<-list()
for(i in 1:length(data))
{
  dat<-data[[i]]
  mean.ct[[i]]<-apply(dat,2,function(x) mean(na.omit(x)))
  f.marginals[[i]]<-list()
  for(j in 1:ncol(dat))
  {
    cate<-sort(unique(dat[,j]))
    if(length(cate)==length(cat.ct[[j]]))
      f.marginals[[i]][[j]]<-as.numeric(table(dat[,j]))+rep(1,length(cate))
    else
    {
      f.marginals[[i]][[j]]<-rep(1,length(cat.ct[[j]]))
      f.marginals[[i]][[j]][cate]<-as.numeric(table(dat[,j]))+rep(1,length(cate))
    }
    f.marginals[[i]][[j]]<-f.marginals[[i]][[j]]/sum(f.marginals[[i]][[j]])
  }
  
}
names(f.marginals)<-country.names
names(mean.ct)<-country.names



#--------------------------- Jeffreys' divergence function

jd.cggm<-function(f1,f2,K1,K2)
  #f1: vector of marginal probabilities for model 1 as a list across the p cultural traits
  #f2: vector of marginal probabilities for model 2 as a list across the p cultural traits
  #K1: precision matrix of model 1 (inverse of the correlation matrix)
  #K2: precision matrix of model 2 (inverse of the correlation matrix)
{
  p<-ncol(K1)
  
  #KL-distance between marginals
  k.marj<-NULL
  for(j in 1:p)
  {
    k.marj[j]<-sum((f1[[j]]-f2[[j]]) * log(f1[[j]]/f2[[j]]))
  }
  k.mar<-sum(k.marj)
  
  #JD-distance between networks
  k.net<- 0.5*(sum(K2*solve(K1)) + sum(K1*solve(K2)))-p
  
  #Final JD distance
  k<-sum(k.mar,k.net)
  return(list(dist.Hap=k.marj[1],
              dist.Tru=k.marj[2],
              dist.Res=k.marj[3],
              dist.Voi=k.marj[4],
              dist.God=k.marj[5],
              dist.Hom=k.marj[6],
              dist.Abo=k.marj[7],
              dist.Nat=k.marj[8],
              dist.Mat=k.marj[9],
              dist.Aut=k.marj[10],
              dist.marg=k.mar,
              dist.net=k.net,
              dist=k))
}


rm(data)
