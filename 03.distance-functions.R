# ====================================================================
#             Cultures as networks of cultural traits: 
#  A unifying framework for measuring culture and cultural distances
#
#     Luca De Benedictis - Roberto Rondinelli - Veronica Vinciotti
#
# 		       Script 02 - Jeffrey's divergence function
# ====================================================================



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