# ====================================================================
#             Cultures as networks of cultural traits: 
#  A unifying framework for measuring culture and cultural distances
#
#     Luca De Benedictis - Roberto Rondinelli - Veronica Vinciotti
#
# 	 Script 01 - Estimate country cultural networks through BDgraph
# ====================================================================


# Load data
data <- readRDS("data")
country.names <- names(data)


# IMPORTANT! 
# You can run the following code ("BDgraph" and "Extract BDgraph objects") to obtain new results
# Otherwise you can skip "BDgraph" and "Extract BDgraph objects" and load our estimates "bdgraph_2million.RData"  
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

#--------------------------- BDgraph

# NOTE: increasing the parameter "iter" in the "bdgraph" function will increase the timing
# NOTE: 10000 iterations take about 2 minutes and 20 seconds to estimate Algeria cultural network

# estimate networks with bdgraph
library(BDgraph)
library(stringr)

for (k in 1:length(data)){
  
  t <- Sys.time()
  
  bdnet <- bdgraph(data[[k]], 
                   method = "gcgm", 
                   algorithm = "bdmcmc", 
                   iter = 1000, #for our elaborations we used 2000000 of iterations
                   not.cont = c(rep(1,dim(data[[k]])[2])), 
                   save = TRUE)
  
  t <- c(t, Sys.time())
  
  saveRDS(bdnet,file=paste('bdnet',k,sep=""))
  saveRDS(t,paste('t',k, sep=""))
  
  t <- NULL
  bdnet <- NULL
}

# import bdgraph objects
nets <- list.files(pattern = "bdnet")
nets <- nets[str_order(nets,numeric=TRUE)]

bdnet <- list()
for (i in 1:length(nets)) {
  bdnet[[i]] <- readRDS(nets[i])
}

names(bdnet) <- country.names

#--------------------------- Extract BDgraph objects

# binary
bdadj <- list() 
for (k in 1:length(bdnet)){
  bdadj[[k]] <- as.matrix(summary(bdnet[[k]])$selected_g)
  print(k)
}
names(bdadj) <- country.names

# precision matrix
bdkhat <- list()
for (k in 1:length(bdnet)){     
  bdkhat[[k]] <- bdnet[[k]]$K_hat
  print(k)
}
names(bdkhat) <- country.names

# posterior probabilities
bdprob <- list() 
for (k in 1:length(bdnet)){
  bdprob[[k]] <- as.matrix(summary(bdnet[[k]])$p_links)
  print(k)
}
names(bdprob) <- country.names

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------




#--------------------------- uncomment to load the BDgraph objects of our elaborations
# load("bdgraph_2million.RData")
# names(bdkhat) <- country.names
# names(bdprob) <- country.names
# names(bdadj) <- country.names
#-------------------------------------------------------------------------------------




#--------------------------- Operations on your BDgraph objects or our loaded BDgraph objects

library(igraph)

# Transforme Precision matrices (K) in terms of correlation
bdkhat_corr <- list()
for (k in 1:length(bdkhat)){
  a <- solve(bdkhat[[k]])
  b <- diag(1/sqrt(diag(a))) %*% a %*% diag(1/sqrt(diag(a)))
  bdkhat_corr[[k]] <- solve(b)
}
names(bdkhat_corr) <- country.names



# Partial correlations - matrices
bdpar_corr <- list()
for (k in 1:length(bdkhat)){
  a <- bdkhat_corr[[k]]
  bdpar_corr[[k]] <- -diag(1/sqrt(diag(a))) %*% a %*% diag(1/sqrt(diag(a)))
  diag(bdpar_corr[[k]])<-1
}
names(bdpar_corr) <- country.names

# Partial correlations - graphs
parcorr_graph <- list() 
for (k in 1:length(bdpar_corr)){
  parcorr_graph[[k]] <- graph_from_adjacency_matrix(bdpar_corr[[k]], 
                                                    mode = "undirected", 
                                                    diag = FALSE,
                                                    weighted = TRUE)
  print(k)   
}
names(parcorr_graph) <- country.names



# Posterior probabilities - symmetrized matrices
bdprob_symm <- list()
for (k in 1:length(bdprob)){
  bdprob_symm[[k]] <- bdprob[[k]] + t(bdprob[[k]])
  rownames(bdprob_symm[[k]]) <- colnames(data[[1]])
  colnames(bdprob_symm[[k]]) <- colnames(data[[1]])
}
names(bdprob_symm) <- country.names

# Posterior probabilities - graphs
bdprob_graph <- list()
for (k in 1:length(bdprob_symm)){
  bdprob_graph[[k]] <- graph_from_adjacency_matrix(bdprob_symm[[k]], 
                                                   mode = "undirected", 
                                                   diag = FALSE,
                                                   weighted = TRUE)
  print(k)
}
names(bdprob_graph) <- country.names



# Signed (partial correlations) - graphs
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



#--------------------------- Store the objects in a file .RData
save(bdkhat_corr,
     bdpar_corr, parcorr_graph,
     bdprob_symm, bdprob_graph,
     sign, sign_graph, 
     file = "bdgraph_estimates.RData")

