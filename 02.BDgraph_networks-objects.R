# ====================================================================
#             Cultures as networks of cultural traits: 
#  A unifying framework for measuring culture and cultural distances
#
#     Luca De Benedictis - Roberto Rondinelli - Veronica Vinciotti
#
# 		     Script 02 - Operations on BDgraph network objects
# ====================================================================



# Load the results of our (or your) elaborations - extracted matrices
data <- readRDS("data")
load("bdgraph_2million.RData") # comment to load your elaborations
#load("bdgraph_estimates")     # uncomment to load your elaborations
library(igraph)


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


# Partial correlation - matrices
bdpar_corr <- list()
for (k in 1:length(bdkhat)){
  a <- bdkhat_corr[[k]]
  bdpar_corr[[k]] <- -diag(1/sqrt(diag(a))) %*% a %*% diag(1/sqrt(diag(a)))
  diag(bdpar_corr[[k]])<-1
}
names(bdpar_corr) <- country.names


# define partial correlation network
parcorr_graph <- list() 
for (k in 1:length(bdpar_corr)){
  parcorr_graph[[k]] <- graph_from_adjacency_matrix(bdpar_corr[[k]], 
                                                    mode = "undirected", 
                                                    diag = FALSE,
                                                    weighted = TRUE)
  print(k)   
}
names(parcorr_graph) <- country.names


# Posterior probability matrices - symmetrized
bdprob_symm <- list()
for (k in 1:length(bdprob)){
  bdprob_symm[[k]] <- bdprob[[k]] + t(bdprob[[k]])
  rownames(bdprob_symm[[k]]) <- colnames(data[[1]])
  colnames(bdprob_symm[[k]]) <- colnames(data[[1]])
}
names(bdprob_symm) <- country.names


# Posterior probability matrices - graph
bdprob_graph <- list()
for (k in 1:length(bdprob_symm)){
  bdprob_graph[[k]] <- graph_from_adjacency_matrix(bdprob_symm[[k]], 
                                                   mode = "undirected", 
                                                   diag = FALSE,
                                                   weighted = TRUE)
  print(k)
}
names(bdprob_graph) <- country.names


# Signed (partial correlation) - graph
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




save(bdkhat_corr,
     bdpar_corr, parcorr_graph,
     bdprob_symm, bdprob_graph,
     sign, sign_graph, 
     file = "bdgraph_estimates_new.RData")

