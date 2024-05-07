# ====================================================================
#             Cultures as networks of cultural traits: 
#  A unifying framework for measuring culture and cultural distances
#
#     Luca De Benedictis - Roberto Rondinelli - Veronica Vinciotti
#
# 	 Script 01 - Estimate country cultural networks through BDgraph
# ====================================================================



#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# IMPORTANT! 
# Our estimates are contained in the object "bdgraph_2million.RData"
# You can run this code to obtain new results otherwise you can skip this script


# Load data 
data <- readRDS("data")


#--------------------------- BDgraph

# NOTE: increasing the parameter "iter" in the "bdgraph" function will increase the timing
# NOTE: 10000 iterations take about 2 minutes and 20 seconds to estimate Algeria cultural network

# estimate networks with bdgraph
library(BDgraph)

for (k in 1:length(data)){
  
  t <- Sys.time()
  
  bdnet <- bdgraph(data[[k]], 
                   method = "gcgm", 
                   algorithm = "bdmcmc", 
                   iter = 10000, #for our elaborations we used 2000000 of iterations
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

names(bdnet) <- names(data)

# extract adjacency matrices: binary, precision matrix, posterior probabilities
bdadj <- list() 
for (k in 1:length(bdnet)){
  bdadj[[k]] <- as.matrix(summary(bdnet[[k]])$selected_g)
  print(k)
}
names(bdadj) <- names(data)

bdkhat <- list()
for (k in 1:length(bdnet)){     
  bdkhat[[k]] <- bdnet[[k]]$K_hat
  print(k)
}
names(bdkhat) <- names(data)

bdprob <- list() 
for (k in 1:length(bdnet)){
  bdprob[[k]] <- as.matrix(summary(bdnet[[k]])$p_links)
  print(k)
}
names(bdprob) <- names(data)


# Store your objects in a file .RData
save(bdadj,bdkhat,bdprob, file = "bdgraph_estimates.RData")

