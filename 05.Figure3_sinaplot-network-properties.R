# ====================================================================
#             Cultures as networks of cultural traits: 
#  A unifying framework for measuring culture and cultural distances
#
#     Luca De Benedictis - Roberto Rondinelli - Veronica Vinciotti
#
# 		                    Script 04 - Figure 3
# ====================================================================



# Load useful objects
load("bdgraph_estimates_new.RData") # required if you start from this script
countries <- readRDS("countries")

country.names <- countries$country_names


#------------------ network measures by country

library(igraph) # required if you didn't load before

# density
den <- NULL
for (k in 1:length(bdprob)){
  den <- c(den, sum(bdprob[[k]]) / ((nrow(bdprob[[k]])*(nrow(bdprob[[k]])-1) )/2) )
}
names(den) <- country.names; den


# weighted degree 
bdprob_graph <- list()
for (k in 1:length(bdprob)){
  bdprob_graph[[k]] <- graph_from_adjacency_matrix(bdprob_symm[[k]], 
                                                   mode = "undirected", 
                                                   diag = FALSE,
                                                   weighted = TRUE)
  print(k)
}
names(bdprob_graph) <- country.names


weighted_degree <- matrix(0, nrow = length(bdprob_graph), ncol = dim(bdprob_symm[[1]])[2])   
weighted_degree_mean <- NULL
weighted_degree_var <- NULL
for (i in 1:length(bdprob_graph)){    
  weighted_degree[i,] <- strength(bdprob_graph[[i]], 
                                  vids = V(bdprob_graph[[i]]), 
                                  loops=FALSE, 
                                  weights =  E(bdprob_graph[[i]])$weight)
  weighted_degree_mean <- c(weighted_degree_mean,
                            mean(weighted_degree[i,]))
  weighted_degree_var <- c(weighted_degree_var,
                           sd(weighted_degree[i,])/mean(weighted_degree[i,]))
}
rownames(weighted_degree) <- country.names
colnames(weighted_degree) <- colnames(bdprob_symm[[1]]) 
View(weighted_degree)

names(weighted_degree_mean) <- country.names; weighted_degree_mean
names(weighted_degree_var) <- country.names; weighted_degree_var 


# Global Clustering Coefficient
library(DirectedClustering)

gcc <- NULL
for (i in 1:length(bdprob_symm)){
  gcc <- c(gcc,ClustF(bdprob_symm[[i]])$GlobalCC)
}

names(gcc) <- country.names; gcc




#------------------ Figure 3 

# prepare dataframe
descr_df <- data.frame(den,
                       weighted_degree_var,
                       gcc,
                       countries$cultural_background)
descr_df$country <- row.names(descr_df)
colnames(descr_df) <- c("density", 
                        "centralization", 
                        "clustering", 
                        "WVS", 
                        "country")
View(descr_df)


descr_df2 <- descr_df %>% 
  pivot_longer(cols=density:clustering,
               values_to = "obs", names_to = "grp")
descr_df2$grp <- as.factor(descr_df2$grp)
descr_df2$grp <- ordered(descr_df2$grp, 
                         levels = c("density", 
                                    "centralization", 
                                    "clustering"))
View(descr_df2)


# plot
library(ggplot2) # required if you didn't load before
library(ggrepel) # required if you didn't load before
library(ggforce)

set.seed(1234)
sina_plot <- ggplot(descr_df2, aes(x=grp,y=obs)) +
  theme_bw() +
  geom_violin() +
  geom_sina(aes(color=WVS, group = grp), size=4) +
  
  geom_label(data=subset(descr_df2, grp=="density" & obs > quantile(obs, probs = c(0.99))),
             aes(label=country, size=10), nudge_x=0.17, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="density" & obs < quantile(obs, probs = c(0.1))),
             aes(label=country, size=10), nudge_x=0.1, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="density" & obs==descr_df2$obs[descr_df2$country=="Philippines"]),
             aes(label=country, size=10), nudge_x=0.1, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="density" & obs==descr_df2$obs[descr_df2$country=="Pakistan"]),
             aes(label=country, size=10), nudge_x=0.6, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="density" & obs==descr_df2$obs[descr_df2$country=="Thailand"]),
             aes(label=country, size=10), nudge_x=0.6, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="centralization" & obs > quantile(obs, probs = c(0.48))),
             aes(label=country, size=10), nudge_x=0.14, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="centralization" & obs < quantile(obs, probs = c(0.01))),
             aes(label=country, size=10), nudge_x=0.17, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="centralization" & obs==descr_df2$obs[descr_df2$country=="United States"]),
             aes(label=country, size=10), nudge_x=0.1, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="centralization" & obs==descr_df2$obs[descr_df2$country=="Philippines"]),
             aes(label=country, size=10), nudge_x=0.1, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="centralization" & obs==descr_df2$obs[descr_df2$country=="Pakistan"]),
             aes(label=country, size=10), nudge_x=0.6, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="centralization" & obs==descr_df2$obs[descr_df2$country=="Thailand"]),
             aes(label=country, size=10), nudge_x=0.6, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="clustering" & obs > quantile(obs, probs = c(0.99))),
             aes(label=country, size=10), nudge_x=0.14, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="clustering" & obs < quantile(obs, probs = c(0.01))),
             aes(label=country, size=10), nudge_x=0.1, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="clustering" & obs==descr_df2$obs[descr_df2$country=="United States"]),
             aes(label=country, size=10), nudge_x=0.1, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="clustering" & obs==descr_df2$obs[descr_df2$country=="Philippines"]),
             aes(label=country, size=10), nudge_x=0.1, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="clustering" & obs==descr_df2$obs[descr_df2$country=="Pakistan"]),
             aes(label=country, size=10), nudge_x=0.6, show.legend = F) +
  geom_label(data=subset(descr_df2, grp=="clustering" & obs==descr_df2$obs[descr_df2$country=="Thailand"]),
             aes(label=country, size=10), nudge_x=0.6, show.legend = F) +
  
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_text(size=18),
        legend.text = element_text(size=18),
        legend.position="bottom",
        legend.title = element_blank()) +
  
  guides(col = guide_legend(nrow = 3,
                            override.aes = list(size=5))); sina_plot
