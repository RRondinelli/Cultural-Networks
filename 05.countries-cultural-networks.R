# ====================================================================
#             Cultures as networks of cultural traits: 
#  A unifying framework for measuring culture and cultural distances
#
#     Luca De Benedictis - Roberto Rondinelli - Veronica Vinciotti
#
# 		    Script 05 - example of country's cultural networks
# ====================================================================



#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# IMPORTANT! 
# You can replicate this code for each of the 54 countries


# Load useful objects
data <- readRDS("data")             # required if you start from this script
load("bdgraph_estimates_new.RData") # networks objects generated in script "02.BDgraph_networks-objects", required if you start from this script 
load("coords.RData")

library(remotes)
remotes::install_version("Rttf2pt1", version = "1.3.8") # do not install if you already did
library(Rttf2pt1)
library(png)
library(ggplot2)
library(igraph)


#------------------ EXAMPLE GRAPH: UNITED STATES

# Generate Distributions (run only first time)
US_trait <- list()

for (i in 1:dim(data$`United States`)[2]){
  
  trait <- eval(substitute(
    ggplot(data$`United States`, aes(x=data$`United States`[,i])) + 
      geom_histogram(binwidth=1,fill="#0033CC") + 
      theme_classic() +
      theme(plot.title = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text=element_blank(),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            line = element_blank())
    ,list(i = i)))
  
  print(i)
  print(trait)
  US_trait[[i]] <- trait
  
}

for (i in 1:dim(data$`United States`)[2]){
  ggsave(paste0("US", i, ".png"), 
         US_trait[[i]], width = 1000, height = 1000, units ="px")
}


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# IMPORTANT!
# Before run the following lines,
# remember to put the US graphs saved above
# in a cartel called "US" that you need to create in your directory 


US_png <- list()
for (i in 1:dim(data$`United States`)[2]){
  US_png[[i]] <- readPNG(paste0("US/US", i, ".png"))
}

# Set up Graph 
US <- parcorr_graph$`United States`
E(US)$trasp <- E(bdprob_graph$`United States`)$weight
E(US)$color <- ifelse(E(sign_graph$`United States`)$weight==1, "green", "red")
E(US)$green <- ifelse(E(sign_graph$`United States`)$weight==1, 0.8, 0)
E(US)$red <- ifelse(E(sign_graph$`United States`)$weight==-1, 0.8, 0)
E(US)$type <- ifelse(E(bdprob_graph$`United States`)$weight>0.5, 1, 3)
V(US)$raster <- US_png[V(US)]
V(US)$name <- c("level of happiness","trust in people",
                "respect for authority","voice through petitions",
                "importance of God","justification of homosexuality",
                "justification of abortion","national pride",
                "post-materialism","obedience vs independence")

# United States graph
plot_US <- plot.igraph(US, directed=F,
                       edge.color = rgb(E(US)$red, E(US)$green, 0, E(US)$trasp),
                       edge.width = (E(US)$weight*E(US)$weight*20)+2, 
                       edge.lty=E(US)$type,
                       vertex.label.color="black",    
                       vertex.label.dist=1.5, 
                       vertex.size=30,
                       vertex.label.cex = 2,
                       vertex.shape="raster",
                       vertex.label.degree=pi/2,
                       # vertex.label.family="LM Roman 10",
                       vertex.label.font=2,
                       layout = coords)



