# ====================================================================
#             Cultures as networks of cultural traits: 
#  A unifying framework for measuring culture and cultural distances
#
#     Luca De Benedictis - Roberto Rondinelli - Veronica Vinciotti
#
# 		            Script 03 - compute distances
# ====================================================================


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# IMPORTANT! 
# Run script 03.distance-functions.R before


# Load useful objects
load("bdgraph_estimates.RData") # networks objects generated in script "01.BDgraph_infer-networks", required if you start from this script 
pca_map6 <- readRDS("pca_map6")
countries <- readRDS("countries")

country.names <- names(data)


#------------------ Calculate distances (apply the jd.cggm function)

dist.marg <- matrix(0,length(bdkhat_corr),length(bdkhat_corr))
dist.net <- matrix(0,length(bdkhat_corr),length(bdkhat_corr))
dist.tot <- matrix(0,length(bdkhat_corr),length(bdkhat_corr))
dist.mean <- matrix(0,length(bdkhat_corr),length(bdkhat_corr))
dist.Hap <- matrix(0, length(bdkhat_corr), length(bdkhat_corr))
dist.Tru <- matrix(0, length(bdkhat_corr), length(bdkhat_corr))
dist.Res <- matrix(0, length(bdkhat_corr), length(bdkhat_corr))
dist.Voi <- matrix(0, length(bdkhat_corr), length(bdkhat_corr))
dist.God <- matrix(0, length(bdkhat_corr), length(bdkhat_corr))
dist.Hom <- matrix(0, length(bdkhat_corr), length(bdkhat_corr))
dist.Abo <- matrix(0, length(bdkhat_corr), length(bdkhat_corr))
dist.Nat <- matrix(0, length(bdkhat_corr), length(bdkhat_corr))
dist.Mat <- matrix(0, length(bdkhat_corr), length(bdkhat_corr))
dist.Aut <- matrix(0, length(bdkhat_corr), length(bdkhat_corr))
for(i in 1: length(bdkhat_corr))
  for(j in 1:length(bdkhat_corr))
  {
    a<-jd.cggm(f.marginals[[i]],f.marginals[[j]],bdkhat_corr[[i]],bdkhat_corr[[j]])
    dist.marg[i,j]<-a$dist.marg
    dist.net[i,j]<-a$dist.net
    dist.tot[i,j]<-a$dist
    dist.mean[i,j]<-sqrt(sum((mean.ct[[i]]-mean.ct[[j]])^2)) #euclidean distance ---> (Mean <- dist(data_map6))
    dist.Hap[i,j]<-a$dist.Hap
    dist.Tru[i,j]<-a$dist.Tru
    dist.Res[i,j]<-a$dist.Res
    dist.Voi[i,j]<-a$dist.Voi
    dist.God[i,j]<-a$dist.God
    dist.Hom[i,j]<-a$dist.Hom
    dist.Abo[i,j]<-a$dist.Abo
    dist.Nat[i,j]<-a$dist.Nat
    dist.Mat[i,j]<-a$dist.Mat
    dist.Aut[i,j]<-a$dist.Aut
  }

colnames(dist.marg) <- colnames(dist.net) <- colnames(dist.tot) <- colnames(dist.mean) <-
  colnames(dist.Hap) <- colnames(dist.Tru) <- colnames(dist.Res) <- colnames(dist.Voi) <- colnames(dist.God) <-
  colnames(dist.Hom) <- colnames(dist.Abo) <- colnames(dist.Nat) <- colnames(dist.Mat) <- colnames(dist.Aut) <-
  rownames(dist.marg) <- rownames(dist.net) <- rownames(dist.tot) <- rownames(dist.mean) <- 
  rownames(dist.Hap) <- rownames(dist.Tru) <- rownames(dist.Res) <- rownames(dist.Voi) <- rownames(dist.God) <-
  rownames(dist.Hom) <- rownames(dist.Abo) <- rownames(dist.Nat) <- rownames(dist.Mat) <- rownames(dist.Aut) <- country.names

# Vectorized distances
dmean<-dist.mean[lower.tri(dist.mean)]
dm<-as.numeric(dist.marg[lower.tri(dist.marg)])
dn<-as.numeric(dist.net[lower.tri(dist.net)])
dt<-as.numeric(dist.tot[lower.tri(dist.tot)])
dHap<-as.numeric(dist.Hap[lower.tri(dist.Hap)])
dTru<-as.numeric(dist.Tru[lower.tri(dist.Tru)])
dRes<-as.numeric(dist.Res[lower.tri(dist.Res)])
dVoi<-as.numeric(dist.Voi[lower.tri(dist.Voi)])
dGod<-as.numeric(dist.God[lower.tri(dist.God)])
dHom<-as.numeric(dist.Hom[lower.tri(dist.Hom)])
dAbo<-as.numeric(dist.Abo[lower.tri(dist.Abo)])
dNat<-as.numeric(dist.Nat[lower.tri(dist.Nat)])
dMat<-as.numeric(dist.Mat[lower.tri(dist.Mat)])
dAut<-as.numeric(dist.Aut[lower.tri(dist.Aut)])



# CulturalMap distance matrix using Euclidean Distance of the countries in the Cultural Map
CulturalMap <- dist(pca_map6$ind$coord[,c(1:2)])
CulturalMap <- as.matrix(CulturalMap)
# Vectorized distance of Cultural Map
dcultmap <- CulturalMap[lower.tri(CulturalMap)]



# Network distances using the Frobenius Norm of their differences

dist.fb.prec<-matrix(0,length(data),length(data))
dist.fb.prob<-matrix(0,length(data),length(data))
dist.fb.corr<-matrix(0,length(data),length(data))
dist.fb.parcorr<-matrix(0,length(data),length(data))
for(i in 1: length(data))
  for(j in 1:length(data))
  {
    dist.fb.prec[i,j] <- norm(bdkhat_corr[[i]]-bdkhat_corr[[j]], type = "F")
    dist.fb.prob[i,j] <- norm(bdprob_symm[[i]]-bdprob_symm[[j]], type = "F")
    dist.fb.corr[i,j] <- norm(solve(bdkhat_corr[[i]])-solve(bdkhat_corr[[j]]), type = "F")
    dist.fb.parcorr[i,j] <- norm(bdpar_corr[[i]]-bdpar_corr[[j]], type = "F")
  }
# Vectorized distances of Frobenius norm
dprob <- as.numeric(dist.fb.prob[lower.tri(dist.fb.prob)])
dpar <- as.numeric(dist.fb.parcorr[lower.tri(dist.fb.parcorr)])




#------------------ Correlation matrix of the proposed cultural distances

# Dataframe
distances_corr <- data.frame(dm,
                             dcultmap,
                             dmean,
                             dn,
                             dpar,
                             dprob)
colnames(distances_corr) <- c("JD marginals",
                              "IW index",
                              "Mean diff",
                              "JD network",
                              "ParCorr diff",
                              "ProbEdge diff")

# Correlation matrix
cor.mat <- round(cor(distances_corr),2)


# Figure 5 - Correlation matrix of different measures of cultural distance
library(ggcorrplot)

ggcorrplot(cor.mat,
           colors=c("#D55E00", "white", "#009E73"),
           lab=TRUE,
           lab_size = 5,
           outline.color = "white") +
  geom_vline(xintercept=1:ncol(cor.mat)-0.5, colour="white", size=4) +
  geom_hline(yintercept=1:ncol(cor.mat)-0.5, colour="white", size=4) +
  theme(text = element_text(face ="bold" 
                            # , family="LM Roman 10"
                            ),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=13),
        legend.title = element_text(size=13))







#------------------ Dataframe of distances

library(igraph) # require if you didn't load before
library(readxl) # require if you didn't load before
library(tidyverse) # require if you didn't load before


# Dataframe of distances

networks_graph <- graph_from_adjacency_matrix(dist.net, mode="undirected", diag=FALSE, weighted=TRUE)
networks_list <- igraph::as_edgelist(networks_graph)

iso2 <- read_excel("iso_alpha_2.xls") # Upload iso2 file

geo_cepii <- read_excel("geo_cepii.xls") # Upload iso3 file
geo_cepii_nodup <- geo_cepii[!duplicated(geo_cepii[,c('iso2')]),]
iso3 <- geo_cepii_nodup %>% 
  select(iso2, iso3)

countries1 <- countries %>% 
  left_join(iso2, by = c("country_names" = "country_name")) %>% 
  mutate(
    iso2 = case_when(country_names %in% "Libya" ~ "LY", TRUE ~
                          case_when(country_names %in% "Palestine" ~ "PS", TRUE ~
                                    case_when(country_names %in% "Trinidad" ~ "TT", TRUE ~
                                              iso2))) 
  ) %>% 
  left_join(iso3, by = c("iso2" = "iso2")) %>%  # Association Country-iso2-iso3
  select(country_names, iso3)

  
# Dataframe 

distances <- data.frame(networks_list,
                        dHap,
                        dTru,
                        dRes,
                        dVoi,
                        dGod,
                        dHom,
                        dAbo,
                        dNat,
                        dMat,
                        dAut,
                        dm,
                        dn,
                        dt,
                        dmean,
                        dcultmap)

colnames(distances) <- c("Country1",
                         "Country2",
                         "level of happiness",
                         "trust in people",
                         "respect for authority",
                         "voice through petitions",
                         "importance of God",
                         "justification of homosexuality",
                         "justification of abortion",
                         "national pride",
                         "post-materialism",
                         "obedience vs. independence",
                         "JD marginals",
                         "JD network",
                         "JD index",
                         "Mean diff",
                         "IW index")

distances1 <- distances %>% 
  inner_join(countries1, by = c("Country1" = "country_names")) %>% 
  inner_join(countries1, by = c("Country2" = "country_names")) %>% 
  relocate(iso3.x, .after = Country2) %>% 
  relocate(iso3.y, .after = iso3.x) %>% 
  rename("Country1.iso3" = "iso3.x",
         "Country2.iso3" = "iso3.y")

write.table(distances1, file="distances_WVS-W6.csv", row.names = FALSE)
# write.csv2(distances1, file="distances_WVS-W6_1.csv")


 
 