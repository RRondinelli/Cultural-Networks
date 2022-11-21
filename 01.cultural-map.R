# ====================================================================
#             Cultures as networks of cultural traits: 
#  A unifying framework for measuring culture and cultural distances
#
#     Luca De Benedictis - Roberto Rondinelli - Veronica Vinciotti
#
# 		  Script 01 - Reproducing the Inglehart-Welzel Cultural Map
# ====================================================================



#--------------------------- GET DATA1
Wave6 <- readRDS("Wave6.rds")


# Prepare data

# Select variables
mapdata <- cbind(Wave6$V2,Wave6$V10,Wave6$V24,Wave6$V69,Wave6$V85,Wave6$V152,
                 Wave6$V203,Wave6$V204,Wave6$V211,Wave6$Y002,Wave6$Y003)
mapdata <- as.data.frame(mapdata)
    colnames(mapdata) <- c("V2",
                           "level of happiness",
                           "trust in people",
                           "respect for authority",
                           "voice through petitions",
                           "importance of God",
                           "justification of homosexuality",
                           "justification of abortion",
                           "national pride",
                           "post-materialism",
                           "obedience vs independence")
    
rm(Wave6)


library(tidyverse)

# Exclude countries associated with value -4 (question not asked in the Wave)
mapdata2 <- mapdata %>%
  filter(!V2 %in% c("112","634","702","414","818","860"))

# Recoding "Autonomy index"
mapdata2$`obedience vs independence`[mapdata2$`obedience vs independence`==2]<-5
mapdata2$`obedience vs independence`[mapdata2$`obedience vs independence`==1]<-4
mapdata2$`obedience vs independence`[mapdata2$`obedience vs independence`==0]<-3
mapdata2$`obedience vs independence`[mapdata2$`obedience vs independence`==-1]<-2 
mapdata2$`obedience vs independence`[mapdata2$`obedience vs independence`==-2]<-1

# Recoding "Proud of Nationality"
mapdata3 <- mapdata2
mapdata3$`national pride`[mapdata3$`national pride`==5] <- NA

rm(mapdata)
rm(mapdata2)

# Vector country names
country_names <- c("Algeria","Azerbaijan","Argentina","Australia","Armenia","Brazil","Chile",
                   "China","Taiwan","Colombia","Cyprus","Ecuador","Estonia","Georgia",
                   "Palestine","Germany","Ghana","Haiti","Hong Kong","India","Iraq","Japan",
                   "Kazakhstan","Jordan","South Korea","Kyrgyzstan","Lebanon","Libya",
                   "Malaysia","Mexico","Morocco","Netherlands","New Zealand","Nigeria",
                   "Pakistan","Peru","Philippines","Poland","Romania","Russia","Rwanda",
                   "Slovenia","South Africa","Zimbabwe","Spain","Sweden","Thailand",
                   "Trinidad","Tunisia","Turkey","Ukraine","United States","Uruguay","Yemen")

# Vector country groups - Inglehart-Welzel (IW) Cultural Map groups
background <- c(1,2,3,4,2,3,3,5,5,3,2,3,6,2,1,7,
                1,3,5,8,1,5,1,1,5,1,1,1,8,3,1,
                7,4,1,1,3,8,9,2,2,1,9,1,1,9,
                7,8,3,1,1,2,4,3,1)
cultural_background <- factor(background, 
                              labels=c("African_Islamic","Orthodox",
                                       "Latin America","English-speaking",
                                       "Confucian","Baltic","Protestant",
                                       "South Asia","Catholic Europe"))

# Country dataframe (names, country code, IW groups, sample size)
sample <- data.frame(table(mapdata3$V2))
  # default ggplot first 9 colors (to use in the next scripts)  
  n_background <- length(levels(cultural_background)) 
  library(scales)
  first9col_ggplot <- hue_pal()(n_background)
  country_colors <- first9col_ggplot[cultural_background]
    
    countries <- data.frame(country_names,sample$Var1,cultural_background,country_colors,sample$Freq)
      colnames(countries) <- c("country_names","country_id","cultural_background","group_colors","sample")
   
 
# One data matrix for each country
country_matrix <- list()
  
  for (i in 1:dim(countries)[1]){
    country_matrix[[i]] <- mapdata3 %>% filter(V2 == countries$country_id[i])
  }

  names(country_matrix) <- countries$country_names


# Deleting the first variable of each data matrix
data <- list()

  for (k in 1:length(country_matrix)){
    data[[k]]<-country_matrix[[k]][,-1]
  }    

# Replace negative values with NA
  for (k in 1:length(data)){
    data[[k]][data[[k]] < 0] <- NA 
  }

names(data) <- countries$country_names


rm(background)
rm(country_matrix)
rm(cultural_background)
rm(mapdata3)
rm(sample)
rm(country_names)
rm(i)
rm(k)


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------


#--------------------------- REPRODUCING INGLEHART-WELZEL CULTURAL MAP

# Average-based matrix
data_map6 <- matrix(0, nrow=length(data), ncol=dim(data[[1]])[2])

  for (i in 1:length(data)){
    for (j in 1:dim(data[[1]])[2]){
      data_map6[i,j] <- mean(data[[i]][,j][which(!is.na(data[[i]][,j]))])
    }
  }
    
  row.names(data_map6) <- countries$country_names
  colnames(data_map6) <- c("level of happiness",
                           "trust in people",
                           "respect for authority",
                           "voice through petitions",
                           "importance of God",
                           "justification of homosexuality",
                           "justification of abortion",
                           "national pride",
                           "post-materialism",
                           "obedience vs independence")
                           

# PCA for Inglehart-Welzel Cultural Map

library(FactoMineR)
library(factoextra)

pca_map6 <- PCA(data_map6, scale.unit=TRUE, ncp=5, graph = FALSE)
    
    # Results of PCA
    pca_map6$eig  #eigenvalues
    pca_map6$var$coord #coordinates of variables
    pca_map6$var$cor #correlations variables/axis
    pca_map6$var$cos2 #squared cosine of variables
    pca_map6$var$contrib #contributions of variables


# Create dataframe for Cultural Map plot
pcacoord_map6 <- as.data.frame(pca_map6$ind$coord[,c(1,2)]) # coordinates of countries
  pcacoord_map6$WVS <- countries$cultural_background  # IW groups
    colnames(pcacoord_map6) <- c("x", "y", "WVS")



library(ggplot2)
library(ggrepel) 
    
# Figure 1 - Correlation circle (to visualize enlarge the Plot window)
culvar <- fviz_pca_var(pca_map6,labelsize=7,axes=c(1,2),repel=TRUE,
                       col.var = "cos2", 
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                       col.circle = "black",
                       labelfont=2) +
                       theme(text = element_text(size = 15),
                             axis.title = element_text(size = 24),
                             axis.text = element_text(size = 20),
                             legend.text = element_text(size=20),
                             legend.title = element_text(size=20),
                             plot.title = element_blank()) +
                       xlab("Dimension 1 (51.59%)") +
                       ylab("Dimension 2 (19.33%)"); culvar

# Figure 2 - Cultural Map (to visualize enlarge the Plot window)
ggplot_map6 <- ggplot(pcacoord_map6, aes(x=x, y=y, colour=WVS, fontface="bold")) +
                      theme_bw() +
                      geom_hline(yintercept=0) +
                      geom_vline(xintercept=0) +
                      geom_point(size=7) +
                      xlab("Dimension 1 (51.59%)") +
                      ylab("Dimension 2 (19.33%)") +
                      theme(plot.title = element_blank(),
                            axis.title.x = element_text(size=28),
                            axis.title.y = element_text(size=28),
                            axis.text=element_text(size=22),
                            legend.title = element_blank(),
                            legend.position="bottom",
                            legend.text = element_text(size=28)) +
                      guides(col = guide_legend(nrow = 3)) +
                      geom_text_repel(label=rownames(pcacoord_map6), 
                                      size=9, show.legend = F); ggplot_map6




