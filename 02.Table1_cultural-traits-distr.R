# ====================================================================
#             Cultures as networks of cultural traits: 
#  A unifying framework for measuring culture and cultural distances
#
#     Luca De Benedictis - Roberto Rondinelli - Veronica Vinciotti
#
# 		                    Script 02 - Table 1
# ====================================================================



# Load Useful objects (if not already generated with "01.cultural-map.R") 
data <- readRDS("data")
data_map6 <- readRDS("data_map6")
countries <- readRDS("countries")


# data object as a one matrix
data1 <- list()
for (i in 1:length(data)){
  data1[[i]] <- cbind(data[[i]], country=countries$country_names[i])
}

names(data1) <- countries$country_names

data_total <- do.call("rbind",data1)
colnames(data_total) <- c("H","T","R","V","G","O","A","P","M","B",
                          "country")


rm(data)
rm(data1)
rm(i)


# variables dataframe

var_code <- c("V10","V24","V69","V85","V152",
              "V203","V204","V211","Y002","Y003")

var_short <- c("H","T","R","V","G","O","A","P","M","B")

var_names <- c("level of happiness",
               "trust in people",
               "respect for authority",
               "voice through petitions",
               "importance of God",
               "justification of homosexuality",
               "justification of abortion",
               "national pride",
               "post-materialism",
               "obedience vs independence")

var_df <- data.frame(var_code, var_short, var_names)


rm(var_code)
rm(var_short)
rm(var_names)


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

library(tidyverse) # required if you didn't load before
library(ggplot2)   # required if you didn't load before
library(ggrepel)   # required if you didn't load before
  

#--------------------------- Distributions

# level of happiness
Happiness_plot <- ggplot(data_total, aes(x=H)) + 
  geom_histogram(binwidth=1,fill="#0033CC") + 
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        line = element_blank()); Happiness_plot

# trust in people
Trust_plot <- ggplot(data_total, aes(x=T)) + 
  geom_histogram(binwidth=1,fill="#0033CC") + 
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        line = element_blank()); Trust_plot

# respect for authority 
Respect_plot <- ggplot(data_total, aes(x=R)) + 
  geom_histogram(binwidth=1,fill="#0033CC") + 
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        line = element_blank()); Respect_plot

# voice through petitions
Voice_plot <- ggplot(data_total, aes(x=V)) + 
  geom_histogram(binwidth=1,fill="#0033CC") + 
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        line = element_blank()); Voice_plot

# importance of God
God_plot <- ggplot(data_total, aes(x=G)) + 
  geom_histogram(binwidth=1,fill="#0033CC") + 
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        line = element_blank()); God_plot

# justification of homosexuality
Homosexuality_plot <- ggplot(data_total, aes(x=O)) + 
  geom_histogram(binwidth=1,fill="#0033CC") + 
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        line = element_blank()); Homosexuality_plot

# justification of abortion
Abortion_plot <- ggplot(data_total, aes(x=A)) + 
  geom_histogram(binwidth=1,fill="#0033CC") + 
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        line = element_blank()); Abortion_plot

# national pride
Nationality_plot <- ggplot(data_total, aes(x=P)) + 
  geom_histogram(binwidth=1,fill="#0033CC") + 
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        line = element_blank()); Nationality_plot

# post-materialism
Materialism_plot <- ggplot(data_total, aes(x=M)) + 
  geom_histogram(binwidth=1,fill="#0033CC") + 
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        line = element_blank()); Materialism_plot

# obedience vs independence
Autonomy_plot <- ggplot(data_total, aes(x=B)) + 
  geom_histogram(binwidth=1,fill="#0033CC") + 
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        line = element_blank()); Autonomy_plot



#--------------------------- Mean

data_map6_1 <- as.data.frame(data_map6)

# global average value for each variable 
means <- data_map6_1 %>%
  summarise_all("mean"); round(means,3)

# minimum average value for each variable 
min_means <- data_map6_1 %>%
  summarise_all("min"); round(min_means,3)

# maximum average value for each variable 
max_means <- data_map6_1 %>%
  summarise_all("max"); round(max_means,3)

# countries with the minimum average value for each variable
countries_min_means <- NULL
for (i in 1:dim(min_means)[2]){
  countries_min_means <- c(countries_min_means,
                           row.names(data_map6_1)[data_map6_1[,i]==min_means[1,i]])
}; countries_min_means

# countries with the maximum average value for each variable
countries_max_means <- NULL
for (i in 1:dim(max_means)[2]){
  countries_max_means <- c(countries_max_means,
                           row.names(data_map6_1)[data_map6_1[,i]==max_means[1,i]])
}; countries_max_means

# mean dataframe
min_max_means <- data.frame("mean" = t(as.matrix(means)),
                            "mean_min" = t(as.matrix(min_means)),
                            "mean_max" = t(as.matrix(max_means))); min_max_means

min_max_means1 <- data.frame("mean" = t(as.matrix(means)),
                             "country_min" = countries_min_means,
                             "mean_min" = t(as.matrix(min_means)),
                             "country_max" = countries_max_means,
                             "mean_max" = t(as.matrix(max_means))); min_max_means1



#--------------------------- Missings

# missings for each variable by country
missings <- data_total %>%
  group_by(country) %>%
  summarise(H = sum(is.na(H)/(count = n())),
            T = sum(is.na(T)/(count = n())),
            R = sum(is.na(R)/(count = n())),
            V = sum(is.na(V)/(count = n())),
            G = sum(is.na(G)/(count = n())),
            O = sum(is.na(O)/(count = n())),
            A = sum(is.na(A)/(count = n())),
            P = sum(is.na(P)/(count = n())),
            M = sum(is.na(M)/(count = n())),
            B = sum(is.na(B)/(count = n()))); missings

missings <- as.data.frame(missings)
  rownames(missings) <- missings$country
    missings <- missings[,-1]

# global missings for each variable
total_missings <- data_total[,-11] %>%
  summarise(H = sum(is.na(H)/(count = n())),
            T = sum(is.na(T)/(count = n())),
            R = sum(is.na(R)/(count = n())),
            V = sum(is.na(V)/(count = n())),
            G = sum(is.na(G)/(count = n())),
            O = sum(is.na(O)/(count = n())),
            A = sum(is.na(A)/(count = n())),
            P = sum(is.na(P)/(count = n())),
            M = sum(is.na(M)/(count = n())),
            B = sum(is.na(B)/(count = n()))); round(total_missings*100,1)

# minimum number of missings for each variable 
min_missings <- missings %>%
  summarise_all("min"); round(min_missings*100,1)

# maximum number of missings for each variable 
max_missings <- missings %>%
  summarise_all("max"); round(max_missings*100,1)

# countries with the minimum number of missings for each variable
countries_min_missings <- list()
for (i in 1:dim(min_missings)[2]){
  countries_min_missings[[i]] <- row.names(missings)[missings[,i]==min_missings[1,i]]
}; countries_min_missings  
names(countries_min_missings) <- names(min_missings)

# countries with the maximum number of missings for each variable
countries_max_missings <- list()
for (i in 1:dim(max_missings)[2]){
  countries_max_missings[[i]] <- row.names(missings)[missings[,i]==max_missings[1,i]]
}; countries_max_missings
names(countries_max_missings) <- names(max_missings)


# missing dataframe
min_max_missings <- data.frame("missing" = t(as.matrix(total_missings))*100,
                               "missing_min" = t(as.matrix(min_missings))*100,
                               "missing_max" = t(as.matrix(max_missings))*100); min_max_missings
min_max_missings1 <- data.frame("missing" = t(as.matrix(total_missings))*100,
                                #"country_min" = countries_min_missings, # multiple names
                                "missing_min" = t(as.matrix(min_missings))*100,
                                "country_max" = countries_max_missings,
                                "missing_max" = t(as.matrix(max_missings))*100); min_max_missings1



#--------------------------- Table 1

var_df <- cbind(var_df, min_max_means, min_max_missings)
View(var_df)

rm(list=ls()[! ls() %in% c("Happiness_plot",
                           "Trust_plot",
                           "Respect_plot",
                           "Voice_plot",
                           "God_plot",
                           "Homosexuality_plot",
                           "Abortion_plot",
                           "Nationality_plot",
                           "Materialism_plot",
                           "Autonomy_plot",
                           "var_df")])
