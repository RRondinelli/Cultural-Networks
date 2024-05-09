## README

Repository for the paper "Luca De Benedictis, Roberto Rondinelli, Veronica Vinciotti, Cultures as networks of cultural traits: a unifying framework for measuring culture and cultural distances. Journal of the Royal Statistical Society Series A: Statistics in Society. 2023. https://doi.org/10.1093/jrsssa/qnac007"

The R objects contained in the R Project allow each script to be run individually, i.e., although there is a chronological order of the scripts (given by the number of each script), you do not need to run the previous ones in order to obtain a specific output.

You can use our elaborations of the graphical models from BDgraph to compute the cultural distances and to visualize the countries' cultural networks. Otherwise you need to follow and run the first part of the code contained in "01.BDgraph_infer-networks.R" script to generate your BDgraph objects.


--------------------------------------------------------------------------------

Note: If you want to apply the code to another set of data, please make sure your set of data is organized in a list, where each element - representing a country - is a data frame with the same variables. Save your list as a RDS file. BDgraph algorithm works also in presence of missing data. Furthermore, in this repository you may find also the object "Wave6.rds", where the original data are stored.  

------------------------------

Note: the object "pca_map6" contains the results of the reproduced Inglehart–Welzel Cultural Map (Figure 2).

------------------------------

Note: file "distances_WVS-W6.csv" contains the cultural distances for each pair of countries denoted by their extended names and by their iso3 international code. This file has been obtained from our BDgraph elaborations and makes cultural distances available for different elements:
- each marginal distribution of the considered cultural traits defined in Table 1;
- "JD marginals", the first additive component of the "JD index" derived in Equation 7;
- "JD network", the second additive component of the "JD index" derived in Equation 7;
- "JD index" derived in Equation 7;
- "Mean diff", the Euclidean norm of the differences between the vector of means of the cultural traits;
- "IW index", an alternative measure of cultural distances describing the Euclidean distances between cultural traits in the reproduced Inglehart–Welzel Cultural Map (Figure 2).

------------------------------

Note: The text font we made use for the Figures of the paper is "LM Roman 10". Here, to avoid problems with the R compilation of the code under different Operating System, we consider the default text font of R.

--------------------------------------------------------------------------------

