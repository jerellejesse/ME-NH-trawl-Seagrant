#Seagrant ME-NH trawl 
#objective 2- anosim and adonis analysis
#J.Jesse
#7/14/21

#libraries
#load packages
library(tidyverse)
library(vegan)
library(pairwiseAdonis)
library(indicspecies)
library(here)

#bring in species matrix for analysis
trawl_data_arrange<-read.csv(here("Data/species_biomass_matrix.csv"))[-1]

#separate meta data from matrix
ME_group_data<-trawl_data_arrange[, c(1,2,3,55,56,57,58)]
ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:53])

#calculate dissimilarity matrix for tests
trawl_dist<-vegdist(ME_NMDS_data,distance="bray")

##### Analysis of similarity test #####
#tests statistically whether there is a significant difference between two or more groups of sampling units
#Works by testing if distances between groups are greater than within groups
#operates on dissimilarity matrix, so pairs well with nMDS
#significant values mean that there is a statistically significant difference in the communities between the groups
#R statistic closer to 1 is more dissimilar
#assumes ranges of dissimilarities within groups are equal- how to check dispersion of groups?

#region
ano_region<- anosim(trawl_dist, trawl_data_arrange$Region, permutations = 9999)
ano_region #regions are statistically different communities 
summary(ano_region)
plot(ano_region) #regions don't look very different in plot though...confidence bands all overlap

#Time
ano_year<- anosim(trawl_dist, trawl_data_arrange$Year, permutations = 9999)
ano_year #years are statistically different communities
summary(ano_year)
plot(ano_year)


#####analysis of variance test#####
#tests whether there is a difference between groups
#works by calculating the sum of squares from the centroid of the group
#like anosim, also sensitive to heterogeneity of groups, so you can do the betadisp first to check
#significant anosim and adonis results can be due to variation associated with groups 
#by terms which is sequential or margin which will tell you the margin effect of adding another term

adonis<-adonis2(trawl_dist~Region*Year, data=ME_group_data, by="terms", permutations = 9999)
adonis
summary(adonis)

#pair-wise test to see what is different
pair<-pairwise.adonis2(trawl_dist~Region, data=ME_group_data, by="terms", permutations = 9999)
summary(pair)
pair #shows all the regions are significantly different except 3 and 4



##### Assumptions #####
#betadisper test homogeneity of dispersion among groups
bd<-betadisper(trawl_dist,ME_group_data$Region) #change to Region or Year
bd
anova(bd) #variance is different within years which violates anosim and adonis assumptions
plot(bd)  #Region seems to pass

#test based on permutations
permutest(bd)

#balanced design?
table(ME_group_data$Region,ME_group_data$Year)


#### Indicator species analysis #####
#tests which species are found more often in one group compared to another
library(indicspecies)

#see which species are found significantly more in each Region
inv_region<-multipatt(ME_NMDS_data, ME_group_data$Region, func = "r.g", control = how(nperm=999))
summary(inv_region)


#see which species are found significantly more in each Year
inv_year<-multipatt(ME_NMDS_data, ME_group_data$Year, func = "r.g", control = how(nperm=999))
summary(inv_year)



