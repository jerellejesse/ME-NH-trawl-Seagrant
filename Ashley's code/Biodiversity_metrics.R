####################################################################################
### Biodiversity metrics to be applied to survey data
### UM Seagrant; climate impacts on Maine coastal fisheries using the ME-NH inshore trawl
### Objective 1 part 1
### 6/4/2018 
### A.E. Weston
####################################################################################

# AEW may want to calculate metrics by classification higher than spp level

### Notes;
### Latest report (2015);
### https://www.maine.gov/dmr/science-research/projects/trawlsurvey/reports/documents/2015.pdf
### survey is all years for fall and then all years for spring 
### no spring 2000 observations (starts fall 2000)
### did not start sampling depth 4 until 2003

#setwd("J:/Research/Kerr Lab/ME_SEAG_2018/ME NH trawl data")
setwd("C:/Users/jjesse/Box/Kerr Lab/Fisheries Science Lab/ME NH Trawl- Seagrant/Seagrant-AEW/ME NH data for GMRI")
trawl <- read.csv("EXPCATCH_forGMRI.csv", header = TRUE) #ME-NH trawl survey catch data



# first drop samples that are not to the species level
trawl_2 <- trawl[!trawl$SCIENTIFIC_NAME == "Anemonia" ,] #anemone
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Pandalus" ,]  #shrimp  #ID to spp also
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Stelleroidea",] #sea stars
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Octopoda",] #octopus
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Clypeasteroida",] # sand dollar
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Yoldia",] #clams
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Calcarea",] #sponge
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Majidae",] #spider crab
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Balanus",] #barnacle
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Stomatopoda",]  #mantis shrimp  #low
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Euphausiacea",] #krill
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Paguroidea",] # hermit crab
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "",]
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Mysidacea",] # mysid shrimp
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Diaphus",]      #lantern fish  #low
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Sepiolidae",] #bobtail squid
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Artediellus",]   #pacific sculpin #low
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Macrouridae",]  # deep sea rattial/ grenadiers  #low
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Paralepididae",] #barracudinas #low
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Clupeidae",]    #herring  #low
trawl_2 <- trawl_2[!trawl_2$SCIENTIFIC_NAME == "Myctophidae",]  #lanternfish  #low

## there are 331 occurances where W_NUM = NA; remove these #AEW check into this (report)
trawl_2 <- trawl_2[!is.na(trawl_2$W_NUM),]
## for some reason there are 38 observations of W_NUM = 0; remove these 
trawl_2 <- trawl_2[!trawl_2$W_NUM == 0,]

# create a new indicator that is year/season/haul
trawl_2$GMRI_INDICATOR <-  cumsum(!duplicated(trawl_2[1:3])) 

#write.csv(trawl_2, "trawl_2.csv")

# setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/ID grouping")
# library(dplyr)
# 
# trophic_groups<-read.csv("trophicgrps.csv")
# trophic_groups<-rename(trophic_groups, SCIENTIFIC_NAME=Scientfic.name, functional_group=fhdbs.functional.group)
# trawl_2_groups<-left_join(trawl_2, trophic_groups, by="SCIENTIFIC_NAME")
#   #not all species are included in a functional group
#   #mostly invertebrates that are not classified
# trawl_2_groups$GMRI_INDICATOR <-  cumsum(!duplicated(group_by(trawl_2_groups, DMR_TRIP_IDENTIFIER, DMR_EFFORT_IDENTIFIER, functional_group)))
# 
 trawl_2<-group_by(trawl_2, DMR_TRIP_IDENTIFIER, DMR_EFFORT_IDENTIFIER)%>%
   mutate(INDICATOR=cur_group_id())

## calculate species richness, Shannon-Weiner diversity index, Simpson's diversity index, and Simpson's evenness index by haul
diff_hauls <- matrix(NA)
N_species <-NULL
N_each_spp <-NULL
total_spp <- NULL
spp_prop <- NULL
H_index <- NULL  
D_index <- NULL 
E_index <- NULL 
hauls <- unique(trawl_2$INDICATOR) # haul within each unique year/season 
N_hauls <- length(hauls) # number of hauls
#N_functional_groups<-length(unique(trawl_2_groups$functional_group))


  for (i in 1:N_hauls) { #loop through each haul within year/season
  diff_hauls <- trawl_2[which(trawl_2$INDICATOR==i),] #subset unique hauls
  N_species[i] <- length(unique(diff_hauls$SCIENTIFIC_NAME))# count the number of unique species
  N_each_spp <- diff_hauls$W_NUM
  total_spp <- sum(N_each_spp)
  spp_prop <- N_each_spp/total_spp # cacluate proportion of each species in a haul
  #spp <- as.vector(diff_hauls$SCIENTIFIC_NAME) # vector of species
  H_index[i] <- -1*(sum(spp_prop*log(spp_prop))) # multiplied by -1 to get non-negative values
  D_index[i] <- 1/(sum(spp_prop^2)) 
  E_index[i] <- D_index[i]*(1/N_species[i]) 
  }


print(N_species)#species richness for each haul
print(H_index)  #Shannon-Weiner index values for each haul
print(D_index)  #Simpsons diversity index values for each haul
print(E_index)  #Simpsons evenness index for each haul (0-1, 1 = complete evenness)



# add indices to haul by haul information file
setwd("C:/Users/jjesse/Box/Kerr Lab/Fisheries Science Lab/ME NH Trawl- Seagrant/Seagrant-AEW/ME NH data for GMRI")
tow <- read.csv("TowInformation.csv")
tow <- tow[-3419,] # remove blank row for spring 2018
indices <- bind_cols(N_species, H_index, D_index, E_index)
ind_by_haul <- cbind(tow, indices)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
write.csv(ind_by_haul, "ind_by_haul.csv")


trawl_3<-group_by(trawl_2, DMR_TRIP_IDENTIFIER, DMR_EFFORT_IDENTIFIER)%>%
  summarise()
ind_by_haul<-bind_cols(trawl_3, indices)%>%
  rename(N_species=...4, H_index=...5, D_index=...6, E_index=...7)
  



#################Taxomonic diversity version 2 made to be more generalizable
############## taxonomic diversity
## need to assign weight for each species combination based on Linnaean classification
## same species; w = 0
## same genus; w = 1
## same family, different genera; w = 2
## same order, different family; w = 3
## same class, different order; w = 4
## same phyla, different class; w = 5
## different phyla; w = 6



#build out survey matrix with species, genus, family, order, class, phylum for each observation
library(taxize)
library(purrr)
library(mgcv)


sci_name <- as.vector(trawl_2$SCIENTIFIC_NAME)  
diff_sci_name <- unique(sci_name) # all unique species names 
tax <- classification(diff_sci_name, db = 'itis') #get classification information for all species observed  

# save taxonomic information from P,C,O,F,G,S levels for possible species 
info <- matrix(NA)
expand <- matrix(NA)
specific <- matrix(NA, nrow = length(diff_sci_name), ncol = 6)
for (i in 1:length(tax)){
  info <- tax[[i]][c('name','rank')] # gives classification and rank for each species
  expand <- info[info$rank == 'phylum'| info$rank == 'class'| info$rank == 'order' | info$rank == 'family' | info$rank == 'genus' | info$rank == 'species',]
  specific[i,] <- as.vector(expand$name)
}
colnames(specific) <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
write.csv(specific, "phylo.csv")
phylo<-read.csv("phylo.csv")

# loop through trawl survey to expand each sample based on above taxonomic information
#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
setwd("C:/Users/jjesse/Box/Kerr Lab/Fisheries Science Lab/ME NH Trawl- Seagrant/Seagrant-AEW/ME NH data for GMRI")

trawl_2 <- read.csv("trawl_2.csv", header = TRUE)
trawl_2 <- trawl_2[,-1]
phylo <- read.csv("phylo.csv", header = TRUE)
phylo <- as.data.frame(phylo)[-1]
sci_name <- as.vector(trawl_2$SCIENTIFIC_NAME)  
expand_all <- matrix(NA, nrow = nrow(trawl_2), ncol = 6)
for (i in 1:nrow(trawl_2)) {
  expand_all[i,] <- as.matrix(phylo[phylo[,"Species"] == sci_name[i],]) 
}

colnames(expand_all) <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")

write.csv(expand_all, "expanded_phylo.csv", row.names = FALSE) #save expanded survey phylogenetic information

# save whole new expanded trawl
trawl_3 <- cbind(trawl_2, expand_all)
write.csv(trawl_3, "trawl_phylo.csv", row.names = FALSE)





####################### now that survey data is reformatted calculate taxonomic diversity indices
#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
setwd("C:/Users/jjesse/Box/Kerr Lab/Fisheries Science Lab/ME NH Trawl- Seagrant/Seagrant-AEW/ME NH data for GMRI")

ext_trawl <- read.csv("trawl_phylo.csv", header = TRUE)

ext_trawl2<-group_by(ext_trawl, DMR_TRIP_IDENTIFIER, DMR_EFFORT_IDENTIFIER)%>%
  mutate(INDICATOR=cur_group_id())
library(mgcv)

hauls <- unique(ext_trawl2$INDICATOR)   
N_hauls <- length(hauls) # number of hauls
N_species <- NULL #N species
sub_species <- NULL # N_species-1
total <- NULL  #xixj
numerator <- NULL #wijxixj
x_y <- matrix(NA, nrow = 6, ncol = 6) 
x <- NULL
y <- NULL
ident <- NULL
weight <- NULL #wij
count <- NULL
total_weight <- NULL
mean_weight <- NULL
weight_var <- NULL
delta <- NULL
delta_star <- NULL
delta_plus <- NULL
delta_var <- NULL
weight_var <- NULL


for (j in 1:N_hauls) {  
diff_hauls <- ext_trawl[which(ext_trawl$GMRI_INDICATOR == j),] #subset unique hauls/functional groups
N_species[j] <- length(unique(diff_hauls$Species))# count the number of unique species in each haul (denominator)
sub_species[j] <- N_species[j]-1
diff <- unique(as.vector(diff_hauls$Species)) # name of each unique species 
combos <- combn(diff, 2) # create combinations of each species/haul (for weight calc)

phylo <- as.matrix(subset(diff_hauls, select = c(Phylum,Class,Order,Family,Genus, Species))) # extract phylogenetic information only
unique_phylo <- uniquecombs(phylo) # subset by unique species information
unique_phylo <- as.data.frame(unique_phylo)

total <- NULL  # reset the length for each haul because they will be different
weight <- NULL # reset  

for (i in 1:ncol(combos)) { # for each unique combination count the number of each species 
  #total[i] <- sum(diff_hauls$Species == combos[1,i]) * sum(diff_hauls$Species == combos[2,i]) #empty vector is always length 210
  total[i] <- diff_hauls[diff_hauls[,22] == combos[1,i],9] * diff_hauls[diff_hauls[,22] == combos[2,i],9]
  x <- unique_phylo[unique_phylo$Species == combos[1,i],]
  y <- unique_phylo[unique_phylo$Species == combos[2,i],]
  x_y <- rbind(x,y)
  
  for (k in 1:ncol(x_y)){ # for each combination calculate the weight value 
    ident[k] <- identical(as.vector(x_y[1,k]), as.vector(x_y[2,k])) # determine how much of phylogenetic information is the same
    weight[i] <- sum(ident == "FALSE") # vector of weights
    #mean_weight[i] <- mean(weight) #rep(mean(weight),length(weight))
    numerator[j] <- sum(total*weight) 
    count[j] <- sum(total)
    mean_weight[j] <- mean(weight)
    total_weight[j] <- sum(weight)
    weight_var[j] <- sum((weight- mean(weight))^2) 
  }
  delta <- (2*numerator)/(N_species*sub_species)
  delta_star <- numerator/(count)
  delta_plus <- (2*total_weight)/(N_species*sub_species)
  delta_var <- (2*weight_var)/(N_species*sub_species) #double check that this equation is correct
}
}
print(delta) # taxonomic diversity
print(delta_star) # taxonomic distinctness
print(delta_plus) # average taxonomic distinctness
print(delta_var) # variation in taxonomic distinctness 



#pull information back together by haul 
tow <- read.csv("ind_by_haul.csv")
tax_indices <- cbind(delta, delta_star, delta_plus, delta_var)
write.csv(tax_indices, "tax_indices.csv")
ind_by_haul <- cbind(tow, tax_indices)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
write.csv(ind_by_haul, "diversity_ind_by_haul.csv")








############################### Visualizing metrics #######################################
#setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/Results")
setwd("C:/Users/jjesse/Box/Kerr Lab/Fisheries Science Lab/ME NH Trawl- Seagrant/Seagrant-AEW/ME NH data for GMRI")
ind_by_haul <- read.csv("diversity_ind_by_haul.csv")[-1]

ind_by_haul2 <- tidyr::separate(ind_by_haul,DMR_TRIP_IDENTIFIER, into=c("SEASON", "YEAR"), sep=2)
## aggregate over years 
fall_inds <- subset(ind_by_haul2, SEASON == "FL")
library('matrixStats')

diff_year <- matrix(NA) 
N_sample <- NULL
start <- 00
end <- 17
yr <- seq(0, 17, by = 1)
avg_N_spp <- NULL
avg_H_ind <- NULL
avg_D_ind <- NULL
avg_E_ind <- NULL
avg_delta <- NULL
avg_delta_plus <- NULL
avg_delta_star <- NULL
avg_delta_var <- NULL
N_quants <- matrix(NA, nrow = length(yr),ncol = 2)
H_quants <- matrix(NA, nrow = length(yr),ncol = 2)
D_quants <- matrix(NA, nrow = length(yr),ncol = 2)
E_quants <- matrix(NA, nrow = length(yr),ncol = 2)
delt_quants <- matrix(NA, nrow = length(yr),ncol = 2)
deltp_quants <- matrix(NA, nrow = length(yr),ncol = 2)
delts_quants <- matrix(NA, nrow = length(yr),ncol = 2)
deltav_quants <- matrix(NA, nrow = length(yr),ncol = 2)




for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year <- fall_inds[which(fall_inds$YEAR == yr[i]),] #subset unique hauls

  avg_N_spp[i] <- mean(diff_year$N_species)
  N_quants [i,] <- quantile(diff_year$N_species, probs = c(0.025,0.975))
  avg_H_ind[i] <- mean(diff_year$H_index)
  H_quants [i,] <- quantile(diff_year$H_index, probs = c(0.025,0.975))
  avg_D_ind[i] <- mean(diff_year$D_index)
  D_quants [i,] <- quantile(diff_year$D_index, probs = c(0.025,0.975))
  avg_E_ind[i] <- mean(diff_year$E_index)
  E_quants [i,] <- quantile(diff_year$E_index, probs = c(0.025,0.975))
  avg_delta[i] <- mean(diff_year$delta)
  delt_quants [i,] <- quantile(diff_year$delta, probs = c(0.025,0.975))
  avg_delta_plus[i] <- mean(diff_year$delta_plus)
  deltp_quants [i,] <- quantile(diff_year$delta_plus, probs = c(0.025,0.975))
  avg_delta_star[i] <- mean(diff_year$delta_star)
  delts_quants [i,] <- quantile(diff_year$delta_star, probs = c(0.025,0.975))
  avg_delta_var[i] <- mean(diff_year$delta_var)
  deltav_quants [i,] <- quantile(diff_year$delta_var, probs = c(0.025,0.975))
}


## spring  
spring_inds <- subset(ind_by_haul2, SEASON == "SP")

diff_year_2 <- matrix(NA) 
N_sample_2 <- NULL
start <- 00
end <- 17
yr <- seq(0, 17, by = 1)
avg_N_spp_2 <- NULL
avg_H_ind_2 <- NULL
avg_D_ind_2 <- NULL
avg_E_ind_2 <- NULL
avg_delta_2 <- NULL
avg_delta_plus_2 <- NULL
avg_delta_star_2 <- NULL
avg_delta_var_2 <- NULL
N_quants_2 <- matrix(NA, nrow = length(yr),ncol = 2)
H_quants_2 <- matrix(NA, nrow = length(yr),ncol = 2)
D_quants_2 <- matrix(NA, nrow = length(yr),ncol = 2)
E_quants_2 <- matrix(NA, nrow = length(yr),ncol = 2)
delt_quants_2 <- matrix(NA, nrow = length(yr),ncol = 2)
deltp_quants_2 <- matrix(NA, nrow = length(yr),ncol = 2)
delts_quants_2 <- matrix(NA, nrow = length(yr),ncol = 2)
deltav_quants_2 <- matrix(NA, nrow = length(yr),ncol = 2)

for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year_2 <- spring_inds[which(spring_inds$YEAR == yr[i]),] #subset unique hauls

  avg_N_spp_2[i] <- mean(diff_year_2$N_species)
  N_quants_2 [i,] <- quantile(diff_year_2$N_species, probs = c(0.025,0.975))
  avg_H_ind_2[i] <- mean(diff_year_2$H_index)
  H_quants_2 [i,] <- quantile(diff_year_2$H_index, probs = c(0.025,0.975))
  avg_D_ind_2[i] <- mean(diff_year_2$D_index)
  D_quants_2 [i,] <- quantile(diff_year_2$D_index, probs = c(0.025,0.975))
  avg_E_ind_2[i] <- mean(diff_year_2$E_index)
  E_quants_2 [i,] <- quantile(diff_year_2$E_index, probs = c(0.025,0.975))
  avg_delta_2[i] <- mean(diff_year_2$delta)
  delt_quants_2 [i,] <- quantile(diff_year_2$delta, probs = c(0.025,0.975))
  avg_delta_plus_2[i] <- mean(diff_year_2$delta_plus)
  deltp_quants_2 [i,] <- quantile(diff_year_2$delta_plus, probs = c(0.025,0.975))
  avg_delta_star_2[i] <- mean(diff_year_2$delta_star)
  delts_quants_2 [i,] <- quantile(diff_year_2$delta_star, probs = c(0.025,0.975))
  avg_delta_var_2[i] <- mean(diff_year_2$delta_var)
  deltav_quants_2 [i,] <- quantile(diff_year_2$delta_var, probs = c(0.025,0.975))
}


library(RColorBrewer)
cols <- brewer.pal(6, "Blues")

layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
layout.show(8)
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
label <- seq(2000, 2017)
label <- as.character(label)

plot(avg_N_spp, type = 'l', xlab = '', ylab = "Number of Species", main = "Average Fall Indices"
     , ylim = c(0, 40), col = cols[5], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(N_quants[,1], rev(N_quants[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_H_ind, type = 'l', xlab = '', ylab = "Shannon-Weiner Index", ylim = c(0,3), col = cols[5], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(H_quants[,1], rev(H_quants[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_D_ind, type = 'l', xlab = '', ylab = "Simpson Diversity Index", ylim = c(0,8), col = cols[5], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(D_quants[,1], rev(D_quants[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_E_ind, type = 'l', xlab = "Survey Year", ylab = "Simpson Evenness Index", ylim = c(0, 0.6), col = cols[5], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(E_quants[,1], rev(E_quants[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_N_spp_2, type = 'l', xlab = '', ylab = '', main = "Average Spring Indices", ylim = c(0, 40), col = cols[5], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(N_quants_2[,1], rev(N_quants_2[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_H_ind_2, type = 'l', xlab = '', ylab = '', ylim = c(0,3), col = cols[5], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(H_quants_2[,1], rev(H_quants_2[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_D_ind_2, type = 'l', xlab = '', ylab = '', ylim = c(0,8), col = cols[5], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(D_quants_2[,1], rev(D_quants_2[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_E_ind_2, type = 'l', xlab = "Survey Year", ylab = '', ylim = c(0,0.6), col = cols[5], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(E_quants_2[,1], rev(E_quants_2[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)



layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R

plot(avg_delta, type = 'l', xlab = '', ylab = "Taxonomic Diversity", main = "Average Fall Indices" 
     , col = cols[5], ylim = c(0, 1700000), xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(delt_quants[,1], rev(delt_quants[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_delta_star, type = 'l', xlab = '', ylab = "Taxonomic Distinctness", ylim = c(2,7), col = cols[5], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(delts_quants[,1], rev(delts_quants[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_delta_plus, type = 'l', xlab = '', ylab = "Average Taxonomic Distinctness", col = cols[5], ylim = c(3,5.5), xaxt = 'n') 
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(deltp_quants[,1], rev(deltp_quants[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_delta_var, type = 'l', xlab = "Survey Year", ylab = "Variation in Taxonomic Diversity", ylim = c(0.5, 2.2), col = cols[5], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(deltav_quants[,1], rev(deltav_quants[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_delta_2, type = 'l', xlab = '', ylab = '', main = "Average Spring Indices", col = cols[5], ylim = c(0, 1700000), xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(delt_quants_2[,1], rev(delt_quants_2[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_delta_star_2, type = 'l', xlab = '', ylab = '', col = cols[5], ylim = c(2,7), xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(delts_quants_2[,1], rev(delts_quants_2[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_delta_plus_2, type = 'l', xlab = '', ylab = '', col = cols[5], ylim = c(3,5.5), xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(deltp_quants_2[,1], rev(deltp_quants_2[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)


plot(avg_delta_var_2, type = 'l', xlab = "Survey Year", ylab = '', col = cols[5], ylim = c(0.5,2.2), xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
polygon(c(seq(1:18), rev(seq(1:18))), c(deltav_quants_2[,1], rev(deltav_quants_2[,2])), col = adjustcolor("gray45", alpha.f = 0.10), border = NA)



### aggregating average indices by region over time 
#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
setwd("C:/Users/jjesse/Box/Kerr Lab/Fisheries Science Lab/ME NH Trawl- Seagrant/Seagrant-AEW/ME NH data for GMRI")
ind_by_haul <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
ind_by_haul2 <- tidyr::separate(ind_by_haul,DMR_TRIP_IDENTIFIER, into=c("SEASON", "YEAR"), sep=2)

library(tidyr)
#ind_by_haul_2 <- separate(ind_by_haul, EFFORT_START_DATE, c("month", "day", "year"))

#fall
fall_inds <- subset(ind_by_haul2, SEASON == "FL")

diff_year <- matrix(NA) 
N_sample <- NULL
start <- 00
end <- 17
yr <- seq(0, 17, by = 1)
region <- unique(ind_by_haul$REGION)
N_regions <- length(region) #5 regions
diff_region <- matrix(NA)
avg_N_spp <- NULL
avg_H_ind <- NULL
avg_D_ind <- NULL
avg_E_ind <- NULL
reg_N <- matrix(NA, nrow = length(yr), ncol = length(region))
reg_H <- matrix(NA, nrow = length(yr), ncol = length(region))
reg_D <- matrix(NA, nrow = length(yr), ncol = length(region))
reg_E <- matrix(NA, nrow = length(yr), ncol = length(region))


for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year <- fall_inds[which(fall_inds$YEAR == yr[i]),] #subset unique hauls

  for (j in 1:N_regions) {  
    diff_region <- diff_year[which(diff_year$REGION == j),] 
    avg_N_spp[j] <- mean(diff_region$N_species)
    avg_H_ind[j] <- mean(diff_region$H_index)
    avg_D_ind[j] <- mean(diff_region$D_index)
    avg_E_ind[j] <- mean(diff_region$E_index)
  }
  reg_N[i,] <- avg_N_spp
  reg_H[i,] <- avg_H_ind
  reg_D[i,] <- avg_D_ind
  reg_E[i,] <- avg_E_ind
}


## spring  
spring_inds <- subset(ind_by_haul2, SEASON == "SP")

diff_year_2 <- matrix(NA) 
N_sample_2 <- NULL
start <- 00
end <- 17
yr <- seq(0, 17, by = 1)
region <- unique(ind_by_haul$REGION)
N_regions <- length(region) #5 regions
diff_region_2 <- matrix(NA)
avg_N_spp_2 <- NULL
avg_H_ind_2 <- NULL
avg_D_ind_2 <- NULL
avg_E_ind_2 <- NULL
reg_N_2 <- matrix(NA, nrow = length(yr), ncol = length(region))
reg_H_2 <- matrix(NA, nrow = length(yr), ncol = length(region))
reg_D_2 <- matrix(NA, nrow = length(yr), ncol = length(region))
reg_E_2 <- matrix(NA, nrow = length(yr), ncol = length(region))

for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year_2 <- spring_inds[which(spring_inds$YEAR == yr[i]),] #subset unique hauls
  
  for (j in 1:N_regions) {  
    diff_region_2 <- diff_year_2[which(diff_year_2$REGION == j),] 
    avg_N_spp_2[j] <- mean(diff_region_2$N_species)
    avg_H_ind_2[j] <- mean(diff_region_2$H_index)
    avg_D_ind_2[j] <- mean(diff_region_2$D_index)
    avg_E_ind_2[j] <- mean(diff_region_2$E_index)
  }
  reg_N_2[i,] <- avg_N_spp_2
  reg_H_2[i,] <- avg_H_ind_2
  reg_D_2[i,] <- avg_D_ind_2
  reg_E_2[i,] <- avg_E_ind_2
}

library(RColorBrewer)
cols <- brewer.pal(6, "Blues")

layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
layout.show(8)
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
label <- seq(2000, 2017)
label <- as.character(label)

plot(reg_N[,1], type = 'l', xlab = '', ylab = "Number of Species", main = "Average Fall Indices by Region"
     , ylim = c(16, 26), col = cols[2], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_N[,2], type = 'l', col = cols[3])
lines(reg_N[,3], type = 'l', col = cols[4])
lines(reg_N[,4], type = 'l', col = cols[5])
lines(reg_N[,5], type = 'l', col = cols[6])


plot(reg_H[,1], type = 'l', xlab = '', ylab = "Shannon-Weiner Index", ylim = c(0.8,2), col = cols[2], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_H[,2], type = 'l', col = cols[3])
lines(reg_H[,3], type = 'l', col = cols[4])
lines(reg_H[,4], type = 'l', col = cols[5])
lines(reg_H[,5], type = 'l', col = cols[6])

plot(reg_D[,1], type = 'l', xlab = '', ylab = "Simpson Diversity Index", ylim = c(1.8,5), col = cols[2], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_D[,2], type = 'l', col = cols[3])
lines(reg_D[,3], type = 'l', col = cols[4])
lines(reg_D[,4], type = 'l', col = cols[5])
lines(reg_D[,5], type = 'l', col = cols[6])

plot(reg_E[,1], type = 'l', xlab = "Survey Year", ylab = "Simpson Evenness Index", ylim = c(0.05,0.26), col = cols[2], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_E[,2], type = 'l', col = cols[3])
lines(reg_E[,3], type = 'l', col = cols[4])
lines(reg_E[,4], type = 'l', col = cols[5])
lines(reg_E[,5], type = 'l', col = cols[6])


plot(reg_N_2[,1], type = 'l', xlab = '', ylab = '', main = "Average Spring Indices by Region", ylim = c(16, 26), col = cols[2], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_N_2[,2], type = 'l', col = cols[3])
lines(reg_N_2[,3], type = 'l', col = cols[4])
lines(reg_N_2[,4], type = 'l', col = cols[5])
lines(reg_N_2[,5], type = 'l', col = cols[6])
legend('topleft', legend = c("Region 1", "Region 2", "Region 3", "Region 4", "Region 5"), col = cols[2:6], lty = 1, bty = 'n', xaxt = 'n')


plot(reg_H_2[,1], type = 'l', xlab = '', ylab = '', ylim = c(0.8,2), col = cols[2], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_H_2[,2], type = 'l', col = cols[3])
lines(reg_H_2[,3], type = 'l', col = cols[4])
lines(reg_H_2[,4], type = 'l', col = cols[5])
lines(reg_H_2[,5], type = 'l', col = cols[6])

plot(reg_D_2[,1], type = 'l', xlab = '', ylab = '', ylim = c(1.8,5), col = cols[2], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_D_2[,2], type = 'l', col = cols[3])
lines(reg_D_2[,3], type = 'l', col = cols[4])
lines(reg_D_2[,4], type = 'l', col = cols[5])
lines(reg_D_2[,5], type = 'l', col = cols[6])

plot(reg_E_2[,1], type = 'l', xlab = "Survey Year", ylab = '', ylim = c(0.05,0.26), col = cols[2], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_E_2[,2], type = 'l', col = cols[3])
lines(reg_E_2[,3], type = 'l', col = cols[4])
lines(reg_E_2[,4], type = 'l', col = cols[5])
lines(reg_E_2[,5], type = 'l', col = cols[6])



########################### aggregating average indices by stratum over time 
#setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/Results")
setwd("C:/Users/jjesse/Box/Kerr Lab/Fisheries Science Lab/ME NH Trawl- Seagrant/Seagrant-AEW/ME NH data for GMRI")


ind_by_haul <- read.csv("ind_by_haul.csv", header = TRUE)

library(tidyr)

#fall
fall_inds <- subset(ind_by_haul2, SEASON == "FL")

diff_year <- matrix(NA) 
N_sample <- NULL
start <- 00
end <- 17
yr <- seq(0, 17, by = 1)
strata <- unique(ind_by_haul$STRATUM)
N_strata <- length(strata) #4
diff_strata <- matrix(NA)
avg_N_spp <- NULL
avg_H_ind <- NULL
avg_D_ind <- NULL
avg_E_ind <- NULL
reg_N <- matrix(NA, nrow = length(yr), ncol = length(strata))
reg_H <- matrix(NA, nrow = length(yr), ncol = length(strata))
reg_D <- matrix(NA, nrow = length(yr), ncol = length(strata))
reg_E <- matrix(NA, nrow = length(yr), ncol = length(strata))


for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year <- fall_inds[which(fall_inds$YEAR == yr[i]),] #subset unique hauls
  
  for (j in 1:N_strata) {  
    diff_strata <- diff_year[which(diff_year$STRATUM == j),] 
    avg_N_spp[j] <- mean(diff_strata$N_species)
    avg_H_ind[j] <- mean(diff_strata$H_index)
    avg_D_ind[j] <- mean(diff_strata$D_index)
    avg_E_ind[j] <- mean(diff_strata$E_index)
  }
  reg_N[i,] <- avg_N_spp
  reg_H[i,] <- avg_H_ind
  reg_D[i,] <- avg_D_ind
  reg_E[i,] <- avg_E_ind
}


## spring  
spring_inds <- subset(ind_by_haul, SEASON == "SP")

diff_year_2 <- matrix(NA) 
N_sample_2 <- NULL
start <- 00
end <- 17
yr <- seq(0, 17, by = 1)
strata <- unique(ind_by_haul$STRATUM)
N_strata <- length(strata) #4
diff_strata_2 <- matrix(NA)
avg_N_spp_2 <- NULL
avg_H_ind_2 <- NULL
avg_D_ind_2 <- NULL
avg_E_ind_2 <- NULL
reg_N_2 <- matrix(NA, nrow = length(yr), ncol = length(strata))
reg_H_2 <- matrix(NA, nrow = length(yr), ncol = length(strata))
reg_D_2 <- matrix(NA, nrow = length(yr), ncol = length(strata))
reg_E_2 <- matrix(NA, nrow = length(yr), ncol = length(strata))

for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year_2 <- spring_inds[which(spring_inds$YEAR == yr[i]),] #subset unique hauls
  
  for (j in 1:N_strata) {  
    diff_strata_2 <- diff_year_2[which(diff_year_2$REGION == j),] 
    avg_N_spp_2[j] <- mean(diff_strata_2$N_species)
    avg_H_ind_2[j] <- mean(diff_strata_2$H_index)
    avg_D_ind_2[j] <- mean(diff_strata_2$D_index)
    avg_E_ind_2[j] <- mean(diff_strata_2$E_index)
  }
  reg_N_2[i,] <- avg_N_spp_2
  reg_H_2[i,] <- avg_H_ind_2
  reg_D_2[i,] <- avg_D_ind_2
  reg_E_2[i,] <- avg_E_ind_2
}

library(RColorBrewer)
cols <- brewer.pal(4, "Purples")
# for SNEC #
cols <- c('orchid', 'mediumorchid', 'mediumorchid4', 'mediumpurple4')

layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
layout.show(8)
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R

pdf('fall_N_ME.pdf')
plot(reg_N[,1], type = 'l', xlab = '', ylab = "Number of Species", main = "Average Fall Indices by Depth Strata"
     , ylim = c(12, 26), col = cols[1], xaxt = 'n', lwd = 3, cex.axis = 1.5, cex.lab = 1.5)
axis(1, at = c(1:18),labels = label, las = 2, cex.axis = 2)
lines(reg_N[,2], type = 'l', col = cols[2], lwd = 3)
lines(reg_N[,3], type = 'l', col = cols[3], lwd = 3)
lines(reg_N[,4], type = 'l', col = cols[4], lwd = 3)
#legend('topleft', legend = c("Depth 1", "Depth 2", "Depth 3", "Depth 4"), col = cols[1:4], lty = 1, bty = 'n', lwd = 3, cex = 1.5)
dev.off()

plot(reg_H[,1], type = 'l', xlab = '', ylab = "Shannon-Weiner Index", ylim = c(0.8,2), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_H[,2], type = 'l', col = cols[2])
lines(reg_H[,3], type = 'l', col = cols[3])
lines(reg_H[,4], type = 'l', col = cols[4])

plot(reg_D[,1], type = 'l', xlab = '', ylab = "Simpson Diversity Index", ylim = c(1.8,5), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_D[,2], type = 'l', col = cols[2])
lines(reg_D[,3], type = 'l', col = cols[3])
lines(reg_D[,4], type = 'l', col = cols[4])

plot(reg_E[,1], type = 'l', xlab = "Survey Year", ylab = "Simpson Evenness Index", ylim = c(0.05,0.26), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_E[,2], type = 'l', col = cols[2])
lines(reg_E[,3], type = 'l', col = cols[3])
lines(reg_E[,4], type = 'l', col = cols[4])


plot(reg_N_2[,1], type = 'l', xlab = '', ylab = '', main = "Average Spring Indices by Depth Strata", ylim = c(16, 26), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_N_2[,2], type = 'l', col = cols[2])
lines(reg_N_2[,3], type = 'l', col = cols[3])
lines(reg_N_2[,4], type = 'l', col = cols[4])
legend('topleft', legend = c("Depth 1", "Depth 2", "Depth 3", "Depth 4"), col = cols[1:4], lty = 1, bty = 'n')


plot(reg_H_2[,1], type = 'l', xlab = '', ylab = '', ylim = c(0.8,2), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_H_2[,2], type = 'l', col = cols[2])
lines(reg_H_2[,3], type = 'l', col = cols[3])
lines(reg_H_2[,4], type = 'l', col = cols[4])

plot(reg_D_2[,1], type = 'l', xlab = '', ylab = '', ylim = c(1.8,5), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_D_2[,2], type = 'l', col = cols[2])
lines(reg_D_2[,3], type = 'l', col = cols[3])
lines(reg_D_2[,4], type = 'l', col = cols[4])

plot(reg_E_2[,1], type = 'l', xlab = "Survey Year", ylab = '', ylim = c(0.05,0.26), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_E_2[,2], type = 'l', col = cols[2])
lines(reg_E_2[,3], type = 'l', col = cols[3])
lines(reg_E_2[,4], type = 'l', col = cols[4])


########################## taxonomic diversity #############################
#fall by region over time 
fall_inds <- subset(ind_by_haul, SEASON == "FL")

diff_year <- matrix(NA) 
N_sample <- NULL
start <- 00
end <- 17
yr <- seq(0, 17, by = 1)
region <- unique(ind_by_haul$REGION)
N_regions <- length(region) #5 regions
diff_region <- matrix(NA)
avg_delta <- NULL
avg_delta_plus <- NULL
avg_delta_star <- NULL
avg_delta_var <- NULL
reg_d <- matrix(NA, nrow = length(yr), ncol = length(region))
reg_p <- matrix(NA, nrow = length(yr), ncol = length(region))
reg_s <- matrix(NA, nrow = length(yr), ncol = length(region))
reg_v <- matrix(NA, nrow = length(yr), ncol = length(region))


for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year <- fall_inds[which(fall_inds$YEAR == yr[i]),] #subset unique hauls
  
  for (j in 1:N_regions) {  
    diff_region <- diff_year[which(diff_year$REGION == j),] 
    avg_delta[j] <- mean(diff_region$delta)
    avg_delta_plus[j] <- mean(diff_region$delta_plus)
    avg_delta_star[j] <- mean(diff_region$delta_star)
    avg_delta_var[j] <- mean(diff_region$delta_var)
  }
  reg_d[i,] <- avg_delta
  reg_p[i,] <- avg_delta_plus
  reg_s[i,] <- avg_delta_star
  reg_v[i,] <- avg_delta_var
}


## spring by region over time 
spring_inds <- subset(ind_by_haul, SEASON == "SP")

diff_year_2 <- matrix(NA) 
N_sample_2 <- NULL
start <- 00
end <- 17
yr <- seq(0, 17, by = 1)
region <- unique(ind_by_haul$REGION)
N_regions <- length(region) #5 regions
diff_region_2 <- matrix(NA)
avg_delta_2 <- NULL
avg_delta_plus_2 <- NULL
avg_delta_star_2 <- NULL
avg_delta_var_2 <- NULL
reg_d_2 <- matrix(NA, nrow = length(yr), ncol = length(region))
reg_p_2 <- matrix(NA, nrow = length(yr), ncol = length(region))
reg_s_2 <- matrix(NA, nrow = length(yr), ncol = length(region))
reg_v_2 <- matrix(NA, nrow = length(yr), ncol = length(region))

for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year_2 <- spring_inds[which(spring_inds$YEAR == yr[i]),] #subset unique hauls
  
  for (j in 1:N_regions) {  
    diff_region_2 <- diff_year_2[which(diff_year_2$REGION == j),] 
    avg_delta_2[j] <- mean(diff_region_2$delta)
    avg_delta_plus_2[j] <- mean(diff_region_2$delta_plus)
    avg_delta_star_2[j] <- mean(diff_region_2$delta_star)
    avg_delta_var_2[j] <- mean(diff_region_2$delta_var)
  }
  reg_d_2[i,] <- avg_delta_2
  reg_p_2[i,] <- avg_delta_plus_2
  reg_s_2[i,] <- avg_delta_star_2
  reg_v_2[i,] <- avg_delta_var_2
}

library(RColorBrewer)
cols <- brewer.pal(6, "Blues")

layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
layout.show(8)
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R

plot(reg_d[,1], type = 'l', xlab = '', ylab = "Taxonomic Diversity", main = "Average Fall Indices by Region" 
     , col = cols[2], ylim = c(0, 1700000), xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_d[,2], type = 'l', col = cols[3])
lines(reg_d[,3], type = 'l', col = cols[4])
lines(reg_d[,4], type = 'l', col = cols[5])
lines(reg_d[,5], type = 'l', col = cols[6])


plot(reg_p[,1], type = 'l', xlab = '', ylab = "Taxonomic Distinctness", ylim = c(4.5,5.2), col = cols[2], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_p[,2], type = 'l', col = cols[3])
lines(reg_p[,3], type = 'l', col = cols[4])
lines(reg_p[,4], type = 'l', col = cols[5])
lines(reg_p[,5], type = 'l', col = cols[6])

plot(reg_s[,1], type = 'l', xlab = '', ylab = "Average Taxonomic Distinctness", col = cols[2], ylim = c(3,5.5), xaxt = 'n') 
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_s[,2], type = 'l', col = cols[3])
lines(reg_s[,3], type = 'l', col = cols[4])
lines(reg_s[,4], type = 'l', col = cols[5])
lines(reg_s[,5], type = 'l', col = cols[6])

plot(reg_v[,1], type = 'l', xlab = "Survey Year", ylab = "Variation in Taxonomic Diversity", ylim = c(1.25, 2), col = cols[2], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_v[,2], type = 'l', col = cols[3])
lines(reg_v[,3], type = 'l', col = cols[4])
lines(reg_v[,4], type = 'l', col = cols[5])
lines(reg_v[,5], type = 'l', col = cols[6])


plot(reg_d_2[,1], type = 'l', xlab = '', ylab = '', main = "Average Spring Indices by Region", col = cols[2], ylim = c(0, 1700000), xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_d_2[,2], type = 'l', col = cols[3])
lines(reg_d_2[,3], type = 'l', col = cols[4])
lines(reg_d_2[,4], type = 'l', col = cols[5])
lines(reg_d_2[,5], type = 'l', col = cols[6])
legend('topleft', legend = c("Region 1", "Region 2", "Region 3", "Region 4", "Region 5"), col = cols[2:6], lty = 1, bty = 'n')


plot(reg_p_2[,1], type = 'l', xlab = '', ylab = '', col = cols[2], ylim = c(4.5,5.2), xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_p_2[,2], type = 'l', col = cols[3])
lines(reg_p_2[,3], type = 'l', col = cols[4])
lines(reg_p_2[,4], type = 'l', col = cols[5])
lines(reg_p_2[,5], type = 'l', col = cols[6])

plot(reg_s_2[,1], type = 'l', xlab = '', ylab = '', col = cols[2], ylim = c(3,5.5), xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_s_2[,2], type = 'l', col = cols[3])
lines(reg_s_2[,3], type = 'l', col = cols[4])
lines(reg_s_2[,4], type = 'l', col = cols[5])
lines(reg_s_2[,5], type = 'l', col = cols[6])

plot(reg_v_2[,1], type = 'l', xlab = "Survey Year", ylab = '', col = cols[2], ylim = c(1.25,2), xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_v_2[,2], type = 'l', col = cols[3])
lines(reg_v_2[,3], type = 'l', col = cols[4])
lines(reg_v_2[,4], type = 'l', col = cols[5])
lines(reg_v_2[,5], type = 'l', col = cols[6])




#############taxonomic diveristy by stratum 

#fall
fall_inds <- subset(ind_by_haul, SEASON == "FL")

diff_year <- matrix(NA) 
N_sample <- NULL
start <- 00
end <- 17
yr <- seq(0, 17, by = 1)
strata <- unique(ind_by_haul$STRATUM)
N_strata <- length(strata) #4
diff_strata <- matrix(NA)
avg_delta <- NULL
avg_delta_plus <- NULL
avg_delta_star <- NULL
avg_delta_var <- NULL
reg_d <- matrix(NA, nrow = length(yr), ncol = length(strata))
reg_p <- matrix(NA, nrow = length(yr), ncol = length(strata))
reg_s <- matrix(NA, nrow = length(yr), ncol = length(strata))
reg_v <- matrix(NA, nrow = length(yr), ncol = length(strata))


for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year <- fall_inds[which(fall_inds$YEAR == yr[i]),] #subset unique hauls
  
  for (j in 1:N_strata) {  
    diff_strata <- diff_year[which(diff_year$STRATUM == j),] 
    avg_delta[j] <- mean(diff_strata$delta)
    avg_delta_plus[j] <- mean(diff_strata$delta_plus)
    avg_delta_star[j] <- mean(diff_strata$delta_star)
    avg_delta_var[j] <- mean(diff_strata$delta_var)
  }
  reg_d[i,] <- avg_delta
  reg_p[i,] <- avg_delta_plus
  reg_s[i,] <- avg_delta_star
  reg_v[i,] <- avg_delta_var
}


## spring  
spring_inds <- subset(ind_by_haul, SEASON == "SP")

diff_year_2 <- matrix(NA) 
N_sample_2 <- NULL
start <- 00
end <- 17
yr <- seq(0, 17, by = 1)
strata <- unique(ind_by_haul$STRATUM)
N_strata <- length(strata) #4
diff_strata_2 <- matrix(NA)
avg_delta_2 <- NULL
avg_delta_plus_2 <- NULL
avg_delta_star_2 <- NULL
avg_delta_var_2 <- NULL
reg_d_2 <- matrix(NA, nrow = length(yr), ncol = length(strata))
reg_p_2 <- matrix(NA, nrow = length(yr), ncol = length(strata))
reg_s_2 <- matrix(NA, nrow = length(yr), ncol = length(strata))
reg_v_2 <- matrix(NA, nrow = length(yr), ncol = length(strata))

for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year_2 <- spring_inds[which(spring_inds$YEAR == yr[i]),] #subset unique hauls
  
  for (j in 1:N_strata) {  
    diff_strata_2 <- diff_year_2[which(diff_year_2$REGION == j),] 
    avg_delta_2[j] <- mean(diff_strata_2$delta)
    avg_delta_plus_2[j] <- mean(diff_strata_2$delta_plus)
    avg_delta_star_2[j] <- mean(diff_strata_2$delta_star)
    avg_delta_var_2[j] <- mean(diff_strata_2$delta_var)
  }
  reg_d_2[i,] <- avg_delta_2
  reg_p_2[i,] <- avg_delta_plus_2
  reg_s_2[i,] <- avg_delta_star_2
  reg_v_2[i,] <- avg_delta_var_2
}


library(RColorBrewer)
cols <- brewer.pal(4, "Reds")

layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
layout.show(8)
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R

plot(reg_d[,1], type = 'l', xlab = '', ylab = "Taxonomic Diversity", main = "Average Fall Indices by Depth Strata"
     , col = cols[1], ylim = c(0,  1700000), xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_d[,2], type = 'l', col = cols[2])
lines(reg_d[,3], type = 'l', col = cols[3])
lines(reg_d[,4], type = 'l', col = cols[4])


plot(reg_p[,1], type = 'l', xlab = '', ylab = "Taxonomic Distinctness", ylim = c(4.5,5.2), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_p[,2], type = 'l', col = cols[2])
lines(reg_p[,3], type = 'l', col = cols[3])
lines(reg_p[,4], type = 'l', col = cols[4])

plot(reg_s[,1], type = 'l', xlab = '', ylab = "Average Taxonomic Distinctness", ylim = c(3,5.5), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_s[,2], type = 'l', col = cols[2])
lines(reg_s[,3], type = 'l', col = cols[3])
lines(reg_s[,4], type = 'l', col = cols[4])

plot(reg_v[,1], type = 'l', xlab = "Survey Year", ylab = "Variation in Taxonomic Diversity", ylim = c(1.25,2), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_v[,2], type = 'l', col = cols[2])
lines(reg_v[,3], type = 'l', col = cols[3])
lines(reg_v[,4], type = 'l', col = cols[4])


plot(reg_d_2[,1], type = 'l', xlab = '', ylab = '', main = "Average Spring Indices by Depth Strata", ylim = c(0,  1700000), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_d_2[,2], type = 'l', col = cols[2])
lines(reg_d_2[,3], type = 'l', col = cols[3])
lines(reg_d_2[,4], type = 'l', col = cols[4])
legend('topleft', legend = c("Depth 1", "Depth 2", "Depth 3", "Depth 4"), col = cols[1:4], lty = 1, bty = 'n')


plot(reg_p_2[,1], type = 'l', xlab = '', ylab = '', ylim = c(4.5,5.2), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_p_2[,2], type = 'l', col = cols[2])
lines(reg_p_2[,3], type = 'l', col = cols[3])
lines(reg_p_2[,4], type = 'l', col = cols[4])

plot(reg_s_2[,1], type = 'l', xlab = '', ylab = '', ylim = c(3,5.5), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_s_2[,2], type = 'l', col = cols[2])
lines(reg_s_2[,3], type = 'l', col = cols[3])
lines(reg_s_2[,4], type = 'l', col = cols[4])

plot(reg_v_2[,1], type = 'l', xlab = "Survey Year", ylab = '', ylim = c(1.25,2), col = cols[1], xaxt = 'n')
axis(1, at = c(1:18),labels = label, las = 2)
lines(reg_v_2[,2], type = 'l', col = cols[2])
lines(reg_v_2[,3], type = 'l', col = cols[3])
lines(reg_v_2[,4], type = 'l', col = cols[4])



###AEW development code;
# specific test code to calculate weights
#phylo <- as.matrix(subset(diff_hauls, select = c(Phylum,Class,Order,Family,Genus, Species))) # extract phylogenetic information only
#unique_phylo <- uniquecombs(phylo) # subset by unique species information
#unique_phylo <- as.data.frame(unique_phylo)
#x_y <- matrix(NA, nrow = 6, ncol = 6)
#x <- NULL
#y <- NULL
#ident <- NULL
#weight <- NULL
#for (j in 1:ncol(combos)){
#  x <- unique_phylo[unique_phylo$Species == combos[1,j],]
#  y <- unique_phylo[unique_phylo$Species == combos[2,j],]
#   x_y <- rbind(x,y)
#  for (k in 1:ncol(x_y)){
#    ident[k] <- identical(as.vector(x_y[1,k]), as.vector(x_y[2,k]))
#    weight[j] <- sum(ident == "FALSE")
#  }
#}



############# AEW original toy example for taxonomic diversity indices
#AP_DF <- 5 #alewife/dogfish
#AP_SD <- 4 #alewife/sand dab
#AP_AS <- 1 #alewife/shad
#SD_AS <- 4 #sad dab/shad
#DF_AS <- 5 #dogfish/shad
#SD_DF <- 5 #sad dab/dogfish

## diversity per haul
#trawl <- read.csv("ext_trawl.csv")
#diff_hauls <- matrix(NA) 
#N_species <- NULL
#delta <- NULL
#delta_star <- NULL
#hauls <- unique(trawl$STATION)  ##### change $station to $EFFORT_SEQ_NO 
#N_hauls <- length(hauls) # number of hauls    
#for (i in 1:N_hauls) {
#  diff_hauls <- trawl[which(trawl$STATION == i),] #subset unique hauls
#  N_species[i] <- length(unique(diff_hauls$SCINAME))# count the number of unique species (denominator)
#  numerator <- (AP_DF*sum(diff_hauls$SCINAME == "Alosa pseudoharengus")*sum(diff_hauls$SCINAME == "Squalus acanthias")) + # each unique combination
#  (AP_SD*sum(diff_hauls$SCINAME == "Alosa pseudoharengus")* sum(diff_hauls$SCINAME == "Scophthalmus aquosus")) +
#  (AP_AS*sum(diff_hauls$SCINAME == "Alosa pseudoharengus")* sum(diff_hauls$SCINAME == "Alosa sapadisima")) +
#  (SD_AS*sum(diff_hauls$SCINAME == "Scophthalmus aquosus")* sum(diff_hauls$SCINAME == "Alosa sapadisima")) +
#  (DF_AS*sum(diff_hauls$SCINAME == "Squalus acanthias")* sum(diff_hauls$SCINAME == "Alosa sapadisima")) +
#  (SD_DF*sum(diff_hauls$SCINAME == "Squalus acanthias")* sum(diff_hauls$SCINAME == "Scophthalmus aquosus")) 
#  delta[i] <- (2*numerator)/(N_species*(N_species-(rep(1,N_hauls)))) #abundance from haul not global
#}
#taxonomic distinctness
#note this is different from equation in proposal ###AEW check
#    delta_denom <- (sum(diff_hauls$SCINAME == "Alosa pseudoharengus")*sum(diff_hauls$SCINAME == "Squalus acanthias")) + # each unique combination
#    (sum(diff_hauls$SCINAME == "Alosa pseudoharengus")* sum(diff_hauls$SCINAME == "Scophthalmus aquosus")) +
#    (sum(diff_hauls$SCINAME == "Alosa pseudoharengus")* sum(diff_hauls$SCINAME == "Alosa sapadisima")) +
#    (sum(diff_hauls$SCINAME == "Scophthalmus aquosus")* sum(diff_hauls$SCINAME == "Alosa sapadisima")) +
#    (sum(diff_hauls$SCINAME == "Squalus acanthias")* sum(diff_hauls$SCINAME == "Alosa sapadisima")) +
#    (freq(diff_hauls$SCINAME == "Squalus acanthias")* sum(diff_hauls$SCINAME == "Scophthalmus aquosus")) 
#    delta_star[i] <- numerator/delta_denom

#print(delta) #taxonomic diversity
#print(delta_star) #taxonomic distinctness

##### average taxonomic diversity
#diff_hauls <- matrix(NA) 
#N_species <- NULL
#delta_plus <- NULL
#hauls <- unique(trawl$STATION)  ##### change $station to $EFFORT_SEQ_NO 
#N_hauls <- length(hauls) # number of hauls    
#for (i in 1:N_hauls) {
#  diff_hauls <- trawl[which(trawl$STATION == i),] #subset unique hauls
#  N_species[i] <- length(unique(diff_hauls$SCINAME))# count the number of unique species (denominator)
#  numerator <- (AP_DF*frequency(diff_hauls$SCINAME == "Alosa pseudoharengus")*frequency(diff_hauls$SCINAME == "Squalus acanthias")) + # each unique combination
#    (AP_SD*frequency(diff_hauls$SCINAME == "Alosa pseudoharengus")* frequency(diff_hauls$SCINAME == "Scophthalmus aquosus")) +
#    (AP_AS*frequency(diff_hauls$SCINAME == "Alosa pseudoharengus")* frequency(diff_hauls$SCINAME == "Alosa sapadisima")) +
#    (SD_AS*frequency(diff_hauls$SCINAME == "Scophthalmus aquosus")* frequency(diff_hauls$SCINAME == "Alosa sapadisima")) +
#    (DF_AS*frequency(diff_hauls$SCINAME == "Squalus acanthias")* frequency(diff_hauls$SCINAME == "Alosa sapadisima")) +
#    (SD_DF*frequency(diff_hauls$SCINAME == "Squalus acanthias")* frequency(diff_hauls$SCINAME == "Scophthalmus aquosus")) 
#  delta_plus[i] <- (2*numerator)/(N_species[i]*(N_species[i]-1)) #abundance from haul not global
#}
#print(delta_plus)


