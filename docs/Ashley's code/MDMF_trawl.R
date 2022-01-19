####################################################################################
### Biodiversity metrics to be applied to survey data
### UM Seagrant; climate impacts on Maine coastal fisheries using the ME-NH inshore trawl
### Objective 1 part 1
### MDMF data
### 8/14/2018
### A.E. Weston
####################################################################################
#Notes from M.Camissa;
#Catch at each station by species.  MDMF 1978-2018 SPRING trawl surveys.  All representative stations (shg<=136).  8/14/2018 M.Camisa											
#W/Resource new/2018_July_Dec/Ashley Weston/MDMF_spring_catch.sql 											
#NOTE*  All catch data is already expanded to the 20 minute standard											
#The following species were removed due to inconsistent sampling throughout the time series; 											
#306	northern shrimp										
#326	green crab										
#330	sand dollar unclassified										
#331	sea urchin and sand dollar unclassified										
#335	hermit crab unclassified										
#338	moon snail, shark eye, baby ear										

# AEW Notes;
#separed by season already
#stratified random based on five regions and six depth zones
#given stratum and station and depth; would need to categorize by region and strata to summarize
#spp IDs appear to be same as NEFSC
#expand sci name to full class 
#spp code > sci name > classification 
## some zeros included in number; 275 fall, 145 spring == eggs or unclassified
## some IDs unclassified 

#setwd("J:/Research/Kerr Lab/MDMF_trawl_2018") # originals
#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/MDMF") #modified date format
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/MDMF")
############################# fall MDMF data formatting to calculate indices ###########################
fall <- read.csv("MDMF_Catch_Data_fall.csv")
#com_fall <- unique(fall$COMMON.NAME) #181 spp
#write.csv(com_fall, "mdmf_comm_fall.csv")

# remove observations that are not classified to the species level 
fall <- fall[!fall$COMMON.NAME == "SHRIMP UNCL", ]                   #218
fall <- fall[!fall$COMMON.NAME == "SPIDER CRAB UNCL", ]              #25574
fall <- fall[!fall$COMMON.NAME == "OCTOPUS UNCL", ]                  #42
fall <- fall[!fall$COMMON.NAME == "SKATE UNCL", ]                    #12
fall <- fall[!fall$COMMON.NAME == "LIZARDFISH UNCL", ]               #4
fall <- fall[!fall$COMMON.NAME == "CORNETFISH UNCL", ]               #2
fall <- fall[!fall$COMMON.NAME == "MANTIS SHRIMP UNCL", ]            #86
fall <- fall[!fall$COMMON.NAME == "HAKE UNCL", ]                     #70
fall <- fall[!fall$COMMON.NAME == "SNAPPER UNCL", ]                  #2
fall <- fall[!fall$COMMON.NAME == "RAZOR AND JACKKNIFE CLAM UNCL", ] #5
fall <- fall[!fall$COMMON.NAME == "HOOKEAR SCULPIN UNCL", ]          #1
fall <- fall[!fall$COMMON.NAME == "LEFTEYE FLOUNDER UNCL", ]         #1
fall <- fall[!fall$COMMON.NAME == "GOBY UNCL", ]                     #72
fall <- fall[!fall$COMMON.NAME == "CUSK-EEL UNCL", ]                 #49
fall <- fall[!fall$COMMON.NAME == "SHRIMP UNCL", ]                   #218
fall <- fall[!fall$COMMON.NAME == "LONGFIN SQUID EGG MOPS", ]        #0
fall <- fall[!fall$COMMON.NAME == "LUMPFISH SNAILFISH UNCL", ]       #3
fall <- fall[!fall$COMMON.NAME == "TONGUEFISH UNCL", ]               #1
fall <- fall[!fall$COMMON.NAME == "SEA BASS UNCL", ]                 #3
fall <- fall[!fall$COMMON.NAME == "UNKNOWN 01", ]                    #2
fall <- fall[!fall$COMMON.NAME == "UNKNOWN 02", ]                    #5
#sum(fall_spp$NUM)
#sum(fall$NUM) #10537117
#(26370/10537117)*100 = 0.25% of observations not ID'd to spp level

# remove remaining observations that have zero NUM
fall <- fall[!fall$NUM == 0,] # 7(5 blue mussels,1 northern horsemussel, 1 lady crab)

# create unique GMRI indicator for station/year combo (already separated by season)
fall$GMRI_INDICATOR <-  cumsum(!duplicated(fall[7:8])) 
write.csv(fall, "MDMF_Catch_Data_fall_2.csv")

#####functional group plots#####

fall<-read.csv("MDMF_Catch_Data_fall_2.csv")[-1]
fall$SEASON<-"Fall"
spring<-read.csv("MDMF_Catch_Data_spring_2.csv")[-1]
spring$SEASON<-"Spring"
mdmf_catch<-bind_rows(fall, spring)

setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/ID Grouping")
mdmf_species<-as.data.frame(unique(mdmf_catch$COMMON.NAME))
#write.csv(mdmf_species, "mdmf_species.csv")
mdmf_groups<-read.csv("mdmf_species.csv")[-1]
#OR
mdmf_groups<-nefsc_species[-1]%>%
  rename(COMMON.NAME=COMNAME)

mdmf_catch_2<-left_join(mdmf_catch, mdmf_groups, by="COMMON.NAME")

mdmf_groups<-group_by(mdmf_catch_2, YEAR, STATION, functional_group)%>%
  mutate(INDICATOR=cur_group_id())

mdmf_groups_sum<-filter(mdmf_groups, WT..KG.!=0)%>%
  group_by(YEAR, functional_group)%>%
  summarise(biomass_kg=sum(WT..KG., na.rm=TRUE)) 

ggplot()+geom_line(data=mdmf_groups_sum, aes(x=factor(YEAR), y=biomass_kg), group=1)+facet_grid(functional_group~.)+ 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year")+ylab("Biomass")+theme(axis.title.x=element_text(size=14), axis.title.y = element_text(size=14))+
  theme(axis.text.x=element_text(size=12), axis.text.y = element_text(size=12))+
  scale_x_discrete(breaks=seq(1978,2018, 5))+
  theme(strip.background = element_rect(fill="white"), strip.text = element_text(size=12))

#by season
mdmf_groups_season_sum<-filter(mdmf_groups, WT..KG.!=0)%>%
  group_by(YEAR, SEASON,functional_group)%>%
  summarise(biomass_kg=sum(WT..KG., na.rm=TRUE)) #sum of weights from all 20 minute hauls

ggplot()+geom_line(data=mdmf_groups_season_sum, aes(x=factor(YEAR), y=biomass_kg), group=1)+facet_grid(functional_group~SEASON)+ 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year")+ylab("Biomass")+theme(axis.title.x=element_text(size=14), axis.title.y = element_text(size=14))+
  theme(axis.text.x=element_text(size=12), axis.text.y = element_text(size=12))+
  scale_x_discrete(breaks=seq(1978,2018, 7))+
  theme(strip.background = element_rect(fill="white"), strip.text = element_text(size=12))


## calculate species richness, Shannon-Weiner diversity index, Simpson's diversity index, and Simpson's evenness index by haul
diff_hauls <- matrix(NA) 
N_species <- NULL
N_each_spp <- NULL
total_spp <- NULL
spp_prop <- NULL
H_index <- NULL  
D_index <- NULL 
E_index <- NULL 
hauls <- unique(fall$GMRI_INDICATOR) # haul within each unique year/season 
N_hauls <- length(hauls) # number of hauls
for (i in 1:N_hauls) { #loop through each haul within year/season
  diff_hauls <- fall[which(fall$GMRI_INDICATOR == i),] #subset unique hauls
  N_species[i] <- length(unique(diff_hauls$COMMON.NAME))# count the number of unique species
  N_each_spp <- diff_hauls$NUM
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


#create new tow by tow information file 
hauls <- unique(fall$GMRI_INDICATOR) # haul within each unique year/season 
N_hauls <- length(hauls) # number of hauls
by_tow <- matrix(NA, nrow = N_hauls, ncol = 10)
for (i in 1:N_hauls) { #loop through each haul within year/season
  diff_hauls <- fall[which(fall$GMRI_INDICATOR == i),] #subset unique hauls
by_tow[i,] <- as.matrix(diff_hauls[1,1:10])
}
colnames(by_tow) <- c("LONGITUDE", "LATITUDE", "CRUISE", "STRATUM", "MONTH", "DAY", "YEAR", "STATION", "DEPTH_M", "TEMP_C")
mdmf_by_tow <- cbind(by_tow, N_species, H_index, D_index, E_index)
write.csv(mdmf_by_tow, "mdmf_fall_by_tow.csv") 


####### taxonomic diversity
## need to assign weight for each species combination based on Linnaean classification
## same species; w = 0
## same genus; w = 1
## same family, different genera; w = 2
## same order, different family; w = 3
## same class, different order; w = 4
## same phyla, different class; w = 5
## different phyla; w = 6

#build out survey matrix with species, genus, family, order, class, phylum for each observation
#some complications because common name not species name
library(taxize)
library(purrr)
library(mgcv)
sci_name <- as.vector(fall$COMMON.NAME)  
diff_sci_name <- unique(sci_name) # all unique species names 
tax <- classification(diff_sci_name, db = 'itis') #get classification information for all species observed  

info <- matrix(NA)
expand <- matrix(NA)
specific <- matrix(NA, nrow = length(diff_sci_name), ncol = 6)
for (i in 1:length(tax)){
  info <- tax[[i]][c('name','rank')] # gives classification and rank for each species
  expand <- info[info$rank == 'phylum'| info$rank == 'class'| info$rank == 'order' | info$rank == 'family' | info$rank == 'genus' | info$rank == 'species',]
  specific[i,] <- as.vector(expand$name)
}

#when all info doesn't show
name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Ariommatidae", "Ariomma", "Ariomma bondi")
rank <- c("kindom", "phylum", "class", "order", "family", "genus", "species")
id <- seq(1, 7, by = 1)
#tax$`LONGFIN SQUID` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$SNAKEBLENNY <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`PLANEHEAD FILEFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`DAUBED SHANNY` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`NORTHERN STONE CRAB` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`SILVER RAG` <- data.frame("name" = name, "rank" = rank, "id" = id)


specific <- cbind(specific, diff_sci_name)
colnames(specific) <- c("Phylum", "Class", "Order", "Family", "Genus", "Species", "Common_name")
write.csv(specific, "mdmf_fall_phylo.csv")

### loop through trawl survey to expand each sample based on above taxonomic information
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/MDMF")
fall_2 <- read.csv("MDMF_Catch_Data_fall_2.csv", header = TRUE)
fall_2 <- fall_2[,-1]
phylo <- read.csv("mdmf_fall_phylo.csv", header = TRUE)
phylo <- as.data.frame(phylo)
sci_name <- as.vector(fall_2$COMMON.NAME)  
expand_all <- matrix(NA, nrow = nrow(fall_2), ncol = 7)
for (i in 1:nrow(fall_2)) {
  expand_all[i,] <- as.matrix(phylo[phylo[,"Common_name"] == sci_name[i],]) 
}

colnames(expand_all) <- c("Phylum", "Class", "Order", "Family", "Genus", "Species", "Common")

write.csv(expand_all, "expanded_MDMF_fall_phylo.csv", row.names = FALSE) #save expanded survey phylogenetic information

# save whole new expanded trawl
fall_3 <- cbind(fall_2, expand_all)
write.csv(fall_3, "MDMF_fall_catch_phylo.csv", row.names = FALSE)

########### now that survey data is reformatted calculate taxonomic diversity indices
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/MDMF")
ext_trawl <- read.csv("MDMF_fall_catch_phylo.csv", header = TRUE)
#ext_trawl <- read.csv("MDMF_spring_catch_phylo.csv", header = TRUE)

library(mgcv)

hauls <- unique(ext_trawl$GMRI_INDICATOR)   
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
  diff_hauls <- ext_trawl[which(ext_trawl$GMRI_INDICATOR == j),] #subset unique hauls
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
    total[i] <- diff_hauls[diff_hauls[,21] == combos[1,i],13] * diff_hauls[diff_hauls[,21] == combos[2,i],13]
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
tow <- read.csv("mdmf_fall_by_tow.csv")
tax_indices <- cbind(delta, delta_star, delta_plus, delta_var)
write.csv(tax_indices, "fall_tax_indices.csv")
ind_by_haul <- cbind(tow, tax_indices)
write.csv(ind_by_haul, "MDMF_fall_div_ind_by_tow.csv")


#################################### spring MDMF data formatting to calculate indices ##############################
spring <- read.csv("MDMF_Catch_Data_spring.csv")
#com_spring <- unique(spring$COMMON.NAME) #113 spp
#write.csv(com_spring, "mdmf_comm_sp.csv")

# remove observations that are not to the spp level
spring <- spring[!spring$COMMON.NAME == "SKATE UNCL",]                    #145
spring <- spring[!spring$COMMON.NAME == "HAKE UNCL",]                     #801
spring <- spring[!spring$COMMON.NAME == "SHRIMP UNCL",]                   #913
spring <- spring[!spring$COMMON.NAME == "SPIDER CRAB UNCL",]              #77050
spring <- spring[!spring$COMMON.NAME == "OCTOPUS UNCL",]                  #47
spring <- spring[!spring$COMMON.NAME == "MANTIS SHRIMP UNCL",]            #72
spring <- spring[!spring$COMMON.NAME == "RAZOR AND JACKKNIFE CLAM UNCL",] #7
spring <- spring[!spring$COMMON.NAME == "LONGFIN SQUID EGG MOPS",]        #0
spring <- spring[!spring$COMMON.NAME == "CONGER EEL UNCL",]               #6
#sum(sp_spp$NUM)
#sum(spring$NUM) #3148352
#(7904/3148352)*100 = 0.25% of observations not id'd to spp level

# remove remaining observations that have zero NUM
spring <- spring[!spring$NUM == 0,] #7 (6 blue mussels, 1 lady crab)
# there is 1 haul that is all sand lance = must be removed to calculate metrics
spring <- spring[!(spring$COMMON.NAME == "NORTHERN SAND LANCE" & spring$NUM == 5100),]


# create unique GMRI indicator for station/year combo (already separated by season)
spring$GMRI_INDICATOR <-  cumsum(!duplicated(spring[7:8])) 
write.csv(spring, "MDMF_Catch_Data_spring_2.csv") 

## calculate species richness, Shannon-Weiner diversity index, Simpson's diversity index, and Simpson's evenness index by haul
diff_hauls <- matrix(NA) 
N_species <- NULL
N_each_spp <- NULL
total_spp <- NULL
spp_prop <- NULL
H_index <- NULL  
D_index <- NULL 
E_index <- NULL 
hauls <- unique(spring$GMRI_INDICATOR) # haul within each unique year/season 
N_hauls <- length(hauls) # number of hauls
for (i in 1:N_hauls) { #loop through each haul within year/season
  diff_hauls <- spring[which(spring$GMRI_INDICATOR == i),] #subset unique hauls
  N_species[i] <- length(unique(diff_hauls$COMMON.NAME))# count the number of unique species
  N_each_spp <- diff_hauls$NUM
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


#create new tow by tow information file 
hauls <- unique(spring$GMRI_INDICATOR) # haul within each unique year/season 
N_hauls <- length(hauls) # number of hauls
by_tow <- matrix(NA, nrow = N_hauls, ncol = 10)
for (i in 1:N_hauls) { #loop through each haul within year/season
  diff_hauls <- spring[which(spring$GMRI_INDICATOR == i),] #subset unique hauls
  by_tow[i,] <- as.matrix(diff_hauls[1,1:10])
}
colnames(by_tow) <- c("LONGITUDE", "LATITUDE", "CRUISE", "STRATUM", "MONTH", "DAY", "YEAR", "STATION", "DEPTH_M", "TEMP_C")
mdmf_by_tow <- cbind(by_tow, N_species, H_index, D_index, E_index)
write.csv(mdmf_by_tow, "mdmf_spring_by_tow.csv") 

####### taxonomic diversity
#build out survey matrix with species, genus, family, order, class, phylum for each observation
#some complications because common name not species name
library(taxize)
library(purrr)
library(mgcv)
sci_name <- as.vector(spring$COMMON.NAME)  
diff_sci_name <- unique(sci_name) # all unique species names 
tax <- classification(diff_sci_name, db = 'itis') #get classification information for all species observed  

info <- matrix(NA)
expand <- matrix(NA)
specific <- matrix(NA, nrow = length(diff_sci_name), ncol = 6)
for (i in 1:length(tax)){
  info <- tax[[i]][c('name','rank')] # gives classification and rank for each species
  expand <- info[info$rank == 'phylum'| info$rank == 'class'| info$rank == 'order' | info$rank == 'family' | info$rank == 'genus' | info$rank == 'species',]
  specific[i,] <- as.vector(expand$name)
}

#when all info doesn't show
name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Stichaeidae", "Lumpenus", "Lumpenus maculatus")
rank <- c("kindom", "phylum", "class", "order", "family", "genus", "species")
id <- seq(1, 7, by = 1)
#tax$`OCEAN POUT` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`LONGFIN SQUID` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$SNAKEBLENNY <- data.frame("name" = name, "rank" = rank, "id" = id)
tax$`DAUBED SHANNY` <- data.frame("name" = name, "rank" = rank, "id" = id)

specific <- cbind(specific, diff_sci_name)
colnames(specific) <- c("Phylum", "Class", "Order", "Family", "Genus", "Species", "Common_name")
write.csv(specific, "mdmf_spring_phylo.csv")

### loop through trawl survey to expand each sample based on above taxonomic information
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/MDMF")
spring_2 <- read.csv("MDMF_Catch_Data_spring_2.csv", header = TRUE)
spring_2 <- spring_2[,-1]
phylo <- read.csv("mdmf_spring_phylo.csv", header = TRUE)
phylo <- as.data.frame(phylo)
sci_name <- as.vector(spring_2$COMMON.NAME)  
expand_all <- matrix(NA, nrow = nrow(spring_2), ncol = 7)
for (i in 1:nrow(spring_2)) {
  expand_all[i,] <- as.matrix(phylo[phylo[,"Common_name"] == sci_name[i],]) 
}

colnames(expand_all) <- c("Phylum", "Class", "Order", "Family", "Genus", "Species", "Common")

write.csv(expand_all, "expanded_MDMF_spring_phylo.csv", row.names = FALSE) #save expanded survey phylogenetic information

# save whole new expanded trawl
spring_3 <- cbind(spring_2, expand_all)
write.csv(spring_3, "MDMF_spring_catch_phylo.csv", row.names = FALSE)

########### now that survey data is reformatted calculate taxonomic diversity indices
## AEW still throwing flag but calculating fine
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/MDMF")
ext_trawl <- read.csv("MDMF_spring_catch_phylo.csv", header = TRUE)
#ext_trawl <- read.csv("MDMF_fall_catch_phylo.csv", header = TRUE)
#ext_trawl <- read.csv("test.csv", header = TRUE)

library(mgcv)

hauls <- unique(ext_trawl$GMRI_INDICATOR)   
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
  diff_hauls <- ext_trawl[which(ext_trawl$GMRI_INDICATOR == j),] #subset unique hauls
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
    total[i] <- diff_hauls[diff_hauls[,21] == combos[1,i],13] * diff_hauls[diff_hauls[,21] == combos[2,i],13]
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
tow <- read.csv("mdmf_spring_by_tow.csv")
tax_indices <- cbind(delta, delta_star, delta_plus, delta_var)
write.csv(tax_indices, "spring_tax_indices.csv")
ind_by_haul <- cbind(tow, tax_indices)
write.csv(ind_by_haul, "MDMF_spring_div_ind_by_tow.csv")





### results vis
setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/MDMF")
mdmf <- read.csv("MDMF_spring_div_ind_by_tow.csv")
plot(mdmf$delta_var, type = 'l')





