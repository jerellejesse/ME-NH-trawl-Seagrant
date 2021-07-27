####################################################################################
### Mulitvariate ordination analysis
### UM Seagrant; climate impacts on Maine coastal fisheries using the ME-NH inshore trawl
### Objective 2
### 6/18/2018
### A.E. Weston
####################################################################################

# define functional traits of each survey sample (taxonomic, spatial, feeding)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant")
sppclass2 <- read.csv("sppclass2.csv") #from KM


#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant")
#ext_trawl <- read.csv("ext_trawl.csv") # read in data

#setwd("J:/Research/Kerr Lab/ME NH trawl data")
setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/ME NH data for GMRI")
trawl <- read.csv("EXPCATCH_forGMRI.csv", header = TRUE) #ME-NH trawl survey catch data


# nonmetric multidimensional scaling
# visual representation of similarity between community and functional groups and how they change over time
library(vegan)
# wcmdscale() # weighted (can do weights = 1)

# columns of variables and rows of samples 
# columns = taxonomic information
# taxonomy will group species = community/functional groups
# space and time? within year and survey stratum
# rows = species 
# usually 2 dimensions (abundance by time @ each station)

# for each season (spring/fall) and year



metaMDS()

# NMDS performed on distance matrix (numeric)
dist()

# goodness of fit is measured with 'stress'


?anosim
?adonis
