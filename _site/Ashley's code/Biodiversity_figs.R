####################################################################################
### Visualizing biodiversity metrics
### UM Seagrant; climate impacts on Maine coastal fisheries using the ME-NH inshore trawl
### Objective 1 part 1
### 7/5/2018
### A.E. Weston
####################################################################################

############### frequency over time of samples not identified to spp level 
setwd("J:/Research/Kerr Lab/ME NH trawl data")
trawl <- read.csv("EXPCATCH_forGMRI.csv", header = TRUE) #ME-NH trawl survey catch data
trawl$GMRI_INDICATOR <-  cumsum(!duplicated(trawl[1:2])) 

ID <- c("Myctophidae")
diff_hauls <- matrix(NA) 
N_spp <- NULL
hauls <- unique(trawl$GMRI_INDICATOR) # haul within each unique year/season 
N_hauls <- length(hauls) # number of hauls
for (i in 1:N_hauls) { #loop through each haul within year/season
  diff_hauls <- trawl[which(trawl$GMRI_INDICATOR == i),] #subset unique hauls
  N_spp[i] <- sum(diff_hauls$SCIENTIFIC_NAME == ID) # insert name here
}

# N of certain species by haul over time 
#barplot(N_spp[1:1518]) # fall
#barplot(N_spp[1519:3418]) # spring
# haul 1518 = end of fall survey 

# want this information by year
library(tidyr)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
ind_by_haul <- read.csv("ind_by_haul.csv", header = TRUE)
ind_by_haul$N_spp_X <- N_spp
by_haul <- separate(ind_by_haul, EFFORT_START_DATE, c("month", "day", "year"))

diff_year <- matrix(NA) 
N_sample <- NULL
start <- 2000
end <- 2017
yr <- seq(2000, 2017, by = 1)
for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year <- by_haul[which(by_haul$year == yr[i]),] #subset unique hauls
  N_sample[i] <- sum(diff_year$N_spp_X) # insert name here
}

barplot(N_sample, ylab = "Frequency", xlab = "Survey Year", main = ID, names.arg = yr, las = 2)


############################## trawl CPUE

setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
CPUE <- read.csv("CPUE.csv", header = TRUE)
CPUE <- CPUE[-36,]

layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 8, 2, byrow = TRUE))
layout.show(16)
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R


plot(CPUE$Alosa.pseudoharengus[1:18], type = 'l', main = "Fall (total #/year)", ylab = "Alosa pseudoharengus")
plot(CPUE$Alosa.pseudoharengus[19:35], type = 'l', main = "Spring (total #/year)", ylab = '') 

plot(CPUE$Alosa.sapidissima[1:18], type = 'l', ylab = "American shad")
plot(CPUE$Alosa.sapidissima[19:35], type = 'l', ylab = '')

plot(CPUE$Astarte.undata[1:18], type = 'l', ylab = "Northern Sea Star")
plot(CPUE$Astarte.undata[19:35], type = 'l', ylab = '') 

plot(CPUE$Cancer.borealis[1:18], type = 'l', ylab = "Jonah crab")
plot(CPUE$Cancer.borealis[19:35], type = 'l', ylab = '')

plot(CPUE$Clupea.harengus[1:18], type = 'l', ylab = "Atlantic herring")
plot(CPUE$Clupea.harengus[19:35], type = 'l', ylab = '')

plot(CPUE$Crangon.septemspinosa[1:18], type = 'l', ylab = "Crangon shrimp")
plot(CPUE$Crangon.septemspinosa[19:35], type = 'l', ylab = '')

plot(CPUE$Dichelopandalus.leptocerus[1:18], type = 'l', ylab = "Dichelo shrimp")
plot(CPUE$Dichelopandalus.leptocerus[19:35], type = 'l', ylab = '')

plot(CPUE$Gadus.morhua[1:18], type = 'l', ylab = "Cod")
plot(CPUE$Gadus.morhua[19:35], type = 'l', ylab = '')

plot(CPUE$Glyptocephalus.cynoglossus[1:18], type = 'l', ylab = 'Witch flounder', main = "Fall (total #/year)")
plot(CPUE$Glyptocephalus.cynoglossus[19:35], type = 'l', ylab = '', main = "Spring (total #/year)")

plot(CPUE$Hippoglossoides.platessoides[1:18], type = 'l', ylab = 'American plaice')
plot(CPUE$Hippoglossoides.platessoides[19:35], type = 'l', ylab = '')

plot(CPUE$Homarus.americanus[1:18], type = 'l', ylab = 'Lobster')
plot(CPUE$Homarus.americanus[19:35], type = 'l', ylab = '')

plot(CPUE$Loligo.pealeii[1:18], type = 'l', ylab = 'Long finned squid')
plot(CPUE$Loligo.pealeii[19:35], type = 'l', ylab = '')

plot(CPUE$Melanogrammus.aeglefinus[1:18], type = 'l', ylab = 'Haddock')
plot(CPUE$Melanogrammus.aeglefinus[19:35], type = 'l', ylab = '')

plot(CPUE$Merluccius.bilinearis[1:18], type = 'l', ylab = 'Whiting')
plot(CPUE$Merluccius.bilinearis[19:35], type = 'l', ylab = '')

plot(CPUE$Myoxocephalus.octodecemspinosus[1:18], type = 'l', ylab = 'Longhorn sculpin')
plot(CPUE$Myoxocephalus.octodecemspinosus[19:35], type = 'l', ylab = '')

plot(CPUE$Osmerus.mordax[1:18], type = 'l', ylab = 'Rainbow smelt')
plot(CPUE$Osmerus.mordax[19:35], type = 'l', ylab = '')

plot(CPUE$Pandalus.borealis[1:18], type = 'l', ylab = 'Northern shrimp', main = "Fall (total #/year)")
plot(CPUE$Pandalus.borealis[19:35], type = 'l', ylab = '', main = "Spring (total #/year)")

plot(CPUE$Pandalus.montagui[1:18], type = 'l', ylab = 'Montagui shrimp')
plot(CPUE$Pandalus.montagui[19:35], type = 'l', ylab = '')

plot(CPUE$Peprilus.triacanthus[1:18], type = 'l', ylab = 'Butterfish')
plot(CPUE$Peprilus.triacanthus[19:35], type = 'l', ylab = '')

plot(CPUE$Placopecten.magellanicus[1:18], type = 'l', ylab = 'Sea scallop')
plot(CPUE$Placopecten.magellanicus[19:35], type = 'l', ylab = '')

plot(CPUE$Pseudopleuronectes.americanus[1:18], type = 'l', ylab = 'Winter flounder')
plot(CPUE$Pseudopleuronectes.americanus[19:35], type = 'l', ylab = '')

plot(CPUE$Scomber.scombrus[1:18], type = 'l', ylab = 'Atlantic mackerel')
plot(CPUE$Scomber.scombrus[19:35], type = 'l', ylab = '')

plot(CPUE$Scophthalmus.aquosus[1:18], type = 'l', ylab = 'Windowpane')
plot(CPUE$Scophthalmus.aquosus[19:35], type = 'l', ylab = '')

plot(CPUE$Sebastes.fasciatus[1:18], type = 'l', ylab = 'Acadian redfish')
plot(CPUE$Sebastes.fasciatus[19:35], type = 'l', ylab = '')

plot(CPUE$Squalus.acanthias[1:18], type = 'l', ylab = 'Spiny dogfish', main = "Fall (total #/year)")
plot(CPUE$Squalus.acanthias[19:35], type = 'l', ylab = '', main = "Spring (total #/year)")

plot(CPUE$Urophycis.chuss[1:18], type = 'l', ylab = 'Red hake')
plot(CPUE$Urophycis.chuss[19:35], type = 'l', ylab = '')

plot(CPUE$Urophycis.tenuis[1:18], type = 'l', ylab = 'White hake')
plot(CPUE$Urophycis.tenuis[19:35], type = 'l', ylab = '')




# species in the Gulf of Maine Georges Bank over time # for SNEC ##
setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/NEFSC")
load('survdat_FL.RData')

# subset strata
GOM_fall <- survdat_FL[survdat_FL$STRATUM == 1130 | survdat_FL$STRATUM == 1140 | survdat_FL$STRATUM == 1150 | survdat_FL$STRATUM == 1160 |
                        survdat_FL$STRATUM == 1170 | survdat_FL$STRATUM == 1180 | survdat_FL$STRATUM == 1190 | survdat_FL$STRATUM == 1200 |
                        survdat_FL$STRATUM == 1210 | survdat_FL$STRATUM == 1220 | survdat_FL$STRATUM == 1230 | survdat_FL$STRATUM == 1240 |
                        survdat_FL$STRATUM == 1250 | survdat_FL$STRATUM == 1260 | survdat_FL$STRATUM == 1270 |
                        survdat_FL$STRATUM == 1280 | survdat_FL$STRATUM == 1290 | survdat_FL$STRATUM == 1300 | survdat_FL$STRATUM == 1360 |
                        survdat_FL$STRATUM == 1370 | survdat_FL$STRATUM == 1380 | survdat_FL$STRATUM == 1390 | survdat_FL$STRATUM == 1400,]
write.csv( GOM_fall, "GOM_fall_spp.csv")
# cod per haul
#cols <- c(3, 43)
#GOM_fall$GMRI_INDICATOR <-  cumsum(!duplicated(GOM_fall[cols])) 


cod <- GOM_fall[GOM_fall$COMNAME == "ATLANTIC COD",]

diff_yrs <- matrix(NA) 
yrs <- unique(GOM_fall$EST_YEAR)
ann_cod <- NULL

for (i in 1:length(yrs)){
  diff_yrs <- cod[which(cod$EST_YEAR == yrs[i]),] 
  ann_cod[i] <- sum(diff_yrs$NUMLEN)
}

# created a pivot table 
#GOM_fall_spp.csv

