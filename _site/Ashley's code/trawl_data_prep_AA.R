# Preliminaries -----------------------------------------------------------
# Source helper functions
source("./Code/HelperFunctions.R")

# Load libraries, using package_check to download if it is missing
packages.needed<- c("tidyverse", "sp", "raster", "geosphere")
package_check(packages.needed)

# Load in the data
load(paste("./Data/", survdat.file, sep = ""))

# Data cleaning -----------------------------------------------------------
# Remove tows from Canadian waters and SEFSC 
strata.ca<- c(01350, 1351, 1310, 1320, 1410, 1420, 1490, 1990, 1410, 1420, 1490, 5440, 5480, 5430) # Canada
strata.se<- unique(survdat$STRATUM[survdat$STRATUM > 3990]) #South east strata and other survey strata
strata.delete<- c(strata.ca, strata.se)
t1<- survdat[!survdat$STRATUM %in% strata.delete,]

# There are duplicated rows for tow, species, biomass, abundance, catchsex for different lengths of individuals. We are only interested in variability in biomass for this work. So, we can reduce the dataset by ignoring these repeating rows.
t2<- t1[!duplicated(t1[c("ID", "SVSPP", "CATCHSEX")]),]

# Now, we have unique tow-species-biomass-abundance records for tows were a species was caught. However, we also need to abesences, so we need biomass and abundance of zero for each species when they were not caught at a specific tow.
# First, we create a null dataset for merging with all species and all trawls
null.dat<- expand.grid("ID" = unique(t2$ID), "SVSPP" = unique(t2$SVSPP))

# Next, create a dataset to match the null but that has species - biomass - abundance for tows where a species was caught.
dat.agg.r<- t2 %>%
  group_by(., ID, SVSPP) %>%
  summarise(., BIOMASS = sum(BIOMASS, na.rm = T), ABUNDANCE = sum(ABUNDANCE, na.rm = T))

# Merge these together to add in null dataset (absences) for tows where a species was not caught
dat.agg.f<- full_join(dat.agg.r, null.dat)

# Convert NAs to 0, these are where both biomass and abundance are NA
dat.agg.f$bio.abund.na<- ifelse(is.na(dat.agg.f$ABUNDANCE) & is.na(dat.agg.f$BIOMASS), "NA", "GOOD")

# Okay, we can make BIOMASS and ABUNDANCE zero where both are NA
dat.agg.f$BIOMASS[is.na(dat.agg.f$bio.abund.na)]<- 0
dat.agg.f$ABUNDANCE[is.na(dat.agg.f$bio.abund.na)]<- 0

# Finally, merge back in the observations with the tow characteristics dataset
tow.dat<- t2[,c(1:29, 35:43)]
tow.dat<- tow.dat[!duplicated(tow.dat$ID),] # Remove duplicate rows

# Merge tow.dat with dat.agg based on "ID"
dat.full<- full_join(tow.dat, dat.agg.f, by = c("ID"))
