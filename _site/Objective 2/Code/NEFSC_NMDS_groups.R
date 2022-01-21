#load libraries and set working directory 
library(tidyverse)
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/NEFSC")

#lad in new data updated for 2020 with corrections
load('NEFSC_BTS_all_seasons_03032021.Rdata')
survdat<-survey$survdat
head(survdat)
names <- as.data.frame(unique(survdat$COMNAME))
#write.csv(names, "uniq_NAMES.csv")

####remove observations that are not to the spp level####
#test <- survdat[!survdat$COMNAME == "NA",]
test<-as.data.frame(table(survdat$COMNAME))
test$COMNAME<-test$Var1
survdat <- survdat[!survdat$COMNAME == "HOOKEAR SCULPIN UNCL",]
survdat <- survdat[!survdat$COMNAME == "SKATE UNCL",]
survdat <- survdat[!survdat$COMNAME == "HAKE UNCL",]
survdat <- survdat[!survdat$COMNAME == "GRENADIER UNCL",]
survdat <- survdat[!survdat$COMNAME == "SCALY DRAGONFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "EEL UNCL",]
survdat <- survdat[!survdat$COMNAME == "SHRIMP UNCL",]
survdat <- survdat[!survdat$COMNAME == "SQUID, CUTTLEFISH, AND OCTOPOD UNCL",]
survdat <- survdat[!survdat$COMNAME == "LING UNCL",]
survdat <- survdat[!survdat$COMNAME == "CANCER CRAB UNCL",]
survdat <- survdat[!survdat$COMNAME == "LANTERNFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "GREENEYE UNCL",]
survdat <- survdat[!survdat$COMNAME == "WATER HAUL",]
survdat <- survdat[!survdat$COMNAME == "FLOUNDER UNCL",]
survdat <- survdat[!survdat$COMNAME == "RIGHTEYE FLOUNDER UNCL",]
survdat <- survdat[!survdat$COMNAME == "LEFTEYE FLOUNDER UNCL",]
survdat <- survdat[!survdat$COMNAME == "OCTOPUS UNCL",]
survdat <- survdat[!survdat$COMNAME == "UNCLASSIFIED FISH",]
survdat <- survdat[!survdat$COMNAME == "TONGUEFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "CARDINALFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "MORA UNCL",]
survdat <- survdat[!survdat$COMNAME == "JACK POMPANO UNCL",]
survdat <- survdat[!survdat$COMNAME == "CUSK-EEL UNCL",]
survdat <- survdat[!survdat$COMNAME == "TRIGGERFISH FILEFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "SEAROBIN UNCL",]
survdat <- survdat[!survdat$COMNAME == "SHARK UNCL",]
survdat <- survdat[!survdat$COMNAME == "ANCHOVY UNCL",]
survdat <- survdat[!survdat$COMNAME == "RAY AND SKATE UNCL",]
survdat <- survdat[!survdat$COMNAME == "BUTTERFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "SPIDER CRAB UNCL",]
survdat <- survdat[!survdat$COMNAME == "HERRING UNCL",]
survdat <- survdat[!survdat$COMNAME == "HEADLIGHTFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "SCORPIONFISH AND ROCKFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "LIZARDFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "HATCHETFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "SEA BASS UNCL",]
survdat <- survdat[!survdat$COMNAME == "MOJARRA UNCL",]
survdat <- survdat[!survdat$COMNAME == "PARALICHTHYS UNCL",]
survdat <- survdat[!survdat$COMNAME == "CORNETFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "STARGAZER UNCL",]
survdat <- survdat[!survdat$COMNAME == "ETROPUS UNCL",]
survdat <- survdat[!survdat$COMNAME == "BATFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "DRUM UNCL",]
survdat <- survdat[!survdat$COMNAME == "PORGY AND PINFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "WRASSE UNCL",]
survdat <- survdat[!survdat$COMNAME == "GRUNT UNCL",]
survdat <- survdat[!survdat$COMNAME == "SNAKE EEL UNCL",]
survdat <- survdat[!survdat$COMNAME == "PARROTFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "SPOON-NOSE EEL UNCL",]
survdat <- survdat[!survdat$COMNAME == "REQUIEM SHARK UNCL",]
survdat <- survdat[!survdat$COMNAME == "SNAPPER UNCL",]
survdat <- survdat[!survdat$COMNAME == "MORAY UNCL",]
survdat <- survdat[!survdat$COMNAME == "PUFFER UNCL",]
survdat <- survdat[!survdat$COMNAME == "CONGER EEL UNCL",]
survdat <- survdat[!survdat$COMNAME == "MACKEREL AND TUNA UNCL",]
survdat <- survdat[!survdat$COMNAME == "COMBTOOTH BLENNY UNCL",]
survdat <- survdat[!survdat$COMNAME == "GOBY UNCL",]
survdat <- survdat[!survdat$COMNAME == "SCULPIN UNCL",]
survdat <- survdat[!survdat$COMNAME == "WHIFF UNCL",]
survdat <- survdat[!survdat$COMNAME == "BOX CRAB UNCL",]
survdat <- survdat[!survdat$COMNAME == "GALATHEID UNCL",]
survdat <- survdat[!survdat$COMNAME == "SWIMMING CRAB UNCL",]
survdat <- survdat[!survdat$COMNAME == "LIGHTFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "WHITING UNCL",]
survdat <- survdat[!survdat$COMNAME == "LUMPFISH SNAILFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "SEA STAR, BRITTLE STAR, AND BASKETSTAR UNCL",]
survdat <- survdat[!survdat$COMNAME == "BASSLET UNCL",]
survdat <- survdat[!survdat$COMNAME == "DAMSELFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "SAND DOLLAR UNCL",]
survdat <- survdat[!survdat$COMNAME == "SEA URCHIN AND SAND DOLLAR UNCL",]
survdat <- survdat[!survdat$COMNAME == "BARRACUDA UNCL",]
survdat <- survdat[!survdat$COMNAME == "PIPEFISH SEAHORSE UNCL",]
survdat <- survdat[!survdat$COMNAME == "MOON SNAIL, SHARK EYE, AND BABY-EAR UNCL",]
survdat <- survdat[!survdat$COMNAME == "HERMIT CRAB UNCL",]
survdat <- survdat[!survdat$COMNAME == "NORTHERN SHORTFIN SQUID EGG MOPS",]
survdat <- survdat[!survdat$COMNAME == "SQUIRRELFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "CALICO CRAB UNCL",]
survdat <- survdat[!survdat$COMNAME == "SHRIMP (PINK,BROWN,WHITE)",]
survdat <- survdat[!survdat$COMNAME == "MANTIS SHRIMP UNCL",]
survdat <- survdat[!survdat$COMNAME == "EELPOUT UNCL",]
survdat <- survdat[!survdat$COMNAME == "SILVERSIDE UNCL",]
survdat <- survdat[!survdat$COMNAME == "BUTTERFLYFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "BARRACUDINA UNCL",]
survdat <- survdat[!survdat$COMNAME == "SNAKE MACKEREL UNCL",]
survdat <- survdat[!survdat$COMNAME == "BOARFISH UNCL",]
survdat <- survdat[!survdat$COMNAME == "LOPHIIFORM UNCL",]
survdat <- survdat[!survdat$COMNAME == "CLINID UNCL",]
survdat <- survdat[!survdat$COMNAME == "SLICKHEAD UNCL",]
survdat <- survdat[!survdat$COMNAME == "BOBTAIL UNCL",]
survdat <- survdat[!survdat$COMNAME == "LONGFIN SQUID EGG MOPS",]
survdat <- survdat[!survdat$COMNAME == "UNKNOWN 01",]
survdat <- survdat[!survdat$COMNAME == "UNKNOWN 02",]
survdat <- survdat[!survdat$COMNAME == "UNKNOWN 03",]
survdat <- survdat[!survdat$COMNAME == "UNKNOWN 04",]
survdat <- survdat[!survdat$COMNAME == "UNKNOWN 05",]
survdat <- survdat[!survdat$COMNAME == "TRASH",]
survdat <- survdat[!survdat$COMNAME == "UNKNOWN 06",]
survdat <- survdat[!survdat$COMNAME == "UNKNOWN 07",]
survdat <- survdat[!survdat$COMNAME == "UNKNOWN 08",]
survdat <- survdat[!survdat$COMNAME == "UCLASSIFIED FISH",]
survdat <- survdat[!survdat$COMNAME == "DOGFISH,UNC",]
survdat <- survdat[!survdat$COMNAME == "NA",]
survdat <- survdat[!is.na(survdat$COMNAME),]
survdat <- survdat[!is.na(survdat$ABUNDANCE),] #202 observations w/0 abundance

# remove surveys in SUMMER and WINTER
survdat <- survdat[!survdat$SEASON == "SUMMER", ]
survdat <- survdat[!survdat$SEASON == "WINTER", ]
#write.csv(survdat, "nefsc_data.csv")

# spp over time by season to use below
survdat_FL <- survdat[survdat$SEASON == "FALL",]
survdat_SP <- survdat[survdat$SEASON == "SPRING",]
save(survdat_FL, file = "survdat_FL.RData")
save(survdat_SP, file = "survdat_SP.RData")
load('survdat_FL.RData')
load('survdat_SP.RData')

#load data for functional groups
survdat<-read.csv("nefsc_data.csv")
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/ID Grouping")
nefsc_groups<-read.csv("nefsc_species.csv")
#check which species are not grouped in nefsc data and fix
anti_join(test,nefsc_groups,by="COMNAME")
survdat$COMNAME[survdat$COMNAME=="CRAB,RED DEEPSEA"]<-"RED DEEPSEA CRAB"
survdat$COMNAME[survdat$COMNAME=="MENHADEN"]<-"ATLANTIC MENHADEN"
survdat$COMNAME[survdat$COMNAME=="SUMMER FLOUNDERS"]<-"SUMMER FLOUNDER"
#add the groups by common name
survdat_groups<-left_join(survdat, nefsc_groups, by="COMNAME")
survdat_groups$functional_group[survdat_groups$functional_group==""]<-"undefined"


#take out candada and SEFSC strata
strata.ca<- c(01350, 1351, 1310, 1320, 1410, 1420, 1490, 1990, 1410, 1420, 1490, 5440, 5480, 5430) # Canada
strata.se<- unique(survdat$STRATUM[survdat$STRATUM > 3990]) #South east strata and other survey strata
strata.delete<- c(strata.ca, strata.se)
survdat_ne<- survdat_groups[!survdat_groups$STRATUM %in% strata.delete,]

#only spring 2020 was conducted so removing the last year
survdat_exclude<-filter(survdat_ne, YEAR<2020)

#rearrange #s per tow instead of each individual
survdat_tow<-group_by(survdat_exclude,YEAR,SEASON,STRATUM)%>%
  mutate(tows=n_distinct(TOW))%>%
  group_by(YEAR,SEASON,STRATUM,functional_group)%>%
  summarise(weight=sum(BIOMASS, na.rm=TRUE)/tows, abundance=sum(ABUNDANCE, na.rm=TRUE)/tows)%>%
  unique()
  
