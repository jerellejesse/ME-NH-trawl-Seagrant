####################################################################################
### Biodiversity metrics to be applied to NEFSC survey data
### UM Seagrant; climate impacts on Maine coastal fisheries using the ME-NH inshore trawl
### Objective 1 part 1
### 7/31/2018
### A.E. Weston
####################################################################################

# AEW first 4 inds did not remove 1 spp hauls - did for last 4
# SPP ID "X UNCL" = not to spp level
# there are observations in winter and summer seasons 
# not on a tow by tow basis
# column header explanations in "SVDBS Elements by Table and Column" doc
# status code = all 10
# statype = 1 or NA
# vessels; AL DE AT HB PC
# abundance is adjusted for vessel - numlen is not 
# The survey attempts to have a standard tow time which changed between Albatross and Bigelow 
 # (which is accounted for in the calibration factors for weight and abundance).
#write.csv(survdat, "survdat.csv")

#setwd("J:/Research/Kerr Lab/ME_SEAG_2018/NEFSC_trawl_07_31_18")
#setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/NEFSC")
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/NEFSC")

#load('Survdat_Nye_allseason.RData') #survdat
load('NEFSC_BTS_all_seasons_03032021.Rdata')
survdat<-survey$survdat
head(survdat)
#names <- unique(survdat$COMNAME) #651
#write.csv(names, "uniq_NAMES.csv")

####remove observations that are not to the spp level####
#test <- survdat[!survdat$COMNAME == "NA",]
test<-as.data.frame(table(survdat$COMNAME))
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
survdat <- survdat[!is.na(survdat$COMNAME),]
survdat <- survdat[!is.na(survdat$ABUNDANCE),] #202 observations w/0 abundance

# remove surveys in SUMMER and WINTER
survdat <- survdat[!survdat$SEASON == "SUMMER", ]
survdat <- survdat[!survdat$SEASON == "WINTER", ]
write.csv(survdat, "nefsc_data.csv")

# spp over time by season to use below
survdat_FL <- survdat[survdat$SEASON == "FALL",]
survdat_SP <- survdat[survdat$SEASON == "SPRING",]
save(survdat_FL, file = "survdat_FL.RData")
save(survdat_SP, file = "survdat_SP.RData")
load('survdat_FL.RData')
load('survdat_SP.RData')


# create unique ID that is year by station to calculate metrics  for each season
# cols <- c(3, 43)
# survdat_FL$GMRI_INDICATOR <-  cumsum(!duplicated(survdat_FL[cols])) 
# survdat_SP$GMRI_INDICATOR <-  cumsum(!duplicated(survdat_SP[cols])) 

survdat<-read.csv("nefsc_data.csv")
nefsc_species<-as.data.frame(unique(survdat$COMNAME))
#write.csv(nefsc_species, "nefsc_species.csv")

nefsc_groups<-read.csv("nefsc_species.csv")

survdat_2<-left_join(survdat, nefsc_groups, by="COMNAME")
survdat_groups<-group_by(survdat_2, EST_YEAR, STATION, functional_group)%>%
  mutate(INDICATOR=cur_group_id())

#####functional group plots#####
survdat_2_sum<-filter(survdat_2, BIOMASS!=0)%>%
  group_by(EST_YEAR, functional_group)%>%
  summarise(biomass_kg=sum(BIOMASS, na.rm=TRUE)) #sum of weights from all 20 minute hauls

ggplot()+geom_line(data=survdat_2_sum, aes(x=factor(EST_YEAR), y=biomass_kg), group=1)+facet_grid(functional_group~.)+ 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year")+ylab("Biomass")+theme(axis.title.x=element_text(size=14), axis.title.y = element_text(size=14))+
  theme(axis.text.x=element_text(size=12), axis.text.y = element_text(size=12))+
  scale_x_discrete(breaks=seq(1963,2018, 5))+
  theme(strip.background = element_rect(fill="white"), strip.text = element_text(size=12))

#by season
survdat_2_season_sum<-filter(survdat_2, BIOMASS!=0)%>%
  group_by(EST_YEAR, SEASON,functional_group)%>%
  summarise(biomass_kg=sum(BIOMASS, na.rm=TRUE)) #sum of weights from all 20 minute hauls

ggplot()+geom_line(data=survdat_2_season_sum, aes(x=factor(EST_YEAR), y=biomass_kg), group=1)+facet_grid(functional_group~SEASON)+ 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year")+ylab("Biomass")+theme(axis.title.x=element_text(size=14), axis.title.y = element_text(size=14))+
  theme(axis.text.x=element_text(size=12), axis.text.y = element_text(size=12))+
  scale_x_discrete(breaks=seq(1963,2018, 7))+
  theme(strip.background = element_rect(fill="white"), strip.text = element_text(size=12))

#####biodiversity metric functional groups#####
survdat_groups<-group_by(survdat_2, DMR_TRIP_IDENTIFIER, DMR_EFFORT_IDENTIFIER, functional_group)%>%
  mutate(N_species=length(unique(SCIENTIFIC_NAME)),
         total_spp=sum(W_NUM),
         spp_prop=W_NUM/total_spp,
         H_index=-1*(sum(spp_prop*log(spp_prop))),
         D_index=1/(sum(spp_prop^2)),
         E_index= D_index*(1/N_species)
  )%>%
  left_join(tow, by=c("DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER"))

#### calculate species richness, Shannon-Weiner diversity index, Simpson's diversity index, and Simpson's evenness index by haul####
diff_hauls <- matrix(NA) 
N_species <- NULL
N_each_spp <- NULL
total_spp <- NULL
spp_prop <- NULL
H_index <- NULL  
D_index <- NULL 
E_index <- NULL 
hauls <- unique(survdat_groups$INDICATOR) # haul within each unique year/season 
N_hauls <- length(hauls)# number of hauls
for (i in 1:N_hauls) { #loop through each haul within year/season
  diff_hauls <- survdat_groups[which(survdat_groups$INDICATOR == i),] #subset unique hauls
  N_species[i] <- length(unique(diff_hauls$COMNAME)) # count the number of unique species
  N_each_spp <- aggregate(diff_hauls$ABUNDANCE ~ diff_hauls$COMNAME, FUN = mean) #diff_hauls$NUM #need to sum across lengths - values aren't the same 
  total_spp <- sum(N_each_spp$`diff_hauls$ABUNDANCE`)
  spp_prop <- N_each_spp$`diff_hauls$ABUNDANCE`/total_spp # calculate proportion of each species in a haul
  H_index[i] <- -1*(sum(spp_prop*log(spp_prop))) # multiplied by -1 to get non-negative values
  D_index[i] <- 1/(sum(spp_prop^2)) 
  E_index[i] <- D_index[i]*(1/N_species[i]) 
}

#create new tow by tow information file 
hauls <- unique(survdat_groups$INDICATOR) # haul within each unique year/season 
N_hauls <- length(hauls) # number of hauls
by_tow <- matrix(NA, nrow = N_hauls, ncol = 48)
for (i in 1:N_hauls) { #loop through each haul within year/season
  diff_hauls <- survdat_groups[which(survdat_groups$INDICATOR == i),] #subset unique hauls
  by_tow[i,] <- as.matrix(diff_hauls[1,1:48])
}

colnames(by_tow) <- c("X","ID", "STATUS_CODE", "EST_YEAR", "EST_MONTH", "EST_DAY", "EST_JULIAN_DAY", "EST_TIME", 
"SEASON", "SVVESSEL","STATYPE", "TOWDUR", "RPM", "DOPDISTB", "DOPDISTW", "BOTSPEED", "BOTTEMP", "SURFTEMP", 
"BOTSALIN", "SURFSALIN", "MINDEPTH", "MAXDEPTH", "AVGDEPTH", "SETDEPTH", "ENDDEPTH", "STRATUM", 
"DECDEG_BEGLAT", "DECDEG_ENDLAT", "DECDEG_BEGLON", "DECDEG_ENDLON", "SVSPPP", "CATCHSEX", "COMNAME", 
"BIOMASS", "ABUNDANCE", "AREA", "AIRTEMP", "CLOUD", "BAROPRESS", "WINDDIR", "WINDSP", "WAVEHGT", "CRUISE6", 
"STATION", "LENGTH", "NUMLEN", "GMRI_INDICATOR", "functional_group")
by_tow <- as.data.frame(by_tow)
by_tow <- by_tow[,-c(30, 31, 32, 33, 34, 44, 45)]

nefsc_by_tow <- cbind(by_tow, N_species, H_index, D_index, E_index)
#write.csv(nefsc_by_tow, "nefsc_SP_by_tow.csv")
#nefsc_by_tow <- read.csv('nefsc_by_tow.csv', header = TRUE)

############## taxonomic diversity ####
########build out survey matrix with species, genus, family, order, class, phylum for each observation - use svspp?
library(taxize)
library(purrr)
library(mgcv)

com_name <- as.vector(survdat$COMNAME)  
diff_com_name <- unique(com_name) # all unique species names 
tax <- classification(diff_com_name, db = 'itis') #get classification information for all species observed  

info <- matrix(NA)
expand <- matrix(NA)
specific <- matrix(NA, nrow = length(diff_com_name), ncol = 6)
for (i in 1:length(tax)){
  info <- tax[[i]][c('name','rank')] # gives classification and rank for each species
  expand <- info[info$rank == 'phylum'| info$rank == 'class'| info$rank == 'order' | info$rank == 'family' | info$rank == 'genus' | info$rank == 'species',]
  specific[i,] <- as.vector(expand$name)
}

#when all info doesn't show
#name <- c("Animalia", "Mollusca", "Cephalopoda", "Myopsida", "Loliginidae", "Loligo", "Loligo pealeii")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Stomiiformes", "Sternoptychidae", "Maurolicus", "Maurolicus weitzmani")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Myctophiformes", "Myctophidae", "Diaphus", "Diaphus dumerilii")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Gadiformes", "Macrouridae", "Coelorhynchus", "Coelorhynchus carminatus")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Trichiuridae", "Benthodesmus", "Benthodesmus simonyi")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Ariommatidae", "Ariomma", "Ariomma bondi")
#name <- c("Animalia", "Chordata", "Elasmobranchii", "Carcharhiniformes", "Sphyrnidae", "Sphyrna", "Sphyrna lewini")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Anguilliformes", "Synaphobranchidae", "Simenchelys", "Simenchelys parasitica")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Sciaenidae", "Leiostomus", "Leiostomus xanthurus")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Scombridae", "Scomberomorus", "Scomberomorus regalis")
#name <- c("Animalia", "Chordata", "Elasmobranchii", "Carcharhiniformes", "Sphyrnidae", "Sphyrna", "Sphyrna tiburo")
#name <- c("Animalia", "Chordata", "Elasmobranchii", "Carcharhiniformes", "Sphyrnidae", "Sphyrna", "Sphyrna mokarran")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Beryciformes", "Trachichthyidae", "Hoplostethus", "Hoplostethus mediterraneus")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Sciaenidae", "Equetus", "Equetus acuminatus")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Myctophiformes", "Myctophidae", "Myctophum", "Myctophum humboldti")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Carangidae", "Caranx", "Caranx ruber")
#name <- c("Animalia", "Chordata", "Elasmobranchii", "Carcharhiniformes", "Sphyrnidae", "Sphyrna", "Sphyrna zygaena")
#name <- c("Animalia", "Chordata", "Reptilia", "Testudines", "Cheloniidae", "Caretta", "Caretta caretta")
#name <- c("Animalia", "Arthropoda", "Malacostraca", "Decapoda", "Lithodidae", "Lithodes", "Lithodes maja")
#name <- c("Animalia", "Mollusca", "Cephalopoda", "Sepiida", "Sepiolidae", "Stoloteuthis", "Stoloteuthis leucoptera")
#name <- c("Animalia", "Mollusca", "Cephalopoda", "Oegopsida", "Enoploteeuthidae", "Abralia", "Abralia veranyi")
#name <- c("Animalia", "Chordata", "Reptilia", "Testudines", "Dermochelyidae", "Dermochelys", "Dermochelys coriacea")
#name <- c("Animalia", "Chordata", "Elasmobranchii", "Lamniformes", "Alopiidae", "Alopias", "Alopias superciliousus")
#name <- c("Animalia", "Echinodermata", "Asteroidea", "Forcipulatida", "Asteriidae", "Asterias", "Asterias vulgaris")
#name <- c("Animalia", "Chordata", "Elasmobranchii", "Lamniformes", "Lamnidae", "Lamna", "Lamna nasus")
#name <- c("Animalia", "Chordata", "Reptilia", "Testudines", "Cheloniidae", "Lepidochelys", "Lepidochelys kempii")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Gadiformes", "Phycidae", "Urophycis", "Urophycis chesteri")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Stichaeidae", "Leptoclinus", "Leptoclinus maculatus")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Tetradontiformes", "Monacanthidae", "Monacanthus", "Monacanthus hispidus")
#name <- c("Animalia", "Arthropoda", "Malacostraca", "Decapoda", "Geryonidae", "Geryon", "Geryon quinquedens")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Syngnathiformes", "Centriscidae", "Macroramphosus", "Macroramphosus scolopax")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Ophidiiformes", "Ophidiidae", "Otophidium", "Otophidium omostigmum")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Labridae", "Xyrichtys", "Xyrichtys novacula")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Sciaenidae", "Equetus", "Equetus umbrosus")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Tetradontiformes", "Ostraciidae", "Lactophrys", "Lactophrys quadricornis")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Pleuronectiformes", "Paralichthyidae", "Ancylopsetta", "Ancylopsetta quadrocellata")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Stromateidae", "Peprilus", "Peprilus alepidotus")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Tetradontiformes", "Ostraciidae", "Lactophrys", "Lactophrys polygonia")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Pomacentridae", "Chromis", "Chromis enchrysurus")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Carangidae", "Carangoides", "Carangoides bartholomaei")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Ophidiiformes", "Ophidiidae", "Ophidion", "Ophidion welshi")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Pleuronectiformes", "Cynoglossidae", "Symphurus", "Symphurus civitatus")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Ophidiiformes", "Ophidiidae", "Ophidion", "Ophidion beani")
#name <- c("Animalia", "Chordata", "Elasmobranchii", "Rajiformes", "Rajidae", "Breviraja", "Breviraja plutonia")
#name <- c("Animalia", "Arthropoda", "Malacostraca", "Decapoda", "Geryonidae", "Geryon", "Geryon fenneri")
#name <- c("Animalia", "Arthropoda", "Malacostraca", "Decapoda", "Penaeidae", "Penaeus", "Penaeus duorarum")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Chaetodontidae", "Chaetodon", "Chaetodon aculeatus")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Chaetodontidae", "Chaetodon", "Chaetodon aya")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Istiophoridae", "Tetrapturus", "Tetrapturus albidus")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Serranidae", "Pronotogrammus", "Pronotogrammus martinicensis")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Priacanthidae", "Priacanthus", " Priacanthus cruentatus")
#name <- c("Animalia", "Arthropoda", "Malacostraca", "Decapoda", "Penaeidae", "Penaeus", "Penaeus aztecus")
#name <- c("Animalia", "Chordata", "Actinopterygii", "Tetradontiformes", "Diodontidae", "Chilomycterus", "Chilomycterus reticulatus")
name <- c("Animalia", "Arthropoda", "Malacostraca", "Decapoda", "Thoridae", "Spirontocaris", "Spirontocaris liljeborgii")


rank <- c("kindom", "phylum", "class", "order", "family", "genus", "species")
id <- seq(1, 7, by = 1)
#tax$`LONGFIN SQUID` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`WEITZMANS PEARLSIDES` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$HEADLIGHTFISH <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`LONGNOSE GRENADIER` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`SIMONYS FROSTFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`SILVER RAG` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`SCALLOPED HAMMERHEAD SHARK`<- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`SNUBNOSE EEL` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$SPOT <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$CERO <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`BONNETHEAD SHARK` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`GREAT HAMMERHEAD SHARK` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`BLACKMOUTHED ALFONSIN` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`HIGH-HAT` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`HUMBOLDTS LANTERNFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`BAR JACK` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`SMOOTH HAMMERHEAD SHARK` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`LOGGERHEAD SEATURTLE` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`NORTHERN STONE CRAB` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`SHIELD BOBTAIL` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`RUPPELL'S ABRALIA` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`LEATHERBACK SEATURTLE` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`BIGEYE THRESHER SHARK` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`BOREAL ASTERIAS` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`PORBEAGLE SHARK` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`KEMP'S RIDLEY SEATURTLE` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`LONGFIN HAKE` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`DAUBED SHANNY` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`PLANEHEAD FILEFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`RED DEEPSEA CRAB` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`LONGSPINE SNIPEFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`POLKA-DOT CUSK-EEL` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`PEARLY RAZORFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$CUBBYU <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`SCRAWLED COWFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`OCELLATED FLOUNDER` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$HARVESTFISH <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`HONEYCOMB COWFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`YELLOWTAIL REEFFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`YELLOW JACK` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`CRESTED CUSK-EEL` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`OFFSHORE TONGUEFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`LONGNOSE CUSK-EEL` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`LONGTAIL SKATE` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`GOLDEN DEEPSEA CRAB` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`PINK SHRIMP` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`LONGSNOUT BUTTERFLYFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`BANK BUTTERFLYFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`WHITE MARLIN` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`ROUGHTONGUE BASS` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`GLASSEYE SNAPPER` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`BROWN SHRIMP` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`SPOTTED BURRFISH` <- data.frame("name" = name, "rank" = rank, "id" = id)
#tax$`FRIENDLY BLADE SHRIMP` <- data.frame("name" = name, "rank" = rank, "id" = id)

specific_2 <- cbind(specific, diff_com_name)
colnames(specific_2) <- c("Phylum", "Class", "Order", "Family", "Genus", "Species", "Common_name")
write.csv(specific_2, "nefsc_phylo.csv")


### loop through trawl survey to expand each sample based on above taxonomic information
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/NEFSC")
phylo <- read.csv("nefsc_phylo.csv", header = TRUE)
phylo <- as.data.frame(phylo)
com_name <- as.vector(survdat$COMNAME)  
expand_all <- matrix(NA, nrow = nrow(survdat), ncol = 7)
for (i in 1:nrow(survdat)) {
  expand_all[i,] <- as.matrix(phylo[phylo[,"Common_name"] == com_name[i],]) 
}

colnames(expand_all) <- c("Phylum", "Class", "Order", "Family", "Genus", "Species", "Common")

write.csv(expand_all, "expanded_NEFSC_phylo.csv", row.names = FALSE) #save expanded survey phylogenetic information

# save whole new expanded trawl
survdat_exp <- cbind(survdat, expand_all)
save(survdat_exp, file = "survdat_exp.RData")

################ now that survey data is reformatted calculate taxonomic diversity indices #######
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/NEFSC")
load('survdat_exp.RData')


survdat_exp_FL <- survdat_exp[survdat_exp$SEASON == "FALL",]
survdat_exp_SP <- survdat_exp[survdat_exp$SEASON == "SPRING",]

cols <- c(3, 43)
survdat_exp_FL$GMRI_INDICATOR_2 <-  cumsum(!duplicated(survdat_exp_FL[cols])) 
survdat_exp_SP$GMRI_INDICATOR_2 <-  cumsum(!duplicated(survdat_exp_SP[cols])) 

#remove hauls that only have 1 spp 
nefsc_FL_by_tow <- read.csv('nefsc_FL_by_tow.csv', header = TRUE)
nefsc_SP_by_tow <- read.csv('nefsc_SP_by_tow.csv', header = TRUE)

low_N_fl <- nefsc_FL_by_tow[nefsc_FL_by_tow$N_species == 1, 40] 
low_N_sp <- nefsc_SP_by_tow[nefsc_SP_by_tow$N_species == 1, 40] 

survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 296),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 428),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 524),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 753),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 1238),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 1464),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 1519),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 1586),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 1588),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 2946),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 2964),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 4252),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 4847),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 5651),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 5658),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 6051),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 6297),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 6404),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 6547),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 6601),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 6719),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 6720),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 6897),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 6976),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 7947),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 8826),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 10915),]
survdat_exp_FL <- survdat_exp_FL[(!survdat_exp_FL$GMRI_INDICATOR_2 == 11520),]

survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 63),]
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 230),]
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 466),]
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 876),]
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 897),]
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 914),]
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 1425),]
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 1747),]
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 3280),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 3316),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 3421),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 3866),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 4751),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 5227),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 5400),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 5577),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 5579),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 5580),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 5581),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 5585),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 5940),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 5988),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 7667),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 7974),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 8910),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 8936),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 9548),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 11310),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 11854),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 15987),] 
survdat_exp_SP <- survdat_exp_SP[(!survdat_exp_SP$GMRI_INDICATOR_2 == 17095),] 

#create new sequence of indicator values to loop over 
cols <- c(3, 43)
survdat_exp_FL$GMRI_INDICATOR_3 <-  cumsum(!duplicated(survdat_exp_FL[cols])) 
survdat_exp_SP$GMRI_INDICATOR_3 <-  cumsum(!duplicated(survdat_exp_SP[cols])) 


library(mgcv)

hauls <- unique(survdat_exp_FL$GMRI_INDICATOR_3)   
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
  diff_hauls <- survdat_exp_FL[which(survdat_exp_FL$GMRI_INDICATOR_3 == j),] #subset unique hauls
  N_species[j] <- length(unique(diff_hauls$Species))# count the number of unique species in each haul (denominator)
  sub_species[j] <- N_species[j]-1
  diff <- unique(as.vector(diff_hauls$Species)) # name of each unique species 
  combos <- combn(diff, 2) # create combinations of each species/haul (for weight calc)
  
  phylo <- as.matrix(subset(diff_hauls, select = c(Phylum,Class,Order,Family,Genus,Species))) # extract phylogenetic information only
  unique_phylo <- uniquecombs(phylo) # subset by unique species information
  unique_phylo <- as.data.frame(unique_phylo)
  
  total <- NULL  # reset the length for each haul because they will be different
  weight <- NULL # reset  
  
  for (i in 1:ncol(combos)) { # for each unique combination count the number of each species 
    total[i] <- mean(diff_hauls[diff_hauls[,52] == combos[1,i],34]) * mean(diff_hauls[diff_hauls[,52] == combos[2,i],34]) 
    x <- unique_phylo[unique_phylo$Species == combos[1,i],]
    y <- unique_phylo[unique_phylo$Species == combos[2,i],]
    x_y <- rbind(x,y)
    
    for (k in 1:ncol(x_y)){ # for each combination calculate the weight value 
      ident[k] <- identical(as.vector(x_y[1,k]), as.vector(x_y[2,k])) # determine how much of phylogenetic information is the same
      weight[i] <- sum(ident == "FALSE") # vector of weights
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

#pull information back together by haul 
tax_indices <- cbind(delta, delta_star, delta_plus, delta_var)
write.csv(tax_indices, "nefsc_FL_tax_indices.csv")


hauls <- unique(survdat_exp_FL$GMRI_INDICATOR_3) # haul within each unique year/season 
N_hauls <- length(hauls) # number of hauls
by_tow <- matrix(NA, nrow = N_hauls, ncol = 46)
for (i in 1:N_hauls) { #loop through each haul within year/season
  diff_hauls <- survdat_exp_FL[which(survdat_exp_FL$GMRI_INDICATOR_3 == i),] #subset unique hauls
  by_tow[i,] <- as.matrix(diff_hauls[1,1:46])
}
colnames(by_tow) <- c("ID", "STATUS_CODE", "EST_YEAR", "EST_MONTH", "EST_DAY", "EST_JULIAN_DAY", "EST_TIME", 
                      "SEASON", "SVVESSEL","STATYPE", "TOWDUR", "RPM", "DOPDISTB", "DOPDISTW", "BOTSPEED", "BOTTEMP", "SURFTEMP", 
                      "BOTSALIN", "SURFSALIN", "MINDEPTH", "MAXDEPTH", "AVGDEPTH", "SETDEPTH", "ENDDEPTH", "STRATUM", 
                      "DECDEG_BEGLAT", "DECDEG_ENDLAT", "DECDEG_BEGLON", "DECDEG_ENDLON", "SVSPPP", "CATCHSEX", "COMNAME", 
                      "BIOMASS", "ABUNDANCE", "AREA", "AIRTEMP", "CLOUD", "BAROPRESS", "WINDDIR", "WINDSP", "WAVEHGT", "CRUISE6", 
                      "STATION", "LENGTH", "NUMLEN", "GMRI_INDICATOR_2")
by_tow <- as.data.frame(by_tow)
by_tow <- by_tow[,-c(30, 31, 32, 33, 34, 44, 45)]
tax_by_tow <- cbind(by_tow, tax_indices)
save(tax_by_tow, file = "NEFSC_FL_taxinds_by_tow.RData")



## spring
cols <- c(3, 43)
survdat_exp_SP$GMRI_INDICATOR_3 <-  cumsum(!duplicated(survdat_exp_SP[cols])) 

library(mgcv)

hauls <- unique(survdat_exp_SP$GMRI_INDICATOR_3)   
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
  diff_hauls <- survdat_exp_SP[which(survdat_exp_SP$GMRI_INDICATOR_3 == j),] #subset unique hauls
  N_species[j] <- length(unique(diff_hauls$Species))# count the number of unique species in each haul (denominator)
  sub_species[j] <- N_species[j]-1
  diff <- unique(as.vector(diff_hauls$Species)) # name of each unique species 
  combos <- combn(diff, 2) # create combinations of each species/haul (for weight calc)
  
  phylo <- as.matrix(subset(diff_hauls, select = c(Phylum,Class,Order,Family,Genus,Species))) # extract phylogenetic information only
  unique_phylo <- uniquecombs(phylo) # subset by unique species information
  unique_phylo <- as.data.frame(unique_phylo)
  
  total <- NULL  # reset the length for each haul because they will be different
  weight <- NULL # reset  
  
  for (i in 1:ncol(combos)) { # for each unique combination count the number of each species 
    total[i] <- mean(diff_hauls[diff_hauls[,52] == combos[1,i],34]) * mean(diff_hauls[diff_hauls[,52] == combos[2,i],34]) 
    x <- unique_phylo[unique_phylo$Species == combos[1,i],]
    y <- unique_phylo[unique_phylo$Species == combos[2,i],]
    x_y <- rbind(x,y)
    
    for (k in 1:ncol(x_y)){ # for each combination calculate the weight value 
      ident[k] <- identical(as.vector(x_y[1,k]), as.vector(x_y[2,k])) # determine how much of phylogenetic information is the same
      weight[i] <- sum(ident == "FALSE") # vector of weights
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

#cat() #message()

#pull information back together by haul 
tax_indices <- cbind(delta, delta_star, delta_plus, delta_var)
write.csv(tax_indices, "nefsc_SP_tax_indices.csv")


hauls <- unique(survdat_exp_SP$GMRI_INDICATOR_3) # haul within each unique year/season 
N_hauls <- length(hauls) # number of hauls
by_tow <- matrix(NA, nrow = N_hauls, ncol = 46)
for (i in 1:N_hauls) { #loop through each haul within year/season
  diff_hauls <- survdat_exp_SP[which(survdat_exp_SP$GMRI_INDICATOR_3 == i),] #subset unique hauls
  by_tow[i,] <- as.matrix(diff_hauls[1,1:46])
}
colnames(by_tow) <- c("ID", "STATUS_CODE", "EST_YEAR", "EST_MONTH", "EST_DAY", "EST_JULIAN_DAY", "EST_TIME", 
                      "SEASON", "SVVESSEL","STATYPE", "TOWDUR", "RPM", "DOPDISTB", "DOPDISTW", "BOTSPEED", "BOTTEMP", "SURFTEMP", 
                      "BOTSALIN", "SURFSALIN", "MINDEPTH", "MAXDEPTH", "AVGDEPTH", "SETDEPTH", "ENDDEPTH", "STRATUM", 
                      "DECDEG_BEGLAT", "DECDEG_ENDLAT", "DECDEG_BEGLON", "DECDEG_ENDLON", "SVSPPP", "CATCHSEX", "COMNAME", 
                      "BIOMASS", "ABUNDANCE", "AREA", "AIRTEMP", "CLOUD", "BAROPRESS", "WINDDIR", "WINDSP", "WAVEHGT", "CRUISE6", 
                      "STATION", "LENGTH", "NUMLEN", "GMRI_INDICATOR_3")
by_tow <- as.data.frame(by_tow)
by_tow <- by_tow[,-c(30, 31, 32, 33, 34, 44, 45)]
tax_by_tow <- cbind(by_tow, tax_indices)
save(tax_by_tow, file = "NEFSC_SP_taxinds_by_tow.RData")






##################################visualizing ###########
###################################### compare seasonal yearly values across surveys #############
setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/NEFSC")
SP_by_tow <- read.csv("nefsc_SP_by_tow.csv", header = TRUE)
FL_by_tow <- read.csv("nefsc_FL_by_tow.csv", header = TRUE)
load('NEFSC_FL_taxinds_by_tow.RData')


#########remove strata from canadian waters and south of hatteras 
can <- c(1351, 1350, 1310, 1320, 1330, 1410,1420, 1490, 1990)
other <- unique(SP_by_tow$STRATUM[SP_by_tow$STRATUM > 3990]) #South of hatteras and scotian shelf 
remove <- c(can, other)
SP_by_tow <- SP_by_tow[!SP_by_tow$STRATUM %in% remove,]
other_2 <- unique(FL_by_tow$STRATUM[FL_by_tow$STRATUM > 3990])
remove_2 <- c(can, other_2)
FL_by_tow <- FL_by_tow[!FL_by_tow$STRATUM %in% remove_2,]




library('matrixStats')
diff_year <- matrix(NA) 
N_sample <- NULL
yr <- seq(1963, 2017, by = 1)

avg_N_spp <- NULL
avg_H_ind <- NULL
avg_D_ind <- NULL
avg_E_ind <- NULL
med_N_spp <- NULL
med_H_ind <- NULL
med_D_ind <- NULL
med_E_ind <- NULL

for (i in 1:length(yr)) { #loop through each haul within year/season 
  diff_year <- FL_by_tow[which(FL_by_tow$EST_YEAR == yr[i]),] 
  avg_N_spp[i] <- mean(diff_year$N_species, na.rm = TRUE)
  avg_H_ind[i] <- mean(diff_year$H_index, na.rm = TRUE)
  avg_D_ind[i] <- mean(diff_year$D_index, na.rm = TRUE)
  avg_E_ind[i] <- mean(diff_year$E_index, na.rm = TRUE)
  med_N_spp[i] <- median(diff_year$N_species, na.rm = TRUE)
  med_H_ind[i] <- median(diff_year$H_index, na.rm = TRUE)
  med_D_ind[i] <- median(diff_year$D_index, na.rm = TRUE)
  med_E_ind[i] <- median(diff_year$E_index, na.rm = TRUE)
}


diff_year <- matrix(NA) 
N_sample <- NULL
yr <- seq(1963, 2017, by = 1)
avg_delta <- NULL
avg_delta_plus <- NULL
avg_delta_star <- NULL
avg_delta_var <- NULL
med_delta <- NULL
med_delta_plus <- NULL
med_delta_star <- NULL
med_delta_var <- NULL
for (i in 1:length(yr)) { #loop through each haul within year/season 
  diff_year <- tax_by_tow[which(tax_by_tow$EST_YEAR == yr[i]),] 
  avg_delta[i] <- mean(diff_year$delta)
  avg_delta_plus[i] <- mean(diff_year$delta_plus)
  avg_delta_star[i] <- mean(diff_year$delta_star)
  avg_delta_var[i] <- mean(diff_year$delta_var)
  med_delta[i] <- median(diff_year$delta)
  med_delta_plus[i] <- median(diff_year$delta_plus)
  med_delta_star[i] <- median(diff_year$delta_star)
  med_delta_var[i] <- median(diff_year$delta_var)
}

# subset NEFSC to look at just GOM/GB offshore samples in fall
# stratum = 1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250, 1260, 1270, 1280, 
# 1290, 1300, 1360, 1370, 1380, 1390, 1400
GOM_fall <- FL_by_tow[FL_by_tow$STRATUM == 1130 | FL_by_tow$STRATUM == 1140 | FL_by_tow$STRATUM == 1150 | FL_by_tow$STRATUM == 1160 |
                          FL_by_tow$STRATUM == 1170 | FL_by_tow$STRATUM == 1180 | FL_by_tow$STRATUM == 1190 | FL_by_tow$STRATUM == 1200 |
                          FL_by_tow$STRATUM == 1210 | FL_by_tow$STRATUM == 1220 | FL_by_tow$STRATUM == 1230 | FL_by_tow$STRATUM == 1240 |
                          FL_by_tow$STRATUM == 1250 | FL_by_tow$STRATUM == 1260 | FL_by_tow$STRATUM == 1270 |
                          FL_by_tow$STRATUM == 1280 | FL_by_tow$STRATUM == 1290 | FL_by_tow$STRATUM == 1300 | FL_by_tow$STRATUM == 1360 |
                          FL_by_tow$STRATUM == 1370 | FL_by_tow$STRATUM == 1380 | FL_by_tow$STRATUM == 1390 | FL_by_tow$STRATUM == 1400,]

GOM_fall_tax <- tax_by_tow[tax_by_tow$STRATUM == 1130 | tax_by_tow$STRATUM == 1140 | tax_by_tow$STRATUM == 1150 | tax_by_tow$STRATUM == 1160 |
                            tax_by_tow$STRATUM == 1170 | tax_by_tow$STRATUM == 1180 | tax_by_tow$STRATUM == 1190 | tax_by_tow$STRATUM == 1200 |
                            tax_by_tow$STRATUM == 1210 | tax_by_tow$STRATUM == 1220 | tax_by_tow$STRATUM == 1230 | tax_by_tow$STRATUM == 1240 |
                            tax_by_tow$STRATUM == 1250 | tax_by_tow$STRATUM == 1260 | tax_by_tow$STRATUM == 1270 |
                            tax_by_tow$STRATUM == 1280 | tax_by_tow$STRATUM == 1290 | tax_by_tow$STRATUM == 1300 | tax_by_tow$STRATUM == 1360 |
                            tax_by_tow$STRATUM == 1370 | tax_by_tow$STRATUM == 1380 | tax_by_tow$STRATUM == 1390 | tax_by_tow$STRATUM == 1400,]

library('matrixStats')
diff_year <- matrix(NA) 
N_sample <- NULL
yr <- seq(1963, 2017, by = 1)

GOM_avg_N_spp <- NULL
GOM_avg_H_ind <- NULL
GOM_avg_D_ind <- NULL
GOM_avg_E_ind <- NULL
GOM_med_N_spp <- NULL
GOM_med_H_ind <- NULL
GOM_med_D_ind <- NULL
GOM_med_E_ind <- NULL

for (i in 1:length(yr)) { #loop through each haul within year/season 
  diff_year <- GOM_fall[which(GOM_fall$EST_YEAR == yr[i]),] 
  GOM_avg_N_spp[i] <- mean(diff_year$N_species, na.rm = TRUE)
  GOM_avg_H_ind[i] <- mean(diff_year$H_index, na.rm = TRUE)
  GOM_avg_D_ind[i] <- mean(diff_year$D_index, na.rm = TRUE)
  GOM_avg_E_ind[i] <- mean(diff_year$E_index, na.rm = TRUE)
  GOM_med_N_spp[i] <- median(diff_year$N_species, na.rm = TRUE)
  GOM_med_H_ind[i] <- median(diff_year$H_index, na.rm = TRUE)
  GOM_med_D_ind[i] <- median(diff_year$D_index, na.rm = TRUE)
  GOM_med_E_ind[i] <- median(diff_year$E_index, na.rm = TRUE)
}


diff_year <- matrix(NA) 
N_sample <- NULL
yr <- seq(1963, 2017, by = 1)
GOM_avg_delta <- NULL
GOM_avg_delta_plus <- NULL
GOM_avg_delta_star <- NULL
GOM_avg_delta_var <- NULL
GOM_med_delta <- NULL
GOM_med_delta_plus <- NULL
GOM_med_delta_star <- NULL
GOM_med_delta_var <- NULL
for (i in 1:length(yr)) { #loop through each haul within year/season 
  diff_year <- GOM_fall_tax[which(GOM_fall_tax$EST_YEAR == yr[i]),] 
  GOM_avg_delta[i] <- mean(diff_year$delta)
  GOM_avg_delta_plus[i] <- mean(diff_year$delta_plus)
  GOM_avg_delta_star[i] <- mean(diff_year$delta_star)
  GOM_avg_delta_var[i] <- mean(diff_year$delta_var)
  GOM_med_delta[i] <- median(diff_year$delta)
  GOM_med_delta_plus[i] <- median(diff_year$delta_plus)
  GOM_med_delta_star[i] <- median(diff_year$delta_star)
  GOM_med_delta_var[i] <- median(diff_year$delta_var)
}

setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/MDMF")
ma_fall_inds <- read.csv("MDMF_fall_div_ind_by_tow.csv")
diff_year_ma <- matrix(NA) 
yr <- seq(1978, 2017, by = 1)

avg_N_spp_ma <- NULL
avg_H_ind_ma <- NULL
avg_D_ind_ma <- NULL
avg_E_ind_ma <- NULL
avg_delta_ma <- NULL
avg_delta_plus_ma <- NULL
avg_delta_star_ma <- NULL
avg_delta_var_ma <- NULL
med_N_spp_ma <- NULL
med_H_ind_ma <- NULL
med_D_ind_ma <- NULL
med_E_ind_ma <- NULL
med_delta_ma <- NULL
med_delta_plus_ma <- NULL
med_delta_star_ma <- NULL
med_delta_var_ma <- NULL
for (i in 1:length(yr)) { #loop through each haul within year/season 
  diff_year_ma <- ma_fall_inds[which(ma_fall_inds$YEAR == yr[i]),] 
  avg_N_spp_ma[i] <- mean(diff_year_ma$N_species, na.rm = TRUE)
  avg_H_ind_ma[i] <- mean(diff_year_ma$H_index, na.rm = TRUE)
  avg_D_ind_ma[i] <- mean(diff_year_ma$D_index, na.rm = TRUE)
  avg_E_ind_ma[i] <- mean(diff_year_ma$E_index, na.rm = TRUE)
  avg_delta_ma[i] <- mean(diff_year_ma$delta, na.rm = TRUE)
  avg_delta_plus_ma[i] <- mean(diff_year_ma$delta_plus, na.rm = TRUE)
  avg_delta_star_ma[i] <- mean(diff_year_ma$delta_star, na.rm = TRUE)
  avg_delta_var_ma[i] <- mean(diff_year_ma$delta_var, na.rm = TRUE)
  med_N_spp_ma[i] <- median(diff_year_ma$N_species, na.rm = TRUE)
  med_H_ind_ma[i] <- median(diff_year_ma$H_index, na.rm = TRUE)
  med_D_ind_ma[i] <- median(diff_year_ma$D_index, na.rm = TRUE)
  med_E_ind_ma[i] <- median(diff_year_ma$E_index, na.rm = TRUE)
  med_delta_ma[i] <- median(diff_year_ma$delta, na.rm = TRUE)
  med_delta_plus_ma[i] <- median(diff_year_ma$delta_plus, na.rm = TRUE)
  med_delta_star_ma[i] <- median(diff_year_ma$delta_star, na.rm = TRUE)
  med_delta_var_ma[i] <- median(diff_year_ma$delta_var, na.rm = TRUE)
}


setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/Results")
me_inds <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
me_fall_inds <- subset(me_inds, SEASON == "FL")
diff_year_me <- matrix(NA) 
yr <- seq(0, 17, by = 1)
avg_N_spp_me <- NULL
avg_H_ind_me <- NULL
avg_D_ind_me <- NULL
avg_E_ind_me <- NULL
avg_delta_me <- NULL
avg_delta_plus_me <- NULL
avg_delta_star_me <- NULL
avg_delta_var_me <- NULL
med_N_spp_me <- NULL
med_H_ind_me <- NULL
med_D_ind_me <- NULL
med_E_ind_me <- NULL
med_delta_me <- NULL
med_delta_plus_me <- NULL
med_delta_star_me <- NULL
med_delta_var_me <- NULL


for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year_me <- me_fall_inds[which(me_fall_inds$YEAR == yr[i]),] #subset unique hauls
  avg_N_spp_me[i] <- mean(diff_year_me$N_species)
  avg_H_ind_me[i] <- mean(diff_year_me$H_index)
  avg_D_ind_me[i] <- mean(diff_year_me$D_index)
  avg_E_ind_me[i] <- mean(diff_year_me$E_index)
  avg_delta_me[i] <- mean(diff_year_me$delta)
  avg_delta_plus_me[i] <- mean(diff_year_me$delta_plus)
  avg_delta_star_me[i] <- mean(diff_year_me$delta_star)
  avg_delta_var_me[i] <- mean(diff_year_me$delta_var)
  med_N_spp_me[i] <- median(diff_year_me$N_species)
  med_H_ind_me[i] <- median(diff_year_me$H_index)
  med_D_ind_me[i] <- median(diff_year_me$D_index)
  med_E_ind_me[i] <- median(diff_year_me$E_index)
  med_delta_me[i] <- median(diff_year_me$delta)
  med_delta_plus_me[i] <- median(diff_year_me$delta_plus)
  med_delta_star_me[i] <- median(diff_year_me$delta_star)
  med_delta_var_me[i] <- median(diff_year_me$delta_var)
}

ma_yr <- seq(16, 55, by = 1)
ma_N <- cbind(ma_yr, avg_N_spp_ma, med_N_spp_ma)
ma_H <- cbind(ma_yr, avg_H_ind_ma, med_H_ind_ma)
ma_D <- cbind(ma_yr, avg_D_ind_ma, med_D_ind_ma)
ma_E <- cbind(ma_yr, avg_E_ind_ma, med_E_ind_ma)
me_yr <- seq(38, 55, by = 1)
me_N <- cbind(me_yr, avg_N_spp_me, med_N_spp_me)
me_H <- cbind(me_yr, avg_H_ind_me, med_H_ind_me)
me_D <- cbind(me_yr, avg_D_ind_me, med_D_ind_me)
me_E <- cbind(me_yr, avg_E_ind_me, med_E_ind_me)



######spring
setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/NEFSC")
load('NEFSC_SP_taxinds_by_tow.RData')


#tax_by_tow <- tax_by_tow[!tax_by_tow$STRATUM %in% remove,]
#spring_inds <- subset(tax_by_tow, SEASON == "SPRING")

library('matrixStats')
diff_year_2 <- matrix(NA) 
yr <- seq(1968, 2018, by = 1)

avg_N_spp_2 <- NULL
avg_H_ind_2 <- NULL
avg_D_ind_2 <- NULL
avg_E_ind_2 <- NULL
med_N_spp_2 <- NULL
med_H_ind_2 <- NULL
med_D_ind_2 <- NULL
med_E_ind_2 <- NULL
for (i in 1:length(yr)) { #loop through each haul within year/season 
  diff_year_2 <- SP_by_tow[which(SP_by_tow$EST_YEAR == yr[i]),] 
  avg_N_spp_2[i] <- mean(diff_year_2$N_species, na.rm = TRUE)
  avg_H_ind_2[i] <- mean(diff_year_2$H_index, na.rm = TRUE)
  avg_D_ind_2[i] <- mean(diff_year_2$D_index, na.rm = TRUE)
  avg_E_ind_2[i] <- mean(diff_year_2$E_index, na.rm = TRUE)
  med_N_spp_2[i] <- median(diff_year_2$N_species, na.rm = TRUE)
  med_H_ind_2[i] <- median(diff_year_2$H_index, na.rm = TRUE)
  med_D_ind_2[i] <- median(diff_year_2$D_index, na.rm = TRUE)
  med_E_ind_2[i] <- median(diff_year_2$E_index, na.rm = TRUE)
}

load('NEFSC_SP_taxinds_by_tow.RData')
diff_year_2 <- matrix(NA) 
yr <- seq(1968, 2017, by = 1)
avg_delta_2 <- NULL
avg_delta_plus_2 <- NULL
avg_delta_star_2 <- NULL
avg_delta_var_2 <- NULL
med_delta_2 <- NULL
med_delta_plus_2 <- NULL
med_delta_star_2 <- NULL
med_delta_var_2 <- NULL
for (i in 1:length(yr)) { #loop through each haul within year/season 
  diff_year_2 <- tax_by_tow[which(tax_by_tow$EST_YEAR == yr[i]),] 
  avg_delta_2[i] <- mean(diff_year_2$delta)
  avg_delta_plus_2[i] <- mean(diff_year_2$delta_plus)
  avg_delta_star_2[i] <- mean(diff_year_2$delta_star)
  avg_delta_var_2[i] <- mean(diff_year_2$delta_var)
  med_delta_2[i] <- median(diff_year_2$delta)
  med_delta_plus_2[i] <- median(diff_year_2$delta_plus)
  med_delta_star_2[i] <- median(diff_year_2$delta_star)
  med_delta_var_2[i] <- median(diff_year_2$delta_var)
}

# subset NEFSC to look at just GOM/GB offshore samples in fall
# stratum = 1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250, 1260, 1270, 1280, 
# 1290, 1300, 1360, 1370, 1380, 1390, 1400
GOM_spring <- SP_by_tow[SP_by_tow$STRATUM == 1130 | SP_by_tow$STRATUM == 1140 | SP_by_tow$STRATUM == 1150 | SP_by_tow$STRATUM == 1160 |
                            SP_by_tow$STRATUM == 1170 | SP_by_tow$STRATUM == 1180 | SP_by_tow$STRATUM == 1190 | SP_by_tow$STRATUM == 1200 |
                            SP_by_tow$STRATUM == 1210 | SP_by_tow$STRATUM == 1220 | SP_by_tow$STRATUM == 1230 | SP_by_tow$STRATUM == 1240 |
                            SP_by_tow$STRATUM == 1250 | SP_by_tow$STRATUM == 1260 | SP_by_tow$STRATUM == 1270 |
                            SP_by_tow$STRATUM == 1280 | SP_by_tow$STRATUM == 1290 | SP_by_tow$STRATUM == 1300 | SP_by_tow$STRATUM == 1360 |
                            SP_by_tow$STRATUM == 1370 | SP_by_tow$STRATUM == 1380 | SP_by_tow$STRATUM == 1390 | SP_by_tow$STRATUM == 1400,]

GOM_spring_tax <- tax_by_tow[tax_by_tow$STRATUM == 1130 | tax_by_tow$STRATUM == 1140 | tax_by_tow$STRATUM == 1150 | tax_by_tow$STRATUM == 1160 |
                              tax_by_tow$STRATUM == 1170 | tax_by_tow$STRATUM == 1180 | tax_by_tow$STRATUM == 1190 | tax_by_tow$STRATUM == 1200 |
                              tax_by_tow$STRATUM == 1210 | tax_by_tow$STRATUM == 1220 | tax_by_tow$STRATUM == 1230 | tax_by_tow$STRATUM == 1240 |
                              tax_by_tow$STRATUM == 1250 | tax_by_tow$STRATUM == 1260 | tax_by_tow$STRATUM == 1270 |
                              tax_by_tow$STRATUM == 1280 | tax_by_tow$STRATUM == 1290 | tax_by_tow$STRATUM == 1300 | tax_by_tow$STRATUM == 1360 |
                              tax_by_tow$STRATUM == 1370 | tax_by_tow$STRATUM == 1380 | tax_by_tow$STRATUM == 1390 | tax_by_tow$STRATUM == 1400,]

library('matrixStats')
diff_year_2 <- matrix(NA) 
yr <- seq(1968, 2018, by = 1)

GOM_avg_N_spp_2 <- NULL
GOM_avg_H_ind_2 <- NULL
GOM_avg_D_ind_2 <- NULL
GOM_avg_E_ind_2 <- NULL
GOM_med_N_spp_2 <- NULL
GOM_med_H_ind_2 <- NULL
GOM_med_D_ind_2 <- NULL
GOM_med_E_ind_2 <- NULL

for (i in 1:length(yr)) { #loop through each haul within year/season 
  diff_year_2 <- GOM_spring[which(GOM_spring$EST_YEAR == yr[i]),] 
  GOM_avg_N_spp_2[i] <- mean(diff_year_2$N_species, na.rm = TRUE)
  GOM_avg_H_ind_2[i] <- mean(diff_year_2$H_index, na.rm = TRUE)
  GOM_avg_D_ind_2[i] <- mean(diff_year_2$D_index, na.rm = TRUE)
  GOM_avg_E_ind_2[i] <- mean(diff_year_2$E_index, na.rm = TRUE)
  GOM_med_N_spp_2[i] <- median(diff_year_2$N_species, na.rm = TRUE)
  GOM_med_H_ind_2[i] <- median(diff_year_2$H_index, na.rm = TRUE)
  GOM_med_D_ind_2[i] <- median(diff_year_2$D_index, na.rm = TRUE)
  GOM_med_E_ind_2[i] <- median(diff_year_2$E_index, na.rm = TRUE)
}


diff_year_2 <- matrix(NA) 
yr <- seq(1968, 2018, by = 1)
GOM_avg_delta_2 <- NULL
GOM_avg_delta_plus_2 <- NULL
GOM_avg_delta_star_2 <- NULL
GOM_avg_delta_var_2 <- NULL
GOM_med_delta_2 <- NULL
GOM_med_delta_plus_2 <- NULL
GOM_med_delta_star_2 <- NULL
GOM_med_delta_var_2 <- NULL
for (i in 1:length(yr)) { #loop through each haul within year/season 
  diff_year_2 <- GOM_spring_tax[which(GOM_spring_tax$EST_YEAR == yr[i]),] 
  GOM_avg_delta_2[i] <- mean(diff_year_2$delta)
  GOM_avg_delta_plus_2[i] <- mean(diff_year_2$delta_plus)
  GOM_avg_delta_star_2[i] <- mean(diff_year_2$delta_star)
  GOM_avg_delta_var_2[i] <- mean(diff_year_2$delta_var)
  GOM_med_delta_2[i] <- median(diff_year_2$delta)
  GOM_med_delta_plus_2[i] <- median(diff_year_2$delta_plus)
  GOM_med_delta_star_2[i] <- median(diff_year_2$delta_star)
  GOM_med_delta_var_2[i] <- median(diff_year_2$delta_var)
}


setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/MDMF")
ma_spring_inds <- read.csv("MDMF_spring_div_ind_by_tow.csv")
diff_year_ma <- matrix(NA) 
yr <- seq(1978, 2017, by = 1)

avg_N_spp_ma_2 <- NULL
avg_H_ind_ma_2 <- NULL
avg_D_ind_ma_2 <- NULL
avg_E_ind_ma_2 <- NULL
avg_delta_ma_2 <- NULL
avg_delta_plus_ma_2 <- NULL
avg_delta_star_ma_2 <- NULL
avg_delta_var_ma_2 <- NULL
med_N_spp_ma_2 <- NULL
med_H_ind_ma_2 <- NULL
med_D_ind_ma_2 <- NULL
med_E_ind_ma_2 <- NULL
med_delta_ma_2 <- NULL
med_delta_plus_ma_2 <- NULL
med_delta_star_ma_2 <- NULL
med_delta_var_ma_2 <- NULL
for (i in 1:length(yr)) { #loop through each haul within year/season 
  diff_year_ma_2 <- ma_spring_inds[which(ma_spring_inds$YEAR == yr[i]),] 
  avg_N_spp_ma_2[i] <- mean(diff_year_ma_2$N_species, na.rm = TRUE)
  avg_H_ind_ma_2[i] <- mean(diff_year_ma_2$H_index, na.rm = TRUE)
  avg_D_ind_ma_2[i] <- mean(diff_year_ma_2$D_index, na.rm = TRUE)
  avg_E_ind_ma_2[i] <- mean(diff_year_ma_2$E_index, na.rm = TRUE)
  avg_delta_ma_2[i] <- mean(diff_year_ma_2$delta)
  avg_delta_plus_ma_2[i] <- mean(diff_year_ma_2$delta_plus)
  avg_delta_star_ma_2[i] <- mean(diff_year_ma_2$delta_star)
  avg_delta_var_ma_2[i] <- mean(diff_year_ma_2$delta_var)
  med_N_spp_ma_2[i] <- median(diff_year_ma_2$N_species, na.rm = TRUE)
  med_H_ind_ma_2[i] <- median(diff_year_ma_2$H_index, na.rm = TRUE)
  med_D_ind_ma_2[i] <- median(diff_year_ma_2$D_index, na.rm = TRUE)
  med_E_ind_ma_2[i] <- median(diff_year_ma_2$E_index, na.rm = TRUE)
  med_delta_ma_2[i] <- median(diff_year_ma_2$delta)
  med_delta_plus_ma_2[i] <- median(diff_year_ma_2$delta_plus)
  med_delta_star_ma_2[i] <- median(diff_year_ma_2$delta_star)
  med_delta_var_ma_2[i] <- median(diff_year_ma_2$delta_var)
}


setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/Results")
me_inds <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
me_spring_inds <- subset(me_inds, SEASON == "SP")
diff_year_me_2 <- matrix(NA) 
yr <- seq(1, 17, by = 1)
avg_N_spp_me_2 <- NULL
avg_H_ind_me_2 <- NULL
avg_D_ind_me_2 <- NULL
avg_E_ind_me_2 <- NULL
avg_delta_me_2 <- NULL
avg_delta_plus_me_2 <- NULL
avg_delta_star_me_2 <- NULL
avg_delta_var_me_2 <- NULL
med_N_spp_me_2 <- NULL
med_H_ind_me_2 <- NULL
med_D_ind_me_2 <- NULL
med_E_ind_me_2 <- NULL
med_delta_me_2 <- NULL
med_delta_plus_me_2 <- NULL
med_delta_star_me_2 <- NULL
med_delta_var_me_2 <- NULL


for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year_me_2 <- me_spring_inds[which(me_spring_inds$YEAR == yr[i]),] 
  avg_N_spp_me_2[i] <- mean(diff_year_me_2$N_species)
  avg_H_ind_me_2[i] <- mean(diff_year_me_2$H_index)
  avg_D_ind_me_2[i] <- mean(diff_year_me_2$D_index)
  avg_E_ind_me_2[i] <- mean(diff_year_me_2$E_index)
  avg_delta_me_2[i] <- mean(diff_year_me_2$delta)
  avg_delta_plus_me_2[i] <- mean(diff_year_me_2$delta_plus)
  avg_delta_star_me_2[i] <- mean(diff_year_me_2$delta_star)
  avg_delta_var_me_2[i] <- mean(diff_year_me_2$delta_var)
  med_N_spp_me_2[i] <- median(diff_year_me_2$N_species)
  med_H_ind_me_2[i] <- median(diff_year_me_2$H_index)
  med_D_ind_me_2[i] <- median(diff_year_me_2$D_index)
  med_E_ind_me_2[i] <- median(diff_year_me_2$E_index)
  med_delta_me_2[i] <- median(diff_year_me_2$delta)
  med_delta_plus_me_2[i] <- median(diff_year_me_2$delta_plus)
  med_delta_star_me_2[i] <- median(diff_year_me_2$delta_star)
  med_delta_var_me_2[i] <- median(diff_year_me_2$delta_var)
}


ma_yr_2 <- seq(11, 50, by = 1)
ma_N_2 <- cbind(ma_yr_2, avg_N_spp_ma_2, med_N_spp_ma_2)
ma_H_2 <- cbind(ma_yr_2, avg_H_ind_ma_2, med_H_ind_ma_2)
ma_D_2 <- cbind(ma_yr_2, avg_D_ind_ma_2, med_D_ind_ma_2)
ma_E_2 <- cbind(ma_yr_2, avg_E_ind_ma_2, med_E_ind_ma_2)
me_yr_2 <- seq(34, 50, by = 1)
me_N_2 <- cbind(me_yr_2, avg_N_spp_me_2, med_N_spp_me_2)
me_H_2 <- cbind(me_yr_2, avg_H_ind_me_2, med_H_ind_me_2)
me_D_2 <- cbind(me_yr_2, avg_D_ind_me_2, med_D_ind_me_2)
me_E_2 <- cbind(me_yr_2, avg_E_ind_me_2, med_E_ind_me_2)


jpeg("presentation_inds_01.jpg")


layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
layout.show(8)
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
label <- seq(1963, 2017)
label <- as.character(label)
label_2 <- seq(1968, 2017)
label_2 <- as.character(label_2)


plot(avg_N_spp, type = 'l', xlab = '', ylab = "Number of Species", main = "Average Fall Indices"
     , ylim = c(8, 40), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray', cex.main = 2)
axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
lines(GOM_avg_N_spp, type = 'l', col = 'slategray2', lwd = 2)
lines(ma_N[,1], ma_N[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_N[,1], me_N[,2], type = 'l', col = 'slateblue', lwd = 2)

plot(avg_H_ind, type = 'l', xlab = '', ylab = "Shannon-Weiner Index", ylim = c(0.75,2), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray')
lines(GOM_avg_H_ind, type = 'l', lwd = 2, cex.axis = 1.5, col = 'slategray2')
axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
lines(ma_H[,1], ma_H[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_H[,1], me_H[,2], type = 'l', col = 'slateblue', lwd = 2)

plot(avg_D_ind, type = 'l', xlab = '', ylab = "Simpson Diversity Index", ylim = c(2,5), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray')
axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
lines(GOM_avg_D_ind, type = 'l', col = 'slategray2', lwd = 2)
lines(ma_D[,1], ma_D[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_D[,1], me_D[,2], type = 'l', col = 'slateblue', lwd = 2)

plot(avg_E_ind, type = 'l', xlab = "Survey Year", ylab = "Simpson Evenness Index", ylim = c(0.1, 0.5), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray')
axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
lines(GOM_avg_E_ind, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_E[,1], ma_E[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_E[,1], me_E[,2], type = 'l', col = 'slateblue', lwd = 2)


plot(avg_N_spp_2, type = 'l', xlab = '', ylab = '', main = "Average Spring Indices", ylim = c(8, 40), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray', cex.main = 2)
axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)
lines(GOM_avg_N_spp_2, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_N_2[,1], ma_N_2[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_N_2[,1], me_N_2[,2], type = 'l', col = 'slateblue', lwd = 2)
legend('topleft', legend = c("NEFSC", "MA", "ME", "Offshore GB/GOM"), col = c('slategray', 'skyblue3', 'slateblue', 'slategray3'), lty = 1, lwd = 2, bty = 'n', cex = 1.2)

plot(avg_H_ind_2, type = 'l', xlab = '', ylab = '', ylim = c(0.75,2), xaxt = 'n', lwd= 2, cex.axis = 1.5, col = 'slategray')
axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)
lines(GOM_avg_H_ind_2, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_H_2[,1], ma_H_2[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_H_2[,1], me_H_2[,2], type = 'l', col = 'slateblue', lwd = 2)


plot(avg_D_ind_2, type = 'l', xlab = '', ylab = '', ylim = c(2,5),  xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray')
axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)
lines(GOM_avg_D_ind_2, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_D_2[,1], ma_D_2[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_D_2[,1], me_D_2[,2], type = 'l', col = 'slateblue', lwd = 2)


plot(avg_E_ind_2, type = 'l', xlab = "Survey Year", ylab = '', ylim = c(0.1,0.5), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray')
axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)
lines(GOM_avg_E_ind_2, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_E_2[,1], ma_E_2[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_E_2[,1], me_E_2[,2], type = 'l', col = 'slateblue', lwd = 2)

dev.off()

#inds_by_area_01.pdf

##### taxonomic diversity inds 
ma_yr <- seq(16, 55, by = 1)
ma_delta <- cbind(ma_yr, avg_delta_ma, med_delta_ma) 
ma_deltap <- cbind(ma_yr, avg_delta_plus_ma, med_delta_plus_ma)
ma_deltas <- cbind(ma_yr, avg_delta_star_ma, med_delta_star_ma)
ma_deltav <- cbind(ma_yr, avg_delta_var_ma, med_delta_var_ma)
me_yr <- seq(38, 55, by = 1)
me_delta <- cbind(me_yr, avg_delta_me, med_delta_me)
me_deltap <- cbind(me_yr, avg_delta_plus_me, med_delta_plus_me)
me_deltas <- cbind(me_yr, avg_delta_star_me, med_delta_star_me)
me_deltav <- cbind(me_yr, avg_delta_var_me, med_delta_var_me)

ma_yr_2 <- seq(11, 50, by = 1)
ma_delta_2 <- cbind(ma_yr_2, avg_delta_ma_2, med_delta_ma_2)
ma_deltap_2 <- cbind(ma_yr_2, avg_delta_plus_ma_2, med_delta_plus_ma_2)
ma_deltas_2 <- cbind(ma_yr_2, avg_delta_star_ma_2, med_delta_star_ma_2)
ma_deltav_2 <- cbind(ma_yr_2, avg_delta_var_ma_2, med_delta_var_ma_2)
me_yr_2 <- seq(34, 50, by = 1)
me_delta_2 <- cbind(me_yr_2, avg_delta_me_2, med_delta_me_2)
me_deltap_2 <- cbind(me_yr_2, avg_delta_plus_me_2, med_delta_plus_me_2)
me_delta_s_2 <- cbind(me_yr_2, avg_delta_star_me_2, med_delta_star_me_2)
me_deltav_2 <- cbind(me_yr_2, avg_delta_var_me_2, med_delta_var_me_2)

layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
options(scipen = 999)

plot(avg_delta, type = 'l', xlab = '', ylab = "Taxonomic Diversity", main = "Average Fall Indices" 
     ,  ylim = c(0, 800000), xaxt = 'n', lwd = 2, col = 'slategray', cex.axis = 1.5, cex.main = 2)
axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
lines(GOM_avg_delta, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_delta[,1], ma_delta[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_delta[,1], me_delta[,2], type = 'l', col = 'slateblue', lwd = 2)


plot(avg_delta_star, type = 'l', xlab = '', ylab = "Taxonomic Distinctness", ylim = c(3.75,5.5), xaxt = 'n', lwd = 2, col = 'slategray', cex.axis = 1.5)
axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
lines(GOM_avg_delta_star, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_deltas[,1], ma_deltas[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_deltas[,1], me_deltas[,2], type = 'l', col = 'slateblue', lwd = 2)


plot(avg_delta_plus, type = 'l', xlab = '', ylab = "Avg. Tax. Distinctness",  ylim = c(4,5), xaxt = 'n', lwd = 2, col = 'slategray', cex.axis = 1.5) 
axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
lines(GOM_avg_delta_plus, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_deltap[,1], ma_deltap[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_deltap[,1], me_deltap[,2], type = 'l', col = 'slateblue', lwd = 2)

plot(avg_delta_var, type = 'l', xlab = "Survey Year", ylab = "Var. in Tax. Distinctness", ylim = c(0.8, 1.8),  xaxt = 'n', lwd = 2, col = 'slategray', cex.axis = 1.5)
axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
lines(GOM_avg_delta_var, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_deltav[,1], ma_deltav[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_deltav[,1], me_deltav[,2], type = 'l', col = 'slateblue', lwd = 2)

plot(avg_delta_2, type = 'l', xlab = '', ylab = '', main = "Average Spring Indices" 
     ,  ylim = c(0, 800000), xaxt = 'n', lwd = 2, col = 'slategray', cex.axis = 1.5, cex.main = 2)
axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)#, las = 2)
lines(GOM_avg_delta_2, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_delta_2[,1], ma_delta_2[,2], type = 'l', col = 'slateblue3', lwd = 2)
lines(me_delta_2[,1], me_delta_2[,2], type = 'l', col = 'slateblue', lwd = 2)
legend('topleft', legend = c("NEFSC", "MA", "ME", "Offshore GOM/GB"), col = c('slategray', 'skyblue3', 'slateblue', 'slategray3'), lty = 1, lwd = 2, bty = 'n', cex = 1.2)


plot(avg_delta_star_2, type = 'l', xlab = '', ylab = '', ylim = c(3.75,5.5), xaxt = 'n', lwd = 2, col = 'slategray', cex.axis = 1.5)
axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)#, las = 2)
lines(GOM_avg_delta_star_2, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_deltas_2[,1], ma_deltas_2[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_delta_s_2[,1], me_delta_s_2[,2], type = 'l', col = 'slateblue', lwd = 2)


plot(avg_delta_plus_2, type = 'l', xlab = '', ylab = '',  ylim = c(4,5), xaxt = 'n', lwd = 2, col = 'slategray', cex.axis = 1.5) 
axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)#, las = 2)
lines(GOM_avg_delta_plus_2, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_deltap_2[,1], ma_deltap_2[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_deltap_2[,1], me_deltap_2[,2], type = 'l', col = 'slateblue', lwd = 2)

plot(avg_delta_var_2, type = 'l', xlab = "Survey Year", ylab = '', ylim = c(0.8, 1.8),  xaxt = 'n', lwd = 2, col = 'slategray', cex.axis = 1.5)
axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)#, las = 2)
lines(GOM_avg_delta_var_2, type = 'l', col = 'slategray3', lwd = 2)
lines(ma_deltav_2[,1], ma_deltav_2[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_deltav_2[,1], me_deltav_2[,2], type = 'l', col = 'slateblue', lwd = 2)

#inds_by_area_02.pdf





###### figures for SNEC ####
pdf("SNEC_ind_trends.pdf")

layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
#layout.show(8)
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
label <- seq(1963, 2017)
label <- as.character(label)
label_2 <- seq(1968, 2017)
label_2 <- as.character(label_2)


plot(avg_N_spp, type = 'l', xlab = '', ylab = "Number of Species", main = "Average Fall Indices"
     , ylim = c(8, 28), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray', cex.main = 2)
axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
#lines(GOM_avg_N_spp, type = 'l', col = 'slategray2', lwd = 2)
#lines(ma_N[,1], ma_N[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_N[,1], me_N[,2], type = 'l', col = 'slateblue', lwd = 2)

plot(avg_H_ind, type = 'l', xlab = '', ylab = "Shannon-Weiner Index", ylim = c(1,1.8), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray')
#lines(GOM_avg_H_ind, type = 'l', lwd = 2, cex.axis = 1.5, col = 'slategray2')
axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
#lines(ma_H[,1], ma_H[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_H[,1], me_H[,2], type = 'l', col = 'slateblue', lwd = 2)

#plot(avg_D_ind, type = 'l', xlab = '', ylab = "Simpson Diversity Index", ylim = c(2,5), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray')
#axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
#lines(GOM_avg_D_ind, type = 'l', col = 'slategray2', lwd = 2)
#lines(ma_D[,1], ma_D[,2], type = 'l', col = 'skyblue3', lwd = 2)
#lines(me_D[,1], me_D[,2], type = 'l', col = 'slateblue', lwd = 2)

plot(avg_E_ind, type = 'l', xlab = "Survey Year", ylab = "Simpson Evenness Index", ylim = c(0.1, 0.4), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray')
axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
#lines(GOM_avg_E_ind, type = 'l', col = 'slategray3', lwd = 2)
#lines(ma_E[,1], ma_E[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_E[,1], me_E[,2], type = 'l', col = 'slateblue', lwd = 2)


plot(avg_delta_plus, type = 'l', xlab = '', ylab = "Avg. Tax. Distinctness",  ylim = c(4.1,5), xaxt = 'n', lwd = 2, col = 'slategray', cex.axis = 1.5) 
axis(1, at = c(1:55),labels = label, cex.axis = 1.5)#, las = 2)
#lines(GOM_avg_delta_plus, type = 'l', col = 'slategray3', lwd = 2)
#lines(ma_deltap[,1], ma_deltap[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_deltap[,1], me_deltap[,2], type = 'l', col = 'slateblue', lwd = 2)


plot(avg_N_spp_2, type = 'l', xlab = '', ylab = '', main = "Average Spring Indices", ylim = c(8, 28), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray', cex.main = 2)
axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)
#lines(GOM_avg_N_spp_2, type = 'l', col = 'slategray3', lwd = 2)
#lines(ma_N_2[,1], ma_N_2[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_N_2[,1], me_N_2[,2], type = 'l', col = 'slateblue', lwd = 2)
legend('topleft', legend = c("NEFSC", "ME-NH"), col = c('slategray', 'slateblue'), lty = 1, lwd = 2, bty = 'n', cex = 1.2)

plot(avg_H_ind_2, type = 'l', xlab = '', ylab = '', ylim = c(1,1.8), xaxt = 'n', lwd= 2, cex.axis = 1.5, col = 'slategray')
axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)
#lines(GOM_avg_H_ind_2, type = 'l', col = 'slategray3', lwd = 2)
#lines(ma_H_2[,1], ma_H_2[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_H_2[,1], me_H_2[,2], type = 'l', col = 'slateblue', lwd = 2)


#plot(avg_D_ind_2, type = 'l', xlab = '', ylab = '', ylim = c(2,5),  xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray')
#axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)
#lines(GOM_avg_D_ind_2, type = 'l', col = 'slategray3', lwd = 2)
#lines(ma_D_2[,1], ma_D_2[,2], type = 'l', col = 'skyblue3', lwd = 2)
#lines(me_D_2[,1], me_D_2[,2], type = 'l', col = 'slateblue', lwd = 2)


plot(avg_E_ind_2, type = 'l', xlab = "Survey Year", ylab = '', ylim = c(0.1,0.4), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slategray')
axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)
#lines(GOM_avg_E_ind_2, type = 'l', col = 'slategray3', lwd = 2)
#lines(ma_E_2[,1], ma_E_2[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_E_2[,1], me_E_2[,2], type = 'l', col = 'slateblue', lwd = 2)

plot(avg_delta_plus_2, type = 'l', xlab = '', ylab = '',  ylim = c(4.1,5), xaxt = 'n', lwd = 2, col = 'slategray', cex.axis = 1.5) 
axis(1, at = c(1:50),labels = label_2, cex.axis = 1.5)#, las = 2)
#lines(GOM_avg_delta_plus_2, type = 'l', col = 'slategray3', lwd = 2)
#lines(ma_deltap_2[,1], ma_deltap_2[,2], type = 'l', col = 'skyblue3', lwd = 2)
lines(me_deltap_2[,1], me_deltap_2[,2], type = 'l', col = 'slateblue', lwd = 2)

dev.off()


#### just fall Maine-NH for SNEC ##
pdf("SNEC_MENH_trends.pdf")

layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
label <- seq(2000, 2017)
label <- as.character(label)

plot(me_N[,2], type = 'l', xlab = '', ylab = "Number of Species", main = "Average Fall Indices"
     , ylim = c(17, 25), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slateblue', cex.main = 2)
axis(1, at = c(1:18),labels = label, cex.axis = 1.5)#, las = 2)

plot(me_H[,2], type = 'l', xlab = '', ylab = "Shannon-Weiner Index", ylim = c(1.2,1.7), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slateblue')
axis(1, at = c(1:18),labels = label, cex.axis = 1.5)#, las = 2)


plot(me_E[,2], type = 'l', xlab = "Survey Year", ylab = "Simpson Evenness Index", ylim = c(0.12, 0.19), xaxt = 'n', lwd = 2, cex.axis = 1.5, col = 'slateblue')
axis(1, at = c(1:18),labels = label, cex.axis = 1.5)#, las = 2)


plot(me_deltap[,2], type = 'l', xlab = '', ylab = "Avg. Tax. Distinctness",  ylim = c(4.6,5), xaxt = 'n', lwd = 2, col = 'slateblue', cex.axis = 1.5) 
axis(1, at = c(1:18),labels = label, cex.axis = 1.5)#, las = 2)

dev.off()



####### MA DMF for Kathryn Ford #####
setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/Results/Figures")
pdf("MDMF_ind_trends.pdf", width = 8, height = 7)

layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
#layout.show(8)
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
label <- seq(1978, 2017)
label <- as.character(label)
label_2 <- seq(1978, 2018)
label_2 <- as.character(label_2)


plot(ma_N[,2], type = 'l', xlab = '', ylab = "Number of Species", main = "Average Fall Indices"
     , ylim = c(10, 18), xaxt = 'n', lwd = 2, cex.axis = 1.5, cex.main = 2, cex.lab = 1.25)
axis(1, at = c(1:40),labels = label, cex.axis = 1.5)#, las = 2)

plot(ma_H[,2], type = 'l', xlab = '', ylab = "Shannon-Weiner Index", ylim = c(1,1.8), xaxt = 'n', lwd = 2, cex.axis = 1.5, cex.lab = 1.25)
axis(1, at = c(1:40),labels = label, cex.axis = 1.5)#, las = 2)

plot(ma_E[,2], type = 'l', xlab = "Survey Year", ylab = "Simpson Evenness Index", ylim = c(0.15, 0.35), xaxt = 'n', lwd = 2, cex.axis = 1.5, cex.lab = 1.25)
axis(1, at = c(1:40),labels = label, cex.axis = 1.5)#, las = 2)

plot(ma_deltap[,2], type = 'l', xlab = '', ylab = "Avg. Tax. Distinctness",  ylim = c(4.5,5), xaxt = 'n', lwd = 2, cex.axis = 1.5, cex.lab = 1.25) 
axis(1, at = c(1:40),labels = label, cex.axis = 1.5)#, las = 2)

plot(ma_N_2[,2], type = 'l', xlab = '', ylab = '', main = "Average Spring Indices", ylim = c(10, 18), xaxt = 'n', lwd = 2, cex.axis = 1.5, cex.main = 2)
axis(1, at = c(1:41),labels = label_2, cex.axis = 1.5)
#legend('topleft', legend = c("NEFSC", "ME-NH"), col = c('slategray', 'slateblue'), lty = 1, lwd = 2, bty = 'n', cex = 1.2)

plot(ma_H_2[,2], type = 'l', xlab = '', ylab = '', ylim = c(1,1.8), xaxt = 'n', lwd= 2, cex.axis = 1.5)
axis(1, at = c(1:41),labels = label_2, cex.axis = 1.5)

plot(ma_E_2[,2], type = 'l', xlab = "Survey Year", ylab = '', ylim = c(0.15,0.35), xaxt = 'n', lwd = 2, cex.axis = 1.5)
axis(1, at = c(1:41),labels = label_2, cex.axis = 1.5)

plot(ma_deltap_2[,2], type = 'l', xlab = '', ylab = '',  ylim = c(4.5,5), xaxt = 'n', lwd = 2, cex.axis = 1.5) 
axis(1, at = c(1:41),labels = label_2, cex.axis = 1.5)#, las = 2)

dev.off()


######## federal trawl
nefsc_fall <- by_tow[by_tow$SEASON == "FALL",]
nefsc_spring <- by_tow[by_tow$SEASON == "SPRING",]
layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
boxplot(nefsc_fall$N_species ~ nefsc_fall$EST_YEAR, ylab = "Number of species", main = "NEFSC Fall")
boxplot(nefsc_fall$H_index ~ nefsc_fall$EST_YEAR, ylab = "Shannon-Weiner Index")
boxplot(nefsc_fall$D_index ~ nefsc_fall$EST_YEAR, ylab = "Simpson Diversity Index")
boxplot(nefsc_fall$E_index ~ nefsc_fall$EST_YEAR, ylab = "Simpson Evenness Index")
boxplot(nefsc_spring$N_species ~ nefsc_spring$EST_YEAR, ylab = "Number of species", main = "NEFSC Spring")
boxplot(nefsc_spring$H_index ~ nefsc_spring$EST_YEAR, ylab = "Shannon-Weiner Index")
boxplot(nefsc_spring$D_index ~ nefsc_spring$EST_YEAR, ylab = "Simpson Diversity Index")
boxplot(nefsc_spring$E_index ~ nefsc_spring$EST_YEAR, ylab = "Simpson Evenness Index")


nefsc_fall <- tax_by_tow[tax_by_tow$SEASON == "FALL",]
nefsc_spring <- tax_by_tow[tax_by_tow$SEASON == "SPRING",]
layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
boxplot(nefsc_fall$delta ~ nefsc_fall$EST_YEAR, ylab = "Taxonomic Diversity", main = "NEFSC Fall")
boxplot(nefsc_fall$delta_star ~ nefsc_fall$EST_YEAR, ylab = "Taxonomic Distinctness", ylim = c(1,6))
boxplot(nefsc_fall$delta_plus ~ nefsc_fall$EST_YEAR, ylab = "Average Taxonomic Distinctness", ylim = c(1,6))
boxplot(nefsc_fall$delta_var ~ nefsc_fall$EST_YEAR, ylab = "Variation in Taxonomic Distinctness", ylim = c(0,5.5))
boxplot(nefsc_spring$delta ~ nefsc_spring$EST_YEAR, ylab = "Taxonomic Diversity", main = "NEFSC Spring")
boxplot(nefsc_spring$delta_star ~ nefsc_spring$EST_YEAR, ylab = "Taxonomic Distinctness", ylim = c(1,6))
boxplot(nefsc_spring$delta_plus ~ nefsc_spring$EST_YEAR, ylab = "Average Taxonomic Distinctness", ylim = c(1,6))
boxplot(nefsc_spring$delta_var ~ nefsc_spring$EST_YEAR, ylab = "Variation in Taxonomic Distinctness", ylim = c(0,5.5))

### ma
layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
boxplot(ma_fall_inds$N_species ~ ma_fall_inds$YEAR, ylab = "Number of species", main = "MDMF Fall", ylim = c( 0,30))
boxplot(ma_fall_inds$H_index ~ ma_fall_inds$YEAR, ylab = "Shannon-Weiner Index")
boxplot(ma_fall_inds$D_index ~ ma_fall_inds$YEAR, ylab = "Simpson Diversity Index")
boxplot(ma_fall_inds$E_index ~ ma_fall_inds$YEAR, ylab = "Simpson Evenness Index", ylim = c(0, 1))
boxplot(ma_spring_inds$N_species ~ ma_spring_inds$YEAR, ylab = "Number of species", main = "MDMF Spring", ylim = c( 0,30))
boxplot(ma_spring_inds$H_index ~ ma_spring_inds$YEAR, ylab = "Shannon-Weiner Index")
boxplot(ma_spring_inds$D_index ~ ma_spring_inds$YEAR, ylab = "Simpson Diversity Index")
boxplot(ma_spring_inds$E_index ~ ma_spring_inds$YEAR, ylab = "Simpson Evenness Index", ylim = c(0, 1))

layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
boxplot(ma_fall_inds$delta ~ ma_fall_inds$YEAR, ylab = "Taxonomic Diversity", main = "MDMF Fall")
boxplot(ma_fall_inds$delta_star ~ ma_fall_inds$YEAR, ylab = "Taxonomic Distinctness", ylim = c(1,6))
boxplot(ma_fall_inds$delta_plus ~ ma_fall_inds$YEAR, ylab = "Average Taxonomic Distinctness", ylim = c(1,6))
boxplot(ma_fall_inds$delta_var ~ ma_fall_inds$YEAR, ylab = "Variation in Taxonomic Distinctness", ylim = c(0,5.5))
boxplot(ma_spring_inds$delta ~ ma_spring_inds$YEAR, ylab = "Taxonomic Diversity", main = "MDMF Spring")
boxplot(ma_spring_inds$delta_star ~ ma_spring_inds$YEAR, ylab = "Taxonomic Distinctness", ylim = c(1,6))
boxplot(ma_spring_inds$delta_plus ~ ma_spring_inds$YEAR, ylab = "Average Taxonomic Distinctness", ylim = c(1,6))
boxplot(ma_spring_inds$delta_var ~ ma_spring_inds$YEAR, ylab = "Variation in Taxonomic Distinctness", ylim = c(0,5.5))

## me-nh 
layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
boxplot(me_fall_inds$N_species ~ me_fall_inds$YEAR, ylab = "Number of species", main = "ME-NH Fall", ylim = c( 0,40))
boxplot(me_fall_inds$H_index ~ me_fall_inds$YEAR, ylab = "Shannon-Weiner Index")
boxplot(me_fall_inds$D_index ~ me_fall_inds$YEAR, ylab = "Simpson Diversity Index")
boxplot(me_fall_inds$E_index ~ me_fall_inds$YEAR, ylab = "Simpson Evenness Index", ylim = c(0, 1))
boxplot(me_spring_inds$N_species ~ me_spring_inds$YEAR, ylab = "Number of species", main = "ME-NH Spring", ylim = c( 0,40))
boxplot(me_spring_inds$H_index ~ me_spring_inds$YEAR, ylab = "Shannon-Weiner Index")
boxplot(me_spring_inds$D_index ~ me_spring_inds$YEAR, ylab = "Simpson Diversity Index")
boxplot(me_spring_inds$E_index ~ me_spring_inds$YEAR, ylab = "Simpson Evenness Index", ylim = c(0, 1))

layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
boxplot(me_fall_inds$delta ~ me_fall_inds$YEAR, ylab = "Taxonomic Diversity", main = "ME-NH Fall")
boxplot(me_fall_inds$delta_star ~ me_fall_inds$YEAR, ylab = "Taxonomic Distinctness", ylim = c(1,6))
boxplot(me_fall_inds$delta_plus ~ me_fall_inds$YEAR, ylab = "Average Taxonomic Distinctness", ylim = c(1,6))
boxplot(me_fall_inds$delta_var ~ me_fall_inds$YEAR, ylab = "Variation in Taxonomic Distinctness", ylim = c(0,5.5))
boxplot(me_spring_inds$delta ~ me_spring_inds$YEAR, ylab = "Taxonomic Diversity", main = "ME-NH Spring")
boxplot(me_spring_inds$delta_star ~ me_spring_inds$YEAR, ylab = "Taxonomic Distinctness", ylim = c(1,6))
boxplot(me_spring_inds$delta_plus ~ me_spring_inds$YEAR, ylab = "Average Taxonomic Distinctness", ylim = c(1,6))
boxplot(me_spring_inds$delta_var ~ me_spring_inds$YEAR, ylab = "Variation in Taxonomic Distinctness", ylim = c(0,5.5))


#### median values across surveys
layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
layout.show(8)
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
label <- seq(1963, 2017)
label <- as.character(label)
label_2 <- seq(1968, 2017)
label_2 <- as.character(label_2)


plot(med_N_spp, type = 'l', xlab = '', ylab = "Number of Species", main = "Median Fall Indices"
     , ylim = c(0, 85), xaxt = 'n', lwd = 2)
axis(1, at = c(1:55),labels = label)#, las = 2)
lines(GOM_med_N_spp, type = 'l', col = 'pink', lwd = 2)
lines(ma_N[,1], ma_N[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_N[,1], me_N[,3], type = 'l', col = 'dark green', lwd = 2)

plot(med_H_ind, type = 'l', xlab = '', ylab = "Shannon-Weiner Index", ylim = c(0.5,3), xaxt = 'n', lwd = 2)
axis(1, at = c(1:55),labels = label)#, las = 2)
lines(GOM_med_H_ind, type = 'l', col = 'pink', lwd = 2)
lines(ma_H[,1], ma_H[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_H[,1], me_H[,3], type = 'l', col = 'dark green', lwd = 2)

plot(med_D_ind, type = 'l', xlab = '', ylab = "Simpson Diversity Index", ylim = c(2,10), xaxt = 'n', lwd = 2)
axis(1, at = c(1:55),labels = label)#, las = 2)
lines(GOM_med_D_ind, type = 'l', col = 'pink', lwd = 2)
lines(ma_D[,1], ma_D[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_D[,1], me_D[,3], type = 'l', col = 'dark green', lwd = 2)

plot(med_E_ind, type = 'l', xlab = "Survey Year", ylab = "Simpson Evenness Index", ylim = c(0.1, 0.5), xaxt = 'n', lwd = 2)
axis(1, at = c(1:55),labels = label)#, las = 2)
lines(GOM_med_E_ind, type = 'l', col = 'pink', lwd = 2)
lines(ma_E[,1], ma_E[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_E[,1], me_E[,3], type = 'l', col = 'dark green', lwd = 2)


plot(med_N_spp_2, type = 'l', xlab = '', ylab = '', main = "Median Spring Indices", ylim = c(0, 85), xaxt = 'n', lwd = 2)
axis(1, at = c(1:50),labels = label_2)
lines(GOM_med_N_spp_2, type = 'l', col = 'pink', lwd = 2)
lines(ma_N_2[,1], ma_N_2[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_N_2[,1], me_N_2[,3], type = 'l', col = 'dark green', lwd = 2)
legend('topright', legend = c("NEFSC", "MADMF", "ME-NH", "Offshore GB/GOM"), col = c('black', 'blue', 'dark green', 'pink'), lty = 1, lwd = 2, bty = 'n')

plot(med_H_ind_2, type = 'l', xlab = '', ylab = '', ylim = c(0.5,3), xaxt = 'n', lwd= 2)
axis(1, at = c(1:50),labels = label_2)
lines(GOM_med_H_ind_2, type = 'l', col = 'pink', lwd = 2)
lines(ma_H_2[,1], ma_H_2[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_H_2[,1], me_H_2[,3], type = 'l', col = 'dark green', lwd = 2)


plot(med_D_ind_2, type = 'l', xlab = '', ylab = '', ylim = c(2,10),  xaxt = 'n', lwd = 2)
axis(1, at = c(1:50),labels = label_2)
lines(GOM_med_D_ind_2, type = 'l', col = 'pink', lwd = 2)
lines(ma_D_2[,1], ma_D_2[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_D_2[,1], me_D_2[,3], type = 'l', col = 'dark green', lwd = 2)


plot(med_E_ind_2, type = 'l', xlab = "Survey Year", ylab = '', ylim = c(0.1,0.5), xaxt = 'n', lwd = 2)
axis(1, at = c(1:50),labels = label_2)
lines(GOM_med_E_ind_2, type = 'l', col = 'pink', lwd = 2)
lines(ma_E_2[,1], ma_E_2[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_E_2[,1], me_E_2[,3], type = 'l', col = 'dark green', lwd = 2)
#inds_by_area_04.pdf

##### median taxonomic diversity inds 

layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
par(mar = c(2, 4,1.5,0.5)) #B,L,T,R
options(scipen = 999)

plot(med_delta, type = 'l', xlab = '', ylab = "Taxonomic Diversity", main = "Median Fall Indices" 
     ,  ylim = c(0, 100000), xaxt = 'n', lwd = 2)
axis(1, at = c(1:55),labels = label)#, las = 2)
lines(GOM_med_delta, type = 'l', col = 'pink', lwd = 2)
lines(ma_delta[,1], ma_delta[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_delta[,1], me_delta[,3], type = 'l', col = 'dark green', lwd = 2)


plot(med_delta_star, type = 'l', xlab = '', ylab = "Taxonomic Distinctness", ylim = c(3.75,5.5), xaxt = 'n', lwd = 2)
axis(1, at = c(1:55),labels = label)#, las = 2)
lines(GOM_med_delta_star, type = 'l', col = 'pink', lwd = 2)
lines(ma_deltas[,1], ma_deltas[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_deltas[,1], me_deltas[,3], type = 'l', col = 'dark green', lwd = 2)


plot(med_delta_plus, type = 'l', xlab = '', ylab = "Average Taxonomic Distinctness",  ylim = c(4,5), xaxt = 'n', lwd = 2) 
axis(1, at = c(1:55),labels = label)#, las = 2)
lines(GOM_med_delta_plus, type = 'l', col = 'pink', lwd = 2)
lines(ma_deltap[,1], ma_deltap[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_deltap[,1], me_deltap[,3], type = 'l', col = 'dark green', lwd = 2)

plot(med_delta_var, type = 'l', xlab = "Survey Year", ylab = "Variation in Taxonomic Distinctness", ylim = c(0.8, 1.8),  xaxt = 'n', lwd = 2)
axis(1, at = c(1:55),labels = label)#, las = 2)
lines(GOM_med_delta_var, type = 'l', col = 'pink', lwd = 2)
lines(ma_deltav[,1], ma_deltav[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_deltav[,1], me_deltav[,3], type = 'l', col = 'dark green', lwd = 2)

plot(med_delta_2, type = 'l', xlab = '', ylab = "Taxonomic Diversity", main = "Median Spring Indices" 
     ,  ylim = c(0, 100000), xaxt = 'n', lwd = 2)
axis(1, at = c(1:50),labels = label_2)#, las = 2)
lines(GOM_med_delta_2, type = 'l', col = 'pink', lwd = 2)
lines(ma_delta_2[,1], ma_delta_2[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_delta_2[,1], me_delta_2[,3], type = 'l', col = 'dark green', lwd = 2)
legend('topleft', legend = c("NEFSC", "MADMF", "ME-NH", "GB/GOM Offshore"), col = c('black', 'blue', 'dark green', 'pink'), lty = 1, lwd = 2, bty = 'n')


plot(med_delta_star_2, type = 'l', xlab = '', ylab = "Taxonomic Distinctness", ylim = c(3.75,5.5), xaxt = 'n', lwd = 2)
lines(GOM_med_delta_star_2, type = 'l', col = 'pink', lwd = 2)
axis(1, at = c(1:50),labels = label_2)#, las = 2)
lines(ma_deltas_2[,1], ma_deltas_2[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_delta_s_2[,1], me_delta_s_2[,3], type = 'l', col = 'dark green', lwd = 2)


plot(med_delta_plus_2, type = 'l', xlab = '', ylab = "Average Taxonomic Distinctness",  ylim = c(4,5), xaxt = 'n', lwd = 2) 
axis(1, at = c(1:50),labels = label_2)#, las = 2)
lines(GOM_med_delta_plus_2, type = 'l', col = 'pink', lwd = 2)
lines(ma_deltap_2[,1], ma_deltap_2[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_deltap_2[,1], me_deltap_2[,3], type = 'l', col = 'dark green', lwd = 2)

plot(med_delta_var_2, type = 'l', xlab = "Survey Year", ylab = "Variation in Taxonomic Diversity", ylim = c(0.8, 1.8),  xaxt = 'n', lwd = 2)
axis(1, at = c(1:50),labels = label_2)#, las = 2)
lines(GOM_med_delta_var_2, type = 'l', col = 'pink', lwd = 2)
lines(ma_deltav_2[,1], ma_deltav_2[,3], type = 'l', col = 'blue', lwd = 2)
lines(me_deltav_2[,1], me_deltav_2[,3], type = 'l', col = 'dark green', lwd = 2)



##### looking at NEFSC spp over time (bubble plots)
# use from above 
spp_num <- subset(survdat_FL, select = c(EST_YEAR, COMNAME, NUMLEN))
write.csv(spp_num, "FL_spp_num.csv")
spp_num_SP <- subset(survdat_SP, select = c(EST_YEAR, COMNAME, NUMLEN))
write.csv(spp_num_SP, "SP_spp_num.csv")
#
# create pivot tables 
fall_spp <- read.csv("FL_spp_num_2.csv", header = TRUE)
head(fall_spp)
year <- as.character(seq(1963, 2008, by = 1))
sp <- "common_name"
colnames(fall_spp) <- c(sp,year)
fall_spp[is.na(fall_spp)] <- 0

require(reshape2)
require(ggplot2)

fall_spp_01 <- fall_spp[1:30,]
fall_spp_01 <- fall_spp[31:60,]
fall_spp_01 <- fall_spp[61:90,]
fall_spp_01 <- fall_spp[91:120,]
fall_spp_01 <- fall_spp[121:150,]
fall_spp_01 <- fall_spp[151:180,]
fall_spp_01 <- fall_spp[181:210,]



FL <- melt(fall_spp_01, id.vars = "common_name", variable.name = "Year", value.name = "Size")
# size difference is too great - add 1 and log transform
FL$Size <- (FL$Size + 1)

ggplot(FL, aes(x = Year, y = common_name)) + geom_point(aes(size = log(Size))) + 
  scale_size(range = range(log(FL$Size))) + theme(axis.text.x = element_text(angle = 90))


################### loop through each year and calculate avg # of each spp/tow######## bubble plots
# create tow/year identifier 
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/NEFSC")
load("survdat_SP.RData")

cols <- c(3, 43)
survdat_FL$AEW <-  cumsum(!duplicated(survdat_FL[cols])) 
survdat_SP$AEW <-  cumsum(!duplicated(survdat_SP[cols])) 


each_spp <- NULL
N_hauls <- unique(survdat_SP$AEW)
all_spp <- unique(survdat_SP$COMNAME) #524
N_spp <- length(all_spp)
N_spp_tow <- matrix(NA, nrow = N_spp, ncol = length(N_hauls))
for (j in 1:length(N_hauls)) {  
  diff_hauls <- survdat_SP[which(survdat_SP$AEW == j),]
  for (i in 1:N_spp){
  each_spp[i] <- sum(diff_hauls[diff_hauls[,32] == all_spp[i],45]) #counting spp that are there not matching correctly
}
N_spp_tow[,j] <- each_spp
}

setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/NEFSC")
save(N_spp_tow, file = "N_spp_tow_SP.RData")
#save(N_spp_tow, file = "N_spp_tow.RData")
load('N_spp_tow.RData')
load('survdat_FL.RData')
load('survdat_SP.RData')


# calc average # each spp/year
years <- NULL
for (i in 1:length(N_hauls)){
  years[i] <- mean(survdat_SP[survdat_SP$AEW == i,3])
}

N_spp_tow_2 <- rbind(years, N_spp_tow)
all_spp <- unique(survdat_SP$COMNAME) #524 #434
all_spp <- as.vector(all_spp)
names <- c("year", all_spp)
row.names(N_spp_tow_2) <- names

tow_spp <- t(N_spp_tow)
d.f <- data.frame(years, tow_spp)

avg_by_yr <- aggregate(.~ years, data = d.f, mean)
avg_by_yr_02 <- t(avg_by_yr)
row.names(avg_by_yr_02) <- names
test <- as.factor(avg_by_yr)
write.csv(avg_by_yr_02, 'avg_by_yr_SP.csv')
avg_spp <- read.csv('avg_by_yr_SP.csv')
#year <- as.character(seq(1963, 2017, by = 1))
year <- as.character(seq(1968, 2018, by = 1))
sp <- "common_name"
colnames(avg_spp) <- c(sp,year)
avg_spp <- avg_spp[-1,]

require(reshape2)
require(ggplot2)

avg_spp_01 <- avg_spp[1:30,]
avg_spp_01 <- avg_spp[31:60,]
avg_spp_01 <- avg_spp[61:90,]
avg_spp_01 <- avg_spp[91:120,]
avg_spp_01 <- avg_spp[121:150,]
avg_spp_01 <- avg_spp[151:180,]
avg_spp_01 <- avg_spp[181:210,]
avg_spp_01 <- avg_spp[211:240,]
avg_spp_01 <- avg_spp[241:270,]
avg_spp_01 <- avg_spp[271:300,]
avg_spp_01 <- avg_spp[301:330,]
avg_spp_01 <- avg_spp[331:360,]
avg_spp_01 <- avg_spp[361:390,]
avg_spp_01 <- avg_spp[391:420,]
avg_spp_01 <- avg_spp[421:450,]
avg_spp_01 <- avg_spp[421:434,] # sp
avg_spp_01 <- avg_spp[451:480,] 
avg_spp_01 <- avg_spp[481:510,]
avg_spp_01 <- avg_spp[511:524,]


FL <- melt(avg_spp_01, id.vars = "common_name", variable.name = "Year", value.name = "Size")
# size difference is too great - add 1 and log transform
FL$Size <- (FL$Size + 1)

ggplot(FL, aes(x = Year, y = common_name)) + geom_point(aes(size = log(Size))) + 
  scale_size(range = range(log(FL$Size))) + theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Spring Avg #/tow")



##### pair-wise correlation plots #######
#mean values
# fall 2000 - 2017
# spring 2001 - 2017
N_spp_matrix <- cbind(avg_N_spp[38:55], GOM_avg_N_spp[38:55] ,avg_N_spp_ma[23:40], avg_N_spp_me)
colnames(N_spp_matrix)  <- c("NEFSC", "GOM", "MA", "ME")
pairs(N_spp_matrix)
#install.packages('psych')
library(psych)

#bivariate scatterplot, histogram, and pearson correlations
layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2))
par(mar = c(2, 4,1.5,0.5)) 
pairs.panels(N_spp_matrix, main = "Fall number of species")

N_spp_matrix_spring <- cbind(avg_N_spp_2[34:50], GOM_avg_N_spp_2[34:50] ,avg_N_spp_ma_2[23:39], avg_N_spp_me_2)
colnames(N_spp_matrix_spring) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(N_spp_matrix_spring, main = "Spring number of species")

H_ind_matrix <- cbind(avg_H_ind[38:55], GOM_avg_H_ind[38:55], avg_H_ind_ma[23:40], avg_H_ind_me)
colnames(H_ind_matrix) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(H_ind_matrix, main = "Fall Shannon-Weiner Index")

H_ind_matrix_spring <- cbind(avg_H_ind_2[34:50], GOM_avg_H_ind_2[34:50], avg_H_ind_ma_2[23:39], avg_H_ind_me_2)
colnames(H_ind_matrix_spring) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(H_ind_matrix_spring, main = "Spring Shannon-Weiner Index")

D_ind_matrix <- cbind(avg_D_ind[38:55], GOM_avg_D_ind[38:55], avg_D_ind_ma[23:40], avg_D_ind_me)
colnames(D_ind_matrix) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(D_ind_matrix, main = "Fall Simpson's Diversity Index")

D_ind_matrix_spring <- cbind(avg_D_ind_2[34:50], GOM_avg_D_ind_2[34:50], avg_D_ind_ma_2[23:39], avg_D_ind_me_2)
colnames(D_ind_matrix_spring) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(D_ind_matrix_spring, main = "Spring Simpson's Diversity Index")

E_ind_matrix <- cbind(avg_E_ind[38:55], GOM_avg_E_ind[38:55], avg_E_ind_ma[23:40], avg_E_ind_me)
colnames(E_ind_matrix) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(E_ind_matrix, main = "Fall Simpson's Evenness Index")

E_ind_matrix_spring <- cbind(avg_E_ind_2[34:50], GOM_avg_E_ind_2[34:50], avg_E_ind_ma_2[23:39], avg_E_ind_me_2)
colnames(E_ind_matrix_spring) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(E_ind_matrix_spring, main = "Spring Simpson's Evenness Index")

delta_matrix <- cbind(avg_delta[38:55], GOM_avg_delta[38:55], avg_delta_ma[23:40], avg_delta_me)
colnames(delta_matrix) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(delta_matrix, main = "Fall Taxonomic Diversity")

delta_matrix_spring <- cbind(avg_delta_2[34:50], GOM_avg_delta_2[34:50], avg_delta_ma_2[23:39], avg_delta_me_2)
colnames(delta_matrix_spring) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(delta_matrix_spring, main = "Spring Taxonomic Diversity")

delta_star_matrix <- cbind(avg_delta_star[38:55], GOM_avg_delta_star[38:55], avg_delta_star_ma[23:40], avg_delta_star_me)
colnames(delta_star_matrix) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(delta_star_matrix, main = "Fall Taxonomic Distinctness")

delta_star_matrix_spring <- cbind(avg_delta_star_2[34:50], GOM_avg_delta_star_2[34:50], avg_delta_star_ma_2[23:39], avg_delta_star_me_2)
colnames(delta_star_matrix_spring) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(delta_star_matrix_spring, main = "Spring Taxonomic Distinctness")

delta_plus_matrix <- cbind(avg_delta_plus[38:55], GOM_avg_delta_plus[38:55], avg_delta_plus_ma[23:40], avg_delta_plus_me)
colnames(delta_plus_matrix) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(delta_plus_matrix, main = "Fall Average Taxonomic Distinctness")

delta_plus_matrix_spring <- cbind(avg_delta_plus_2[34:50], GOM_avg_delta_plus_2[34:50], avg_delta_plus_ma_2[23:39], avg_delta_plus_me_2)
colnames(delta_plus_matrix_spring) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(delta_plus_matrix_spring, main = "Spring Average Taxonomic Distinctness")

delta_var_matrix <- cbind(avg_delta_var[38:55], GOM_avg_delta_var[38:55], avg_delta_var_ma[23:40], avg_delta_var_me)
colnames(delta_var_matrix) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(delta_var_matrix, main = "Fall Variation in Taxonomic Distinctness")

delta_var_matrix_spring <- cbind(avg_delta_var_2[34:50], GOM_avg_delta_var_2[34:50], avg_delta_var_ma_2[23:39], avg_delta_var_me_2)
colnames(delta_var_matrix_spring) <- c("NEFSC", "GOM", "MA", "ME")
pairs.panels(delta_var_matrix_spring, main = "Spring Variation in Taxonomic Distinctness")

#negative relationship between MA and ME
#strong positive between NEFSC and GOM

