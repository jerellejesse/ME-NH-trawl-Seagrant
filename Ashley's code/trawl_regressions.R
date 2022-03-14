####################################################################################
### Testing for correlations btw surveys and trends for each survey
### UM Seagrant; climate impacts on Maine coastal fisheries using the ME-NH inshore trawl
### Objective 1 part 1
### 10/15/2018
### A.E. Weston
####################################################################################

# don't need to clip time series

# Fall
# whole NEFSC trawl
#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/NEFSC")
setwd("C:/Users/jjesse/Box/Kerr Lab/Fisheries Science Lab/ME NH Trawl- Seagrant/Seagrant-AEW/NEFSC")
FL_by_tow <- read.csv("nefsc_FL_by_tow.csv", header = TRUE)
SP_by_tow <- read.csv("nefsc_SP_by_tow.csv", header = TRUE)

#load('NEFSC_taxinds_by_tow.RData')

#########remove strata from canadian waters and south of hatteras 
can <- c(1351, 1350, 1310, 1320, 1330, 1410,1420, 1490, 1990)
other <- unique(FL_by_tow$STRATUM[FL_by_tow$STRATUM > 3990]) #South of hatteras and scotian shelf 
remove <- c(can, other)
fall_by_tow <- FL_by_tow[!FL_by_tow$STRATUM %in% remove,]

other_2 <- unique(SP_by_tow$STRATUM[SP_by_tow$STRATUM > 3990])
remove <- c(can, other_2)
spring_by_tow <- SP_by_tow[SP_by_tow$SEASON == "SPRING",]
fall_by_tow <- fall_by_tow[fall_by_tow$EST_YEAR >= 2000,] # clip to same years 
spring_by_tow <- spring_by_tow[spring_by_tow$EST_YEAR >= 2000,] # clip to same years 

#tax_by_tow <- tax_by_tow[!tax_by_tow$STRATUM %in% remove,]
#fall_inds <- subset(tax_by_tow, SEASON == "FALL")
#spring_inds <- subset(tax_by_tow, SEASON == "SPRING")


surv <- rep("NEFSC", 16660) 
surv <- rep("NEFSC", 5680) 

cols <- c(4, 41) #N species
fall_N <- fall_by_tow[,cols]
NEFSC_N_spp <- cbind(fall_N, surv)
colnames(NEFSC_N_spp) <- c("YEAR", "N_species", "surv")
cols <- c(4, 42)
fall_H <- fall_by_tow[,cols]
NEFSC_fall_H <- cbind(fall_H, surv)
colnames(NEFSC_fall_H) <- c("YEAR", "H_index", "surv")
cols <- c(4, 43)
fall_D <- fall_by_tow[,cols]
NEFSC_fall_D <- cbind(fall_D, surv)
colnames(NEFSC_fall_D) <- c("YEAR", "D_index", "surv")
cols <- c(4,44)
fall_E <- fall_by_tow[,cols]
NEFSC_fall_E <- cbind(fall_E, surv)
colnames(NEFSC_fall_E) <- c("YEAR", "E_index", "surv")




cols <- c(4, 41)
surv <- rep("NEFSC", 17289)
surv <- rep("NEFSC", 6418)
spring_N <- spring_by_tow[,cols]
NEFSC_N_sp_spp <- cbind(spring_N, surv)
colnames(NEFSC_N_sp_spp) <- c("YEAR", "N_species", "surv")
cols <- c(4,42)
spring_H <- spring_by_tow[,cols]
NEFSC_spring_H <- cbind(spring_H, surv)
colnames(NEFSC_spring_H) <- c("YEAR", "H_index", "surv")
cols <- c(4,43)
spring_D <- spring_by_tow[,cols]
NEFSC_spring_D <- cbind(spring_D, surv)
colnames(NEFSC_spring_D) <- c("YEAR", "D_index", "surv")
cols <- c(4,44)
spring_E <- spring_by_tow[,cols]
NEFSC_spring_E <- cbind(spring_E, surv)
colnames(NEFSC_spring_E) <- c("YEAR", "E_index", "surv")


# GOM and GB subset
GOM_fall <- fall_by_tow[fall_by_tow$STRATUM == 1130 | fall_by_tow$STRATUM == 1140 | fall_by_tow$STRATUM == 1150 | fall_by_tow$STRATUM == 1160 |
                          fall_by_tow$STRATUM == 1170 | fall_by_tow$STRATUM == 1180 | fall_by_tow$STRATUM == 1190 | fall_by_tow$STRATUM == 1200 |
                          fall_by_tow$STRATUM == 1210 | fall_by_tow$STRATUM == 1220 | fall_by_tow$STRATUM == 1230 | fall_by_tow$STRATUM == 1240 |
                          fall_by_tow$STRATUM == 1250 | fall_by_tow$STRATUM == 1260 | fall_by_tow$STRATUM == 1270 |
                          fall_by_tow$STRATUM == 1280 | fall_by_tow$STRATUM == 1290 | fall_by_tow$STRATUM == 1300 | fall_by_tow$STRATUM == 1360 |
                          fall_by_tow$STRATUM == 1370 | fall_by_tow$STRATUM == 1380 | fall_by_tow$STRATUM == 1390 | fall_by_tow$STRATUM == 1400,]

GOM_fall <- GOM_fall[GOM_fall$EST_YEAR >= 2000,]

#GOM_fall_tax <- fall_inds[fall_inds$STRATUM == 1130 | fall_inds$STRATUM == 1140 | fall_inds$STRATUM == 1150 | fall_inds$STRATUM == 1160 |
#                            fall_inds$STRATUM == 1170 | fall_inds$STRATUM == 1180 | fall_inds$STRATUM == 1190 | fall_inds$STRATUM == 1200 |
#                            fall_inds$STRATUM == 1210 | fall_inds$STRATUM == 1220 | fall_inds$STRATUM == 1230 | fall_inds$STRATUM == 1240 |
#                            fall_inds$STRATUM == 1250 | fall_inds$STRATUM == 1260 | fall_inds$STRATUM == 1270 |
#                            fall_inds$STRATUM == 1280 | fall_inds$STRATUM == 1290 | fall_inds$STRATUM == 1300 | fall_inds$STRATUM == 1360 |
#                            fall_inds$STRATUM == 1370 | fall_inds$STRATUM == 1380 | fall_inds$STRATUM == 1390 | fall_inds$STRATUM == 1400,]

surv <- rep("GOM", 7090)
surv <- rep("GOM", 2262)

cols <- c(4,41)
GOM_N <- GOM_fall[,cols]
GOM_N_spp <- cbind(GOM_N, surv)
colnames(GOM_N_spp) <- c("YEAR", "N_species", "surv")
cols <- c(4,42)
GOM_H <- GOM_fall[,cols]
GOM_H__fall <- cbind(GOM_H, surv)
colnames(GOM_H__fall) <- c("YEAR", "H_index", "surv")
cols <- c(4,43)
GOM_D <- GOM_fall[,cols]
GOM_D_fall <- cbind(GOM_D, surv)
colnames(GOM_D_fall) <- c("YEAR", "D_index", "surv")
cols <- c(4,44)
GOM_E <- GOM_fall[,cols]
GOM_E_fall <- cbind(GOM_E, surv)
colnames(GOM_E_fall) <- c("YEAR", "E_index", "surv")


GOM_spring <- spring_by_tow[spring_by_tow$STRATUM == 1130 | spring_by_tow$STRATUM == 1140 | spring_by_tow$STRATUM == 1150 | spring_by_tow$STRATUM == 1160 |
                              spring_by_tow$STRATUM == 1170 | spring_by_tow$STRATUM == 1180 | spring_by_tow$STRATUM == 1190 | spring_by_tow$STRATUM == 1200 |
                              spring_by_tow$STRATUM == 1210 | spring_by_tow$STRATUM == 1220 | spring_by_tow$STRATUM == 1230 | spring_by_tow$STRATUM == 1240 |
                              spring_by_tow$STRATUM == 1250 | spring_by_tow$STRATUM == 1260 | spring_by_tow$STRATUM == 1270 |
                              spring_by_tow$STRATUM == 1280 | spring_by_tow$STRATUM == 1290 | spring_by_tow$STRATUM == 1300 | spring_by_tow$STRATUM == 1360 |
                              spring_by_tow$STRATUM == 1370 | spring_by_tow$STRATUM == 1380 | spring_by_tow$STRATUM == 1390 | spring_by_tow$STRATUM == 1400,]
GOM_spring <- GOM_spring[GOM_spring$EST_YEAR >= 2001,]

#GOM_spring_tax <- spring_inds[spring_inds$STRATUM == 1130 | spring_inds$STRATUM == 1140 | spring_inds$STRATUM == 1150 | spring_inds$STRATUM == 1160 |
#                                spring_inds$STRATUM == 1170 | spring_inds$STRATUM == 1180 | spring_inds$STRATUM == 1190 | spring_inds$STRATUM == 1200 |
#                                spring_inds$STRATUM == 1210 | spring_inds$STRATUM == 1220 | spring_inds$STRATUM == 1230 | spring_inds$STRATUM == 1240 |
#                                spring_inds$STRATUM == 1250 | spring_inds$STRATUM == 1260 | spring_inds$STRATUM == 1270 |
#                                spring_inds$STRATUM == 1280 | spring_inds$STRATUM == 1290 | spring_inds$STRATUM == 1300 | spring_inds$STRATUM == 1360 |
#                                spring_inds$STRATUM == 1370 | spring_inds$STRATUM == 1380 | spring_inds$STRATUM == 1390 | spring_inds$STRATUM == 1400,]

surv <- rep("GOM", 6375)
surv <- rep("GOM", 2298)
cols <- c(4,41)
GOM_N <- GOM_spring[,cols]
GOM_N_sp_spp <- cbind(GOM_N, surv)
colnames(GOM_N_sp_spp) <- c("YEAR", "N_species", "surv")
cols <- c(4,42)
GOM_H_sp <- GOM_spring[,cols]
GOM_H_spring <- cbind(GOM_H_sp, surv)
colnames(GOM_H_spring) <- c("YEAR", "H_index", "surv")
cols <- c(4,43)
GOM_D_sp <- GOM_spring[,cols]
GOM_D_spring <- cbind(GOM_D_sp, surv)
colnames(GOM_D_spring) <- c("YEAR", "D_index", "surv")
cols <- c(4,44)
GOM_E_sp <- GOM_spring[,cols]
GOM_E_spring <- cbind(GOM_E_sp, surv)
colnames(GOM_E_spring) <- c("YEAR", "E_index", "surv")

# Mass DMF
#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/MDMF")
setwd("C:/Users/jjesse/Box/Kerr Lab/Fisheries Science Lab/ME NH Trawl- Seagrant/Seagrant-AEW/MDMF")

ma_fall_inds <- read.csv("MDMF_fall_div_ind_by_tow.csv")
ma_fall_inds <- ma_fall_inds[ma_fall_inds$YEAR >= 2000,]
ma_spring_inds <- read.csv("MDMF_spring_div_ind_by_tow.csv")
ma_spring_inds <- ma_spring_inds[ma_spring_inds$YEAR >= 2000,]

cols <- c(8,12)
surv <- rep("MA", 3635)
surv <- rep("MA", 1690)

ma_fall_N <- ma_fall_inds[,cols]
ma_N_spp <- cbind(ma_fall_N, surv)
cols <- c(8,13)
ma_H <- ma_fall_inds[,cols]
ma_H_fall <- cbind(ma_H, surv)
cols <- c(8,14)
ma_D <- ma_fall_inds[,cols]
ma_D_fall <- cbind(ma_D, surv)
cols <- c(8,15)
ma_E <- ma_fall_inds[,cols]
ma_E_fall <- cbind(ma_E, surv)

cols <- c(9,13)
ma_spring_N <- ma_spring_inds[,cols]
surv <- rep("MA", 3977)
surv <- rep("MA", 1890)
ma_sp_N_spp <- cbind(ma_spring_N, surv)
cols <- c(9, 14)
ma_H_sp <- ma_spring_inds[,cols]
ma_H_spring <- cbind(ma_H_sp, surv)
cols <- c(9,15)
ma_D_sp <- ma_spring_inds[,cols]
ma_D_spring <- cbind(ma_D_sp, surv)
cols <- c(9,16)
ma_E_sp <- ma_spring_inds[,cols]
ma_E_spring <- cbind(ma_E_sp, surv)

# ME-NH 
#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
setwd("C:/Users/jjesse/Box/Kerr Lab/Fisheries Science Lab/ME NH Trawl- Seagrant/Seagrant-AEW/Results")

me_inds <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
me_fall_inds <- subset(me_inds, SEASON == "FL")
me_spring_inds <- subset(me_inds, SEASON == 'SP')
YEAR <- me_fall_inds$YEAR + 2000
me_fall_inds$YEAR <- YEAR
surv <- rep("ME", 1518)
cols <- c(4, 32)
me_fall_N <- me_fall_inds[,cols]
N_spp <- cbind(me_fall_N, surv)
cols <- c(4,33)
me_H <- me_fall_inds[,cols]
me_H_fall <- cbind(me_H, surv)
cols <- c(4,34)
me_D <- me_fall_inds[,cols]
me_D_fall <- cbind(me_D, surv)
cols <- c(4, 35)
me_E <- me_fall_inds[,cols]
me_E_fall <- cbind(me_E, surv)

YEAR <- me_spring_inds$YEAR +2000
me_spring_inds$YEAR <- YEAR
surv <- rep("ME", 1900)
cols <- c(4, 32)
me_spring_N <- me_spring_inds[,cols]
me_sp_N_spp <- cbind(me_spring_N, surv)
cols <- c(4,33)
me_H_sp <- me_spring_inds[,cols]
me_H_spring <- cbind(me_H_sp, surv)
cols <- c(4, 34)
me_D_sp <- me_spring_inds[,cols]
me_D_spring <- cbind(me_D_sp, surv)
cols <- c(4,35)
me_E_sp <- me_spring_inds[,cols]
me_E_spring <- cbind(me_E_sp, surv)

#### Taxonomic indices ####
#nefsc
setwd("C:/Users/jjesse/Box/Kerr Lab/Fisheries Science Lab/ME NH Trawl- Seagrant/Seagrant-AEW/NEFSC")

fall_tax<-read.csv("NEFSC_all_metrics_FL.csv")
spring_tax<-read.csv("NEFSC_all_metrics_SP.csv")
#remove strata from canadian waters and south of hatteras 
can <- c(1351, 1350, 1310, 1320, 1330, 1410,1420, 1490, 1990)
other <- unique(fall_tax$STRATUM[fall_tax$STRATUM > 3990]) #South of hatteras and scotian shelf 
remove <- c(can, other)
fall_tax_tow <- fall_tax[!fall_tax$STRATUM %in% remove,]

other_2 <- unique(spring_tax$STRATUM[spring_tax$STRATUM > 3990])
remove <- c(can, other_2)
spring_tax_tow <- spring_tax[spring_tax$SEASON == "SPRING",]
fall_tax_tow <- fall_tax_tow[fall_tax_tow$EST_YEAR >= 2000,] # clip to same years 
spring_tax_tow <- spring_tax_tow[spring_tax_tow$EST_YEAR >= 2000,] 

#FALL
surv<- rep("NEFSC", 5680)#fall

cols <- c(4, 41) #delta
fall_taxdiv <- fall_tax_tow[,cols]
NEFSC_fall_taxdiv <- cbind(fall_taxdiv, surv)
colnames(NEFSC_fall_taxdiv) <- c("YEAR", "delta", "surv")
cols <- c(4, 42) #delta star
fall_taxdist <- fall_tax_tow[,cols]
NEFSC_fall_taxdist <- cbind(fall_taxdist, surv)
colnames(NEFSC_fall_taxdist) <- c("YEAR", "delta_star", "surv")
cols <- c(4, 43) #delta plus
fall_avgtax <- fall_tax_tow[,cols]
NEFSC_fall_avgtax <- cbind(fall_avgtax, surv)
colnames(NEFSC_fall_avgtax) <- c("YEAR", "delta_plus", "surv")
cols <- c(4, 44) #delta var
fall_taxvar <- fall_tax_tow[,cols]
NEFSC_fall_taxdvar <- cbind(fall_taxvar, surv)
colnames(NEFSC_fall_taxvar) <- c("YEAR", "delta_star", "surv")


#spring
surv<- rep("NEFSC", 6414)#spring
cols <- c(4, 41) #delta
spring_taxdiv <- spring_tax_tow[,cols]
NEFSC_spring_taxdiv <- cbind(spring__taxdiv, surv)
colnames(NEFSC_spring_taxdiv) <- c("YEAR", "delta", "surv")
cols <- c(4, 42) #delta star
spring_taxdist <- spring_tax_tow[,cols]
NEFSC_spring_taxdist <- cbind(spring_taxdist, surv)
colnames(NEFSC_spring_taxdist) <- c("YEAR", "delta_star", "surv")
cols <- c(4, 43) #delta plus
spring_avgtax <- spring_tax_tow[,cols]
NEFSC_spring_avgtax <- cbind(spring_avgtax, surv)
colnames(NEFSC_spring_avgtax) <- c("YEAR", "delta_plus", "surv")
cols <- c(4, 44) #delta var
spring_taxvar <- spring_tax_tow[,cols]
NEFSC_spring_taxdvar <- cbind(spring_taxvar, surv)
colnames(NEFSC_spring_taxvar) <- c("YEAR", "delta_star", "surv")


## GOM


## MA


## ME-NH



####### LMs between surveys #########
# Fall N_species
all_N_spp <- rbind(ma_N_spp, N_spp, GOM_N_spp, NEFSC_N_spp)

#install.packages('lsmeans')
library(lsmeans)
N.interaction <- lm(N_species ~ YEAR*surv, data = all_N_spp)
anova(N.interaction)
N.interaction$coefficients
N.lst <- lstrends(N.interaction, "surv", var = "YEAR")
N.lst
pairs(N.lst)

# Fall H index
all_H_fall <- rbind(ma_H_fall, me_H_fall, NEFSC_fall_H, GOM_H__fall)
fall.H.inter <- lm(H_index ~ YEAR*surv, data = all_H_fall)
fall.H.lst <- lstrends(fall.H.inter, "surv", var = "YEAR")
pairs(fall.H.lst)

all_D_fall <- rbind(ma_D_fall, me_D_fall, NEFSC_fall_D, GOM_D_fall)
fall.D.inter <- lm(D_index ~ YEAR*surv, data = all_D_fall)
fall.D.lst <- lstrends(fall.D.inter, "surv", var = "YEAR")
pairs(fall.D.lst)

all_E_fall <- rbind(ma_E_fall, me_E_fall, NEFSC_fall_E, GOM_E_fall)
fall.E.inter <- lm(E_index ~ YEAR*surv, data = all_E_fall)
fall.E.lst <- lstrends(fall.E.inter, "surv", var = "YEAR")
pairs(fall.E.lst)

# Spring N_species
all_N_sp_spp <- rbind(ma_sp_N_spp, me_sp_N_spp, GOM_N_sp_spp, NEFSC_N_sp_spp)
spr.N.interaction <- lm(N_species ~ YEAR*surv, data = all_N_sp_spp)
spr.N.lst <- lstrends(spr.N.interaction, "surv", var = "YEAR")
pairs(spr.N.lst)

all_H_spring <- rbind(ma_H_spring, me_H_spring, GOM_H_spring, NEFSC_spring_H)
spr.H.inter <- lm(H_index ~ YEAR*surv, data = all_H_spring)
spr.H.lst <- lstrends(spr.H.inter, "surv", var = "YEAR")
pairs(spr.H.lst)

all_D_spring <- rbind(ma_D_spring, me_D_spring, GOM_D_spring, NEFSC_spring_D)
spr.D.inter <- lm(D_index ~ YEAR*surv, data = all_D_spring)
spr.D.lst <- lstrends(spr.D.inter, "surv", var = "YEAR")
pairs(spr.D.lst)

all_E_spring <- rbind(ma_E_spring, me_E_spring, GOM_E_spring, NEFSC_spring_E)
spr.E.inter <- lm(E_index ~ YEAR*surv, data = all_E_spring)
spr.E.lst <- lstrends(spr.E.inter, "surv", var = "YEAR")
pairs(spr.E.lst)






######## Linear Models for each survey ##################
# X ~ (year)

# NEFSC fall
N_NEFSC_FL <- lm(data = fall_by_tow, N_species ~ EST_YEAR)
summary(N_NEFSC_FL)
H_NEFSC_FL <- lm(data = fall_by_tow, H_index ~ EST_YEAR)
summary(H_NEFSC_FL)
D_NEFSC_FL <- lm(data = fall_by_tow, D_index ~ EST_YEAR)
summary(D_NEFSC_FL)
E_NEFSC_FL <- lm(data = fall_by_tow, E_index ~ EST_YEAR)
summary(E_NEFSC_FL)

# NEFSC spring
N_NEFSC_SP <- lm(data = spring_by_tow, N_species ~ EST_YEAR)
summary(N_NEFSC_SP)
H_NEFSC_SP <- lm(data = spring_by_tow, H_index ~ EST_YEAR)
summary(H_NEFSC_SP)
D_NEFSC_SP <- lm(data = spring_by_tow, D_index ~ EST_YEAR)
summary(D_NEFSC_SP)
E_NEFSC_SP <- lm(data = spring_by_tow, E_index ~ EST_YEAR)
summary(E_NEFSC_SP)


# GOM fall
N_GOM_FL <- lm(data = GOM_fall, N_species ~ EST_YEAR)
summary(N_GOM_FL)
H_GOM_FL <- lm(data = GOM_fall, H_index ~ EST_YEAR)
summary(H_GOM_FL)
D_GOM_FL <- lm(data = GOM_fall, D_index ~ EST_YEAR)
summary(D_GOM_FL)
E_GOM_FL <- lm(data = GOM_fall, E_index ~ EST_YEAR)
summary(E_GOM_FL)

# GOM spring
N_GOM_SP <- lm(data = GOM_spring, N_species ~ EST_YEAR)
summary(N_GOM_SP)
H_GOM_SP <- lm(data = GOM_spring, H_index ~ EST_YEAR)
summary(H_GOM_SP)
D_GOM_SP <- lm(data = GOM_spring, D_index ~ EST_YEAR)
summary(D_GOM_SP)
E_GOM_SP <- lm(data = GOM_spring, E_index ~ EST_YEAR)
summary(E_GOM_SP)


# MA fall
N_MA_FL <- lm(data = ma_fall_inds, N_species ~ YEAR)
summary(N_MA_FL)
H_MA_FL <- lm(data = ma_fall_inds, H_index ~ YEAR)
summary(H_MA_FL)
D_MA_FL <- lm(data = ma_fall_inds, D_index ~ YEAR)
summary(D_MA_FL)
E_MA_FL <- lm(data = ma_fall_inds, E_index ~ YEAR)
summary(E_MA_FL)

# MA spring
N_MA_SP <- lm(data = ma_spring_inds, N_species ~ YEAR)
summary(N_MA_SP)
H_MA_SP <- lm(data = ma_spring_inds, H_index ~ YEAR)
summary(H_MA_SP)
D_MA_SP <- lm(data = ma_spring_inds, D_index ~ YEAR)
summary(D_MA_SP)
E_MA_SP <- lm(data = ma_spring_inds, E_index ~ YEAR)
summary(E_MA_SP)


# ME fall
N_ME_FL <- lm(data = me_fall_inds, N_species ~ YEAR)
summary(N_ME_FL)
H_ME_FL <- lm(data = me_fall_inds, H_index ~ YEAR)
summary(H_ME_FL)
D_ME_FL <- lm(data = me_fall_inds, D_index ~ YEAR)
summary(D_ME_FL)
E_ME_FL <- lm(data = me_fall_inds, E_index ~ YEAR)
summary(E_ME_FL)


# ME spring
N_ME_SP <- lm(data = me_spring_inds, N_species ~ YEAR)
summary(N_ME_SP)
H_ME_SP <- lm(data = me_spring_inds, H_index ~ YEAR)
summary(H_ME_SP)
D_ME_SP <- lm(data = me_spring_inds, D_index ~ YEAR)
summary(D_ME_SP)
E_ME_SP <- lm(data = me_spring_inds, E_index ~ YEAR)
summary(E_ME_SP)


#### Taxonomic indices linear models ####

# NEFSC fall
taxdiv_NEFSC_FL <- lm(data = fall_by_tow, delta ~ EST_YEAR)
summary(taxdiv_NEFSC_FL)
taxdist_NEFSC_FL <- lm(data = fall_by_tow, delta_star ~ EST_YEAR)
summary(taxdist_NEFSC_FL)
avgtax_NEFSC_FL <- lm(data = fall_by_tow, delta_plus ~ EST_YEAR)
summary(avgtax_NEFSC_FL)
taxvar_NEFSC_FL <- lm(data = fall_by_tow, delta_var ~ EST_YEAR)
summary(taxvar_NEFSC_FL)

# NEFSC spring
tax_NEFSC_SP <- lm(data = spring_by_tow, delta ~ EST_YEAR)
summary(tax_NEFSC_SP)
taxdist_NEFSC_SP <- lm(data = spring_by_tow, delta_star ~ EST_YEAR)
summary(taxdist_NEFSC_SP)
avgtax_NEFSC_SP <- lm(data = spring_by_tow, delta_plus ~ EST_YEAR)
summary(avgtax_NEFSC_SP)
taxvar_NEFSC_SP <- lm(data = spring_by_tow, delta_var ~ EST_YEAR)
summary(taxvar_NEFSC_SP)


# GOM fall
tax_GOM_FL <- lm(data = GOM_fall, delta ~ EST_YEAR)
summary(tax_GOM_FL)
taxdist_GOM_FL <- lm(data = GOM_fall, delta_star ~ EST_YEAR)
summary(taxdist_GOM_FL)
avgtax_GOM_FL <- lm(data = GOM_fall, delta_plus ~ EST_YEAR)
summary(avgtax_GOM_FL)
taxvar_GOM_FL <- lm(data = GOM_fall, delta_var~ EST_YEAR)
summary(taxvar_GOM_FL)

# GOM spring
tax_GOM_SP <- lm(data = GOM_spring, delta ~ EST_YEAR)
summary(tax_GOM_SP)
taxdist_GOM_SP <- lm(data = GOM_spring, delta_star ~ EST_YEAR)
summary(taxdist_GOM_SP)
avgtax_GOM_SP <- lm(data = GOM_spring, delta_plus ~ EST_YEAR)
summary(avgtax_GOM_SP)
taxvar_GOM_SP <- lm(data = GOM_spring, delta_var ~ EST_YEAR)
summary(taxvar_GOM_SP)


## MA spring
tax_MA_SP <- lm(data = ma_spring_inds, delta ~ YEAR)
summary(tax_MA_SP)
taxdist_MA_SP <- lm(data = ma_spring_inds, delta_star ~ YEAR)
summary(taxdist_MA_SP)
avgtaxdist_MA_SP <- lm(data = ma_spring_inds, delta_plus ~ YEAR)
summary(avgtaxdist_MA_SP)
taxvar_MA_SP <- lm(data = ma_spring_inds, delta_var ~ YEAR)
summary(taxvar_MA_SP)


# MA fall
tax_MA_FL <- lm(data = ma_fall_inds, delta ~ YEAR)
summary(tax_MA_FL)
taxdist_MA_FL <- lm(data = ma_fall_inds, delta_star ~ YEAR)
summary(taxdist_MA_FL)
avgtaxdist_MA_FL <- lm(data = ma_fall_inds, delta_plus ~ YEAR)
summary(avgtaxdist_MA_FL)
taxvar_MA_FL <- lm(data = ma_fall_inds, delta_var ~ YEAR)
summary(taxvar_MA_FL)



## ME-NH spring
tax_ME_SP <- lm(data = me_spring_inds, delta ~ YEAR)
summary(tax_ME_SP)
taxdist_ME_SP <- lm(data = me_spring_inds, delta_star ~ YEAR)
summary(taxdist_ME_SP)
avgtaxdist_ME_SP <- lm(data = me_spring_inds, delta_plus ~ YEAR)
summary(avgtaxdist_ME_SP)
taxvar_ME_SP <- lm(data = me_spring_inds, delta_var ~ YEAR)
summary(taxvar_ME_SP)



# ME-NH fall
tax_ME_FL <- lm(data = me_fall_inds, delta ~ YEAR)
summary(tax_ME_FL)
taxdist_ME_FL <- lm(data = me_fall_inds, delta_star ~ YEAR)
summary(taxdist_ME_FL)
avgtaxdist_ME_FL <- lm(data = me_fall_inds, delta_plus ~ YEAR)
summary(vgtaxdist_ME_FL)
taxvar_ME_FL <- lm(data = me_fall_inds, delta_var ~ YEAR)
summary(taxvar_ME_FL)
