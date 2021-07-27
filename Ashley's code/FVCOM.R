########################################################
## FVCOM data; surface/bottom salinity/temperature 1978- 2018
## pulled by Chang Liu (SMAST)
## A.E. Weston
## 10/23/2018
########################################################

# 3 separate files (1978-2013, 2014-2016, 2017-2018)
#setwd("J:/Research/Kerr Lab/FVCOM_sur_bot_tempsal")
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/FVCOM_sur_bot_tempsal")

#library(sp)
# Defining CRS spatial information for the two datasets used  (A. Allyn)
#proj.wgs84 <- CRS("+init=epsg:4326") #WGS84
#proj.utm <- CRS("+init=epsg:2960") #UTM 19

library(RNetCDF)
# observations for 1978-2013
# dimensions = [time, layer (surface/bottom), node] 
# time (432) = 36 yrs * 12 mon
# time = "days since 1858-11-17 00:00:00"
# 48451 observations (nodes)
sal_temp_start <- 'GoM_surbot_tempsal_1978-2013.nc' #reads in file 
historic.sal.temp <- open.nc(sal_temp_start) # opens the file
print.nc(historic.sal.temp) # gives general information about the file
obs_78_13 <- read.nc(historic.sal.temp) # creates an R object that can be manipulated
close.nc(historic.sal.temp) #closes the file
str(obs_78_13)
#:CoordinateProjection = "none" ;



time <- obs_78_13$Times
# matrix of surface temperature (time, location); 48451, 432
surf_temp <- obs_78_13$temp[,1,] 
# matrix of bottom temperature (time, location); 48451, 432
bot_temp <- obs_78_13$temp[,2,]
# matrix of surface salinity (time, location); 48451, 432
surf_sal <- obs_78_13$salinity[,1,]
# matrix of surface salinity (time, location); 48451, 432
bot_sal <- obs_78_13$salinity[,2,]
lon <- obs_78_13$lon
lat <- obs_78_13$lat


# reformat so each row is an observation of lat, long, mon, year, surf temp value # 20930832
lat_vec <- rep(lat, 432)
lon_vec <- rep(lon, 432)
mon <- rep(1:12, each = 48451)
mon_vec <- rep(mon, times = 36)
yrs_vec <- rep(1978:2013, each = 581412)
surf_temp_vec <- as.vector(surf_temp) 
bot_temp_vec <- as.vector(bot_temp)
surf_sal_vec <- as.vector(surf_sal)
bot_sal_vec <- as.vector(bot_sal)

observ <- cbind(lat_vec, lon_vec, mon_vec, yrs_vec, surf_temp_vec, bot_temp_vec, surf_sal_vec, bot_sal_vec)
observ_early <- as.data.frame(observ)
library(sp)
coordinates(observ_early) <- c("lat_vec", "lon_vec")



# ME-NH trawl fall and spring 
# subset lat, long, mon, yr to search FVCOM for (need to reduce to 2013)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
me_inds <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
#me_fall_inds <- subset(me_inds, SEASON == "FL") #fall
me_fall_inds <- subset(me_inds, SEASON == "SP") #spring
library(tidyr)
me_fall_inds_2 <- separate(me_fall_inds, EFFORT_START_DATE, into = c("month", "day", "year"), sep="/")
me_fall_inds_2 <- me_fall_inds_2[me_fall_inds_2$year < 2014,]

station <- subset(me_fall_inds_2, select = c(START_LATITUDE, START_LONGITUDE, month, year))
station <- as.data.frame(station)
coordinates(station) <- c("START_LATITUDE", "START_LONGITUDE")

# GOM fall and spring 
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/GOM")
#GOM_inds <- read.csv("GOM_fall.csv")
GOM_inds <- read.csv("GOM_spring.csv")
GOM_inds <- GOM_inds[GOM_inds$EST_YEAR < 2014,]
station <- subset(GOM_inds, select = c(DECDEG_BEGLAT, DECDEG_BEGLON, EST_MONTH, EST_YEAR))
station <- as.data.frame(station)
coordinates(station) <- c("DECDEG_BEGLAT", "DECDEG_BEGLON")


# MDMF fall and spring
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/MDMF")
#ma_inds <- read.csv("MDMF_fall_div_ind_by_tow.csv", header = TRUE)
ma_inds <- read.csv("MDMF_spring_div_ind_by_tow.csv", header = TRUE)
ma_inds <- ma_inds[ma_inds$YEAR < 2014,]
station <- subset(ma_inds, select = c(LATITUDE, LONGITUDE, MONTH, YEAR))
station <- as.data.frame(station)
coordinates(station) <- c("LATITUDE", "LONGITUDE")


######################early ####
# search FVCOM observ to find closest value to trawl station lat, long, month, year
#yrs <- seq(2000, 2013, by = 1) # ME-NH fall
#yrs <- seq(2001, 2013, by = 1) # ME-NH spring
yrs <- seq(1978, 2013, by = 1) # GOM fall and MDMF

minDist <- NULL
closestSite <- NULL
total_minDist <- matrix(NA, nrow = length(yrs), ncol = 280)
total_closestSite <- matrix(NA, nrow = length(yrs), ncol = 280)

for (j in 1:length(yrs)){
  #year_sub <- station[which(station$year == yrs[j]),] # ME-NH
  #year_sub <- station[which(station$EST_YEAR == yrs[j]),] # GOM
  year_sub <- station[which(station$YEAR == yrs[j]),] # MA
  minDist <- NULL
  closestSite <- NULL
for (i in 1:nrow(year_sub)){
  # find the closest coordinates within a given month and year
  #distVec <- spDistsN1(observ_early[which(observ_early$yrs_vec == yrs[j] & observ_early$mon_vec == year_sub$month[i]),], year_sub[i,], longlat = TRUE)  # ME-NH
  #distVec <- spDistsN1(observ_early[which(observ_early$yrs_vec == yrs[j] & observ_early$mon_vec == year_sub$EST_MONTH[i]),], year_sub[i,], longlat = TRUE)  # GOM
  distVec <- spDistsN1(observ_early[which(observ_early$yrs_vec == yrs[j] & observ_early$mon_vec == year_sub$MONTH[i]),], year_sub[i,], longlat = TRUE)  # MA
  minDist[i] <- min(distVec) 
  closestSite[i] <- which.min(distVec)
}
total_minDist[j,] <- c(minDist, rep(NA, 280 - length(minDist))) # each year is a row (column = each tow)
total_closestSite[j,] <- c(closestSite, rep(NA, 280 - length(closestSite))) 
}
#write.csv(total_minDist, "total_minDist.csv")
#write.csv(total_closestSite, "total_closestSite.csv")
#write.csv(total_minDist, "total_minDist_SP.csv")
#write.csv(total_closestSite, "total_closestSite_SP.csv")
#write.csv(total_minDist, "total_minDist_FL_GOM.csv")
#write.csv(total_closestSite, "total_closestSite_FL_GOM.csv")
#write.csv(total_minDist, "total_minDist_SP_GOM.csv")
#write.csv(total_closestSite, "total_closestSite_SP_GOM.csv")
#write.csv(total_minDist, "total_minDist_FL_MA.csv")
#write.csv(total_closestSite, "total_closestSite_FL_MA.csv")
write.csv(total_minDist, "total_minDist_SP_MA.csv")
write.csv(total_closestSite, "total_closestSite_SP_MA.csv")



#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
#total_minDist <- read.csv("total_minDist.csv")
#total_closestSite <- read.csv("total_closestSite.csv")

# create long vector and then remove NAs to get tows/years in order (assign FVCOM value to tows)
flip_closestSite <- t(total_closestSite)
closestSite_vec <- as.vector(flip_closestSite)
closestSite_vec <- closestSite_vec[!is.na(closestSite_vec)]

flip_minDist <- t(total_minDist)
minDist_vec <- as.vector(flip_minDist)
minDist_vec <- minDist_vec[!is.na(minDist_vec)]

# extract temperature information by tow # AEW these values were wrong
#surf_temp <- as(observ_early[closestSite_vec,]$surf_temp_vec, "numeric")
#final <- data.frame(coordinates(station), closestSite_vec, minDist_vec, surf_temp)
#write.csv(final, "ME_NH_FL_00_13_ST.csv")
ME_NH_FVCOM_surf_temp <- read.csv("ME_NH_FL_00_13_ST.csv")
# add month and year 
ME_NH_ST <- cbind(me_fall_inds_2$month, me_fall_inds_2$year, ME_NH_FVCOM_surf_temp)

# spring
#final <- data.frame(coordinates(station), station$month, station$year, closestSite_vec, minDist_vec)

# GOM
station <- station[station$EST_YEAR > 1977,]
final <- data.frame(coordinates(station), station$EST_MONTH, station$EST_YEAR, closestSite_vec, minDist_vec)

#MA
final <- data.frame(coordinates(station), station$MONTH, station$YEAR, closestSite_vec, minDist_vec)


# AEW not grabbing correct information (using whole data set not just subyear)
#j <- 1
#i <- 3
#FV_sub <- observ_early[which(observ_early$yrs_vec == yrs[j] & observ_early$mon_vec == year_sub$month[i]),]
#station_sub <- year_sub[i,]
# 35686 site for this tow
#test_temp <- as(FV_sub[35686,]$surf_temp_vec, "numeric")
#lrg <- observ_early[36139,]
#pos <- FV_sub[36139,]

### find FVCOM surface temperature associated with FVCOM location 
total_ST <- matrix(NA, nrow = length(yrs), ncol = 280)
total_BT <- matrix(NA, nrow = length(yrs), ncol = 280)
total_SS <- matrix(NA, nrow = length(yrs), ncol = 280)
total_BS <- matrix(NA, nrow = length(yrs), ncol = 280)
for (j in 1:length(yrs)){
  sub_FVCOM <- observ_early[which(observ_early$yrs_vec == yrs[j]),]
  #sub_ME_NH <- ME_NH_ST[which(ME_NH_ST$`me_fall_inds_2$year`== yrs[j]),]
  #sub_ME_NH <- final[which(final$station.year == yrs[j]),]
  #sub_ME_NH <- final[which(final$station.EST_YEAR == yrs[j]),]
  sub_ME_NH <- final[which(final$station.YEAR == yrs[j]),]
  ST <- NULL
  BT <- NULL
  SS <- NULL
  BS <- NULL
  for (i in 1:nrow(sub_ME_NH)){
    # find the closest coordinates within a given month and year
    #position <- sub_FVCOM[which(sub_FVCOM$mon_vec == sub_ME_NH$`me_fall_inds_2$month`[i]),] 
    #position <- sub_FVCOM[which(sub_FVCOM$mon_vec == sub_ME_NH$station.month[i]),]  
    #position <- sub_FVCOM[which(sub_FVCOM$mon_vec == sub_ME_NH$station.EST_MONTH[i]),]  
    position <- sub_FVCOM[which(sub_FVCOM$mon_vec == sub_ME_NH$station.MONTH[i]),]  
    ST[i] <- as(position[sub_ME_NH$closestSite_vec[i],]$surf_temp_vec, "numeric")
    BT[i] <- as(position[sub_ME_NH$closestSite_vec[i],]$bot_temp_vec, "numeric")
    SS[i] <- as(position[sub_ME_NH$closestSite_vec[i],]$surf_sal_vec, "numeric")
    BS[i] <- as(position[sub_ME_NH$closestSite_vec[i],]$bot_sal_vec, "numeric")
  }
  total_ST[j,] <- c(ST, rep(NA, 280 - length(ST))) # each year is a row (column = each tow)
  total_BT[j,] <- c(BT, rep(NA, 280 - length(BT)))
  total_SS[j,] <- c(SS, rep(NA, 280 - length(SS)))
  total_BS[j,] <- c(BS, rep(NA, 280 - length(BS)))
}

flip_total_ST <- t(total_ST)
ST_vec <- as.vector(flip_total_ST)
ST_vec <- ST_vec[!is.na(ST_vec)]

flip_total_BT <- t(total_BT)
BT_vec <- as.vector(flip_total_BT)
BT_vec <- BT_vec[!is.na(BT_vec)]

flip_total_SS <- t(total_SS)
SS_vec <- as.vector(flip_total_SS)
SS_vec <- SS_vec[!is.na(SS_vec)]

flip_total_BS <- t(total_BS)
BS_vec <- as.vector(flip_total_BS)
BS_vec <- BS_vec[!is.na(BS_vec)]

# add surface temperature to tow by tow information 
ME_NH_FVCOM_surf_temp <- read.csv("ME_NH_FL_00_13_ST.csv")
ME_NH_ST <- ME_NH_ST[,-8]
ME_NH_ST_02 <- cbind(ME_NH_ST, ST_vec, BT_vec, SS_vec, BS_vec)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
write.csv(ME_NH_ST_02, "ME_NH_FL_00_13_ST_BT_SS_BS.csv")

#spring
spring_early <- cbind(final, ST_vec, BT_vec, SS_vec, BS_vec)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
write.csv(spring_early, "ME_NH_SP_01_13_ST_BT_SS_BS.csv")

spring_early <- cbind(final, ST_vec, BT_vec, SS_vec, BS_vec)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
write.csv(spring_early, "GOM_FL_78_13_ST_BT_SS_BS.csv")
write.csv(spring_early, "GOM_SP_78_13_ST_BT_SS_BS.csv")


# MA
spring_early <- cbind(final, ST_vec, BT_vec, SS_vec, BS_vec)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
write.csv(spring_early, "MA_FL_78_13_ST_BT_SS_BS.csv")
write.csv(spring_early, "MA_SP_78_13_ST_BT_SS_BS.csv")





############################## observations for years 2014-2016; same grid as previous years ##################################
#proj.NAD83<- CRS("+init=epsg:3557")
# try ESRI:102684
#proj4string(observ_early)<- proj.wgs84
#proj4string(observ)<- proj.NAD83
#observ_decdeg <- spTransform(observ, proj.wgs84)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/FVCOM_sur_bot_tempsal")
library(RNetCDF)
sal_temp_mid <- 'GoM_surbot_tempsal_2014-2016.nc'
mid.sal.temp <- open.nc(sal_temp_mid)
print.nc(mid.sal.temp)
obs_14_16 <- read.nc(mid.sal.temp)
close.nc(mid.sal.temp)
#:CoordinateProjection = "init=nad83:1802" ;
# http://www.spatialreference.org/ref/esri/102284/

time <- obs_14_16$Times
# matrix of surface temperature (time, location); 48451, 432
surf_temp <- obs_14_16$temp[,1,] 
# matrix of bottom temperature (time, location); 48451, 432
bot_temp <- obs_14_16$temp[,2,]
# matrix of surface salinity (time, location); 48451, 432
surf_sal <- obs_14_16$salinity[,1,]
# matrix of surface salinity (time, location); 48451, 432
bot_sal <- obs_14_16$salinity[,2,]

# use same mesh as 1978-2013
lon <- obs_78_13$lon
lat <- obs_78_13$lat
#lat <- obs_14_16$y
#lon <- obs_14_16$x

## 2014-2016
# reformat so each row is an observation of lat, long, mon, year, surf temp, bot temp, surf sal, bot sal  # 1744236
# same spatial coverage as 2000-2013 
lat_vec <- rep(lat, 36)
lon_vec <- rep(lon, 36)
mon <- rep(1:12, each = 48451)
mon_vec <- rep(mon, times = 3)
yrs_vec <- rep(2014:2016, each = 581412)
surf_temp_vec <- as.vector(surf_temp) 
bot_temp_vec <- as.vector(bot_temp)
surf_sal_vec <- as.vector(surf_sal)
bot_sal_vec <- as.vector(bot_sal)

observ <- cbind(lat_vec, lon_vec, mon_vec, yrs_vec, surf_temp_vec, bot_temp_vec, surf_sal_vec, bot_sal_vec)
observ <- as.data.frame(observ)
library(sp)
coordinates(observ) <- c("lat_vec", "lon_vec")


# ME-NH trawl fall and spring
# subset lat, long, mon, yr to search FVCOM for (2014-2016)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
me_inds <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
#me_fall_inds <- subset(me_inds, SEASON == "FL") # fall
me_fall_inds <- subset(me_inds, SEASON == "SP") # spring

library(tidyr)
me_fall_inds_2 <- separate(me_fall_inds, EFFORT_START_DATE, into = c("month", "day", "year"), sep="/")
me_fall_inds_2 <- me_fall_inds_2[me_fall_inds_2$year > 2013,]
me_fall_inds_2 <- me_fall_inds_2[me_fall_inds_2$year <2017,]
station <- subset(me_fall_inds_2, select = c(START_LATITUDE, START_LONGITUDE, month, year))
station <- as.data.frame(station)
coordinates(station) <- c("START_LATITUDE", "START_LONGITUDE")


#GOM fall and spring 
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/GOM")
#GOM_inds <- read.csv("GOM_fall.csv")
GOM_inds <- read.csv("GOM_spring.csv")
GOM_inds <- GOM_inds[GOM_inds$EST_YEAR > 2013,]
GOM_inds <- GOM_inds[GOM_inds$EST_YEAR < 2017,]
station <- subset(GOM_inds, select = c(DECDEG_BEGLAT, DECDEG_BEGLON, EST_MONTH, EST_YEAR))
station <- as.data.frame(station)
coordinates(station) <- c("DECDEG_BEGLAT", "DECDEG_BEGLON")

# MDMF fall and spring
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/MDMF")
#ma_inds <- read.csv("MDMF_fall_div_ind_by_tow.csv", header = TRUE)
ma_inds <- read.csv("MDMF_spring_div_ind_by_tow.csv", header = TRUE)
ma_inds <- ma_inds[ma_inds$YEAR > 2013,]
ma_inds <- ma_inds[ma_inds$YEAR < 2017,]
station <- subset(ma_inds, select = c(LATITUDE, LONGITUDE, MONTH, YEAR))
station <- as.data.frame(station)
coordinates(station) <- c("LATITUDE", "LONGITUDE")


# search FVCOM observ to find closest value to trawl station lat, long, month, year
yrs <- seq(2014, 2016, by = 1)
minDist <- NULL
closestSite <- NULL
total_minDist <- matrix(NA, nrow = length(yrs), ncol = 150)
total_closestSite <- matrix(NA, nrow = length(yrs), ncol = 150)

for (j in 1:length(yrs)){
  #ME-NH:
  #year_sub <- station[which(station$year == yrs[j]),]
  #GOM:
  #year_sub <- station[which(station$EST_YEAR == yrs[j]),]
  #MA: 
  year_sub <- station[which(station$YEAR == yrs[j]),]
  
  minDist <- NULL
  closestSite <- NULL
  for (i in 1:nrow(year_sub)){
    # find the closest coordinates within a given month and year
    #ME-NH:
    #distVec <- spDistsN1(observ[which(observ$yrs_vec == yrs[j] & observ$mon_vec == year_sub$month[i]),], year_sub[i,], longlat = TRUE)  
    #GOM:
    #distVec <- spDistsN1(observ[which(observ$yrs_vec == yrs[j] & observ$mon_vec == year_sub$EST_MONTH[i]),], year_sub[i,], longlat = TRUE)  
    #MA:
    distVec <- spDistsN1(observ[which(observ$yrs_vec == yrs[j] & observ$mon_vec == year_sub$MONTH[i]),], year_sub[i,], longlat = TRUE)  
    minDist[i] <- min(distVec) 
    closestSite[i] <- which.min(distVec)
  }
  total_minDist[j,] <- c(minDist, rep(NA, 150 - length(minDist))) # each year is a row (column = each tow)
  total_closestSite[j,] <- c(closestSite, rep(NA, 150 - length(closestSite))) 
}
#write.csv(total_minDist, "total_minDist_14_16.csv") 
#write.csv(total_closestSite, "total_closestSite_14_16.csv")
#write.csv(total_minDist, "total_minDist_14_16_SP.csv") 
#write.csv(total_closestSite, "total_closestSite_14_16_SP.csv")
#write.csv(total_minDist, "total_minDist_14_16_SP_GOM.csv") 
#write.csv(total_closestSite, "total_closestSite_14_16_SP_GOM.csv")
#write.csv(total_minDist, "total_minDist_14_16_SP_GOM.csv") 
#write.csv(total_closestSite, "total_closestSite_14_16_SP_GOM.csv")
#write.csv(total_minDist, "total_minDist_14_16_FL_MA.csv") 
#write.csv(total_closestSite, "total_closestSite_14_16_FL_MA.csv")
write.csv(total_minDist, "total_minDist_14_16_SP_MA.csv") 
write.csv(total_closestSite, "total_closestSite_14_16_SP_MA.csv")



## create long vector and then remove NAs to get tows/years in order (assign FVCOM value to tows)
flip_closestSite <- t(total_closestSite)
closestSite_vec <- as.vector(flip_closestSite)
closestSite_vec <- closestSite_vec[!is.na(closestSite_vec)]

flip_minDist <- t(total_minDist)
minDist_vec <- as.vector(flip_minDist)
minDist_vec <- minDist_vec[!is.na(minDist_vec)]

#final <- data.frame(coordinates(station), station$month, station$year, closestSite_vec, minDist_vec) #ME-NH
#final <- data.frame(coordinates(station), station$EST_MONTH, station$EST_YEAR, closestSite_vec, minDist_vec) #GOM
final <- data.frame(coordinates(station), station$MONTH, station$YEAR, closestSite_vec, minDist_vec) #GOM



### find FVCOM surface temperature associated with FVCOM location 
total_ST <- matrix(NA, nrow = length(yrs), ncol = 150)
total_BT <- matrix(NA, nrow = length(yrs), ncol = 150)
total_SS <- matrix(NA, nrow = length(yrs), ncol = 150)
total_BS <- matrix(NA, nrow = length(yrs), ncol = 150)
for (j in 1:length(yrs)){
  sub_FVCOM <- observ[which(observ$yrs_vec == yrs[j]),]
  #sub_ME_NH <- final[which(final$station.year == yrs[j]),] # ME-NH
  #sub_ME_NH <- final[which(final$station.EST_YEAR == yrs[j]),] #GOM
  sub_ME_NH <- final[which(final$station.YEAR == yrs[j]),] #MA
  ST <- NULL
  BT <- NULL
  SS <- NULL
  BS <- NULL
  for (i in 1:nrow(sub_ME_NH)){
    # find the closest coordinates within a given month and year
    #position <- sub_FVCOM[which(sub_FVCOM$mon_vec == sub_ME_NH$station.month[i]),] 
    #position <- sub_FVCOM[which(sub_FVCOM$mon_vec == sub_ME_NH$station.EST_MONTH[i]),] 
    position <- sub_FVCOM[which(sub_FVCOM$mon_vec == sub_ME_NH$station.MONTH[i]),]  
    ST[i] <- as(position[sub_ME_NH$closestSite_vec[i],]$surf_temp_vec, "numeric")
    BT[i] <- as(position[sub_ME_NH$closestSite_vec[i],]$bot_temp_vec, "numeric")
    SS[i] <- as(position[sub_ME_NH$closestSite_vec[i],]$surf_sal_vec, "numeric")
    BS[i] <- as(position[sub_ME_NH$closestSite_vec[i],]$bot_sal_vec, "numeric")
  }
  total_ST[j,] <- c(ST, rep(NA, 150 - length(ST))) # each year is a row (column = each tow)
  total_BT[j,] <- c(BT, rep(NA, 150 - length(BT)))
  total_SS[j,] <- c(SS, rep(NA, 150 - length(SS)))
  total_BS[j,] <- c(BS, rep(NA, 150 - length(BS)))
}

flip_total_ST <- t(total_ST)
ST_vec <- as.vector(flip_total_ST)
ST_vec <- ST_vec[!is.na(ST_vec)]

flip_total_BT <- t(total_BT)
BT_vec <- as.vector(flip_total_BT)
BT_vec <- BT_vec[!is.na(BT_vec)]

flip_total_SS <- t(total_SS)
SS_vec <- as.vector(flip_total_SS)
SS_vec <- SS_vec[!is.na(SS_vec)]

flip_total_BS <- t(total_BS)
BS_vec <- as.vector(flip_total_BS)
BS_vec <- BS_vec[!is.na(BS_vec)]

# add surface temperature to tow by tow information 
ME_NH_ST_02 <- cbind(final, ST_vec, BT_vec, SS_vec, BS_vec)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
#write.csv(ME_NH_ST_02, "ME_NH_FL_14_16_ST_BT_SS_BS.csv")
#write.csv(ME_NH_ST_02, "ME_NH_SP_14_16_ST_BT_SS_BS.csv")
#write.csv(ME_NH_ST_02, "GOM_FL_14_16_ST_BT_SS_BS.csv")
#write.csv(ME_NH_ST_02, "GOM_SP_14_16_ST_BT_SS_BS.csv")
#write.csv(ME_NH_ST_02, "MA_FL_14_16_ST_BT_SS_BS.csv")
write.csv(ME_NH_ST_02, "MA_SP_14_16_ST_BT_SS_BS.csv")




################################ data for 2017-2018 April is on a different grid than the previous data ##########################
library(RNetCDF)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/FVCOM_sur_bot_tempsal")
sal_temp_end <- 'GoM4_surbot_tempsal_2017-2018Apr.nc'
end.sal.temp <- open.nc(sal_temp_end)
print.nc(end.sal.temp)
obs_17_18 <- read.nc(end.sal.temp)
close.nc(end.sal.temp)

setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/FVCOM_sur_bot_tempsal")
Gom4 <- 'gom4_grid_full.nc'
grid <- open.nc(Gom4)
print.nc(grid)
grid_17_18 <- read.nc(grid)
close.nc(grid)
lat <- grid_17_18$lat
lon <- grid_17_18$lon

# cut off months in 2018 (only 4)
time <- obs_17_18$Times
time <- time[1:12]
# matrix of surface temperature (time, location); 53087, 16
surf_temp <- obs_17_18$temp[,1,]
surf_temp <- surf_temp[,1:12]
# matrix of bottom temperature
bot_temp <- obs_17_18$temp[,2,]
bot_temp <- bot_temp[,1:12]
# matrix of surface salinity 
surf_sal <- obs_17_18$salinity[,1,]
surf_sal <- surf_sal[,1:12]
# matrix of surface salinity 
bot_sal <- obs_17_18$salinity[,2,]
bot_sal <- bot_sal[,1:12]


# reformat so each row is an observation of lat, long, mon, year, surf temp value 
lat_vec <- rep(lat, 12)
lon_vec <- rep(lon, 12)
mon <- rep(1:12, each = 53087)
mon_vec <- rep(mon, times = 1)
yrs_vec <- rep(2017, 637044)
surf_temp_vec <- as.vector(surf_temp) 
bot_temp_vec <- as.vector(bot_temp) 
surf_sal_vec <- as.vector(surf_sal)
bot_sal_vec <- as.vector(bot_sal)



observ_late <- cbind(lat_vec, lon_vec, mon_vec, yrs_vec, surf_temp_vec, bot_temp_vec, surf_sal_vec, bot_sal_vec)
observ_late <- as.data.frame(observ_late)
library(sp)
coordinates(observ_late) <- c("lat_vec", "lon_vec")



### ME-NH goes to fall/spring 2017
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
me_inds <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
#me_fall_inds <- subset(me_inds, SEASON == "FL") # fall
me_fall_inds <- subset(me_inds, SEASON == "SP") # spring

library(tidyr)
me_fall_inds_2 <- separate(me_fall_inds, EFFORT_START_DATE, into = c("month", "day", "year"), sep="/")
me_fall_inds_2 <- me_fall_inds_2[me_fall_inds_2$year == 2017,]

station <- subset(me_fall_inds_2, select = c(START_LATITUDE, START_LONGITUDE, month, year))
station <- as.data.frame(station)
coordinates(station) <- c("START_LATITUDE", "START_LONGITUDE")



### GOM fall/spring 
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/GOM")
#GOM_inds <- read.csv("GOM_fall.csv")
#GOM_inds <- GOM_inds[GOM_inds$EST_YEAR == 2017,]
GOM_inds <- read.csv("GOM_spring.csv")
GOM_inds <- GOM_inds[GOM_inds$EST_YEAR == 2017,]

station <- subset(GOM_inds, select = c(DECDEG_BEGLAT, DECDEG_BEGLON, EST_MONTH, EST_YEAR))
station <- as.data.frame(station)
coordinates(station) <- c("DECDEG_BEGLAT", "DECDEG_BEGLON")




# MDMF fall and spring
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/MDMF")
#ma_fall <- read.csv("MDMF_fall_div_ind_by_tow.csv", header = TRUE)
ma_inds <- read.csv("MDMF_spring_div_ind_by_tow.csv", header = TRUE)
ma_inds <- ma_inds[ma_inds$YEAR == 2017,]
station <- subset(ma_inds, select = c(LATITUDE, LONGITUDE, MONTH, YEAR))
station <- as.data.frame(station)
coordinates(station) <- c("LATITUDE", "LONGITUDE")


# search FVCOM observ to find closest value to trawl station lat, long, month, year
minDist <- NULL
closestSite <- NULL
for (i in 1:nrow(station)){
  # find the closest coordinates within a given month and year
  #ME-NH:
  #distVec <- spDistsN1(observ_late[which(observ_late$mon_vec == station$month[i]),], station[i,], longlat = TRUE) 
  #GOM:
  #distVec <- spDistsN1(observ_late[which(observ_late$mon_vec == station$EST_MONTH[i]),], station[i,], longlat = TRUE)  
  #MA:
  distVec <- spDistsN1(observ_late[which(observ_late$mon_vec == station$MONTH[i]),], station[i,], longlat = TRUE)  
  minDist[i] <- min(distVec) 
  closestSite[i] <- which.min(distVec)
}

# ME-NH:
#full <- data.frame(coordinates(station), station$month, station$year, closestSite, minDist)
# GOM:
#full <- data.frame(coordinates(station), station$EST_MONTH, station$EST_YEAR, closestSite, minDist)
# MA:
full <- data.frame(coordinates(station), station$MONTH, station$YEAR, closestSite, minDist)


# subset observ_late by month to get correct closestSite
  ST <- NULL
  BT <- NULL
  SS <- NULL
  BS <- NULL
  for (i in 1:nrow(full)){
    # find the closest coordinates within a given month and year
    position <- observ_late[which(observ_late$mon_vec == full$station.MONTH[i]),]  
    ST[i] <- as(position[full$closestSite[i],]$surf_temp_vec, "numeric")
    BT[i] <- as(position[full$closestSite[i],]$bot_temp_vec, "numeric")
    SS[i] <- as(position[full$closestSite[i],]$surf_sal_vec, "numeric")
    BS[i] <- as(position[full$closestSite[i],]$bot_sal_vec, "numeric")
  }

final <- data.frame(full, ST, BT, SS, BS)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
#write.csv(final, "ME_NH_FL_17_ST_BT_SS_BS.csv")
#write.csv(final, "ME_NH_SP_17_ST_BT_SS_BS.csv")
#write.csv(final, "GOM_FL_17_ST_BT_SS_BS.csv")
#write.csv(final, "GOM_SP_17_ST_BT_SS_BS.csv")
#write.csv(final, "MA_FL_17_ST_BT_SS_BS.csv")
write.csv(final, "MA_SP_17_ST_BT_SS_BS.csv")








##### AEW need to rbind all years and then tack onto the end of tow by tow 
# fall ME-NH
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
beg <- read.csv("ME_NH_FL_00_13_ST_BT_SS_BS.csv")
beg_2 <- subset(beg, select = c(START_LATITUDE, START_LONGITUDE, me_fall_inds_2.month, me_fall_inds_2.year, closestSite_vec, minDist_vec, ST_vec, BT_vec, SS_vec, BS_vec))
colnames(beg_2) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")
mid <- read.csv("ME_NH_FL_14_16_ST_BT_SS_BS.csv")
mid <- mid[,-1]
colnames(mid) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")
end <- read.csv("ME_NH_FL_17_ST_BT_SS_BS.csv")
end <- end[,-1]
colnames(end) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")

FVCOM_by_tow <- rbind(beg_2, mid, end)
write.csv(FVCOM_by_tow, "FVCOM_ME_NH_fall.csv")


# spring ME-NH
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
beg <- read.csv("ME_NH_SP_01_13_ST_BT_SS_BS.csv")
beg <- beg[,-1]
colnames(beg) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")
mid <- read.csv("ME_NH_SP_14_16_ST_BT_SS_BS.csv")
mid <- mid[,-1]
colnames(mid) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")
end <- read.csv("ME_NH_SP_17_ST_BT_SS_BS.csv")
end <- end[,-1]
colnames(end) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")

FVCOM_by_tow <- rbind(beg, mid, end)
write.csv(FVCOM_by_tow, "FVCOM_ME_NH_spring.csv")

## fall GOM
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
beg <- read.csv("GOM_FL_78_13_ST_BT_SS_BS.csv")
beg <- beg[,-1]
colnames(beg) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")
mid <- read.csv("GOM_FL_14_16_ST_BT_SS_BS.csv")
mid <- mid[,-1]
colnames(mid) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")
end <- read.csv("GOM_FL_17_ST_BT_SS_BS.csv")
end <- end[,-1]
colnames(end) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")

FVCOM_by_tow <- rbind(beg, mid, end)
write.csv(FVCOM_by_tow, "FVCOM_GOM_fall.csv")

## spring GOM
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
beg <- read.csv("GOM_SP_78_13_ST_BT_SS_BS.csv")
beg <- beg[,-1]
colnames(beg) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")
mid <- read.csv("GOM_SP_14_16_ST_BT_SS_BS.csv")
mid <- mid[,-1]
colnames(mid) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")
end <- read.csv("GOM_SP_17_ST_BT_SS_BS.csv")
end <- end[,-1]
colnames(end) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")

FVCOM_by_tow <- rbind(beg, mid, end)
write.csv(FVCOM_by_tow, "FVCOM_GOM_spring.csv")


## fall MA
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
beg <- read.csv("MA_FL_78_13_ST_BT_SS_BS.csv")
beg <- beg[,-1]
colnames(beg) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")
mid <- read.csv("MA_FL_14_16_ST_BT_SS_BS.csv")
mid <- mid[,-1]
colnames(mid) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")
end <- read.csv("MA_FL_17_ST_BT_SS_BS.csv")
end <- end[,-1]
colnames(end) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")

FVCOM_by_tow <- rbind(beg, mid, end)
write.csv(FVCOM_by_tow, "FVCOM_MA_fall.csv")

## fall MA
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
beg <- read.csv("MA_SP_78_13_ST_BT_SS_BS.csv")
beg <- beg[,-1]
colnames(beg) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")
mid <- read.csv("MA_SP_14_16_ST_BT_SS_BS.csv")
mid <- mid[,-1]
colnames(mid) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")
end <- read.csv("MA_SP_17_ST_BT_SS_BS.csv")
end <- end[,-1]
colnames(end) <- c("FV_start_lat", "FV_start_lon", "FV_month", "FV_year", "FV_closest_site", "FV_min_dist", "FV_surf_temp", "FV_bot_temp", "FV_surf_sal", "FV_bot_sal")

FVCOM_by_tow <- rbind(beg, mid, end)
write.csv(FVCOM_by_tow, "FVCOM_MA_spring.csv")






################################################################ NEFSC trawl
setwd('C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/NEFSC')
FL <- read.csv('nefsc_FL_by_tow.csv')
date <- cbind(FL$EST_MONTH, FL$EST_YEAR) #month/year of tow
stations <- cbind(FL$DECDEG_BEGLAT, FL$DECDEG_BEGLON, date) #lat, long, mon, yr information
# find FVCOM location closest to trawl station lat, long, month, year


################### visualizing spatial coverage 
library('ggplot2')
library('ggthemes')
library('ggmap') 
library('maps')  # lots of geographic outlines
library('mapdata') 
library('rgdal')
library('ggplot2')
require('gridExtra')
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
usa <- map_data("usa")
ggplot() + 
  geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-76, -56),  ylim = c(35.1, 46.5), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2),plot.margin=unit(c(1,1,-0.5,1), "cm")) +
  geom_point(aes(x = obs_78_13$lon, y = obs_78_13$lat)) + 
  labs(x = "Longitude", y = "Latitude", title = "FVCOM spatial coverage 1978-2013")

latitude <- obs_78_13$lat
longitude <- obs_78_13$lon
plot(latitude ~ longitude, pch = 16, main = "FVCOM spatial coverage 1978-2013")

lat <- obs_14_16$x
lon <- obs_14_16$y
plot(lat, lon, pch = 16, main = "FVCOM spatial coverage 2014-2016")

lats <- obs_17_18$x
longs <- obs_17_18$y
plot(lats, longs, pch = 16, main = "FVCOM spatial coverage 2017-2018")
