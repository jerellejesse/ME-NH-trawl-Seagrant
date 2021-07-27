####################################################################################
### Exploring  environmental metrics
### UM Seagrant; climate impacts on Maine coastal fisheries using the ME-NH inshore trawl
### Objective 1 part 1
### 7/27/2018
### A.E. Weston
####################################################################################

#different number of hauls for spring and fall seasons 

setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/Results")
inds <- read.csv("diversity_ind_by_haul.csv", header = TRUE)

fall <- inds[inds$SEASON == "FL",]
spring <- inds[inds$SEASON == "SP",]

plot(fall$WATER_TEMP_C, type = 'l', ylim = c(3,14))
lines(spring$WATER_TEMP_C, type = 'l', col = 'blue')

plot(fall$SURFACE_TEMP_C, type = 'l', ylim = c(3,17))
lines(spring$SURFACE_TEMP_C, type = 'l', col = 'blue')


plot(fall$SALINITY, type = 'l', ylim = c(26,37))
lines(spring$SALINITY, type = 'l', col = 'blue')

plot(fall$SURFACE_SALINITY, type = 'l', ylim = c(0,36))
lines(spring$SURFACE_SALINITY, type = 'l', col = 'blue')






################ aggregating average env information season for each year 
## Note I removed NAs 

setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/Results")
ind_by_haul <- read.csv("diversity_ind_by_haul.csv", header = TRUE)

#fall
fall_inds <- subset(ind_by_haul, SEASON == "FL")

diff_year <- matrix(NA) 
N_sample <- NULL
start <- 00
end <- 17
yr <- seq(0, 17, by = 1)
temp <- NULL
SST <- NULL
sal <- NULL
SSS <- NULL


for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year <- fall_inds[which(fall_inds$YEAR == yr[i]),] #subset unique hauls
temp[i] <- mean(na.omit(diff_year$WATER_TEMP_C))
SST[i] <- mean(na.omit(diff_year$SURFACE_TEMP_C))
sal[i] <- mean(na.omit(diff_year$SALINITY))
SSS[i] <- mean(na.omit(diff_year$SURFACE_SALINITY))
}

label <- seq(2000, 2017)
label <- as.character(label)
plot(temp, type = 'l', xlab = 'Year', xaxt = 'n', ylab = "Temperature (C)", main = "Average Fall Bottom Temperature", lwd = 2, cex.lab = 1.25)
axis(1, at = c(1:18),labels = label, las = 2)

plot(SST, type = 'l', xlab = 'Year', xaxt = 'n', ylab = "Temperature (C)", main = "Average Fall Sea Surface Temperature", lwd = 2, cex.lab = 1.25)
axis(1, at = c(1:18),labels = label, las = 2)

plot(sal, type = 'l', xlab = 'Year', xaxt = 'n', ylab = "Salinity (psu)", main = "Average Fall Bottom Salinity", lwd = 2, cex.lab = 1.25)
axis(1, at = c(1:18),labels = label, las = 2)

plot(SSS, type = 'l', xlab = 'Year', xaxt = 'n', ylab = "Salinity (psu)", main = "Average Fall Sea Surface Salinity", lwd = 2, cex.lab = 1.25)
axis(1, at = c(1:18),labels = label, las = 2)


# Fall FVCOM average values for SNEC pres ##
setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/Results/GAMMs")
fall <- read.csv("ME_NH_fall_full.csv")

diff_year <- matrix(NA) 
yr <- seq(0, 16, by = 1)
temp <- NULL
SST <- NULL

for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year <- fall[which(fall$YEAR == (yr[i] +2000)),] #subset unique hauls
  temp[i] <- mean(na.omit(diff_year$FV_bot_temp))
  SST[i] <- mean(na.omit(diff_year$FV_surf_temp))

}

label <- seq(2000, 2017)
label <- as.character(label)
plot(temp, type = 'l', xlab = 'Year', xaxt = 'n', ylab = "Temperature (C)", main = "Average Fall FVCOM Bottom Temperature", lwd = 2, cex.lab = 1.25, ylim = c(8,12))
axis(1, at = c(1:18),labels = label, las = 2)

plot(SST, type = 'l', xlab = 'Year', xaxt = 'n', ylab = "Temperature (C)", main = "Average Fall FVCOM Sea Surface Temperature", lwd = 2, cex.lab = 1.25, ylim = c(9,15))
axis(1, at = c(1:18),labels = label, las = 2)



### spring 
spring_inds <- subset(ind_by_haul, SEASON == "SP")

diff_year <- matrix(NA) 
N_sample <- NULL
start <- 00
end <- 17
yr <- seq(0, 17, by = 1)
sp_temp <- NULL
sp_SST <- NULL
sp_sal <- NULL
sp_SSS <- NULL


for (i in 1:length(yr)) { #loop through each haul within year/season
  diff_year <- spring_inds[which(spring_inds$YEAR == yr[i]),] #subset unique hauls
  sp_temp[i] <- mean(na.omit(diff_year$WATER_TEMP_C))
  sp_SST[i] <- mean(na.omit(diff_year$SURFACE_TEMP_C))
  sp_sal[i] <- mean(na.omit(diff_year$SALINITY))
  sp_SSS[i] <- mean(na.omit(diff_year$SURFACE_SALINITY))
}

label <- seq(2000, 2017)
label <- as.character(label)
plot(sp_temp, type = 'l', xlab = 'Year', xaxt = 'n', ylab = "Temperature (C)", main = "Average Spring Bottom Temperature", lwd = 2, cex.lab = 1.25)
axis(1, at = c(1:18),labels = label, las = 2)

plot(sp_SST, type = 'l', xlab = 'Year', xaxt = 'n', ylab = "Temperature (C)", main = "Average Spring Sea Surface Temperature", lwd = 2, cex.lab = 1.25)
axis(1, at = c(1:18),labels = label, las = 2)

plot(sp_sal, type = 'l', xlab = 'Year', xaxt = 'n', ylab = "Salinity (psu)", main = "Average Spring Bottom Salinity", lwd = 2, cex.lab = 1.25)
axis(1, at = c(1:18),labels = label, las = 2)

plot(sp_SSS, type = 'l', xlab = 'Year', xaxt = 'n', ylab = "Salinity (psu)", main = "Average Spring Sea Surface Salinity", lwd = 2, cex.lab = 1.25)
axis(1, at = c(1:18),labels = label, las = 2)




