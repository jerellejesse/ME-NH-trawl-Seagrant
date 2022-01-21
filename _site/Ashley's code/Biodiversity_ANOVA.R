####################################################################################
### Exploring  haul by haul data 
### UM Seagrant; climate impacts on Maine coastal fisheries using the ME-NH inshore trawl
### Objective 1 part 1
### 7/27/2018
### A.E. Weston
####################################################################################

# < 0.05 = there is a significant difference 
# > 0.05 = no significant difference 
# bartlett; < 0.5 = sig diff
# shapiro; < 0.5 = not normally distributed

setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
inds <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
fall_inds <- subset(inds, SEASON == "FL")
spring_inds <- subset(inds, SEASON == "SP")

N_spp_fit_all <- aov(N_species ~ SEASON*YEAR*REGION*STRATUM, data = inds) #START_LATITUDE*START_LONGITUDE*START_DEPTH*WATER_TEMP_C*SALINITY
summary(test)
plot(test)
summary(N_spp_fit_all)
N_spp_fit_fall <- aov(N_species ~ YEAR*REGION*STRATUM, data = fall_inds) 
summary(N_spp_fit_fall)
N_spp_fit_spring <- aov(N_species ~ YEAR*REGION*STRATUM, data = spring_inds) 
summary(N_spp_fit_spring)
plot(N_spp_fit)
bartlett.test(N_species ~ SEASON, data = inds) #diff
bartlett.test(N_species ~ YEAR, data = inds) #diff
bartlett.test(N_species ~ REGION, data = inds) #diff
bartlett.test(N_species ~ STRATUM, data = inds) #diff
shapiro.test(fall_inds$N_species) # not normally distributed
hist(spring_inds$N_species)


H_ind_fit <- aov(H_index ~ SEASON*YEAR*REGION*STRATUM, data = inds)
summary(H_ind_fit)
H_ind_fit_fall <- aov(H_index ~ YEAR*REGION*STRATUM, data = fall_inds)
summary(H_ind_fit_fall)
H_ind_fit_spring <- aov(H_index ~ YEAR*REGION*STRATUM, data = spring_inds)
summary(H_ind_fit_spring)
plot(H_ind_fit)
shapiro.test(inds$H_index) # not normally distributed
hist(spring_inds$H_index)


D_ind_fit <- aov(D_index ~ SEASON*YEAR*REGION*STRATUM, data = inds)
summary(D_ind_fit)
D_ind_fit_fall <- aov(D_index ~ YEAR*REGION*STRATUM, data = fall_inds)
summary(D_ind_fit_fall)
D_ind_fit_spring <- aov(D_index ~ YEAR*REGION*STRATUM, data = spring_inds)
summary(D_ind_fit_spring)
plot(D_ind_fit)
shapiro.test(inds$D_index) #not normally distributed
hist(inds$D_index)


E_ind_fit <- aov(E_index ~ SEASON*YEAR*REGION*STRATUM, data = inds)
summary(E_ind_fit)
E_ind_fit_fall <- aov(E_index ~ YEAR*REGION*STRATUM, data = fall_inds)
summary(E_ind_fit_fall)
E_ind_fit_spring <- aov(E_index ~ YEAR*REGION*STRATUM, data = spring_inds)
summary(E_ind_fit_spring)
plot(E_ind_fit)
shapiro.test(inds$E_index) #not normally distributed
hist(inds$E_index)


delta_fit <- aov(delta ~ SEASON*YEAR*REGION*STRATUM, data = inds)
summary(delta_fit)
delta_fit_fall <- aov(delta ~ YEAR*REGION*STRATUM, data = fall_inds)
summary(delta_fit_fall)
delta_fit_spring <- aov(delta ~ YEAR*REGION*STRATUM, data = spring_inds)
summary(delta_fit_spring)
plot(delta_fit)
shapiro.test(inds$delta) #not normally distributed
hist(inds$delta)


delta_star_fit <- aov(delta_star ~ SEASON*YEAR*REGION*STRATUM, data = inds)
summary(delta_star_fit)
delta_star_fit_fall <- aov(delta_star ~ YEAR*REGION*STRATUM, data = fall_inds)
summary(delta_star_fit_fall)
delta_star_fit_spring <- aov(delta_star ~ YEAR*REGION*STRATUM, data = spring_inds)
summary(delta_star_fit_spring)
plot(delta_star_fit)
shapiro.test(inds$delta_star) #not normally distributed
hist(inds$delta_star)


delta_plus_fit <- aov(delta_plus ~ SEASON*YEAR*REGION*STRATUM, data = inds)
summary(delta_plus_fit)
delta_plus_fit_fall <- aov(delta_plus ~ YEAR*REGION*STRATUM, data = fall_inds)
summary(delta_plus_fit_fall)
delta_plus_fit_spring <- aov(delta_plus ~ YEAR*REGION*STRATUM, data = spring_inds)
summary(delta_plus_fit)
plot(delta_plus_fit_spring)
shapiro.test(inds$delta_plus) #not normally distributed
hist(inds$delta_plus)


delta_var_fit <- aov(delta_var ~ SEASON*YEAR*REGION*STRATUM, data = inds)
summary(delta_var_fit)
delta_var_fit_fall <- aov(delta_var ~ YEAR*REGION*STRATUM, data = fall_inds)
summary(delta_var_fit_fall)
delta_var_fit_spring <- aov(delta_var ~ YEAR*REGION*STRATUM, data = spring_inds)
summary(delta_var_fit_spring)
plot(delta_var_fit)
shapiro.test(inds$delta_var) #not normally distributed
hist(inds$delta_var) #not normally distributed





## LK
# ANOVA
#coreSr88 <- aov(medianSr88  ~ Capture.Location*Spawning.time,data =SRdata_coremed)
#summary(coreSr88)
#Plots to examine normality, variance, outliers 
#plot(coreSr88)
#Test for Equal Variance
#bartlett.test(medianSr88~Spawning.time, data=SRdata_coremed)
# test for Normality
#shapiro.test(SRdata_coremed$medianSr88)


