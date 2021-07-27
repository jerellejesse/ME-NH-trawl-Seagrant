####################################################################################
### GAMMS applied to survey biodiversity metrics 
### UM Seagrant; climate impacts on Maine coastal fisheries using the ME-NH inshore trawl
### Objective 1 part 2
### 6/20/2018
### A.E. Weston
####################################################################################


## helpful resources;
# http://www.sfs.uni-tuebingen.de/~jvanrij/Tutorial/GAMM.html#analysis
# https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gamm.html
# LK; https://www.fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# http://www.statsoft.com/Textbook/Generalized-Additive-Models # good building up resource from lm
# https://slideplayer.com/slide/11411340/
# https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
# https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/


##notes;
# linear regression uses least-squares fit for predictor variables (X) to predict dependent variable (Y)
# GLM response variables don't have to be normal or continuous, link function for effects of predictor on dependent variable 
# GAM predict Y by estimating functions of X which are connected via link function, smooth function
# smooths; https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/smooth.terms.html
# fit data so s is as smooth as possible # default is k-1 and thin plate regression spline

# mixed model or not? "variable that describes your data sample as a subset of the data you could have collected"
# - causes lumpyness; in our case year 
# fixed effect; parameters associated with the entire population or with repeatable levels of experimental units
# random effect; parameters associated with idividual experimental units drawn at random from a population; 
#   random deviation from a mean density, blocking factor
# error term is gaussian (normal) by default
# random effects can be 1) random intercept (adjust the height of other modelterms with a constant value: s(Subject, bs="re"))
# 2) random slope (adjust the slope of the trend of a numeric predictor: s(Subject, Time, bs="re")),
# 3) or random smooths (adjust the trend of a numeric predictor in a nonlinear way: s(Time, Subject, bs="fs", m=1))

#side notes;
# temporal autocorrelation btw observations acf()
# REML = restricted maximum likelihood used in generalized additive models
# number of knots can be variable but informed by term within specifications (i.e. kmin always 3, kmax = 12 for months)
# edf = effective degrees of freedom ~ effective N pars
# gamm (nlme; nonlinear mixed effects) vs. gamm4 (lme4; linear mixed effects) - seems to be the new and improved version 
# specifying random effects in gamm4; https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
# overdispersion = variance greater than mean
# interactions; is one mediated by the other; using s() must be same scale, use te() if not same scale and includes main effects
# don't need to do in log space because no responses <= 0

#landings;
#https://www.maine.gov/dmr/commercial-fishing/landings/documents/AnnualLandingsValue.table.pdf
#setwd("J:/Research/Kerr Lab/ME NH trawl data")
#landings <- read.csv('ME_landings.csv', header = TRUE)
#https://www.st.nmfs.noaa.gov/commercial-fisheries/commercial-landings/annual-landings/index
#setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant")
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant")
landings <- read.csv('Landings.csv', header = TRUE)

## landings go through 2016
#### remove commas from matrix
metric_tons <- as.numeric(gsub( ",", "", landings$Metric.Tons))
lbs <- as.numeric(gsub( ",", "", landings$Pounds))
dollars <- as.numeric(gsub(",", "", landings$X.))
year <- landings$Year
State <- as.vector(landings$State)
landings_02 <- cbind(year, State, metric_tons, lbs, dollars)
landings_02 <- as.data.frame(landings_02)

ME_NH_land <- landings_02[landings_02$State == "Maine" | landings_02$State == "New Hampshire",]
write.csv(ME_NH_land, "ME_NH_land.csv")

MA_land <- landings_02[landings_02$State == "Massachusetts",]
write.csv(MA_land, "MA_land.csv")

#GOM_land <- landings_02[landings_02$State == "Maine" | landings_02$State == "New Hampshire" | landings_02$State == "Massachusetts",]
NE_land <-  landings_02[landings_02$State == "Maine" | landings_02$State == "New Hampshire" | landings_02$State == "Massachusetts" |
                     landings_02$State == "Rhode Island" | landings_02$State == "Connecticut" | landings_02$State == "New York" |
                     landings_02$State == "New Jersey" | landings_02$State == "Delaware"| landings_02$State == "Maryland" |
                     landings_02$State == "Virginia",]
write.csv(NE_land, "NE_land.csv")

# Maine New hampshire landings
ME_NH_landings <- read.csv("ME_NH_land.csv")
total <- NULL
yrs <- seq(1960, 2016, by = 1)
for (i in 1:length(yrs)){
  ind_year <- ME_NH_landings[ME_NH_landings$year == yrs[i],]
  total[i] <- sum(ind_year$metric_tons)
}
by_year <- cbind(yrs, total)

# expand survey data to include landings by year (fall)
#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/Results")
library(mgcv)
library(MASS)
library(gamm4)

inds <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
fall <- inds[inds$SEASON == "FL",]
fall$YEAR <- (fall$YEAR + 2000)
fall <- fall[!fall$YEAR == 2017,]

#### add landings by year
fall_years <- fall$YEAR
expand <- matrix(NA, nrow = nrow(fall), ncol = 2)
for (i in 1:nrow(fall)){
  expand[i,] <- as.matrix(by_year[by_year[,"yrs"] == fall_years[i],])
}
colnames(expand) <- c("year", "metric_tons")

ME_NH_fall <- cbind(fall, expand)
write.csv(ME_NH_fall, "ME_NH_fall_exp.csv")

test <- fall[fall$YEAR > 1999 & fall$YEAR < 2014,]
exp <- read.csv("ME_NH_fall_exp.csv")

##### add FVCOM by tow
#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/Results")
FVCOM_fall <- read.csv("FVCOM_ME_NH_fall.csv")
FVCOM <- FVCOM_fall[FVCOM_fall$FV_year < 2017,]

ME_NH_fall_full <- cbind(exp, FVCOM)
#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/GAMMs")
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/Results/GAMMs")
write.csv(ME_NH_fall_full, "ME_NH_fall_full.csv")


### (spring)
spring <- inds[inds$SEASON == "SP",]
spring$YEAR <- (spring$YEAR + 2000)
spring <- spring[!spring$YEAR == 2017,]
spring_years <- spring$YEAR
expand <- matrix(NA, nrow = nrow(spring), ncol = 2)
for (i in 1:nrow(spring)){
  expand[i,] <- as.matrix(by_year[by_year[,"yrs"] == spring_years[i],])
}
colnames(expand) <- c("year", "metric_tons")

ME_NH_spring <- cbind(spring, expand)
write.csv(ME_NH_spring, "ME_NH_spring_exp.csv")
exp <- read.csv("ME_NH_spring_exp.csv")

##### add FVCOM by tow
#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/Results")
FVCOM_spring <- read.csv("FVCOM_ME_NH_spring.csv")
FVCOM <- FVCOM_spring[FVCOM_spring$FV_year < 2017,]

ME_NH_spring_full <- cbind(exp, FVCOM)
#setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/Results/GAMMs")
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/Results/GAMMs")
write.csv(ME_NH_spring_full, "ME_NH_spring_full.csv")







####ME-NH fall models#####
# trawl observations 
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/Results")
fall <- read.csv("ME_NH_fall_exp.csv")
# FVCOM observations 
#setwd("C:/Users/aweston/Box/Ashley Weston (System Account)/Seagrant/Results/GAMMs")
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/Results/GAMMs")
fall <- read.csv("ME_NH_fall_full.csv")
### number of spp 
#1 choose response distribution - start w/normal distribution
hist(fall$N_species) # start w/normal distribution

#2 choose k - let GCV find optimal 

#3 autocorrelation? 
# lat/long = correlated
# bottom/surface salinity = correlated
plot(fall[,20], fall[,23])
# yes so fit w/GAMM

#4 is k large enough? diagnostics ok?
# diagnostic/residual plots; QQ,resid vs. pred
# take care when interpretting results
# k-index; further below 1 = missed pattern in resids
# k is too low if edf ~ k'

N_gam <- gam(N_species ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + #re specified differently (lmer format) 
               s(SURFACE_SALINITY) +  s(START_DEPTH)+ 
               s(START_LONGITUDE, START_LATITUDE) + s(YEAR),  data = fall) 
gam.check(N_gam)
plot(N_gam)
summary(N_gam)
# edfs close to k'
mean(resid(N_gam)^2) #mean squared error (lower = better)

# other package
#N_FL<- gamm(N_species ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
#               s(SURFACE_SALINITY) +  s(YEAR, bs = 're') + #bs = 're' (base smooth = random effect) # random intercept 
#               s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE), data = fall) 
#summary(N_FL$gam)

#all individual terms
N_Fall <- gamm4(N_species ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + #re specified differently (lmer format) 
               s(SURFACE_SALINITY) +  s(START_DEPTH)+ s(START_LATITUDE) +
                s(START_LONGITUDE), random = ~ (1|YEAR) , data = fall) # random intercept w/fixed mean
summary(N_Fall$gam)
mean(resid(N_Fall$gam)^2)

#interact lat/long
# with trawl temp/salinity
N_Fall_2 <- gamm4(N_species ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) +  s(metric_tons) +
                    s(SURFACE_SALINITY) +  s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = fall) 

gam.check(N_Fall_2$gam)
summary(N_Fall_2$gam)
plot(resid(N_Fall_2$gam))
abline(h = 0)
mean(resid(N_Fall_2$gam)^2)

# improves model fit (based on adj Rsq)

#interact lat/long/depth
N_Fall_3 <- gamm4(N_species ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) +  
                    s(SURFACE_SALINITY) +  s(START_LATITUDE, START_LONGITUDE, START_DEPTH), random = ~ (1|YEAR) , data = fall) 
gam.check(N_Fall_3$gam)
# does not improve model fit
mean(resid(N_Fall_3$gam)^2)

# interact lat/long, bot sal/surf sal
N_Fall_4 <- gamm4(N_species ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY, SURFACE_SALINITY) +  
                    +  s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = fall) 
gam.check(N_Fall_4$gam)
summary(N_Fall_4$gam)
# doesn't make much difference
mean(resid(N_Fall_4$gam)^2)


# removing start depth doesn't improve fit
N_Fall_2.1 <- gamm4(N_species ~  s(SURFACE_TEMP_C) + s(WATER_TEMP_C) + s(SALINITY) + s(SURFACE_SALINITY) +
                      + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = fall) 
gam.check(N_Fall_2.1$gam)
summary(N_Fall_2.1$gam)


## best model fit is N_Fall_2
test <- gam(N_species ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) +  
                    s(SURFACE_SALINITY) +  s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE) + YEAR,  data = fall) 
gam.check(test)

#5 significant trend? 
# interpretting results
summary(N_Fall_2$gam) # importance of terms 
print(N_Fall_2$gam) # edf; higher = more complex splines 
confint(N_Fall$gam)

plot(N_Fall_2$gam)



# other tools/testing 
# few fit: covariate by smooth(covariate, edf)
summary(N_Fall_2$mer) #random effects; std = variability/residual = e term, fixed effects;
print(N_Fall$mer) #std
acf(residuals(N_Fall$gam))
vis.gam(N_Fall$gam, view = c("START_LATITUDE", "START_LONGITUDE")) #N spp is highest at low lat and high long
vis.gam(N_Fall$gam, plot.type = "contour", view = c("START_LATITUDE", "START_LONGITUDE"))
vis.gam(N_Fall_2$gam, view = c("WATER_TEMP_C", "SURFACE_TEMP_C")) 
vis.gam(N_Fall_2$gam, view = c("SALINITY", "SURFACE_SALINITY"))

# using FVCOM temp/salinity
N_Fall_FV <- gamm4(N_species ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(metric_tons) +
                     s(FV_surf_sal) +  s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = fall) 
gam.check(N_Fall_FV$gam)
summary(N_Fall_FV$gam)
plot(resid(N_Fall_FV$gam))
plot(N_Fall_FV$gam)


library(mgcv)
glm.test <- glm(N_species ~ (FV_bot_temp) + (FV_surf_temp) + (FV_bot_sal) +  (metric_tons) +
                  (FV_surf_sal) +  (START_DEPTH) + (START_LATITUDE * START_LONGITUDE) , data = fall)
summary(glm.test)
glm.test.sp <- glm(N_species ~ (FV_bot_temp) + (FV_surf_temp) + (FV_bot_sal) +  (metric_tons) +
                  (FV_surf_sal) +  (START_DEPTH) + (START_LATITUDE * START_LONGITUDE) , data = spring_02)
summary(glm.test.sp)
spring_02 <- spring[-665,]



test <- gamm4(N_species ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) +
                    s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) + (1|metric_tons) , data = fall) 
summary(test$gam)

test_2 <- gamm4(N_species ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(metric_tons) +
                     s(FV_surf_sal) +  s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|FV_year) + (1|FV_month) , data = fall) 
summary(test_2$gam)
# using month as a RE does not change significance

devtools::install_github('gavinsimpson/gratia')
library(gratia)


### Shannon-Weiner Index
# for all terms; min number of knots is 3, 4 knots is too many 
#H_FL <- gamm(H_index ~ s(WATER_TEMP_C, k = 3) + s(SURFACE_TEMP_C, k = 3) + s(SALINITY, k = 3) + 
#               s(SURFACE_SALINITY, k = 3) + s(YEAR, bs = 're', k = 3) + 
#               s(START_DEPTH, k = 3)+ s(START_LATITUDE, k = 3) + s(START_LONGITUDE, k = 3), data = fall) 
#summary(H_FL$gam)
H_FL <- gamm(H_index ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
               s(SURFACE_SALINITY) + s(YEAR, bs = 're', k = 17) + 
               s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE, k = 3), data = fall) 
H_Fall <-  gamm4(H_index ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + s(metric_tons), random = ~ (1|YEAR), data = fall) 
summary(H_Fall$gam)
#1 bottom temp, surface temp, surface salinity = significant
#2 surface temp, surface salinity, start depth, start lat, start long = significant
plot(H_FL$gam)
plot(H_FL$lme)

H_Fall_FV <- gamm4(H_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(metric_tons) +
                     s(FV_surf_sal) +  s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = fall) 
summary(H_Fall_FV$gam)
plot(H_Fall_FV$gam)
### Simpson's Diversity Index
# min number of knots is 3, 4/5/6/7/8/9 work - significance changes depending on knots 
#N_knots <- 9
#D_FL <- gamm(D_index  ~ s(WATER_TEMP_C, k = N_knots) + s(SURFACE_TEMP_C, k = N_knots) + s(SALINITY, k = N_knots) +
#               s(SURFACE_SALINITY, k = N_knots) + s(YEAR, bs = 're', k = N_knots) +
#               s(START_DEPTH, k = N_knots)+ s(START_LATITUDE, k = N_knots) + s(START_LONGITUDE, k = N_knots), data = fall) 
#summary(D_FL$gam)

D_FL <- gamm(D_index  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) +
               s(SURFACE_SALINITY) + s(YEAR, bs = 're', k = 17) +
               s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE, k = 3), data = fall) 
D_Fall <- gamm4(D_index  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) +
                  s(SURFACE_SALINITY)  + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                  s(metric_tons), random = ~ (1|YEAR), data = fall)
summary(D_Fall$gam)
#1 surface temp, surface salinity = significant
#2 surface salinity, depth, lat, long = significant
plot(D_FL$gam)
plot(D_FL$lme)

D_Fall_FV <- gamm4(D_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(metric_tons) +
                     s(FV_surf_sal) +  s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = fall) 
summary(D_Fall_FV$gam)
plot(D_Fall_FV$gam)
### Simpson's Evenness Index
E_FL <- gamm(E_index  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
               s(SURFACE_SALINITY) + s(YEAR, bs = 're') +
               s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE), data = fall) 
summary(E_FL$gam)
E_Fall <- gamm4(E_index  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
               s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                 s(metric_tons), random = ~ (1|YEAR), data = fall) 
summary(E_Fall$gam)
#1 bottom temp and surface temp = significant
plot(E_FL$gam)
plot(E_FL$lme)

E_Fall_FV <- gamm4(E_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(metric_tons) +
                     s(FV_surf_sal) +  s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = fall) 
summary(E_Fall_FV$gam)
plot(E_Fall_FV$gam)
### Taxonomic diversity
# min number of knots = 3, 4 works, significance levels not different 
N_knots <- 3
delta_FL <- gamm(delta  ~ s(WATER_TEMP_C, k = N_knots) + s(SURFACE_TEMP_C, k = N_knots) + s(SALINITY, k = N_knots) + 
                   s(SURFACE_SALINITY, k = N_knots) + s(YEAR, bs = 're', k = N_knots) +
                   s(START_DEPTH, k = N_knots)+ s(START_LATITUDE, k = N_knots) + s(START_LONGITUDE, k = N_knots), data = fall) 
summary(delta_FL$gam)
#1 bottom temp = significant
delta_FL <- gamm(delta  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                   s(SURFACE_SALINITY) + s(YEAR, bs = 're', k = 17) +
                   s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE, k = 3), data = fall) 
summary(delta_FL$gam)
delta_Fall <- gamm4(delta ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                      s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                      s(metric_tons), random = ~ (1|YEAR), data = fall)
summary(delta_Fall$gam)

delta_Fall_FV <- gamm4(delta ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(metric_tons) +
                     s(FV_surf_sal) +  s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = fall) 
summary(delta_Fall_FV$gam)
plot(delta_Fall_FV$gam)

### Taxonomic distinctness
delta_star_FL <- gamm(delta_star  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                        s(SURFACE_SALINITY) + s(YEAR, bs = 're') +
                        s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE), data = fall) 
summary(delta_star_FL$gam)
#1 surface temp, bottom salinity, surface salinity, and bottom temp = significant
delta_star_Fall <- gamm4(delta_star ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                           s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                           s(metric_tons), random = ~ (1|YEAR), data = fall)
summary(delta_star_Fall$gam)

delta_star_Fall_FV <- gamm4(delta_star ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(metric_tons) +
                     s(FV_surf_sal) +  s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = fall) 
summary(delta_star_Fall_FV$gam)
plot(delta_star_Fall_FV$gam)

### Average taxonomic distinctness
delta_plus_FL <- gamm(delta_plus  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                        s(SURFACE_SALINITY) + s(YEAR, bs = 're') +
                        s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE), data = fall) 
summary(delta_plus_FL$gam)
#1 surface temp, bottom salinity, bottom temp, and surface salinity, and year = significant
delta_plus_Fall <- gamm4(delta_plus ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                           s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                           s(metric_tons), random = ~ (1|YEAR), data = fall)
summary(delta_plus_Fall$gam)

delta_plus_Fall_FV <- gamm4(delta_plus ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(metric_tons) +
                     s(FV_surf_sal) +  s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = fall) 
summary(delta_plus_Fall_FV$gam)
plot(delta_plus_Fall_FV$gam)
### Variation in taxonomic distinctness
delta_var_FL <- gamm(delta_var  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) +
                       s(SURFACE_SALINITY) + s(YEAR, bs = 're') +
                       s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE), data = fall) 
summary(delta_var_FL$gam)
delta_var_Fall <- gamm4(delta_var ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                          s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                          s(metric_tons), random = ~ (1|YEAR), data = fall)
summary(delta_var_Fall$gam)
#1 year, surface temp, and bottom temp = significant

delta_var_Fall_FV <- gamm4(delta_var ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(metric_tons) +
                     s(FV_surf_sal) +  s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = fall) 
summary(delta_var_Fall_FV$gam)
plot(delta_var_Fall_FV$gam)

####ME-NH spring models #####
library(gamm4)
# trawl observations:
spring <- read.csv("ME_NH_spring_exp.csv")
# FVCOM observations 
spring <- read.csv("ME_NH_spring_full.csv")
N_SP <- gamm(N_species ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
               s(SURFACE_SALINITY) + s(YEAR, bs = 're') +
               s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE), data = spring) 
summary(N_SP$gam)
# bottom salinity, surface salinity, surface temp, and year = significant
N_Spring  <- gamm4(N_species ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                     s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                     s(metric_tons), random = ~ (1|YEAR), data = spring) 
summary(N_Spring$gam)

FV_N_sp <- gamm4(N_species ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) + s(FV_surf_sal) + s(metric_tons) +
                                              s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = spring) 
summary(FV_N_sp$gam)
plot(FV_N_sp$gam)

### Shannon-Weiner Index
H_SP <- gamm(H_index ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
               s(SURFACE_SALINITY) + s(YEAR, bs = 're') +
               s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE), data = spring) 
summary(H_SP$gam)
H_Spring <- gamm4(H_index ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                    s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                    s(metric_tons), random = ~ (1|YEAR), data = spring) 
summary(H_Spring$gam)
#1 bottom salinity, surface temp, and year = significant
FV_H_sp <- gamm4(H_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) + s(FV_surf_sal) + s(metric_tons) +
                   s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = spring) 
summary(FV_H_sp$gam)
plot(FV_H_sp$gam)


### Simpson's Diversity Index
D_SP <- gamm(D_index  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
               s(SURFACE_SALINITY) + s(YEAR, bs = 're') +
               s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE), data = spring) 
summary(D_SP$gam)
D_Spring <- gamm4(D_index ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                    s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                    s(metric_tons), random = ~ (1|YEAR), data = spring)
summary(D_Spring$gam)
#1 year, surface temp, bottom salinity = significant 
FV_D_sp <- gamm4(D_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) + s(FV_surf_sal) + s(metric_tons) +
                   s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = spring) 
summary(FV_D_sp$gam)
plot(FV_D_sp$gam)

### Simpson's Evenness Index
E_SP <- gamm(E_index  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
               s(SURFACE_SALINITY) + s(YEAR, bs = 're') +
               s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE), data = spring) 
summary(E_SP$gam)
E_Spring <- gamm4(E_index ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                    s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                    s(metric_tons), random = ~ (1|YEAR), data = spring)
summary(E_Spring$gam)
#1 surface salinity, year, bottom salinity, and surface temp = significant
FV_E_sp <- gamm4(E_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) + s(FV_surf_sal) + s(metric_tons) +
                   s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = spring) 
summary(FV_E_sp$gam)
plot(FV_E_sp$gam)

### Taxonomic diversity 
#N_knots <- 3 # 3/4/5 knots work - significance changes w/knots 
#delta_SP <- gamm(delta  ~ s(WATER_TEMP_C, k = N_knots) + s(SURFACE_TEMP_C, k = N_knots) + s(SALINITY, k = N_knots) + 
#                   s(SURFACE_SALINITY, k = N_knots) + s(YEAR, bs = 're', k = N_knots) +
#                   s(START_DEPTH, k = N_knots)+ s(START_LATITUDE, k = N_knots) + s(START_LONGITUDE, k = N_knots), data = spring) 
#summary(delta_SP$gam)

delta_SP <- gamm(delta  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                   s(SURFACE_SALINITY) + s(YEAR, bs = 're', k = 17) +
                   s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE, k = 3), data = spring) 
summary(delta_SP$gam)
delta_Spring <- gamm4(delta ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                        s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                        s(metric_tons), random = ~ (1|YEAR), data = spring)
summary(delta_Spring$gam)
#1 surface temp, bottom salinity, surface salinity, year = significant
#2 surface temp, lat, long = significant 
FV_delta_sp <- gamm4(delta ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) + s(FV_surf_sal) + s(metric_tons) +
                   s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = spring) 
summary(FV_delta_sp$gam)
plot(FV_delta_sp$gam)


### Taxonomic distinctness
delta_star_SP <- gamm(delta_star  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                        s(SURFACE_SALINITY) + s(YEAR, bs = 're') +
                        s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE), data = spring) 
summary(delta_star_SP$gam)
summary(delta_star_SP$lme)
delta_star_Spring <- gamm4(delta_star ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                             s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                             s(metric_tons), random = ~ (1|YEAR), data = spring)
summary(delta_star_Spring$gam)
#1 bottom temp and year = significant
FV_delta_star_sp <- gamm4(delta_star ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) + s(FV_surf_sal) + s(metric_tons) +
                       s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = spring) 
summary(FV_delta_star_sp$gam)
plot(FV_delta_star_sp$gam)

### Average taxonomic distinctness
delta_plus_SP <- gamm(delta_plus  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                        s(SURFACE_SALINITY) + s(YEAR, bs = 're') +
                        s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE), data = spring) 
summary(delta_plus_SP$gam)
delta_plus_Spring <- gamm4(delta_plus ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                             s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                             s(metric_tons), random = ~ (1|YEAR), data = spring)
summary(delta_plus_Spring$gam)
#1 bottom salinity, bottom temp, surface salinity, surface temp = significant 

FV_delta_plus_sp <- gamm4(delta_plus ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) + s(FV_surf_sal) + s(metric_tons) +
                            s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = spring) 
summary(FV_delta_plus_sp$gam)
plot(FV_delta_plus_sp$gam)

### Variation in taxonomic distinctness
delta_var_SP <- gamm(delta_var  ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                       s(SURFACE_SALINITY) + s(YEAR, bs = 're') +
                       s(START_DEPTH)+ s(START_LATITUDE) + s(START_LONGITUDE), data = spring) 
summary(delta_var_SP$gam)
delta_var_Spring <- gamm4(delta_var ~ s(WATER_TEMP_C) + s(SURFACE_TEMP_C) + s(SALINITY) + 
                            s(SURFACE_SALINITY) + s(START_DEPTH)+ s(START_LATITUDE, START_LONGITUDE) + 
                            s(metric_tons), random = ~ (1|YEAR), data = spring)
summary(delta_var_Spring$gam)
#1 bottom temp, year, bottom salinity, surface temp = significant

FV_delta_var_sp <- gamm4(delta_var ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) + s(FV_surf_sal) + s(metric_tons) +
                            s(START_DEPTH) + s(START_LATITUDE, START_LONGITUDE), random = ~ (1|YEAR) , data = spring) 
summary(FV_delta_var_sp$gam)
plot(FV_delta_var_sp$gam)



###### env ####
surf_temp <- by_tow$SURFTEMP
sum(is.na(surf_temp))
2269/20148 # 11% of tows do not have temp
boxplot(surf_temp ~ by_tow$EST_YEAR)
bot_temp <- by_tow$BOTTEMP
sum(is.na(bot_temp))
2708/20148 #13%
surf_sal <- by_tow$SURFSALIN
sum(is.na(surf_sal))
11461/20148 #56%
bot_sal <- by_tow$BOTSALIN
sum(is.na(bot_sal))
11610/20148 #57

btemp <- survdat_FL$BOTTEMP
sum(is.na(btemp))
163037/1439465 #11%
stemp <- survdat_FL$SURFTEMP
sum(is.na(stemp))
142910/1439465 #9%
bsal <- survdat_FL$BOTSALIN
sum(is.na(bsal))
708015/1439465 #50 % 
ssal <- survdat_FL$SURFSALIN
sum(is.na(ssal))
696972/1439465 #48%


setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/GAMMs")
fall <- read.csv("ME_NH_fall_full.csv")
fall <- fall[, -1:-4]
plot(fall$FV_bot_temp ~ fall$FV_year)
plot(fall$N_species ~ fall$FV_bot_temp, xlab = "FVCOM bottom temp", ylab = "Number of species", main = "ME-NH Fall")
plot(fall$N_species ~ fall$FV_surf_temp)
plot(fall$N_species ~ fall$FV_bot_sal)
plot(fall$N_species ~ fall$FV_surf_sal)
plot(fall$N_species ~ fall$START_DEPTH)
plot(fall$N_species ~ fall$START_LATITUDE)
plot(fall$H_index ~ fall$FV_bot_temp)
plot(fall$H_index ~ fall$FV_surf_temp)
plot(fall$H_index ~ fall$FV_bot_sal)
plot(fall$D_index ~ fall$FV_bot_temp)
plot(fall$delta ~ fall$FV_bot_temp)
plot(fall$delta_star ~ fall$FV_bot_temp)
plot(fall$delta_plus ~ fall$FV_bot_temp)
plot(fall$delta_var ~ fall$FV_bot_temp)
plot(fall$delta_var ~ fall$FV_surf_temp)

plot(fall$FV_bot_temp ~ fall$FV_year, xlab = "Year", ylab = "Bottom Temp", main = "FVCOM Fall")
plot(fall$FV_surf_temp ~ fall$FV_year, xlab = "Year", ylab = "Surface Temp", main = "FVCOM Fall")
plot(fall$FV_bot_sal ~ fall$FV_year, xlab = "Year", ylab = "Bottom Salinity", main = "FVCOM Fall")
plot(fall$FV_surf_sal ~ fall$FV_year, xlab = "Year", ylab = "Surface Sal", main = "FVCOM Fall")


spring <- read.csv("ME_NH_spring_full.csv")
plot(spring$N_species ~ spring$FV_bot_temp)
plot(spring$N_species ~ spring$FV_surf_temp)
plot(spring$N_species ~ spring$FV_bot_sal)
plot(spring$N_species ~ spring$FV_surf_sal)

plot(spring$FV_bot_temp ~ spring$FV_year, xlab = "Year", ylab = "Bottom temp", main = "FVCOM Spring")
plot(spring$FV_surf_temp ~ spring$FV_year, xlab = "Year", ylab = "Surface temp", main = "FVCOM Spring")
plot(spring$FV_bot_sal ~ spring$FV_year, xlab = "Year", ylab = "Bottom salinity", main = "FVCOM Spring")
plot(spring$FV_surf_sal ~ spring$FV_year, xlab = "Year", ylab = "Surface salinity", main = "FVCOM Spring")





####GOM trawl fall ####
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/NEFSC")
SP_by_tow <- read.csv("nefsc_SP_by_tow.csv", header = TRUE)
FL_by_tow <- read.csv("nefsc_FL_by_tow.csv", header = TRUE)
load('NEFSC_FL_taxinds_by_tow.RData')
FL_tax_by_tow <- tax_by_tow
load('NEFSC_SP_taxinds_by_tow.RData')
SP_tax_by_tow <- tax_by_tow
# remove non-taxonomic tows that have only 1 spp observed so that data can be combined
FL_by_tow <- FL_by_tow[!FL_by_tow$N_species == 1,]
FL_inds <- subset(FL_by_tow, select = c(N_species, H_index, D_index, E_index))
clip_spring <- SP_by_tow[!SP_by_tow$N_species == 1,]
SP_inds <- subset(SP_by_tow, select = c(N_species, H_index, D_index, E_index))

NEFSC_fall <- cbind(FL_tax_by_tow, FL_inds)
write.csv(NEFSC_fall, "NEFSC_all_metrics_FL.csv")
NEFSC_spring <- cbind(SP_tax_by_tow, SP_inds)
write.csv(NEFSC_spring, "NEFSC_all_metrics_SP.csv")


####remove strata from canadian waters and south of hatteras 
NEFSC_fall <- read.csv("NEFSC_all_metrics_FL.csv")
can <- c(1351, 1350, 1310, 1320, 1330, 1410,1420, 1490, 1990)
FL_other <- unique(NEFSC_fall$STRATUM[NEFSC_fall$STRATUM > 3990]) #South of hatteras and scotian shelf 
FL_remove <- c(can, FL_other)
clip_fall <- NEFSC_fall[!NEFSC_fall$STRATUM %in% FL_remove,]
NEFSC_spring <- read.csv("NEFSC_all_metrics_SP.csv")
SP_other <- unique(NEFSC_spring$STRATUM[NEFSC_spring$STRATUM > 3990])
SP_remove <- c(can, SP_other)
clip_spring <- NEFSC_spring[!NEFSC_spring$STRATUM %in% SP_remove,]
# select strata in GOM/GB
GOM_fall <- clip_fall[clip_fall$STRATUM == 1130 | clip_fall$STRATUM == 1140 | clip_fall$STRATUM == 1150 | clip_fall$STRATUM == 1160 |
                        clip_fall$STRATUM == 1170 | clip_fall$STRATUM == 1180 | clip_fall$STRATUM == 1190 | clip_fall$STRATUM == 1200 |
                        clip_fall$STRATUM == 1210 | clip_fall$STRATUM == 1220 | clip_fall$STRATUM == 1230 | clip_fall$STRATUM == 1240 |
                        clip_fall$STRATUM == 1250 | clip_fall$STRATUM == 1260 | clip_fall$STRATUM == 1270 |
                        clip_fall$STRATUM == 1280 | clip_fall$STRATUM == 1290 | clip_fall$STRATUM == 1300 | clip_fall$STRATUM == 1360 |
                        clip_fall$STRATUM == 1370 | clip_fall$STRATUM == 1380 | clip_fall$STRATUM == 1390 | clip_fall$STRATUM == 1400,]


GOM_spring <- clip_spring[clip_spring$STRATUM == 1130 | clip_spring$STRATUM == 1140 | clip_spring$STRATUM == 1150 | clip_spring$STRATUM == 1160 |
                          clip_spring$STRATUM == 1170 | clip_spring$STRATUM == 1180 | clip_spring$STRATUM == 1190 | clip_spring$STRATUM == 1200 |
                          clip_spring$STRATUM == 1210 | clip_spring$STRATUM == 1220 | clip_spring$STRATUM == 1230 | clip_spring$STRATUM == 1240 |
                          clip_spring$STRATUM == 1250 | clip_spring$STRATUM == 1260 | clip_spring$STRATUM == 1270 |
                          clip_spring$STRATUM == 1280 | clip_spring$STRATUM == 1290 | clip_spring$STRATUM == 1300 | clip_spring$STRATUM == 1360 |
                          clip_spring$STRATUM == 1370 | clip_spring$STRATUM == 1380 | clip_spring$STRATUM == 1390 | clip_spring$STRATUM == 1400,]

setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/GOM")
write.csv(GOM_fall, "GOM_fall.csv")
write.csv(GOM_spring, "GOM_spring.csv")

## must clip time series to 1978-2017
fl <- read.csv("GOM_fall.csv")
fl <- fl[fl$EST_YEAR > 1977,]
sp <- read.csv("GOM_spring.csv")
sp <- sp[sp$EST_YEAR > 1977,]
sp <- sp[sp$EST_YEAR < 2018,]


### add FVCOM by tow
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
FVCOM_fall <- read.csv("FVCOM_GOM_fall.csv")
GOM_fall_full <- cbind(fl, FVCOM_fall)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/GAMMs")
write.csv(GOM_fall_full, "GOM_fall_full.csv")

FVCOM_spring <- read.csv("FVCOM_GOM_spring.csv")
GOM_spring_full <- cbind(sp, FVCOM_spring)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/GAMMs")
write.csv(GOM_spring_full, "GOM_spring_full.csv")




####fall w/trawl observations
#### SALINITY OBSERVATIONS DO NOT START UNTIL AFTER 1992

### fall w/FVCOM ovbservations
## (1978-2017)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/GAMMs")
GOM_fall <- read.csv("GOM_fall_full.csv")

library(gamm4)
# number of species
GOM_N_FL <- gamm4(N_species ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
  s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(GOM_N$gam)
plot(GOM_N$gam)

FV_GOM_N_FL <- gamm4(N_species ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
  s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(FV_GOM_N_FL$gam)
plot(FV_GOM_N_FL$gam)

#Shannon-weiner
GOM_H_FL <- gamm4(H_index ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
  s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(GOM_H_FL$gam)

FV_GOM_H_FL <- gamm4(H_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
  s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(FV_GOM_H_FL$gam)
plot(FV_GOM_H_FL$gam)

#simpsons d
GOM_D_FL <- gamm4(D_index ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
  s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(GOM_D_FL$gam)

FV_GOM_D_FL <- gamm4(D_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
 s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(FV_GOM_D_FL$gam)
plot(FV_GOM_D_FL$gam)

# simpsons evenness
GOM_E_FL <- gamm4(E_index ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
  s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(GOM_E_FL$gam)

FV_GOM_E_FL <- gamm4(E_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
  s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(FV_GOM_E_FL$gam)
plot(FV_GOM_E_FL$gam)

#tax diversity
GOM_delta_FL <- gamm4(delta ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
   s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(GOM_delta_FL$gam)

FV_GOM_delta_FL <- gamm4(delta ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(FV_GOM_delta_FL$gam)


# tax distinctness
GOM_delta_star_FL <- gamm4(delta_star ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
   s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(GOM_delta_star_FL$gam)

FV_GOM_delta_star_FL <- gamm4(delta_star ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(FV_GOM_delta_star_FL$gam)
plot(FV_GOM_delta_star_FL$gam)

# avg tax distinctness
GOM_delta_plus_FL <- gamm4(delta_plus ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
    s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(GOM_delta_plus_FL$gam)

FV_GOM_delta_plus_FL <- gamm4(delta_plus ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(FV_GOM_delta_plus_FL$gam)
plot(FV_GOM_delta_plus_FL$gam)


# var tax distinctness
GOM_delta_var_FL <- gamm4(delta_var ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
    s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(GOM_delta_var_FL$gam)

FV_GOM_delta_var_FL <- gamm4(delta_var ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_fall) 
summary(FV_GOM_delta_var_FL$gam)
plot(FV_GOM_delta_var_FL$gam)



####GOM trawl spring####
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/GAMMs")
GOM_spring <- read.csv("GOM_spring_full.csv")

library(gamm4)
# N species
GOM_N_SP <- gamm4(N_species ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
  s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(GOM_N_SP$gam)

FV_GOM_N_SP <- gamm4(N_species ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(FV_GOM_N_SP$gam)
plot(FV_GOM_N_SP$gam)

# shannon-weiner
GOM_H_SP <- gamm4(H_index ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
  s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(GOM_H_SP$gam)

FV_GOM_H_SP <- gamm4(H_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(FV_GOM_H_SP$gam)
plot(FV_GOM_H_SP$gam)

# simpsons d
GOM_D_SP <- gamm4(D_index ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
  s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(GOM_D_SP$gam)

FV_GOM_D_SP <- gamm4(D_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(FV_GOM_D_SP$gam)
plot(FV_GOM_D_SP$gam)

# simpsons evenness
GOM_E_SP <- gamm4(E_index ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
  s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(GOM_E_SP$gam)
FV_GOM_E_SP <- gamm4(E_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(FV_GOM_E_SP$gam)
plot(FV_GOM_E_SP$gam)

# tax diversity
GOM_delta_SP <- gamm4(delta ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
  s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(GOM_delta_SP$gam)

FV_GOM_delta_SP <- gamm4(delta ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(FV_GOM_delta_SP$gam)


# tax distinctness
GOM_delta_star_SP <- gamm4(delta_star ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
   s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(GOM_delta_star_SP$gam)

FV_GOM_delta_star_SP <- gamm4(delta_star ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(FV_GOM_delta_star_SP$gam)


# avg tax distinctness
GOM_delta_plus_SP <- gamm4(delta_plus ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
   s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(GOM_delta_plus_SP$gam)

FV_GOM_delta_plus_SP <- gamm4(delta_plus ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(FV_GOM_delta_plus_SP$gam)
plot(FV_GOM_delta_plus_SP$gam)

# var tax distinctness
GOM_delta_var_SP <- gamm4(delta_var ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) +  s(SURFSALIN) + 
   s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(GOM_delta_var_SP$gam)

FV_GOM_delta_var_SP <- gamm4(delta_var ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(SETDEPTH)+ s(DECDEG_BEGLAT, DECDEG_BEGLON) , random = ~ (1|EST_YEAR), data = GOM_spring) 
summary(FV_GOM_delta_var_SP$gam)
plot(FV_GOM_delta_var_SP$gam)



plot(GOM_fall$BOTTEMP ~ GOM_fall$EST_YEAR, xlab = "Year", ylab = "Bottom temperature", main = "GOM fall trawl observations")
plot(GOM_fall$SURFTEMP ~ GOM_fall$EST_YEAR, xlab = "Year", ylab = "Surface temperature", main = "GOM fall trawl observations")
plot(GOM_fall$BOTSALIN ~ GOM_fall$EST_YEAR, xlab = "Year", ylab = "Bottom salinity", main = "GOM fall trawl observations")
plot(GOM_fall$SURFSALIN ~ GOM_fall$EST_YEAR, xlab = "Year", ylab = "Surface salinity", main = "GOM fall trawl observations")

plot(GOM_spring$BOTTEMP ~ GOM_spring$EST_YEAR, xlab = "Year", ylab = "Bottom temperature", main = "GOM spring trawl observations")
plot(GOM_spring$SURFTEMP ~ GOM_spring$EST_YEAR, xlab = "Year", ylab = "Surface temperature", main = "GOM spring trawl observations")
plot(GOM_spring$BOTSALIN ~ GOM_spring$EST_YEAR, xlab = "Year", ylab = "Bottom salinity", main = "GOM spring trawl observations")
plot(GOM_spring$SURFSALIN ~ GOM_spring$EST_YEAR, xlab = "Year", ylab = "Surface salinity", main = "GOM spring trawl observations")

plot(GOM_fall$FV_bot_temp ~ GOM_fall$EST_YEAR, xlab = "Year", ylab = "Bottom Temperature", main = "GOM FVCOM fall")
plot(GOM_fall$FV_surf_temp ~ GOM_fall$EST_YEAR, xlab = "Year", ylab = "Surface Temperature", main = "GOM FVCOM fall")
plot(GOM_fall$FV_bot_sal ~ GOM_fall$EST_YEAR, xlab = "Year", ylab = "Bottom Salinity", main = "GOM FVCOM fall")
plot(GOM_fall$FV_surf_sal ~ GOM_fall$EST_YEAR, xlab = "Year", ylab = "Surface Salinity", main = "GOM FVCOM fall")

plot(GOM_spring$FV_bot_temp ~ GOM_spring$EST_YEAR, xlab = "Year", ylab = "Bottom Temperature", main = "GOM FVCOM spring")
plot(GOM_spring$FV_surf_temp ~ GOM_spring$EST_YEAR, xlab = "Year", ylab = "Surface Temperature", main = "GOM FVCOM spring")
plot(GOM_spring$FV_bot_sal ~ GOM_spring$EST_YEAR, xlab = "Year", ylab = "Bottom Salinity", main = "GOM FVCOM spring")
plot(GOM_spring$FV_surf_sal ~ GOM_spring$EST_YEAR, xlab = "Year", ylab = "Surface Salinity", main = "GOM FVCOM spring")

plot(GOM_fall$DECDEG_BEGLAT ~ GOM_fall$DECDEG_BEGLON)
plot(GOM_spring$DECDEG_BEGLAT ~ GOM_spring$DECDEG_BEGLON)

plot(GOM_spring$FV_bot_temp ~ GOM_spring$N_species)


####MDMF fall GAMMs####
## only has temperature observations (from trawl)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/MDMF")
MA_trawl <- read.csv("MDMF_fall_div_ind_by_tow.csv")
MA_trawl_02 <- read.csv("MDMF_spring_div_ind_by_tow.csv")
MA_trawl_02 <- MA_trawl_02[!MA_trawl_02$YEAR == 2018,]

setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/")
MA_FV <- read.csv("FVCOM_MA_fall.csv")
MA_fall <- cbind(MA_trawl, MA_FV)
MA_FV <- read.csv("FVCOM_MA_spring.csv")
MA_spring <- cbind(MA_trawl_02, MA_FV)


setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/GAMMs")
write.csv(MA_fall, "MA_fall_full.csv")
write.csv(MA_spring, "MA_spring_full.csv")


MA_fall <- read.csv("MA_fall_full.csv")
library(gamm4)
# number of species
FV_MA_N_FL <- gamm4(N_species ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                       s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_fall) 
summary(FV_MA_N_FL$gam)
plot(FV_MA_N_FL$gam)

# S-W
FV_MA_H_FL <- gamm4(H_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                      s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_fall) 
summary(FV_MA_H_FL$gam)
plot(FV_MA_H_FL$gam)

# Simpsons D
FV_MA_D_FL <- gamm4(D_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                      s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_fall) 
summary(FV_MA_D_FL$gam)
plot(FV_MA_D_FL$gam)

#simpsons e
FV_MA_E_FL <- gamm4(E_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                      s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_fall) 
summary(FV_MA_E_FL$gam)
plot(FV_MA_E_FL$gam)

#tax diversity
FV_MA_delta_FL <- gamm4(delta ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                      s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_fall) 
summary(FV_MA_delta_FL$gam)

# tax distinctness
FV_MA_delta_star_FL <- gamm4(delta_star ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                          s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_fall) 
summary(FV_MA_delta_star_FL$gam)
plot(FV_MA_delta_star_FL$gam)

# avg tax distinctness
FV_MA_delta_plus_FL <- gamm4(delta_plus ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                          s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_fall) 
summary(FV_MA_delta_plus_FL$gam)
plot(FV_MA_delta_plus_FL$gam)

# var in tax distinctness
FV_MA_delta_var_FL <- gamm4(delta_var ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                          s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_fall) 
summary(FV_MA_delta_var_FL$gam)
plot(FV_MA_delta_var_FL$gam)




####MDMF spring GAMMs####
library(gamm4)
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/GAMMs")
MA_spring <- read.csv("MA_spring_full.csv")
# number of species
FV_MA_N_SP <- gamm4(N_species ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                      s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_spring) 
summary(FV_MA_N_SP$gam)
plot(FV_MA_N_SP$gam)

# S-W
FV_MA_H_SP <- gamm4(H_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                      s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_spring) 
summary(FV_MA_H_SP$gam)
plot(FV_MA_N_SP$gam)

# Simpsons D
FV_MA_D_SP <- gamm4(D_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                      s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_spring) 
summary(FV_MA_D_SP$gam)
plot(FV_MA_D_SP$gam)

#simpsons e
FV_MA_E_SP <- gamm4(E_index ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                      s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_spring) 
summary(FV_MA_E_SP$gam)
plot(FV_MA_E_SP$gam)

#tax diversity
FV_MA_delta_SP <- gamm4(delta ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                          s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_spring) 
summary(FV_MA_delta_SP$gam)

# tax distinctness
FV_MA_delta_star_SP <- gamm4(delta_star ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                               s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_spring) 
summary(FV_MA_delta_star_SP$gam)
plot(FV_MA_delta_star_SP$gam)

# avg tax distinctness
FV_MA_delta_plus_SP <- gamm4(delta_plus ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                               s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_spring) 
summary(FV_MA_delta_plus_SP$gam)
plot(FV_MA_delta_plus_SP$gam)

# var in tax distinctness
FV_MA_delta_var_SP <- gamm4(delta_var ~ s(FV_bot_temp) + s(FV_surf_temp) + s(FV_bot_sal) +  s(FV_surf_sal) + 
                              s(DEPTH_M)+ s(LATITUDE, LONGITUDE) , random = ~ (1|YEAR), data = MA_spring) 
summary(FV_MA_delta_var_SP$gam)
plot(FV_MA_delta_var_SP$gam)



####env boxplots#### 
library(ggplot2)

# ME-NH fall
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/GAMMs")
fall <- read.csv("ME_NH_fall_full.csv")

library(reshape2)
library(ggpubr)

# ST
trawl_ST <- subset(fall, select = c(YEAR, SURFACE_TEMP_C))
trawl <- rep("trawl", 1417)
trawl_ST <- cbind(trawl_ST, trawl)
colnames(trawl_ST) <- c("Year", "Temperature", "Source")
FV_ST <- subset(fall, select = c(YEAR, FV_surf_temp))
FVCOM <- rep("FVCOM", 1417)
FV_ST <- cbind(FV_ST, FVCOM)
colnames(FV_ST) <- c("Year", "Temperature", "Source")
ME_fall <- rbind(trawl_ST, FV_ST)
ME_fall <- ME_fall[!is.na(ME_fall$Temperature),]
ST <- ggplot(data = ME_fall, aes(x = factor(ME_fall$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("ME-NH fall surface temp") + labs(x = "Year", y = "Surface Temperature") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(4,16))

#BT
trawl_BT <- subset(fall, select = c(YEAR, WATER_TEMP_C))
trawl_BT <- cbind(trawl_BT, trawl)
colnames(trawl_BT) <- c("Year", "Temperature", "Source")
FV_BT <- subset(fall, select = c(YEAR, FV_bot_temp))
FV_BT <- cbind(FV_BT, FVCOM)
colnames(FV_BT) <- c("Year", "Temperature", "Source")
ME_fall_BT <- rbind(trawl_BT, FV_BT)
BT <- ggplot(data = ME_fall_BT, aes(x = factor(ME_fall_BT$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("ME-NH fall bottom temp") + labs(x = "Year", y = "Bottom Temperature") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(4,16))

trawl_SS <- subset(fall, select = c(YEAR, SURFACE_SALINITY))
trawl_SS <- cbind(trawl_SS, trawl)
colnames(trawl_SS) <- c("Year", "Temperature", "Source")
FV_SS <- subset(fall, select = c(YEAR, FV_surf_sal))
FV_SS <- cbind(FV_SS, FVCOM)
colnames(FV_SS) <- c("Year", "Temperature", "Source")
ME_fall_SS <- rbind(trawl_SS, FV_SS)
SS <- ggplot(data = ME_fall_SS, aes(x = factor(ME_fall_SS$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("ME-NH fall surface salinity") + labs(x = "Year", y = "Surface Salinity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(0,36))


#BS
trawl_BS <- subset(fall, select = c(YEAR, SALINITY))
trawl_BS <- cbind(trawl_BS, trawl)
colnames(trawl_BS) <- c("Year", "Temperature", "Source")
FV_BS <- subset(fall, select = c(YEAR, FV_bot_sal))
FV_BS <- cbind(FV_BS, FVCOM)
colnames(FV_BS) <- c("Year", "Temperature", "Source")
ME_fall_BS <- rbind(trawl_BS, FV_BS)
BS <- ggplot(data = ME_fall_BS, aes(x = factor(ME_fall_BS$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("ME-NH fall bottom salinity") + labs(x = "Year", y = "Bottom Salinity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(0,36))

ggarrange(ST, BT, SS, BS, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)

### spring me nh 
spring <- read.csv("ME_NH_spring_full.csv")
trawl_ST <- subset(spring, select = c(YEAR, SURFACE_TEMP_C))
trawl <- rep("trawl", 1778)
trawl_ST <- cbind(trawl_ST, trawl)
colnames(trawl_ST) <- c("Year", "Temperature", "Source")
FV_ST <- subset(spring, select = c(YEAR, FV_surf_temp))
FVCOM <- rep("FVCOM", 1778)
FV_ST <- cbind(FV_ST, FVCOM)
colnames(FV_ST) <- c("Year", "Temperature", "Source")
ME_spring_ST <- rbind(trawl_ST, FV_ST)
ST <- ggplot(data = ME_spring_ST, aes(x = factor(ME_spring_ST$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("ME-NH spring surface temp") + labs(x = "Year", y = "Surface Temperature") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(3,16))

#BT
trawl_BT <- subset(spring, select = c(YEAR, WATER_TEMP_C))
trawl_BT <- cbind(trawl_BT, trawl)
colnames(trawl_BT) <- c("Year", "Temperature", "Source")
FV_BT <- subset(spring, select = c(YEAR, FV_bot_temp))
FV_BT <- cbind(FV_BT, FVCOM)
colnames(FV_BT) <- c("Year", "Temperature", "Source")
ME_spring_BT <- rbind(trawl_BT, FV_BT)
BT <- ggplot(data = ME_spring_BT, aes(x = factor(ME_spring_BT$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("ME-NH spring bottom temp") + labs(x = "Year", y = "Bottom Temperature") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(3,16))

trawl_SS <- subset(spring, select = c(YEAR, SURFACE_SALINITY))
trawl_SS <- cbind(trawl_SS, trawl)
colnames(trawl_SS) <- c("Year", "Temperature", "Source")
FV_SS <- subset(spring, select = c(YEAR, FV_surf_sal))
FV_SS <- cbind(FV_SS, FVCOM)
colnames(FV_SS) <- c("Year", "Temperature", "Source")
ME_spring_SS <- rbind(trawl_SS, FV_SS)
SS <- ggplot(data = ME_spring_SS, aes(x = factor(ME_spring_SS$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("ME-NH spring surface salinity") + labs(x = "Year", y = "Surface Salinity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(0,36))


#BS
trawl_BS <- subset(spring, select = c(YEAR, SALINITY))
trawl_BS <- cbind(trawl_BS, trawl)
colnames(trawl_BS) <- c("Year", "Temperature", "Source")
FV_BS <- subset(spring, select = c(YEAR, FV_bot_sal))
FV_BS <- cbind(FV_BS, FVCOM)
colnames(FV_BS) <- c("Year", "Temperature", "Source")
ME_spring_BS <- rbind(trawl_BS, FV_BS)
BS <- ggplot(data = ME_spring_BS, aes(x = factor(ME_spring_BS$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("ME-NH spring bottom salinity") + labs(x = "Year", y = "Bottom Salinity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(0,36))

ggarrange(ST, BT, SS, BS, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)


### GOM fall
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/GAMMs")
fall <- read.csv("GOM_fall_full.csv")

trawl_ST <- subset(fall, select = c(FV_year, SURFTEMP))
trawl <- rep("trawl", 5215)
trawl_ST <- cbind(trawl_ST, trawl)
colnames(trawl_ST) <- c("Year", "Temperature", "Source")
FV_ST <- subset(fall, select = c(FV_year, FV_surf_temp))
FVCOM <- rep("FVCOM", 5215)
FV_ST <- cbind(FV_ST, FVCOM)
colnames(FV_ST) <- c("Year", "Temperature", "Source")
GOM_fall <- rbind(trawl_ST, FV_ST)
ST <- ggplot(data = GOM_fall, aes(x = factor(GOM_fall$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("GOM fall surface temp") + labs(x = "Year", y = "Surface Temperature") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(4,16))

#BT
trawl_BT <- subset(fall, select = c(FV_year, BOTTEMP))
trawl_BT <- cbind(trawl_BT, trawl)
colnames(trawl_BT) <- c("Year", "Temperature", "Source")
FV_BT <- subset(fall, select = c(FV_year, FV_bot_temp))
FV_BT <- cbind(FV_BT, FVCOM)
colnames(FV_BT) <- c("Year", "Temperature", "Source")
GOM_fall_BT <- rbind(trawl_BT, FV_BT)
BT <- ggplot(data = GOM_fall_BT, aes(x = factor(GOM_fall_BT$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("GOM fall bottom temp") + labs(x = "Year", y = "Bottom Temperature") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(4,16))

trawl_SS <- subset(fall, select = c(FV_year, SURFSALIN))
trawl_SS <- cbind(trawl_SS, trawl)
colnames(trawl_SS) <- c("Year", "Temperature", "Source")
FV_SS <- subset(fall, select = c(FV_year, FV_surf_sal))
FV_SS <- cbind(FV_SS, FVCOM)
colnames(FV_SS) <- c("Year", "Temperature", "Source")
GOM_fall_SS <- rbind(trawl_SS, FV_SS)
SS <- ggplot(data = GOM_fall_SS, aes(x = factor(GOM_fall_SS$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("GOM fall surface salinity") + labs(x = "Year", y = "Surface Salinity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(29,36))


#BS
trawl_BS <- subset(fall, select = c(FV_year, BOTSALIN))
trawl_BS <- cbind(trawl_BS, trawl)
colnames(trawl_BS) <- c("Year", "Temperature", "Source")
FV_BS <- subset(fall, select = c(FV_year, FV_bot_sal))
FV_BS <- cbind(FV_BS, FVCOM)
colnames(FV_BS) <- c("Year", "Temperature", "Source")
GOM_fall_BS <- rbind(trawl_BS, FV_BS)
BS <- ggplot(data = GOM_fall_BS, aes(x = factor(GOM_fall_BS$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("GOM fall bottom salinity") + labs(x = "Year", y = "Bottom Salinity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(29,36))

ggarrange(ST, BT, SS, BS, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)


# GOM spring
spring <- read.csv("GOM_spring_full.csv")
trawl_ST <- subset(spring, select = c(FV_year,SURFTEMP))
trawl <- rep("trawl", 5041)
trawl_ST <- cbind(trawl_ST, trawl)
colnames(trawl_ST) <- c("Year", "Temperature", "Source")
FV_ST <- subset(spring, select = c(FV_year, FV_surf_temp))
FVCOM <- rep("FVCOM", 5041)
FV_ST <- cbind(FV_ST, FVCOM)
colnames(FV_ST) <- c("Year", "Temperature", "Source")
GOM_spring_ST <- rbind(trawl_ST, FV_ST)
ST <- ggplot(data = GOM_spring_ST, aes(x = factor(GOM_spring_ST$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("GOM spring surface temp") + labs(x = "Year", y = "Surface Temperature") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(3,16))

#BT
trawl_BT <- subset(spring, select = c(FV_year, BOTTEMP))
trawl_BT <- cbind(trawl_BT, trawl)
colnames(trawl_BT) <- c("Year", "Temperature", "Source")
FV_BT <- subset(spring, select = c(FV_year, FV_bot_temp))
FV_BT <- cbind(FV_BT, FVCOM)
colnames(FV_BT) <- c("Year", "Temperature", "Source")
GOM_spring_BT <- rbind(trawl_BT, FV_BT)
BT <- ggplot(data = GOM_spring_BT, aes(x = factor(GOM_spring_BT$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("GOM spring bottom temp") + labs(x = "Year", y = "Bottom Temperature") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(3,16))

trawl_SS <- subset(spring, select = c(FV_year, SURFSALIN))
trawl_SS <- cbind(trawl_SS, trawl)
colnames(trawl_SS) <- c("Year", "Temperature", "Source")
FV_SS <- subset(spring, select = c(FV_year, FV_surf_sal))
FV_SS <- cbind(FV_SS, FVCOM)
colnames(FV_SS) <- c("Year", "Temperature", "Source")
GOM_spring_SS <- rbind(trawl_SS, FV_SS)
SS <- ggplot(data = GOM_spring_SS, aes(x = factor(GOM_spring_SS$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("GOM spring surface salinity") + labs(x = "Year", y = "Surface Salinity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(29,36))


#BS
trawl_BS <- subset(spring, select = c(FV_year, BOTSALIN))
trawl_BS <- cbind(trawl_BS, trawl)
colnames(trawl_BS) <- c("Year", "Temperature", "Source")
FV_BS <- subset(spring, select = c(FV_year, FV_bot_sal))
FV_BS <- cbind(FV_BS, FVCOM)
colnames(FV_BS) <- c("Year", "Temperature", "Source")
GOM_spring_BS <- rbind(trawl_BS, FV_BS)
BS <- ggplot(data = GOM_spring_BS, aes(x = factor(GOM_spring_BS$Year), y = Temperature)) + 
  geom_boxplot(aes(fill = Source)) + ggtitle("GOM spring bottom salinity") + labs(x = "Year", y = "Bottom Salinity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(29,36))

ggarrange(ST, BT, SS, BS, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)



# MDMF fall
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/GAMMs")
MA_fall <- read.csv("MA_fall_full.csv")
plot(MA_fall$FV_surf_temp ~ MA_fall$FV_year, xlab = "Year", ylab = "Temperature", main = "MDMF fall ST")
boxplot(MA_fall$FV_surf_temp ~ MA_fall$FV_year, xlab = "Year", ylab = "Temperature", main = "MDMF fall ST")
plot(MA_fall$FV_bot_temp ~ MA_fall$FV_year, xlab = "Year", ylab = "Temperature", main = "MDMF fall BT")
plot(MA_fall$TEMP_C ~ MA_fall$FV_year)
plot(MA_fall$FV_surf_sal ~ MA_fall$FV_year, xlab = "Year", ylab = "Salinity", main = "MDMF fall SS")
plot(MA_fall$FV_bot_sal ~ MA_fall$FV_year, xlab = "Year", ylab = "Salinity", main = "MDMF fall BS")

MA_spring <- read.csv("MA_spring_full.csv")
plot(MA_spring$FV_surf_temp ~ MA_spring$FV_year, xlab = "Year", ylab = "Temperature", main = "MDMF spring ST")
plot(MA_spring$FV_bot_temp ~ MA_spring$FV_year, xlab = "Year", ylab = "Temperature", main = "MDMF spring BT")
plot(MA_spring$FV_surf_sal ~ MA_spring$FV_year, xlab = "Year", ylab = "Salinity", main = "MDMF spring SS")
plot(MA_spring$FV_bot_sal ~ MA_spring$FV_year, xlab = "Year", ylab = "Salinity", main = "MDMF spring BS")





####################### NEFSC trawl GAMMs
# Fall number of species 
setwd('C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/NEFSC')
library(mgcv)
library(MASS)
library(gamm4)

FL <- read.csv('nefsc_FL_by_tow.csv')
FL <- FL[FL$EST_YEAR >= 1992,]
#SP <- read.csv('nefsc_SP_by_tow.csv')
#SP <- SP[SP$EST_YEAR >= 1992,]
load("NEFSC_FL_taxinds_by_tow.RData")
FL_tax <- FL_tax[FL_tax$EST_YEAR > 1992,]


 
NEFSC_N_Fall <- gamm4(N_species ~ s(BOTTEMP) + s(SURFTEMP) + s(BOTSALIN) + s(SURFSALIN) +  
                    +  s(AVGDEPTH) + s(DECDEG_BEGLAT, DECDEG_BEGLON), random = ~ (1|EST_YEAR) , data = FL) 


summary(NEFSC_N_Fall$gam) # importance of terms 
print(NEFSC_N_Fall$gam) # edf; higher = more complex splines 

plot(NEFSC_N_Fall$gam)

# taxonomic indices
load("NEFSC_FL_taxinds_by_tow.RData")
write.csv(tax_by_tow, "tax_by_tow_FL.csv")
FL_tax <- read.csv("tax_by_tow_FL.csv")
FL_tax <- FL_tax[FL_tax$EST_YEAR > 1992,]


### AEW toy code testing ####
#setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant")
#library(mgcv)
### read in metrics/haul 
## as example say these are all from the same season 
#metric <- read.csv("map_index.csv")
#metric <- read.csv("test_gamm.csv") # more observations
### subset metrics by season (Fall/Spring)
#model <- gamm(H_index ~ s(Temp) + s(Year, bs = 're'), data = metric) 
#summary(model)
#plot(model)

