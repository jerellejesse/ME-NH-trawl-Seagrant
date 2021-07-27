####################################################################################
### Spatial maps of biodiversity metrics 
### UM Seagrant; climate impacts on Maine coastal fisheries using the ME-NH inshore trawl
### Objective 1 part 3
### 6/4/2018
### A.E. Weston
####################################################################################

### AEW for later use; canadian waters = strata 01351, 01350

####plotting indices spatially
#install.packages('rworldmap')

setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant")
#map_index <- read.csv("map_index.csv")
#library(rworldmap)
#vignette('rworldmap')
#newmap <- getMap(resolution = "li")
#plot(newmap, xlim = c(-73, -68), ylim = c(41.5, 45), asp = 1)
#points(map_index[,2], map_index[,1], col = "blue", cex = .6)
#mapBubbles(dF = map_index, nameX = "map_lon", nameY = "map_lat", nameZSize = "N_species", xlim = c(-73, -68), ylim = c(41.5, 45))

#coastline resolution does not look good


library('ggplot2')
library('ggthemes')
library('ggmap') 
library('maps')  # lots of geographic outlines
library('mapdata') # more higher-resolution outlines

# haul information for all years
map_index <- read.csv("map_index.csv")
usa <- map_data("usa")
region <- ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + coord_fixed(1.3)
gom <- region + coord_fixed(xlim = c(-72, -68),  ylim = c(41.5, 44.5), ratio = 1.3) # zoom in to gulf of maine
format <- gom + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # get rid of background lines
pts <- data.frame(long = map_index$map_lon, lat = map_index$map_lat, stringsAsFactors = FALSE) # read in coordinates of points
format + geom_point(data = pts, aes(x = long, y = lat), color = "light blue", size = map_index$N_species) + labs(x = "Longitude", y = "Latitude", title = "Number of species per haul", size = 'N_species')# add number of species/haul to map
#format + geom_point(data = pts, aes(x = long, y = lat), color = "light pink", size = map_index$H_index) + labs(x = "Longitude", y = "Latitude", size = 'N_species') # add Shannon-Weiner index/haul to mapxlab("Longitude")

# a different view 
world <- ggplot() + borders("world", colour = "gray85", fill = 'gray80') + theme_map()
zoom <- world + coord_fixed(xlim = c(-72, -68),  ylim = c(41.5, 44.5), ratio = 1.3)
zoom + geom_point(aes(x = map_lon, y = map_lat, size = N_species), data = map_index, colour = 'cyan', alpha = 0.5) + labs(size = 'N_species', xlab = 'Longitude', ylab = 'Latitude', title = "Number of species per haul") 


##### animate maps to change by year ####
#devtools::install_github("dgrtwo/gganimate")
#library(installr)
#install.ImageMagick()
#install.packages('magick')
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
map_index <- read.csv("ind_by_haul.csv")
library('ggplot2')
library('ggthemes')
library('ggmap') 
library('maps')  # lots of geographic outlines
library('mapdata') # more higher-resolution outlines
library('gganimate')

map_index <- map_index[20:40,]
anim <- ggplot(data = map_index, aes(x = START_LONGITUDE, y = START_LATITUDE, size = N_species, frame = X), alpha = 0.5) + 
  geom_point() + borders("world", colour = "gray85", fill = 'gray80') + theme_map() +
  coord_fixed(xlim = c(-72, -68),  ylim = c(41.5, 44.5), ratio = 1.3) +
  labs(size = 'N_species', xlab = 'Longitude', ylab = 'Latitude', title = "Number of species per haul") 

gganimate(anim, "species_per_haul.gif", saver = "gif")


anim <- ggplot(data = map_index, aes(x = START_LONGITUDE, y = START_LATITUDE, size = H_index, frame = X), alpha = 0.5) + 
  geom_point() + borders("world", colour = "gray85", fill = 'gray80') + theme_map() +
  coord_fixed(xlim = c(-72, -68),  ylim = c(41.5, 44.5), ratio = 1.3) +
  labs(size = 'H_index', xlab = 'Longitude', ylab = 'Latitude', title = "H_index by haul") 

gganimate(anim, "H_index_per_haul.gif", saver = "gif")





##### multipanel plots #### 
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
inds <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
fall <- inds[inds$SEASON == "FL",]
spring <- inds[inds$SEASON == "SP",]

plot(by_tow$DECDEG_BEGLAT ~ by_tow$DECDEG_BEGLON, main = "Stations sampled", ylab = "Latitude", xlab = "Longitude", pch = 20)
points(ma_spring_inds$LATITUDE ~ ma_spring_inds$LONGITUDE, col = 'pink', pch = 20)
points(fall$START_LATITUDE ~ fall$START_LONGITUDE, col = 'yellow', pch = 20)



######### plot indices over time spattially
#### reading in shape files
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Shape files/us_medium_shoreline")
library('rgdal')
library('ggplot2')
require('gridExtra')
US <- readOGR(dsn = ".", layer = "us_medium_shoreline") 
US_2 <- fortify(US) # convert to dataframe

### species richness/haul
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
ind_by_haul <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
sub_pts <- ind_by_haul[ind_by_haul$YEAR == 0 | ind_by_haul$YEAR == 01 | ind_by_haul$YEAR == 02,]

#layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))
#layout.show(6)
#par(mar = c(2, 4,1.5,0.5)) #B,L,T,R

pdf('Num_spp_4.pdf')
ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts$N_species/10, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "slateblue4") +
  labs(x = "Longitude", y = "Latitude", title = "2000-2002", size = 'N species') 


sub_pts_2 <- ind_by_haul[ind_by_haul$YEAR == 3 | ind_by_haul$YEAR == 04 | ind_by_haul$YEAR == 05,]
p2 <- ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_2, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_2$N_species/10, show.legend = FALSE) +
  scale_color_gradient(low = "gray88", high = "slateblue4") +
  labs(x = "Longitude", y = "Latitude", title = "2003-2005", size = 'N species') 

sub_pts_3 <- ind_by_haul[ind_by_haul$YEAR == 6 | ind_by_haul$YEAR == 7 | ind_by_haul$YEAR == 8,]
p3 <- ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_3, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_3$N_species/10, show.legend = FALSE) +
  scale_color_gradient(low = "gray88", high = "slateblue4") +
  labs(x = "Longitude", y = "Latitude", title = "2006-2008", size = 'N species') 


sub_pts_4 <- ind_by_haul[ind_by_haul$YEAR == 9 | ind_by_haul$YEAR == 10 | ind_by_haul$YEAR == 11,]
p4 <- ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_4, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_4$N_species/10, show.legend = FALSE) +
  scale_color_gradient(low = "gray88", high = "slateblue4") +
  labs(x = "Longitude", y = "Latitude", title = "2009-2011", size = 'N species') 

sub_pts_5 <- ind_by_haul[ind_by_haul$YEAR == 12 | ind_by_haul$YEAR == 13 | ind_by_haul$YEAR == 14,]
p5 <- ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_5, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_5$N_species/10, show.legend = FALSE) +
  scale_color_gradient(low = "gray88", high = "slateblue4") +
  labs(x = "Longitude", y = "Latitude", title = "2012-2014", size = 'N species') 

sub_pts_6 <- ind_by_haul[ind_by_haul$YEAR == 15 | ind_by_haul$YEAR == 16 | ind_by_haul$YEAR == 17,]
ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_6, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_6$N_species/12, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "slateblue4") +
  labs(x = "Longitude", y = "Latitude", title = "2015-2017", size = 'N species') 

#grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, widths = c(1,1))
#dev.off()

########  quicker plotting
library('ggplot2')
library('ggthemes')
library('ggmap') 
library('maps')  # lots of geographic outlines
library('mapdata') 
library('rgdal')
library('ggplot2')
require('gridExtra')
setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
ind_by_haul <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
sub_pts <- ind_by_haul[ind_by_haul$YEAR == 0 | ind_by_haul$YEAR == 01 | ind_by_haul$YEAR == 02,]
setwd('C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/Spatial plots/Multiplots')
pdf('me_nh_N_spp_2.pdf')
usa <- map_data("usa")
P1 <- ggplot() + 
  geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2),plot.margin=unit(c(1,1,-0.5,1), "cm")) +
  geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts$N_species/20, show.legend = FALSE) +
  scale_color_gradient(low = "gray88", high = "indianred4") +
  labs(x = "Longitude", y = "Latitude", title = "2000-2002", size = 'N species') 

sub_pts_2 <- ind_by_haul[ind_by_haul$YEAR == 3 | ind_by_haul$YEAR == 04 | ind_by_haul$YEAR == 05,]
P2 <- ggplot() + 
  geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2),plot.margin=unit(c(1,1,-0.5,1), "cm")) +
  geom_point(data = sub_pts_2, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_2$N_species/20, show.legend = FALSE) +
  scale_color_gradient(low = "gray88", high = "indianred4") +
  labs(x = "Longitude", y = "Latitude", title = "2003-2005", size = 'N species') 

sub_pts_3 <- ind_by_haul[ind_by_haul$YEAR == 6 | ind_by_haul$YEAR == 7 | ind_by_haul$YEAR == 8,]
P3 <- ggplot() + 
  geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2),plot.margin=unit(c(1,1,-0.5,1), "cm")) +
  geom_point(data = sub_pts_3, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_3$N_species/20, show.legend = FALSE) +
  scale_color_gradient(low = "gray88", high = "indianred4") +
  labs(x = "Longitude", y = "Latitude", title = "2006-2008", size = 'N species') 


sub_pts_4 <- ind_by_haul[ind_by_haul$YEAR == 9 | ind_by_haul$YEAR == 10 | ind_by_haul$YEAR == 11,]
P4 <- ggplot() + 
  geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2),plot.margin=unit(c(1,1,-0.5,1), "cm")) +
  geom_point(data = sub_pts_4, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_4$N_species/20, show.legend = FALSE) +
  scale_color_gradient(low = "gray88", high = "indianred4") +
  labs(x = "Longitude", y = "Latitude", title = "2009-2011", size = 'N species') 

sub_pts_5 <- ind_by_haul[ind_by_haul$YEAR == 12 | ind_by_haul$YEAR == 13 | ind_by_haul$YEAR == 14,]
P5 <- ggplot() + 
  geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2),plot.margin=unit(c(1,1,-0.5,1), "cm")) +
  geom_point(data = sub_pts_5, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_5$N_species/20, show.legend = FALSE) +
  scale_color_gradient(low = "gray88", high = "indianred4") +
  labs(x = "Longitude", y = "Latitude", title = "2012-2014", size = 'N species') 

sub_pts_6 <- ind_by_haul[ind_by_haul$YEAR == 15 | ind_by_haul$YEAR == 16 | ind_by_haul$YEAR == 17,]
P6 <- ggplot() + 
  geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),  plot.margin=unit(c(1,1,-0.5,1), "cm"),
        legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),legend.position = c(0.9, 0.4)) +
  geom_point(data = sub_pts_6, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_6$N_species/20, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "indianred4") +
  labs(x = "Longitude", y = "Latitude", title = "2015-2017", size = 'N species') 

grid.arrange(P1, P2, P3, P4, P5, P6, ncol = 2, widths = c(1,1))
dev.off()

######### heat maps
library('ggplot2')
library('ggthemes')
library('ggmap') 
library('maps')  # lots of geographic outlines
library('mapdata') 
library('rgdal')
library('ggplot2')
require('gridExtra')
require(reshape2)

setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
ind_by_haul <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
sub_pts <- ind_by_haul[ind_by_haul$YEAR == 0,]
setwd('C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/Spatial plots/Multiplots')
usa <- map_data("usa")


# points only
 ggplot(sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE)) + 
  geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(42, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  axis.line = element_line(colour = "white"), legend.position = c(0.9, 0.2),plot.margin=unit(c(1,1,-0.5,1), "cm")) +
  geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE), size = sub_pts$N_species/10, show.legend = TRUE) +
 labs(x = "Longitude", y = "Latitude", title = "Number of Species 2000", size = 'N species') 

 # points with density
 ggplot(sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE)) + 
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, na.rm = TRUE) +
  geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE), size = sub_pts$N_species/10, show.legend = TRUE) +
  labs(x = "Longitude", y = "Latitude", title = "Number of Species 2000", size = 'N species') +
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), legend.position = c(0.9, 0.2),plot.margin=unit(c(1,1,-0.5,1), "cm")) +
  geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black") 

 # show density of where points are 
 ggplot(sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE)) + 
   stat_density_2d( data = sub_pts, aes(fill = ..level..), geom = "polygon", bins = 10) +
   scale_fill_gradient(low = "gray88", high = "blue4")+
   #geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE), size = sub_pts$N_species, show.legend = TRUE) +
   labs(x = "Longitude", y = "Latitude", title = "Number of Species 2000", size = 'N species') +
   coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), legend.position = c(0.9, 0.2),plot.margin=unit(c(1,1,-0.5,1), "cm")) +
   geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black") 
 
 # contour needs all possible x,y,z combos
 
 ############# combo heat maps ##################
 #gridded N_species - does overlap coastline
 ind_by_haul_FL <- ind_by_haul[ind_by_haul$SEASON == 'FL',]
 ggplot(ind_by_haul_FL, aes(x = START_LONGITUDE, y = START_LATITUDE)) + 
   # change z depending on metric to be plotted
   stat_summary_2d(data = ind_by_haul_FL, aes(z = delta_var), fun = mean, bins = 200) + 
   scale_fill_gradient(low = "gray", high = "lightseagreen")+ #blue4 darkgreen violetred4 sienna3 royalblue4 goldenrod
   #geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE), size = sub_pts$N_species/10, show.legend = TRUE) +
   labs(x = "Longitude", y = "Latitude", title = "Fall Variation in Taxonomic Distinctness", size = 'N species') +
   coord_fixed(xlim = c(-71, -67),  ylim = c(42, 45), ratio = 1.3) +
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), legend.position = c(0.9, 0.05),plot.margin=unit(c(1,1,-0.5,1), "cm")) +
   geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black") +
   facet_wrap(~ YEAR, ncol = 5)
 
 ind_by_haul_SP <- ind_by_haul[ind_by_haul$SEASON == 'SP',]
 ggplot(ind_by_haul_SP, aes(x = START_LONGITUDE, y = START_LATITUDE)) + 
   stat_summary_2d(data = ind_by_haul_SP, aes(z = delta_var), fun = mean, bins = 200) +
   scale_fill_gradient(low = "gray", high = "lightseagreen")+
   #geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE), size = sub_pts$N_species/10, show.legend = TRUE) +
   labs(x = "Longitude", y = "Latitude", title = "Spring Variation in Taxonomic Distinctness", size = 'N species') +
   coord_fixed(xlim = c(-71, -67),  ylim = c(42, 45), ratio = 1.3) +
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), legend.position = c(0.9, 0.05),plot.margin=unit(c(1,1,-0.5,1), "cm")) +
   geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black") +
   facet_wrap(~ YEAR, ncol = 5)
 
 
 
 
 
 
 
######### Shannon-Weiner index ####
ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE, color = H_index), size = sub_pts$H_index, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "dark green") +
  labs(x = "Longitude", y = "Latitude", title = "Shannon-Weiner Index (2000-2002)", size = 'H_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_2, aes(x = START_LONGITUDE, y = START_LATITUDE, color = H_index), size = sub_pts_2$H_index, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "dark green") +
  labs(x = "Longitude", y = "Latitude", title = "Shannon-Weiner Index (2003-2005)", size = 'H_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9,0.2)) +
  geom_point(data = sub_pts_3, aes(x = START_LONGITUDE, y = START_LATITUDE, color = H_index), size = sub_pts_3$H_index, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "dark green") +
  labs(x = "Longitude", y = "Latitude", title = "Shannon-Weiner Index (2006-2008)", size = 'H_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9,0.2)) +
  geom_point(data = sub_pts_4, aes(x = START_LONGITUDE, y = START_LATITUDE, color = H_index), size = sub_pts_4$H_index, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "dark green") +
  labs(x = "Longitude", y = "Latitude", title = "Shannon-Weiner Index (2009-2011)", size = 'H_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_5, aes(x = START_LONGITUDE, y = START_LATITUDE, color = H_index), size = sub_pts_5$H_index, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "dark green") +
  labs(x = "Longitude", y = "Latitude", title = "Shannon-Weiner Index (2012-2014)", size = 'H_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_6, aes(x = START_LONGITUDE, y = START_LATITUDE, color = H_index), size = sub_pts_6$H_index, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "dark green") +
  labs(x = "Longitude", y = "Latitude", title = "Shannon-Weiner Index (2015-2017)", size = 'H_index') 


### Simpson's Diversity Index

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE, color = D_index), size = sub_pts$D_index, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "palevioletred4") +
  labs(x = "Longitude", y = "Latitude", title = "Simpson's Diversity Index (2000-2002)", size = 'D_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_2, aes(x = START_LONGITUDE, y = START_LATITUDE, color = D_index), size = sub_pts_2$D_index, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "palevioletred4") +
  labs(x = "Longitude", y = "Latitude", title = "Simpson's Diversity Index (2003-2005)", size = 'D_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9,0.2)) +
  geom_point(data = sub_pts_3, aes(x = START_LONGITUDE, y = START_LATITUDE, color = D_index), size = sub_pts_3$D_index, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "palevioletred4") +
  labs(x = "Longitude", y = "Latitude", title = "Simpson's Diversity Index (2006-2008)", size = 'D_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9,0.2)) +
  geom_point(data = sub_pts_4, aes(x = START_LONGITUDE, y = START_LATITUDE, color = D_index), size = sub_pts_4$D_index, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "palevioletred4") +
  labs(x = "Longitude", y = "Latitude", title = "Simpson's Diversity Index (2009-2011)", size = 'D_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_5, aes(x = START_LONGITUDE, y = START_LATITUDE, color = D_index), size = sub_pts_5$D_index, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "palevioletred4") +
  labs(x = "Longitude", y = "Latitude", title = "Simpson's Diversity Index (2012-2014)", size = 'D_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_6, aes(x = START_LONGITUDE, y = START_LATITUDE, color = D_index), size = sub_pts_6$D_index, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "palevioletred4") +
  labs(x = "Longitude", y = "Latitude", title = "Simpson's Diversity Index (2015-2017)", size = 'D_index') 


### Simpsons evenness index 
ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE, color = E_index), size = sub_pts$E_index*5, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "orchid4") +
  labs(x = "Longitude", y = "Latitude", title = "Simpson's Evenness Index (2000-2002)", size = 'E_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_2, aes(x = START_LONGITUDE, y = START_LATITUDE, color = E_index), size = sub_pts_2$E_index*5, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "orchid4") +
  labs(x = "Longitude", y = "Latitude", title = "Simpson's Evenness Index (2003-2005)", size = 'E_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9,0.2)) +
  geom_point(data = sub_pts_3, aes(x = START_LONGITUDE, y = START_LATITUDE, color = E_index), size = sub_pts_3$E_index*5, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "orchid4") +
  labs(x = "Longitude", y = "Latitude", title = "Simpson's Evenness Index (2006-2008)", size = 'E_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9,0.2)) +
  geom_point(data = sub_pts_4, aes(x = START_LONGITUDE, y = START_LATITUDE, color = E_index), size = sub_pts_4$E_index*5, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "orchid4") +
  labs(x = "Longitude", y = "Latitude", title = "Simpson's Evenness Index (2009-2011)", size = 'E_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_5, aes(x = START_LONGITUDE, y = START_LATITUDE, color = E_index), size = sub_pts_5$E_index*5, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "orchid4") +
  labs(x = "Longitude", y = "Latitude", title = "Simpson's Evenness Index (2012-2014)", size = 'E_index') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_6, aes(x = START_LONGITUDE, y = START_LATITUDE, color = E_index), size = sub_pts_6$E_index*5, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "orchid4") +
  labs(x = "Longitude", y = "Latitude", title = "Simpson's Evenness Index (2015-2017)", size = 'E_index') 



### taxonomic distinctness
ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_star), size = sub_pts$delta_star/2, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "darkorange3") +
  labs(x = "Longitude", y = "Latitude", title = "Taxonomic Distinctness (2000-2002)", size = 'delta_star') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_2, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_star), size = sub_pts_2$delta_star/2, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "darkorange3") +
  labs(x = "Longitude", y = "Latitude", title = "Taxonomic Distinctness (2003-2005)", size = 'delta_star') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9,0.2)) +
  geom_point(data = sub_pts_3, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_star), size = sub_pts_3$delta_star/2, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "darkorange3") +
  labs(x = "Longitude", y = "Latitude", title = "Taxonomic Distinctness (2006-2008)", size = 'delta_star') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9,0.2)) +
  geom_point(data = sub_pts_4, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_star), size = sub_pts_4$delta_star/2, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "darkorange3") +
  labs(x = "Longitude", y = "Latitude", title = "Taxonomic Distinctness (2009-2011)", size = 'delta_star') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_5, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_star), size = sub_pts_5$delta_star/2, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "darkorange3") +
  labs(x = "Longitude", y = "Latitude", title = "Taxonomic Distinctness (2012-2014)", size = 'delta_star') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_6, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_star), size = sub_pts_6$delta_star/2, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "darkorange3") +
  labs(x = "Longitude", y = "Latitude", title = "Taxonomic Distinctness (2015-2017)", size = 'delta_star') 


### average taxonomic distinctness
ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_plus), size = sub_pts$delta_plus/2, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "red3") +
  labs(x = "Longitude", y = "Latitude", title = "Average Taxonomic Distinctness (2000-2002)", size = 'delta_plus') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_2, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_plus), size = sub_pts_2$delta_plus/2, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "red3") +
  labs(x = "Longitude", y = "Latitude", title = "Average Taxonomic Distinctness (2003-2005)", size = 'delta_plus') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9,0.2)) +
  geom_point(data = sub_pts_3, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_plus), size = sub_pts_3$delta_plus/2, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "red3") +
  labs(x = "Longitude", y = "Latitude", title = "Average Taxonomic Distinctness (2006-2008)", size = 'delta_plus') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9,0.2)) +
  geom_point(data = sub_pts_4, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_plus), size = sub_pts_4$delta_plus/2, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "red3") +
  labs(x = "Longitude", y = "Latitude", title = "Average Taxonomic Distinctness (2009-2011)", size = 'delta_plus') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_5, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_plus), size = sub_pts_5$delta_plus/2, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "red3") +
  labs(x = "Longitude", y = "Latitude", title = "Average Taxonomic Distinctness (2012-2014)", size = 'delta_plus') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_6, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_plus), size = sub_pts_6$delta_plus/2, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "red3") +
  labs(x = "Longitude", y = "Latitude", title = "Average Taxonomic Distinctness (2015-2017)", size = 'delta_plus') 

### Variation in taxonomic distinctness
ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_var), size = sub_pts$delta_var, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "blue3") +
  labs(x = "Longitude", y = "Latitude", title = "Variation in Taxonomic Distinctness (2000-2002)", size = 'delta_var') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_2, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_var), size = sub_pts_2$delta_var, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "blue3") +
  labs(x = "Longitude", y = "Latitude", title = "Variation in Taxonomic Distinctness (2003-2005)", size = 'delta_var') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9,0.2)) +
  geom_point(data = sub_pts_3, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_var), size = sub_pts_3$delta_var, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "blue3") +
  labs(x = "Longitude", y = "Latitude", title = "Variation in Taxonomic Distinctness (2006-2008)", size = 'delta_var') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9,0.2)) +
  geom_point(data = sub_pts_4, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_var), size = sub_pts_4$delta_var, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "blue3") +
  labs(x = "Longitude", y = "Latitude", title = "Variation in Taxonomic Distinctness (2009-2011)", size = 'delta_var') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_5, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_var), size = sub_pts_5$delta_var, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "blue3") +
  labs(x = "Longitude", y = "Latitude", title = "Variation in Taxonomic Distinctness (2012-2014)", size = 'delta_var') 

ggplot() + 
  geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_6, aes(x = START_LONGITUDE, y = START_LATITUDE, color = delta_var), size = sub_pts_6$delta_var, show.legend = TRUE) +
  scale_color_gradient(low = "gray88", high = "blue3") +
  labs(x = "Longitude", y = "Latitude", title = "Variation in Taxonomic Distinctness (2015-2017)", size = 'delta_var') 



#############################################################################################

setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
ind_by_haul <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
library(tidyr)
library(SDMTools)


color <- colorRampPalette(c("gray88", "black"))
col  <- color(20)
plot(US, xlim = c(-71, -67), ylim = c(41.5, 45), asp = 1, main = "Species Richness")
points(sub_pts$START_LONGITUDE, sub_pts$START_LATITUDE, cex = sub_pts$N_species/10, col = 'navy blue')

points(ind_by_haul$START_LONGITUDE, ind_by_haul$START_LATITUDE, cex = ind_by_haul$N_species/10, col = col[ind_by_haul$YEAR])
#legend(bottomright, bty = 'n', pt.cex = ind_by_haul$N_species/10, legend = )



color <- colorRampPalette(c("gray88", "dark blue"))
col  <- color(20)
plot(US, xlim = c(-71, -67), ylim = c(41.5, 45), asp = 1, main = "Shannon-Weiner Index")
points(ind_by_haul[ind_by_haul$YEAR == 0, ind_by_haul$START_LONGITUDE], 
       ind_by_haul[ind_by_haul$YEAR == 0, ind_by_haul$START_LATITUDE], 
       cex = ind_by_haul[ind_by_haul$YEAR == 0, ind_by_haul$H_index], col = "dark blue") ###AEW 
#legend(bottomright, bty = 'n', legend = )



color <- colorRampPalette(c("gray88", "dark green"))
col  <- color(20)
plot(US, xlim = c(-71, -67), ylim = c(41.5, 45), asp = 1, main = "Simpson's Diversity Index")
points(ind_by_haul$START_LONGITUDE, ind_by_haul$START_LATITUDE, cex = ind_by_haul$D_index/3, col = col[ind_by_haul$YEAR])



color <- colorRampPalette(c("gray88","violetred4"))
col  <- color(20)
plot(US, xlim = c(-71, -67), ylim = c(41.5, 45), asp = 1, main = "Simpson's Evenness Index")
points(ind_by_haul$START_LONGITUDE, ind_by_haul$START_LATITUDE, cex = ind_by_haul$E_index*2, col = col[ind_by_haul$YEAR])


color <- colorRampPalette(c("gray88","purple3"))
col  <- color(20)
plot(US, xlim = c(-71, -67), ylim = c(41.5, 45), asp = 1, main = "Taxonomic Distinctness")
points(ind_by_haul$START_LONGITUDE, ind_by_haul$START_LATITUDE, cex = ind_by_haul$delta_star/2, col = col[ind_by_haul$YEAR])


color <- colorRampPalette(c("gray88","steelblue"))
col  <- color(20)
plot(US, xlim = c(-71, -67), ylim = c(41.5, 45), asp = 1, main = "Average Taxonomic Distinctness")
points(ind_by_haul$START_LONGITUDE, ind_by_haul$START_LATITUDE, cex = ind_by_haul$delta_plus/2, col = col[ind_by_haul$YEAR])


color <- colorRampPalette(c("gray88","darkorange1"))vvxcvxv
col  <- color(20)
plot(US, xlim = c(-71, -67), ylim = c(41.5, 45), asp = 1, main = "Variation in Taxonomic Distinctness")
points(ind_by_haul$START_LONGITUDE, ind_by_haul$START_LATITUDE, cex = ind_by_haul$delta_var/2, col = col[ind_by_haul$YEAR])

######################### dev testing for N_species plots 
library('ggplot2')
library('ggthemes')
library('ggmap') 
library('maps')  # lots of geographic outlines
library('mapdata') # more higher-resolution outlines
require('gridExtra')


setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results")
ind_by_haul <- read.csv("diversity_ind_by_haul.csv", header = TRUE)
sub_pts <- ind_by_haul[ind_by_haul$YEAR == 0 | ind_by_haul$YEAR == 01 | ind_by_haul$YEAR == 02,]

# haul information for all years
pdf('Num_spp_over_time.pdf')

usa <- map_data("usa")
p1 <- ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + coord_fixed(1.3) +
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) + # zoom in to gulf of maine
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts$N_species/12*2, show.legend = TRUE) +
scale_color_gradient(low = "gray88", high = "darkblue") +
labs(x = "Longitude", y = "Latitude", title = "2000-2002", size = 'N species') 

 sub_pts_2 <- ind_by_haul[ind_by_haul$YEAR == 3 | ind_by_haul$YEAR == 04 | ind_by_haul$YEAR == 05,]
p2 <- ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + coord_fixed(1.3) +
   coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) + # zoom in to gulf of maine
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
   geom_point(data = sub_pts_2, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_2$N_species/12*2, show.legend = TRUE) +
   scale_color_gradient(low = "gray88", high = "darkblue") +
   labs(x = "Longitude", y = "Latitude", title = "2003-2005", size = 'N species') 
 
 sub_pts_3 <- ind_by_haul[ind_by_haul$YEAR == 6 | ind_by_haul$YEAR == 7 | ind_by_haul$YEAR == 8,]
p3 <- ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + coord_fixed(1.3) +
   coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) + # zoom in to gulf of maine
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
   geom_point(data = sub_pts_3, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_3$N_species/12*2, show.legend = TRUE) +
   scale_color_gradient(low = "gray88", high = "darkblue") +
   labs(x = "Longitude", y = "Latitude", title = "2006-2008", size = 'N species') 
 
 
 sub_pts_4 <- ind_by_haul[ind_by_haul$YEAR == 9 | ind_by_haul$YEAR == 10 | ind_by_haul$YEAR == 11,]
p4 <- ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + coord_fixed(1.3) +
   coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) + # zoom in to gulf of maine
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
   geom_point(data = sub_pts_4, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_4$N_species/12*2, show.legend = TRUE) +
   scale_color_gradient(low = "gray88", high = "darkblue") +
   labs(x = "Longitude", y = "Latitude", title = "2009-2011", size = 'N species') 
 
 sub_pts_5 <- ind_by_haul[ind_by_haul$YEAR == 12 | ind_by_haul$YEAR == 13 | ind_by_haul$YEAR == 14,]
 
p5 <- ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + coord_fixed(1.3) +
   coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) + # zoom in to gulf of maine
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
   geom_point(data = sub_pts_5, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_5$N_species/12*2, show.legend = TRUE) +
   scale_color_gradient(low = "gray88", high = "darkblue") +
   labs(x = "Longitude", y = "Latitude", title = "2012-2014", size = 'N species') 
 
 sub_pts_6 <- ind_by_haul[ind_by_haul$YEAR == 15 | ind_by_haul$YEAR == 16 | ind_by_haul$YEAR == 17,]
p6 <- ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + coord_fixed(1.3) +
   coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) + # zoom in to gulf of maine
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
   geom_point(data = sub_pts_6, aes(x = START_LONGITUDE, y = START_LATITUDE, color = N_species), size = sub_pts_6$N_species/12*2, show.legend = TRUE) +
   scale_color_gradient(low = "gray88", high = "darkblue") +
   labs(x = "Longitude", y = "Latitude", title = "2015-2017", size = 'N species') 
 
 dev.off()
 
 
 
 pdf("H_index_over_time.pdf")
 
 ggplot() + 
ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + coord_fixed(1.3) +
   coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) + # zoom in to gulf of maine
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
   geom_point(data = sub_pts, aes(x = START_LONGITUDE, y = START_LATITUDE, color = H_index), size = sub_pts$H_index, show.legend = TRUE) +
   scale_color_gradient(low = "gray88", high = "dark green") +
   labs(x = "Longitude", y = "Latitude", title = "Shannon-Weiner Index (2000-2002)", size = 'H_index') 
 
ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + coord_fixed(1.3) +
   coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) + # zoom in to gulf of maine
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
   geom_point(data = sub_pts_2, aes(x = START_LONGITUDE, y = START_LATITUDE, color = H_index), size = sub_pts_2$H_index, show.legend = TRUE) +
   scale_color_gradient(low = "gray88", high = "dark green") +
   labs(x = "Longitude", y = "Latitude", title = "Shannon-Weiner Index (2003-2005)", size = 'H_index') 
 
ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + coord_fixed(1.3) +
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) + # zoom in to gulf of maine
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_3, aes(x = START_LONGITUDE, y = START_LATITUDE, color = H_index), size = sub_pts_3$H_index, show.legend = TRUE) +
   scale_color_gradient(low = "gray88", high = "dark green") +
   labs(x = "Longitude", y = "Latitude", title = "Shannon-Weiner Index (2006-2008)", size = 'H_index') 
 
ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + coord_fixed(1.3) +
  coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) + # zoom in to gulf of maine
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
  geom_point(data = sub_pts_4, aes(x = START_LONGITUDE, y = START_LATITUDE, color = H_index), size = sub_pts_4$H_index, show.legend = TRUE) +
   scale_color_gradient(low = "gray88", high = "dark green") +
   labs(x = "Longitude", y = "Latitude", title = "Shannon-Weiner Index (2009-2011)", size = 'H_index') 
 
 ggplot() + 
   geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
   coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
   geom_point(data = sub_pts_5, aes(x = START_LONGITUDE, y = START_LATITUDE, color = H_index), size = sub_pts_5$H_index, show.legend = TRUE) +
   scale_color_gradient(low = "gray88", high = "dark green") +
   labs(x = "Longitude", y = "Latitude", title = "Shannon-Weiner Index (2012-2014)", size = 'H_index') 
 
 ggplot() + 
   geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
   coord_fixed(xlim = c(-71, -67),  ylim = c(41.5, 45), ratio = 1.3) +
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.2)) +
   geom_point(data = sub_pts_6, aes(x = START_LONGITUDE, y = START_LATITUDE, color = H_index), size = sub_pts_6$H_index, show.legend = TRUE) +
   scale_color_gradient(low = "gray88", high = "dark green") +
   labs(x = "Longitude", y = "Latitude", title = "Shannon-Weiner Index (2015-2017)", size = 'H_index') 
 
 dev.off()
 

# plot surveys spatially #### 
 setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Shape files/us_medium_shoreline")
 library('rgdal')
 library('ggplot2')
 require('gridExtra')
 US <- readOGR(dsn = ".", layer = "us_medium_shoreline") 
 US_2 <- fortify(US) # convert to dataframe
 
 ### species richness/haul
 setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/GAMMs")
 ME <- read.csv("ME_NH_fall_full.csv")
 setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/MDMF")
 MA_SP <- read.csv("MDMF_spring_div_ind_by_tow.csv") 
 MA_FL <- read.csv("MDMF_fall_div_ind_by_tow.csv") 
 
 setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/NEFSC")
 NEFSC <- read.csv("nefsc_FL_by_tow.csv", header = TRUE)
 can <- c(1351, 1350, 1310, 1320, 1330, 1410,1420, 1490, 1990)
 FL_other <- unique(NEFSC$STRATUM[NEFSC$STRATUM > 3990]) #South of hatteras and scotian shelf 
 FL_remove <- c(can, FL_other)
 NEFSC <- NEFSC[!NEFSC$STRATUM %in% FL_remove,]
 
 library(ggplot2)
 ggplot() + 
   coord_fixed(xlim = c(-77.5, -63),  ylim = c(33.5, 45), ratio = 1.3) +
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"), text = element_text(size = 15)) +
   geom_point(data = NEFSC, aes(x = DECDEG_BEGLON, y = DECDEG_BEGLAT), color = "slategray", alpha = 0.2) + 
   geom_point(data = ME, aes(x = START_LONGITUDE, y = START_LATITUDE), color = "slateblue", alpha = 0.2) +
   geom_point(data = MA_SP, aes(x = LONGITUDE, y = LATITUDE), color = "skyblue3", alpha = 0.2) +
   geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
   labs(x = "Longitude", y = "Latitude", title = "Survey Spatial Coverage") 
 setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/Figures")
 ggsave("surv_spatial_coverage.jpg")
 
 
 ggplot() + 
   coord_fixed(xlim = c(-71, -65.5),  ylim = c(40.5, 45), ratio = 1.3) +
   theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"), text = element_text(size = 15)) +
   geom_point(data = NEFSC, aes(x = DECDEG_BEGLON, y = DECDEG_BEGLAT), color = "slategray", alpha = 0.2) + 
   geom_point(data = ME, aes(x = START_LONGITUDE, y = START_LATITUDE), color = "slateblue", alpha = 0.2) +
   geom_point(data = MA_SP, aes(x = LONGITUDE, y = LATITUDE), color = "skyblue3", alpha = 0.2) +
   geom_path(data = US_2, aes(x = long, y = lat, group = group), color = "black") + 
   labs(x = "Longitude", y = "Latitude", title = "Survey Spatial Coverage") 
 setwd("C:/Users/aweston/OneDrive - Gulf of Maine Research Institute/Seagrant/Results/Figures")
 ggsave("surv_spatial_coverage_GOM.jpg")
 
 
 
 ##### timelines ####
ME_T <- seq(2000, 2017)
MA_T <- seq(1978, 2017) 
NE_T <- seq(1963, 2017)

ggplot() +
  theme_bw() +
  ylim(0.5, 3.5) + xlim(1962, 2018) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none', plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm") ,
        axis.line = element_line(colour = "black"), text = element_text(size = 25),axis.text.y = element_blank(), axis.title.y=element_blank()) +
geom_segment(aes(x = 1963, y = 1, xend = 2017, yend = 1, size = 2), color = "slategray") +
geom_segment(aes(x = 1978, y = 2, xend = 2017, yend = 2, size = 2), color = "skyblue3") +
geom_segment(aes(x = 2000, y = 3, xend = 2017, yend = 3, size = 2), color = "slateblue") +
labs(x = "Year", title = "Survey Durations") 
ggsave("surv_yrs.jpg")


 
 