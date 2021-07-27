#Seagrant ME-NH trawl project 
#J.Jesse
#March 2021
#objective 2: functional groups
#Nonmetric multidimensional scaling 

#set directory
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/Objective 2")

#load packages
library(tidyverse)
library(vegan)
library(ggrepel)
library(ggforce)
library(ggnewscale)

#read in data (functional groups were added in Biodiversity_metrics_groups.R)
trawl_data<-read.csv("ME_trawl_NMDS_data.csv")[,c(2:17, 20)]

#arrange data for NMDS in vegan
#sample rows-> year, stratum (do we also want plots with region?)
#functional groups columns-> abundance of each group, characteristics of the community from that sample/site

######Year and Stratum and Region NMDS #####
trawl_data_arrange<-filter(trawl_data,W_WT !=0)%>%
  group_by(STRATUM,REGION,YEAR, functional_group)%>%
  summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
  filter(functional_group!="")%>%
  spread(functional_group, wt_kg)
trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:7])
ME_group_data<-trawl_data_arrange[, c(1:3)]


#abundance instead of biomass
trawl_data_arrange<-filter(trawl_data,W_WT !=0)%>%
  group_by(STRATUM,REGION,YEAR, functional_group)%>%
  summarise(num=mean(W_NUM, na.rm=TRUE))%>%
  filter(functional_group!="")%>%
  spread(functional_group, num)
trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:7])
ME_group_data<-trawl_data_arrange[, c(1:3)]

#FALL NMDS
# trawl_data_arrange<-filter(trawl_data,W_WT !=0)%>%
#   filter(SEASON=='FL')%>%
#   group_by(STRATUM,REGION,YEAR, functional_group)%>%
#   summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
#   filter(functional_group!="")%>%
#   spread(functional_group, wt_kg)
# 
# trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
# ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:7])
# 
# ME_group_data<-trawl_data_arrange[, c(1:3)]

#SPRING NMDS
# trawl_data_arrange<-filter(trawl_data,W_WT !=0)%>%
#   filter(SEASON=='SP')%>%
#   group_by(STRATUM,REGION,YEAR, functional_group)%>%
#   summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
#   filter(functional_group!="")%>%
#   spread(functional_group, wt_kg)
# 
# trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
# ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:7])
# # ME_group_data<-trawl_data_arrange[, c(1:3)]


#run the NMDS
ME_NMDS=metaMDS(ME_NMDS_data, # Our community-by-functional group matrix
                k=2, # The number of reduced dimensions
                trymax=200) #increase iterations

#plot the results
plot(ME_NMDS)
ordiplot(ME_NMDS)
orditorp(ME_NMDS,display="species",col="red",air=0.01)
orditorp(ME_NMDS,display="sites",cex=1.25,air=0.01)

#extract NMDS scores for ggplot
data.scores = as.data.frame(scores(ME_NMDS))
#add columns to data frame 
data.scores$Stratum = trawl_data_arrange$STRATUM
data.scores$Region = trawl_data_arrange$REGION
data.scores$Year = trawl_data_arrange$YEAR

#make hulls for the plot (discovered later that ggforce can do this within plotting code)
region.1 <- data.scores[data.scores$Region == 1, ][chull(data.scores[data.scores$Region == 1, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
region.2 <- data.scores[data.scores$Region == 2, ][chull(data.scores[data.scores$Region == 2, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
region.3 <- data.scores[data.scores$Region == 3, ][chull(data.scores[data.scores$Region == 3, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
region.4 <- data.scores[data.scores$Region == 4, ][chull(data.scores[data.scores$Region == 4, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
region.5 <- data.scores[data.scores$Region == 5, ][chull(data.scores[data.scores$Region == 5, c("NMDS1", "NMDS2")]), ]  # hull values for grp A

hull.data_region <- rbind(region.1, region.2, region.3, region.3, region.4, region.5)  #combine

stratum.1 <- data.scores[data.scores$Stratum == 1, ][chull(data.scores[data.scores$Stratum == 1, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
stratum.2 <- data.scores[data.scores$Stratum == 2, ][chull(data.scores[data.scores$Stratum == 2, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
stratum.3 <- data.scores[data.scores$Stratum == 3, ][chull(data.scores[data.scores$Stratum == 3, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
stratum.4 <- data.scores[data.scores$Stratum == 4, ][chull(data.scores[data.scores$Stratum == 4, c("NMDS1", "NMDS2")]), ]  # hull values for grp A

hull.data_stratum <- rbind(stratum.1, stratum.2, stratum.3, stratum.3, stratum.4)  #combine


year.0<-data.scores[data.scores$Year == 0, ][chull(data.scores[data.scores$Year ==0, c("NMDS1", "NMDS2")]), ] 
year.1<-data.scores[data.scores$Year == 1, ][chull(data.scores[data.scores$Year ==1, c("NMDS1", "NMDS2")]), ] 
year.2<-data.scores[data.scores$Year == 2, ][chull(data.scores[data.scores$Year ==2, c("NMDS1", "NMDS2")]), ] 
year.3<-data.scores[data.scores$Year == 3, ][chull(data.scores[data.scores$Year ==3, c("NMDS1", "NMDS2")]), ] 
year.4<-data.scores[data.scores$Year == 4, ][chull(data.scores[data.scores$Year ==4, c("NMDS1", "NMDS2")]), ] 
year.5<-data.scores[data.scores$Year == 5, ][chull(data.scores[data.scores$Year ==5, c("NMDS1", "NMDS2")]), ] 
year.6<-data.scores[data.scores$Year == 6, ][chull(data.scores[data.scores$Year ==6, c("NMDS1", "NMDS2")]), ] 
year.7<-data.scores[data.scores$Year == 7, ][chull(data.scores[data.scores$Year ==7, c("NMDS1", "NMDS2")]), ] 
year.8<-data.scores[data.scores$Year == 8, ][chull(data.scores[data.scores$Year ==8, c("NMDS1", "NMDS2")]), ] 
year.9<-data.scores[data.scores$Year == 9, ][chull(data.scores[data.scores$Year ==9, c("NMDS1", "NMDS2")]), ] 
year.10<-data.scores[data.scores$Year == 10, ][chull(data.scores[data.scores$Year ==10, c("NMDS1", "NMDS2")]), ] 
year.11<-data.scores[data.scores$Year == 11, ][chull(data.scores[data.scores$Year ==11, c("NMDS1", "NMDS2")]), ] 
year.12<-data.scores[data.scores$Year == 12, ][chull(data.scores[data.scores$Year ==12, c("NMDS1", "NMDS2")]), ] 
year.13<-data.scores[data.scores$Year == 13, ][chull(data.scores[data.scores$Year ==13, c("NMDS1", "NMDS2")]), ] 
year.14<-data.scores[data.scores$Year == 14, ][chull(data.scores[data.scores$Year ==14, c("NMDS1", "NMDS2")]), ] 
year.15<-data.scores[data.scores$Year == 15, ][chull(data.scores[data.scores$Year ==15, c("NMDS1", "NMDS2")]), ] 
year.16<-data.scores[data.scores$Year == 16, ][chull(data.scores[data.scores$Year ==16, c("NMDS1", "NMDS2")]), ] 
year.17<-data.scores[data.scores$Year == 17, ][chull(data.scores[data.scores$Year ==17, c("NMDS1", "NMDS2")]), ] 

hull.data.year<-rbind(year.0, year.1, year.2, year.3, year.4, year.5, year.6, year.7, year.8, year.9, year.10, year.11, year.12, year.13, year.14, year.15, year.16, year.17)

species.scores <- as.data.frame(scores(ME_NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)

#plot NMDS for REgion
p1 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( colour = factor(Region), size=1))+ 
  geom_polygon(data=hull.data_region, aes(x=NMDS1,y=NMDS2, fill=factor(Region), colour=factor(Region)), alpha=0.3, show.legend = F)+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Stratum")+
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))

#plot for stratum
p2 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( colour = as.factor(Stratum), size=1))+ 
  geom_polygon(data=hull.data_stratum, aes(x=NMDS1,y=NMDS2, colour=factor(Stratum), fill=factor(Stratum)), alpha=0.3, show.legend = F)+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Stratum", y = "NMDS2", shape = "Region")+
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))

#plot for year
p3 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( colour = Year, size=1))+ 
  geom_polygon(data=hull.data_stratum, aes(x=NMDS1,y=NMDS2, colour=Year, fill=Year, group=Year), alpha=0.3, show.legend = F)+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Year", y = "NMDS2", shape = "Region")+
  scale_colour_gradient(name="Year",low="light blue", high="navy")+
  scale_fill_gradient(low="light blue",high="navy")

#ellipse region plot
p4 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = factor(Region), size=1))+ 
  geom_mark_ellipse(aes(fill=factor(Region) ,label=factor(Region), color=factor(Region)), size=1, alpha=0.1)+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Year", fill="Region") +
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))

#ellipse stratum plot
p5 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = factor(Stratum), size=1))+ 
  geom_mark_ellipse(aes(fill=factor(Stratum),label=factor(Stratum), color=factor(Stratum)), size=1, alpha=0.2)+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Stratum", y = "NMDS2", shape = "Year", fill="Stratum") +
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))

#ellipse year plot
p6 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = factor(Year), size=1))+ 
  geom_mark_ellipse(aes( fill=factor(Year),color=factor(Year)), size=1, alpha=0.1)+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Year", y = "NMDS2", shape = "Year", fill="Year") +
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=18)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=18)))

#ellipse region and year plot
p7 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 3, aes(shape=Year), color="black")+ 
  scale_shape_binned(breaks = c(6, 12, 18))+
  guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Region), label=factor(Region)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold", color="#8c8c8c")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Year", fill="Region")

    
#ellipse stratum and year plot
p8 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 3, aes(shape=Year), color="black")+ 
  scale_shape_binned(breaks = c(6, 12, 18))+
  guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Stratum), label=factor(Stratum)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold", color="#8c8c8c")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Stratum", y = "NMDS2", shape = "Year", fill="Stratum")
   
#ellipse stratum and region plot
p9 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape=factor(Stratum), colour = Region, size=1))+ 
  scale_colour_gradient(name = "Region", low = "light grey", high = "black")+
  ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(fill=factor(Stratum),color=factor(Stratum), label=factor(Stratum)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Stratum", y = "NMDS2", shape = "Stratum", fill="Stratum") 
  
#ellipse stratum and region plot #2
p10 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape=factor(Region), colour = Stratum, size=1))+ 
  scale_colour_gradient(name = "Region", low = "light grey", high = "black")+
  ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(fill=factor(Region),color=factor(Region)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Region", fill="Region") 

#####analysis of similarity test######
ano_stratum<- anosim(ME_NMDS_data,trawl_data_arrange$STRATUM, distance = "bray", permutations = 999)
summary(ano_stratum) #stratum are statistically different 
plot(ano_stratum)

ano_region<- anosim(ME_NMDS_data, trawl_data_arrange$REGION, distance = "bray", permutations = 999)
ano_region #regions are statistically different 
plot(ano_region)

ano_year<- anosim(ME_NMDS_data, trawl_data_arrange$YEAR, distance = "bray", permutations = 999)
ano_year #years are statistically different 
plot(ano_year)


#####analysis of variance test#####
adonis_year<-adonis(ME_NMDS_data~YEAR+STRATUM+REGION, ME_group_data, distance="bray", permutations = 9999)
adonis_year

#use this if you don't have a balanced design- order doesn't matter for terms
adonis2(ME_NMDS_data~YEAR+STRATUM+REGION, ME_group_data, distance="bray", permutations = 9999 ,by="margin")

#summarized data instead of each individual haul
######Year only NMDS #####
trawl_data_arrange_year<-filter(trawl_data,W_WT !=0)%>%
  group_by(YEAR, functional_group)%>%
  summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
  filter(functional_group!="")%>%
  spread(functional_group, wt_kg)

ME_NMDS_data_year<-as.matrix(trawl_data_arrange_year[,2:5])

#run the NMDS
set.seed(123)
ME_NMDS_year<-metaMDS(ME_NMDS_data_year, # Our community-by-species matrix
                k=2) # The number of reduced dimensions

#plot the results
plot(ME_NMDS_year)
ordiplot(ME_NMDS_year)
orditorp(ME_NMDS_year,display="species",col="red",air=0.01)
orditorp(ME_NMDS_year,display="sites",cex=1.25,air=0.01)

#extract NMDS scores for ggplot
data.scores_year<- as.data.frame(scores(ME_NMDS_year))
#add columns to data frame 
data.scores_year$Year = trawl_data_arrange_year$YEAR

species.scores <- as.data.frame(scores(ME_NMDS_year, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)

#plot NMDS for Year communities
plot_year <- ggplot(data.scores_year, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Year, size=1))+ 
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Year", y = "NMDS2") +
  scale_colour_gradient(name = "Year", 
      low = "light blue", high = "navy", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))


#stratum and region not enough data for NMDS to converge
#stress is nearly zero= insufficient data


###### Year and stratum NMDS #####
trawl_data_arrange<-filter(trawl_data,W_WT !=0)%>%
  group_by(YEAR,STRATUM, functional_group)%>%
  summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
  filter(functional_group!="")%>%
  spread(functional_group, wt_kg)

#replace NAs with zero, only occurs for benthos group
trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0

ME_NMDS_data<-as.matrix(trawl_data_arrange[,3:6])

#run the NMDS
set.seed(123)
ME_NMDS=metaMDS(ME_NMDS_data, # Our community-by-species matrix
                     k=2) # The number of reduced dimensions

#plot the results
plot(ME_NMDS)
ordiplot(ME_NMDS)
orditorp(ME_NMDS,display="species",col="red",air=0.01)
orditorp(ME_NMDS,display="sites",cex=1.25,air=0.01)

#extract NMDS scores for ggplot
data.scores = as.data.frame(scores(ME_NMDS))
#add columns to data frame 
data.scores$Year = trawl_data_arrange$YEAR
data.scores$Stratum = trawl_data_arrange$STRATUM

species.scores <- as.data.frame(scores(ME_NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)

#make hulls for the plot
stratum.1 <- data.scores[data.scores$Stratum == 1, ][chull(data.scores[data.scores$Stratum == 1, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
stratum.2 <- data.scores[data.scores$Stratum == 2, ][chull(data.scores[data.scores$Stratum == 2, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
stratum.3 <- data.scores[data.scores$Stratum == 3, ][chull(data.scores[data.scores$Stratum == 3, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
stratum.4 <- data.scores[data.scores$Stratum == 4, ][chull(data.scores[data.scores$Stratum == 4, c("NMDS1", "NMDS2")]), ]  # hull values for grp A

hull.data <- rbind(stratum.1, stratum.2, stratum.3, stratum.3, stratum.4)  #combine

#plot NMDS for Year/stratum communities
p1 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( colour = as.factor(Stratum), size=1))+ 
  geom_polygon(data=hull.data, aes(x=NMDS1,y=NMDS2, fill=factor(Stratum), colour=factor(Stratum)), alpha=0.3, show.legend = F)+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), fontface="bold", size=10)+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
        labs(x = "NMDS1", colour = "Stratum", y = "NMDS2") +
        scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
        scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))#
       

p2<-ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape=factor(Stratum),colour=Year, size=1))+ 
  geom_polygon(data=hull.data, aes(x=NMDS1,y=NMDS2, fill=factor(Stratum),colour=Stratum, group=factor(Stratum)), alpha=0.3, show.legend = T)+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), fontface="bold", size=10)+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Year",fill="Stratum",shape="Stratum", y = "NMDS2") +
  scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))

#ellipse stratum plot
p3 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( colour = factor(Stratum), size=1))+ 
  geom_mark_ellipse(aes(fill=factor(Stratum), label=factor(Stratum), color=factor(Stratum)))+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Stratum", y = "NMDS2", shape = "Year", fill="Stratum") +
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))

#ellipse stratum and year plot
p4<- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape=Year, size=1), color="grey")+ 
  scale_shape_binned(breaks = c(6, 12, 18))+
  guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Stratum), label=factor(Stratum)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Stratum", y = "NMDS2", shape = "Year", fill="Stratum")

#analysis of similarity test
ano<- anosim(ME_NMDS_data, trawl_data_arrange$STRATUM, distance = "bray", permutations = 9999)
ano

ano<- anosim(ME_NMDS_data, trawl_data_arrange$YEAR, distance = "bray", permutations = 9999)
ano


###### Year and Region NMDS #####
trawl_data_arrange<-filter(trawl_data,W_WT !=0)%>%
  group_by(YEAR,REGION, functional_group)%>%
  summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
  filter(functional_group!="")%>%
  spread(functional_group, wt_kg)


ME_NMDS_data<-as.matrix(trawl_data_arrange[,3:6])

#run the NMDS
set.seed(123)
ME_NMDS=metaMDS(ME_NMDS_data, # Our community-by-species matrix
                k=2) # The number of reduced dimensions

#plot the results
plot(ME_NMDS)
ordiplot(ME_NMDS)
orditorp(ME_NMDS,display="species",col="red",air=0.01)
orditorp(ME_NMDS,display="sites",cex=1.25,air=0.01)

#extract NMDS scores for ggplot
data.scores = as.data.frame(scores(ME_NMDS))
#add columns to data frame 
data.scores$Year = trawl_data_arrange$YEAR
data.scores$Region = trawl_data_arrange$REGION

#make hulls for the plot
region.1 <- data.scores[data.scores$Region == 1, ][chull(data.scores[data.scores$Region == 1, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
region.2 <- data.scores[data.scores$Region == 2, ][chull(data.scores[data.scores$Region == 2, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
region.3 <- data.scores[data.scores$Region == 3, ][chull(data.scores[data.scores$Region == 3, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
region.4 <- data.scores[data.scores$Region == 4, ][chull(data.scores[data.scores$Region == 4, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
region.5 <- data.scores[data.scores$Region == 5, ][chull(data.scores[data.scores$Region == 5, c("NMDS1", "NMDS2")]), ]  # hull values for grp A

hull.data <- rbind(region.1, region.2, region.3, region.3, region.4, region.5)  #combine


species.scores <- as.data.frame(scores(ME_NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)

#plot NMDS 
p1 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( colour = as.factor(Region), size=1))+ 
  geom_polygon(data=hull.data, aes(x=NMDS1,y=NMDS2, fill=factor(Region), colour=factor(Region)), alpha=0.3, show.legend = F)+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Year") +
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))


p2<-ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape=factor(Region),colour=Year, size=1))+ 
  geom_polygon(data=hull.data, aes(x=NMDS1,y=NMDS2, fill=factor(Region),colour=Region, group=factor(Region)), alpha=0.3, show.legend = T)+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), fontface="bold", size=10)+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Year",fill="Region",shape="Region", y = "NMDS2") +
  scale_colour_gradient(name = "Year", low = "light grey", high = "black")+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))

#ellipse region plot
p3 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( colour = factor(Region), size=1))+ 
  geom_mark_ellipse(aes(fill=factor(Region), label=factor(Region), color=factor(Region)))+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Year", fill="Region") +
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))

#ellipse region and year plot

p4 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape=Year, size=1), color="grey")+ 
  scale_shape_binned(breaks = c(6, 12, 18))+
  guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Region), label=factor(Region)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Year", fill="Region")

#analysis of similarity test
ano<- anosim(ME_NMDS_data, trawl_data_arrange$YEAR, distance = "bray", permutations = 9999)
ano

ano<- anosim(ME_NMDS_data, trawl_data_arrange$REGION, distance = "bray", permutations = 9999)
ano




###### Stratum and Region NMDS #####
trawl_data_arrange<-filter(trawl_data,W_WT !=0)%>%
  group_by(STRATUM,REGION, functional_group)%>%
  summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
  filter(functional_group!="")%>%
  spread(functional_group, wt_kg)


ME_NMDS_data<-as.matrix(trawl_data_arrange[,3:6])

#run the NMDS
set.seed(123)
ME_NMDS=metaMDS(ME_NMDS_data, # Our community-by-species matrix
                k=2) # The number of reduced dimensions

#plot the results
plot(ME_NMDS)
ordiplot(ME_NMDS)
orditorp(ME_NMDS,display="species",col="red",air=0.01)
orditorp(ME_NMDS,display="sites",cex=1.25,air=0.01)

#extract NMDS scores for ggplot
data.scores = as.data.frame(scores(ME_NMDS))
#add columns to data frame 
data.scores$Stratum = trawl_data_arrange$STRATUM
data.scores$Region = trawl_data_arrange$REGION

#make hulls for the plot
region.1 <- data.scores[data.scores$Region == 1, ][chull(data.scores[data.scores$Region == 1, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
region.2 <- data.scores[data.scores$Region == 2, ][chull(data.scores[data.scores$Region == 2, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
region.3 <- data.scores[data.scores$Region == 3, ][chull(data.scores[data.scores$Region == 3, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
region.4 <- data.scores[data.scores$Region == 4, ][chull(data.scores[data.scores$Region == 4, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
region.5 <- data.scores[data.scores$Region == 5, ][chull(data.scores[data.scores$Region == 5, c("NMDS1", "NMDS2")]), ]  # hull values for grp A

hull.data_region <- rbind(region.1, region.2, region.3, region.3, region.4, region.5)  #combine

stratum.1 <- data.scores[data.scores$Stratum == 1, ][chull(data.scores[data.scores$Stratum == 1, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
stratum.2 <- data.scores[data.scores$Stratum == 2, ][chull(data.scores[data.scores$Stratum == 2, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
stratum.3 <- data.scores[data.scores$Stratum == 3, ][chull(data.scores[data.scores$Stratum == 3, c("NMDS1", "NMDS2")]), ]  # hull values for grp A
stratum.4 <- data.scores[data.scores$Stratum == 4, ][chull(data.scores[data.scores$Stratum == 4, c("NMDS1", "NMDS2")]), ]  # hull values for grp A

hull.data_stratum <- rbind(stratum.1, stratum.2, stratum.3, stratum.3, stratum.4)  #combine

species.scores <- as.data.frame(scores(ME_NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)

#plot
p1 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( shape = factor(Stratum), colour = factor(Region), size=1))+ 
  geom_polygon(data=hull.data_region, aes(x=NMDS1,y=NMDS2, fill=factor(Region), colour=factor(Region), group=factor(Region)), alpha=0.3, show.legend = T)+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Stratum", fill="Region")+
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))

p2 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2, color=factor(Stratum), fill=factor(Stratum))) + 
  geom_point(size = 4, aes( shape = factor(Region), size=1))+ 
  geom_polygon(data=hull.data_stratum, aes(x=NMDS1,y=NMDS2), alpha=0.3, show.legend = T)#+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Stratum", y = "NMDS2", shape = "Region", fill="Stratum")+
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))

#ellipse region and stratum plot
p3<- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
    geom_point(size = 4, aes(shape = factor(Stratum), size=1))+ 
    geom_mark_ellipse(aes(fill=factor(Region), color=factor(Region), label=factor(Region)))+
    geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
    theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
          axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
          legend.text = element_text(size = 20, face ="bold", colour ="black"), 
          legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
          axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
          legend.title = element_text(size = 22, colour = "black", face = "bold"), 
          panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.key=element_blank()) + 
    labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Stratum", fill="Region")+
    scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
    scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))

#ellipse stratum and region plot
p4<- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
    geom_point(size = 4, aes(shape = factor(Region), size=1))+ 
    geom_mark_ellipse(aes(fill=factor(Stratum), color=factor(Stratum), label=factor(Stratum)))+
    geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
    theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
          axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
          legend.text = element_text(size = 20, face ="bold", colour ="black"), 
          legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
          axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
          legend.title = element_text(size = 22, colour = "black", face = "bold"), 
          panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.key=element_blank()) + 
    labs(x = "NMDS1", colour = "Stratum", y = "NMDS2", shape = "Region", fill="Stratum")+
    scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
    scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))



#analysis of similarity test
ano<- anosim(ME_NMDS_data, trawl_data_arrange$STRATUM, distance = "bray", permutations = 9999)
ano

ano<- anosim(ME_NMDS_data, trawl_data_arrange$REGION, distance = "bray", permutations = 9999)
ano


#####Environmental data###########
# Fall FVCOM average values
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/Results/GAMMs")
fall <- read.csv("ME_NH_fall_full.csv")

###### Questions #####
# Was there only three stratum until 2003? Looks like stratum 4 doesn't show up until then...
#Is it appropriate to do NMDS on summarized data like I have in the extra analysis here?