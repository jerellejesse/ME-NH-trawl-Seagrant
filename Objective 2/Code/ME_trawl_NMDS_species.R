#Seagrant ME-NH trawl project 
#J.Jesse
#March 2021
#objective 2: taxonomic species community
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
trawl_data<-read.csv("full_me_dmr_expcatch.csv")
trawl_data<-read.csv("MaineDMR_Trawl_Survey_Catch_Data_2021-05-14.csv")

#arrange data for NMDS in vegan
#sample rows-> year, stratum (do we also want plots with region?)
#functional groups columns-> abundance of each group, characteristics of the community from that sample/site
top_species_biomass<-filter(trawl_data,W_WT !=0)%>%
  group_by(COMMON_NAME)%>%
  summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
  filter(wt_kg>0.55)#%>%
  filter(wt_kg>2.15)

top_species_abundance<-filter(trawl_data,W_NUM !=0)%>%
  group_by(COMMON_NAME)%>%
  summarise(num=mean(W_NUM, na.rm=TRUE))%>%
  filter(num>7.75)#%>%
  filter(num>28)
  
##### NMDS groupings #####
#Year and Stratum and Region NMDS 
trawl_data_arrange<-filter(trawl_data,W_WT !=0)%>%
  group_by(STRATUM,REGION,YEAR, COMMON_NAME)%>%
  summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
  inner_join(top_species_biomass[1], by="COMMON_NAME")%>%
  spread(COMMON_NAME, wt_kg)

trawl_data_arrange[is.na(trawl_data_arrange)]<-0
ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:53])
#ME_NMDS_data_2<-sqrt(ME_NMDS_data)

ME_group_data<-trawl_data_arrange[, c(1:3)]

#Year, stratum, season 
trawl_data_arrange<-filter(trawl_data,W_WT !=0)%>%
  group_by(STRATUM,SEASON,YEAR, COMMON_NAME)%>%
  summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
  inner_join(top_species_biomass[1], by="COMMON_NAME")%>%
  spread(COMMON_NAME, wt_kg)

trawl_data_arrange[is.na(trawl_data_arrange)]<-0
ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:53])
#ME_NMDS_data_2<-sqrt(ME_NMDS_data)

ME_group_data<-trawl_data_arrange[, c(1:3)]

#Year, region, season 
trawl_data_arrange<-filter(trawl_data,W_WT !=0)%>%
  group_by(REGION,SEASON,YEAR, COMMON_NAME)%>%
  summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
  inner_join(top_species_biomass[1], by="COMMON_NAME")%>%
  spread(COMMON_NAME, wt_kg)

trawl_data_arrange[is.na(trawl_data_arrange)]<-0
ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:53])
#ME_NMDS_data_2<-sqrt(ME_NMDS_data)

ME_group_data<-trawl_data_arrange[, c(1:3)]

#FALL NMDS
# trawl_data_arrange<-filter(trawl_data,W_WT !=0)%>%
#   filter(SEASON=="FL")%>%
#   group_by(STRATUM,REGION,YEAR, COMMON_NAME)%>%
#   summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
#   inner_join(top_species_biomass[1], by="COMMON_NAME")%>%
#   spread(COMMON_NAME, wt_kg)
# 
# trawl_data_arrange[is.na(trawl_data_arrange)]<-0
# ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:51])
# 
# ME_group_data<-trawl_data_arrange[, c(1:3)]

#Spring NMDS
# trawl_data_arrange<-filter(trawl_data,W_WT !=0)%>%
#   filter(SEASON=="SP")%>%
#   group_by(STRATUM,REGION,YEAR, COMMON_NAME)%>%
#   summarise(wt_kg=mean(W_WT, na.rm=TRUE))%>%
#   inner_join(top_species_abundance[1], by="COMMON_NAME")%>%
#   spread(COMMON_NAME, wt_kg)
# 
# trawl_data_arrange[is.na(trawl_data_arrange)]<-0
# ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:50])
# #ME_NMDS_data_2<-sqrt(ME_NMDS_data)
# 
# ME_group_data<-trawl_data_arrange[, c(1:3)]

# 5 year groupings
trawl_data_arrange$YEAR_GROUPS[trawl_data_arrange$YEAR %in% c(0:4)]<-"2000-2004"
trawl_data_arrange$YEAR_GROUPS[trawl_data_arrange$YEAR %in% c(5:9)]<-"2005-2009"
trawl_data_arrange$YEAR_GROUPS[trawl_data_arrange$YEAR %in% c(10:14)]<-"2010-2014"
trawl_data_arrange$YEAR_GROUPS[trawl_data_arrange$YEAR %in% c(15:17)]<-"2015-2017"

#decades
trawl_data_arrange$YEAR_DECADES[trawl_data_arrange$YEAR %in% c(0:9)]<-"2000-2009"
trawl_data_arrange$YEAR_DECADES[trawl_data_arrange$YEAR %in% c(10:17)]<-"2010-2017"

ME_group_data<-trawl_data_arrange[, c(1,2,3,54,55)]


#run the NMDS
ME_NMDS=metaMDS(ME_NMDS_data, # Our community-by-species matrix
                k=2, trymax = 200) # The number of reduced dimensions

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
data.scores$Season = trawl_data_arrange$SEASON
data.scores$Year_groups = trawl_data_arrange$YEAR_GROUPS
data.scores$Year_decades = trawl_data_arrange$YEAR_DECADES

#ellipse region plot
p4 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = factor(Region), size=1))+ 
  geom_mark_ellipse(aes(fill=factor(Region) ,label=factor(Region), color=factor(Region)), size=1, alpha=0.1)+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
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
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
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
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
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

# year and season
p6 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size=4,aes(shape=factor(Season),color = Year_groups))+ 
  #scale_colour_gradient(name = "Stratum", low = "light grey", high = "black")+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Year_groups), label=factor(Year_groups)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=4)))+
  scale_color_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=4)))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Year", y = "NMDS2", shape = "Season", fill="Season") 

#ellipse region and year plot
p7 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape=Year_groups, color=factor(Region)))+ 
  #scale_shape_binned(breaks = c(6, 12, 18))+
  #guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Region), label=factor(Region)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=5)))+
  scale_color_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=5)))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
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
  geom_point(size = 4, aes(shape=Year_groups,color=factor(Stratum), size=1))+ 
  #scale_shape_binned(breaks = c(6, 12, 18))+
  #guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Stratum), label=factor(Stratum)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Stratum", y = "NMDS2", shape = "Year", fill="Stratum")

#ellipse stratum and year plot
p8 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape=factor(Stratum),color=factor(Year_groups), size=1))+ 
  #scale_shape_binned(breaks = c(6, 12, 18))+
  #guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Year_groups), label=factor(Year_groups)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Year_groups", y = "NMDS2", shape = "Stratum", fill="Year_groups")

#ellipse stratum and region plot
p9 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape=factor(Region), colour = factor(Stratum), size=1))+ 
  #scale_colour_gradient(name = "Region", low = "light grey", high = "black")+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Stratum), label=factor(Stratum)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
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
  geom_point(size = 4, aes(shape=factor(Stratum), colour = factor(Region), size=1))+ 
  #scale_colour_gradient(name = "Region", low = "light grey", high = "black")+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Region)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Stratum") 

#season and stratum
p11 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size=4,aes(shape=factor(Stratum),color = Season))+ 
  #scale_colour_gradient(name = "Stratum", low = "light grey", high = "black")+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Season), label=factor(Stratum)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=2)))+
  scale_color_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=2)))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Season", y = "NMDS2", shape = "Stratum", fill="Season") 

#season and region
p11 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size=4,aes(shape=factor(Region),color = Season))+ 
  #scale_colour_gradient(name = "Stratum", low = "light grey", high = "black")+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Season), label=factor(Season)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=2)))+
  scale_color_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=2)))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Season", y = "NMDS2", shape = "Region", fill="Season") 

#season and region
p11 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size=4,aes(shape=factor(Season),color = factor(Region)))+ 
  #scale_colour_gradient(name = "Stratum", low = "light grey", high = "black")+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Region), label=factor(Region)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=5)))+
  scale_color_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=5)))+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Season", fill="Region") 

#season and year
p11 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size=4,aes(shape=factor(Year_groups),color = Season))+ 
  #scale_colour_gradient(name = "Stratum", low = "light grey", high = "black")+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Season), label=factor(Season)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=2)))+
  scale_color_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=2)))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Season", y = "NMDS2", shape = "Year", fill="Season") 


#Decade plots
p12 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = factor(Year_decades), size=1))+ 
  geom_mark_ellipse(aes( fill=factor(Year_decades),color=factor(Year_decades), label=factor(Year_decades)), size=1, alpha=0.1)+
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
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=2)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=2)))

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
adonis_year<-adonis(ME_NMDS_data~YEAR+REGION+STRATUM, ME_group_data, distance="bray", permutations = 9999)
adonis_year

adonis2(ME_NMDS_data~YEAR+REGION+STRATUM, ME_group_data, distance="bray", permutations = 9999, by="margin")

########   top 50 species: abundance     #####################
top_species_abundance<-filter(trawl_data,W_NUM !=0)%>%
  group_by(COMMON_NAME)%>%
  summarise(num=mean(W_NUM, na.rm=TRUE))%>% 
  filter(num>7.75)

######Year and Stratum and Region NMDS #####
trawl_data_arrange<-filter(trawl_data,W_NUM !=0)%>%
  group_by(STRATUM,REGION,YEAR, COMMON_NAME)%>%
  summarise(num=mean(W_NUM, na.rm=TRUE))%>%
  inner_join(top_species_abundance[1], by="COMMON_NAME")%>%
  spread(COMMON_NAME, num)

trawl_data_arrange[is.na(trawl_data_arrange)]<-0
trawl_data_arrange$ID<-seq(1,345, by=1)
ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:53])
ME_NMDS_data_2<-sqrt(ME_NMDS_data)

ME_group_data<-trawl_data_arrange[, c(1:3,54)]

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
data.scores$Year = trawl_data_arrange$YEAR



#ellipse region plot
p4 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = factor(Region), size=1))+ 
  geom_mark_ellipse(aes(fill=factor(Region) ,label=factor(Region), color=factor(Region)), size=1, alpha=0.1)+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
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
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
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
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
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
  geom_point(size = 4, aes(shape=factor(Region), colour = Year, size=1))+ 
  scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(fill=factor(Region),color=factor(Region)), alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=5)))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", color="Region", y = "NMDS2", shape = "Region", fill="Region") 

#ellipse stratum and year plot
p8 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape=Year, size=1))+ 
  scale_shape_binned(breaks = c(6, 12, 18))+
  guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Stratum), label=factor(Stratum)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
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
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
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
adonis_year<-adonis(ME_NMDS_data~YEAR+REGION+STRATUM, ME_group_data, distance="bray", permutations = 9999)
adonis_year

