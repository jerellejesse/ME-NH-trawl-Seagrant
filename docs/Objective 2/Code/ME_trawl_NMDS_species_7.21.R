#Seagrant ME-NH trawl project 
#J.Jesse
#July 2021- simplified code
#objective 2: functional groups
#Nonmetric multidimensional scaling 

#load packages
library(tidyverse)
library(vegan)
library(ggrepel)
library(ggforce)
library(ggnewscale)
library(ggthemes)
library(here)

#updated trawl data
trawl_data<-read.csv(here("Data/MaineDMR_Trawl_Survey_Catch_Data_2021-05-14.csv"))

#filter out 2020 because it is only spring no fall
trawl_data<-filter(trawl_data, Year<2020)

#remove shrimp
shrimp<-c("zebra lebbeid","shrimp", "shrimp northern","shrimp montagui","shrimp dichelo", "shrimp crangon", "shrimp pink glass", "polar lebbeid", "spiny lebbeid", "pandalus propinquus","arctic eualid","spiny lebbeid","shrimp sculptured")
other<-c("stars sea brittle baskets","star brittle","axius serratus","sea sponges","waved astarte","northern sea star","sea anemone","northern cyclocardia","trash species","sand dollar uncl","barnacle","snail moon")
other2<-c("krill", "mysidacea","gulf seasnail", "seasnail gelationous","seasnail atlantic")

#picking out top 50 biomass species (using average from all years, seasons, stratum, regions)
top_species_biomass<-filter(trawl_data,!COMMON_NAME %in% shrimp)%>%
  filter(!COMMON_NAME %in% other)%>%
  group_by(COMMON_NAME)%>%
  summarise(biomass=mean(Expanded_Weight_kg, na.rm=TRUE))%>%
  arrange(desc(biomass))%>%
  top_n(50)

#select top species in data frame
trawl_data<-left_join(top_species_biomass[1],trawl_data, by="COMMON_NAME")


#picking out top 50 abundance species (using average from all years, seasons, stratum, regions)
top_species_abundance<-filter(trawl_data,!COMMON_NAME %in% shrimp)%>%
  filter(!COMMON_NAME %in% other)%>%
  filter(!COMMON_NAME %in% other2)%>%
  group_by(COMMON_NAME)%>%
  summarise(abundance=mean(Expanded_Catch, na.rm=TRUE))%>%
  arrange(desc(abundance))%>%
  top_n(50)

#select top species in data frame
trawl_data<-left_join(top_species_abundance[1],trawl_data, by="COMMON_NAME")


#arrange data for NMDS in vegan
#sample rows-> year, region,season (do we also want plots with stratum?)
#functional groups columns-> abundance or biomass of each group, characteristics of the community from that sample/site

##### NMDS set up #####
#all possible grouping variables- does not converge

#stratified mean to get convergence and simplify plotting- 
#do we want to weight by the area of the stratum/total area of survey like in IEA?
#Year, region, season as grouping variable
#drop stratum because it is less interesting and more expected results
trawl_data_arrange<-group_by(trawl_data, Year,Season,Region,Stratum,Tow_Number,COMMON_NAME)%>% 
  summarise(tow_weight=sum(Expanded_Weight_kg, na.rm=TRUE))%>% #species biomass summed for each tow/ functional group
  group_by(Season,Region,Stratum, Year,COMMON_NAME)%>% 
  summarise(avg_weight=mean(tow_weight, na.rm=TRUE))%>% #average biomass per tow for each region, stratum, functional group
  group_by(Season,Region, Year,COMMON_NAME)%>% 
  summarise(biomass=mean(avg_weight, na.rm=TRUE))%>% #average biomass per tow for region/functional group, averaging over stratum now
  spread(COMMON_NAME, biomass) #tidy
trawl_data_arrange[is.na(trawl_data_arrange)]<-0


# #abundance instead of biomass- pick your grouping variables below
# #Year, region, season as grouping variable
# #drop stratum because it is less interesting and more expected
trawl_data_arrange<-group_by(trawl_data, Year,Season,Region,Stratum,Tow_Number,COMMON_NAME)%>%
  summarise(tow_catch=sum(Expanded_Catch, na.rm=TRUE))%>% #species abundance summed for each tow/ functional group
  group_by(Season,Region,Stratum, Year,COMMON_NAME)%>%
  summarise(avg_catch=mean(tow_catch, na.rm=TRUE))%>% #average abundance per tow for each region, stratum, functional group
  group_by(Season,Region, Year,COMMON_NAME)%>%
  summarise(abundance=mean(avg_catch, na.rm=TRUE))%>% #average abundance per tow for region/functional group, averaging over stratum now
  spread(COMMON_NAME, abundance) #tidy
trawl_data_arrange[is.na(trawl_data_arrange)]<-0

# 
# #Can run the season separate if needed
# #FALL NMDS
# trawl_data_arrange<-filter(trawl_data,Season=="Fall")%>%
#   group_by(trawl_data, Year,Season,Region,Stratum,Tow_Number,COMMON_NAME)%>% 
#   summarise(tow_weight=sum(Expanded_Weight_kg, na.rm=TRUE))%>% #species biomass summed for each tow/ functional group
#   group_by(Season,Region,Stratum, Year,COMMON_NAME)%>% 
#   summarise(avg_weight=mean(tow_weight, na.rm=TRUE))%>% #average biomass per tow for each region, stratum, functional group
#   group_by(Season,Region, Year,COMMON_NAME)%>% 
#   summarise(biomass=mean(avg_weight, na.rm=TRUE))%>% #average biomass per tow for region/functional group, averaging over stratum now
#   spread(COMMON_NAME, biomass) #tidy
# 
# trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
# ME_NMDS_data<-as.matrix(trawl_data_arrange[,3:52])
# ME_group_data<-trawl_data_arrange[, c(1:2)]
# 
# #SPRING NMDS
# trawl_data_arrange<-filter(trawl_data,Season=="Spring")%>%
#   group_by(trawl_data, Year,Season,Region,Stratum,Tow_Number,COMMON_NAME)%>% 
#   summarise(tow_weight=sum(Expanded_Weight_kg, na.rm=TRUE))%>% #species biomass summed for each tow/ functional group
#   group_by(Season,Region,Stratum, Year,COMMON_NAME)%>% 
#   summarise(avg_weight=mean(tow_weight, na.rm=TRUE))%>% #average biomass per tow for each region, stratum, functional group
#   group_by(Season,Region, Year,COMMON_NAME)%>% 
#   summarise(biomass=mean(avg_weight, na.rm=TRUE))%>% #average biomass per tow for region/functional group, averaging over stratum now
#   spread(COMMON_NAME, biomass) #tidy
# 
# trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
# ME_NMDS_data<-as.matrix(trawl_data_arrange[,3:52])
# ME_group_data<-trawl_data_arrange[, c(1:2)]

#other grouping variables for plotting-----------------------------------------------------------
# 5 year groupings
trawl_data_arrange$YEAR_GROUPS[trawl_data_arrange$Year %in% c(2000:2004)]<-"2000-2004"
trawl_data_arrange$YEAR_GROUPS[trawl_data_arrange$Year %in% c(2005:2009)]<-"2005-2009"
trawl_data_arrange$YEAR_GROUPS[trawl_data_arrange$Year %in% c(2010:2014)]<-"2010-2014"
trawl_data_arrange$YEAR_GROUPS[trawl_data_arrange$Year %in% c(2015:2019)]<-"2015-2019"

#decades
trawl_data_arrange$YEAR_DECADES[trawl_data_arrange$Year %in% c(2000:2009)]<-"2000-2009"
trawl_data_arrange$YEAR_DECADES[trawl_data_arrange$Year %in% c(2010:2019)]<-"2010-2019"

#new region
trawl_data_arrange$REGION_NEW[trawl_data_arrange$Region %in% c(1,2)]<-"West of Penobscot Bay"
trawl_data_arrange$REGION_NEW[trawl_data_arrange$Region %in% c(3)]<-"Penobscot Bay"
trawl_data_arrange$REGION_NEW[trawl_data_arrange$Region %in% c(4,5)]<-"East of Penobscot Bay"

#region (e-W) and 5-year groups
trawl_data_arrange$REGION_YEAR[trawl_data_arrange$Region %in% c(1,2)&trawl_data_arrange$Year %in% c(2000:2004) ]<-"West of Penobscot Bay, 2000-2004"
trawl_data_arrange$REGION_YEAR[trawl_data_arrange$Region %in% c(1,2)&trawl_data_arrange$Year %in% c(2005:2009) ]<-"West of Penobscot Bay, 2005-2009"
trawl_data_arrange$REGION_YEAR[trawl_data_arrange$Region %in% c(1,2)&trawl_data_arrange$Year %in% c(2010:2014) ]<-"West of Penobscot Bay, 2010-2014"
trawl_data_arrange$REGION_YEAR[trawl_data_arrange$Region %in% c(1,2)&trawl_data_arrange$Year %in% c(2015:2019) ]<-"West of Penobscot Bay, 2015-2019"
trawl_data_arrange$REGION_YEAR[trawl_data_arrange$Region %in% c(4,5)&trawl_data_arrange$Year %in% c(2000:2004) ]<-"East of Penobscot Bay, 2000-2004"
trawl_data_arrange$REGION_YEAR[trawl_data_arrange$Region %in% c(4,5)&trawl_data_arrange$Year %in% c(2005:2009) ]<-"East of Penobscot Bay, 2005-2009"
trawl_data_arrange$REGION_YEAR[trawl_data_arrange$Region %in% c(4,5)&trawl_data_arrange$Year %in% c(2010:2014) ]<-"East of Penobscot Bay, 2010-2014"
trawl_data_arrange$REGION_YEAR[trawl_data_arrange$Region %in% c(4,5)&trawl_data_arrange$Year %in% c(2015:2019) ]<-"East of Penobscot Bay, 2015-2019"

#season and 5-year groups
trawl_data_arrange$SEASON_YEAR[trawl_data_arrange$Season %in% c("Fall")&trawl_data_arrange$Year %in% c(2000:2004) ]<-"Fall, 2000-2004"
trawl_data_arrange$SEASON_YEAR[trawl_data_arrange$Season %in% c("Fall")&trawl_data_arrange$Year %in% c(2005:2009) ]<-"Fall, 2005-2009"
trawl_data_arrange$SEASON_YEAR[trawl_data_arrange$Season %in% c("Fall")&trawl_data_arrange$Year %in% c(2010:2014) ]<-"Fall, 2010-2014"
trawl_data_arrange$SEASON_YEAR[trawl_data_arrange$Season %in% c("Fall")&trawl_data_arrange$Year %in% c(2015:2019) ]<-"Fall, 2015-2019"
trawl_data_arrange$SEASON_YEAR[trawl_data_arrange$Season %in% c("Spring")&trawl_data_arrange$Year %in% c(2000:2004) ]<-"Spring, 2000-2004"
trawl_data_arrange$SEASON_YEAR[trawl_data_arrange$Season %in% c("Spring")&trawl_data_arrange$Year %in% c(2005:2009) ]<-"Spring, 2005-2009"
trawl_data_arrange$SEASON_YEAR[trawl_data_arrange$Season %in% c("Spring")&trawl_data_arrange$Year %in% c(2010:2014) ]<-"Spring, 2010-2014"
trawl_data_arrange$SEASON_YEAR[trawl_data_arrange$Season %in% c("Spring")&trawl_data_arrange$Year %in% c(2015:2019) ]<-"Spring, 2015-2019"

#save matrix for future use if needed
#write.csv(trawl_data_arrange, "species_abundance_matrix.csv")

#set up final grouping data into dataframe
ME_group_data<-trawl_data_arrange[, c(1,2,3,55,56,57,58)]
ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:53])


##### run the NMDS #####
ME_NMDS_distance<-vegdist(ME_NMDS_data, method="bray")

ME_NMDS=metaMDS(ME_NMDS_distance, # Our community-by-functional group matrix
                k=2, # The number of reduced dimensions
                method="bray",
                trymax=200) #increase iterations

#extract NMDS scores for ggplot
data.scores = as.data.frame(scores(ME_NMDS))
#add columns to data frame 
data.scores$Stratum = trawl_data_arrange$Stratum
data.scores$Region = trawl_data_arrange$Region
data.scores$Year = trawl_data_arrange$Year
data.scores$Season= trawl_data_arrange$Season
data.scores$Year_groups= trawl_data_arrange$YEAR_GROUPS
data.scores$Year_decades= trawl_data_arrange$YEAR_DECADES
data.scores$Region_new=trawl_data_arrange$REGION_NEW
data.scores$Region_year=trawl_data_arrange$REGION_YEAR
data.scores$Season_year=trawl_data_arrange$SEASON_YEAR


#### Plot NMDS #####


#Regional differences plots---------------------------------------------------------------------
data.scores_region<-filter(data.scores,Region_new!="Penobscot Bay")%>%
  filter(Season=="Spring")
p1 <- ggplot() + 
  geom_point(data=data.scores_region, aes(x = NMDS1, y = NMDS2, shape=factor(Year_groups),color=factor(Region_new)),size = 3)+ 
  geom_mark_ellipse(data=data.scores_region,aes(x=NMDS1,y=NMDS2,color=factor(Region_new)), size=1, alpha=0.15)+
  scale_color_colorblind()+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 22),
        legend.text = element_text(size = 22, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24),
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"),
        legend.title = element_text(size = 24, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())+
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Year")

#Plot for region/year combo
#lines/paths plot
data.scores_east<-filter(data.scores, Region_new=="East of Penobscot Bay")%>%
  filter(Season=="Fall")
data.scores_west<-filter(data.scores, Region_new=="West of Penobscot Bay")%>%
  filter(Season=="Fall")

p2 <- ggplot() + 
  geom_point(data=data.scores_east, aes(x = NMDS1, y = NMDS2,color=factor(Region_year), shape=factor(Region_new)),size = 3)+ 
  geom_mark_ellipse(data=data.scores_east,aes(x=NMDS1,y=NMDS2,color=factor(Region_year)), size=1, alpha=0.15)+
  scale_color_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=4)))+
  geom_path(data=data.scores_east, aes(x=NMDS1,y=NMDS2,group=factor(Region_new)))+
  geom_text_repel(data=data.scores_east,aes(x=NMDS1,y=NMDS2,label=Year), size=4, fontface="bold", color="black")+
  ggnewscale::new_scale_color()+
  geom_point(data=data.scores_west, aes(x = NMDS1, y = NMDS2,color=factor(Region_year), shape=factor(Region_new)),size = 3)+ 
  geom_mark_ellipse(data=data.scores_west,aes(x=NMDS1,y=NMDS2,color=factor(Region_year)), size=1, alpha=0.15)+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  geom_path(data=data.scores_west, aes(x=NMDS1,y=NMDS2,group=factor(Region_new)))+
  geom_text_repel(data=data.scores_west,aes(x=NMDS1,y=NMDS2,label=Year), size=4, fontface="bold", color="black")+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 22),
        legend.text = element_text(size = 22, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24),
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"),
        legend.title = element_text(size = 24, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())+
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Year", fill="Region")

#without lines
p2b <- ggplot() + 
  geom_point(data=data.scores_east, aes(x = NMDS1, y = NMDS2,color=factor(Region_year), shape=factor(Region_new)),size = 3)+ 
  geom_mark_ellipse(data=data.scores_east,aes(x=NMDS1,y=NMDS2,color=factor(Region_year)), size=1, alpha=0.15)+
  scale_color_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=4)))+
  #geom_path(data=data.scores_east, aes(x=NMDS1,y=NMDS2,group=factor(Region_new)))+
  #geom_text_repel(data=data.scores_east,aes(x=NMDS1,y=NMDS2,label=Year), size=4, fontface="bold", color="black")+
  ggnewscale::new_scale_color()+
  geom_point(data=data.scores_west, aes(x = NMDS1, y = NMDS2,color=factor(Region_year), shape=factor(Region_new)),size = 3)+ 
  geom_mark_ellipse(data=data.scores_west,aes(x=NMDS1,y=NMDS2,color=factor(Region_year)), size=1, alpha=0.15)+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  #geom_path(data=data.scores_west, aes(x=NMDS1,y=NMDS2,group=factor(Region_new)))+
  #geom_text_repel(data=data.scores_west,aes(x=NMDS1,y=NMDS2,label=Year), size=4, fontface="bold", color="black")+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 22),
        legend.text = element_text(size = 22, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24),
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"),
        legend.title = element_text(size = 24, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())+
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Year", fill="Region")



#Time differences plots------------------------------------------------------------------------
data.scores_region<-filter(data.scores,Region_new!="Penobscot Bay")%>%
  filter(Season=="Fall")

p3 <- ggplot() + 
  geom_point(data=data.scores_region, aes(x = NMDS1, y = NMDS2,color=factor(Year_groups), shape=factor(Region_new)),size = 3)+ 
  geom_mark_ellipse(data=data.scores_region,aes(x=NMDS1,y=NMDS2,color=factor(Year_groups)), size=1, alpha=0.15)+
  scale_color_colorblind()+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 22),
        legend.text = element_text(size = 22, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24),
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"),
        legend.title = element_text(size = 24, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())+
  labs(x = "NMDS1", colour = "Year", y = "NMDS2", shape = "Region")



#Seasonal differences plots----------------------------------------------------------------------
#ellipse season and year plot
data.scores_east<-filter(data.scores, Region_new=="East of Penobscot Bay")
data.scores_west<-filter(data.scores, Region_new=="West of Penobscot Bay")

p4 <- ggplot(data.scores_west, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 3, aes(shape=factor(Year_groups), color=factor(Season)))+ 
  geom_mark_ellipse(aes(color=factor(Season)), size=1, alpha=0.15)+
  scale_color_colorblind()+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Season", y = "NMDS2", shape = "Year", fill="Season")


data.scores_spring<-filter(data.scores, Season=="Spring")%>%
  filter(Region_new=="West of Penobscot Bay")
data.scores_fall<-filter(data.scores, Season=="Fall")%>%
  filter(Region_new=="West of Penobscot Bay")

#lines/path
p5 <- ggplot() + 
  geom_point(data=data.scores_fall, aes(x = NMDS1, y = NMDS2,color=factor(Season_year), shape=factor(Season)),size = 3)+ 
  geom_mark_ellipse(data=data.scores_fall,aes(x=NMDS1,y=NMDS2,color=factor(Season_year)), size=1, alpha=0.15)+
  scale_color_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=4)))+
  geom_path(data=data.scores_fall, aes(x=NMDS1,y=NMDS2,group=factor(Season)))+
  geom_text_repel(data=data.scores_fall,aes(x=NMDS1,y=NMDS2,label=Year), size=4, fontface="bold", color="black")+
  ggnewscale::new_scale_color()+
  geom_point(data=data.scores_spring, aes(x = NMDS1, y = NMDS2,color=factor(Season_year), shape=factor(Season)),size = 3)+ 
  geom_mark_ellipse(data=data.scores_spring,aes(x=NMDS1,y=NMDS2,color=factor(Season_year)), size=1, alpha=0.15)+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  geom_path(data=data.scores_spring, aes(x=NMDS1,y=NMDS2,group=factor(Season)),color="navy")+
  geom_text_repel(data=data.scores_spring,aes(x=NMDS1,y=NMDS2,label=Year), size=4, fontface="bold", color="black")+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 22),
        legend.text = element_text(size = 22, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24),
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"),
        legend.title = element_text(size = 24, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())
labs(x = "NMDS1", colour = "Season", y = "NMDS2", shape = "Year", fill="Season")

#without lines
p5b <- ggplot() + 
  geom_point(data=data.scores_fall, aes(x = NMDS1, y = NMDS2,color=factor(Season_year), shape=factor(Season)),size = 3)+ 
  geom_mark_ellipse(data=data.scores_fall,aes(x=NMDS1,y=NMDS2,color=factor(Season_year)), size=1, alpha=0.15)+
  scale_color_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=4)))+
  #geom_path(data=data.scores_fall, aes(x=NMDS1,y=NMDS2,group=factor(Season)))+
  #geom_text_repel(data=data.scores_fall,aes(x=NMDS1,y=NMDS2,label=Year), size=4, fontface="bold", color="black")+
  ggnewscale::new_scale_color()+
  geom_point(data=data.scores_spring, aes(x = NMDS1, y = NMDS2,color=factor(Season_year), shape=factor(Season)),size = 3)+ 
  geom_mark_ellipse(data=data.scores_spring,aes(x=NMDS1,y=NMDS2,color=factor(Season_year)), size=1, alpha=0.15)+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  #geom_path(data=data.scores_spring, aes(x=NMDS1,y=NMDS2,group=factor(Season)),color="navy")+
  #geom_text_repel(data=data.scores_spring,aes(x=NMDS1,y=NMDS2,label=Year), size=4, fontface="bold", color="black")+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 22),
        legend.text = element_text(size = 22, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24),
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"),
        legend.title = element_text(size = 24, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())+
  labs(x = "NMDS1", colour = "Season", y = "NMDS2", shape = "Year", fill="Season")

#Stratum differences plots-----------------------------------------------------------------------
#Go back up to aggregating code and average over a different grouping variable and rerun NMDS



