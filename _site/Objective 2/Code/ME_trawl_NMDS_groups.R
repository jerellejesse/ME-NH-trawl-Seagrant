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
library(ggthemes)

setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/ID grouping")
groups<-read.csv("species_groups.csv")
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/Objective 2")
trawl<-read.csv("full_me_dmr_expcatch.csv")
groups<-full_join(groups,trawl,by="COMMON_NAME")%>%
  select(COMMON_NAME,SCIENTIFIC_NAME,functional_group)%>%
  distinct()


trawl_data<-read.csv("MaineDMR_Trawl_Survey_Catch_Data_2021-05-14.csv")
trawl_3_groups<-left_join(trawl_data, groups, by="COMMON_NAME") #state of the ecosystem groups

trawl_data<-left_join(trawl_data_update, groups, by="COMMON_NAME") #state of the ecosystem groups
trawl_data$functional_group[trawl_data$COMMON_NAME=="flounder atlantic windowpane (sand dab)"]<-"piscivore"
trawl_data$functional_group[is.na(trawl_data$functional_group)]<-"undefined"
trawl_data$functional_group[trawl_data$functional_group==""]<-"undefined"
trawl_data<-filter(trawl_data, Year<2020)
#arrange data for NMDS in vegan
#sample rows-> year, stratum (do we also want plots with region?)
#functional groups columns-> abundance of each group, characteristics of the community from that sample/site

#####NMDS groupings#####
#haul- all grouping variables- does not converge
trawl_data_arrange<-filter(trawl_data,Expanded_Weight_kg !=0)%>%
  group_by(Year,Season, Stratum, Region)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(Stratum,Region, Year,Season, functional_group)%>%
  summarise(weight=sum(Expanded_Weight_kg, na.rm=TRUE)/tows)%>%
  group_by(Stratum,Region, Year,Season, functional_group)%>%
  summarise(weight=mean(weight, na.rm=TRUE))%>%
  spread(functional_group, weight)

trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
ME_NMDS_data<-as.matrix(trawl_data_arrange[,5:8])
ME_group_data<-trawl_data_arrange[, c(1:4)]

#average by one variable to get convergence
#Year and Stratum and Region NMDS 
trawl_data_arrange<-filter(trawl_data,Expanded_Weight_kg !=0)%>%
  group_by(Year,Stratum,Region)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(Stratum,Region, Year,functional_group)%>%
  summarise(weight=sum(Expanded_Weight_kg, na.rm=TRUE)/tows)%>%
  group_by(Stratum,Region, Year,functional_group)%>%
  summarise(weight=mean(weight, na.rm=TRUE))%>%
  spread(functional_group, weight)

trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:7])
ME_group_data<-trawl_data_arrange[, c(1:3)]

#Year, stratum, season
trawl_data_arrange<-filter(trawl_data,Expanded_Weight_kg !=0)%>%
  group_by(Year,Stratum,Season)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(Stratum,Year,Season,functional_group)%>%
  summarise(weight=sum(Expanded_Weight_kg, na.rm=TRUE)/tows)%>%
  group_by(Stratum,Season, Year,functional_group)%>%
  summarise(weight=mean(weight, na.rm=TRUE))%>%
  spread(functional_group, weight)

trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:7])
ME_group_data<-trawl_data_arrange[, c(1:3)]


#Year, region, season
trawl_data_arrange<-filter(trawl_data,Expanded_Weight_kg !=0)%>%
  #filter(Region %in% c("1","2"))%>%
  group_by(Year,Season,Region)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(Season,Region, Year,functional_group)%>%
  summarise(weight=sum(Expanded_Weight_kg, na.rm=TRUE)/tows)%>%
  group_by(Season,Region, Year,functional_group)%>%
  summarise(weight=mean(weight, na.rm=TRUE))%>%
  spread(functional_group, weight)

trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:7])
ME_group_data<-trawl_data_arrange[, c(1:3)]

#abundance instead of biomass- pick your grouping variables below
trawl_data_arrange<-filter(trawl_data,Expanded_Weight_kg !=0)%>%
  group_by(Year,Stratum,Region)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(Stratum,Region, Year,functional_group)%>%
  summarise(weight=sum(Expanded_Catch, na.rm=TRUE)/tows)%>%
  group_by(Stratum,Region, Year,functional_group)%>%
  summarise(weight=mean(weight, na.rm=TRUE))%>%
  spread(functional_group, weight)

trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
ME_NMDS_data<-as.matrix(trawl_data_arrange[,4:7])
ME_group_data<-trawl_data_arrange[, c(1:3)]

#FALL NMDS
trawl_data_arrange<-filter(trawl_data,Expanded_Weight_kg !=0)%>%
  filter(Season=="Fall")%>%
  group_by(Year,Region)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(Region, Year,functional_group)%>%
  summarise(weight=sum(Expanded_Weight_kg, na.rm=TRUE)/tows)%>%
  group_by(Region, Year,functional_group)%>%
  summarise(weight=mean(weight, na.rm=TRUE))%>%
  spread(functional_group, weight)

trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
ME_NMDS_data<-as.matrix(trawl_data_arrange[,3:6])
ME_group_data<-trawl_data_arrange[, c(1:2)]

#SPRING NMDS
trawl_data_arrange<-filter(trawl_data,Expanded_Weight_kg !=0)%>%
  filter(Season=="Spring")%>%
  group_by(Year,Region)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(Region, Year,functional_group)%>%
  summarise(weight=sum(Expanded_Weight_kg, na.rm=TRUE)/tows)%>%
  group_by(Region, Year,functional_group)%>%
  summarise(weight=mean(weight, na.rm=TRUE))%>%
  spread(functional_group, weight)

trawl_data_arrange$benthos[is.na(trawl_data_arrange$benthos)]<-0
ME_NMDS_data<-as.matrix(trawl_data_arrange[,3:6])
ME_group_data<-trawl_data_arrange[, c(1:2)]

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


ME_group_data<-trawl_data_arrange[, c(1,2,8,9,10,11)]



##### run the NMDS #####
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
data.scores$Stratum = trawl_data_arrange$Stratum
data.scores$Region = trawl_data_arrange$Region
data.scores$Year = trawl_data_arrange$Year
data.scores$Season= trawl_data_arrange$Season
data.scores$Year_groups= trawl_data_arrange$YEAR_GROUPS
data.scores$Year_decades= trawl_data_arrange$YEAR_DECADES
data.scores$Region_new=trawl_data_arrange$REGION_NEW
data.scores$Region_year=trawl_data_arrange$REGION_YEAR
data.scores$Season_year=trawl_data_arrange$SEASON_YEAR

#ellipse region plot
p4<- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
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
  geom_point(size = 4, aes(colour = factor(Year_groups), size=1))+ 
  geom_mark_ellipse(aes( fill=factor(Year_groups),color=factor(Year_groups), label=factor(Year_groups)), size=1, alpha=0.1)+
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
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))

#ellipse season plot
p7 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = factor(Season), size=1))+ 
  geom_mark_ellipse(aes( fill=factor(Season),color=factor(Season)), size=1, alpha=0.1)+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Season", y = "NMDS2", shape = "Season", fill="Season") +
  scale_colour_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=2)))+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=2)))

#ellipse region and year plot
data.scores_east<-filter(data.scores, Region_new!="Penobscot Bay")%>%
  filter(Region_new !="West of Penobscot Bay")%>%
  filter(Region==c(4,5))
data.scores_west<-filter(data.scores, Region_new!="Penobscot Bay")%>%
  filter(Region_new !="East of Penobscot Bay")%>%
  filter(Region==c(1,2))

#lines/paths plot
p8 <- ggplot() + 
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
  geom_text_repel(data=data.scores_west,aes(x=NMDS1,y=NMDS2,label=Year), size=4, fontface="bold", color="black")#+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 22),
        legend.text = element_text(size = 22, face ="bold", colour ="black"),
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24),
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"),
        legend.title = element_text(size = 24, colour = "black", face = "bold"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Year", fill="Region")
  
#ellipses
data.scores_region<-filter(data.scores,Region_new!="Penobscot Bay")
  p8 <- ggplot() + 
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
  
#ellipse stratum and year plot
data.scores_region<-filter(data.scores,Region_new=="West of Penobscot Bay")
p9 <- ggplot(data.scores_region, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 3, aes(shape=factor(Year_groups), color=factor(Stratum)))+ 
  #scale_shape_binned(breaks = c(6, 12, 18))+
  #guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Stratum)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=4)))+
  scale_color_colorblind()+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold", color="black")+
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
p9 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 3, aes(shape=factor(Region_new), color=factor(Year_groups)))+ 
  #scale_shape_binned(breaks = c(6, 12, 18))+
  #guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Year_groups)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=4)))+
  scale_color_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=4)))+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold", color="black")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Year", y = "NMDS2", shape = "Stratum", fill="Year")

#ellipse season and year plot
p10 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 3, aes(shape=factor(Year_groups), color=factor(Season)))+ 
  #scale_shape_binned(breaks = c(6, 12, 18))+
  #guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Season), label=factor(Season)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=2)))+
  scale_color_manual(values=scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=2)))+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold", color="black")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Season", y = "NMDS2", shape = "Year", fill="Season")

#ellipse season and year plot
data.scores_spring<-filter(data.scores, Season=="Spring")#%>%
  #filter(Region==5)

data.scores_fall<-filter(data.scores, Season=="Fall")#%>%
  #filter(Region==5)

#lines/path
p10 <- ggplot() + 
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

#ellipses
p10 <- ggplot() + 
  geom_point(data=data.scores_spring, aes(x = NMDS1, y = NMDS2,color=factor(Region_new), shape=factor(Year_groups)),size = 3)+ 
  geom_mark_ellipse(data=data.scores_spring,aes(x=NMDS1,y=NMDS2,color=factor(Region_new)), size=1, alpha=0.15)+
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

#ellipse season and year plot
data.scores_region<-filter(data.scores, Region_new==c("West of Penobscot Bay"))

p10 <- ggplot(data.scores_region, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 3, aes(shape=factor(Region_new), color=factor(Year_decades)))+ 
  #scale_shape_binned(breaks = c(6, 12, 18))+
  #guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Year_decades)), size=1, alpha=0.15)+
  #scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_color_manual(values = c("#000000","#56B4E9"))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold", color="#8c8c8c")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Year", y = "NMDS2", shape = "Region", fill="Year")

#ellipse season and year plot
p10 <- ggplot(data.scores_region, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 3, aes(shape=factor(Year_groups), color=factor(Season)))+ 
  #scale_shape_binned(breaks = c(6, 12, 18))+
  #guides(shape=guide_bins(show.limits= TRUE))+
  #scale_colour_gradient(name = "Year", low = "light grey", high = "black", breaks=seq(0,16, by=4), label=seq(2000,2016, by=4))+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Season)), size=1, alpha=0.15)+
  #scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=4)))+
  scale_color_manual(values = c("#000000","#56B4E9"))+
  #geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold", color="#8c8c8c")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Season", y = "NMDS2", shape = "Year", fill="Season")


#ellipse stratum and region plot
p11 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape=factor(Region), colour = factor(Stratum), size=1))+ 
  #scale_colour_gradient(name = "Region", low = "light grey", high = "black")+
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
  labs(x = "NMDS1", colour = "Stratum", y = "NMDS2", shape = "Region", fill="Stratum") 
  
#ellipse stratum and region plot #2
p12 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape=factor(Stratum), color = factor(Region), size=1))+ 
  #scale_colour_gradient(name = "Region", low = "light grey", high = "black")+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Region)), size=1, alpha=0.15)+
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
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Stratum", fill="Region") 

#season and stratum
p13 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size=4,aes(shape=factor(Stratum),color = Season), size=1)+ 
  #scale_colour_gradient(name = "Stratum", low = "light grey", high = "black")+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Season), label=factor(Stratum)), size=1, alpha=0.15)+
  scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=2)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=2)))+
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=10, fontface="bold")+
  theme(axis.text.y = element_text(colour = "black", size = 20, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 20), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 22), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Season", y = "NMDS2", shape = "Stratum", fill="Season") 

#Region and season
p13 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size=4,aes(shape=factor(Region),color = Season), size=1)+ 
  #scale_colour_gradient(name = "Stratum", low = "light grey", high = "black")+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Season), label=factor(Region)), size=1, alpha=0.15)+
  #scale_fill_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=2)))+
  scale_color_manual(values=scales::seq_gradient_pal("light blue", "navy", "Lab")(seq(0,1,length.out=2)))+
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

#Region and season
p13 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size=4,aes(shape=factor(Season),color = factor(Region)))+ 
  #scale_colour_gradient(name = "Stratum", low = "light grey", high = "black")+
  #ggnewscale::new_scale_color()+
  geom_mark_ellipse(aes(color=factor(Region), label=factor(Season)), size=1, alpha=0.15)+
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

#Decade plots
p14 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = factor(Year_decades), size=1))+ 
  geom_mark_ellipse(aes( fill=factor(Year_decades),color=factor(Year_decades), label=factor(Year_decades)), size=1, alpha=0.1)+
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

ano_season<-anosim(ME_NMDS_data, trawl_data_arrange$SEASON, distance = "bray", permutations = 999)
ano_season

#####analysis of variance test#####
adonis_year<-adonis(ME_NMDS_data~YEAR+STRATUM+REGION, ME_group_data, distance="bray", permutations = 9999)
adonis_year

#use this if you don't have a balanced design- order doesn't matter for terms
adonis2(ME_NMDS_data~YEAR_GROUPS+STRATUM+SEASON, ME_group_data, distance="bray", permutations = 9999 ,by="margin")

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