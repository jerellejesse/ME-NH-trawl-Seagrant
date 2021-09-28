#plots for ME-NH trawl
#JJ

#load packages
library(tidyverse)
library(vegan)
library(ggrepel)
library(ggforce)
library(ggnewscale)
library(ggthemes)
library(here)

#read in data (functional groups were added in Biodiversity_metrics_groups.R)
trawl_data<-read.csv(here("Data/ME_trawl_NMDS_data.csv"))[,c(2:17, 20)]
#updated data
trawl_data_update<-read.csv(here("Data/MaineDMR_Trawl_Survey_Catch_Data_2021-05-14.csv"))


all_catch<-filter(trawl_data_update, Season=="Fall")%>%
  group_by(Year,Region,Stratum,Tow_Number)%>%
  summarise(weight=sum(Expanded_Weight_kg, na.rm=TRUE))%>%
  group_by(Year)%>%
  summarise(catch=mean(weight))
  
ggplot(all_catch)+geom_line(aes(y=catch, x=Year),size=1)+
  theme(axis.text.y = element_text(colour = "black", size = 20), 
        axis.text.x = element_text(colour = "black", size = 20), 
        legend.text = element_text(size = 20, colour ="black"), 
        legend.position = "bottom", axis.title.y = element_text(size = 22), 
        axis.title.x = element_text(size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5))

###species plots###
#mean catch/weight
species<-filter(trawl_data, COMMON_NAME %in% c("menhaden atlantic","herring atlantic"))%>%
  group_by(YEAR, SEASON,COMMON_NAME)%>%
  summarise(weight=mean(W_WT,na.rm=TRUE), catch=mean(W_NUM, na.rm=TRUE))%>%
  filter(SEASON=="SP")

species_update<-group_by(trawl_data_update, Year,Season)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(Year,COMMON_NAME,Season,tows)%>%
  summarise(weight=sum(Expanded_Weight_kg,na.rm=TRUE), catch=sum(Expanded_Catch,na.rm=TRUE))%>%
  mutate(weight_tow=weight/tows, catch_tow=catch/tows)#%>%
  filter(Season=="Fall")#%>%
  filter(COMMON_NAME %in% c("herring atlantic","menhaden atlantic"))

ggplot(species_update)+geom_line(aes(y=weight_tow, x=Year, group=COMMON_NAME,color=COMMON_NAME),size=1)+
  labs(x="Year", y="CPUE")+
  #scale_x_continuous(breaks=seq(2000,2019,by=1),labels=c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))+
  #scale_color_manual(values=c("navy","grey25"),name="Species", labels=c("Herring", "Menhaden"))+
  theme(axis.text.y = element_text(colour = "black", size = 20), 
        axis.text.x = element_text(colour = "black", size = 20), 
        legend.text = element_text(size = 20, colour ="black"), 
        legend.position = "bottom", axis.title.y = element_text(size = 22), 
        axis.title.x = element_text(size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5))

name.labs<-c("Herring","Menhaden")
names(name.labs)<-c("Herring Atlantic","Menhaden Atlantic")
ggplot(species_update)+geom_line(aes(y=catch_tow, x=Year),size=1)+
  labs(x="Year", y="CPUE")+
  facet_grid(rows=vars(COMMON_NAME),scales="free", labeller = labeller(COMMON_NAME=name.labs))+
  theme(strip.text.x=element_text(size=20), strip.text.y=element_text(size=20))+
  #scale_x_continuous(breaks=seq(0,17,by=1),labels=c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017"))+
  #scale_color_manual(values=c("navy","dark grey"),name="Species", labels=c("Herring", "Menhaden"))+
  theme(axis.text.y = element_text(colour = "black", size = 20), 
        axis.text.x = element_text(colour = "black", size = 20), 
        legend.text = element_text(size = 20, colour ="black"), 
        legend.position = "bottom", axis.title.y = element_text(size = 22), 
        axis.title.x = element_text(size = 22, colour = "black"), 
        legend.title = element_text(size = 22, colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5))


cpue_species<-group_by(trawl_data_update,Year,Season)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(COMMON_NAME,Year,Season)%>%
  mutate(biomass=sum(Expanded_Weight_kg, na.rm = T),catch=sum(Expanded_Catch, na.rm=T))%>%
  mutate(weight_percent=biomass/tows, catch_percent=catch/tows)%>%
  group_by(Year,COMMON_NAME)%>%
  summarise(weight_prop=mean(weight_percent),catch_prop=mean(catch_percent))

ggplot(cpue_species)+
  geom_line(aes(x=Year, y=weight_prop, color=COMMON_NAME, group=COMMON_NAME), size=1)+
  theme_classic()+
  labs(x="Year", y="Biomass/ tow (kg)", color="Species")+
  theme(text=element_text(size=20))

top10<-group_by(cpue_species, COMMON_NAME)%>%
  summarise(mean(weight_prop))
cpue_species$COMMON_NAME[!cpue_species$COMMON_NAME %in% c("lobster american","hake silver (whiting)","herring atlantic","dogfish spiny","alewife")]<-"Other"

ggplot(cpue_species)+
  geom_bar(aes(x=Year, y=weight_prop, fill=COMMON_NAME), position="fill", stat = "identity")+
  labs(x="Year", y="Proportion of Biomass/ tow (kg)", color="Species")+
  scale_fill_colorblind()+
  theme(text=element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 22), 
        legend.text = element_text(size = 22, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 24, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())

ggplot(cpue_species)+
  geom_line(aes(x=Year, y=weight_prop, color=COMMON_NAME, group=COMMON_NAME), size=1)+
  theme_classic()+
  scale_color_colorblind()+
  labs(x="Year", y="biomass/ tow", color="Species")+
  #scale_x_discrete(labels =c(seq(2000,2017, by=1)))+
  theme(text=element_text(size=22))


####functional group plots####
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/ID grouping")
groups<-read.csv("species_groups.csv")
setwd("C:/Users/jjesse/Desktop/GMRI/ME NH Trawl/Seagrant/Objective 2")
trawl<-read.csv("full_me_dmr_expcatch.csv")
groups<-full_join(groups,trawl,by="COMMON_NAME")%>%
  select(COMMON_NAME,SCIENTIFIC_NAME,functional_group)%>%
  distinct()


trawl_data<-read.csv("MaineDMR_Trawl_Survey_Catch_Data_2021-05-14.csv")
trawl_3_groups<-left_join(trawl_data, groups, by="COMMON_NAME") #state of the ecosystem groups

#not all species are included in a functional group
empty_groups<-filter(trawl_3_groups, functional_group=="")
unique(empty_groups$COMMON_NAME)

empty_groups_3<-filter(trawl_3_groups, functional_group=="")
#lots that are not classified?
empty_group_average<-group_by(empty_groups_3,COMMON_NAME)%>%
  summarise(wt_kg=mean(Expanded_Weight_kg), num=mean(Expanded_Catch))

functional<-group_by(trawl_3_groups,functional_group)%>%
  summarise(weight=sum(Expanded_Weight_kg,na.rm=TRUE), catch=sum(Expanded_Catch, na.rm=TRUE))%>%
  mutate(total_weight=sum(weight), total_catch=sum(catch))%>%
  mutate(weight_prop=weight/total_weight,ctach_prop=catch/total_catch)

#biomass/haul in each group
biomass_per_group<-group_by(trawl_3_groups,functional_group)%>%
  summarise(biomass=sum(Expanded_Weight_kg, na.rm = T))%>%
  mutate(total=sum(biomass))%>%
  mutate(percent=(biomass/total)*100)
# groups account for 89% of total biomass

#abundance/haul in each group
abundance_per_group<-group_by(trawl_3_groups,functional_group)%>%
  summarise(number=mean(Expanded_Catch, na.rm = T))%>%
  mutate(total=sum(number))%>%
  mutate(percent=(number/total)*100)
# groups account for 50% of total abundance


#number of different species included
number_species<-group_by(trawl_3_groups,functional_group)%>%
  summarise(species=length(unique(COMMON_NAME)))%>%
  mutate(total=sum(species))%>%
  mutate(percent=(species/total)*100)
#55 species included in groups ~30%

cpue_groups<-filter(trawl_3_groups, functional_group!="")%>%
  group_by(Year,Season)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(Year,functional_group,Season,tows)%>%
  summarise(weight=sum(Expanded_Weight_kg,na.rm=TRUE), catch=sum(Expanded_Catch,na.rm=TRUE))%>%
  mutate(weight_tow=weight/tows, catch_tow=catch/tows)

cpue_species<-filter(trawl_3_groups, functional_group!="")%>%
  group_by(Year,Season)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(Year,COMMON_NAME,Season,tows)%>%
  summarise(weight=sum(Expanded_Weight_kg,na.rm=TRUE), catch=sum(Expanded_Catch,na.rm=TRUE))%>%
  mutate(weight_tow=weight/tows, catch_tow=catch/tows)


#####functional group plots#####
trawl_3_groups$functional_group[trawl_3_groups$functional_group==""]<-"undefined"
trawl_3_groups$functional_group[is.na(trawl_3_groups$functional_group)]<-"undefined"

#cpue each year for weight and catch
cpue_year<-group_by(trawl_3_groups,Year,Season)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(functional_group,Year,Season)%>%
  mutate(biomass=sum(Expanded_Weight_kg, na.rm = T),catch=sum(Expanded_Catch, na.rm=T))%>%
  mutate(weight_percent=biomass/tows, catch_percent=catch/tows)%>%
  group_by(Year,functional_group)%>%
  summarise(weight_prop=mean(weight_percent),catch_prop=mean(catch_percent))

ggplot(cpue_year)+
  geom_line(aes(x=Year, y=weight_prop, color=functional_group, group=functional_group), size=1)+
  theme_classic()+
  scale_color_colorblind()+
  labs(x="Year", y="Biomass/ tow (kg)", color="Functional group")+
  theme(text=element_text(size=20))

 ggplot(cpue_year)+
  geom_line(aes(x=Year, y=weight_prop, group=functional_group), size=1)+
  geom_point(aes(x=Year, y=weight_prop, group=functional_group),size=1.5)+
  scale_color_colorblind()+
  labs(x="Year", y="Biomass/ tow (kg)", color="Functional group")+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_discrete(breaks=c("00","02","04","06","08","10","12","14","16"), labels =c(seq(2000,2017, by=2)))+
  theme(text=element_text(size=20))+
  facet_grid(functional_group ~.)

ggplot(cpue_year)+
  geom_bar(aes(x=Year, y=weight_prop, fill=functional_group), position="fill", stat = "identity")+
  scale_fill_colorblind(name="Functional Group")+
  labs(x="Year", y="Proportion of Biomass/ tow (kg)", color="Functional group")+
  theme(text=element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 22), 
        legend.text = element_text(size = 22, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 24, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())


ggplot(cpue_year)+
  geom_line(aes(x=Year, y=weight_prop, color=functional_group, group=functional_group), size=1)+
  theme_classic()+
  scale_color_colorblind()+
  labs(x="Year", y="biomass/ tow", color="Functional group")+
  #scale_x_discrete(labels =c(seq(2000,2017, by=1)))+
  theme(text=element_text(size=22))



#by season
ggplot()+geom_line(data=cpue_groups, aes(x=Year, y=weight_tow), group=1, size=1)+
  facet_grid(functional_group~Season)+ 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year")+ylab("Biomass/tow (kg)")+theme(axis.title.x=element_text(size=22), axis.title.y = element_text(size=22))+
  theme(axis.text.x=element_text(size=20, angle=90), axis.text.y = element_text(size=20))+
  scale_x_discrete(labels=seq(2000,2017,1))+
  theme(strip.background = element_rect(fill="white"), strip.text = element_text(size=20))



#by region
cpue_region<-filter(trawl_3_groups, functional_group!="")%>%
  group_by(Year,Region)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(Year,functional_group,Region,tows)%>%
  summarise(weight=sum(Expanded_Weight_kg,na.rm=TRUE), catch=sum(Expanded_Catch,na.rm=TRUE))%>%
  mutate(weight_tow=weight/tows, catch_tow=catch/tows)

ggplot()+geom_line(data=cpue_region, aes(x=Year, y=weight_tow), group=1, size=1)+
  facet_grid(functional_group~Region)+ 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year")+ylab("Biomass/ Tow (kg)")+theme(axis.title.x=element_text(size=22), axis.title.y = element_text(size=22))+
  theme(axis.text.x=element_text(size=20, angle=90), axis.text.y = element_text(size=20))+
  theme(strip.background = element_rect(fill="white"), strip.text = element_text(size=20))

#by stratum
cpue_stratum<-filter(trawl_3_groups, functional_group!="")%>%
  group_by(Year,Stratum)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(Year,functional_group,Stratum,tows)%>%
  summarise(weight=sum(Expanded_Weight_kg,na.rm=TRUE), catch=sum(Expanded_Catch,na.rm=TRUE))%>%
  mutate(weight_tow=weight/tows, catch_tow=catch/tows)

ggplot()+geom_line(data=cpue_stratum, aes(x=Year, y=weight_tow), group=1, size=1)+
  facet_grid(functional_group~Stratum)+ 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year")+ylab("Biomass/ Tow (kg)")+theme(axis.title.x=element_text(size=22), axis.title.y = element_text(size=22))+
  theme(axis.text.x=element_text(size=20, angle=90), axis.text.y = element_text(size=20))+
  theme(strip.background = element_rect(fill="white"), strip.text = element_text(size=20))


####Each functional group####
benthivore<-filter(trawl_3_groups, functional_group=="benthivore")
cpue_benthivore<-group_by(benthivore,Year,Season)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(COMMON_NAME,Year,Season)%>%
  mutate(biomass=sum(Expanded_Weight_kg, na.rm = T),catch=sum(Expanded_Catch, na.rm=T))%>%
  mutate(weight_percent=biomass/tows, catch_percent=catch/tows)%>%
  group_by(Year,COMMON_NAME)%>%
  summarise(weight_prop=mean(weight_percent),catch_prop=mean(catch_percent))

ggplot(cpue_benthivore)+
  geom_line(aes(x=Year, y=weight_prop, color=COMMON_NAME, group=COMMON_NAME), size=1)+
  theme_classic()+
  labs(x="Year", y="Biomass/ tow (kg)", color="Species")+
  theme(text=element_text(size=20))

top10<-group_by(cpue_benthivore, COMMON_NAME)%>%
  summarise(mean(weight_prop))
cpue_benthivore$COMMON_NAME[!cpue_benthivore$COMMON_NAME %in% c("lobster american","american plaice (dab)","flounder winter","haddock","crab jonah","flounder atlantic witch (grey sole)","flounder yellowtail","scup","skate barndoor")]<-"Other"

ggplot(cpue_benthivore)+
  geom_bar(aes(x=Year, y=weight_prop, fill=COMMON_NAME), position="fill", stat = "identity")+
  labs(x="Year", y="Proportion of Biomass/ tow (kg)", color="Species")+
  scale_fill_colorblind()+
  theme(text=element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 22), 
        legend.text = element_text(size = 22, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 24, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())


benthos<-filter(trawl_3_groups, functional_group=="benthos")
cpue_benthos<-group_by(benthos,Year,Season)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(COMMON_NAME,Year,Season)%>%
  mutate(biomass=sum(Expanded_Weight_kg, na.rm = T),catch=sum(Expanded_Catch, na.rm=T))%>%
  mutate(weight_percent=biomass/tows, catch_percent=catch/tows)%>%
  group_by(Year,COMMON_NAME)%>%
  summarise(weight_prop=mean(weight_percent),catch_prop=mean(catch_percent))

ggplot(cpue_benthos)+
  geom_line(aes(x=Year, y=weight_prop, color=COMMON_NAME, group=COMMON_NAME), size=1)+
  theme_classic()+
  labs(x="Year", y="Biomass/ tow (kg)", color="Species")+
  theme(text=element_text(size=20))

top10<-group_by(cpue_benthos, COMMON_NAME)%>%
  summarise(mean(weight_prop))

ggplot(cpue_benthos)+
  geom_bar(aes(x=Year, y=weight_prop, fill=COMMON_NAME), position="fill", stat = "identity")+
  labs(x="Year", y="Proportion of Biomass/ tow (kg)", color="Species")+
  scale_fill_colorblind()+
  theme(text=element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 22), 
        legend.text = element_text(size = 22, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 24, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())


piscivore<-filter(trawl_3_groups, functional_group=="piscivore")
cpue_piscivore<-group_by(piscivore,Year,Season)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(COMMON_NAME,Year,Season)%>%
  mutate(biomass=sum(Expanded_Weight_kg, na.rm = T),catch=sum(Expanded_Catch, na.rm=T))%>%
  mutate(weight_percent=biomass/tows, catch_percent=catch/tows)%>%
  group_by(Year,COMMON_NAME)%>%
  summarise(weight_prop=mean(weight_percent),catch_prop=mean(catch_percent))

ggplot(cpue_piscivore)+
  geom_line(aes(x=Year, y=weight_prop, color=COMMON_NAME, group=COMMON_NAME), size=1)+
  theme_classic()+
  labs(x="Year", y="Biomass/ tow (kg)", color="Species")+
  theme(text=element_text(size=20))


top10<-group_by(cpue_piscivore, COMMON_NAME)%>%
  summarise(sum(weight_prop))
cpue_piscivore$COMMON_NAME[!cpue_piscivore$COMMON_NAME %in% c("hake silver (whiting)","dogfish spiny","hake atlantic red","hake white","redfish acadian ocean perch","monkfish","squid short-finned")]<-"Other"

ggplot(cpue_piscivore)+
  geom_bar(aes(x=Year, y=weight_prop, fill=COMMON_NAME), position="fill", stat = "identity")+
  labs(x="Year", y="Proportion of Biomass/ tow (kg)", color="Species")+
  scale_fill_colorblind()+
  theme(text=element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 22), 
        legend.text = element_text(size = 22, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 24, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())



planktivore<-filter(trawl_3_groups, functional_group=="planktivore")
cpue_planktivore<-group_by(planktivore,Year,Season)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(COMMON_NAME,Year,Season)%>%
  mutate(biomass=sum(Expanded_Weight_kg, na.rm = T),catch=sum(Expanded_Catch, na.rm=T))%>%
  mutate(weight_percent=biomass/tows, catch_percent=catch/tows)%>%
  group_by(Year,COMMON_NAME)%>%
  summarise(weight_prop=mean(weight_percent),catch_prop=mean(catch_percent))

ggplot(cpue_planktivore)+
  geom_line(aes(x=Year, y=weight_prop, color=COMMON_NAME, group=COMMON_NAME), size=1)+
  theme_classic()+
  labs(x="Year", y="Biomass/ tow (kg)", color="Species")+
  theme(text=element_text(size=20))

top10<-group_by(cpue_planktivore, COMMON_NAME)%>%
  summarise(sum(weight_prop))
cpue_planktivore$COMMON_NAME[!cpue_planktivore$COMMON_NAME %in% c("herring atlantic","alewife","sculpin longhorn","butterfish","mackerel atlantic","herring blueback","lumpfish")]<-"Other"

ggplot(cpue_planktivore)+
  geom_bar(aes(x=Year, y=weight_prop, fill=COMMON_NAME), position="fill", stat = "identity")+
  labs(x="Year", y="Proportion of Biomass/ tow (kg)", color="Species")+
  scale_fill_colorblind()+
  theme(text=element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 22), 
        legend.text = element_text(size = 22, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 24, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())



undefined<-filter(trawl_3_groups, functional_group=="undefined")
cpue_undefined<-group_by(undefined,Year,Season)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(COMMON_NAME,Year,Season)%>%
  mutate(biomass=sum(Expanded_Weight_kg, na.rm = T),catch=sum(Expanded_Catch, na.rm=T))%>%
  mutate(weight_percent=biomass/tows, catch_percent=catch/tows)%>%
  group_by(Year,COMMON_NAME)%>%
  summarise(weight_prop=mean(weight_percent),catch_prop=mean(catch_percent))

ggplot(cpue_undefined)+
  geom_line(aes(x=Year, y=weight_prop, color=COMMON_NAME, group=COMMON_NAME), size=1)+
  theme_classic()+
  labs(x="Year", y="Biomass/ tow (kg)", color="Species")+
  theme(text=element_text(size=20))

top10<-group_by(cpue_undefined, COMMON_NAME)%>%
  summarise(sum(weight_prop))
cpue_undefined$COMMON_NAME[!cpue_undefined$COMMON_NAME %in% c("monkfish","stars sea brittle baskets","smelt rainbow","crab atlantic rock","sturgeon atlantic","sea sponges", "waved astrate")]<-"Other"

ggplot(cpue_undefined)+
  geom_bar(aes(x=Year, y=weight_prop, fill=COMMON_NAME), position="fill", stat = "identity")+
  labs(x="Year", y="Proportion of Biomass/ tow (kg)", color="Species")+
  scale_fill_colorblind()+
  theme(text=element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 22), 
        legend.text = element_text(size = 22, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 24, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())


#no shrimp
no_shrimp<-filter(trawl_3_groups, functional_group=="undefined")%>%
  filter(!COMMON_NAME  %in% c("shrimp northern","shrimp montagui","shrimp","shrimp dichelo"))

cpue_no_shrimp<-group_by(no_shrimp,Year,Season)%>%
  mutate(tows=n_distinct(Tow_Number))%>%
  group_by(COMMON_NAME,Year,Season)%>%
  mutate(biomass=sum(Expanded_Weight_kg, na.rm = T),catch=sum(Expanded_Catch, na.rm=T))%>%
  mutate(weight_percent=biomass/tows, catch_percent=catch/tows)%>%
  group_by(Year,COMMON_NAME)%>%
  summarise(weight_prop=mean(weight_percent),catch_prop=mean(catch_percent))

cpue_no_shrimp$COMMON_NAME[!cpue_no_shrimp$COMMON_NAME %in% c("monkfish","stars sea brittle baskets","smelt rainbow","crab atlantic rock","sturgeon atlantic","sea sponges", "waved astrate")]<-"Other"

ggplot(cpue_no_shrimp)+
  geom_bar(aes(x=Year, y=weight_prop, fill=COMMON_NAME), position="fill", stat = "identity")+
  labs(x="Year", y="Proportion of Biomass/ tow (kg)", color="Species")+
  scale_fill_colorblind()+
  theme(text=element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black", size = 22, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 22), 
        legend.text = element_text(size = 22, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 24), 
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"), 
        legend.title = element_text(size = 24, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.key=element_blank())
