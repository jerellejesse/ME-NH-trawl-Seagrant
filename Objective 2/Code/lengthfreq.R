library(here)
library(ggplot2)
library(ggforce)
setwd("C:/Users/jjesse/Desktop/GMRI/ME-NH-trawl-Seagrant/Data")
data<-read.csv("full_me_dmr_lengthfreq.csv")
data$SAMPLE_LENGTH<-as.numeric(data$SAMPLE_LENGTH)

pdf("species_lengthfreq.pdf")

for(i in 1:18){
  
  print(ggplot()+
          geom_histogram(data=data, aes(x=SAMPLE_LENGTH), stat="count")+
          theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y=element_blank())+
          xlim(0,100)+
          facet_wrap_paginate(~COMMON_NAME, ncol=2, nrow=4, page=i))
}
dev.off()
