
##### NOTE this is in progress, data cleaning not yet performed!!

library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)
library(tibble)
library(neonDiv)

datapath<-'/Users/kelsey/Github/RCN-KY/2025 Raw Release Data/'
resultspath<-'/Users/kelsey/Github/RCN-KY/Clean Data/'

# load data

neonFish <- readRDS(paste0(datapath, 'neonFish.Robj'))

names(neonFish)

vars<-neonFish$variables_20107

field<-neonFish$fsh_fieldData
count<-neonFish$fsh_bulkCount
perfish<-neonFish$fsh_perFish
perpass<-neonFish$fsh_perPass

field$date<-date(field$boutEndDate)
field<-field[is.na(field$samplingImpractical),]

reachLength<-field %>% group_by(namedLocation,eventID) %>% summarise(length=mean(measuredReachLength))

reachdens<-count %>% group_by(namedLocation,eventID,scientificName) %>% summarise(count=sum(bulkFishCount))
reachdens$density<-NA
  
for (i in 1:nrow(i)){
  reachdens$density[i]<-reachdens
}

load('~/Downloads/data_fish.rda')

##

fieldpass<-right_join(field,perpass,join_by('eventID'=='eventID'))

countpass<-right_join(fieldpass,count,join_by('eventID'=='eventID'))

perfish$year<-year(perfish$boutEndDate)
perfish$condition<-perfish$fishWeight/perfish$fishTotalLength
perfish %>% group_by(scientificName,siteID,year,fishLifeStage) %>% summarise(count=length(unique(uid)),condition=mean(condition,na.rm=TRUE))->sumry

sumry.common_adult <- sumry %>%
  filter(fishLifeStage=='adult',count>=10)





  