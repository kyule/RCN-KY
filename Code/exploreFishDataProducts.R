
##### NOTE this is in progress, data cleaning not yet performed!!


library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)
library(tibble)

datapath<-'/Users/kelsey/Github/RCN-KY/2025 Raw Release Data/'
resultspath<-'/Users/kelsey/Github/RCN-KY/Clean Data/'

# load data

neonFish <- readRDS(paste0(datapath, 'neonFish.Robj'))

names(neonFish)

fishpass<-left_join(perfish,perpass,join_by('eventID'='eventID'))




perfish$year<-year(perfish$boutEndDate)
perfish$condition<-perfish$fishWeight/perfish$fishTotalLength
perfish %>% group_by(scientificName,siteID,year,fishLifeStage) %>% summarise(count=length(unique(uid)),condition=mean(condition,na.rm=TRUE))->sumry

sumry.common_adult <- sumry %>%
  filter(fishLifeStage=='adult',count>=10) 
  