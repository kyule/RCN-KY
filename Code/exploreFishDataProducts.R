
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

field<-neonFish$fsh_fieldData
names(field)

perpass<-neonFish$fsh_perFish
names(perpass)

bulk<-neonFish$fsh_bulkCount
names(bulk)

perpass$year<-year(perpass$boutEndDate)
perpass$condition<-perpass$fishTotalLength/perpass$fishWeight
perpass %>% group_by(scientificName,siteID,year,fishLifeStage) %>% summarise(count=length(unique(uid)),condition=mean(condition,na.rm=TRUE))->sumry

sumry %>%
  ggplot(aes(x = year, y = condition, 
             color = scientificName,
             group = siteID)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("condition") + 
  xlab("collection Date") +
  facet_wrap( ~ siteID, scales = "free_y")

                                                               