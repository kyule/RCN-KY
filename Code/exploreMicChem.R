
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

chem <- readRDS(paste0(datapath, 'neonMicChem.Robj'))
names(chem)

field<-chem$alg_fieldData


dom<-chem$alg_domainLabChemistry
dom<-dom[which(dom$analysisType=="chlorophyll/pheophytin"),]
fielddom<-full_join(field,dom,join_by("parentSampleID"=="parentSampleID"))
fielddom<-fielddom[!is.na(fielddom$parentSampleID),]
fielddom<-fielddom[!is.na(fielddom$uid.y),]

lab<-chem$alg_algaeExternalLabDataPerSample
lab<-lab[which(lab$analyte=="chlorophyll a"),]
lab <- lab %>%
  group_by(across(-c(replicate, analyteConcentration))) %>%
  summarise(analyteConcentration = mean(analyteConcentration), .groups = "drop")

full<-full_join(fielddom,lab, join_by('sampleID'=='sampleID'))

full<-full[!is.na(full$uid),]

full$year<-year(full$collectDate)

sumry <- full %>% group_by(siteID,year,habitatType) %>% summarise(chlor=mean(analyteConcentration,na.rm=TRUE))


sumry %>%
  ggplot(aes(x = year, y = chlor, 
             color = siteID,
             group = siteID)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab(" chlorophyll a concentration") + 
  xlab("year") +
  facet_wrap( ~ siteID, scales = "free_y")
