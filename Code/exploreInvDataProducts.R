library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)

datapath<-'/Users/kelsey/Github/RCN-KY/2025 Raw Release Data/'
resultspath<-'/Users/kelsey/Github/RCN-KY/Clean Data/'

# load data

neonInv <- readRDS(paste0(datapath, 'neonInv.Robj'))
lmRel<-read.csv(paste0(resultspath,'Benke_lengthMass_macroinverts.csv'))
lmRel2<-read.csv(paste0(resultspath,'Methot_lengthMass_macroinverts.csv'))
taxa<-read.csv(paste0(resultspath,'MacroinvertebrateTaxa.csv'))

names(neonInv)

field<-neonInv$inv_fieldData
samp<-neonInv$inv_persample
tax<-neonInv$inv_taxonomyProcessed
names(field)
names(samp)
names(tax)

field<-field[is.na(field$samplingImpractical),]

# remove apparent duplicates

field <- field %>%
  distinct(across(-uid), .keep_all = TRUE)


samp <- samp %>% 
  distinct(across(-uid), .keep_all = TRUE)

tax <- tax %>%
  distinct(across(-uid), .keep_all = TRUE)


inv<-full_join(field,samp,join_by("sampleID"=="sampleID"))


dupes_field<-inv[which(inv$uid.x %in% inv$uid.x[duplicated(inv$uid.x)]),]
#seem to be true duplicates, basically only differ in protocol and sort date, safe to remove
inv<-inv[!duplicated(inv$uid.x),]

dupes_samp<-inv[which(inv$uid.y %in% inv$uid.y[duplicated(inv$uid.y)]),]
#all are just missing records for the sample having been sent to the lab entirely, treat as if sampling did not happen
inv<-inv[!duplicated(inv$uid.y),]

inv_full<-full_join(inv,tax,join_by("sampleID"=="sampleID"))

dupes_tax<-inv_full[which(inv_full$uid %in% inv_full$uid[duplicated(inv_full$uid)]),]
# no dupes here


inv<-inv_full %>% select(siteID.x,
                 namedLocation.x,
                 aquaticSiteType,
                 eventID,
                 sampleID,
                 habitatType,
                 samplerType,
                 benthicArea,
                 substratumSizeClass,
                 benthicArea,
                 ponarDepth,
                 snagLength,
                 snagDiameter,
                 targetTaxaPresent,
                 acceptedTaxonID,
                 invertebrateLifeStage,
                 sizeClass,
                 estimatedTotalCount)

# 4108 duplicates here, explore them now

dupes <- inv %>%
  filter(duplicated(.) | duplicated(., fromLast = TRUE))
HOPB.20161011.SURBER.5<-inv_full[which(inv_full$sampleID=='HOPB.20161011.SURBER.5'),]
# looks like duplication happens when the size "category" differs typically due to damage to the specimen... ok to ignore

# drop couple of samples that do not match to field data
inv<-inv[!is.na(inv$siteID.x),]

#rename columns
names(inv)<-str_replace(names(inv),"[.]x","")


# add higher level taxon relationship
upper<-lmRel %>% group_by(UpperTaxon) %>% summarise(b=mean(b),a=mean(a))
upper$Taxon<-upper$UpperTaxon
upper<-upper[,c(1,4,2,3)]
lmRel<-rbind(lmRel,upper)
lmRel<- lmRel %>% distinct()

# find taxon for relationship

taxaFound<-data.frame(taxa=unique(inv$acceptedTaxonID),a=NA,b=NA)

bad_matches <- c()  # create an empty vector

for (i in 1:nrow(taxaFound)){
  print(i)
  neontax <- taxa[which(taxa$taxonID == taxaFound$taxa[i]), ]
  if (nrow(neontax) == 1){
    if (neontax$family %in% lmRel$Taxon){
      matched_rows <- which(lmRel$Taxon == neontax$family)
      if (length(matched_rows) == 1){
        taxaFound$a[i] <- lmRel$a[matched_rows]
        taxaFound$b[i] <- lmRel$b[matched_rows]
      } else {
        bad_matches <- c(bad_matches, i)  # record problematic i
      }
    }
    else if (neontax$order %in% lmRel$Taxon){
      matched_rows <- which(lmRel$Taxon == neontax$order)
      if (length(matched_rows) == 1){
        taxaFound$a[i] <- lmRel$a[matched_rows]
        taxaFound$b[i] <- lmRel$b[matched_rows]
      } else {
        bad_matches <- c(bad_matches, i)
      }
    }
    else if (neontax$class %in% lmRel$Taxon){
      matched_rows <- which(lmRel$Taxon == neontax$class)
      if (length(matched_rows) == 1){
        taxaFound$a[i] <- lmRel$a[matched_rows]
        taxaFound$b[i] <- lmRel$b[matched_rows]
      } else {
        bad_matches <- c(bad_matches, i)
      }
    }
    else if (neontax$subphylum %in% lmRel$Taxon){
      matched_rows <- which(lmRel$Taxon == neontax$subphylum)
      if (length(matched_rows) == 1){
        taxaFound$a[i] <- lmRel$a[matched_rows]
        taxaFound$b[i] <- lmRel$b[matched_rows]
      } else {
        bad_matches <- c(bad_matches, i)
      }
    }
    else if (neontax$family %in% lmRel2$Taxon){
      matched_rows <- which(lmRel2$Taxon == neontax$family)
      if (length(matched_rows) == 1){
        taxaFound$a[i] <- lmRel2$a[matched_rows]
        taxaFound$b[i] <- lmRel2$b[matched_rows]
      } else {
        bad_matches <- c(bad_matches, i)
      }
    }
    else if (neontax$subclass %in% lmRel2$Taxon){
      matched_rows <- which(lmRel2$Taxon == neontax$subclass)
      if (length(matched_rows) == 1){
        taxaFound$a[i] <- lmRel2$a[matched_rows]
        taxaFound$b[i] <- lmRel2$b[matched_rows]
      } else {
        bad_matches <- c(bad_matches, i)
      }
    }
    else if (neontax$class %in% lmRel2$Taxon){
      matched_rows <- which(lmRel2$Taxon == neontax$class)
      if (length(matched_rows) == 1){
        taxaFound$a[i] <- lmRel2$a[matched_rows]
        taxaFound$b[i] <- lmRel2$b[matched_rows]
      } else {
        bad_matches <- c(bad_matches, i)
      }
    }
    else if (neontax$class %in% lmRel2$Taxon){
      matched_rows <- which(lmRel2$Taxon == neontax$class)
      if (length(matched_rows) == 1){
        taxaFound$a[i] <- lmRel2$a[matched_rows]
        taxaFound$b[i] <- lmRel2$b[matched_rows]
      } else {
        bad_matches <- c(bad_matches, i)
      }
    }
    else if (neontax$phylum %in% lmRel2$Taxon){
      matched_rows <- which(lmRel2$Taxon == neontax$phylum)
      if (length(matched_rows) == 1){
        taxaFound$a[i] <- lmRel2$a[matched_rows]
        taxaFound$b[i] <- lmRel2$b[matched_rows]
      } else {
        bad_matches <- c(bad_matches, i)
      }
    }
  }
}

#investigate any bad matches
taxaFound[bad_matches,] # none found

# about 3/4 of them are found

notFound<-taxa[which(taxa$taxonID %in% taxaFound$taxa[is.na(taxaFound$a)]),]

#roundworms and ribbonworms use scaling = van den Hoogen, J., Geisen, S., Routh, D., Ferris, H., Traunspurger, W., Wardle, D. A., de Goede, R. G. M., Adams, B. J., Ahmad, W., Andriuzzi, W. S., Bardgett, R. D., Bonkowski, M., Campos-Herrera, R., Cares, J. E., Caruso, T., de Brito Caixeta, L., Chen, X., Costa, S. R., Creamer, R., ... Crowther, T. W. (2019). Soil nematode abundance and functional group composition at a global scale. Nature, 572(7768), 194–198. https://doi.org/10.1038/s41586-019-1418-6

taxaFound$a[which(taxaFound$taxa %in% c("NEMSP1","NEMSP","NEMSP2","PROSP37"))]<-0.479
taxaFound$b[which(taxaFound$taxa %in% c("NEMSP1","NEMSP","NEMSP2","PROSP37"))]<-2.56

# for other arthropods and tardigrades use scaling from Ganihar (1997): “Biomass estimates of terrestrial arthropods based on body length.” Journal of Biosciences, 22(2), 219–224.

notFoundarth<-notFound$acceptedTaxonID[which(notFound$phylum %in% c("Arthropoda","Tardigrada"))]
taxaFound$a[which(taxaFound$taxa %in% notFoundarth)]<-0.055
taxaFound$b[which(taxaFound$taxa %in% notFoundarth)]<-2.62

# how many missing now
notFound<-taxa[which(taxa$taxonID %in% taxaFound$taxa[is.na(taxaFound$a)]),]

# still missing cnidaria, bryozoa, and flatworms for now

# calculate the biomass
# add  mass based on size class relationships
inv$mass<-NA

for (i in 1:nrow(inv)){
  print(i)
  taxon<-taxaFound[which(taxaFound$taxa==inv$acceptedTaxonID[i]),]
  if(nrow(taxon)==1){
    inv$mass[i]<-taxon$a*inv$sizeClass[i]^taxon$b
  }
}

inv$totMass<-inv$estimatedTotalCount*inv$mass
inv$totMassPerArea<-inv$totMass/inv$benthicArea
inv$indivPerArea<-inv$estimatedTotalCount/inv$benthicArea

#remove rivers
inv<-inv[which(inv$aquaticSiteType!="river"),]

# if no target taxa found put mass and indivs at 0

inv$totMassPerArea[which(inv$targetTaxaPresent!="Y")]<-0
inv$indivPerArea[which(inv$targetTaxaPresent!="Y")]<-0


# Summarize

sumry <- inv %>% group_by(aquaticSiteType,siteID,habitatType,eventID) %>% summarise(massPerArea=sum(totMassPerArea,na.rm=T),indsPerArea=sum(indivPerArea))
sumry <- sumry %>%
  mutate(year = str_sub(eventID, 6, 9))

sumry %>%
  ggplot(aes(x = year, y = massPerArea, 
             color = habitatType, 
             group = habitatType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("total biomass per area") + 
  xlab("year") +
  facet_wrap( ~ siteID, scales = "free_y")

sumry %>%
  ggplot(aes(x = year, y = indsPerArea, 
             color = habitatType, 
             group = habitatType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("total individuals per area") + 
  xlab("year") +
  facet_wrap( ~ siteID, scales = "free_y")

write.csv(inv,paste0(resultspath,"invertebrates.csv"),row.names=FALSE)
