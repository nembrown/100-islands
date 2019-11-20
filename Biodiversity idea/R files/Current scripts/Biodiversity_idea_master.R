library(here)

#here() starts at C:/Users/norahbrown/Dropbox/Projects/100-islands

# Load data  ---------------------------------------------------------------


ben_fish_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_speciesabundance_20142018.csv")

ben_pelagic_demersal_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_fishcodes_demersal_pelagic.csv")

ben_bycatch_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_bycatch_20142018.csv")

ben_size_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_specieslength_20142018.csv")

ben_habitat_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_habitat_20142018.csv")

ben_weights<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_specieslength_20142018_biomass_complete.csv")


#if I want to include approximates of 2014 net size do thes file_nb
ben_netdimensions<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//netdimensions_nb.csv")

#head(ben_netdimensions)
##head(ben_fish_data)
##head(ben_bycatch_data)

#head(ben_fish_data)
#ggplot(ben_fish_data %>% filter(species=="herr"), aes(x=month, y=abundance, color=site))+geom_point()

### Need to include May 
# load packages ----------------------------------------------------------
library(rlang)
library(tidyr)
library(vegan)
library(ggplot2)
library(ggcorrplot)
library(doBy)
library(plyr)
library(dplyr)
library(doBy)
library(cowplot)
library(viridis)
library(matrixStats)
library(tidyverse)

#detach(package:plyr)
#detach(package:dplyr)

# Net dimensions ----------------------------------------------------------
# take net dimnesion data, make it long for each set to match the other files

ben_netdimensions<-ben_netdimensions %>% gather(replicate, volume, vol_set1,vol_set2)
ben_netdimensions$replicate[ben_netdimensions$replicate=="vol_set1"]<-"1"
ben_netdimensions$replicate[ben_netdimensions$replicate=="vol_set2"]<-"2"
#head(ben_netdimensions)

##adding area in here for the percent cover of eelgrass calculations later
ben_netdimensions<-ben_netdimensions %>% gather(area_replicate, area, area_set1,area_set2)
ben_netdimensions$area_replicate[ben_netdimensions$area_replicate=="area_set1"]<-"1"
ben_netdimensions$area_replicate[ben_netdimensions$area_replicate=="area_set2"]<-"2"
#head(ben_netdimensions)


# use only summer months data (July and August 7&8)
#use the dnetdimensions where we don't estimate missing data
# mean net volume used at that site in the summer
ben_netdimensions$volume<-as.numeric(ben_netdimensions$volume)
ben_netdimensions$area<-as.numeric(ben_netdimensions$area)
ben_netdimensions_year <-ben_netdimensions %>% filter(between(month, 7,8)) %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
ben_netdimensions_year <- ben_netdimensions_year[,c(1,11,12)]
#head(ben_netdimensions_year)




#sum of total net volume in given summer
ben_netdimensions_summer <-ben_netdimensions%>% filter(between(month, 7,8)) %>% group_by(site, year) %>% 
  summarise(sum_volume = sum(volume, na.rm=TRUE)) 
ben_netdimensions_summer$sum_volume[ben_netdimensions_summer$sum_volume==0]<-"NA"
#str(ben_netdimensions_summer)
ben_netdimensions_summer$sum_volume<-as.numeric(ben_netdimensions_summer$sum_volume)

# Fish abundance data cleaning ------------------------------------------------------
#only summer months 


ben_fish_data <- ben_fish_data %>% filter(between(month, 7,8))
#make wide format to calculate number of species
#use sum of abundance to reflect total of all fish caught in a given summer

ben_fish_data_wide_year <-ben_fish_data %>% group_by(site, year, species) %>% 
  summarise(sum_abundance = sum(abundance, na.rm=TRUE)) %>% 
  spread(species, sum_abundance) %>% 
  replace(is.na(.), 0) 

#str(ben_fish_data_wide_year)

#calculate richness & abundance  from the wide dataframe
ben_fish_data_wide_year_richness<-ben_fish_data_wide_year[,c(1,2)]
ben_fish_data_wide_year_richness$fish_richness<-specnumber(ben_fish_data_wide_year[,-c(1,2)])
#ben_fish_data_wide_year_richness$fish_diversity<-diversity(ben_fish_data_wide_year[,-1], index="shannon")
ben_fish_data_wide_year_richness$fish_abundance<-rowSums(ben_fish_data_wide_year[,-c(1,2)],na.rm = TRUE)
##head(ben_fish_data_wide_year_richness)

#adjust richness and abundance by total volume seined at that site
#bym3 is correcting problem that some sites were sampled more than others (aka species/Area curve)
ben_fish_data_wide_year_richness<-merge(ben_fish_data_wide_year_richness, ben_netdimensions_summer)
ben_fish_data_wide_year_richness$fish_richness_bym3<-ben_fish_data_wide_year_richness$fish_richness/(ben_fish_data_wide_year_richness$sum_volume)
ben_fish_data_wide_year_richness$fish_abundance_bym3<-ben_fish_data_wide_year_richness$fish_abundance/(ben_fish_data_wide_year_richness$sum_volume)

#average these values across years
ben_fish_data_wide_year_richness <- ben_fish_data_wide_year_richness[,-2] %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
##head(ben_fish_data_wide_year_richness)

###demersal vs. pelagic

##head(ben_fish_data)
#head(ben_pelagic_demersal_data)
names(ben_pelagic_demersal_data)[1]<-"species"

ben_pelagic_demersal_data2<-merge(ben_pelagic_demersal_data,ben_fish_data, by="species" )
ben_pelagic_data <- ben_pelagic_demersal_data2 %>% filter(Depth.Behaviour=="Pelagic")
ben_demersal_data <- ben_pelagic_demersal_data2 %>% filter(Depth.Behaviour=="Demersal")

ben_demersal_data_wide_year <-ben_demersal_data %>% group_by(site, year, species) %>% 
  summarise(sum_abundance = sum(abundance, na.rm=TRUE)) %>% 
  spread( species, sum_abundance) %>% 
  replace(is.na(.), 0) 

##head(ben_demersal_data_wide_year)

#calculate richness & abundance  from the wide dataframe
ben_demersal_data_wide_year_richness<-ben_demersal_data_wide_year[,c(1,2)]
ben_demersal_data_wide_year_richness$demersal_richness<-specnumber(ben_demersal_data_wide_year[,-c(1,2)])
#ben_demersal_data_wide_year_richness$demersal_diversity<-diversity(ben_demersal_data_wide_year[,-1], index="shannon")
ben_demersal_data_wide_year_richness$demersal_abundance<-rowSums(ben_demersal_data_wide_year[,-c(1,2)],na.rm = TRUE)
##head(ben_demersal_data_wide_year_richness)

#adjust richness and abundance by total volume seined at that site
#bym3 is correcting problem that some sites were sampled more than others (aka species/Area curve)
ben_demersal_data_wide_year_richness<-merge(ben_demersal_data_wide_year_richness, ben_netdimensions_summer)
ben_demersal_data_wide_year_richness$demersal_richness_bym3<-ben_demersal_data_wide_year_richness$demersal_richness/(ben_demersal_data_wide_year_richness$sum_volume)
ben_demersal_data_wide_year_richness$demersal_abundance_bym3<-ben_demersal_data_wide_year_richness$demersal_abundance/(ben_demersal_data_wide_year_richness$sum_volume)

#average these values across years
ben_demersal_data_wide_year_richness <- ben_demersal_data_wide_year_richness[,-2] %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
#head(ben_demersal_data_wide_year_richness)


ben_pelagic_data_wide_year <-ben_pelagic_data %>% group_by(site, year, species) %>% 
  summarise(sum_abundance = sum(abundance, na.rm=TRUE)) %>% 
  spread( species, sum_abundance) %>% 
  replace(is.na(.), 0) 

##head(ben_pelagic_data_wide_year)

#calculate richness & abundance  from the wide dataframe
ben_pelagic_data_wide_year_richness<-ben_pelagic_data_wide_year[,c(1,2)]
ben_pelagic_data_wide_year_richness$pelagic_richness<-specnumber(ben_pelagic_data_wide_year[,-c(1,2)])
#ben_pelagic_data_wide_year_richness$pelagic_diversity<-diversity(ben_pelagic_data_wide_year[,-1], index="shannon")
ben_pelagic_data_wide_year_richness$pelagic_abundance<-rowSums(ben_pelagic_data_wide_year[,-c(1,2)],na.rm = TRUE)
##head(ben_pelagic_data_wide_year_richness)

#adjust richness and abundance by total volume seined at that site
#bym3 is correcting problem that some sites were sampled more than others (aka species/Area curve)
ben_pelagic_data_wide_year_richness<-merge(ben_pelagic_data_wide_year_richness, ben_netdimensions_summer)
ben_pelagic_data_wide_year_richness$pelagic_richness_bym3<-ben_pelagic_data_wide_year_richness$pelagic_richness/(ben_pelagic_data_wide_year_richness$sum_volume)
ben_pelagic_data_wide_year_richness$pelagic_abundance_bym3<-ben_pelagic_data_wide_year_richness$pelagic_abundance/(ben_pelagic_data_wide_year_richness$sum_volume)

#average these values across years
ben_pelagic_data_wide_year_richness <- ben_pelagic_data_wide_year_richness[,-2] %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
##head(ben_pelagic_data_wide_year_richness)


# Bycatch data cleaning ---------------------------------------------------
#data cleaning 
##head(ben_bycatch_data)
names(ben_bycatch_data)[1]<-"site"
ben_bycatch_data[, 7] <- as.numeric(as.character( ben_bycatch_data[, 7] ))

#summer months only 
ben_bycatch_data <-ben_bycatch_data %>% filter(between(month, 7,8))

#same as above
ben_bycatch_data_wide_year <-ben_bycatch_data %>%  replace(is.na(.), 0) %>% group_by(site, year, species) %>% 
  summarise(sum_abundance = sum(estimate, na.rm=TRUE)) %>% 
  spread( species, sum_abundance) %>% 
  replace(is.na(.), 0)

##head(ben_bycatch_data_wide_year)
ben_bycatch_data_wide_year_richness<-ben_bycatch_data_wide_year[,c(1,2)]
ben_bycatch_data_wide_year_richness$bycatch_richness<-specnumber(ben_bycatch_data_wide_year[,-c(1,2)])
ben_bycatch_data_wide_year_richness$bycatch_abundance<-rowSums(ben_bycatch_data_wide_year[,-c(1,2)],na.rm = TRUE)
##head(ben_bycatch_data_wide_year_richness)

ben_bycatch_data_wide_year_richness<-merge(ben_bycatch_data_wide_year_richness, ben_netdimensions_summer)
ben_bycatch_data_wide_year_richness$bycatch_richness_bym3<-ben_bycatch_data_wide_year_richness$bycatch_richness/(ben_bycatch_data_wide_year_richness$sum_volume)
ben_bycatch_data_wide_year_richness$bycatch_abundance_bym3<-ben_bycatch_data_wide_year_richness$bycatch_abundance/(ben_bycatch_data_wide_year_richness$sum_volume)

ben_bycatch_data_wide_year_richness <- ben_bycatch_data_wide_year_richness[,-2] %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
##head(ben_bycatch_data_wide_year_richness)

# Fish Biomass data cleaning ----------------------------------------------

#This is the length data -- adding back in the by replicate data ... makes more sense. 
ben_size_data2 <-ben_size_data %>% filter(between(month, 7,8))
ben_size_data2[, 8] <- as.numeric(as.character( ben_size_data2[, 8] ))

#str(ben_size_data2)

ben_size_data_wide_year <-ben_size_data2 %>%  replace(is.na(.), 0) %>% group_by(site, year, month, day, replicate, species) %>% 
  summarise(fish_length = mean(length, na.rm=TRUE)) %>% 
  spread( species, fish_length) %>% 
  replace(is.na(.), 0)

#head(ben_size_data_wide_year)
ben_size_data_wide_year_richness<-ben_size_data_wide_year[,c(1,2, 3, 4, 5)]
ben_size_data_wide_year_richness$fish_length<-rowMeans(ben_size_data_wide_year[,-c(1,2,3,4,5)],na.rm = TRUE)
ben_size_data_wide_year_richness$fish_sd<-rowSds(as.matrix(ben_size_data_wide_year[,-c(1,2,3,4,5)]),na.rm = TRUE)
#head(ben_size_data_wide_year_richness)

ben_size_data_wide_year_richness <- ben_size_data_wide_year_richness[,-c(2,3,4,5)] %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
#head(ben_size_data_wide_year_richness)

### Now adding in the biomass data from genevieve - fish base ... times the length by the LW relationship for that species
#Also do this by replicate
#head(ben_weights)
ben_weights<-ben_weights[,-c(15,16,17,18, 19)]
ben_weights2 <-ben_weights %>% filter(between(month, 7,8))

ben_weights_wide_year <-ben_weights2 %>%  replace(is.na(.), 0) %>% group_by(site, year, month, day, replicate, species) %>% 
  summarise(fish_weight = mean(Weight..g., na.rm=TRUE)) %>% 
  spread( species, fish_weight) %>% 
  replace(is.na(.), 0)

#481 x 97 
#head(ben_weights_wide_year)

ben_weights_wide_year_richness<-ben_weights_wide_year[,c(1,2,3,4,5)]
ben_weights_wide_year_richness$fish_av_weight<-rowMeans(ben_weights_wide_year[,-c(1,2,3,4,5)],na.rm = TRUE)
ben_weights_wide_year_richness$fish_weight_sd<-rowSds(as.matrix(ben_weights_wide_year[,-c(1,2,3,4,5)]),na.rm = TRUE)
#head(ben_weights_wide_year_richness)

ben_weights_wide_year_richness <- ben_weights_wide_year_richness[,-c(2,3,4,5)] %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
#head(ben_weights_wide_year_richness)

ben_fish_data_wide_year_2 <-ben_fish_data %>% group_by(site, year, month, day, replicate, species) %>% 
  summarise(sum_abundance = sum(abundance, na.rm=TRUE)) %>% 
  spread( species, sum_abundance) %>% 
  replace(is.na(.), 0) 

#488 x 97
#head(ben_fish_data_wide_year_2)

#biomass is missing 8 rows ... abundance missing 1 rows
ben_fish_data_wide_year_3<-ben_fish_data_wide_year_2
ben_fish_data_wide_year_3$code<-paste(ben_fish_data_wide_year_3$site, ben_fish_data_wide_year_3$year, ben_fish_data_wide_year_3$month, ben_fish_data_wide_year_3$day, ben_fish_data_wide_year_3$replicate)
ben_weights_wide_year_3<-ben_weights_wide_year
ben_weights_wide_year_3$code<-paste(ben_weights_wide_year_3$site, ben_weights_wide_year_3$year, ben_weights_wide_year_3$month, ben_weights_wide_year_3$day, ben_weights_wide_year_3$replicate)

subset(ben_fish_data_wide_year_3, !(code %in% ben_weights_wide_year_3$code))
#8

subset(ben_weights_wide_year_3, !(code %in% ben_fish_data_wide_year_3$code))
#1 

#head(ben_weights_wide_year_3$code)
ben_weights_wide_year_4 <- ben_weights_wide_year_3 %>% filter(code!="ppo 2014 6 14 2")
#taking out the code column - if I go back to 5 to 8 months this is different bc fewer species are present
ben_weights_wide_year_4 <- ben_weights_wide_year_4[,-90]

ben_fish_data_wide_year_4 <- ben_fish_data_wide_year_3 %>% filter(! code %in% c("fan1 2018 7 15 2", "hdo 2015 5 3 2", 
                                                                                "ppo 2018 5 29 2", "ris1 2018 7 19 2", 
                                                                                "ssp 2014 7 2 2", "ssp 2016 5 8 2", 
                                                                                "wfb 2014 6 11 1", "wfb 2014 6 11 2"))
ben_fish_data_wide_year_4<- ben_fish_data_wide_year_4[,-90]



#combining size and abundance to get "biomass" estimate
ben_biomass_data_wide_year<-(ben_weights_wide_year_4[,-c(1,2,3,4,5)])*(ben_fish_data_wide_year_4[,-c(1,2,3,4,5)])
ben_biomass_data_wide_year_nf<-ben_weights_wide_year_4[,c(1,2,3,4,5)]
ben_biomass_data_wide_year_nf$fish_biomass<-rowSums(ben_biomass_data_wide_year)
#str(ben_biomass_data_wide_year_nf)

#correcting for total net volume
ben_netdimensions_summer4 <-ben_netdimensions%>% filter(between(month, 7,8)) %>% group_by(site, year, month, day, replicate) %>% 
  summarise(sum_volume = sum(volume, na.rm=TRUE)) 
ben_netdimensions_summer4$sum_volume[ben_netdimensions_summer4$sum_volume==0]<-"NA"
#head(ben_netdimensions_summer4)
ben_netdimensions_summer4$sum_volume<-as.numeric(ben_netdimensions_summer4$sum_volume)

ben_biomass_data_wide_year_nf<-merge(ben_biomass_data_wide_year_nf, ben_netdimensions_summer4)
ben_biomass_data_wide_year_nf$fish_biomass_bym3<-ben_biomass_data_wide_year_nf$fish_biomass/(ben_biomass_data_wide_year_nf$sum_volume)

ben_biomass_data_wide_year_nf2 <-ben_biomass_data_wide_year_nf[,-c(2,3,4,5)]%>% group_by(site) %>% summarise_if(is.numeric, list(~mean(., na.rm=TRUE), ~sd(., na.rm=TRUE)))
#str(ben_biomass_data_wide_year_nf2)


# Pelagic and demersal biomass --------------------------------------------


#Pelagic and demersal data - changed to biomass 

ben_pelagic_demersal_weights<-merge(ben_pelagic_demersal_data,ben_weights, by="species" )
##head(ben_pelagic_demersal_weights)


ben_pelagic_weights <- ben_pelagic_demersal_weights %>% filter(Depth.Behaviour=="Pelagic")
ben_demersal_weights <- ben_pelagic_demersal_weights %>% filter(Depth.Behaviour=="Demersal")

ben_demersal_weights2 <-ben_demersal_weights %>% filter(between(month, 7,8))

ben_demersal_weights_wide_year <-ben_demersal_weights2 %>%  replace(is.na(.), 0) %>% group_by(site, year, month, day, replicate, species) %>% 
  summarise(fish_weight = mean(Weight..g., na.rm=TRUE)) %>% 
  spread( species, fish_weight) %>% 
  replace(is.na(.), 0)


ben_demersal_data_wide_year2 <-ben_demersal_data %>% group_by(site, year, month, day, replicate, species) %>% 
  summarise(sum_abundance = sum(abundance, na.rm=TRUE)) %>% 
  spread( species, sum_abundance) %>% 
  replace(is.na(.), 0)

ben_demersal_data_wide_year_3<-ben_demersal_data_wide_year2
ben_demersal_data_wide_year_3$code<-paste(ben_demersal_data_wide_year_3$site, ben_demersal_data_wide_year_3$year, ben_demersal_data_wide_year_3$month, ben_demersal_data_wide_year_3$day, ben_demersal_data_wide_year_3$replicate)

ben_demersal_weights_wide_year_3<-ben_demersal_weights_wide_year
ben_demersal_weights_wide_year_3$code<-paste(ben_demersal_weights_wide_year_3$site, ben_demersal_weights_wide_year_3$year, ben_demersal_weights_wide_year_3$month, ben_demersal_weights_wide_year_3$day, ben_demersal_weights_wide_year_3$replicate)

#head(subset(ben_demersal_data_wide_year_3, !(code %in% ben_demersal_weights_wide_year_3$code)))
#16

ben_demersal_weights_wide_year_3

ben_demersal_data_wide_year_4<-subset(ben_demersal_data_wide_year_3, (code %in% ben_demersal_weights_wide_year_3$code))
#taking out the code column
ben_demersal_data_wide_year_4<- ben_demersal_data_wide_year_4[,-68]




#combining size and abundance to get "demersal_biomass" estimate
ben_demersal_biomass_data_wide_year<-(ben_demersal_weights_wide_year[,-c(1,2,3,4,5)])*(ben_demersal_data_wide_year_4[,-c(1,2,3,4,5)])
ben_demersal_biomass_data_wide_year_nf<-ben_demersal_weights_wide_year[,c(1,2,3,4,5)]
ben_demersal_biomass_data_wide_year_nf$fish_demersal_biomass<-rowSums(ben_demersal_biomass_data_wide_year)
#head(ben_demersal_biomass_data_wide_year_nf)

#correcting for total net volume
ben_demersal_biomass_data_wide_year_nf<-merge(ben_demersal_biomass_data_wide_year_nf, ben_netdimensions_summer4)
ben_demersal_biomass_data_wide_year_nf$fish_demersal_biomass_bym3<-ben_demersal_biomass_data_wide_year_nf$fish_demersal_biomass/(ben_demersal_biomass_data_wide_year_nf$sum_volume)
ben_demersal_biomass_data_wide_year_nf <-ben_demersal_biomass_data_wide_year_nf[,-c(2,3,4,5)]%>% group_by(site) %>% summarise_if(is.numeric, list(~mean(., na.rm=TRUE), ~sd(., na.rm=TRUE)))
#head(ben_demersal_biomass_data_wide_year_nf)

###ben size data etc for pelagic and pelagic only 
ben_pelagic_weights2 <-ben_pelagic_weights %>% filter(between(month, 7,8))

ben_pelagic_weights_wide_year <-ben_pelagic_weights2 %>%  replace(is.na(.), 0) %>% group_by(site, year, month, day, replicate, species) %>% 
  summarise(fish_weight = mean(Weight..g., na.rm=TRUE)) %>% 
  spread( species, fish_weight) %>% 
  replace(is.na(.), 0)

ben_pelagic_data_wide_year2 <-ben_pelagic_data %>% group_by(site, year, month, day, replicate, species) %>% 
  summarise(sum_abundance = sum(abundance, na.rm=TRUE)) %>% 
  spread( species, sum_abundance) %>% 
  replace(is.na(.), 0)

ben_pelagic_data_wide_year_3<-ben_pelagic_data_wide_year2
ben_pelagic_data_wide_year_3$code<-paste(ben_pelagic_data_wide_year_3$site, ben_pelagic_data_wide_year_3$year, ben_pelagic_data_wide_year_3$month, ben_pelagic_data_wide_year_3$day, ben_pelagic_data_wide_year_3$replicate)

ben_pelagic_weights_wide_year_3<-ben_pelagic_weights_wide_year
ben_pelagic_weights_wide_year_3$code<-paste(ben_pelagic_weights_wide_year_3$site, ben_pelagic_weights_wide_year_3$year, ben_pelagic_weights_wide_year_3$month, ben_pelagic_weights_wide_year_3$day, ben_pelagic_weights_wide_year_3$replicate)

subset(ben_pelagic_data_wide_year_3, !(code %in% ben_pelagic_weights_wide_year_3$code))
#31 that are missing!!! that's a lot (out of 365)

subset(ben_pelagic_weights_wide_year_3, !(code %in% ben_pelagic_data_wide_year_3$code))
#1 in this one 

ben_pelagic_data_wide_year_3

ben_pelagic_data_wide_year_4<-subset(ben_pelagic_data_wide_year_3, (code %in% ben_pelagic_weights_wide_year_3$code))
ben_pelagic_data_wide_year_4<- ben_pelagic_data_wide_year_4[,-17]

ben_pelagic_weights_wide_year_4<-subset(ben_pelagic_weights_wide_year_3, (code %in% ben_pelagic_data_wide_year_3$code))
ben_pelagic_weights_wide_year_4<- ben_pelagic_weights_wide_year_4[,-17]


#combining size and abundance to get "pelagic_biomass" estimate
ben_pelagic_biomass_data_wide_year<-(ben_pelagic_weights_wide_year_4[,-c(1,2,3,4,5)])*(ben_pelagic_data_wide_year_4[,-c(1,2,3,4,5)])
ben_pelagic_biomass_data_wide_year_nf<-ben_pelagic_weights_wide_year_4[,c(1,2,3,4,5)]
ben_pelagic_biomass_data_wide_year_nf$fish_pelagic_biomass<-rowSums(ben_pelagic_biomass_data_wide_year)
#head(ben_pelagic_biomass_data_wide_year_nf)

#correcting for total net volume
ben_pelagic_biomass_data_wide_year_nf<-merge(ben_pelagic_biomass_data_wide_year_nf, ben_netdimensions_summer4)
ben_pelagic_biomass_data_wide_year_nf$fish_pelagic_biomass_bym3<-ben_pelagic_biomass_data_wide_year_nf$fish_pelagic_biomass/(ben_pelagic_biomass_data_wide_year_nf$sum_volume)
ben_pelagic_biomass_data_wide_year_nf <-ben_pelagic_biomass_data_wide_year_nf[,-c(2,3,4,5)]%>% group_by(site) %>% summarise_if(is.numeric,list(~mean(., na.rm=TRUE), ~sd(., na.rm=TRUE)))
#head(ben_pelagic_biomass_data_wide_year_nf)

# Merging fish, bycatch, biomass, net dimensions ------------------------------------------
#merging files together into one data frame
#head(ben_fish_data_wide_year_richness)
#head(ben_bycatch_data_wide_year_richness)
#head(ben_size_data_wide_year_richness)
#head(ben_weights_wide_year_richness)

#head(ben_biomass_data_wide_year_nf)
#head(ben_pelagic_biomass_data_wide_year_nf)
#head(ben_netdimensions_year)
#head(ben_biomass_data_wide_year_nf2)

fish_richness_merged_tran_year<-merge(ben_fish_data_wide_year_richness, ben_netdimensions_year, by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_richness_merged_tran_year, ben_bycatch_data_wide_year_richness[,-4], by="site", all=TRUE)
# fish_bycatch_richness_merged_tran_year[is.na(fish_bycatch_richness_merged_tran_year)] <- 0
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_size_data_wide_year_richness, by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_weights_wide_year_richness, by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_biomass_data_wide_year_nf2[,-c(3,6)], by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_pelagic_data_wide_year_richness[,-4], by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_demersal_data_wide_year_richness[,-4], by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_demersal_biomass_data_wide_year_nf[,-c(3,6)], by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_pelagic_biomass_data_wide_year_nf[,-c(3,6)], by="site", all=TRUE)

#head(fish_bycatch_richness_merged_tran_year)

#commented out the bycatch part b/c I think no bycatch = NA not zero ... but check the fish notes for "no bycatch" and then assign a zero. 

#correcting richness and abundance for average net dimensions at that site 
#as opposed to bym3, this _corrected is correcting for the problem that not every site was seined with 
#the same sized net 
##then times 100 to get estimate of # fish per 100m3 fished 
# fish_bycatch_richness_merged_tran_year$fish_biomass_corrected<-((fish_bycatch_richness_merged_tran_year$fish_biomass)/fish_bycatch_richness_merged_tran_year$volume)*100
# fish_bycatch_richness_merged_tran_year$bycatch_abundance_corrected<-((fish_bycatch_richness_merged_tran_year$bycatch_abundance)/fish_bycatch_richness_merged_tran_year$volume)*100
# fish_bycatch_richness_merged_tran_year$fish_abundance_corrected<-((fish_bycatch_richness_merged_tran_year$fish_abundance)/fish_bycatch_richness_merged_tran_year$volume)*100
fish_bycatch_richness_merged_tran_year$bycatch_richness_corrected<-((fish_bycatch_richness_merged_tran_year$bycatch_richness)/fish_bycatch_richness_merged_tran_year$volume)*100
fish_bycatch_richness_merged_tran_year$fish_richness_corrected<-((fish_bycatch_richness_merged_tran_year$fish_richness)/fish_bycatch_richness_merged_tran_year$volume)*100
fish_bycatch_richness_merged_tran_year$marine_richness_corrected<-(fish_bycatch_richness_merged_tran_year$fish_richness_corrected+fish_bycatch_richness_merged_tran_year$bycatch_richness_corrected)
# fish_bycatch_richness_merged_tran_year$pelagic_biomass_corrected<-((fish_bycatch_richness_merged_tran_year$fish_pelagic_biomass)/fish_bycatch_richness_merged_tran_year$volume)*100
# fish_bycatch_richness_merged_tran_year$pelagic_abundance_corrected<-((fish_bycatch_richness_merged_tran_year$pelagic_abundance)/fish_bycatch_richness_merged_tran_year$volume)*100
# fish_bycatch_richness_merged_tran_year$demersal_biomass_corrected<-((fish_bycatch_richness_merged_tran_year$fish_demersal_biomass)/fish_bycatch_richness_merged_tran_year$volume)*100
# fish_bycatch_richness_merged_tran_year$demersal_abundance_corrected<-((fish_bycatch_richness_merged_tran_year$demersal_abundance)/fish_bycatch_richness_merged_tran_year$volume)*100
fish_bycatch_richness_merged_tran_year$pelagic_richness_corrected<-((fish_bycatch_richness_merged_tran_year$pelagic_richness)/fish_bycatch_richness_merged_tran_year$volume)*100
fish_bycatch_richness_merged_tran_year$demersal_richness_corrected<-((fish_bycatch_richness_merged_tran_year$demersal_richness)/fish_bycatch_richness_merged_tran_year$volume)*100

fish_bycatch_richness_merged_tran_year$prop_pelagic_richness<-fish_bycatch_richness_merged_tran_year$pelagic_richness/(fish_bycatch_richness_merged_tran_year$fish_richness)
fish_bycatch_richness_merged_tran_year$prop_pelagic_abundance<-fish_bycatch_richness_merged_tran_year$pelagic_abundance/(fish_bycatch_richness_merged_tran_year$fish_abundance)

#Calculating marine richness by combining fish and invertebrates
fish_bycatch_richness_merged_tran_year$marine_richness<-(fish_bycatch_richness_merged_tran_year$fish_richness+fish_bycatch_richness_merged_tran_year$bycatch_richness)
fish_bycatch_richness_merged_tran_year$marine_richness_bym3<-(fish_bycatch_richness_merged_tran_year$fish_richness_bym3+fish_bycatch_richness_merged_tran_year$bycatch_richness_bym3)
fish_bycatch_richness_merged_tran_year$marine_richness_corrected<-(fish_bycatch_richness_merged_tran_year$fish_richness_corrected+fish_bycatch_richness_merged_tran_year$bycatch_richness_corrected)

#head(fish_bycatch_richness_merged_tran_year)
write.csv(fish_bycatch_richness_merged_tran_year, "C:Biodiversity idea//Output files//fish_bycatch_richness_merged_tran_year.csv")

# Matching terrestrial transects to beachseine sites ----------------------

#This is one option: 
# #pulls from output files of R script - "Assigned points"
# hakai_sites_distance_tran<-read.csv("C:Biodiversity idea//Output files//Distance_btwn_points_transects.csv")
# hakai_sites_distance_tran<-hakai_sites_distance_tran[,-1]
# ##head(hakai_sites_distance_tran)
# names(hakai_sites_distance_tran)[3]<-"unq_tran"
# names(hakai_sites_distance_tran)[6]<-"site"
# hakai_sites_distance_tran<- hakai_sites_distance_tran%>% filter(Distance < 5)
# ##head(hakai_sites_distance_tran )



#This is working with a 1km radius around the transects instead
hakai_sites_distance_tran<-read.csv("C:Biodiversity idea//Output files//paired_sites_by_radius.csv")
hakai_sites_distance_tran<-hakai_sites_distance_tran[,-1]
#head(hakai_sites_distance_tran)

fish_bycatch_richness_merged_tran<-merge(fish_bycatch_richness_merged_tran_year, hakai_sites_distance_tran, by="site")

#head(fish_bycatch_richness_merged_tran)

### averaging across sites
fish_bycatch_richness_merged_tran <- fish_bycatch_richness_merged_tran %>% group_by(unq_tran) %>% summarise_if(is.numeric, mean, na.rm=TRUE)


# Loading and merging terrestrial data (at 0m) by transect ------------------------------------

#transect data
by_tran_master_0m<-read.csv("C:Food web idea//Data by person//Norah.data//by_tran_master_0m.csv")
#head(by_tran_master_0m)
by_tran_master_0m<-by_tran_master_0m[,-1]

 
xs3=quantile(na.omit(by_tran_master_0m$d15n),c(0,1/2, 1))
labels3 <- c("low d15N", "high d15N")
by_tran_master_0m<- by_tran_master_0m %>% mutate(d15n.cat = cut(d15n, xs3, labels = labels3))


### adding in tree diversity (transect level)
by_tran_master<-read.csv("C:Food web idea//Data by person//Norah.data//by_tran_master.csv")
#head(by_tran_master)
by_tran_master<-by_tran_master[,-1]
paste(
which( colnames(by_tran_master)=="tree_richness" ),
which( colnames(by_tran_master)=="tree_abundance"),
which( colnames(by_tran_master)=="sum_basal"),
which( colnames(by_tran_master)=="site_mean_by_tran" ),
which( colnames(by_tran_master)=="wrack_richness"),
which( colnames(by_tran_master)=="HAB2000"),
which( colnames(by_tran_master)=="MEAN_egarea1k"),
which( colnames(by_tran_master)=="MEAN_kparea1k"),
which( colnames(by_tran_master)=="d34s"),
which( colnames(by_tran_master)=="Radius_m_1000"),
which( colnames(by_tran_master)=="sum_1km"),
which( colnames(by_tran_master)=="WAVE_EXPOSURE"),
sep=","
)


# which( colnames(by_tran_master)=="herb_richness" )
# which( colnames(by_tran_master)=="herb_cover")

by_tran_master_subset<-by_tran_master[,c(1,16,19,20,112,111,101,48,59,15,108,81,99)]
#head(by_tran_master_subset)



#adding in a few interesting island-level components
by_isl_master<-read.csv("C:Food web idea//Data by person//Owen's data//by_isl_master.csv")
by_isl_master<-by_isl_master[,-1]
#head(by_isl_master)
paste(
  which( colnames(by_isl_master)=="unq_isl" ),
  which( colnames(by_isl_master)=="mammal_richness" ),
  which( colnames(by_isl_master)=="total_richness" ),
  which( colnames(by_isl_master)=="bird.richness" ),
  which( colnames(by_isl_master)=="bird.density" ),
  which( colnames(by_isl_master)=="habitat_het" ),
  which( colnames(by_isl_master)=="log_Area" ),
  which( colnames(by_isl_master)=="Neighb_250" ),
  which( colnames(by_isl_master)=="NDVI_mean" ),
  which( colnames(by_isl_master)=="Perimeter" ),
  which( colnames(by_isl_master)=="PA_norml" ),
  which( colnames(by_isl_master)=="DistW_ML" ),
  which( colnames(by_isl_master)=="Dist_Near" ),
  which( colnames(by_isl_master)=="Area" ),
  which( colnames(by_isl_master)=="size.cat2" ),
  which( colnames(by_isl_master)=="eagles" ),
  which( colnames(by_isl_master)=="ravens" ),
  which( colnames(by_isl_master)=="node" ),
  which( colnames(by_isl_master)=="SLOPE" ),
  which( colnames(by_isl_master)=="slope_mean" ),
   sep=","
)

by_isl_master_subset<-by_isl_master[,c(1,97,103,47,46,102,98,19,20,14,15,17,18,13,105,66,65,107,53,30)]
#head(by_isl_master_subset)

by_tran_master_0m_with_isl<-merge(by_tran_master_0m, by_isl_master_subset, by="unq_isl", all=TRUE)
by_tran_master_0m_with_isl<-merge(by_tran_master_0m_with_isl, by_tran_master_subset, by="unq_tran", all=TRUE)


#head(by_tran_master_0m_with_isl)


#head(fish_bycatch_richness_merged_tran)

#merging terrestrial with marine and adding in marine site information, saving file
fish_richness_merged_tran_isl<-merge(fish_bycatch_richness_merged_tran, by_tran_master_0m_with_isl, by="unq_tran", all.y=TRUE)
#head(fish_richness_merged_tran_isl)

xs4=quantile(na.omit(fish_richness_merged_tran_isl$fish_biomass_bym3_mean),c(0,0.25,0.75, 1))
labels4 <- c("low fish biomass", "med fish biomass", "high fish biomass")
fish_richness_merged_tran_isl<- fish_richness_merged_tran_isl %>% mutate(fish_biomass_bym3_cat = cut(fish_biomass_bym3_mean, xs4, labels = labels4))

length(xs4)
length(labels4)

fish_richness_merged_tran_isl$combined_richness_corrected<-fish_richness_merged_tran_isl$wrack_richness+fish_richness_merged_tran_isl$marine_richness_corrected
fish_richness_merged_tran_isl$eelgrass_cover_1km<-(fish_richness_merged_tran_isl$MEAN_egarea1k)/(fish_richness_merged_tran_isl$Radius_m_1000)
fish_richness_merged_tran_isl$habitat_cover_1km<-(fish_richness_merged_tran_isl$sum_1km)/(fish_richness_merged_tran_isl$Radius_m_1000)


levels(fish_richness_merged_tran_isl$WAVE_EXPOSURE)[levels(fish_richness_merged_tran_isl$WAVE_EXPOSURE)=="VP"]<-1
levels(fish_richness_merged_tran_isl$WAVE_EXPOSURE)[levels(fish_richness_merged_tran_isl$WAVE_EXPOSURE)=="P"]<-2
levels(fish_richness_merged_tran_isl$WAVE_EXPOSURE)[levels(fish_richness_merged_tran_isl$WAVE_EXPOSURE)=="SP"]<-3
levels(fish_richness_merged_tran_isl$WAVE_EXPOSURE)[levels(fish_richness_merged_tran_isl$WAVE_EXPOSURE)=="SE"]<-4
levels(fish_richness_merged_tran_isl$WAVE_EXPOSURE)[levels(fish_richness_merged_tran_isl$WAVE_EXPOSURE)=="E"]<-5
levels(fish_richness_merged_tran_isl$WAVE_EXPOSURE)[levels(fish_richness_merged_tran_isl$WAVE_EXPOSURE)=="CE"]<-6

write.csv(fish_richness_merged_tran_isl, "C:Biodiversity idea//Output files//fish_richness_merged_tran_isl.csv")
head(fish_richness_merged_tran_isl)