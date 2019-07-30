setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Biodiversity idea")
#change to norahbrown if on work computer



# Load data  ---------------------------------------------------------------


ben_fish_data<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_speciesabundance_20142018.csv")

ben_pelagic_demersal_data<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_fishcodes_demersal_pelagic.csv")

ben_bycatch_data<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_bycatch_20142018.csv")

ben_size_data<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_specieslength_20142018.csv")

ben_habitat_data<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_habitat_20142018.csv")

ben_weights<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_specieslength_20142018_biomass_complete.csv")


#if I want to include approximates of 2014 net size do thes file_nb
ben_netdimensions<-read.csv("C:Ben.data//beachseine_calvert_NB//netdimensions_nb.csv")

#head(ben_netdimensions)
#head(ben_fish_data)
#head(ben_bycatch_data)

head(ben_fish_data)
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


# Net dimensions ----------------------------------------------------------
# take net dimnesion data, make it long for each set to match the other files

ben_netdimensions<-ben_netdimensions %>% gather(replicate, volume, vol_set1,vol_set2)
ben_netdimensions$replicate[ben_netdimensions$replicate=="vol_set1"]<-"1"
ben_netdimensions$replicate[ben_netdimensions$replicate=="vol_set2"]<-"2"
head(ben_netdimensions)

# use only summer months data (July and August 7&8)
#use the dnetdimensions where we don't estimate missing data
# mean net volume used at that site in the summer
ben_netdimensions$volume<-as.numeric(ben_netdimensions$volume)
ben_netdimensions_year <-ben_netdimensions %>% filter(between(month, 5,8)) %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
ben_netdimensions_year <- ben_netdimensions_year[,c(1,5)]
head(ben_netdimensions_year)

#sum of total net volume in given summer
ben_netdimensions_summer <-ben_netdimensions%>% filter(between(month, 5,8)) %>% group_by(site, year) %>% 
  summarise(sum_volume = sum(volume, na.rm=TRUE)) 
ben_netdimensions_summer$sum_volume[ben_netdimensions_summer$sum_volume==0]<-"NA"
head(ben_netdimensions_summer)
ben_netdimensions_summer$sum_volume<-as.numeric(ben_netdimensions_summer$sum_volume)


# Fish data cleaning ------------------------------------------------------
#View(ben_fish_data)
#only summer months 
ben_fish_data <- ben_fish_data %>% filter(between(month, 5,8))


#make wide format to calculate number of species
#use sum of abundance to reflect total of all fish caught in a given summer 
ben_fish_data_wide_year <-ben_fish_data %>% group_by(site, year, species) %>% 
  summarise(sum_abundance = sum(abundance, na.rm=TRUE)) %>% 
  spread( species, sum_abundance) %>% 
  replace(is.na(.), 0) 

head(ben_fish_data_wide_year)

#calculate richness & abundance  from the wide dataframe
ben_fish_data_wide_year_richness<-ben_fish_data_wide_year[,c(1,2)]
ben_fish_data_wide_year_richness$fish_richness<-specnumber(ben_fish_data_wide_year[,-c(1,2)])
#ben_fish_data_wide_year_richness$fish_diversity<-diversity(ben_fish_data_wide_year[,-1], index="shannon")
ben_fish_data_wide_year_richness$fish_abundance<-rowSums(ben_fish_data_wide_year[,-c(1,2)],na.rm = TRUE)
#head(ben_fish_data_wide_year_richness)

#adjust richness and abundance by total volume seined at that site
#bym3 is correcting problem that some sites were sampled more than others (aka species/Area curve)
ben_fish_data_wide_year_richness<-merge(ben_fish_data_wide_year_richness, ben_netdimensions_summer)
ben_fish_data_wide_year_richness$fish_richness_bym3<-ben_fish_data_wide_year_richness$fish_richness/(ben_fish_data_wide_year_richness$sum_volume)
ben_fish_data_wide_year_richness$fish_abundance_bym3<-ben_fish_data_wide_year_richness$fish_abundance/(ben_fish_data_wide_year_richness$sum_volume)

#average these values across years
ben_fish_data_wide_year_richness <- ben_fish_data_wide_year_richness[,-2] %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
#head(ben_fish_data_wide_year_richness)

###demersal vs. pelagic

#head(ben_fish_data)
head(ben_pelagic_demersal_data)
names(ben_pelagic_demersal_data)[1]<-"species"

ben_pelagic_demersal_data2<-merge(ben_pelagic_demersal_data,ben_fish_data, by="species" )
ben_pelagic_data <- ben_pelagic_demersal_data2 %>% filter(Depth.Behaviour=="Pelagic")
ben_demersal_data <- ben_pelagic_demersal_data2 %>% filter(Depth.Behaviour=="Demersal")

ben_demersal_data_wide_year <-ben_demersal_data %>% group_by(site, year, species) %>% 
  summarise(sum_abundance = sum(abundance, na.rm=TRUE)) %>% 
  spread( species, sum_abundance) %>% 
  replace(is.na(.), 0) 

#head(ben_demersal_data_wide_year)

#calculate richness & abundance  from the wide dataframe
ben_demersal_data_wide_year_richness<-ben_demersal_data_wide_year[,c(1,2)]
ben_demersal_data_wide_year_richness$demersal_richness<-specnumber(ben_demersal_data_wide_year[,-c(1,2)])
#ben_demersal_data_wide_year_richness$demersal_diversity<-diversity(ben_demersal_data_wide_year[,-1], index="shannon")
ben_demersal_data_wide_year_richness$demersal_abundance<-rowSums(ben_demersal_data_wide_year[,-c(1,2)],na.rm = TRUE)
#head(ben_demersal_data_wide_year_richness)

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

#head(ben_pelagic_data_wide_year)

#calculate richness & abundance  from the wide dataframe
ben_pelagic_data_wide_year_richness<-ben_pelagic_data_wide_year[,c(1,2)]
ben_pelagic_data_wide_year_richness$pelagic_richness<-specnumber(ben_pelagic_data_wide_year[,-c(1,2)])
#ben_pelagic_data_wide_year_richness$pelagic_diversity<-diversity(ben_pelagic_data_wide_year[,-1], index="shannon")
ben_pelagic_data_wide_year_richness$pelagic_abundance<-rowSums(ben_pelagic_data_wide_year[,-c(1,2)],na.rm = TRUE)
#head(ben_pelagic_data_wide_year_richness)

#adjust richness and abundance by total volume seined at that site
#bym3 is correcting problem that some sites were sampled more than others (aka species/Area curve)
ben_pelagic_data_wide_year_richness<-merge(ben_pelagic_data_wide_year_richness, ben_netdimensions_summer)
ben_pelagic_data_wide_year_richness$pelagic_richness_bym3<-ben_pelagic_data_wide_year_richness$pelagic_richness/(ben_pelagic_data_wide_year_richness$sum_volume)
ben_pelagic_data_wide_year_richness$pelagic_abundance_bym3<-ben_pelagic_data_wide_year_richness$pelagic_abundance/(ben_pelagic_data_wide_year_richness$sum_volume)

#average these values across years
ben_pelagic_data_wide_year_richness <- ben_pelagic_data_wide_year_richness[,-2] %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
#head(ben_pelagic_data_wide_year_richness)


# Bycatch data cleaning ---------------------------------------------------
#data cleaning 
#head(ben_bycatch_data)
names(ben_bycatch_data)[1]<-"site"
ben_bycatch_data[, 7] <- as.numeric(as.character( ben_bycatch_data[, 7] ))

#summer months only 
ben_bycatch_data <-ben_bycatch_data %>% filter(between(month, 5,8))

#same as above
ben_bycatch_data_wide_year <-ben_bycatch_data %>%  replace(is.na(.), 0) %>% group_by(site, year, species) %>% 
  summarise(sum_abundance = sum(estimate, na.rm=TRUE)) %>% 
  spread( species, sum_abundance) %>% 
  replace(is.na(.), 0)

#head(ben_bycatch_data_wide_year)
ben_bycatch_data_wide_year_richness<-ben_bycatch_data_wide_year[,c(1,2)]
ben_bycatch_data_wide_year_richness$bycatch_richness<-specnumber(ben_bycatch_data_wide_year[,-c(1,2)])
ben_bycatch_data_wide_year_richness$bycatch_abundance<-rowSums(ben_bycatch_data_wide_year[,-c(1,2)],na.rm = TRUE)
#head(ben_bycatch_data_wide_year_richness)

ben_bycatch_data_wide_year_richness<-merge(ben_bycatch_data_wide_year_richness, ben_netdimensions_summer)
ben_bycatch_data_wide_year_richness$bycatch_richness_bym3<-ben_bycatch_data_wide_year_richness$bycatch_richness/(ben_bycatch_data_wide_year_richness$sum_volume)
ben_bycatch_data_wide_year_richness$bycatch_abundance_bym3<-ben_bycatch_data_wide_year_richness$bycatch_abundance/(ben_bycatch_data_wide_year_richness$sum_volume)

ben_bycatch_data_wide_year_richness <- ben_bycatch_data_wide_year_richness[,-2] %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
#head(ben_bycatch_data_wide_year_richness)

# Fish Biomass data cleaning ----------------------------------------------

#This is the length data -- adding back in the by replicate data ... makes more sense. 
ben_size_data2 <-ben_size_data %>% filter(between(month, 5,8))
ben_size_data2[, 8] <- as.numeric(as.character( ben_size_data2[, 8] ))

str(ben_size_data2)

ben_size_data_wide_year <-ben_size_data2 %>%  replace(is.na(.), 0) %>% group_by(site, year, month, day, replicate, species) %>% 
  summarise(fish_length = mean(length, na.rm=TRUE)) %>% 
  spread( species, fish_length) %>% 
  replace(is.na(.), 0)

head(ben_size_data_wide_year)
ben_size_data_wide_year_richness<-ben_size_data_wide_year[,c(1,2, 3, 4, 5)]
ben_size_data_wide_year_richness$fish_length<-rowMeans(ben_size_data_wide_year[,-c(1,2,3,4,5)],na.rm = TRUE)
ben_size_data_wide_year_richness$fish_sd<-rowSds(as.matrix(ben_size_data_wide_year[,-c(1,2,3,4,5)]),na.rm = TRUE)
head(ben_size_data_wide_year_richness)

ben_size_data_wide_year_richness <- ben_size_data_wide_year_richness[,-c(2,3,4,5)] %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(ben_size_data_wide_year_richness)

### Now adding in the biomass data from genevieve - fish base ... times the length by the LW relationship for that species
#Also do this by replicate
head(ben_weights)
ben_weights<-ben_weights[,-c(15,16,17,18, 19)]
ben_weights2 <-ben_weights %>% filter(between(month, 5,8))

ben_weights_wide_year <-ben_weights2 %>%  replace(is.na(.), 0) %>% group_by(site, year, month, day, replicate, species) %>% 
  summarise(fish_weight = mean(Weight..g., na.rm=TRUE)) %>% 
  spread( species, fish_weight) %>% 
  replace(is.na(.), 0)

#481 x 97 
head(ben_weights_wide_year)

ben_weights_wide_year_richness<-ben_weights_wide_year[,c(1,2,3,4,5)]
ben_weights_wide_year_richness$fish_av_weight<-rowMeans(ben_weights_wide_year[,-c(1,2,3,4,5)],na.rm = TRUE)
ben_weights_wide_year_richness$fish_weight_sd<-rowSds(as.matrix(ben_weights_wide_year[,-c(1,2,3,4,5)]),na.rm = TRUE)
head(ben_weights_wide_year_richness)

ben_weights_wide_year_richness <- ben_weights_wide_year_richness[,-c(2,3,4,5)] %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(ben_weights_wide_year_richness)

ben_fish_data_wide_year_2 <-ben_fish_data %>% group_by(site, year, month, day, replicate, species) %>% 
  summarise(sum_abundance = sum(abundance, na.rm=TRUE)) %>% 
  spread( species, sum_abundance) %>% 
  replace(is.na(.), 0) 

#488 x 97
head(ben_fish_data_wide_year_2)

#biomass is missing 8 rows ... abundance missing 1 rows
ben_fish_data_wide_year_3<-ben_fish_data_wide_year_2
ben_fish_data_wide_year_3$code<-paste(ben_fish_data_wide_year_3$site, ben_fish_data_wide_year_3$year, ben_fish_data_wide_year_3$month, ben_fish_data_wide_year_3$day, ben_fish_data_wide_year_3$replicate)
ben_weights_wide_year_3<-ben_weights_wide_year
ben_weights_wide_year_3$code<-paste(ben_weights_wide_year_3$site, ben_weights_wide_year_3$year, ben_weights_wide_year_3$month, ben_weights_wide_year_3$day, ben_weights_wide_year_3$replicate)

subset(ben_fish_data_wide_year_3, !(code %in% ben_weights_wide_year_3$code))
#8

subset(ben_weights_wide_year_3, !(code %in% ben_fish_data_wide_year_3$code))
#1 

head(ben_weights_wide_year_3$code)
ben_weights_wide_year_4 <- ben_weights_wide_year_3 %>% filter(code!="ppo 2014 6 14 2")
ben_weights_wide_year_4 <- ben_weights_wide_year_4[,-98]

ben_fish_data_wide_year_4 <- ben_fish_data_wide_year_3 %>% filter(! code %in% c("fan1 2018 7 15 2", "hdo 2015 5 3 2", 
                                                                                "ppo 2018 5 29 2", "ris1 2018 7 19 2", 
                                                                                "ssp 2014 7 2 2", "ssp 2016 5 8 2", 
                                                                                "wfb 2014 6 11 1", "wfb 2014 6 11 2"))
ben_fish_data_wide_year_4<- ben_fish_data_wide_year_4[,-98]



#combining size and abundance to get "biomass" estimate
ben_biomass_data_wide_year<-(ben_weights_wide_year_4[,-c(1,2,3,4,5)])*(ben_fish_data_wide_year_4[,-c(1,2,3,4,5)])
ben_biomass_data_wide_year_nf<-ben_weights_wide_year_4[,c(1,2,3,4,5)]
ben_biomass_data_wide_year_nf$fish_biomass<-rowSums(ben_biomass_data_wide_year)
head(ben_biomass_data_wide_year_nf)

#correcting for total net volume
ben_netdimensions_summer4 <-ben_netdimensions%>% filter(between(month, 5,8)) %>% group_by(site, year, month, replicate) %>% 
  summarise(sum_volume = sum(volume, na.rm=TRUE)) 
ben_netdimensions_summer4$sum_volume[ben_netdimensions_summer4$sum_volume==0]<-"NA"
head(ben_netdimensions_summer4)
ben_netdimensions_summer4$sum_volume<-as.numeric(ben_netdimensions_summer4$sum_volume)

 


head(ben_netdimensions_summer)
ben_biomass_data_wide_year_nf<-merge(ben_biomass_data_wide_year_nf, ben_netdimensions_summer4)
ben_biomass_data_wide_year_nf$fish_biomass_bym3<-ben_biomass_data_wide_year_nf$fish_biomass/(ben_biomass_data_wide_year_nf$sum_volume)
ben_biomass_data_wide_year_nf <-ben_biomass_data_wide_year_nf[,-2]%>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(ben_biomass_data_wide_year_nf)

ben_size_data_wide_year_richness <- ben_size_data_wide_year_richness[,-c(2,3,4,5)] %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(ben_size_data_wide_year_richness)

#Pelagic and demersal data - changed to biomass 

ben_pelagic_demersal_weights<-merge(ben_pelagic_demersal_data,ben_weights, by="species" )
#head(ben_pelagic_demersal_weights)


ben_pelagic_weights <- ben_pelagic_demersal_weights %>% filter(Depth.Behaviour=="Pelagic")
ben_demersal_weights <- ben_pelagic_demersal_weights %>% filter(Depth.Behaviour=="Demersal")

ben_demersal_weights2 <-ben_demersal_weights %>% filter(between(month, 5,8))

ben_demersal_weights_wide_year <-ben_demersal_weights2 %>%  replace(is.na(.), 0) %>% group_by(site, year, species) %>% 
  summarise(fish_weight = mean(Weight..g., na.rm=TRUE)) %>% 
  spread( species, fish_weight) %>% 
  replace(is.na(.), 0)

#combining size and abundance to get "demersal_biomass" estimate
ben_demersal_biomass_data_wide_year<-(ben_demersal_weights_wide_year[,-c(1,2)])*(ben_demersal_data_wide_year[,-c(1,2)])
ben_demersal_biomass_data_wide_year_nf<-ben_demersal_weights_wide_year[,c(1,2)]
ben_demersal_biomass_data_wide_year_nf$fish_demersal_biomass<-rowSums(ben_demersal_biomass_data_wide_year)
#head(ben_demersal_biomass_data_wide_year_nf)

#correcting for total net volume
ben_demersal_biomass_data_wide_year_nf<-merge(ben_demersal_biomass_data_wide_year_nf, ben_netdimensions_summer)
ben_demersal_biomass_data_wide_year_nf$fish_demersal_biomass_bym3<-ben_demersal_biomass_data_wide_year_nf$fish_demersal_biomass/(ben_demersal_biomass_data_wide_year_nf$sum_volume)
ben_demersal_biomass_data_wide_year_nf <-ben_demersal_biomass_data_wide_year_nf[,-2]%>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
#head(ben_demersal_biomass_data_wide_year_nf)

###ben size data etc for pelagic and pelagic only 
ben_pelagic_weights2 <-ben_pelagic_weights %>% filter(between(month, 5,8))

ben_pelagic_weights_wide_year <-ben_pelagic_weights2 %>%  replace(is.na(.), 0) %>% group_by(site, year, species) %>% 
  summarise(fish_weight = mean(Weight..g., na.rm=TRUE)) %>% 
  spread( species, fish_weight) %>% 
  replace(is.na(.), 0)

#combining size and abundance to get "pelagic_biomass" estimate
ben_pelagic_biomass_data_wide_year<-(ben_pelagic_size_data_wide_year[,-c(1,2)])*(ben_pelagic_data_wide_year[,-c(1,2)])
ben_pelagic_biomass_data_wide_year_nf<-ben_pelagic_size_data_wide_year[,c(1,2)]
ben_pelagic_biomass_data_wide_year_nf$fish_pelagic_biomass<-rowSums(ben_pelagic_biomass_data_wide_year)
#head(ben_pelagic_biomass_data_wide_year_nf)

#correcting for total net volume
ben_pelagic_biomass_data_wide_year_nf<-merge(ben_pelagic_biomass_data_wide_year_nf, ben_netdimensions_summer)
ben_pelagic_biomass_data_wide_year_nf$fish_pelagic_biomass_bym3<-ben_pelagic_biomass_data_wide_year_nf$fish_pelagic_biomass/(ben_pelagic_biomass_data_wide_year_nf$sum_volume)
ben_pelagic_biomass_data_wide_year_nf <-ben_pelagic_biomass_data_wide_year_nf[,-2]%>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(ben_pelagic_biomass_data_wide_year_nf)

# Merging fish, bycatch, biomass, net dimensions ------------------------------------------
#merging files together into one data frame
head(ben_fish_data_wide_year_richness)
head(ben_bycatch_data_wide_year_richness)
head(ben_size_data_wide_year_richness)
head(ben_weights_wide_year_richness)

head(ben_biomass_data_wide_year_nf)
head(ben_pelagic_biomass_data_wide_year_nf)
head(ben_netdimensions_year)

fish_richness_merged_tran_year<-merge(ben_fish_data_wide_year_richness, ben_netdimensions_year, by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_richness_merged_tran_year, ben_bycatch_data_wide_year_richness[,-4], by="site", all=TRUE)
# fish_bycatch_richness_merged_tran_year[is.na(fish_bycatch_richness_merged_tran_year)] <- 0
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_size_data_wide_year_richness, by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_weights_wide_year_richness, by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_biomass_data_wide_year_nf[,-3], by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_pelagic_data_wide_year_richness[,-4], by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_demersal_data_wide_year_richness[,-4], by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_demersal_biomass_data_wide_year_nf[,-3], by="site", all=TRUE)
fish_bycatch_richness_merged_tran_year<-merge(fish_bycatch_richness_merged_tran_year, ben_pelagic_biomass_data_wide_year_nf[,-3], by="site", all=TRUE)

head(fish_bycatch_richness_merged_tran_year)

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

#head(fish_bycatch_richness_merged_tran_year)
setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Biodiversity idea")
write.csv(fish_bycatch_richness_merged_tran_year, "C:Output files//fish_bycatch_richness_merged_tran_year.csv")


# Matching terrestrial transects to beachseine sites ----------------------

#This is one option: 
# #pulls from output files of R script - "Assigned points"
# hakai_sites_distance_tran<-read.csv("C:Output files//Distance_btwn_points_transects.csv")
# hakai_sites_distance_tran<-hakai_sites_distance_tran[,-1]
# #head(hakai_sites_distance_tran)
# names(hakai_sites_distance_tran)[3]<-"unq_tran"
# names(hakai_sites_distance_tran)[6]<-"site"
# hakai_sites_distance_tran<- hakai_sites_distance_tran%>% filter(Distance < 5)
# #head(hakai_sites_distance_tran )


#This is working with a 1km radius around the transects instead
hakai_sites_distance_tran<-read.csv("C:Output files//paired_sites_by_radius.csv")
hakai_sites_distance_tran<-hakai_sites_distance_tran[,-1]
head(hakai_sites_distance_tran)

fish_bycatch_richness_merged_tran<-merge(fish_bycatch_richness_merged_tran_year, hakai_sites_distance_tran, by="site")

head(fish_bycatch_richness_merged_tran)

# Loading and merging terrestrial data (at 0m) by transect ------------------------------------

#transect data
setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Food web idea")
by_tran_master_0m<-read.csv("C:Data by person\\Norah.data\\by_tran_master_0m.csv")
head(by_tran_master_0m)
by_tran_master_0m<-by_tran_master_0m[,-1]

### adding in tree diversity (transect level)
by_tran_master<-read.csv("C:Data by person\\Norah.data\\by_tran_master.csv")
#head(by_tran_master)
by_tran_master<-by_tran_master[,-1]
which( colnames(by_tran_master)=="tree_richness" )
which( colnames(by_tran_master)=="tree_abundance")
which( colnames(by_tran_master)=="sum_basal")
# which( colnames(by_tran_master)=="shrub_richness" )
# which( colnames(by_tran_master)=="shrub_cover")
# which( colnames(by_tran_master)=="herb_richness" )
# which( colnames(by_tran_master)=="herb_cover")

by_tran_master_subset<-by_tran_master[,c(1,15,18,19)]
head(by_tran_master_subset)



#adding in a few interesting island-level components
by_isl_master<-read.csv("C:Data by person\\Owen's data//by_isl_master.csv")
by_isl_master<-by_isl_master[,-1]
#head(by_isl_master)
paste(
  which( colnames(by_isl_master)=="unq_isl" ),
  which( colnames(by_isl_master)=="mammal_richness" ),
  which( colnames(by_isl_master)=="bird.richness" ),
  which( colnames(by_isl_master)=="habitat_het" ),
  which( colnames(by_isl_master)=="log_Area" ),
  which( colnames(by_isl_master)=="Neighb_250" ),
  which( colnames(by_isl_master)=="NDVI_mean" ),
  which( colnames(by_isl_master)=="Perimeter" ),
  which( colnames(by_isl_master)=="PA_norml" ),
  which( colnames(by_isl_master)=="DistW_ML" ),
  which( colnames(by_isl_master)=="Dist_Near" ),
  which( colnames(by_isl_master)=="Area" ),
  sep=","
)

by_isl_master_subset<-by_isl_master[,c(1,97,47,102,98,19,20,14,15,17,18,13)]
#head(by_isl_master_subset)

by_tran_master_0m_with_isl<-merge(by_tran_master_0m, by_isl_master_subset, by="unq_isl", all=TRUE)
by_tran_master_0m_with_isl<-merge(by_tran_master_0m_with_isl, by_tran_master_subset, by="unq_tran", all=TRUE)


head(by_tran_master_0m_with_isl)


#head(fish_bycatch_richness_merged_tran)

#merging terrestrial with marine and adding in marine site information, saving file
fish_richness_merged_tran_isl<-merge(fish_bycatch_richness_merged_tran, by_tran_master_0m_with_isl, by="unq_tran", all.y=TRUE)
fish_richness_merged_tran_isl<-merge(fish_richness_merged_tran_isl, ben_habitat_data, by="site")
head(fish_richness_merged_tran_isl)
setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Biodiversity idea")
write.csv(fish_richness_merged_tran_isl, "C:Output files//fish_richness_merged_tran_isl.csv")

#how many beachseine sites - 12, how many transects - 106
length(unique(fish_richness_merged_tran_isl$site))
length(unique(fish_richness_merged_tran_isl$unq_tran))
#I think I added more after I fixed the issues with Owen's plots! 

# Determining best scale of comparison -----------------------------------------

ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+geom_text(aes(label=unq_tran))

ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl, aes(x=log(fish_abundance_bym3+1), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")

ggplot(fish_richness_merged_tran_isl, aes(x=fish_av_weight, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=d15n, col=PA_norml))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_c()
ggplot(fish_richness_merged_tran_isl, aes(x=log(fish_abundance_bym3+1), y=d15n, col=PA_norml))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_c()

ggplot(fish_richness_merged_tran_isl, aes(x=log(fish_abundance_bym3+1), y=d15n))+geom_point()+geom_smooth(aes(),method="gam")




 head(fish_richness_merged_tran_isl)

#plotting the relationship between n15 and fish richness at various distances
n15_100<-ggplot(fish_richness_merged_tran_isl_100, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("100 m")
n15_250<-ggplot(fish_richness_merged_tran_isl_250, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("250 m")
n15_300<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("300 m")
n15_400<-ggplot(fish_richness_merged_tran_isl_400, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("400 m")
n15_500<-ggplot(fish_richness_merged_tran_isl_500, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("500 m")
n15_600<-ggplot(fish_richness_merged_tran_isl_600, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("600 m")
n15_850<-ggplot(fish_richness_merged_tran_isl_850, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("850 m")
n15_3k<-ggplot(fish_richness_merged_tran_isl_3k, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("3k")
n15_2k<-ggplot(fish_richness_merged_tran_isl_2k, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("2k")
n15_1000<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("1000 m")
plot_grid(n15_100, n15_250, n15_300, n15_400, n15_500,n15_600,n15_850, n15_1000,n15_2k,n15_3k,  ncol=5)
ggsave("C:Plots//Transect//Resources_terr_var//fish_richness_n15_scale_by_day_volume.png", width=40, height=20, unit="cm")

#plotting the relationship between n15 and fish biomass at various distances
n15_100<-ggplot(fish_richness_merged_tran_isl_100, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("100 m")
n15_250<-ggplot(fish_richness_merged_tran_isl_250, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("250 m")
n15_300<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("300 m")
n15_400<-ggplot(fish_richness_merged_tran_isl_400, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("400 m")
n15_500<-ggplot(fish_richness_merged_tran_isl_500, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("500 m")
n15_600<-ggplot(fish_richness_merged_tran_isl_600, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("600 m")
n15_850<-ggplot(fish_richness_merged_tran_isl_850, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("850 m")
n15_3k<-ggplot(fish_richness_merged_tran_isl_3k, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("3k")
n15_2k<-ggplot(fish_richness_merged_tran_isl_2k, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("2k")
n15_1000<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("1000 m")
plot_grid(n15_100, n15_250, n15_300, n15_400, n15_500,n15_600,n15_850, n15_1000,n15_2k,n15_3k,  ncol=5)
ggsave("C:Plots//Transect//Resources_terr_var//fish_biomass_n15_scale_by_day_volume.png", width=40, height=20, unit="cm")



#plotting the relationship between s and fish richness at various distances
s_100<-ggplot(fish_richness_merged_tran_isl_100, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("100 m")
s_250<-ggplot(fish_richness_merged_tran_isl_250, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("250 m")
s_300<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("300 m")
s_400<-ggplot(fish_richness_merged_tran_isl_400, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("400 m")
s_500<-ggplot(fish_richness_merged_tran_isl_500, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("500 m")
s_600<-ggplot(fish_richness_merged_tran_isl_600, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("600 m")
s_750<-ggplot(fish_richness_merged_tran_isl_750, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("750 m")
s_4k<-ggplot(fish_richness_merged_tran_isl_4k, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("4k")
s_5k<-ggplot(fish_richness_merged_tran_isl_5k, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("5k")
s_3k<-ggplot(fish_richness_merged_tran_isl_3k, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("3k")
s_2k<-ggplot(fish_richness_merged_tran_isl_2k, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("2k")
s_1000<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("1k")
plot_grid(s_100, s_250, s_300, s_400, s_500,s_600,s_750,s_1000, s_2k,s_3k,s_4k,s_5k , ncol=6)
ggsave("C:Plots//Transect//Resources_terr_var//fish_richness_s_scale_by_day_volume.png", width=40, height=20, unit="cm")


s_100<-ggplot(fish_richness_merged_tran_isl_100, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("100 m")
s_250<-ggplot(fish_richness_merged_tran_isl_250, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("250 m")
s_300<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("300 m")
s_400<-ggplot(fish_richness_merged_tran_isl_400, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("400 m")
s_500<-ggplot(fish_richness_merged_tran_isl_500, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("500 m")
s_600<-ggplot(fish_richness_merged_tran_isl_600, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("600 m")
s_750<-ggplot(fish_richness_merged_tran_isl_750, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("750 m")
s_4k<-ggplot(fish_richness_merged_tran_isl_4k, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("4k")
s_5k<-ggplot(fish_richness_merged_tran_isl_5k, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("5k")
s_3k<-ggplot(fish_richness_merged_tran_isl_3k, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("3k")
s_2k<-ggplot(fish_richness_merged_tran_isl_2k, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("2k")
s_1000<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("1k")
plot_grid(s_100, s_250, s_300, s_400, s_500,s_600,s_750,s_1000, s_2k,s_3k,s_4k,s_5k , ncol=6)
ggsave("C:Plots//Transect//Resources_terr_var//fish_biomass_s_scale_by_day_volume.png", width=40, height=20, unit="cm")

## For both marine indicators, 300m jumps out as tighest relationship
##but 1km might be more reasonable for otter feeding .. think about this. 



# Plotting Chemistry by marine richness ---------------------------------------

#soil chemistry
marine_d15n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="gam")
marine_d13c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d13c))+geom_point()+geom_smooth(method="gam")
marine_c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=c))+geom_point()+geom_smooth(method="gam")
marine_n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=n))+geom_point()+geom_smooth(method="lm")
marine_cn<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=cn))+geom_point()+geom_smooth(method="gam")
marine_s<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=s))+geom_point()+geom_smooth(method="gam")
plot_grid(marine_d15n, marine_d13c, marine_c, marine_n, marine_cn, marine_s,ncol=3)
ggsave("C:Plots//Transect//Chemistry//marine_richness_soilchem.png", width=40, height=20, unit="cm")


#salal chemistry 
gash_d15n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n_gash))+geom_point()+geom_smooth(method="lm")
gash_d13c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d13c_gash))+geom_point()+geom_smooth(method="lm")
gash_c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=c_gash))+geom_point()+geom_smooth(method="lm")
gash_n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=n_gash))+geom_point()+geom_smooth(method="lm")
gash_cn<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=cn_gash))+geom_point()+geom_smooth(method="lm")
gash_s<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=s_gash))+geom_point()+geom_smooth(method="lm")
plot_grid(gash_d15n, gash_d13c, gash_c, gash_n, gash_cn, gash_s,ncol=3)
ggsave("C:Plots//Transect//Chemistry//marine_richness_gash.png", width=40, height=20, unit="cm")

#myanthemum chemistry
midi_d15n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n_midi))+geom_point()+geom_smooth(method="lm")
midi_d13c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d13c_midi))+geom_point()+geom_smooth(method="lm")
midi_c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=c_midi))+geom_point()+geom_smooth(method="lm")
midi_n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=n_midi))+geom_point()+geom_smooth(method="lm")
midi_cn<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=cn_midi))+geom_point()+geom_smooth(method="lm")
midi_s<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=s_midi))+geom_point()+geom_smooth(method="lm")
plot_grid(midi_d15n, midi_d13c, midi_c, midi_n, midi_cn, midi_s,ncol=3)
ggsave("C:Plots//Transect//Chemistry//marine_richness_midi.png", width=40, height=20, unit="cm")

### beetle chemistry
beetles_d15n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n_beetles))+geom_point()+geom_smooth(method="lm")
beetles_d13c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d13c_beetles))+geom_point()+geom_smooth(method="lm")
beetles_c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=c_beetles))+geom_point()+geom_smooth(method="lm")
beetles_n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=n_beetles))+geom_point()+geom_smooth(method="lm")
beetles_cn<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=cn_beetles))+geom_point()+geom_smooth(method="lm")
plot_grid(beetles_d15n, beetles_d13c, beetles_c, beetles_n, beetles_cn,ncol=3)
ggsave("C:Plots//Transect//Chemistry//marine_richness_beetles.png", width=40, height=20, unit="cm")

###weevil chemistry
weevils_d15n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n_weevils))+geom_point()+geom_smooth(method="lm")
weevils_d13c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d13c_weevils))+geom_point()+geom_smooth(method="lm")
weevils_c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=c_weevils))+geom_point()+geom_smooth(method="lm")
weevils_n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=n_weevils))+geom_point()+geom_smooth(method="lm")
weevils_cn<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=cn_weevils))+geom_point()+geom_smooth(method="lm")
plot_grid(weevils_d15n, weevils_d13c, weevils_c, weevils_n, weevils_cn,ncol=3)
ggsave("C:Plots//Transect//Chemistry//marine_richness_weevils.png", width=40, height=20, unit="cm")

ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n_beetles))+geom_point()+geom_smooth(method="lm")+geom_text(aes(label=unq_tran))
ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n_weevils))+geom_point()+geom_smooth(method="lm")+geom_text(aes(label=unq_tran))
ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")+geom_text(aes(label=unq_tran))


### isopods chemistry
isopods_d15n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")
isopods_d13c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d13c_isopods))+geom_point()+geom_smooth(method="lm")
isopods_c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=c_isopods))+geom_point()+geom_smooth(method="lm")
isopods_n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=n_isopods))+geom_point()+geom_smooth(method="lm")
isopods_cn<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=cn_isopods))+geom_point()+geom_smooth(method="lm")
plot_grid(isopods_d15n, isopods_d13c, isopods_c, isopods_n, isopods_cn,ncol=3)
ggsave("C:Plots//Transect//Chemistry//marine_richness_isopods.png", width=40, height=20, unit="cm")


# Terrestrial ecology and marine richness ---------------------------------
#head(fish_richness_merged_tran_isl_300)

ggplot(fish_richness_merged_tran_isl_300, aes(y=shrub_cover, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
ggplot(fish_richness_merged_tran_isl_1k, aes(y=shrub_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")


ggplot(fish_richness_merged_tran_isl_300, aes(y=herb_cover, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
ggplot(fish_richness_merged_tran_isl_300, aes(y=herb_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_tran_isl_300, aes(y=plant_evenness, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
ggplot(fish_richness_merged_tran_isl_300, aes(y=plant_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_tran_isl_300, aes(y=plant_evenness, x=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(fish_richness_merged_tran_isl_300, aes(y=plant_richness, x=d15n))+geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_evenness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))
ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_birdfood_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson")) 
ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_detritivore_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))
ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_carnivore_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson")) 

ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_detritivore_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_detritivore_abs_abundance, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_detritivore_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_evenness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))
ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_birdfood_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson")) 
ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_detritivore_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))
ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_carnivore_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson")) 
ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_herbivore_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))
ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_parasite_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))
ggplot(fish_richness_merged_tran_isl_300, aes(y=insect_omnivore_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))

ggplot(fish_richness_merged_tran_isl_1k, aes(y=log(insect_abs_abundance+1), x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(fish_richness_merged_tran_isl_300, aes(y=(insect_beat_av_abundance), x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(fish_richness_merged_tran_isl_300, aes(y=(insect_pitfall_av_abundance), x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))

ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=pelagic_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=demersal_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))


View(fish_richness_merged_tran_isl_300)

#transect level
marine_plant<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=plant.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_pc1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")
marine_tree<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_treeabun<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
marine_insect<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_insectabund<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=log(insect_abs_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_plant,marine_pc1, marine_tree,marine_treeabun,marine_insect,marine_insectabund,ncol=3)
ggsave("C:Plots//Transect//Resources_terr_var//marine_richness_terrestrial.png", width=40, height=20, unit="cm")


###Fish richness, fish 
fish_plant_richness<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_plant_evenness<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=plant_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_plant_total_cover<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=total_cover))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_pc1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")+theme_classic()

fish_plant_richness2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_plant_evenness2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=plant_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_plant_total_cover2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=total_cover))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_pc12<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=pc1))+geom_point()+geom_smooth(method="lm")+theme_classic()

plot_grid(fish_plant_richness, fish_plant_evenness, fish_plant_total_cover, fish_pc1, 
          fish_plant_richness2, fish_plant_evenness2, fish_plant_total_cover2, fish_pc12, ncol=4)
ggsave("C:Plots//Compiled plots//Plants_fish.png", width=40, height=20, unit="cm")

###Plant_fish
fish_plant_richness<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_plant_evenness<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=plant_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_plant_total_cover<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=total_cover))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_pc1<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")+theme_classic()

fish_plant_richness2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_plant_evenness2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=plant_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_plant_total_cover2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=total_cover))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_pc12<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=pc1))+geom_point()+geom_smooth(method="lm")+theme_classic()

plot_grid(fish_plant_richness, fish_plant_evenness, fish_plant_total_cover, fish_pc1, 
          fish_plant_richness2, fish_plant_evenness2, fish_plant_total_cover2, fish_pc12, ncol=4)
ggsave("C:Plots//Compiled plots//Plants_fish_1k.png", width=40, height=20, unit="cm")


###Tree, fish richness
fish_tree_richness<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_tree_abundance<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_sum_basal<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=sum_basal))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_NDVI<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")+theme_classic()

fish_tree_richness2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_tree_abundance2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=tree_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_sum_basal2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=sum_basal))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_NDVI2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")+theme_classic()

plot_grid(fish_tree_richness, fish_tree_abundance, fish_sum_basal, fish_NDVI,  
          fish_tree_richness2, fish_tree_abundance2, fish_sum_basal2, fish_NDVI2,  ncol=4)
ggsave("C:Plots//Compiled plots//trees_fish.png", width=40, height=20, unit="cm")

##Tree, fish
fish_tree_richness<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_tree_abundance<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_sum_basal<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=sum_basal))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_NDVI<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")+theme_classic()

fish_tree_richness2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_tree_abundance2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=tree_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_sum_basal2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=sum_basal))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_NDVI2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")+theme_classic()

plot_grid(fish_tree_richness, fish_tree_abundance, fish_sum_basal, fish_NDVI,
          fish_tree_richness2, fish_tree_abundance2, fish_sum_basal2, fish_NDVI2, ncol=4)
ggsave("C:Plots//Compiled plots//trees_fish_1k.png", width=40, height=20, unit="cm")



###insect, fish richness
fish_insect_richness<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_insect_abs_abundance<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=insect_abs_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_evenness<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=insect_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_pitfall_av_abundance<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=insect_pitfall_av_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()

fish_insect_richness2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_insect_abs_abundance2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=insect_abs_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_evenness2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=insect_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_pitfall_av_abundance2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=insect_pitfall_av_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()

plot_grid(fish_insect_richness, fish_insect_abs_abundance, fish_insect_pitfall_av_abundance, fish_insect_evenness,
          fish_insect_richness2, fish_insect_abs_abundance2, fish_insect_pitfall_av_abundance2, fish_insect_evenness2,  ncol=4)
ggsave("C:Plots//Compiled plots//insects_fish.png", width=40, height=20, unit="cm")

##insect, fish
fish_insect_richness<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_insect_abs_abundance<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=insect_abs_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_evenness<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=insect_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_pitfall_av_abundance<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=insect_pitfall_av_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()

fish_insect_richness2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_insect_abs_abundance2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=insect_abs_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_evenness2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=insect_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_pitfall_av_abundance2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=insect_pitfall_av_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()

plot_grid(fish_insect_richness, fish_insect_abs_abundance, fish_insect_pitfall_av_abundance, fish_insect_evenness,  
          fish_insect_richness2, fish_insect_abs_abundance2, fish_insect_pitfall_av_abundance2, fish_insect_evenness2,  ncol=4)
ggsave("C:Plots//Compiled plots//insects_fish_1k.png", width=40, height=20, unit="cm")




###birds, fish richness
fish_mammal_richness<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_bird.richness<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()

fish_mammal_richness2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=mammal_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_bird.richness2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()

plot_grid(fish_mammal_richness, fish_bird.richness,
          fish_mammal_richness2, fish_bird.richness2,  ncol=2)
ggsave("C:Plots//Compiled plots//birds_fish.png", width=40, height=20, unit="cm")


###birds, fish richness
fish_mammal_richness<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_bird.richness<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()

fish_mammal_richness2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=mammal_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_bird.richness2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()

plot_grid(fish_mammal_richness, fish_bird.richness,
          fish_mammal_richness2, fish_bird.richness2,  ncol=2)
ggsave("C:Plots//Compiled plots//birds_fish_1k.png", width=40, height=20, unit="cm")















#just by fish richness
fish_plant<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=plant.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_pc1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")
fish_tree<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_treeabun<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
fish_insect<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_insectabund<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=log(insect_abs_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(fish_plant,fish_pc1, fish_tree,fish_treeabun,fish_insect,fish_insectabund,ncol=3)
ggsave("C:Plots//Transect//Resources_terr_var//fish_richness_terrestrial.png", width=40, height=20, unit="cm")

#just by fish richness
fish_plant<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=plant.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_pc1<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")
fish_tree<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_treeabun<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
fish_insect<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_insectabund<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=log(insect_abs_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(fish_plant,fish_pc1, fish_tree,fish_treeabun,fish_insect,fish_insectabund,ncol=3)
ggsave("C:Plots//Transect//Resources_terr_var//fish_richness_terrestrial_1k.png", width=40, height=20, unit="cm")




###Just insects # wait until I get the beetle data - b/c can't do it w/o beetles/weevils
# marine_beetles<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
# marine_isopods<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=crustacea_richness))+geom_point()+geom_smooth(method="lm")
# ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=crustacea_abundance))+geom_point()+geom_smooth(method="lm")
# plot_grid(marine_plant,marine_pc1, marine_tree,marine_treeabun,marine_insect,marine_insectabund,ncol=3)
# ggsave("C:Plots//Transect//Resources_terr_var//marine_richness_terrestrial.png", width=40, height=20, unit="cm")


# island level
marine_bird<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_habitathet<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
marine_NDVI<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
marine_mammal<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_bird,marine_mammal, marine_habitathet,marine_NDVI, ncol=4)
ggsave("C:Plots//Transect//Resources_terr_var//marine_richness_terrestrial_isl.png", width=40, height=20, unit="cm")




# Plotting marine resources vs. marine variables----------------------------------------------------------------

setwd("C:/Users/norahbrown/Dropbox/Projects/100 islands/Biodiversity idea")

#Just marine variables to eachother
marine1<-ggplot(fish_bycatch_richness_merged_tran_year, aes(x=fish_richness_corrected, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
marine2<-ggplot(fish_bycatch_richness_merged_tran_year, aes(x=log(fish_abundance_bym3), y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine3<-ggplot(fish_bycatch_richness_merged_tran_year, aes(x=fish_richness_corrected, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
marine4<-ggplot(fish_bycatch_richness_merged_tran_year, aes(x=bycatch_richness_corrected, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")



marine16<-ggplot(fish_bycatch_richness_merged_tran_year, aes(x=wrack_richness, y=log_site_sum_by_tran))+geom_point()+geom_smooth(method="lm")
marine17<-ggplot(fish_bycatch_richness_merged_tran_year, aes(x=fish_richness_corrected, y=fish_length))+geom_point()+geom_smooth(method="lm")
plot_grid(marine1, marine2, marine3, marine17, ncol=2)
ggsave("C:Plots//Transect//Resources_marine_var//marine_plot.png")

ggplot(fish_bycatch_richness_merged_tran_year, aes(x=fish_richness_corrected, y=log(fish_abundance_bym3+1)))+geom_point()+geom_smooth(method="gam")


#nearby habitat abundance on fish and bycatch
marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_HAB2000, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_HAB2000, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine8<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_HAB2000, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
marine9<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_HAB2000, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine10<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_HAB2000, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine11<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_HAB2000, y=log_site_sum_by_isl))+geom_point()+geom_smooth(method="lm")
plot_grid(marine6, marine8, marine11, marine7,marine9,marine10, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//hab2000_fish_bycatch_corrected.png")



marine_hab_1006<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(sum_100m), y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_1007<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(sum_100m), y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_1008<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(sum_100m), y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_1009<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(sum_100m), y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_10010<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(sum_100m), y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_10011<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(sum_100m), y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_1006, marine_hab_1008, marine_hab_10011, marine_hab_1007,marine_hab_1009,marine_hab_10010, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//hab100_fish_bycatch_corrected.png")

marine_hab_5006<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_500m, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_5007<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_500m, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_5008<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_500m, y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_5009<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_500m, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50010<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_500m, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50011<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_500m, y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_5006, marine_hab_5008, marine_hab_50011, marine_hab_5007,marine_hab_5009,marine_hab_50010, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//hab500_fish_bycatch_corrected.png")


marine_hab_5006<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_egarea250, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_5007<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_egarea250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_5008<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_egarea250, y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_5009<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_egarea250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50010<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_egarea250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
plot_grid(marine_hab_5006, marine_hab_5008, marine_hab_5007,marine_hab_5009,marine_hab_50010, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//egra250_fish_bycatch_corrected.png")


#head(fish_richness_merged_tran_isl_300)
marine_hab_kelp6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_kparea250, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_kelp7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_kparea250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_kelp8<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_kparea250, y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_kelp9<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_kparea250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_kelp10<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_kparea250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
plot_grid(marine_hab_kelp6, marine_hab_kelp8, marine_hab_kelp7,marine_hab_kelp9,marine_hab_kelp10, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//habkelp_fish_bycatch_corrected.png")






marine_hab_2506<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_250m, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_2507<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_250m, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_2508<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_250m, y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_2509<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_250m, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_25010<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_250m, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_25011<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_250m, y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_2506, marine_hab_2508, marine_hab_25011, marine_hab_2507,marine_hab_2509,marine_hab_25010, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//hab250_fish_bycatch_corrected.png")


veg_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=bycatch_abundance_bym3))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=bycatch_richness_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=log(fish_abundance_bym3)))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=fish_richness_corrected))+geom_boxplot()+ theme(legend.position=c(0.9,0.90))+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=log(fish_biomass)))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=fish_length))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
plot_grid(veg_marine4, veg_marine5, veg_marine6, veg_marine3, veg_marine2, veg_marine1, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//resource_primary_intertidal_habitat.png")


subtidal_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=bycatch_abundance_bym3))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=bycatch_richness_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=log(fish_abundance_bym3)))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=fish_richness_corrected))+geom_boxplot()+ theme(legend.position=c(0.9,0.90))+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=log(fish_biomass)))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=fish_length))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
plot_grid(subtidal_marine4, subtidal_marine5, subtidal_marine6, subtidal_marine3, subtidal_marine2, subtidal_marine1, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//resource_primary_subtidal_habitat.png")



# Plotting Marine resources vs. terrestrial variables  ---------------------------------------

# marine resources vs. n15 soilk
n_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=log(bycatch_abundance_bym3+1)))+geom_point()+geom_smooth(method="gam")
n_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="gam")
n_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=log(fish_abundance_bym3+1)))+geom_point()+geom_smooth(method="gam")
n_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="gam")
n_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=log(fish_biomass_bym3+1)))+geom_point()+geom_smooth(method="gam")
n_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=fish_length))+geom_point()+geom_smooth(method="gam")
n_marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=marine_richness_corrected))+geom_point()+geom_smooth(method="gam")
n_marine8<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=fish_sd))+geom_point()+geom_smooth(method="gam")

plot_grid(n_marine4, n_marine5, n_marine7, n_marine3, n_marine2, n_marine1,n_marine6,n_marine8, ncol=4)
ggsave("C:Plots//Transect//Resources_terr_var//d15n_marine_corrected.png", width=30, height=20, unit="cm")

s_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=log(bycatch_abundance_bym3+1)))+geom_point()+geom_smooth(method="gam")
s_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="gam")
s_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=log(fish_abundance_bym3+1)))+geom_point()+geom_smooth(method="gam")
s_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=fish_richness_corrected))+geom_point()+geom_smooth(method="gam")
s_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=log(fish_biomass_bym3+1)))+geom_point()+geom_smooth(method="gam")
s_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=fish_length))+geom_point()+geom_smooth(method="gam")
s_marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=marine_richness_corrected))+geom_point()+geom_smooth(method="gam")
s_marine8<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=fish_sd))+geom_point()+geom_smooth(method="gam")

plot_grid(s_marine4, s_marine5, s_marine7, s_marine3, s_marine2, s_marine1,s_marine6,s_marine8, ncol=4)
ggsave("C:Plots//Transect//Resources_terr_var//s_marine_corrected.png", width=30, height=20, unit="cm")


# marine resources vs. n15 soil
n_plant_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
n_plant_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
n_plant_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
n_plant_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
n_plant_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
n_plant_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=fish_length))+geom_point()+geom_smooth(method="lm")
n_plant_marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")
plot_grid(n_plant_marine4, n_plant_marine5, n_plant_marine7, n_plant_marine3, n_plant_marine2, n_plant_marine1, ncol=3)
ggsave("C:Plots//Transect//Resources_terr_var//d15n_plant_marine_corrected.png")




library(mgcv)
####GAM marine resources vs. n15n
n_marine_gam1<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam2<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam3<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam4<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam5<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam6<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=fish_length))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
plot_grid(n_marine_gam4, n_marine_gam5, n_marine_gam6, n_marine_gam3, n_marine_gam2, n_marine_gam1, ncol=3)
ggsave("C:Plots//Transect//Resources_terr_var//d15n_marine_corrected_GAM.png")


#fish abundance
fish1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=d15n))+geom_point()+geom_smooth(method="lm")
fish2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish13<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish19<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish20<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish1,fish2,fish3,fish4,fish13,fish19,fish20, ncol=4)
ggsave("C:Plots//Transect//Resources_terr_var//fish_abund_terrestrial.png")

#fish richness
fish5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish8<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
fish14<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish21<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish22<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish8,fish5,fish6,fish7,fish14,fish21,fish22, ncol=4)
ggsave("C:Plots//Transect//Resources_terr_var//fish_richness_terrestrial.png")

#fish biomass
fish9<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish10<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish11<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish12<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=d15n))+geom_point()+geom_smooth(method="lm")
fish15<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish23<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish24<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish12, fish9,fish10,fish11,fish15,fish23,fish24, ncol=4)
ggsave("C:Plots//Transect//Resources_terr_var//fish_biomass_bym3_terrestrial.png")

###fish richness
fish1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
fish2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish10<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish12<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
fish13<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish14<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
fish19<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish20<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(fish1,fish2,fish3,fish4,fish10,fish12, fish13,fish14,fish19,fish20, ncol=5)
ggsave("C:Plots//Transect//Resources_terr_var//fish_richness_terrestrial.png", width=40, height=20, unit="cm")


###fish abundance
fish1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=d15n))+geom_point()+geom_smooth(method="lm")
fish2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish10<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish12<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=mammal_richness))+geom_point()+geom_smooth(method="lm")
fish13<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish14<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=tree_abundance))+geom_point()+geom_smooth(method="lm")
fish19<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish20<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(fish1,fish2,fish3,fish4,fish10,fish12, fish13,fish14,fish19,fish20, ncol=5)
ggsave("C:Plots//Transect//Resources_terr_var//fish_abundance_terrestrial.png", width=40, height=20, unit="cm")




###bycatch richness and abundance
bycatch1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
bycatch2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
bycatch10<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
bycatch12<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
bycatch13<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch14<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
bycatch19<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch20<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_richness_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(bycatch1,bycatch2,bycatch3,bycatch4,bycatch10,bycatch12, bycatch13,bycatch14,bycatch19,bycatch20, ncol=5)
ggsave("C:Plots//Transect//Resources_terr_var//bycatch_richness_terrestrial.png", width=40, height=20, unit="cm")




bycatch5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_abundance_bym3, y=d15n))+geom_point()+geom_smooth(method="lm")
bycatch6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_abundance_bym3, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_abundance_bym3, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch8<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_abundance_bym3, y=habitat_het))+geom_point()+geom_smooth(method="lm")
bycatch9<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_abundance_bym3, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
bycatch11<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_abundance_bym3, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
bycatch15<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_abundance_bym3, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch16<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_abundance_bym3, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
bycatch17<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_abundance_bym3, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch18<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_abundance_bym3, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(bycatch5,bycatch6,bycatch7,bycatch8,bycatch9,bycatch11,bycatch15,bycatch16,bycatch17,bycatch18,ncol=5)
ggsave("C:Plots//Transect//Resources_terr_var//bycatch_abundance_terrestrial.png", width=40, height=20, unit="cm")



# Plotting Marine resources vs. biogeography ----------------------------

#histogram of marine variab
hist_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_abundance_bym3))+geom_density()
hist_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=bycatch_richness_corrected))+geom_density()
hist_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3)))+geom_density()
hist_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected))+geom_density()
hist_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3)))+geom_density()
hist_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_length))+geom_density()
hist_marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected))+geom_density()
hist_marine8<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_abundance_bym3))+geom_density()

plot_grid(hist_marine4, hist_marine5, hist_marine6, hist_marine3, hist_marine2, hist_marine1, hist_marine7, ncol=4)
ggsave("C:Plots//Transect//Resources_biogeog//histogram_marine.png")



### Marine resources vs. Island area
lat_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
lat_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
lat_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
lat_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
lat_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
lat_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=fish_length))+geom_point()+geom_smooth(method="lm")
lat_marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

plot_grid(lat_marine4, lat_marine5, lat_marine6, lat_marine3, lat_marine2, lat_marine1, lat_marine7, ncol=3)
ggsave("C:Plots//Transect//Resources_biogeog//Latitude_marine.png")


### Marine resources vs. Island area
A_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
A_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
A_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
A_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
A_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
A_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=fish_length))+geom_point()+geom_smooth(method="lm")
A_marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

plot_grid(A_marine4, A_marine5, A_marine6, A_marine3, A_marine2, A_marine1, A_marine7, ncol=3)
ggsave("C:Plots//Transect//Resources_biogeog//Area_marine.png", width=40, height=40, unit="cm")



###Coverage of neighbouring land mass, LOW NEighb_250 = high exposure
neib1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
neib2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
neib4<- ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
neib5<- ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
neib6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
neib7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=fish_length))+geom_point()+geom_smooth(method="lm")
neib8<-ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

plot_grid(neib1,neib2,neib4,neib5,neib6,neib7,neib8, ncol=3)
ggsave("C:Plots//Transect//Resources_biogeog//Neighb250_marine.png")

#head(fish_richness_merged_tran_isl_300)
### Marine resources vs. Island area
DN_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
DN_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
DN_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
DN_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
DN_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
DN_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=fish_length))+geom_point()+geom_smooth(method="lm")
DN_marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

plot_grid(DN_marine4, DN_marine5, DN_marine6, DN_marine3, DN_marine2, DN_marine1, DN_marine7, ncol=3)
ggsave("C:Plots//Transect//Resources_biogeog//Dist_Near_marine.png", width=40, height=40, unit="cm")





###########3
# d15n bycatch ---------------------------------------------------------------
fish_richness_merged_tran_isl_300_zscores<-fish_richness_merged_tran_isl_300
fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected<-scale(fish_richness_merged_tran_isl_300$bycatch_richness_corrected, center=TRUE, scale=TRUE)
fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected.unscaled <-fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected * attr(fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected, 'scaled:center')
fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected<-as.numeric(fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected)
ggplot(fish_richness_merged_tran_isl_300_zscores, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")

str(fish_richness_merged_tran_isl_300_zscores)

qqp(fish_richness_merged_tran_isl_300_zscores$d15n)
qqp(fish_richness_merged_tran_isl_300_zscores$d15n, "lnorm")

lm.d15n.bycatch<-lm(d15n ~ bycatch_richness_corrected, data=fish_richness_merged_tran_isl_300_zscores)
gam.lm.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = fish_richness_merged_tran_isl_300_zscores, select=TRUE, method="REML")
gam.loglink.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = fish_richness_merged_tran_isl_300_zscores, family = gaussian(link="log"), select=TRUE, method="REML")

gam.gamma.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = fish_richness_merged_tran_isl_300_zscores, family = Gamma, select=TRUE, method="REML")
gam.tweedie.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = fish_richness_merged_tran_isl_300_zscores, family = tw, select=TRUE, method="REML")


AICtab( lm.d15n.bycatch,  gam.lm.d15n.bycatch, gam.loglink.d15n.bycatch, gam.gamma.d15n.bycatch, gam.tweedie.d15n.bycatch)

#gam.lm.d15n.bycatch


plot(gam.gamma.d15n.bycatch, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
appraise(gam.gamma.d15n.bycatch)
qq_plot(gam.gamma.d15n.bycatch, method = 'simulate')
k.check(gam.gamma.d15n.bycatch)
summary(gam.gamma.d15n.bycatch)



fam.gam.d15n.bycatch <- family(gam.gamma.d15n.bycatch)
fam.gam.d15n.bycatch
str(fam.gam.d15n.bycatch)
ilink.gam.d15n.bycatch<- fam.gam.d15n.bycatch$linkinv
ilink.gam.d15n.bycatch


mod.d15n.bycatch<-gam.gamma.d15n.bycatch
ndata.d15n.bycatch <- with(fish_richness_merged_tran_isl_300_zscores, data_frame(bycatch_richness_corrected = seq(min(bycatch_richness_corrected), max(bycatch_richness_corrected),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.d15n.bycatch <- add_column(ndata.d15n.bycatch, fit = predict(mod.d15n.bycatch, newdata = ndata.d15n.bycatch, type = 'response'))

predict(mod.d15n.bycatch, newdata = ndata.d15n.bycatch, type = 'response')
ndata.d15n.bycatch <- bind_cols(ndata.d15n.bycatch, setNames(as_tibble(predict(mod.d15n.bycatch, ndata.d15n.bycatch, se.fit = TRUE)[1:2]),
                                                             c('fit_link','se_link')))

## create the interval and backtransform

ndata.d15n.bycatch <- mutate(ndata.d15n.bycatch,
                             fit_resp  = ilink.gam.d15n.bycatch(fit_link),
                             right_upr = ilink.gam.d15n.bycatch(fit_link + (2 * se_link)),
                             right_lwr = ilink.gam.d15n.bycatch(fit_link - (2 * se_link)))

fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected<-scale(fish_richness_merged_tran_isl_300$bycatch_richness_corrected, center=TRUE, scale=TRUE)

ndata.d15n.bycatch$bycatch_richness_corrected.unscaled<-ndata.d15n.bycatch$bycatch_richness_corrected * attr(fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected, 'scaled:center')


# plot 
plt.d15n.bycatch <- ggplot(ndata.d15n.bycatch, aes(x = bycatch_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = fish_richness_merged_tran_isl_300_zscores)+
  xlab(expression("Bycatch richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.bycatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.bycatch
ggsave("C:Plots//Transect//d15n_bycatch_400m.png")



# d15n GAM marine richness ------------------------------------------------

library(tidyr)
library(bbmle) 
library(glmmTMB)
library(doBy)
library(plyr)
require(dplyr)
library(ggplot2) 
library(doBy)
library(grid)
library(glmmADMB)
library(betareg)
library(lmtest)
library(fitdistrplus)
library(visreg)
library(lme4)
library(coefplot)
library(arm)
library(lmerTest)
library(boot)
library(MASS)
require(scales)
library(car)
library(knitr)
library(tidyverse)
library(kableExtra)
library(multcomp)
library(arm) ## for sim()
library(descr)  ## for LogRegR2
require(reshape2)
library(ggplot2)
library(grid)
library(DHARMa)
library(gap)
library(qrnn)
library(mgcv)
library(colorspace)
library(gratia)
library(cowplot)




#### marine total catch

fish_richness_merged_tran_isl_300_zscores<-fish_richness_merged_tran_isl_300
fish_richness_merged_tran_isl_300_zscores$marine_richness_corrected<-scale(fish_richness_merged_tran_isl_300$marine_richness_corrected, center=TRUE, scale=TRUE)
fish_richness_merged_tran_isl_300_zscores$marine_richness_corrected.unscaled <-fish_richness_merged_tran_isl_300_zscores$marine_richness_corrected * attr(fish_richness_merged_tran_isl_300_zscores$marine_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_tran_isl_300_zscores$marine_richness_corrected, 'scaled:center')
fish_richness_merged_tran_isl_300_zscores$marine_richness_corrected<-as.numeric(fish_richness_merged_tran_isl_300_zscores$marine_richness_corrected)
ggplot(fish_richness_merged_tran_isl_300_zscores, aes(y=d15n, x=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

qqp(fish_richness_merged_tran_isl_300_zscores$d15n)
qqp(fish_richness_merged_tran_isl_300_zscores$d15n, "lnorm")

lm.d15n.marinecatch<-lm(d15n ~ marine_richness_corrected, data=fish_richness_merged_tran_isl_300_zscores)
gam.lm.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = fish_richness_merged_tran_isl_300_zscores, select=TRUE, method="REML")
gam.loglink.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = fish_richness_merged_tran_isl_300_zscores, family = gaussian(link="log"), select=TRUE, method="REML")

gam.gamma.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = fish_richness_merged_tran_isl_300_zscores, family = Gamma, select=TRUE, method="REML")
gam.tweedie.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = fish_richness_merged_tran_isl_300_zscores, family = tw, select=TRUE, method="REML")


AICtab( lm.d15n.marinecatch,  gam.lm.d15n.marinecatch, gam.loglink.d15n.marinecatch, gam.gamma.d15n.marinecatch, gam.tweedie.d15n.marinecatch)

#gam.lm.d15n.marinecatch


plot(gam.lm.d15n.marinecatch , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
appraise(gam.lm.d15n.marinecatch )
qq_plot(gam.lm.d15n.marinecatch , method = 'simulate')
k.check(gam.lm.d15n.marinecatch )
summary(gam.lm.d15n.marinecatch )



fam.gam.d15n.marinecatch <- family(gam.lm.d15n.marinecatch )
fam.gam.d15n.marinecatch
str(fam.gam.d15n.marinecatch)
ilink.gam.d15n.marinecatch<- fam.gam.d15n.marinecatch$linkinv
ilink.gam.d15n.marinecatch


mod.d15n.marinecatch<-gam.lm.d15n.marinecatch 
ndata.d15n.marinecatch <- with(fish_richness_merged_tran_isl_300_zscores, data_frame(marine_richness_corrected = seq(min(marine_richness_corrected), max(marine_richness_corrected),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.d15n.marinecatch <- add_column(ndata.d15n.marinecatch, fit = predict(mod.d15n.marinecatch, newdata = ndata.d15n.marinecatch, type = 'response'))

predict(mod.d15n.marinecatch, newdata = ndata.d15n.marinecatch, type = 'response')
ndata.d15n.marinecatch <- bind_cols(ndata.d15n.marinecatch, setNames(as_tibble(predict(mod.d15n.marinecatch, ndata.d15n.marinecatch, se.fit = TRUE)[1:2]),
                                                                     c('fit_link','se_link')))

## create the interval and backtransform

ndata.d15n.marinecatch <- mutate(ndata.d15n.marinecatch,
                                 fit_resp  = ilink.gam.d15n.marinecatch(fit_link),
                                 right_upr = ilink.gam.d15n.marinecatch(fit_link + (2 * se_link)),
                                 right_lwr = ilink.gam.d15n.marinecatch(fit_link - (2 * se_link)))

fish_richness_merged_tran_isl_300_zscores$marine_richness_corrected<-scale(fish_richness_merged_tran_isl_300$marine_richness_corrected, center=TRUE, scale=TRUE)

ndata.d15n.marinecatch$marine_richness_corrected.unscaled<-ndata.d15n.marinecatch$marine_richness_corrected * attr(fish_richness_merged_tran_isl_300_zscores$marine_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_tran_isl_300_zscores$marine_richness_corrected, 'scaled:center')


# plot 
plt.d15n.marinecatch <- ggplot(ndata.d15n.marinecatch, aes(x = marine_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = fish_richness_merged_tran_isl_300_zscores)+
  xlab(expression("Marine catch richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.marinecatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.marinecatch
ggsave("C:Plots//Transect//d15n_bycatch.png")


# d15n fish richness ------------------------------------------------------


fish_richness_merged_tran_isl_300_zscores<-fish_richness_merged_tran_isl_300
fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected<-scale(fish_richness_merged_tran_isl_300$fish_richness_corrected, center=TRUE, scale=TRUE)
fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected.unscaled <-fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected * attr(fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected, 'scaled:center')
fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected<-as.numeric(fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected)
ggplot(fish_richness_merged_tran_isl_300_zscores, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")

str(fish_richness_merged_tran_isl_300_zscores)

qqp(fish_richness_merged_tran_isl_300_zscores$d15n)
qqp(fish_richness_merged_tran_isl_300_zscores$d15n, "lnorm")

gamma.12.fish_richness_corrected<-fitdistr(fish_richness_merged_tran_isl_300$fish_richness_corrected+0.01, "gamma")
qqp(fish_richness_merged_tran_isl_300$fish_richness_corrected, "gamma", shape = gamma.12.fish_richness_corrected$estimate[[1]], rate = gamma.12.fish_richness_corrected$estimate[[2]])

lm.d15n.fish<-lm(d15n ~ fish_richness_corrected, data=fish_richness_merged_tran_isl_300_zscores)
gam.lm.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = fish_richness_merged_tran_isl_300_zscores, select=TRUE, method="REML")
gam.loglink.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = fish_richness_merged_tran_isl_300_zscores, family = gaussian(link="log"), select=TRUE, method="REML")
gam.gamma.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = fish_richness_merged_tran_isl_300_zscores, family = Gamma, select=TRUE, method="REML")
gam.tweedie.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = fish_richness_merged_tran_isl_300_zscores, family = tw, select=TRUE, method="REML")


AICtab( gam.gamma.d15n.fish,  lm.d15n.fish, gam.loglink.d15n.fish, gam.lm.d15n.fish, gam.tweedie.d15n.fish)

#gam.gam.gamma.d15n.fish


plot(gam.gamma.d15n.fish, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
appraise(gam.gamma.d15n.fish)
qq_plot(gam.gamma.d15n.fish, method = 'simulate')
k.check(gam.gamma.d15n.fish)
summary(gam.gamma.d15n.fish)



fam.gam.d15n.fish <- family(gam.gamma.d15n.fish)
fam.gam.d15n.fish
str(fam.gam.d15n.fish)
ilink.gam.d15n.fish<- fam.gam.d15n.fish$linkinv
ilink.gam.d15n.fish


mod.d15n.fish<-gam.gamma.d15n.fish
ndata.d15n.fish <- with(fish_richness_merged_tran_isl_300_zscores, data_frame(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.d15n.fish <- add_column(ndata.d15n.fish, fit = predict(mod.d15n.fish, newdata = ndata.d15n.fish, type = 'response'))

predict(mod.d15n.fish, newdata = ndata.d15n.fish, type = 'response')
ndata.d15n.fish <- bind_cols(ndata.d15n.fish, setNames(as_tibble(predict(mod.d15n.fish, ndata.d15n.fish, se.fit = TRUE)[1:2]),
                                                       c('fit_link','se_link')))

## create the interval and backtransform

ndata.d15n.fish <- mutate(ndata.d15n.fish,
                          fit_resp  = ilink.gam.d15n.fish(fit_link),
                          right_upr = ilink.gam.d15n.fish(fit_link + (2 * se_link)),
                          right_lwr = ilink.gam.d15n.fish(fit_link - (2 * se_link)))

fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected<-scale(fish_richness_merged_tran_isl_300$fish_richness_corrected, center=TRUE, scale=TRUE)
ndata.d15n.fish$fish_richness_corrected.unscaled<-ndata.d15n.fish$fish_richness_corrected * attr(fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.d15n.fish <- ggplot(ndata.d15n.fish, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = fish_richness_merged_tran_isl_300_zscores)+
  xlab(expression("fish richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.fish,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.fish
ggsave("C:Plots//Transect//d15n_fish_400.png")





# Correlation -------------------------------------------------------------

which( colnames(fish_richness_merged_tran_isl_300)=="Exp_SHZN" )
which( colnames(fish_richness_merged_tran_isl_300)=="lat" )
which( colnames(fish_richness_merged_tran_isl_300)=="size.cat2" )
which( colnames(fish_richness_merged_tran_isl_300)=="notes" )

str(fish_richness_merged_tran_isl_300)
corr_by_isl_selected_fish<-fish_richness_merged_tran_isl_300 %>% select_if(is.numeric)
str(corr_by_isl_selected_fish)


corr_by_isl_selected_fish_2 <- round(cor(corr_by_isl_selected_fish, use="pairwise.complete.obs"), 1)
#head(corr_by_isl_selected_fish_2[, 1:6])
p.mat_by_isl_selected_fish_2 <- cor_pmat(corr_by_isl_selected_fish)
#head(p.mat_by_isl_selected_fish_2[, 1:4])

ggcorrplot(corr_by_isl_selected_fish_2)
ggcorrplot(corr_by_isl_selected_fish_2, hc.order = TRUE, type = "lower",
           outline.col = "white")
ggcorrplot(corr_by_isl_selected_fish_2, hc.order = TRUE,
           type = "lower", p.mat = p.mat_by_isl_selected_fish_2)

ggcorrplot(corr_by_isl_selected_fish_2, hc.order = TRUE, type = "lower",
           lab = TRUE)
ggsave("C:Plots//Transect//Resources_terr_var//ggcorrplot_marine_terrestrial.png",  width=40, height=40, unit="cm")

library(purrr)
library(ggcorrplot)
library(corrr)
library(PerformanceAnalytics)


corr_by_isl_selected_fish %>% correlate() %>%  network_plot(min_cor = 0.7)
ggsave("C:Plots//Transect//Resources_terr_var//networkplot_marine_terrestrial_0.7.png")
