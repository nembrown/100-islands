library(here)

# Load data  ---------------------------------------------------------------


ben_fish_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_speciesabundance_20142018.csv")

ben_pelagic_demersal_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_fishcodes_demersal_pelagic.csv")

ben_bycatch_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_bycatch_20142018.csv")

ben_size_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_specieslength_20142018.csv")

ben_habitat_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_habitat_20142018.csv")

ben_weights<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_specieslength_20142018_biomass_complete.csv")

ben_environment_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_environmental_20142018.csv")

ben_netdimensions<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//netdimensions_nb.csv")

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


# Net dimensions ----------------------------------------------------------
# take net dimnesion data, make it long for each set to match the other files

ben_netdimensions<-ben_netdimensions %>% gather(replicate, volume, vol_set1,vol_set2)
ben_netdimensions$replicate[ben_netdimensions$replicate=="vol_set1"]<-"1"
ben_netdimensions$replicate[ben_netdimensions$replicate=="vol_set2"]<-"2"
head(ben_netdimensions)

##adding area in here for the percent cover of eelgrass calculations later
ben_netdimensions<-ben_netdimensions %>% gather(area_replicate, area, area_set1,area_set2)
ben_netdimensions$area_replicate[ben_netdimensions$area_replicate=="area_set1"]<-"1"
ben_netdimensions$area_replicate[ben_netdimensions$area_replicate=="area_set2"]<-"2"
head(ben_netdimensions)


# use only summer months data (July and August 7&8)
#use the netdimensions where we don't estimate missing data
# mean net volume used at that site in the summer
ben_netdimensions$volume<-as.numeric(ben_netdimensions$volume)
ben_netdimensions$area<-as.numeric(ben_netdimensions$area)
ben_netdimensions_year <-ben_netdimensions %>% filter(between(month, 5,8)) %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
ben_netdimensions_year <- ben_netdimensions_year[,c(1,11,12)]
head(ben_netdimensions_year)



#sum of total net volume in given summer
ben_netdimensions_summer <-ben_netdimensions%>% filter(between(month, 5,8)) %>% group_by(site, year) %>% 
  summarise(sum_volume = sum(volume, na.rm=TRUE)) 
ben_netdimensions_summer$sum_volume[ben_netdimensions_summer$sum_volume==0]<-"NA"
head(ben_netdimensions_summer)
ben_netdimensions_summer$sum_volume<-as.numeric(ben_netdimensions_summer$sum_volume)

ben_netdimensions_day <-ben_netdimensions%>% filter(between(month, 5,8)) %>% group_by(site, year, month, day) %>% 
  summarise(sum_volume = sum(volume, na.rm=TRUE)) 
ben_netdimensions_day$sum_volume[ben_netdimensions_day$sum_volume==0]<-"NA"
head(ben_netdimensions_day)
ben_netdimensions_day$sum_volume<-as.numeric(ben_netdimensions_day$sum_volume)


# Fish abundance data cleaning ------------------------------------------------------
#only summer months 


ben_fish_data <- ben_fish_data %>% filter(between(month, 5,8))
#make wide format to calculate number of species
#use sum of abundance to reflect total of all fish caught in a given summer

ben_fish_data_wide_year_2 <-ben_fish_data %>% group_by(site, year, month, day, species) %>% 
  summarise(sum_abundance = sum(abundance, na.rm=TRUE)) %>% 
  spread(species, sum_abundance) %>% 
  replace(is.na(.), 0) 

head(ben_fish_data_wide_year_2)

#calculate richness & abundance  from the wide dataframe
ben_fish_data_wide_year_richness_2<-ben_fish_data_wide_year_2[,c(1,2,3,4)]
ben_fish_data_wide_year_richness_2$fish_richness<-specnumber(ben_fish_data_wide_year_2[,-c(1,2,3,4)])
ben_fish_data_wide_year_richness_2$fish_abundance<-rowSums(ben_fish_data_wide_year_2[,-c(1,2,3,4)],na.rm = TRUE)
head(ben_fish_data_wide_year_richness_2)

#adjust richness and abundance by total volume seined at that site
#bym3 is correcting problem that some sites were sampled more than others (aka species/Area curve)
ben_fish_data_wide_year_richness_2<-merge(ben_fish_data_wide_year_richness_2, ben_netdimensions_day)
ben_fish_data_wide_year_richness_2$fish_richness_bym3<-ben_fish_data_wide_year_richness_2$fish_richness/(ben_fish_data_wide_year_richness_2$sum_volume)
ben_fish_data_wide_year_richness_2$fish_abundance_bym3<-ben_fish_data_wide_year_richness_2$fish_abundance/(ben_fish_data_wide_year_richness_2$sum_volume)

#this file we will skip the averaging step!
head(ben_netdimensions_day)

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
ben_size_data_wide_year_richness_2<-ben_size_data_wide_year[,c(1,2, 3, 4, 5)]
ben_size_data_wide_year_richness_2$fish_length<-rowMeans(ben_size_data_wide_year[,-c(1,2,3,4,5)],na.rm = TRUE)
ben_size_data_wide_year_richness_2$fish_sd<-rowSds(as.matrix(ben_size_data_wide_year[,-c(1,2,3,4,5)]),na.rm = TRUE)
head(ben_size_data_wide_year_richness_2)

ben_size_data_wide_year_richness_2 <- ben_size_data_wide_year_richness_2[,-c(5)] %>% group_by(site, year, month, day) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(ben_size_data_wide_year_richness_2)

#getting rid of by year summing

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

ben_weights_wide_year_richness_2<-ben_weights_wide_year[,c(1,2,3,4,5)]
ben_weights_wide_year_richness_2$fish_av_weight<-rowMeans(ben_weights_wide_year[,-c(1,2,3,4,5)],na.rm = TRUE)
ben_weights_wide_year_richness_2$fish_weight_sd<-rowSds(as.matrix(ben_weights_wide_year[,-c(1,2,3,4,5)]),na.rm = TRUE)
head(ben_weights_wide_year_richness_2)

#again, replaced line by year with one for day 
ben_weights_wide_year_richness_2 <- ben_weights_wide_year_richness_2[,-c(5)] %>% group_by(site, year, month, day) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(ben_weights_wide_year_richness_2)


#here combining in a different way, not for counting # sp. but for weighing each individual fish
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

head(ben_weights_wide_year_4)

ben_fish_data_wide_year_4 <- ben_fish_data_wide_year_3 %>% filter(! code %in% c("fan1 2018 7 15 2", "hdo 2015 5 3 2", 
                                                                                "ppo 2018 5 29 2", "ris1 2018 7 19 2", 
                                                                                "ssp 2014 7 2 2", "ssp 2016 5 8 2", 
                                                                                "wfb 2014 6 11 1", "wfb 2014 6 11 2"))
ben_fish_data_wide_year_4<- ben_fish_data_wide_year_4[,-98]

head(ben_fish_data_wide_year_4)

#combining size and abundance to get "biomass" estimate
ben_biomass_data_wide_year<-(ben_weights_wide_year_4[,-c(1,2,3,4,5)])*(ben_fish_data_wide_year_4[,-c(1,2,3,4,5)])
ben_biomass_data_wide_year_nf<-ben_weights_wide_year_4[,c(1,2,3,4,5)]
ben_biomass_data_wide_year_nf$fish_biomass<-rowSums(ben_biomass_data_wide_year)
head(ben_biomass_data_wide_year_nf)

#correcting for total net volume
ben_biomass_data_wide_year_nf<-merge(ben_biomass_data_wide_year_nf, ben_netdimensions_day)
ben_biomass_data_wide_year_nf$fish_biomass_bym3<-ben_biomass_data_wide_year_nf$fish_biomass/(ben_biomass_data_wide_year_nf$sum_volume)

head(ben_biomass_data_wide_year_nf)

#changed this line to just one value per site per day, but average the replicates (to match the environmental data)
ben_biomass_data_wide_year_nf1 <-ben_biomass_data_wide_year_nf[,-c(5)]%>% group_by(site, year, month, day) %>% summarise_if(is.numeric, list(~mean(., na.rm=TRUE), ~sd(., na.rm=TRUE)))
head(ben_biomass_data_wide_year_nf1)


# Merging fish, bycatch, biomass, net dimensions ------------------------------------------
#merging files together into one data frame
head(ben_netdimensions_year)
head(ben_fish_data_wide_year_richness_2)
head(ben_size_data_wide_year_richness_2)
head(ben_weights_wide_year_richness_2)

fish_bycatch_richness_merged_tran_year_2<-merge(ben_fish_data_wide_year_richness_2, ben_size_data_wide_year_richness_2, by=c("site", "year", "month", "day"), all=TRUE)
fish_bycatch_richness_merged_tran_year_2<-merge(fish_bycatch_richness_merged_tran_year_2, ben_weights_wide_year_richness_2, by=c("site", "year", "month", "day"), all=TRUE)
fish_bycatch_richness_merged_tran_year_2<-merge(fish_bycatch_richness_merged_tran_year_2, ben_biomass_data_wide_year_nf1[,-c(6,9)], by=c("site", "year", "month", "day"), all=TRUE)


#correcting richness and abundance for average net dimensions at that site 
fish_bycatch_richness_merged_tran_year_2$fish_richness_corrected<-((fish_bycatch_richness_merged_tran_year_2$fish_richness)/fish_bycatch_richness_merged_tran_year_2$sum_volume)

#### Adding environmental data
head(ben_environment_data)
ben_environment_data_d1<-ben_environment_data %>% filter(location=="d1")
ben_environment_data_d10<-ben_environment_data %>% filter(location=="d10")

fish_bycatch_richness_merged_tran_year_2<-merge(fish_bycatch_richness_merged_tran_year_2, ben_environment_data_d10, by=c("site", "year", "month", "day"))


#### Adding habitat data
head(ben_habitat_data)
fish_bycatch_richness_merged_tran_year_2<-merge(fish_bycatch_richness_merged_tran_year_2, ben_habitat_data, by="site")
head(fish_bycatch_richness_merged_tran_year_2)


fish_bycatch_richness_merged_tran_year_2$subtidal_total_cover<- fish_bycatch_richness_merged_tran_year_2$subtidal_primary_cover+fish_bycatch_richness_merged_tran_year_2$subtidal_secondary_cover
fish_bycatch_richness_merged_tran_year_2$intertidal_total_cover<- fish_bycatch_richness_merged_tran_year_2$intertidal_primary_cover+fish_bycatch_richness_merged_tran_year_2$intertidal_secondary_cover
fish_bycatch_richness_merged_tran_year_2$total_habitat_cover<- (fish_bycatch_richness_merged_tran_year_2$intertidal_total_cover+fish_bycatch_richness_merged_tran_year_2$subtidal_total_cover)/2

#fish_bycatch_richness_merged_tran_year$habitat_area<- (fish_bycatch_richness_merged_tran_year$area)*((fish_bycatch_richness_merged_tran_year$subtidal_total_cover)/100)


head(fish_bycatch_richness_merged_tran_year_2)
write.csv(fish_bycatch_richness_merged_tran_year_2, "C:Biodiversity idea//Output files//fish_biomass_day.csv")

fish_biomass_day<-fish_bycatch_richness_merged_tran_year_2


ggplot(fish_biomass_day, aes(x=total_habitat_cover, y=log(fish_biomass_bym3_mean+1)))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=temp, y=log(fish_biomass_bym3_mean+1)))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=as.numeric(salinity), y=log(fish_biomass_bym3_mean+1), col=as.factor(month)))+
  geom_point()+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=as.numeric(secchi), y=log(fish_biomass_bym3_mean+1), col=as.factor(month)))+
  geom_point()+scale_colour_viridis_d()+theme_bw()



ggplot(fish_biomass_day, aes(x=as.numeric(temp), y=log(fish_biomass_bym3_mean+1), col=as.factor(month)))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=as.numeric(aspect), y=log(fish_biomass_bym3_mean+1), col=as.factor(month)))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()


install.packages("circular")
library(circular)


fish_biomass_day_circular<-fish_biomass_day

fish_biomass_day_circular<- circular(fish_biomass_day_circular, units = "degrees", template = "geographics") 

plot.circular(fish_biomass_day$aspect, col=factor(fish_biomass_day$fish_richness_corrected))


ggplot(fish_biomass_day, aes(x=aspect, y=fish_richness_corrected))+plot.circular(fish_biomass_day$aspect)

install.packages("circular")
library(circular)

fish_biomass_day$aspect<- circular(fish_biomass_day$aspect, units = "degrees", template = "geographics") 

plot.circular(fish_biomass_day$aspect, col=as.factor(fish_biomass_day$month))


ggplot(fish_biomass_day, aes(x=aspect, y=fish_richness_corrected))+plot.circular(fish_biomass_day$aspect)

ggplot(fish_biomass_day, aes(x=ph, y=log(fish_biomass_bym3_mean+1)))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=temp, y=fish_richness_corrected))+
  geom_point()+geom_smooth(aes(), method="gam", alpha=0.10)+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=ph, y=fish_richness_corrected))+
  geom_point()+geom_smooth(aes(), method="gam", alpha=0.10)+scale_colour_viridis_d()+theme_bw()


ggplot(fish_biomass_day, aes(x=as.numeric(salinity), y=log(fish_biomass_bym3_mean+1), col=as.factor(month)))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=as.numeric(secchi), y=fish_richness_corrected))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()



ggplot(fish_biomass_day, aes(x=lat, y=fish_richness_corrected))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=as.factor(month), y=fish_biomass_bym3_mean))+
  geom_boxplot()+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=exposure, y=log(fish_biomass_bym3_mean)))+
  geom_boxplot()+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=aspect, y=fish_biomass_bym3_mean))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()


ggplot(fish_biomass_day, aes(x=as.numeric(slope_intertidal), y=fish_biomass_bym3_mean))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=as.numeric(slope_subtidal), y=fish_biomass_bym3_mean))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()


ggplot(fish_biomass_day, aes(x=freshwater_input, y=fish_biomass_bym3_mean))+
  geom_boxplot()+scale_colour_viridis_d()+theme_bw()+ylim(0,20)
#maybe want to only have no freshwater input sites? 


ggplot(fish_biomass_day, aes(x=total_habitat_cover, y=fish_richness_corrected, col=super_node))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=subtidal_total_cover, y=bycatch_richness_corrected, col=super_node))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()

ggplot(fish_biomass_day, aes(x=subtidal_total_cover, y=marine_richness_corrected, col=super_node))+
  geom_point()+geom_smooth(aes(), method="lm", alpha=0.10)+scale_colour_viridis_d()+theme_bw()

