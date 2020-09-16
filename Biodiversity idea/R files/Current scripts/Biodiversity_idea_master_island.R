library(here)

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


#load fish data - an output of from ben_fish_cleaning.R
fish_bycatch_richness_merged_isl_year<- read.csv("C:Biodiversity idea//Output files//fish_bycatch_richness_merged_tran_year.csv")
head(fish_bycatch_richness_merged_isl_year)

# Matching terrestrial transects to beachseine sites ----------------------

##pulls from output files of R script - "Assign closest points to radius"
#This is working with a 2km radius around the transects - pairing beachseine sites to transects within 2km
hakai_sites_distance_tran<-read.csv("Biodiversity idea//Output files//paired_sites_by_radius.csv")
length(unique(hakai_sites_distance_tran$unq_tran))
#171 transects match within 2km! (updated after splitting transects properly)
head(hakai_sites_distance_tran)

fish_bycatch_richness_merged_isl<-merge(fish_bycatch_richness_merged_isl_year, hakai_sites_distance_tran, by="site")
head(fish_bycatch_richness_merged_isl)
fish_bycatch_richness_merged_isl$unq_isl<-strtrim(fish_bycatch_richness_merged_isl$unq_tran, 4)

### averaging across islands
fish_bycatch_richness_merged_isl <- fish_bycatch_richness_merged_isl %>% group_by(unq_isl) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
length(fish_bycatch_richness_merged_isl$unq_isl)
#50 islands match with fish data

###FISH with unq_tran added but no transect data NO UNQ_ISL
write.csv(fish_bycatch_richness_merged_isl, "C:Biodiversity idea//Output files//fish_bycatch_richness_merged_isl.csv", row.names=FALSE)
head(fish_bycatch_richness_merged_isl)
fish_bycatch_richness_merged_isl[duplicated(fish_bycatch_richness_merged_isl),]


# Loading terrestrial data by island------------------------------------

fish_richness_merged_isl<-fish_bycatch_richness_merged_isl
# 
# fish_richness_merged_isl$combined_richness_corrected<-fish_richness_merged_isl$wrack_richness+fish_richness_merged_isl$marine_richness_corrected
# fish_richness_merged_isl$eelgrass_cover_2km<-(fish_richness_merged_isl$MEAN_egarea2k)/(fish_richness_merged_isl$Radius_m_2000)
# fish_richness_merged_isl$habitat_cover_2km<-(fish_richness_merged_isl$sum_2km)/(fish_richness_merged_isl$Radius_m_2000)


length(unique(fish_richness_merged_isl$unq_isl))
#50 islands that match with fish biomass data

# adding in arch sites ----------------------------------------------------
#### Adding in arch site by distance

distance_btwn_points_midden_transects<- read.csv("C:Biodiversity idea//Output files//Distance_btwn_points_midden_transects.csv")
head(distance_btwn_points_midden_transects)
names(distance_btwn_points_midden_transects)[3]<-"unq_tran"
names(distance_btwn_points_midden_transects)[5]<-"distance_to_midden"
distance_btwn_points_midden_transects$unq_isl<-strtrim(distance_btwn_points_midden_transects$unq_tran, 4)
distance_btwn_points_midden_isl <- distance_btwn_points_midden_transects %>% group_by(unq_isl) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(distance_btwn_points_midden_isl)


fish_richness_merged_isl_arch<-merge(fish_richness_merged_isl, distance_btwn_points_midden_isl[,c(1,5)], by="unq_isl")
head(fish_richness_merged_isl_arch)

by_isl_master<-read.csv("C:Food web idea//Data by person//Norah.data//by_isl_master.csv")
head(by_isl_master)

by_isl_master$log_Area<-log(by_isl_master$Area)

#head(fish_richness_merged_isl_arch)
fish_richness_merged_isl_arch_2 <-merge(fish_richness_merged_isl_arch, by_isl_master, by="unq_isl", all=TRUE)
fish_richness_merged_isl_arch_2$log_site_mean_by_isl <- log(fish_richness_merged_isl_arch_2$site_mean_by_isl+1)
fish_richness_merged_isl_arch_2$log_MEAN_kparea2k <- log(fish_richness_merged_isl_arch_2$MEAN_kparea2k+1)
fish_richness_merged_isl_arch_2$log_MEAN_egarea2k <- log(fish_richness_merged_isl_arch_2$MEAN_egarea2k+1)
fish_richness_merged_isl_arch_2$log_MEAN_rockarea2000 <- log(fish_richness_merged_isl_arch_2$MEAN_rockarea2000+1)
fish_richness_merged_isl_arch_2$log_Rock<- log(fish_richness_merged_isl_arch_2$Rock)
fish_richness_merged_isl_arch_2$log_DistW_ML<- log(fish_richness_merged_isl_arch_2$DistW_ML)
fish_richness_merged_isl_arch_2$log_Dist_Near<- log(fish_richness_merged_isl_arch_2$Dist_Near)
fish_richness_merged_isl_arch_2$log_distance_to_midden<- log(fish_richness_merged_isl_arch_2$distance_to_midden)
# fish_richness_merged_isl_arch_2$log_distance_to_fish<- log(fish_richness_merged_isl_arch_2$distance_to_fish)
fish_richness_merged_isl_arch_2$Bog_area<- fish_richness_merged_isl_arch_2$Bog_Vegetation + fish_richness_merged_isl_arch_2$Bog_Water 
fish_richness_merged_isl_arch_2$log_Bog_area<- log(fish_richness_merged_isl_arch_2$Bog_area+1)
fish_richness_merged_isl_arch_2$log_fish_biomass_bym3_mean <- log(fish_richness_merged_isl_arch_2$fish_biomass_bym3_mean+1)
fish_richness_merged_isl_arch_2$log_bycatch_biomass_bym3_mean <- log(fish_richness_merged_isl_arch_2$bycatch_biomass_bym3_mean+1)




#master_transect[,c("seaweed_all", "fish_all", "marine_invert_pres_all","midden_feature_sem","fish_feature_sem")] <- lapply(master_transect[,c("seaweed_all", "fish_all", "marine_invert_pres_all","midden_feature_sem","fish_feature_sem")], ordered)
master_island<-fish_richness_merged_isl_arch_2
write.csv(master_island, "C:Biodiversity idea//Output files//master_island.csv", row.names=FALSE)

head(master_island)
length(unique(master_island$unq_isl))
#101 