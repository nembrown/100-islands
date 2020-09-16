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
# I didn't recalculate the radius for the centre of the island so as to not bias the big vs. small islands, since a big island with a center gps point would have way less water area
fish_bycatch_richness_merged_isl <- fish_bycatch_richness_merged_isl %>% group_by(unq_isl) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
length(fish_bycatch_richness_merged_isl$unq_isl)

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
fish_richness_merged_isl_arch_2 <-merge(fish_richness_merged_isl_arch, by_isl_master, by="unq_isl", all.x=TRUE)
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



hist(fish_richness_merged_isl_arch_2$plot_cc)

master_transect$slope_isl
###########################################################










plot(master_transect$log_Area~ log(master_transect$Bog_area+1))
hist(master_transect$site_mean_by_tran, breaks=100)



master_transect<- master_transect[complete.cases(master_transect$node), ]

master_transect$super_node<-master_transect$node
master_transect$super_node[master_transect$node=="PR"]<-"SCPR"
master_transect$super_node[master_transect$node=="SC"]<-"SCPR"
master_transect$super_node[master_transect$node=="TB"]<-"TBADMMGS"
master_transect$super_node[master_transect$node=="AD"]<-"TBADMMGS"
master_transect$super_node[master_transect$node=="MM"]<-"TBADMMGS"
master_transect$super_node[master_transect$node=="GS"]<-"TBADMMGS"
# master_transect$super_node[master_transect$node=="TQ"]<-"TQSTCV"
# master_transect$super_node[master_transect$node=="ST"]<-"TQSTCV"
# master_transect$super_node[master_transect$node=="CV"]<-"TQSTCV"

master_transect$super_node



# unq_isl -----------------------------------------------------------------


#adding in a few interesting island-level components
by_isl_master<-read.csv("C:Food web idea//Data by person//Owen's data//by_isl_master.csv")
head(by_isl_master)
by_isl_master_col_names<-c("unq_isl",
       "mammal_richness",
       "total_richness",
       "bird.richness",
       "bird.density",
       "habitat_het",
       "log_Area",
       "Neighb_250",
       "NDVI_mean",
       "Perimeter",
       "PA_norml",
       "DistW_ML",
       "Dist_Near",
       "Area",
       "size.cat2",
       "eagles",
       "ravens",
       "node",
       "SLOPE",
       "slope_mean",
       "Radius_m_2000",
       "sum_2km",
       "SITE_SUM",
       "WAVE_EXPOSURE" )

by_isl_master_subset<-by_isl_master[,colnames(by_isl_master) %in% by_isl_master_col_names]
###head(by_isl_master_subset)

###head(fish_bycatch_richness_merged_isl)
fish_bycatch_richness_merged_isl$unq_isl<-str_sub(fish_bycatch_richness_merged_isl$unq_tran, start=0, end=4)

##head(fish_bycatch_richness_merged_isl)
fish_bycatch_richness_merged_isl <-fish_bycatch_richness_merged_isl %>% group_by(unq_isl) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE)


fish_richness_merged_isl<-merge(fish_bycatch_richness_merged_isl, by_isl_master, by="unq_isl", all.y=TRUE)
##head(fish_richness_merged_isl)
length(unique(fish_richness_merged_isl$unq_isl))
#102 islands

xs4<- quantile(na.omit(fish_richness_merged_isl$fish_biomass_bym3_mean),c(0,0.25,0.75, 1))
labels4 <- c("low fish biomass", "med fish biomass", "high fish biomass")
fish_richness_merged_isl<- fish_richness_merged_isl %>% 
  mutate(fish_biomass_bym3_cat_isl = cut(fish_biomass_bym3_mean, xs4, labels = labels4))

xs_N<- quantile(na.omit(fish_richness_merged_isl$d15n),c(0,0.25,0.75, 1))
labels_N <- c("low N15", "med N15", "high N15")
fish_richness_merged_isl<- fish_richness_merged_isl %>% 
  mutate(d15n_cat_isl = cut(d15n, xs_N, labels = labels_N))

#d34S isn't included at the island level.... 
xs_S<- quantile(na.omit(fish_richness_merged_isl$d34s),c(0,0.25,0.75, 1))
labels_S <- c("low S34", "med S34", "high S34")
fish_richness_merged_isl<- fish_richness_merged_isl %>% 
  mutate(d34s_cat_isl = cut(d34s, xs_S, labels = labels_S))


fish_richness_merged_isl$combined_richness_corrected<-fish_richness_merged_isl$wrack_richness+fish_richness_merged_isl$marine_richness_corrected
fish_richness_merged_isl$eelgrass_cover_2km<-(fish_richness_merged_isl$MEAN_egarea2k)/(fish_richness_merged_isl$Radius_m_2000)
fish_richness_merged_isl$habitat_cover_2km<-(fish_richness_merged_isl$sum_2km)/(fish_richness_merged_isl$Radius_m_2000)
fish_richness_merged_isl$d15n_over_N<-(fish_richness_merged_isl$d15n)/(fish_richness_merged_isl$n)

  
  
xs_habcover<- quantile(na.omit(fish_richness_merged_isl$habitat_cover_2km),c(0,0.25,0.75, 1))
labels_habcover <- c("low habitat cover", "med habitat cover", "high habitat cover")
fish_richness_merged_isl<- fish_richness_merged_isl %>% 
  mutate(habcover_cat_isl = cut(habitat_cover_2km, xs_habcover, labels = labels_habcover))


xs_fish.richness<- quantile(na.omit(fish_richness_merged_isl$fish_richness_corrected),c(0,0.25,0.75, 1))
labels_fish.richness <- c("low fish.richness", "med fish.richness", "high fish.richness")
fish_richness_merged_isl<- fish_richness_merged_isl %>% 
  mutate(fish.richness_cat_isl = cut(fish_richness_corrected, xs_fish.richness, labels = labels_fish.richness))

xs_wrack.richness<- quantile(na.omit(fish_richness_merged_isl$wrack_richness),c(0,0.25,0.75, 1))
fish_richness_merged_isl$wrack.richness_cat_isl[fish_richness_merged_isl$wrack_richness==0]<-"no wrack"
fish_richness_merged_isl$wrack.richness_cat_isl[fish_richness_merged_isl$wrack_richness>0 & fish_richness_merged_isl$wrack_richness<10]<-"low wrack richness"
fish_richness_merged_isl$wrack.richness_cat_isl[fish_richness_merged_isl$wrack_richness>10]<-"high wrack richness"

xs_SITE_SUM<- quantile(na.omit(fish_richness_merged_isl$SITE_SUM),c(0,0.25,0.75, 1))
fish_richness_merged_isl$SITE_SUM_cat_isl[fish_richness_merged_isl$SITE_SUM==0]<-"no wrack"
fish_richness_merged_isl$SITE_SUM_cat_isl[fish_richness_merged_isl$SITE_SUM>0 & fish_richness_merged_isl$SITE_SUM<169]<-"low beach wrack"
fish_richness_merged_isl$SITE_SUM_cat_isl[fish_richness_merged_isl$SITE_SUM>169]<-"high beach wrack"

fish_richness_merged_isl$bird.density<-as.numeric(as.character(fish_richness_merged_isl$bird.density))
xs_bird.density<- quantile(na.omit(fish_richness_merged_isl$bird.density),c(0,0.25,0.75, 1))
fish_richness_merged_isl$bird.density_cat_isl[fish_richness_merged_isl$bird.density<13]<-"low bird density"
fish_richness_merged_isl$bird.density_cat_isl[fish_richness_merged_isl$bird.density>13 & fish_richness_merged_isl$bird.density<26]<-"med bird density"
fish_richness_merged_isl$bird.density_cat_isl[fish_richness_merged_isl$bird.density>26]<-"high bird density"


xs_schooling_fish_biomass_bym3_mean<- quantile(na.omit(fish_richness_merged_isl$schooling_fish_biomass_bym3_mean),c(0,0.25,0.75, 1))
labels_schooling_fish_biomass_bym3_mean <- c("low schooling fish biomass", "med schooling fish biomass", "high schooling fish biomass")
fish_richness_merged_isl<- fish_richness_merged_isl %>% 
  mutate(schooling_fish_biomass_bym3_mean_cat_isl = cut(schooling_fish_biomass_bym3_mean, xs_schooling_fish_biomass_bym3_mean, labels = labels_schooling_fish_biomass_bym3_mean))

xs_individual_fish_biomass_bym3_mean<- quantile(na.omit(fish_richness_merged_isl$individual_fish_biomass_bym3_mean),c(0,0.25,0.75, 1))
labels_individual_fish_biomass_bym3_mean <- c("low individual fish biomass", "med individual fish biomass", "high individual fish biomass")
fish_richness_merged_isl<- fish_richness_merged_isl %>% 
  mutate(individual_fish_biomass_bym3_mean_cat_isl = cut(individual_fish_biomass_bym3_mean, xs_individual_fish_biomass_bym3_mean, labels = labels_individual_fish_biomass_bym3_mean))

xs_bycatch_biomass_bym3_mean<- quantile(na.omit(fish_richness_merged_isl$bycatch_biomass_bym3_mean),c(0,0.25,0.75, 1))
labels_bycatch_biomass_bym3_mean <- c("low invert biomass", "med invert biomass", "high invert biomass")
fish_richness_merged_isl<- fish_richness_merged_isl %>% 
  mutate(bycatch_biomass_bym3_mean_cat_isl = cut(bycatch_biomass_bym3_mean, xs_bycatch_biomass_bym3_mean, labels = labels_bycatch_biomass_bym3_mean))


write.csv(fish_richness_merged_isl, "C:Biodiversity idea//Output files//master_island.csv", row.names=FALSE)

head(fish_richness_merged_isl)
