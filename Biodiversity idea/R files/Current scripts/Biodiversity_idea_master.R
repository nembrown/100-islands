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
fish_bycatch_richness_merged_tran_year<- read.csv("C:Biodiversity idea//Output files//fish_bycatch_richness_merged_tran_year.csv")


# Matching terrestrial transects to beachseine sites ----------------------

##pulls from output files of R script - "Assign closest points to radius"
#This is working with a 2km radius around the transects - pairing beachseine sites to transects within 2km
hakai_sites_distance_tran<-read.csv("Biodiversity idea//Output files//paired_sites_by_radius.csv")
length(unique(hakai_sites_distance_tran$unq_tran))
#263 transects match within 2km! 

fish_bycatch_richness_merged_tran<-merge(fish_bycatch_richness_merged_tran_year, hakai_sites_distance_tran, by="site")

#head(fish_bycatch_richness_merged_tran)
length(unique(fish_bycatch_richness_merged_tran$unq_tran))

### averaging across sites
fish_bycatch_richness_merged_tran <- fish_bycatch_richness_merged_tran %>% group_by(unq_tran) %>% summarise_if(is.numeric, mean, na.rm=TRUE)

# xs4<-quantile(fish_bycatch_richness_merged_tran$fish_biomass_bym3_mean,c(0,0.25,0.75,1))
# labels4 <- c("low fish biomass", "med fish biomass", "high fish biomass")
# fish_bycatch_richness_merged_tran<- fish_bycatch_richness_merged_tran %>% mutate(fish_biomass_bym3_cat_tran = cut(fish_biomass_bym3_mean, xs4, labels = labels4))
# fish_bycatch_richness_merged_tran$fish_biomass_bym3_cat_tran[fish_bycatch_richness_merged_tran$fish_biomass_bym3_mean<2]<-"low fish biomass"

#fish_bycatch_richness_merged_tran$unq_isl<-gsub('.{1}$', '', fish_bycatch_richness_merged_tran$unq_tran)

###FISH with unq_tran added but no transect data NO UNQ_ISL
write.csv(fish_bycatch_richness_merged_tran, "C:Biodiversity idea//Output files//fish_bycatch_richness_merged_tran.csv", row.names=FALSE)
head(fish_bycatch_richness_merged_tran)



# Loading terrestrial data by transect ------------------------------------

#transect data at the shoreline - i.e. 0m plot
by_tran_master_0m<-read.csv("C:Food web idea//Data by person//Norah.data//by_tran_master_0m.csv")

#### need to change the by_tran master file b/c it has repeat transects! 

#making changes to the 0m transect file
by_tran_master_0m_2<-by_tran_master_0m 

xs3=quantile(na.omit(by_tran_master_0m_2$d15n),c(0,1/2, 1))
labels3 <- c("low d15N", "high d15N")
by_tran_master_0m_2<- by_tran_master_0m %>% mutate(d15n.cat = cut(d15n, xs3, labels = labels3))
by_tran_master_0m_2$d15n.cat[by_tran_master_0m_2$d15n<0]<-"low d15N"
by_tran_master_0m_2$d15n.cat[by_tran_master_0m_2$d15n>19]<-"high d15N"

by_tran_master_0m_2$d15N_over_N<-by_tran_master_0m_2$d15n/by_tran_master_0m_2$n


head(by_tran_master_0m_2)

by_tran_master_0m_2$slope_degrees<-(180*(atan(by_tran_master_0m_2$slope/100)))/pi

#taking out unq_isl for merging purposes
by_tran_master_0m_2<-by_tran_master_0m_2 %>% dplyr::select(-unq_isl)

### adding in tree diversity (at the whole transect level, i.e. from 0 to 40m)
by_tran_master<-read.csv("C:Food web idea//Data by person//Norah.data//by_tran_master.csv")
head(by_tran_master)
by_tran_master_col_names<-c("unq_isl", "unq_tran","plot_cc", "tree_richness" ,
                            "tree_abundance",
                            "sum_basal",
                            "site_mean_by_tran" ,
                            "wrack_richness",
                            "HAB2000",
                            "MEAN_egarea2k",
                            "MEAN_kparea2k",
                            "MEAN_rockarea2000",
                            "Radius_m_2000",
                            "sum_2km",
                            "WAVE_EXPOSURE",
                            "SITE_SUM",
                            "SLOPE",
                            "SUBSTRATE")


by_tran_master_subset<-by_tran_master[,colnames(by_tran_master) %in% by_tran_master_col_names]
by_tran_master_subset <- by_tran_master_subset %>% group_by(unq_tran)

by_tran_master_subset$beachy_substrate<- ifelse(grepl("ROCK", by_tran_master_subset$SUBSTRATE), "0", "1")
by_tran_master_subset$beachy_substrate<-as.character(by_tran_master_subset$beachy_substrate)
by_tran_master_subset$beachy_substrate<-as.numeric(by_tran_master_subset$beachy_substrate)
by_tran_master_subset$SLOPE_degrees<-(180*(atan(by_tran_master_subset$SLOPE/100)))/pi



by_tran_master_0m_with_tran<-merge(by_tran_master_0m_2, by_tran_master_subset, by="unq_tran", all=TRUE)
head(by_tran_master_0m_with_tran)




# merging terrestrial with marine and adding in marine site information --------

fish_richness_merged_tran<-merge(fish_bycatch_richness_merged_tran, by_tran_master_0m_with_tran, by="unq_tran", all.y=TRUE)
#head(fish_richness_merged_tran)


fish_richness_merged_tran$combined_richness_corrected<-fish_richness_merged_tran$wrack_richness+fish_richness_merged_tran$marine_richness_corrected
fish_richness_merged_tran$eelgrass_cover_2km<-(fish_richness_merged_tran$MEAN_egarea2k)/(fish_richness_merged_tran$Radius_m_2000)
fish_richness_merged_tran$habitat_cover_2km<-(fish_richness_merged_tran$sum_2km)/(fish_richness_merged_tran$Radius_m_2000)


write.csv(fish_richness_merged_tran, "C:Biodiversity idea//Output files//fish_richness_merged_tran.csv", row.names=FALSE)
#####View(fish_richness_merged_tran)

length(unique(fish_richness_merged_tran$unq_tran))
#565 tran but that includes Is and Bs


# adding in arch sites ----------------------------------------------------
#arch sites paired
arch_sites_distance_tran<-read.csv("Biodiversity idea//Output files//paired_arch_by_radius_300.csv")
head(arch_sites_distance_tran)
length(unique(arch_sites_distance_tran$unq_tran))
#81 unique transects if using 300m radius 

fish_richness_merged_tran_arch<-merge(fish_richness_merged_tran, arch_sites_distance_tran, by="unq_tran", all.x=TRUE)

#head(fish_richness_merged_tran_arch)
length(unique(fish_richness_merged_tran_arch$unq_tran))

##adding in arch data from output file fed from arch sites cleaning.R
arch_data<-read.csv("C:Biodiversity idea//Output files//arch_sites_selected.csv")
#head(arch_data)
arch_data_simple<-arch_data[ , c("site_id", "CMT", "clam_garden", "midden_feature", "fish_feature", "canoe_skid")]
#head(arch_data_simple)

fish_richness_merged_tran_arch<-merge(fish_richness_merged_tran_arch, arch_data_simple, by="site_id", all.x=TRUE)
head(fish_richness_merged_tran_arch)

#for sem:
fish_richness_merged_tran_arch$midden_feature_sem<-as.character(fish_richness_merged_tran_arch$midden_feature)
fish_richness_merged_tran_arch$midden_feature_sem<- dplyr::recode(fish_richness_merged_tran_arch$midden_feature_sem, yes = "1", no="0")
fish_richness_merged_tran_arch$midden_feature_sem[is.na(fish_richness_merged_tran_arch$midden_feature_sem)] <- 0
fish_richness_merged_tran_arch$midden_feature_sem<-as.numeric(fish_richness_merged_tran_arch$midden_feature_sem)

fish_richness_merged_tran_arch$fish_feature_sem<-as.character(fish_richness_merged_tran_arch$fish_feature)
fish_richness_merged_tran_arch$fish_feature_sem<-dplyr::recode(fish_richness_merged_tran_arch$fish_feature_sem, yes = "1", no="0")
fish_richness_merged_tran_arch$fish_feature_sem[is.na(fish_richness_merged_tran_arch$fish_feature_sem)] <- 0
fish_richness_merged_tran_arch$fish_feature_sem<-as.numeric(fish_richness_merged_tran_arch$fish_feature_sem)

fish_richness_merged_tran_arch$canoe_skid_sem<-as.character(fish_richness_merged_tran_arch$canoe_skid)
fish_richness_merged_tran_arch$canoe_skid_sem<-dplyr::recode(fish_richness_merged_tran_arch$canoe_skid_sem, yes = "1", no="0")
fish_richness_merged_tran_arch$canoe_skid_sem[is.na(fish_richness_merged_tran_arch$canoe_skid_sem)] <- 0
fish_richness_merged_tran_arch$canoe_skid_sem<-as.numeric(fish_richness_merged_tran_arch$canoe_skid_sem)


fish_richness_merged_tran_arch$CMT<-as.factor(fish_richness_merged_tran_arch$CMT)
fish_richness_merged_tran_arch$clam_garden<-as.factor(fish_richness_merged_tran_arch$clam_garden)
fish_richness_merged_tran_arch$midden_feature<-factor(fish_richness_merged_tran_arch$midden_feature)
fish_richness_merged_tran_arch$fish_feature<-as.factor(fish_richness_merged_tran_arch$fish_feature)
fish_richness_merged_tran_arch$canoe_skid<-as.factor(fish_richness_merged_tran_arch$canoe_skid)



#### Adding in arch site by distance

distance_btwn_points_midden_transects<- read.csv("C:Biodiversity idea//Output files//Distance_btwn_points_midden_transects.csv")
head(distance_btwn_points_midden_transects)
names(distance_btwn_points_midden_transects)[3]<-"unq_tran"
names(distance_btwn_points_midden_transects)[5]<-"distance_to_midden"


fish_richness_merged_tran_arch<-merge(fish_richness_merged_tran_arch, distance_btwn_points_midden_transects[,c(3,5)], by="unq_tran")
head(fish_richness_merged_tran_arch)

distance_btwn_points_any_arch_transects<- read.csv("C:Biodiversity idea//Output files//Distance_btwn_points_any_arch_transects.csv")
##View(distance_btwn_points_any_arch_transects)
names(distance_btwn_points_any_arch_transects)[3]<-"unq_tran"
names(distance_btwn_points_any_arch_transects)[5]<-"distance_to_any_arch"

fish_richness_merged_tran_arch<-merge(fish_richness_merged_tran_arch, distance_btwn_points_any_arch_transects[,c(3,5)], by="unq_tran")
head(fish_richness_merged_tran_arch)


distance_btwn_points_fish_transects<- read.csv("C:Biodiversity idea//Output files//Distance_btwn_points_fish_transects.csv")
##View(distance_btwn_points_fish_transects)
names(distance_btwn_points_fish_transects)[3]<-"unq_tran"
names(distance_btwn_points_fish_transects)[5]<-"distance_to_fish"

fish_richness_merged_tran_arch<-merge(fish_richness_merged_tran_arch, distance_btwn_points_fish_transects[,c(3,5)], by="unq_tran")
#View(fish_richness_merged_tran_arch)

write.csv(fish_richness_merged_tran_arch, "C:Biodiversity idea//Output files//fish_richness_merged_tran_arch.csv", row.names=FALSE)



###culturally important plants
plant_data_cult_richness<- read.csv("C:Biodiversity idea//Output files//plant_data_cult_richness.csv")
head(plant_data_cult_richness)
fish_richness_merged_tran_arch_2<-merge(fish_richness_merged_tran_arch, plant_data_cult_richness, all=TRUE)
head(fish_richness_merged_tran_arch_2)


### adding marine remains from owen and chris's notes - changed away from pres_abs
combined_otter_mean_tran<- read.csv("C:Biodiversity idea//Output files//combined_otter_mean_tran.csv")
head(combined_otter_mean_tran)
hist(combined_otter_mean_tran$pres_fish)

fish_richness_merged_tran_arch_2<-merge(fish_richness_merged_tran_arch_2, combined_otter_mean_tran, by="unq_tran", all=TRUE)

fish_richness_merged_tran_arch_2$unq_isl<-strtrim(fish_richness_merged_tran_arch_2$unq_tran, 4)
fish_richness_merged_tran_arch_2$node<-strtrim(fish_richness_merged_tran_arch_2$unq_tran, 2)

head(fish_richness_merged_tran_arch_2)


##### adding in island-level characteristics to the transect file
#head(fish_richness_merged_tran_arch)

by_isl_master<-read.csv("C:Food web idea//Data by person//Owen's data//by_isl_master.csv")
head(by_isl_master)
by_isl_master_col_names_tran<-c("unq_isl",
                                "Area",
                                "PA_norml",
                                "eagles",
                                "ravens",
                                "site_sum_by_isl", 
                                "Rock", 
                                "DistW_ML", 
                                "Dist_Near", 
                                "habitat_het", "elevation_max", "elevation_mean", 
                                "slope_mean", "NDVI_mean", "Neighb_250", "Bog_Water", "Bog_Vegetation")

by_isl_master_subset_tran<-by_isl_master[,colnames(by_isl_master) %in% by_isl_master_col_names_tran]
head(by_isl_master_subset_tran)

by_isl_master_subset_tran$log_Area<-log(by_isl_master_subset_tran$Area)

#head(fish_richness_merged_tran_arch)
fish_richness_merged_tran_arch_2 <-merge(fish_richness_merged_tran_arch_2, by_isl_master_subset_tran, by="unq_isl", all.x=TRUE)
fish_richness_merged_tran_arch_2$log_site_mean_by_tran <- log(fish_richness_merged_tran_arch_2$site_mean_by_tran+1)
fish_richness_merged_tran_arch_2$log_MEAN_kparea2k <- log(fish_richness_merged_tran_arch_2$MEAN_kparea2k+1)
fish_richness_merged_tran_arch_2$log_MEAN_egarea2k <- log(fish_richness_merged_tran_arch_2$MEAN_egarea2k+1)
fish_richness_merged_tran_arch_2$log_Rock<- log(fish_richness_merged_tran_arch_2$Rock+1)
fish_richness_merged_tran_arch_2$log_DistW_ML<- log(fish_richness_merged_tran_arch_2$DistW_ML)
fish_richness_merged_tran_arch_2$log_Dist_NearL<- log(fish_richness_merged_tran_arch_2$Dist_Near)
fish_richness_merged_tran_arch_2$log_distance_to_midden<- log(fish_richness_merged_tran_arch_2$distance_to_midden)
fish_richness_merged_tran_arch_2$Bog_area<- fish_richness_merged_tran_arch_2$Bog_Vegetation + fish_richness_merged_tran_arch_2$Bog_Water 
fish_richness_merged_tran_arch_2$log_Bog_area<- log(fish_richness_merged_tran_arch_2$Bog_area+1)



#master_transect[,c("seaweed_all", "fish_all", "marine_invert_pres_all","midden_feature_sem","fish_feature_sem")] <- lapply(master_transect[,c("seaweed_all", "fish_all", "marine_invert_pres_all","midden_feature_sem","fish_feature_sem")], ordered)
master_transect<-fish_richness_merged_tran_arch_2

View(master_transect)


write.csv(master_transect, "C:Biodiversity idea//Output files//master_transect.csv", row.names=FALSE)


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

###View(fish_bycatch_richness_merged_tran)
fish_bycatch_richness_merged_tran$unq_isl<-str_sub(fish_bycatch_richness_merged_tran$unq_tran, start=0, end=4)

##head(fish_bycatch_richness_merged_tran)
fish_bycatch_richness_merged_isl <-fish_bycatch_richness_merged_tran %>% group_by(unq_isl) %>%
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
