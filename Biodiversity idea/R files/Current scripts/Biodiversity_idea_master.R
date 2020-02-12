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

fish_bycatch_richness_merged_tran_year<- read.csv("C:Biodiversity idea//Output files//fish_bycatch_richness_merged_tran_year.csv", row.names=FALSE)



# Matching terrestrial transects to beachseine sites ----------------------

#This is one option: 
# #pulls from output files of R script - "Assigned points"
# hakai_sites_distance_tran<-read.csv("C:Biodiversity idea//Output files//Distance_btwn_points_transects.csv")
# hakai_sites_distance_tran<-hakai_sites_distance_tran[,-1]
# ###head(hakai_sites_distance_tran)
# names(hakai_sites_distance_tran)[3]<-"unq_tran"
# names(hakai_sites_distance_tran)[6]<-"site"
# hakai_sites_distance_tran<- hakai_sites_distance_tran%>% filter(Distance < 5)
# ###head(hakai_sites_distance_tran )



#This is working with a 1km radius around the transects instead
hakai_sites_distance_tran<-read.csv("Biodiversity idea//Output files//paired_sites_by_radius.csv")
hakai_sites_distance_tran<-hakai_sites_distance_tran[,-1]
##View(hakai_sites_distance_tran)
length(unique(hakai_sites_distance_tran$unq_tran))
#30 unique transects if using 1km radius , 49 if I use 1.5km, 61 if I use 2km 

fish_bycatch_richness_merged_tran<-merge(fish_bycatch_richness_merged_tran_year, hakai_sites_distance_tran, by="site")

head(fish_bycatch_richness_merged_tran)
length(unique(fish_bycatch_richness_merged_tran$unq_tran))

### averaging across sites
fish_bycatch_richness_merged_tran <- fish_bycatch_richness_merged_tran %>% group_by(unq_tran) %>% summarise_if(is.numeric, mean, na.rm=TRUE)

xs4<-quantile(fish_bycatch_richness_merged_tran$fish_biomass_bym3_mean,c(0,0.25,0.75,1))
labels4 <- c("low fish biomass", "med fish biomass", "high fish biomass")
fish_bycatch_richness_merged_tran<- fish_bycatch_richness_merged_tran %>% mutate(fish_biomass_bym3_cat_tran = cut(fish_biomass_bym3_mean, xs4, labels = labels4))
fish_bycatch_richness_merged_tran$fish_biomass_bym3_cat_tran[fish_bycatch_richness_merged_tran$fish_biomass_bym3_mean<2]<-"low fish biomass"

#fish_bycatch_richness_merged_tran$unq_isl<-gsub('.{1}$', '', fish_bycatch_richness_merged_tran$unq_tran)



###FISH with unq_tran added but no transect data NO UNQ_ISL
write.csv(fish_bycatch_richness_merged_tran, "C:Biodiversity idea//Output files//fish_bycatch_richness_merged_tran.csv", row.names=FALSE)
head(fish_bycatch_richness_merged_tran)
#22 islands with 1km, 36 with 1.5km, 40 with 2 km 



# Loading and merging terrestrial data (at 0m) by transect ------------------------------------

#transect data
by_tran_master_0m<-read.csv("C:Food web idea//Data by person//Norah.data//by_tran_master_0m.csv")
#by_tran_master_0m<-by_tran_master_0m[,-1]

#### need to change the by_ran master file b/c it has repeat transects! 
by_tran_master_0m_2<-by_tran_master_0m 

xs3=quantile(na.omit(by_tran_master_0m_2$d15n),c(0,1/2, 1))
labels3 <- c("low d15N", "high d15N")
by_tran_master_0m_2<- by_tran_master_0m %>% mutate(d15n.cat = cut(d15n, xs3, labels = labels3))
by_tran_master_0m_2$d15n.cat[by_tran_master_0m_2$d15n<0]<-"low d15N"
by_tran_master_0m_2$d15n.cat[by_tran_master_0m_2$d15n>19]<-"high d15N"

by_tran_master_0m_2$d15N_over_N<-by_tran_master_0m_2$d15n/by_tran_master_0m_2$n


head(by_tran_master_0m_2)
by_tran_master_0m_2<-by_tran_master_0m_2 %>% select(-unq_isl)
### adding in tree diversity (transect level)
by_tran_master<-read.csv("C:Food web idea//Data by person//Norah.data//by_tran_master.csv")
by_tran_master_col_names<-c("unq_isl", "unq_tran", "tree_richness" ,
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
                            "SLOPE")


by_tran_master_subset<-by_tran_master[,colnames(by_tran_master) %in% by_tran_master_col_names]
by_tran_master_subset <- by_tran_master_subset %>% group_by(unq_tran)


by_tran_master_0m_with_tran<-merge(by_tran_master_0m_2, by_tran_master_subset, by="unq_tran", all.x=TRUE)
head(by_tran_master_0m_with_tran)



#merging terrestrial with marine and adding in marine site information, saving file
fish_richness_merged_tran<-merge(fish_bycatch_richness_merged_tran, by_tran_master_0m_with_tran, by="unq_tran", all.y=TRUE)
head(fish_richness_merged_tran)


fish_richness_merged_tran$combined_richness_corrected<-fish_richness_merged_tran$wrack_richness+fish_richness_merged_tran$marine_richness_corrected
fish_richness_merged_tran$eelgrass_cover_2km<-(fish_richness_merged_tran$MEAN_egarea2k)/(fish_richness_merged_tran$Radius_m_2000)
fish_richness_merged_tran$habitat_cover_2km<-(fish_richness_merged_tran$sum_2km)/(fish_richness_merged_tran$Radius_m_2000)


write.csv(fish_richness_merged_tran, "C:Biodiversity idea//Output files//fish_richness_merged_tran.csv", row.names=FALSE)
###View(fish_richness_merged_tran)

length(unique(fish_richness_merged_tran$unq_tran))
#392 tran ... b/c this is all in the 0m file... 

##View(fish_richness_merged_tran)




#### adding in arch sites
arch_sites_distance_tran<-read.csv("Biodiversity idea//Output files//paired_arch_by_radius_300.csv")
arch_sites_distance_tran<-arch_sites_distance_tran[,-1]
head(arch_sites_distance_tran)
length(unique(arch_sites_distance_tran$unq_tran))
#20 unique transects if using 300m radius 

fish_richness_merged_tran_arch<-merge(fish_richness_merged_tran, arch_sites_distance_tran, by="unq_tran", all.x=TRUE)

head(fish_richness_merged_tran_arch)
length(unique(fish_richness_merged_tran_arch$unq_tran))

##adding in arch data
arch_data<-read.csv("C:Biodiversity idea//Output files//arch_sites_selected.csv")
head(arch_data)
arch_data_simple<-arch_data[ , c("site_id", "CMT", "clam_garden", "midden_feature", "fish_feature")]
head(arch_data_simple)

fish_richness_merged_tran_arch<-merge(fish_richness_merged_tran_arch, arch_data_simple, by="site_id", all.x=TRUE)
head(fish_richness_merged_tran_arch)
fish_richness_merged_tran_arch$CMT<-as.factor(fish_richness_merged_tran_arch$CMT)
fish_richness_merged_tran_arch$clam_garden<-as.factor(fish_richness_merged_tran_arch$clam_garden)
fish_richness_merged_tran_arch$midden_feature<-factor(fish_richness_merged_tran_arch$midden_feature, ordered=TRUE)
fish_richness_merged_tran_arch$fish_feature<-as.factor(fish_richness_merged_tran_arch$fish_feature)

#for sem:
fish_richness_merged_tran_arch$midden_feature_sem<-fish_richness_merged_tran_arch$midden_feature
 recode(fish_richness_merged_tran_arch$midden_feature_sem, yes = "1", no="0")
fish_richness_merged_tran_arch$midden_feature_sem<-as.numeric(fish_richness_merged_tran_arch$midden_feature_sem)

write.csv(fish_richness_merged_tran_arch, "C:Biodiversity idea//Output files//fish_richness_merged_tran_arch.csv", row.names=FALSE)

##### adding in island-level characteristics to the transect file
head(fish_richness_merged_tran_arch)

by_isl_master<-read.csv("C:Food web idea//Data by person//Owen's data//by_isl_master.csv")
head(by_isl_master)
by_isl_master_col_names_tran<-c("unq_isl",
                          "PA_norml",
                           "eagles",
                           "ravens",
                           "node", 
                          "site_sum_by_isl", 
                          "Rock")

#ravens eagles site_sum_by_isl SLOPE PA_norml Rock slope_mean

by_isl_master_subset_tran<-by_isl_master[,colnames(by_isl_master) %in% by_isl_master_col_names_tran]
##head(by_isl_master_subset)

head(fish_richness_merged_tran_arch)
fish_richness_merged_tran_arch_2 <-merge(fish_richness_merged_tran_arch, by_isl_master_subset_tran, by="unq_isl", all.y=TRUE)






# unq_isl -----------------------------------------------------------------


#adding in a few interesting island-level components
by_isl_master<-read.csv("C:Food web idea//Data by person//Owen's data//by_isl_master.csv")
by_isl_master<-by_isl_master[,-1]
##head(by_isl_master)
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
##head(by_isl_master_subset)


#head(fish_bycatch_richness_merged_tran)
fish_bycatch_richness_merged_isl <-fish_bycatch_richness_merged_tran %>% group_by(unq_isl) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE)


fish_richness_merged_isl<-merge(fish_bycatch_richness_merged_isl, by_isl_master, by="unq_isl", all.y=TRUE)
#head(fish_richness_merged_isl)
length(unique(fish_richness_merged_isl$unq_isl))
#103 islands

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


write.csv(fish_richness_merged_isl, "C:Biodiversity idea//Output files//fish_richness_merged_isl.csv", row.names=FALSE)


