#loading necesasry libraries

library(here)
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
library(cowplot)



# dataframes --------------------------------------------------------------


fish_richness_merged_isl<-read.csv("C:Biodiversity idea//Output files//fish_richness_merged_isl.csv")
# View(fish_richness_merged_isl)

isotope_master<-read.csv("C:Food web idea//Data by person//Owen's data/isotope_master.csv")
head(isotope_master)
levels(isotope_master$group)


isotope_master_fish<-merge(isotope_by_isl_gathered4, fish_richness_merged_isl, by="unq_isl")

head(isotope_master_fish)
isotope_master_fish_feathers<-isotope_master_fish %>% filter(group =="bird_feathers")
xs_C_feathers<- quantile(na.omit(isotope_master_fish_feathers$d13c.x),c(0,0.25,0.75, 1))
labels_C_feathers <- c("low d13c feathers", "med d13c feathers", "high d13c feathers")
isotope_master_fish_feathers<- isotope_master_fish_feathers%>% 
  mutate(d13c_feathers_cat_isl = cut(d13c.x, xs_C_feathers, labels = labels_C_feathers))

isotope_master_fish_feces<-isotope_master_fish %>% filter(group =="bird_feces")
xs_C_feces<- quantile(na.omit(isotope_master_fish_feces$d13c.x),c(0,0.25,0.75, 1))
labels_C_feces <- c("low d13c feces", "med d13c feces", "high d13c feces")
isotope_master_fish_feces<- isotope_master_fish_feces%>% 
  mutate(d13c_feces_cat_isl = cut(d13c.x, xs_C_feces, labels = labels_C_feces))



#Correlations
head(master_transect)

sem_variables_names<-c( "fish_biomass_bym3_mean", "bycatch_biomass_bym3_mean", "MEAN_kparea2k", "MEAN_egarea2k",
                        "SLOPE", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope", "site_sum_by_isl",
                        "ravens", "midden_feature_sem", "fish_feature_sem", "cult_imp_plant_richness", "d15n",
                        "otter_pres_all", "marine_invert_pres_all", "fish_all", "distance_to_any_arch", "distance_to_midden",
                        "distance_to_fish")

master_transec_sem_subset<-master_transect[, colnames(master_transect) %in% sem_variables_names]
str(master_transec_sem_subset)

corr_by_isl_selected_2 <- round(cor(master_transec_sem_subset, use="pairwise.complete.obs"), 1)
#head(corr_by_isl_selected_2[, 1:6])
p.mat_by_isl_selected_2 <- cor_pmat(master_transec_sem_subset)
#head(p.mat_by_isl_selected_2[, 1:4])

ggcorrplot(corr_by_isl_selected_2)
ggcorrplot(corr_by_isl_selected_2, hc.order = TRUE, type = "lower",
           outline.col = "white")
ggcorrplot(corr_by_isl_selected_2, hc.order = TRUE,
           type = "lower", p.mat = p.mat_by_isl_selected_2)
ggcorrplot(corr_by_isl_selected_2, hc.order = TRUE, type = "lower",
           lab = TRUE)


#Network plot
master_transec_sem_subset %>% correlate() %>%  network_plot(min_cor = 0.1)

#using Performance Analytics
chart.Correlation(master_transec_sem_subset , histogram=TRUE, pch=19)


human_influence_variables_names<-c( "fish_biomass_bym3_mean", "bycatch_biomass_bym3_mean", "MEAN_kparea2k", "MEAN_egarea2k",
                        "SLOPE", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope", "midden_feature_sem", "fish_feature_sem", "cult_imp_plant_richness", "d15n",
                         "distance_to_midden", "distance_to_fish", "log_DistW_ML", "log_Dist_Near", "habitat_het", "elevation_max", "elevation_mean", "slope_mean", "NDVI_mean", "Neighb_250")

master_transec_sem_subset<-master_transect[, colnames(master_transect) %in% human_influence_variables_names]
str(master_transec_sem_subset)

corr_by_isl_selected_2 <- round(cor(master_transec_sem_subset, use="pairwise.complete.obs"), 1)
p.mat_by_isl_selected_2 <- cor_pmat(master_transec_sem_subset)
ggcorrplot(corr_by_isl_selected_2, hc.order = TRUE, type = "lower",lab = TRUE)

#Network plot
master_transec_sem_subset %>% correlate() %>%  network_plot(min_cor = 0.1)

#using Performance Analytics
chart.Correlation(master_transec_sem_subset , histogram=TRUE, pch=19)


ggplot(master_transect, aes(x=log_distance_to_midden, y=d15n))+ geom_point()+geom_smooth(method="lm")


# Arch --------------------------------------------------------------------


fish_bycatch_richness_merged_tran_year_arch<-read.csv("C:Biodiversity idea//Output files//fish_bycatch_richness_merged_tran_year_arch.csv")

### arch and fish
head(fish_bycatch_richness_merged_tran_year_arch)
# write.csv(fish_bycatch_richness_merged_tran_year_arch, "C:Biodiversity idea//Output files//fish_bycatch_richness_merged_tran_year_arch.csv", row.names=FALSE)

ggplot(fish_bycatch_richness_merged_tran_year_arch, aes(x=fish_feature, y=fish_pelagic_biomass_bym3_mean, col=fish_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_bycatch_richness_merged_tran_year_arch, aes(x=fish_feature, y=fish_biomass_bym3_mean, col=fish_feature))+
  geom_boxplot()+geom_jitter(width=0.2)



ggplot(fish_bycatch_richness_merged_tran_year_arch, aes(x=midden_feature, y=fish_pelagic_biomass_bym3_mean, col=midden_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

  ggplot(fish_bycatch_richness_merged_tran_year_arch, aes(x=midden_feature, y=fish_bycatch_biomass, col=midden_feature))+
  geom_boxplot()+geom_jitter(width=0.2)


ggplot(fish_bycatch_richness_merged_tran_year_arch, aes(x=fish_feature, y=fish_richness_corrected, col=fish_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_bycatch_richness_merged_tran_year_arch, aes(x=clam_garden, y=bycatch_richness_corrected, col=clam_garden))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_bycatch_richness_merged_tran_year_arch, aes(x=midden_feature, y=bycatch_richness_corrected, col=midden_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_bycatch_richness_merged_tran_year_arch, aes(x=fish_feature, y=schooling_fish_biomass_bym3_mean, col=fish_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

# Productivity vs. marin e subsidies ---------------------------------------

###All plants
plot_grid(fish_biomass_schooling_plant, fish_biomass_schooling_NDVI, fish_biomass_schooling_tree , ncol=3)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//plants.schooling.biomass.predictors.png", width=30, height=10, unit="cm")

plot_grid(fish_biomass_individual_plant, fish_biomass_individual_NDVI, fish_biomass_individual_tree , ncol=3)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//plants.individual.biomass.predictors.png", width=30, height=10, unit="cm")

plot_grid(fish_biomass_plant, fish_biomass_NDVI, fish_biomass_tree , ncol=3)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//plants.biomass.predictors.png", width=30, height=10, unit="cm")

plot_grid(soil.N_plant, soil.N_NDVI, soil.N_tree, soil.N_herb, soil.N_det, soil.N_carn, soil.N_birdfood, soil.N_bird, ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//all_N.soil.png", width=40, height=20, unit="cm")


plot_grid(soil.S_plant, soil.S_NDVI, soil.S_tree, soil.S_herb, soil.S_det, soil.S_carn, soil.S_birdfood, soil.S_bird,ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//all_S.soil.png", width=40, height=20, unit="cm")


plot_grid(fish_biomass_plant, fish_biomass_NDVI, fish_biomass_tree, fish_biomass_herb, fish_biomass_det, fish_biomass_carn, fish_biomass_bf, bird_fish_biomass,ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//all_fish_biomass.png", width=40, height=20, unit="cm")


plot_grid(plant_feather.plot.C, NDVI_feather.plot.C, tree_feather.plot.C, herb_feather.plot.C, det_feather.plot.C,carn_feather.plot.C, bf_feather.plot.C, bird_feather.plot.C,ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//all_feather.C.png", width=40, height=20, unit="cm")

plot_grid(plant_insects_ISO.plot.C, NDVI_insects_ISO.plot.C, tree_insects_ISO.plot.C, herb_insects_ISO.plot.C, det_insects_ISO.plot.C,carn_insects_ISO.plot.C, bf_insects_ISO.plot.C, bird_insects_ISO.plot.C,ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//all_insects_ISO.C.png", width=40, height=20, unit="cm")

plot_grid(plant_insects_COL.plot.C, NDVI_insects_COL.plot.C, tree_insects_COL.plot.C, herb_insects_COL.plot.C, det_insects_COL.plot.C,carn_insects_COL.plot.C, bf_insects_COL.plot.C, bird_insects_COL.plot.C,ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//all_insects_COL.C.png", width=40, height=20, unit="cm")

plot_grid(plant_Mouse_feces.plot.C, NDVI_Mouse_feces.plot.C, tree_Mouse_feces.plot.C, herb_Mouse_feces.plot.C, det_Mouse_feces.plot.C,carn_Mouse_feces.plot.C, bf_Mouse_feces.plot.C, bird_Mouse_feces.plot.C,ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//all_Mouse_feces.C.png", width=40, height=20, unit="cm")

plot_grid(plant_bird_feces.plot.C, NDVI_bird_feces.plot.C, tree_bird_feces.plot.C, herb_bird_feces.plot.C, det_bird_feces.plot.C,carn_bird_feces.plot.C, bf_bird_feces.plot.C, bird_bird_feces.plot.C,ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//all_bird_feces.C.png", width=40, height=20, unit="cm")

plot_grid(bird_fish_area, bird_bycatch_area, bird_habcover_area, bird_sitesum_area,ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//bird_area_marine.png", width=20, height=10, unit="cm")

plot_grid(tree_fish_area, tree_bycatch_area, tree_habcover_area, tree_sitesum_area,ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//tree_area_marine.png", width=20, height=10, unit="cm")

plot_grid(insect_herbivore_fish_area, insect_herbivore_bycatch_area, insect_herbivore_habcover_area, insect_herbivore_sitesum_area,ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//insect_herbivore_area_marine.png", width=20, height=10, unit="cm")

plot_grid(insect_birdfood_fish_area, insect_birdfood_bycatch_area, insect_birdfood_habcover_area, insect_birdfood_sitesum_area,ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//insect_birdfood_area_marine.png", width=20, height=10, unit="cm")


plot_grid(insect_detritivore_fish_area, insect_detritivore_bycatch_area, insect_detritivore_habcover_area, insect_detritivore_sitesum_area,ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//insect_detritivore_area_marine.png", width=20, height=10, unit="cm")


plot_grid(insect_carnivore_fish_area, insect_carnivore_bycatch_area, insect_carnivore_habcover_area, insect_carnivore_sitesum_area,ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//insect_carnivore_area_marine.png", width=20, height=10, unit="cm")


plot_grid(plant_fish_area, plant_bycatch_area, plant_habcover_area, plant_sitesum_area,ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//plant_area_marine.png", width=20, height=10, unit="cm")


plot_grid(bird_d15n_area, bird_d34s_area,bird_d13c_feathers_area,bird_d13c_feces_area, ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//bird_area_chemistry.png", width=20, height=10, unit="cm")

plot_grid(tree_d15n_area, tree_d34s_area,tree_d13c_feathers_area,tree_d13c_feces_area,ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//tree_area_chemistry.png", width=20, height=10, unit="cm")

plot_grid(insect_herbivore_d15n_area, insect_herbivore_d34s_area,insect_herbivore_d13c_feathers_area,insect_herbivore_d13c_feces_area,ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//insect_herbivore_area_chemistry.png", width=20, height=10, unit="cm")

plot_grid(plant_d15n_area, plant_d34s_area,plant_d13c_feathers_area,plant_d13c_feces_area,ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//plant_area_chemistry.png", width=20, height=10, unit="cm")


plot_grid(insect_carnivore_d15n_area, insect_carnivore_d34s_area,insect_carnivore_d13c_feathers_area,insect_carnivore_d13c_feces_area,ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//insect_carnivore_area_chemistry.png", width=20, height=10, unit="cm")

plot_grid(insect_detritivore_d15n_area, insect_detritivore_d34s_area,insect_detritivore_d13c_feathers_area,insect_detritivore_d13c_feces_area,ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//insect_detritivore_area_chemistry.png", width=20, height=10, unit="cm")

plot_grid(insect_birdfood_d15n_area, insect_birdfood_d34s_area, insect_birdfood_d13c_feathers_area,insect_birdfood_d13c_feces_area, ncol=2)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//insect_birdfood_area_chemistry.png", width=20, height=10, unit="cm")



#####
colorset_richness_habcover = c("low habitat cover"="black" , "high habitat cover" ="#1baff5" )
colorset_richness_SITE_SUM = c("low beach wrack"="black" , "high beach wrack" ="#1baff5" )
colorset_richness_fish_biomass_bym3 = c("low fish biomass"="black" , "high fish biomass" ="#1baff5" )
colorset_richness_bycatch_biomass_bym3_mean = c("low invert biomass"="black" , "high invert biomass" ="#1baff5" )
colorset_richness_d15n = c("low N15"="black" , "high N15" ="#1baff5" )
colorset_richness_d34s = c("low S34"="black" , "high S34" ="#1baff5" )
colorset_richness_d13c_feathers = c("low d13c feathers"="black" , "high d13c feathers" ="#1baff5" )
colorset_richness_d13c_feces = c("low d13c feces"="black" , "high d13c feces" ="#1baff5" )


##### Bird richness vs area

# Richness vs.  area ------------------------------------------------------

bird_habcover_area<-ggplot(fish_richness_merged_isl %>% filter(! habcover_cat_isl=="NA" & ! habcover_cat_isl=="med habitat cover"), aes( y=log(bird.richness), x=log_Area, col=habcover_cat_isl, fill=habcover_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_habcover)+ scale_fill_manual(values=colorset_richness_habcover)

bird_sitesum_area<-ggplot(fish_richness_merged_isl %>% filter(! SITE_SUM_cat_isl=="NA" & ! SITE_SUM_cat_isl=="no wrack"), aes( y=log(bird.richness), x=log_Area, col=SITE_SUM_cat_isl, fill=SITE_SUM_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_SITE_SUM)+ scale_fill_manual(values=colorset_richness_SITE_SUM)

bird_fish_area<-ggplot(fish_richness_merged_isl %>% filter(! fish_biomass_bym3_cat_isl=="NA" & ! fish_biomass_bym3_cat_isl=="med fish biomass"), aes( y=log(bird.richness), x=log_Area, col=fish_biomass_bym3_cat_isl, fill=fish_biomass_bym3_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_fish_biomass_bym3)+ scale_fill_manual(values=colorset_richness_fish_biomass_bym3)

bird_bycatch_area<-ggplot(fish_richness_merged_isl %>% filter(! bycatch_biomass_bym3_mean_cat_isl=="NA" & ! bycatch_biomass_bym3_mean_cat_isl=="med invert biomass"), aes( y=log(bird.richness), x=log_Area, col=bycatch_biomass_bym3_mean_cat_isl, fill=bycatch_biomass_bym3_mean_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_bycatch_biomass_bym3_mean)+ scale_fill_manual(values=colorset_richness_bycatch_biomass_bym3_mean)

bird_d15n_area<-ggplot(fish_richness_merged_isl %>% filter(! d15n_cat_isl=="NA" & ! d15n_cat_isl=="med N15"), aes( y=log(bird.richness), x=log_Area, col=d15n_cat_isl, fill=d15n_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d15n)+ scale_fill_manual(values=colorset_richness_d15n)

bird_d34s_area<-ggplot(fish_richness_merged_isl %>% filter(! d34s_cat_isl=="NA" & ! d34s_cat_isl=="med S34"), aes( y=log(bird.richness), x=log_Area, col=d34s_cat_isl, fill=d34s_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d34s)+ scale_fill_manual(values=colorset_richness_d34s)

bird_d13c_feathers_area<-ggplot(isotope_master_fish_feathers %>% filter(! d13c_feathers_cat_isl=="NA" & ! d13c_feathers_cat_isl=="med d13c feathers"), aes( y=log(bird.richness), x=log_Area, col=d13c_feathers_cat_isl, fill=d13c_feathers_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feathers)+ scale_fill_manual(values=colorset_richness_d13c_feathers)

bird_d13c_feces_area<-ggplot(isotope_master_fish_feces %>% filter(! d13c_feces_cat_isl=="NA" & ! d13c_feces_cat_isl=="med d13c feces"), aes( y=log(bird.richness), x=log_Area, col=d13c_feces_cat_isl, fill=d13c_feces_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feces)+ scale_fill_manual(values=colorset_richness_d13c_feces)



#### Tree
tree_habcover_area<-ggplot(fish_richness_merged_isl %>% filter(! habcover_cat_isl=="NA" & ! habcover_cat_isl=="med habitat cover"), aes( y=log(tree_richness), x=log_Area, col=habcover_cat_isl, fill=habcover_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_habcover)+ scale_fill_manual(values=colorset_richness_habcover)

tree_sitesum_area<-ggplot(fish_richness_merged_isl %>% filter(! SITE_SUM_cat_isl=="NA" & ! SITE_SUM_cat_isl=="no wrack"), aes( y=log(tree_richness), x=log_Area, col=SITE_SUM_cat_isl, fill=SITE_SUM_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_SITE_SUM)+ scale_fill_manual(values=colorset_richness_SITE_SUM)

tree_fish_area<-ggplot(fish_richness_merged_isl %>% filter(! fish_biomass_bym3_cat_isl=="NA" & ! fish_biomass_bym3_cat_isl=="med fish biomass"), aes( y=log(tree_richness), x=log_Area, col=fish_biomass_bym3_cat_isl, fill=fish_biomass_bym3_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_fish_biomass_bym3)+ scale_fill_manual(values=colorset_richness_fish_biomass_bym3)

tree_bycatch_area<-ggplot(fish_richness_merged_isl %>% filter(! bycatch_biomass_bym3_mean_cat_isl=="NA" & ! bycatch_biomass_bym3_mean_cat_isl=="med invert biomass"), aes( y=log(tree_richness), x=log_Area, col=bycatch_biomass_bym3_mean_cat_isl, fill=bycatch_biomass_bym3_mean_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_bycatch_biomass_bym3_mean)+ scale_fill_manual(values=colorset_richness_bycatch_biomass_bym3_mean)

tree_d15n_area<-ggplot(fish_richness_merged_isl %>% filter(! d15n_cat_isl=="NA" & ! d15n_cat_isl=="med N15"), aes( y=log(tree_richness), x=log_Area, col=d15n_cat_isl, fill=d15n_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d15n)+ scale_fill_manual(values=colorset_richness_d15n)

tree_d34s_area<-ggplot(fish_richness_merged_isl %>% filter(! d34s_cat_isl=="NA" & ! d34s_cat_isl=="med S34"), aes( y=log(tree_richness), x=log_Area, col=d34s_cat_isl, fill=d34s_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d34s)+ scale_fill_manual(values=colorset_richness_d34s)

tree_d13c_feathers_area<-ggplot(isotope_master_fish_feathers %>% filter(! d13c_feathers_cat_isl=="NA" & ! d13c_feathers_cat_isl=="med d13c feathers"), aes( y=log(tree_richness), x=log_Area, col=d13c_feathers_cat_isl, fill=d13c_feathers_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feathers)+ scale_fill_manual(values=colorset_richness_d13c_feathers)

tree_d13c_feces_area<-ggplot(isotope_master_fish_feces %>% filter(! d13c_feces_cat_isl=="NA" & ! d13c_feces_cat_isl=="med d13c feces"), aes( y=log(tree_richness), x=log_Area, col=d13c_feces_cat_isl, fill=d13c_feces_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feces)+ scale_fill_manual(values=colorset_richness_d13c_feces)



#######
#Herbivore
insect_herbivore_habcover_area<-ggplot(fish_richness_merged_isl %>% filter(! habcover_cat_isl=="NA" & ! habcover_cat_isl=="med habitat cover"), aes( y=log(insect_herbivore_richness), x=log_Area, col=habcover_cat_isl, fill=habcover_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_habcover)+ scale_fill_manual(values=colorset_richness_habcover)

insect_herbivore_sitesum_area<-ggplot(fish_richness_merged_isl %>% filter(! SITE_SUM_cat_isl=="NA" & ! SITE_SUM_cat_isl=="no wrack"), aes( y=log(insect_herbivore_richness), x=log_Area, col=SITE_SUM_cat_isl, fill=SITE_SUM_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_SITE_SUM)+ scale_fill_manual(values=colorset_richness_SITE_SUM)

insect_herbivore_fish_area<-ggplot(fish_richness_merged_isl %>% filter(! fish_biomass_bym3_cat_isl=="NA" & ! fish_biomass_bym3_cat_isl=="med fish biomass"), aes( y=log(insect_herbivore_richness), x=log_Area, col=fish_biomass_bym3_cat_isl, fill=fish_biomass_bym3_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_fish_biomass_bym3)+ scale_fill_manual(values=colorset_richness_fish_biomass_bym3)

insect_herbivore_bycatch_area<-ggplot(fish_richness_merged_isl %>% filter(! bycatch_biomass_bym3_mean_cat_isl=="NA" & ! bycatch_biomass_bym3_mean_cat_isl=="med invert biomass"), aes( y=log(insect_herbivore_richness), x=log_Area, col=bycatch_biomass_bym3_mean_cat_isl, fill=bycatch_biomass_bym3_mean_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_bycatch_biomass_bym3_mean)+ scale_fill_manual(values=colorset_richness_bycatch_biomass_bym3_mean)

insect_herbivore_d15n_area<-ggplot(fish_richness_merged_isl %>% filter(! d15n_cat_isl=="NA" & ! d15n_cat_isl=="med N15"), aes( y=log(insect_herbivore_richness), x=log_Area, col=d15n_cat_isl, fill=d15n_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d15n)+ scale_fill_manual(values=colorset_richness_d15n)

insect_herbivore_d34s_area<-ggplot(fish_richness_merged_isl %>% filter(! d34s_cat_isl=="NA" & ! d34s_cat_isl=="med S34"), aes( y=log(insect_herbivore_richness), x=log_Area, col=d34s_cat_isl, fill=d34s_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d34s)+ scale_fill_manual(values=colorset_richness_d34s)

insect_herbivore_d13c_feathers_area<-ggplot(isotope_master_fish_feathers %>% filter(! d13c_feathers_cat_isl=="NA" & ! d13c_feathers_cat_isl=="med d13c feathers"), aes( y=log(insect_herbivore_richness), x=log_Area, col=d13c_feathers_cat_isl, fill=d13c_feathers_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feathers)+ scale_fill_manual(values=colorset_richness_d13c_feathers)

insect_herbivore_d13c_feces_area<-ggplot(isotope_master_fish_feces %>% filter(! d13c_feces_cat_isl=="NA" & ! d13c_feces_cat_isl=="med d13c feces"), aes( y=log(insect_herbivore_richness), x=log_Area, col=d13c_feces_cat_isl, fill=d13c_feces_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feces)+ scale_fill_manual(values=colorset_richness_d13c_feces)



#birdfood
insect_birdfood_habcover_area<-ggplot(fish_richness_merged_isl %>% filter(! habcover_cat_isl=="NA" & ! habcover_cat_isl=="med habitat cover"), aes( y=log(insect_birdfood_richness), x=log_Area, col=habcover_cat_isl, fill=habcover_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_habcover)+ scale_fill_manual(values=colorset_richness_habcover)

insect_birdfood_sitesum_area<-ggplot(fish_richness_merged_isl %>% filter(! SITE_SUM_cat_isl=="NA" & ! SITE_SUM_cat_isl=="no wrack"), aes( y=log(insect_birdfood_richness), x=log_Area, col=SITE_SUM_cat_isl, fill=SITE_SUM_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_SITE_SUM)+ scale_fill_manual(values=colorset_richness_SITE_SUM)

insect_birdfood_fish_area<-ggplot(fish_richness_merged_isl %>% filter(! fish_biomass_bym3_cat_isl=="NA" & ! fish_biomass_bym3_cat_isl=="med fish biomass"), aes( y=log(insect_birdfood_richness), x=log_Area, col=fish_biomass_bym3_cat_isl, fill=fish_biomass_bym3_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_fish_biomass_bym3)+ scale_fill_manual(values=colorset_richness_fish_biomass_bym3)

insect_birdfood_bycatch_area<-ggplot(fish_richness_merged_isl %>% filter(! bycatch_biomass_bym3_mean_cat_isl=="NA" & ! bycatch_biomass_bym3_mean_cat_isl=="med invert biomass"), aes( y=log(insect_birdfood_richness), x=log_Area, col=bycatch_biomass_bym3_mean_cat_isl, fill=bycatch_biomass_bym3_mean_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_bycatch_biomass_bym3_mean)+ scale_fill_manual(values=colorset_richness_bycatch_biomass_bym3_mean)

insect_birdfood_d15n_area<-ggplot(fish_richness_merged_isl %>% filter(! d15n_cat_isl=="NA" & ! d15n_cat_isl=="med N15"), aes( y=log(insect_birdfood_richness), x=log_Area, col=d15n_cat_isl, fill=d15n_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d15n)+ scale_fill_manual(values=colorset_richness_d15n)

insect_birdfood_d34s_area<-ggplot(fish_richness_merged_isl %>% filter(! d34s_cat_isl=="NA" & ! d34s_cat_isl=="med S34"), aes( y=log(insect_birdfood_richness), x=log_Area, col=d34s_cat_isl, fill=d34s_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d34s)+ scale_fill_manual(values=colorset_richness_d34s)

insect_birdfood_d13c_feathers_area<-ggplot(isotope_master_fish_feathers %>% filter(! d13c_feathers_cat_isl=="NA" & ! d13c_feathers_cat_isl=="med d13c feathers"), aes( y=log(insect_birdfood_richness), x=log_Area, col=d13c_feathers_cat_isl, fill=d13c_feathers_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feathers)+ scale_fill_manual(values=colorset_richness_d13c_feathers)

insect_birdfood_d13c_feces_area<-ggplot(isotope_master_fish_feces %>% filter(! d13c_feces_cat_isl=="NA" & ! d13c_feces_cat_isl=="med d13c feces"), aes( y=log(insect_birdfood_richness), x=log_Area, col=d13c_feces_cat_isl, fill=d13c_feces_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feces)+ scale_fill_manual(values=colorset_richness_d13c_feces)



#detritivore
insect_detritivore_habcover_area<-ggplot(fish_richness_merged_isl %>% filter(! habcover_cat_isl=="NA" & ! habcover_cat_isl=="med habitat cover"), aes( y=log(insect_detritivore_richness), x=log_Area, col=habcover_cat_isl, fill=habcover_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_habcover)+ scale_fill_manual(values=colorset_richness_habcover)

insect_detritivore_sitesum_area<-ggplot(fish_richness_merged_isl %>% filter(! SITE_SUM_cat_isl=="NA" & ! SITE_SUM_cat_isl=="no wrack"), aes( y=log(insect_detritivore_richness), x=log_Area, col=SITE_SUM_cat_isl, fill=SITE_SUM_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_SITE_SUM)+ scale_fill_manual(values=colorset_richness_SITE_SUM)

insect_detritivore_fish_area<-ggplot(fish_richness_merged_isl %>% filter(! fish_biomass_bym3_cat_isl=="NA" & ! fish_biomass_bym3_cat_isl=="med fish biomass"), aes( y=log(insect_detritivore_richness), x=log_Area, col=fish_biomass_bym3_cat_isl, fill=fish_biomass_bym3_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_fish_biomass_bym3)+ scale_fill_manual(values=colorset_richness_fish_biomass_bym3)

insect_detritivore_bycatch_area<-ggplot(fish_richness_merged_isl %>% filter(! bycatch_biomass_bym3_mean_cat_isl=="NA" & ! bycatch_biomass_bym3_mean_cat_isl=="med invert biomass"), aes( y=log(insect_detritivore_richness), x=log_Area, col=bycatch_biomass_bym3_mean_cat_isl, fill=bycatch_biomass_bym3_mean_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_bycatch_biomass_bym3_mean)+ scale_fill_manual(values=colorset_richness_bycatch_biomass_bym3_mean)

insect_detritivore_d15n_area<-ggplot(fish_richness_merged_isl %>% filter(! d15n_cat_isl=="NA" & ! d15n_cat_isl=="med N15"), aes( y=log(insect_detritivore_richness), x=log_Area, col=d15n_cat_isl, fill=d15n_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d15n)+ scale_fill_manual(values=colorset_richness_d15n)

insect_detritivore_d34s_area<-ggplot(fish_richness_merged_isl %>% filter(! d34s_cat_isl=="NA" & ! d34s_cat_isl=="med S34"), aes( y=log(insect_detritivore_richness), x=log_Area, col=d34s_cat_isl, fill=d34s_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d34s)+ scale_fill_manual(values=colorset_richness_d34s)

insect_detritivore_d13c_feathers_area<-ggplot(isotope_master_fish_feathers %>% filter(! d13c_feathers_cat_isl=="NA" & ! d13c_feathers_cat_isl=="med d13c feathers"), aes( y=log(insect_detritivore_richness), x=log_Area, col=d13c_feathers_cat_isl, fill=d13c_feathers_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feathers)+ scale_fill_manual(values=colorset_richness_d13c_feathers)

insect_detritivore_d13c_feces_area<-ggplot(isotope_master_fish_feces %>% filter(! d13c_feces_cat_isl=="NA" & ! d13c_feces_cat_isl=="med d13c feces"), aes( y=log(insect_detritivore_richness), x=log_Area, col=d13c_feces_cat_isl, fill=d13c_feces_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feces)+ scale_fill_manual(values=colorset_richness_d13c_feces)




#### carnivore
insect_carnivore_habcover_area<-ggplot(fish_richness_merged_isl %>% filter(! habcover_cat_isl=="NA" & ! habcover_cat_isl=="med habitat cover"), aes( y=log(insect_carnivore_richness), x=log_Area, col=habcover_cat_isl, fill=habcover_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_habcover)+ scale_fill_manual(values=colorset_richness_habcover)

insect_carnivore_sitesum_area<-ggplot(fish_richness_merged_isl %>% filter(! SITE_SUM_cat_isl=="NA" & ! SITE_SUM_cat_isl=="no wrack"), aes( y=log(insect_carnivore_richness), x=log_Area, col=SITE_SUM_cat_isl, fill=SITE_SUM_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_SITE_SUM)+ scale_fill_manual(values=colorset_richness_SITE_SUM)

insect_carnivore_fish_area<-ggplot(fish_richness_merged_isl %>% filter(! fish_biomass_bym3_cat_isl=="NA" & ! fish_biomass_bym3_cat_isl=="med fish biomass"), aes( y=log(insect_carnivore_richness), x=log_Area, col=fish_biomass_bym3_cat_isl, fill=fish_biomass_bym3_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_fish_biomass_bym3)+ scale_fill_manual(values=colorset_richness_fish_biomass_bym3)

insect_carnivore_bycatch_area<-ggplot(fish_richness_merged_isl %>% filter(! bycatch_biomass_bym3_mean_cat_isl=="NA" & ! bycatch_biomass_bym3_mean_cat_isl=="med invert biomass"), aes( y=log(insect_carnivore_richness), x=log_Area, col=bycatch_biomass_bym3_mean_cat_isl, fill=bycatch_biomass_bym3_mean_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_bycatch_biomass_bym3_mean)+ scale_fill_manual(values=colorset_richness_bycatch_biomass_bym3_mean)

insect_carnivore_d15n_area<-ggplot(fish_richness_merged_isl %>% filter(! d15n_cat_isl=="NA" & ! d15n_cat_isl=="med N15"), aes( y=log(insect_carnivore_richness), x=log_Area, col=d15n_cat_isl, fill=d15n_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d15n)+ scale_fill_manual(values=colorset_richness_d15n)

insect_carnivore_d34s_area<-ggplot(fish_richness_merged_isl %>% filter(! d34s_cat_isl=="NA" & ! d34s_cat_isl=="med S34"), aes( y=log(insect_carnivore_richness), x=log_Area, col=d34s_cat_isl, fill=d34s_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d34s)+ scale_fill_manual(values=colorset_richness_d34s)

insect_carnivore_d13c_feathers_area<-ggplot(isotope_master_fish_feathers %>% filter(! d13c_feathers_cat_isl=="NA" & ! d13c_feathers_cat_isl=="med d13c feathers"), aes( y=log(insect_carnivore_richness), x=log_Area, col=d13c_feathers_cat_isl, fill=d13c_feathers_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feathers)+ scale_fill_manual(values=colorset_richness_d13c_feathers)

insect_carnivore_d13c_feces_area<-ggplot(isotope_master_fish_feces %>% filter(! d13c_feces_cat_isl=="NA" & ! d13c_feces_cat_isl=="med d13c feces"), aes( y=log(insect_carnivore_richness), x=log_Area, col=d13c_feces_cat_isl, fill=d13c_feces_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feces)+ scale_fill_manual(values=colorset_richness_d13c_feces)

#### plant
plant_habcover_area<-ggplot(fish_richness_merged_isl %>% filter(! habcover_cat_isl=="NA" & ! habcover_cat_isl=="med habitat cover"), aes( y=log(plant_richness), x=log_Area, col=habcover_cat_isl, fill=habcover_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_habcover)+ scale_fill_manual(values=colorset_richness_habcover)

plant_sitesum_area<-ggplot(fish_richness_merged_isl %>% filter(! SITE_SUM_cat_isl=="NA" & ! SITE_SUM_cat_isl=="no wrack"), aes( y=log(plant_richness), x=log_Area, col=SITE_SUM_cat_isl, fill=SITE_SUM_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_SITE_SUM)+ scale_fill_manual(values=colorset_richness_SITE_SUM)

plant_fish_area<-ggplot(fish_richness_merged_isl %>% filter(! fish_biomass_bym3_cat_isl=="NA" & ! fish_biomass_bym3_cat_isl=="med fish biomass"), aes( y=log(plant_richness), x=log_Area, col=fish_biomass_bym3_cat_isl, fill=fish_biomass_bym3_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_fish_biomass_bym3)+ scale_fill_manual(values=colorset_richness_fish_biomass_bym3)

plant_bycatch_area<-ggplot(fish_richness_merged_isl %>% filter(! bycatch_biomass_bym3_mean_cat_isl=="NA" & ! bycatch_biomass_bym3_mean_cat_isl=="med invert biomass"), aes( y=log(plant_richness), x=log_Area, col=bycatch_biomass_bym3_mean_cat_isl, fill=bycatch_biomass_bym3_mean_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_bycatch_biomass_bym3_mean)+ scale_fill_manual(values=colorset_richness_bycatch_biomass_bym3_mean)

plant_d15n_area<-ggplot(fish_richness_merged_isl %>% filter(! d15n_cat_isl=="NA" & ! d15n_cat_isl=="med N15"), aes( y=log(plant_richness), x=log_Area, col=d15n_cat_isl, fill=d15n_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d15n)+ scale_fill_manual(values=colorset_richness_d15n)

plant_d34s_area<-ggplot(fish_richness_merged_isl %>% filter(! d34s_cat_isl=="NA" & ! d34s_cat_isl=="med S34"), aes( y=log(plant_richness), x=log_Area, col=d34s_cat_isl, fill=d34s_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d34s)+ scale_fill_manual(values=colorset_richness_d34s)

plant_d13c_feathers_area<-ggplot(isotope_master_fish_feathers %>% filter(! d13c_feathers_cat_isl=="NA" & ! d13c_feathers_cat_isl=="med d13c feathers"), aes( y=log(plant_richness), x=log_Area, col=d13c_feathers_cat_isl, fill=d13c_feathers_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feathers)+ scale_fill_manual(values=colorset_richness_d13c_feathers)

plant_d13c_feces_area<-ggplot(isotope_master_fish_feces %>% filter(! d13c_feces_cat_isl=="NA" & ! d13c_feces_cat_isl=="med d13c feces"), aes( y=log(plant_richness), x=log_Area, col=d13c_feces_cat_isl, fill=d13c_feces_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+ theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
  scale_colour_manual(values=colorset_richness_d13c_feces)+ scale_fill_manual(values=colorset_richness_d13c_feces)


ggplot(fish_richness_merged_isl %>% filter(! fish_biomass_bym3_cat_isl=="NA" & ! fish_biomass_bym3_cat_isl=="med fish biomass"), aes( y=log(insect_richness), x=log_Area, col=fish_biomass_bym3_cat_isl, fill=fish_biomass_bym3_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()

ggplot(fish_richness_merged_isl %>% filter(! schooling_fish_biomass_bym3_mean_cat_isl=="NA" & ! schooling_fish_biomass_bym3_mean_cat_isl=="med schooling fish biomass"), aes( y=log(insect_richness), x=log_Area, col=schooling_fish_biomass_bym3_mean_cat_isl, fill=schooling_fish_biomass_bym3_mean_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()


ggplot(fish_richness_merged_isl %>% filter(! individual_fish_biomass_bym3_mean_cat_isl=="NA" & ! individual_fish_biomass_bym3_mean_cat_isl=="med individual fish biomass"), aes( y=log(insect_richness), x=log_Area, col=individual_fish_biomass_bym3_mean_cat_isl, fill=individual_fish_biomass_bym3_mean_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()


# Aggregating plots by d13C -----------------------------------------------


#feather plots

bird_feather.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feathers"), aes( y=bird.density, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird feathers")

NDVI_feather.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feathers"), aes( y=NDVI_mean, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird feathers")

plant_feather.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feathers"), aes( y=total_cover, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird feathers")

tree_feather.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feathers"), aes( y=sum_basal, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird feathers")

det_feather.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feathers"), aes( y=log(insect_detritivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird feathers")

herb_feather.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feathers"), aes( y=log(insect_herbivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird feathers")

carn_feather.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feathers"), aes( y=log(insect_carnivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird feathers")

bf_feather.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feathers"), aes( y=log(insect_birdfood_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird feathers")


######## detritivore plots
bird_insects_ISO.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=bird.density, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")

NDVI_insects_ISO.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=NDVI_mean, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")

plant_insects_ISO.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=total_cover, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")

tree_insects_ISO.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=sum_basal, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")

det_insects_ISO.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=log(insect_detritivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")

herb_insects_ISO.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=log(insect_herbivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")

carn_insects_ISO.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=log(insect_carnivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")

bf_insects_ISO.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=log(insect_birdfood_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")

# carn plots
bird_insects_COL.plot.C<- ggplot(isotope_master %>% filter(group=="insects_COL"), aes( y=bird.density, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in carnivorous insects")

NDVI_insects_COL.plot.C<- ggplot(isotope_master %>% filter(group=="insects_COL"), aes( y=NDVI_mean, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in carnivorous insects")

plant_insects_COL.plot.C<- ggplot(isotope_master %>% filter(group=="insects_COL"), aes( y=total_cover, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in carnivorous insects")

tree_insects_COL.plot.C<- ggplot(isotope_master %>% filter(group=="insects_COL"), aes( y=sum_basal, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in carnivorous insects")

det_insects_COL.plot.C<- ggplot(isotope_master %>% filter(group=="insects_COL"), aes( y=log(insect_detritivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in carnivorous insects")

herb_insects_COL.plot.C<- ggplot(isotope_master %>% filter(group=="insects_COL"), aes( y=log(insect_herbivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in carnivorous insects")

carn_insects_COL.plot.C<- ggplot(isotope_master %>% filter(group=="insects_COL"), aes( y=log(insect_carnivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in carnivorous insects")

bf_insects_COL.plot.C<- ggplot(isotope_master %>% filter(group=="insects_COL"), aes( y=log(insect_birdfood_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in carnivorous insects")


## Mouse feces
bird_Mouse_feces.plot.C<- ggplot(isotope_master %>% filter(group=="Mouse feces"), aes( y=bird.density, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in Mouse feces")

NDVI_Mouse_feces.plot.C<- ggplot(isotope_master %>% filter(group=="Mouse feces"), aes( y=NDVI_mean, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in Mouse feces")

plant_Mouse_feces.plot.C<- ggplot(isotope_master %>% filter(group=="Mouse feces"), aes( y=total_cover, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in Mouse feces")

tree_Mouse_feces.plot.C<- ggplot(isotope_master %>% filter(group=="Mouse feces"), aes( y=sum_basal, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in Mouse feces")

det_Mouse_feces.plot.C<- ggplot(isotope_master %>% filter(group=="Mouse feces"), aes( y=log(insect_detritivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in Mouse feces")

herb_Mouse_feces.plot.C<- ggplot(isotope_master %>% filter(group=="Mouse feces"), aes( y=log(insect_herbivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in Mouse feces")

carn_Mouse_feces.plot.C<- ggplot(isotope_master %>% filter(group=="Mouse feces"), aes( y=log(insect_carnivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in Mouse feces")

bf_Mouse_feces.plot.C<- ggplot(isotope_master %>% filter(group=="Mouse feces"), aes( y=log(insect_birdfood_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in Mouse feces")

### bird feces
bird_bird_feces.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feces"), aes( y=bird.density, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird_feces")

NDVI_bird_feces.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feces"), aes( y=NDVI_mean, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird_feces")

plant_bird_feces.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feces"), aes( y=total_cover, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird_feces")

tree_bird_feces.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feces"), aes( y=sum_basal, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird_feces")

det_bird_feces.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feces"), aes( y=log(insect_detritivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird_feces")

herb_bird_feces.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feces"), aes( y=log(insect_herbivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird_feces")

carn_bird_feces.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feces"), aes( y=log(insect_carnivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird_feces")

bf_bird_feces.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feces"), aes( y=log(insect_birdfood_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird_feces")


# Bird productivity -------------------------------------------------------


#bird prodcutivity
#View(fish_richness_merged_isl)

bird_fish_biomass<-ggplot(fish_richness_merged_isl, aes( y=bird.density, x=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+xlab("fish_biomass")

ggplot(fish_richness_merged_isl, aes( y=bird.richness, x=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_biomass")


bird_bycatch_biomass<-ggplot(fish_richness_merged_isl, aes( y=bird.density, x=bycatch_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("bycatch_biomass")

bird_fish_bycatch_biomass<-ggplot(fish_richness_merged_isl, aes( y=bird.density, x=fish_bycatch_biomass))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_bycatch_biomass")


bird_fish_biomass_schooling<-ggplot(fish_richness_merged_isl, aes( y=bird.density, x=schooling_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("schooling_fish_biomass")

bird_fish_biomass_individual<-ggplot(fish_richness_merged_isl, aes( y=bird.density, x=individual_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("individual_fish_biomass")


bird_habitat_cover<-ggplot(fish_richness_merged_isl, aes( y=bird.density, x=log(HAB2000)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("nearby marine habitat")

bird_wrack_shore<-ggplot(fish_richness_merged_isl, aes( y=bird.density, x=log(SITE_SUM+1)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("wrack on shore")

plot_grid(bird_fish_biomass, bird_bycatch_biomass, bird_habitat_cover, bird_wrack_shore, ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//bird.density.marine.biomass.predictors.png", width=40, height=10, unit="cm")




fish_richness<-ggplot(fish_richness_merged_isl, aes( y=bird.density, x=fish_richness_corrected))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_richness")

wrack_richness<-ggplot(fish_richness_merged_isl, aes( y=bird.density, x=wrack_richness))+
                         geom_point(size=2)+geom_smooth() +theme_classic()+xlab("wrack richness")
  
bycatch_richness<-ggplot(fish_richness_merged_isl, aes( y=bird.density, x=bycatch_richness_corrected))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("bycatch_richness")
  
plot_grid(fish_richness, bycatch_richness, wrack_richness, ncol=3)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//bird.density.marine.richness.predictors.png", width=40, height=10, unit="cm")





feather.plot.N<- ggplot(isotope_master %>% filter(group=="bird_feathers"), aes( y=bird.density, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in bird feathers")

feather.plot.C<- ggplot(isotope_master %>% filter(group=="bird_feathers"), aes( y=bird.density, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in bird feathers")

soil.S_bird<-ggplot(fish_richness_merged_isl, aes( y=bird.density, x=d34s))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s in soil")

soil.N_bird<-ggplot(fish_richness_merged_isl, aes( y=bird.density, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15n in soil")

plot_grid(soil.N_bird, soil.S_bird, feather.plot.C, feather.plot.N, ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//bird.density.marine.chemistry.predictors.png", width=40, height=10, unit="cm")

ggplot(fish_richness_merged_isl %>% filter(! bird.density_cat_isl =="NA" & ! bird.density_cat_isl =="med bird density"), aes( y=d15n, x=n, col=bird.density_cat_isl, fill=bird.density_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()


ggplot(fish_richness_merged_isl %>% filter(! fish_biomass_bym3_cat_isl=="NA" & ! fish_biomass_bym3_cat_isl=="med fish biomass"), aes( y=log(bird.richness), x=log_Area, col=fish_biomass_bym3_cat_isl, fill=fish_biomass_bym3_cat_isl))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()


View(fish_richness_merged_isl)
# NDVI Prod ---------------------------------------------------------------


#NDVI productivity
herbivores.plot.N<- ggplot(isotope_master %>% filter(group=="insects_CUR"), aes( y=NDVI_mean, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in herbivores")

isopods.plot.N<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=NDVI_mean, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in isopods")

isopods.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=NDVI_mean, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")

gash.plot.N<- ggplot(isotope_master %>% filter(group=="gash"), aes( y=NDVI_mean, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in salal")

soil.N_NDVI<-ggplot(fish_richness_merged_isl, aes( y=NDVI_mean, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15n in soil")

soil.S_NDVI<-ggplot(fish_richness_merged_isl, aes( y=NDVI_mean, x=d34s))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s in soil")

plot_grid(soil.N_NDVI, soil.S_NDVI, gash.plot.N, herbivores.plot.N, isopods.plot.N, isopods.plot.C, ncol=3)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//NDVI.marine.chemistry.predictors.png", width=30, height=20, unit="cm")


fish_biomass_NDVI<-ggplot(fish_richness_merged_isl, aes( y=NDVI_mean, x=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_biomass")

bycatch_biomass_NDVI<-ggplot(fish_richness_merged_isl, aes( y=NDVI_mean, x=bycatch_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("bycatch_biomass")

fish_bycatch_biomass_NDVI<-ggplot(fish_richness_merged_isl, aes( y=NDVI_mean, x=fish_bycatch_biomass))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_bycatch_biomass")

fish_biomass_schooling_NDVI<-ggplot(fish_richness_merged_isl, aes( y=NDVI_mean, x=schooling_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("schooling_fish_biomass")

fish_biomass_individual_NDVI<-ggplot(fish_richness_merged_isl, aes( y=NDVI_mean, x=individual_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("individual_fish_biomass")

NDVI_habitat_cover<-ggplot(fish_richness_merged_isl, aes( y=NDVI_mean, x=log(HAB2000)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("nearby marine habitat")

NDVI_wrack_shore<-ggplot(fish_richness_merged_isl, aes( y=NDVI_mean, x=log(SITE_SUM+1)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("wrack on shore")

plot_grid(fish_biomass_NDVI, bycatch_biomass_NDVI, NDVI_habitat_cover, NDVI_wrack_shore, ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//NDVI.marine.biomass.predictors.png", width=40, height=10, unit="cm")


# Plant productivity ------------------------------------------------------


#plant cover
plant_herbivores.plot.N<- ggplot(isotope_master %>% filter(group=="insects_CUR"), aes( y=total_cover, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in herbivores")

plant_isopods.plot.N<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=total_cover, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in isopods")

plant_isopods.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=total_cover, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")

plant_gash.plot.N<- ggplot(isotope_master %>% filter(group=="gash"), aes( y=total_cover, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in salal")

soil.N_plant<-ggplot(fish_richness_merged_isl, aes( y=total_cover, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15n in soil")

soil.S_plant<-ggplot(fish_richness_merged_isl, aes( y=total_cover, x=d34s))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s in soil")

plot_grid(soil.N_plant, soil.S_plant, plant_gash.plot.N, plant_herbivores.plot.N, plant_isopods.plot.N, plant_isopods.plot.C, col=3)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//Plant_cover.marine.chemistry.predictors.png", width=30, height=20, unit="cm")


fish_bycatch_biomass_plant<-ggplot(fish_richness_merged_isl, aes( y=total_cover, x=fish_bycatch_biomass))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_bycatch_biomass")


bycatch_biomass_plant<-ggplot(fish_richness_merged_isl, aes( y=total_cover, x=bycatch_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("bycatch_biomass")+ylim(0,200)

fish_biomass_plant<-ggplot(fish_richness_merged_isl, aes( y=total_cover, x=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_biomass")+ylim(0,200)


fish_biomass_schooling_plant<-ggplot(fish_richness_merged_isl, aes( y=total_cover, x=schooling_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("schooling_fish_biomass")


fish_biomass_individual_plant<-ggplot(fish_richness_merged_isl, aes( y=total_cover, x=individual_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("individual_fish_biomass")

plant_habitat_cover<-ggplot(fish_richness_merged_isl, aes( y=total_cover, x=log(HAB2000)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("nearby marine habitat")+ylim(0,200)

plant_wrack_shore<-ggplot(fish_richness_merged_isl, aes( y=total_cover, x=log(SITE_SUM+1)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("wrack on shore")+ylim(0,200)

plot_grid(fish_biomass_plant,bycatch_biomass_plant, plant_habitat_cover, plant_wrack_shore, ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//Plant_cover.marine.biomass.predictors.png", width=40, height=10, unit="cm")



# Tree prod ---------------------------------------------------------------


#tree sum basal
tree_herbivores.plot.N<- ggplot(isotope_master %>% filter(group=="insects_CUR"), aes( y=sum_basal, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in herbivores")

tree_isopods.plot.N<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=sum_basal, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in isopods")

tree_isopods.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=sum_basal, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")


tree_gash.plot.N<- ggplot(isotope_master %>% filter(group=="gash"), aes( y=sum_basal, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in salal")

soil.N_tree<-ggplot(fish_richness_merged_isl, aes( y=sum_basal, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15n in soil")

soil.S_tree<-ggplot(fish_richness_merged_isl, aes( y=sum_basal, x=d34s))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s in soil")

plot_grid(soil.N_tree, soil.S_tree, tree_gash.plot.N, tree_herbivores.plot.N, tree_isopods.plot.N, tree_isopods.plot.C, col=3)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//sum_basal.marine.chemistry.predictors.png", width=30, height=20, unit="cm")



bycatch_biomass_tree<-ggplot(fish_richness_merged_isl, aes( y=sum_basal, x=bycatch_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("bycatch_biomass")

fish_biomass_tree<-ggplot(fish_richness_merged_isl, aes( y=sum_basal, x=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_biomass")

fish_biomass_schooling_tree<-ggplot(fish_richness_merged_isl, aes( y=sum_basal, x=schooling_fish_biomass_bym3_mean))+
  #geom_errorbarh(aes(xmin=schooling_fish_biomass_bym3_mean-schooling_fish_biomass_bym3_sd, xmax=schooling_fish_biomass_bym3_mean+schooling_fish_biomass_bym3_sd)) +
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("schooling_fish_biomass")

fish_biomass_individual_tree<-ggplot(fish_richness_merged_isl, aes( y=sum_basal, x=individual_fish_biomass_bym3_mean))+
  #geom_errorbarh(aes(xmin=individual_fish_biomass_bym3_mean-individual_fish_biomass_bym3_sd, xmax=individual_fish_biomass_bym3_mean+individual_fish_biomass_bym3_sd)) +
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("individual_fish_biomass")

tree_habitat_cover<-ggplot(fish_richness_merged_isl, aes( y=sum_basal, x=log(HAB2000)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("nearby marine habitat")

tree_wrack_shore<-ggplot(fish_richness_merged_isl, aes( y=sum_basal, x=log(SITE_SUM+1)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("wrack on shore")

plot_grid(fish_biomass_tree, bycatch_biomass_tree, tree_habitat_cover, tree_wrack_shore, ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//sum_basal.marine.biomass.predictors.png", width=40, height=10, unit="cm")



# Herbivore productivity --------------------------------------------------


#herbivore abundance

herb_herbivores.plot.N<- ggplot(isotope_master %>% filter(group=="insects_CUR"), aes( y=insect_herbivore_pitfall_av_abundance, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in herbivores")

herb_isopods.plot.N<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=insect_herbivore_pitfall_av_abundance, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in isopods")

herb_isopods.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=insect_herbivore_pitfall_av_abundance, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")


herb_gash.plot.N<- ggplot(isotope_master %>% filter(group=="gash"), aes( y=insect_herbivore_pitfall_av_abundance, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in salal")

soil.N_herb<-ggplot(fish_richness_merged_isl, aes( y=insect_herbivore_pitfall_av_abundance, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15n in soil")

soil.S_herb<-ggplot(fish_richness_merged_isl, aes( y=insect_herbivore_pitfall_av_abundance, x=d34s))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s in soil")

plot_grid(soil.N_herb, soil.S_herb, herb_gash.plot.N, herb_herbivores.plot.N, herb_isopods.plot.N, herb_isopods.plot.C, col=3)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//herbivore_abundance.marine.chemistry.predictors.png", width=30, height=20, unit="cm")


bycatch_biomass_herb<-ggplot(fish_richness_merged_isl, aes( y=insect_herbivore_pitfall_av_abundance, x=bycatch_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("bycatch_biomass")

fish_biomass_herb<-ggplot(fish_richness_merged_isl, aes( y=insect_herbivore_pitfall_av_abundance, x=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_biomass")

fish_biomass_schooling_herb<-ggplot(fish_richness_merged_isl, aes( y=insect_herbivore_pitfall_av_abundance, x=schooling_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("schooling_fish_biomass")

fish_biomass_individual_herb<-ggplot(fish_richness_merged_isl, aes( y=insect_herbivore_pitfall_av_abundance, x=individual_fish_biomass_bym3_mean))+
  #geom_errorbarh(aes(xmin=individual_fish_biomass_bym3_mean-individual_fish_biomass_bym3_sd, xmax=individual_fish_biomass_bym3_mean+individual_fish_biomass_bym3_sd)) +
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("individual_fish_biomass")

habitat_cover_herb<-ggplot(fish_richness_merged_isl, aes( y=insect_herbivore_pitfall_av_abundance, x=log(HAB2000)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("nearby marine habitat")

wrack_shore_herb<-ggplot(fish_richness_merged_isl, aes( y=insect_herbivore_pitfall_av_abundance, x=log(SITE_SUM+1)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("wrack on shore")

plot_grid(fish_biomass_herb, bycatch_biomass_herb, habitat_cover_herb, wrack_shore_herb, ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//herbivore_abundance.marine.biomass.predictors.png", width=40, height=10, unit="cm")


# Carnivore prod ----------------------------------------------------------


#carnivore

carn_carnivores.plot.C<- ggplot(isotope_master %>% filter(group=="insects_COL"), aes( y=insect_carnivore_pitfall_av_abundance, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13c in carnivores")

carn_isopods.plot.N<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=insect_carnivore_pitfall_av_abundance, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in isopods")

carn_isopods.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=insect_carnivore_pitfall_av_abundance, x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")


carn_gash.plot.N<- ggplot(isotope_master %>% filter(group=="gash"), aes( y=insect_carnivore_pitfall_av_abundance, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in salal")

soil.N_carn<-ggplot(fish_richness_merged_isl, aes( y=insect_carnivore_pitfall_av_abundance, x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15n in soil")

soil.S_carn<-ggplot(fish_richness_merged_isl, aes( y=insect_carnivore_pitfall_av_abundance, x=d34s))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s in soil")

plot_grid(soil.N_carn, soil.S_carn, carn_gash.plot.N, carn_carnivores.plot.C, carn_isopods.plot.N, carn_isopods.plot.C, col=3)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//carnivore_abundance.marine.chemistry.predictors.png", width=30, height=20, unit="cm")


bycatch_biomass_carn<-ggplot(fish_richness_merged_isl, aes( y=insect_carnivore_pitfall_av_abundance, x=bycatch_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("bycatch_biomass")

fish_bycatch_biomass_carn<-ggplot(fish_richness_merged_isl, aes( y=insect_carnivore_pitfall_av_abundance, x=fish_bycatch_biomass))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_bycatch_biomass")


fish_biomass_carn<-ggplot(fish_richness_merged_isl, aes( y=insect_carnivore_pitfall_av_abundance, x=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_biomass")

fish_biomass_schooling_carn<-ggplot(fish_richness_merged_isl, aes( y=insect_carnivore_pitfall_av_abundance, x=schooling_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("schooling_fish_biomass")

fish_biomass_individual_carn<-ggplot(fish_richness_merged_isl, aes( y=insect_carnivore_pitfall_av_abundance, x=individual_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("individual_fish_biomass")

habitat_cover<-ggplot(fish_richness_merged_isl, aes( y=insect_carnivore_pitfall_av_abundance, x=log(HAB2000)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("nearby marine habitat")

wrack_shore<-ggplot(fish_richness_merged_isl, aes( y=insect_carnivore_pitfall_av_abundance, x=log(SITE_SUM+1)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("wrack on shore")

plot_grid(fish_biomass_carn, bycatch_biomass_carn, habitat_cover, wrack_shore, ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//carnivore_abundance.marine.biomass.predictors.png", width=40, height=10, unit="cm")


# Detritivore prod --------------------------------------------------------

det_detritivores.plot.C<- ggplot(isotope_master %>% filter(group=="insects_COL"), aes( y=log(insect_detritivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13c in detritivores")

det_isopods.plot.N<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=log(insect_detritivore_pitfall_av_abundance), x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in isopods")

det_isopods.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=log(insect_detritivore_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")


det_gash.plot.N<- ggplot(isotope_master %>% filter(group=="gash"), aes( y=log(insect_detritivore_pitfall_av_abundance), x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in salal")

soil.N_det<-ggplot(fish_richness_merged_isl, aes( y=log(insect_detritivore_pitfall_av_abundance), x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15n in soil")

soil.S_det<-ggplot(fish_richness_merged_isl, aes( y=log(insect_detritivore_pitfall_av_abundance), x=d34s))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s in soil")

plot_grid(soil.N_det, soil.S_det, det_gash.plot.N, det_detritivores.plot.C, det_isopods.plot.N, det_isopods.plot.C, col=3)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//detritivore_abundance.marine.chemistry.predictors.png", width=30, height=20, unit="cm")


bycatch_biomass_det<-ggplot(fish_richness_merged_isl, aes( y=log(insect_detritivore_pitfall_av_abundance), x=bycatch_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("bycatch_biomass")

fish_biomass_det<-ggplot(fish_richness_merged_isl, aes( y=log(insect_detritivore_pitfall_av_abundance), x=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_biomass")

fish_biomass_schooling_det<-ggplot(fish_richness_merged_isl, aes( y=log(insect_detritivore_pitfall_av_abundance), x=schooling_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("schooling_fish_biomass")

fish_biomass_individual_det<-ggplot(fish_richness_merged_isl, aes( y=log(insect_detritivore_pitfall_av_abundance), x=individual_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("individual_fish_biomass")

habitat_cover<-ggplot(fish_richness_merged_isl, aes( y=log(insect_detritivore_pitfall_av_abundance), x=log(HAB2000)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("nearby marine habitat")

wrack_shore<-ggplot(fish_richness_merged_isl, aes( y=log(insect_detritivore_pitfall_av_abundance), x=log(SITE_SUM+1)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("wrack on shore")

plot_grid(fish_biomass_det, bycatch_biomass_det, habitat_cover, wrack_shore, ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//detritivore_abundance.marine.biomass.predictors.png", width=40, height=10, unit="cm")



# Birdfood prod -----------------------------------------------------------



#bird food cover
birdfood_birdfoods.plot.C<- ggplot(isotope_master %>% filter(group=="insects_COL"), aes( y=log(insect_birdfood_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in carnivores")

birdfood_isopods.plot.N<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=log(insect_birdfood_pitfall_av_abundance), x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15N in isopods")

birdfood_isopods.plot.C<- ggplot(isotope_master %>% filter(group=="insects_ISO"), aes( y=log(insect_birdfood_pitfall_av_abundance), x=d13c))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13C in isopods")

soil.N_birdfood<-ggplot(fish_richness_merged_isl, aes( y=log(insect_birdfood_pitfall_av_abundance), x=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15n in soil")

soil.S_birdfood<-ggplot(fish_richness_merged_isl, aes( y=log(insect_birdfood_pitfall_av_abundance), x=d34s))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s in soil")

plot_grid(soil.N_birdfood, soil.S_birdfood, birdfood_birdfoods.plot.C, birdfood_isopods.plot.N, birdfood_isopods.plot.C, col=3)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//birdfood_abundance.marine.chemistry.predictors.png", width=30, height=20, unit="cm")





#birdfood
bycatch_biomass_bf<-ggplot(fish_richness_merged_isl, aes( y=log(insect_birdfood_pitfall_av_abundance), x=bycatch_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("bycatch_biomass")


fish_biomass_bf<-ggplot(fish_richness_merged_isl, aes( y=log(insect_birdfood_pitfall_av_abundance), x=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("fish_biomass")


fish_biomass_schooling_bf<-ggplot(fish_richness_merged_isl, aes( y=log(insect_birdfood_pitfall_av_abundance), x=schooling_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("schooling_fish_biomass")

fish_biomass_individual_bf<-ggplot(fish_richness_merged_isl, aes( y=log(insect_birdfood_pitfall_av_abundance), x=individual_fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("individual_fish_biomass")


habitat_cover<-ggplot(fish_richness_merged_isl, aes( y=log(insect_birdfood_pitfall_av_abundance), x=log(HAB2000)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("nearby marine habitat")

wrack_shore<-ggplot(fish_richness_merged_isl, aes( y=log(insect_birdfood_pitfall_av_abundance), x=log(SITE_SUM+1)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("wrack on shore")

plot_grid(fish_biomass_bf, bycatch_biomass_bf, habitat_cover, wrack_shore, ncol=4)
ggsave("C:Biodiversity idea//Plots//Subsidy Productivity//birdfood_abundance.marine.biomass.predictors.png", width=40, height=10, unit="cm")




# Relating marine variables to each other ---------------------------------


#### relating marine variables

ggplot(fish_richness_merged_isl, aes(d34s, y=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s soil")

ggplot(fish_richness_merged_isl, aes(y=d34s, x=log(Area)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("Area")


ggplot(master_transect, aes(d15n, y=fish_biomass_bym3_mean))+
         geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15n soil")


ggplot(master_transect, aes(y=d15n, x=distance_to_midden))+ geom_point()+geom_smooth(method="lm")
ggplot(master_transect, aes(y=d34s, x=distance_to_midden))+ geom_point()+geom_smooth(method="lm")

ggplot(master_transect, aes(y=cult_imp_plant_richness, x=distance_to_any_arch))+ geom_point()+geom_smooth()
ggplot(master_transect, aes(y=d15n, x=log(distance_to_midden)))+ geom_point()+geom_smooth()
ggplot(master_transect, aes(y=d15n, x=log(distance_to_any_arch)))+ geom_point()+geom_smooth()



ggplot(master_transect, aes(y=plot_cc, x=midden_feature))+ geom_boxplot()



ggplot(fish_richness_merged_isl, aes(d34s, y=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s soil")


ggplot(fish_richness_merged_isl, aes(d34s, y=log(HAB2000)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s soil")

ggplot(fish_richness_merged_isl, aes(d34s, y=eelgrass_cover_2km))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s soil")

ggplot(fish_richness_merged_isl, aes(d34s, y=eelgrass_cover_2km))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d34s soil")


ggplot(isotope_master_fish, aes( x=Area, y=d13c.x, col=group))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()

ggplot(isotope_master_fish %>% filter(group=="insects_ISO"), aes( x=d13c.x, y=d34s.y))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+xlab("d13c iso")


ggplot(isotope_master_fish %>% filter(group=="bird_feathers"), aes( y=d13c.x, x=insect_birdfood_beat_av_abundance))+
  geom_point(size=2)+geom_smooth() +theme_classic()

ggplot(isotope_master_fish %>% filter(group=="bird_feces"), aes( x=d13c.x, y=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+xlab("d13c bird feces")


ggplot(isotope_master_fish %>% filter(group=="insects_ISO"), aes( x=d13c.x, y=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d13c isopod")

ggplot(isotope_master_fish %>% filter(group=="insects_COL"), aes( x=d13c.x, y=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+xlab("d13c carnivores")


ggplot(isotope_master_fish %>% filter(group=="Mouse hair"), aes( x=d13c.x, y=fish_biomass_bym3_mean))+
  geom_point(size=2)+geom_smooth(method="lm") +theme_classic()+xlab("d13c Mouse feces")




ggplot(isotope_master_fish, aes( y=bird.density, x=insect_birdfood_beat_av_abundance))+
  geom_point(size=2)+geom_smooth() +theme_classic()

ggplot(fish_richness_merged_isl, aes(y=NDVI_mean, x=total_cover))+geom_point()+geom_smooth()







ggplot(fish_richness_merged_isl, aes(col=fish_biomass_bym3_cat_isl, fill=fish_biomass_bym3_cat_isl, y=log(bird.richness), x=log(Area)))+
  geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_isl %>% filter(habcover_cat_isl!="med habitat cover"), aes(col=habcover_cat_isl, fill=habcover_cat_isl, y=log(bird.richness), x=log(Area)))+
  geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_isl %>% filter(d15n_cat_isl!="med N15"), aes(col=d15n_cat_isl, fill=d15n_cat_isl, y=log(bird.richness), x=log(Area)))+
  geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_isl %>% filter(d34s_cat_isl!="med S34"), aes(col=d34s_cat_isl, fill=d34s_cat_isl, y=log(bird.richness), x=log(Area)))+
  geom_point()+geom_smooth(method="lm")


ggplot(fish_richness_merged_isl %>% filter(fish.richness_cat_isl!="med fish.richness"), aes(col=fish.richness_cat_isl, fill=fish.richness_cat_isl, y=log(bird.richness), x=log(Area)))+
  geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_isl  %>% filter(wrack.richness_cat_isl!="NA"), aes(col=wrack.richness_cat_isl, fill=wrack.richness_cat_isl, y=log(bird.richness), x=log(Area)))+
  geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_isl  %>% filter(SITE_SUM_cat_isl!="NA"), aes(col=SITE_SUM_cat_isl, fill=SITE_SUM_cat_isl, y=log(bird.richness), x=log(Area)))+
  geom_point()+geom_smooth(method="lm")






# Histograms --------------------------------------------------------------

ggplot(fish_richness_merged_isl, aes(fish_biomass_bym3_mean))+ geom_density(alpha=.7)
ggplot(fish_richness_merged_isl, aes(individual_fish_biomass_bym3_mean))+ geom_density(alpha=.7)

ggplot(fish_richness_merged_isl, aes(bycatch_biomass_bym3_mean))+ geom_density(alpha=.7)


ggplot(fish_richness_merged_isl, aes(fish_abundance_bym3))+ geom_density(alpha=.7)




###
head(fish_richness_merged_isl)
ggplot(fish_richness_merged_isl, aes( y=bird.density, x=d15n, col=log(insect_pitfall_av_abundance)))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("d15n in soil")+scale_colour_viridis()


#checked Area, Wave exposure, bird richness, % eelgrass, wrack on shore, 
#maybe something with Neighb 250
#maybe insect abundance

ggplot(fish_richness_merged_isl, aes( y=bird.density, x=Neighb_250, col=d15n))+
  geom_point(size=2)+geom_smooth() +theme_classic()+xlab("Neighb_250")+scale_colour_viridis()









### the problem with this is that the Area is by island not transect ..... 
#Could jitter points
# I think we need to do island-level stats for the Area. 







ggplot(fish_richness_merged_isl, aes(col=fish_biomass_bym3_cat_isl, y=tree_richness, x=log(Area)))+
  geom_point()+geom_smooth(method="lm")


ggplot(master_transect, aes(x=habitat_cover_1km, y=bird.density))+
  geom_point()+geom_smooth()


ggplot(master_transect, aes(x=HAB2000, y=bird.density))+
  geom_point()+geom_smooth()

ggplot(master_transect, aes(x=d15n, y=bird.density))+
  geom_point(aes(col=size.cat2))+geom_smooth()

ggplot(master_transect, aes(x=d34s, y=bird.density))+
  geom_point(aes(col=WAVE_EXPOSURE))+geom_smooth()




ggplot(master_transect, aes(x=eelgrass_cover_1km, y=fish_biomass_bym3_mean,col=node))+geom_point()+geom_smooth(method="lm")

ggplot(master_transect, aes(x=eelgrass_cover_1km, y=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")



ggplot(master_transect, aes(x=habitat_cover_1km, y=fish_demersal_biomass_bym3_mean, col=d34s))+
  geom_point()+geom_smooth(method="lm")+scale_colour_viridis()

ggplot(master_transect, aes(x=habitat_cover_1km, y=fish_demersal_biomass_bym3_mean, col=d15n))+
  geom_point()+geom_smooth(method="lm")+scale_colour_viridis()




# Determining best scale of comparison -----------------------------------------

###adding a change here
ggplot(master_transect, aes(x=as.numeric(WAVE_EXPOSURE), y=habitat_cover_1km, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")

ggplot(master_transect, aes(y=habitat_cover_1km, x=d15n, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(master_transect, aes(y=fish_biomass_bym3_mean, x=d34s))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(master_transect, aes(y=fish_biomass_bym3_mean, x=d15n))+geom_point()+geom_smooth(aes(),method="lm")



ggplot(master_transect, aes(x=log(HAB2000), y=d34s, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(master_transect, aes(x=log(HAB2000), y=d15n, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(master_transect, aes(x=slope_mean, y=d15n, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(master_transect, aes(x=slope_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")

ggplot(master_transect, aes(x=SLOPE, y=d15n, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(master_transect, aes(x=SLOPE, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(master_transect, aes(x=slope_mean, y=d34s, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(master_transect, aes(x=slope_mean, y=d34s))+geom_point()+geom_smooth(aes(),method="lm")

ggplot(master_transect, aes(x=SLOPE, y=slope_mean, col=d34s))+geom_point()+geom_smooth(aes(),method="lm")+ geom_abline(intercept = 0, slope = 1)+scale_colour_viridis()


ggplot(master_transect, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(master_transect, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(master_transect, aes(x=marine_richness_bym3, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(master_transect, aes(x=fish_richness_bym3, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(master_transect, aes(x=combined_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(master_transect, aes(x=wrack_richness, y=eagles))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(master_transect, aes(y=fish_biomass_bym3_sd, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(aes(),method="lm")

ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=d34s))+geom_point()+geom_smooth(aes(),method="gam")


ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=d15n, colour=size.cat2))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()

ggplot(master_transect, aes(col=log(Area), y=plant_richness, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis()



#### fish biomass
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=total_cover))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=shrub_cover))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=herb_cover))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=tree_abundance))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=sum_basal))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=NDVI_mean))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()


ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=shrub_cover))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=herb_cover))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=tree_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=sum_basal))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=NDVI_mean))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=insect_abs_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()+ylim(0,5000)
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=insect_herbivore_pitfall_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(master_transect, aes(x=fish_richness_corrected, y=total_cover))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_richness_corrected, y=shrub_cover))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_richness_corrected, y=herb_cover))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_richness_corrected, y=sum_basal))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_abs_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()+ylim(0,4000)
ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_pitfall_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()+ylim(0,500)
ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_beat_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_pitfall_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_detritivore_pitfall_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_carnivore_pitfall_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_herbivore_pitfall_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(master_transect, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=bird.richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=log(site_mean_by_tran + 1), y=bird.richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=insect_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=log(site_mean_by_tran+1), y=insect_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


#head(master_transect)

ggplot(master_transect, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=plant_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()

ggplot(master_transect, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=tree_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


#head(master_transect)



ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=bird.density))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_richness_corrected, y=bird.density))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=marine_richness_corrected, y=bird.density))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(master_transect, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()

ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=insect_abs_abundance))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()


ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=bird.density))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()

data_subset3 <- master_transect[ , "fish_biomass_bym3_cat"]
master_transect_nona<- master_transect[complete.cases(data_subset3), ] 
ggplot(master_transect_nona, aes(col=fish_biomass_bym3_cat, y=bird.richness, x=log_Area))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()



ggplot(master_transect, aes(x=fish_biomass_bym3_mean, y=shrub_cover))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=fish_richness_corrected, y=shrub_cover))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=d15n, y=shrub_cover))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()


ggplot(master_transect, aes(x=log(Area), y=log(plant_richness), colour=fish_biomass_bym3_cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=log(Area), y=log(plant_richness), colour=d15n.cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()



ggplot(master_transect, aes(x=log(Area), y=log(tree_richness), colour=d15n.cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()
ggplot(master_transect, aes(x=log(Area), y=log(tree_richness), colour=fish_biomass_bym3_cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()


ggplot(master_transect, aes(x=log(Area), y=log(insect_richness), colour=d15n.cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()

ggplot(master_transect, aes(x=log(Area), y=log(insect_richness), colour=fish_biomass_bym3_cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()

ggplot(master_transect, aes(x=log(Area), y=log(insect_richness), colour=fish_biomass_bym3_mean))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_c()

ggplot(master_transect, aes(x=log(Area), y=insect_richness, colour=fish_biomass_bym3_cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()

ggplot(master_transect, aes(x=log(Area), y=log(insect_richness), colour=d15n))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_c()



ggplot(master_transect, aes(x=log(Area), y=d15n, colour=fish_biomass_bym3_mean))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis()
ggplot(master_transect, aes(x=log(Area), y=d15n, colour=fish_biomass_bym3_cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()




ggplot(master_transect, aes(x=log(Area), y=d15n, colour=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis()


ggplot(master_transect, aes(x=fish_av_weight, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(master_transect, aes(x=fish_richness_corrected, y=d15n, col=PA_norml))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_c()
ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=d15n, col=PA_norml))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_c()

ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=d15n))+geom_point()+geom_smooth(aes(),method="gam")




#head(master_transect)

#plotting the relationship between n15 and fish richness at various distances
n15_100<-ggplot(master_transect_100, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("100 m")
n15_250<-ggplot(master_transect_250, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("250 m")
n15_300<-ggplot(master_transect, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("300 m")
n15_400<-ggplot(master_transect_400, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("400 m")
n15_500<-ggplot(master_transect_500, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("500 m")
n15_600<-ggplot(master_transect_600, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("600 m")
n15_850<-ggplot(master_transect_850, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("850 m")
n15_3k<-ggplot(master_transect_3k, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("3k")
n15_2k<-ggplot(master_transect_2k, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("2k")
n15_1000<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("1000 m")
plot_grid(n15_100, n15_250, n15_300, n15_400, n15_500,n15_600,n15_850, n15_1000,n15_2k,n15_3k,  ncol=5)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_richness_n15_scale_by_day_volume.png", width=40, height=20, unit="cm")

#plotting the relationship between n15 and fish biomass at various distances
n15_100<-ggplot(master_transect_100, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("100 m")
n15_250<-ggplot(master_transect_250, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("250 m")
n15_300<-ggplot(master_transect, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("300 m")
n15_400<-ggplot(master_transect_400, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("400 m")
n15_500<-ggplot(master_transect_500, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("500 m")
n15_600<-ggplot(master_transect_600, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("600 m")
n15_850<-ggplot(master_transect_850, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("850 m")
n15_3k<-ggplot(master_transect_3k, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("3k")
n15_2k<-ggplot(master_transect_2k, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("2k")
n15_1000<-ggplot(master_transect_1k, aes(x=log(fish_biomass_bym3+1), y=d15n))+geom_point()+geom_smooth(method="gam")+ggtitle("1000 m")
plot_grid(n15_100, n15_250, n15_300, n15_400, n15_500,n15_600,n15_850, n15_1000,n15_2k,n15_3k,  ncol=5)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_biomass_n15_scale_by_day_volume.png", width=40, height=20, unit="cm")



#plotting the relationship between s and fish richness at various distances
s_100<-ggplot(master_transect_100, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("100 m")
s_250<-ggplot(master_transect_250, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("250 m")
s_300<-ggplot(master_transect, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("300 m")
s_400<-ggplot(master_transect_400, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("400 m")
s_500<-ggplot(master_transect_500, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("500 m")
s_600<-ggplot(master_transect_600, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("600 m")
s_750<-ggplot(master_transect_750, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("750 m")
s_4k<-ggplot(master_transect_4k, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("4k")
s_5k<-ggplot(master_transect_5k, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("5k")
s_3k<-ggplot(master_transect_3k, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("3k")
s_2k<-ggplot(master_transect_2k, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("2k")
s_1000<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("1k")
plot_grid(s_100, s_250, s_300, s_400, s_500,s_600,s_750,s_1000, s_2k,s_3k,s_4k,s_5k , ncol=6)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_richness_s_scale_by_day_volume.png", width=40, height=20, unit="cm")


s_100<-ggplot(master_transect_100, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("100 m")
s_250<-ggplot(master_transect_250, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("250 m")
s_300<-ggplot(master_transect, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("300 m")
s_400<-ggplot(master_transect_400, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("400 m")
s_500<-ggplot(master_transect_500, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("500 m")
s_600<-ggplot(master_transect_600, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("600 m")
s_750<-ggplot(master_transect_750, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("750 m")
s_4k<-ggplot(master_transect_4k, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("4k")
s_5k<-ggplot(master_transect_5k, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("5k")
s_3k<-ggplot(master_transect_3k, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("3k")
s_2k<-ggplot(master_transect_2k, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("2k")
s_1000<-ggplot(master_transect_1k, aes(x=log(fish_biomass_bym3+1), y=s))+geom_point()+geom_smooth(method="lm")+ggtitle("1k")
plot_grid(s_100, s_250, s_300, s_400, s_500,s_600,s_750,s_1000, s_2k,s_3k,s_4k,s_5k , ncol=6)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_biomass_s_scale_by_day_volume.png", width=40, height=20, unit="cm")

## For both marine indicators, 300m jumps out as tighest relationship
##but 1km might be more reasonable for otter feeding .. think about this. 



# Plotting Chemistry by marine richness ---------------------------------------

#soil chemistry
marine_d15n<-ggplot(master_transect, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="gam")
marine_d13c<-ggplot(master_transect, aes(x=marine_richness_corrected, y=d13c))+geom_point()+geom_smooth(method="gam")
marine_c<-ggplot(master_transect, aes(x=marine_richness_corrected, y=c))+geom_point()+geom_smooth(method="gam")
marine_n<-ggplot(master_transect, aes(x=marine_richness_corrected, y=n))+geom_point()+geom_smooth(method="lm")
marine_cn<-ggplot(master_transect, aes(x=marine_richness_corrected, y=cn))+geom_point()+geom_smooth(method="gam")
marine_s<-ggplot(master_transect, aes(x=marine_richness_corrected, y=s))+geom_point()+geom_smooth(method="gam")
plot_grid(marine_d15n, marine_d13c, marine_c, marine_n, marine_cn, marine_s,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Chemistry//marine_richness_soilchem.png", width=40, height=20, unit="cm")


#salal chemistry 
gash_d15n<-ggplot(master_transect, aes(x=marine_richness_corrected, y=d15n_gash))+geom_point()+geom_smooth(method="lm")
gash_d13c<-ggplot(master_transect, aes(x=marine_richness_corrected, y=d13c_gash))+geom_point()+geom_smooth(method="lm")
gash_c<-ggplot(master_transect, aes(x=marine_richness_corrected, y=c_gash))+geom_point()+geom_smooth(method="lm")
gash_n<-ggplot(master_transect, aes(x=marine_richness_corrected, y=n_gash))+geom_point()+geom_smooth(method="lm")
gash_cn<-ggplot(master_transect, aes(x=marine_richness_corrected, y=cn_gash))+geom_point()+geom_smooth(method="lm")
gash_s<-ggplot(master_transect, aes(x=marine_richness_corrected, y=s_gash))+geom_point()+geom_smooth(method="lm")
plot_grid(gash_d15n, gash_d13c, gash_c, gash_n, gash_cn, gash_s,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Chemistry//marine_richness_gash.png", width=40, height=20, unit="cm")

#myanthemum chemistry
midi_d15n<-ggplot(master_transect, aes(x=marine_richness_corrected, y=d15n_midi))+geom_point()+geom_smooth(method="lm")
midi_d13c<-ggplot(master_transect, aes(x=marine_richness_corrected, y=d13c_midi))+geom_point()+geom_smooth(method="lm")
midi_c<-ggplot(master_transect, aes(x=marine_richness_corrected, y=c_midi))+geom_point()+geom_smooth(method="lm")
midi_n<-ggplot(master_transect, aes(x=marine_richness_corrected, y=n_midi))+geom_point()+geom_smooth(method="lm")
midi_cn<-ggplot(master_transect, aes(x=marine_richness_corrected, y=cn_midi))+geom_point()+geom_smooth(method="lm")
midi_s<-ggplot(master_transect, aes(x=marine_richness_corrected, y=s_midi))+geom_point()+geom_smooth(method="lm")
plot_grid(midi_d15n, midi_d13c, midi_c, midi_n, midi_cn, midi_s,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Chemistry//marine_richness_midi.png", width=40, height=20, unit="cm")

### beetle chemistry
beetles_d15n<-ggplot(master_transect, aes(x=marine_richness_corrected, y=d15n_beetles))+geom_point()+geom_smooth(method="lm")
beetles_d13c<-ggplot(master_transect, aes(x=marine_richness_corrected, y=d13c_beetles))+geom_point()+geom_smooth(method="lm")
beetles_c<-ggplot(master_transect, aes(x=marine_richness_corrected, y=c_beetles))+geom_point()+geom_smooth(method="lm")
beetles_n<-ggplot(master_transect, aes(x=marine_richness_corrected, y=n_beetles))+geom_point()+geom_smooth(method="lm")
beetles_cn<-ggplot(master_transect, aes(x=marine_richness_corrected, y=cn_beetles))+geom_point()+geom_smooth(method="lm")
plot_grid(beetles_d15n, beetles_d13c, beetles_c, beetles_n, beetles_cn,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Chemistry//marine_richness_beetles.png", width=40, height=20, unit="cm")

###weevil chemistry
weevils_d15n<-ggplot(master_transect, aes(x=marine_richness_corrected, y=d15n_weevils))+geom_point()+geom_smooth(method="lm")
weevils_d13c<-ggplot(master_transect, aes(x=marine_richness_corrected, y=d13c_weevils))+geom_point()+geom_smooth(method="lm")
weevils_c<-ggplot(master_transect, aes(x=marine_richness_corrected, y=c_weevils))+geom_point()+geom_smooth(method="lm")
weevils_n<-ggplot(master_transect, aes(x=marine_richness_corrected, y=n_weevils))+geom_point()+geom_smooth(method="lm")
weevils_cn<-ggplot(master_transect, aes(x=marine_richness_corrected, y=cn_weevils))+geom_point()+geom_smooth(method="lm")
plot_grid(weevils_d15n, weevils_d13c, weevils_c, weevils_n, weevils_cn,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Chemistry//marine_richness_weevils.png", width=40, height=20, unit="cm")

ggplot(master_transect, aes(x=marine_richness_corrected, y=d15n_beetles))+geom_point()+geom_smooth(method="lm")+geom_text(aes(label=unq_tran))
ggplot(master_transect, aes(x=marine_richness_corrected, y=d15n_weevils))+geom_point()+geom_smooth(method="lm")+geom_text(aes(label=unq_tran))
ggplot(master_transect, aes(x=marine_richness_corrected, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")+geom_text(aes(label=unq_tran))


### isopods chemistry
isopods_d15n<-ggplot(master_transect, aes(x=marine_richness_corrected, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")
isopods_d13c<-ggplot(master_transect, aes(x=marine_richness_corrected, y=d13c_isopods))+geom_point()+geom_smooth(method="lm")
isopods_c<-ggplot(master_transect, aes(x=marine_richness_corrected, y=c_isopods))+geom_point()+geom_smooth(method="lm")
isopods_n<-ggplot(master_transect, aes(x=marine_richness_corrected, y=n_isopods))+geom_point()+geom_smooth(method="lm")
isopods_cn<-ggplot(master_transect, aes(x=marine_richness_corrected, y=cn_isopods))+geom_point()+geom_smooth(method="lm")
plot_grid(isopods_d15n, isopods_d13c, isopods_c, isopods_n, isopods_cn,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Chemistry//marine_richness_isopods.png", width=40, height=20, unit="cm")


# Terrestrial ecology and marine richness ---------------------------------
##head(master_transect)

ggplot(master_transect, aes(y=shrub_cover, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
ggplot(master_transect_1k, aes(y=shrub_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")


ggplot(master_transect, aes(y=herb_cover, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
ggplot(master_transect, aes(y=herb_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")

ggplot(master_transect, aes(y=plant_evenness, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
ggplot(master_transect, aes(y=plant_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")

ggplot(master_transect, aes(y=plant_evenness, x=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(master_transect, aes(y=plant_richness, x=d15n))+geom_point()+geom_smooth(method="lm")

ggplot(master_transect, aes(y=insect_evenness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))
ggplot(master_transect, aes(y=insect_birdfood_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson")) 
ggplot(master_transect, aes(y=insect_detritivore_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))
ggplot(master_transect, aes(y=insect_carnivore_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson")) 

ggplot(master_transect, aes(y=insect_detritivore_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(master_transect, aes(y=insect_detritivore_abs_abundance, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(master_transect, aes(y=insect_detritivore_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(master_transect, aes(y=insect_evenness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))
ggplot(master_transect, aes(y=insect_birdfood_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson")) 
ggplot(master_transect, aes(y=insect_detritivore_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))
ggplot(master_transect, aes(y=insect_carnivore_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson")) 
ggplot(master_transect, aes(y=insect_herbivore_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))
ggplot(master_transect, aes(y=insect_parasite_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))
ggplot(master_transect, aes(y=insect_omnivore_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))

ggplot(master_transect_1k, aes(y=log(insect_abs_abundance+1), x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(master_transect, aes(y=(insect_beat_av_abundance), x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(master_transect, aes(y=(insect_pitfall_av_abundance), x=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))

ggplot(master_transect, aes(y=d15n, x=pelagic_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(master_transect, aes(y=d15n, x=demersal_richness_corrected))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))


#head(master_transect)

#transect level
marine_plant<-ggplot(master_transect, aes(x=marine_richness_corrected, y=plant.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_pc1<-ggplot(master_transect, aes(x=marine_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")
marine_tree<-ggplot(master_transect, aes(x=marine_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_treeabun<-ggplot(master_transect, aes(x=marine_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
marine_insect<-ggplot(master_transect, aes(x=marine_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_insectabund<-ggplot(master_transect, aes(x=marine_richness_corrected, y=log(insect_abs_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_plant,marine_pc1, marine_tree,marine_treeabun,marine_insect,marine_insectabund,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//marine_richness_terrestrial.png", width=40, height=20, unit="cm")


###Fish richness, fish 
fish_plant_richness<-ggplot(master_transect, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_plant_evenness<-ggplot(master_transect, aes(x=fish_richness_corrected, y=plant_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_plant_total_cover<-ggplot(master_transect, aes(x=fish_richness_corrected, y=total_cover))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_pc1<-ggplot(master_transect, aes(x=fish_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")+theme_classic()

fish_plant_richness2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_plant_evenness2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=plant_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_plant_total_cover2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=total_cover))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_pc12<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=pc1))+geom_point()+geom_smooth(method="lm")+theme_classic()

plot_grid(fish_plant_richness, fish_plant_evenness, fish_plant_total_cover, fish_pc1, 
          fish_plant_richness2, fish_plant_evenness2, fish_plant_total_cover2, fish_pc12, ncol=4)
ggsave("C:Biodiversity idea//Plots//Compiled plots//Plants_fish.png", width=40, height=20, unit="cm")

###Plant_fish
fish_plant_richness<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_plant_evenness<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=plant_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_plant_total_cover<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=total_cover))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_pc1<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")+theme_classic()

fish_plant_richness2<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_plant_evenness2<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=plant_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_plant_total_cover2<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=total_cover))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_pc12<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=pc1))+geom_point()+geom_smooth(method="lm")+theme_classic()

plot_grid(fish_plant_richness, fish_plant_evenness, fish_plant_total_cover, fish_pc1, 
          fish_plant_richness2, fish_plant_evenness2, fish_plant_total_cover2, fish_pc12, ncol=4)
ggsave("C:Biodiversity idea//Plots//Compiled plots//Plants_fish_1k.png", width=40, height=20, unit="cm")


###Tree, fish richness
fish_tree_richness<-ggplot(master_transect, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_tree_abundance<-ggplot(master_transect, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_sum_basal<-ggplot(master_transect, aes(x=fish_richness_corrected, y=sum_basal))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_NDVI<-ggplot(master_transect, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")+theme_classic()

fish_tree_richness2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_tree_abundance2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=tree_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_sum_basal2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=sum_basal))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_NDVI2<-ggplot(master_transect, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")+theme_classic()

plot_grid(fish_tree_richness, fish_tree_abundance, fish_sum_basal, fish_NDVI,  
          fish_tree_richness2, fish_tree_abundance2, fish_sum_basal2, fish_NDVI2,  ncol=4)
ggsave("C:Biodiversity idea//Plots//Compiled plots//trees_fish.png", width=40, height=20, unit="cm")

##Tree, fish
fish_tree_richness<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_tree_abundance<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_sum_basal<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=sum_basal))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_NDVI<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")+theme_classic()

fish_tree_richness2<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_tree_abundance2<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=tree_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_sum_basal2<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=sum_basal))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_NDVI2<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")+theme_classic()

plot_grid(fish_tree_richness, fish_tree_abundance, fish_sum_basal, fish_NDVI,
          fish_tree_richness2, fish_tree_abundance2, fish_sum_basal2, fish_NDVI2, ncol=4)
ggsave("C:Biodiversity idea//Plots//Compiled plots//trees_fish_1k.png", width=40, height=20, unit="cm")



###insect, fish richness
fish_insect_richness<-ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_insect_abs_abundance<-ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_abs_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_evenness<-ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_pitfall_av_abundance<-ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_pitfall_av_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()

fish_insect_richness2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_insect_abs_abundance2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=insect_abs_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_evenness2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=insect_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_pitfall_av_abundance2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=insect_pitfall_av_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()

plot_grid(fish_insect_richness, fish_insect_abs_abundance, fish_insect_pitfall_av_abundance, fish_insect_evenness,
          fish_insect_richness2, fish_insect_abs_abundance2, fish_insect_pitfall_av_abundance2, fish_insect_evenness2,  ncol=4)
ggsave("C:Biodiversity idea//Plots//Compiled plots//insects_fish.png", width=40, height=20, unit="cm")

##insect, fish
fish_insect_richness<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_insect_abs_abundance<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=insect_abs_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_evenness<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=insect_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_pitfall_av_abundance<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=insect_pitfall_av_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()

fish_insect_richness2<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_insect_abs_abundance2<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=insect_abs_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_evenness2<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=insect_evenness))+geom_point()+geom_smooth(method="lm")+theme_classic()
fish_insect_pitfall_av_abundance2<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=insect_pitfall_av_abundance))+geom_point()+geom_smooth(method="lm")+theme_classic()

plot_grid(fish_insect_richness, fish_insect_abs_abundance, fish_insect_pitfall_av_abundance, fish_insect_evenness,  
          fish_insect_richness2, fish_insect_abs_abundance2, fish_insect_pitfall_av_abundance2, fish_insect_evenness2,  ncol=4)
ggsave("C:Biodiversity idea//Plots//Compiled plots//insects_fish_1k.png", width=40, height=20, unit="cm")




###birds, fish richness
fish_mammal_richness<-ggplot(master_transect, aes(x=fish_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_bird.richness<-ggplot(master_transect, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()

fish_mammal_richness2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=mammal_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_bird.richness2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3+1), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()

plot_grid(fish_mammal_richness, fish_bird.richness,
          fish_mammal_richness2, fish_bird.richness2,  ncol=2)
ggsave("C:Biodiversity idea//Plots//Compiled plots//birds_fish.png", width=40, height=20, unit="cm")


###birds, fish richness
fish_mammal_richness<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_bird.richness<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()

fish_mammal_richness2<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=mammal_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_bird.richness2<-ggplot(master_transect_1k, aes(x=log(fish_abundance_bym3+1), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()

plot_grid(fish_mammal_richness, fish_bird.richness,
          fish_mammal_richness2, fish_bird.richness2,  ncol=2)
ggsave("C:Biodiversity idea//Plots//Compiled plots//birds_fish_1k.png", width=40, height=20, unit="cm")















#just by fish richness
fish_plant<-ggplot(master_transect, aes(x=fish_richness_corrected, y=plant.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_pc1<-ggplot(master_transect, aes(x=fish_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")
fish_tree<-ggplot(master_transect, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_treeabun<-ggplot(master_transect, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
fish_insect<-ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_insectabund<-ggplot(master_transect, aes(x=fish_richness_corrected, y=log(insect_abs_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(fish_plant,fish_pc1, fish_tree,fish_treeabun,fish_insect,fish_insectabund,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_richness_terrestrial.png", width=40, height=20, unit="cm")

#just by fish richness
fish_plant<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=plant.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_pc1<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")
fish_tree<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_treeabun<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
fish_insect<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_insectabund<-ggplot(master_transect_1k, aes(x=fish_richness_corrected, y=log(insect_abs_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(fish_plant,fish_pc1, fish_tree,fish_treeabun,fish_insect,fish_insectabund,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_richness_terrestrial_1k.png", width=40, height=20, unit="cm")




###Just insects # wait until I get the beetle data - b/c can't do it w/o beetles/weevils
# marine_beetles<-ggplot(master_transect, aes(x=marine_richness_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
# marine_isopods<-ggplot(master_transect, aes(x=marine_richness_corrected, y=crustacea_richness))+geom_point()+geom_smooth(method="lm")
# ggplot(master_transect, aes(x=marine_richness_corrected, y=crustacea_abundance))+geom_point()+geom_smooth(method="lm")
# plot_grid(marine_plant,marine_pc1, marine_tree,marine_treeabun,marine_insect,marine_insectabund,ncol=3)
# ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//marine_richness_terrestrial.png", width=40, height=20, unit="cm")


# island level
marine_bird<-ggplot(master_transect, aes(x=marine_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_habitathet<-ggplot(master_transect, aes(x=marine_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
marine_NDVI<-ggplot(master_transect, aes(x=marine_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
marine_mammal<-ggplot(master_transect, aes(x=marine_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_bird,marine_mammal, marine_habitathet,marine_NDVI, ncol=4)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//marine_richness_terrestrial_isl.png", width=40, height=20, unit="cm")




# Plotting marine resources vs. marine variables----------------------------------------------------------------

setwd("C:Biodiversity idea///Users/Norah//Dropbox/Projects/100 islands/Biodiversity idea")

#Just marine variables to eachother
marine1<-ggplot(fish_bycatch_richness_merged_tran_year, aes(x=fish_richness_corrected, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
marine2<-ggplot(fish_bycatch_richness_merged_tran_year, aes(x=log(fish_abundance_bym3), y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine3<-ggplot(fish_bycatch_richness_merged_tran_year, aes(x=fish_richness_corrected, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
marine4<-ggplot(fish_bycatch_richness_merged_tran_year, aes(x=bycatch_richness_corrected, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")



marine16<-ggplot(fish_bycatch_richness_merged_tran_year, aes(x=wrack_richness, y=log_site_sum_by_tran))+geom_point()+geom_smooth(method="lm")
marine17<-ggplot(fish_bycatch_richness_merged_tran_year, aes(x=fish_richness_corrected, y=fish_length))+geom_point()+geom_smooth(method="lm")
plot_grid(marine1, marine2, marine3, marine17, ncol=2)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//marine_plot.png")

ggplot(fish_bycatch_richness_merged_tran_year, aes(x=fish_richness_corrected, y=log(fish_abundance_bym3+1)))+geom_point()+geom_smooth(method="gam")


#nearby habitat abundance on fish and bycatch
marine6<-ggplot(master_transect, aes(x=log_HAB2000, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine7<-ggplot(master_transect, aes(x=log_HAB2000, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine8<-ggplot(master_transect, aes(x=log_HAB2000, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
marine9<-ggplot(master_transect, aes(x=log_HAB2000, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine10<-ggplot(master_transect, aes(x=log_HAB2000, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine11<-ggplot(master_transect, aes(x=log_HAB2000, y=log_site_sum_by_isl))+geom_point()+geom_smooth(method="lm")
plot_grid(marine6, marine8, marine11, marine7,marine9,marine10, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//hab2000_fish_bycatch_corrected.png")



marine_hab_1006<-ggplot(master_transect, aes(x=log(sum_100m), y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_1007<-ggplot(master_transect, aes(x=log(sum_100m), y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_1008<-ggplot(master_transect, aes(x=log(sum_100m), y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_1009<-ggplot(master_transect, aes(x=log(sum_100m), y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_10010<-ggplot(master_transect, aes(x=log(sum_100m), y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_10011<-ggplot(master_transect, aes(x=log(sum_100m), y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_1006, marine_hab_1008, marine_hab_10011, marine_hab_1007,marine_hab_1009,marine_hab_10010, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//hab100_fish_bycatch_corrected.png")

marine_hab_5006<-ggplot(master_transect, aes(x=sum_500m, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_5007<-ggplot(master_transect, aes(x=sum_500m, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_5008<-ggplot(master_transect, aes(x=sum_500m, y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_5009<-ggplot(master_transect, aes(x=sum_500m, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50010<-ggplot(master_transect, aes(x=sum_500m, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50011<-ggplot(master_transect, aes(x=sum_500m, y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_5006, marine_hab_5008, marine_hab_50011, marine_hab_5007,marine_hab_5009,marine_hab_50010, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//hab500_fish_bycatch_corrected.png")


marine_hab_5006<-ggplot(master_transect, aes(x=MEAN_egarea250, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_5007<-ggplot(master_transect, aes(x=MEAN_egarea250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_5008<-ggplot(master_transect, aes(x=MEAN_egarea250, y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_5009<-ggplot(master_transect, aes(x=MEAN_egarea250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50010<-ggplot(master_transect, aes(x=MEAN_egarea250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
plot_grid(marine_hab_5006, marine_hab_5008, marine_hab_5007,marine_hab_5009,marine_hab_50010, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//egra250_fish_bycatch_corrected.png")


##head(master_transect)
marine_hab_kelp6<-ggplot(master_transect, aes(x=MEAN_kparea250, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_kelp7<-ggplot(master_transect, aes(x=MEAN_kparea250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_kelp8<-ggplot(master_transect, aes(x=MEAN_kparea250, y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_kelp9<-ggplot(master_transect, aes(x=MEAN_kparea250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_kelp10<-ggplot(master_transect, aes(x=MEAN_kparea250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
plot_grid(marine_hab_kelp6, marine_hab_kelp8, marine_hab_kelp7,marine_hab_kelp9,marine_hab_kelp10, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//habkelp_fish_bycatch_corrected.png")


marine_hab_2506<-ggplot(master_transect, aes(x=sum_250m, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_2507<-ggplot(master_transect, aes(x=sum_250m, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_2508<-ggplot(master_transect, aes(x=sum_250m, y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_2509<-ggplot(master_transect, aes(x=sum_250m, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_25010<-ggplot(master_transect, aes(x=sum_250m, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_25011<-ggplot(master_transect, aes(x=sum_250m, y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_2506, marine_hab_2508, marine_hab_25011, marine_hab_2507,marine_hab_2509,marine_hab_25010, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//hab250_fish_bycatch_corrected.png")


veg_marine1<-ggplot(master_transect, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=bycatch_abundance_bym3))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine2<-ggplot(master_transect, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=bycatch_richness_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine3<-ggplot(master_transect, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=log(fish_abundance_bym3)))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine4<-ggplot(master_transect, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=fish_richness_corrected))+geom_boxplot()+ theme(legend.position=c(0.9,0.90))+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine5<-ggplot(master_transect, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=log(fish_biomass)))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine6<-ggplot(master_transect, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=fish_length))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
plot_grid(veg_marine4, veg_marine5, veg_marine6, veg_marine3, veg_marine2, veg_marine1, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//resource_primary_intertidal_habitat.png")


subtidal_marine1<-ggplot(master_transect, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=bycatch_abundance_bym3))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine2<-ggplot(master_transect, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=bycatch_richness_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine3<-ggplot(master_transect, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=log(fish_abundance_bym3)))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine4<-ggplot(master_transect, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=fish_richness_corrected))+geom_boxplot()+ theme(legend.position=c(0.9,0.90))+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine5<-ggplot(master_transect, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=log(fish_biomass)))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine6<-ggplot(master_transect, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=fish_length))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
plot_grid(subtidal_marine4, subtidal_marine5, subtidal_marine6, subtidal_marine3, subtidal_marine2, subtidal_marine1, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//resource_primary_subtidal_habitat.png")



# Plotting Marine resources vs. terrestrial variables  ---------------------------------------

# marine resources vs. n15 soilk
n_marine1<-ggplot(master_transect, aes(y=d15n, x=log(bycatch_abundance_bym3+1)))+geom_point()+geom_smooth(method="gam")
n_marine2<-ggplot(master_transect, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="gam")
n_marine3<-ggplot(master_transect, aes(y=d15n, x=log(fish_abundance_bym3+1)))+geom_point()+geom_smooth(method="gam")
n_marine4<-ggplot(master_transect, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="gam")
n_marine5<-ggplot(master_transect, aes(y=d15n, x=log(fish_biomass_bym3+1)))+geom_point()+geom_smooth(method="gam")
n_marine6<-ggplot(master_transect, aes(y=d15n, x=fish_length))+geom_point()+geom_smooth(method="gam")
n_marine7<-ggplot(master_transect, aes(y=d15n, x=marine_richness_corrected))+geom_point()+geom_smooth(method="gam")
n_marine8<-ggplot(master_transect, aes(y=d15n, x=fish_sd))+geom_point()+geom_smooth(method="gam")

plot_grid(n_marine4, n_marine5, n_marine7, n_marine3, n_marine2, n_marine1,n_marine6,n_marine8, ncol=4)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//d15n_marine_corrected.png", width=30, height=20, unit="cm")

s_marine1<-ggplot(master_transect, aes(y=s, x=log(bycatch_abundance_bym3+1)))+geom_point()+geom_smooth(method="gam")
s_marine2<-ggplot(master_transect, aes(y=s, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="gam")
s_marine3<-ggplot(master_transect, aes(y=s, x=log(fish_abundance_bym3+1)))+geom_point()+geom_smooth(method="gam")
s_marine4<-ggplot(master_transect, aes(y=s, x=fish_richness_corrected))+geom_point()+geom_smooth(method="gam")
s_marine5<-ggplot(master_transect, aes(y=s, x=log(fish_biomass_bym3+1)))+geom_point()+geom_smooth(method="gam")
s_marine6<-ggplot(master_transect, aes(y=s, x=fish_length))+geom_point()+geom_smooth(method="gam")
s_marine7<-ggplot(master_transect, aes(y=s, x=marine_richness_corrected))+geom_point()+geom_smooth(method="gam")
s_marine8<-ggplot(master_transect, aes(y=s, x=fish_sd))+geom_point()+geom_smooth(method="gam")

plot_grid(s_marine4, s_marine5, s_marine7, s_marine3, s_marine2, s_marine1,s_marine6,s_marine8, ncol=4)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//s_marine_corrected.png", width=30, height=20, unit="cm")


# marine resources vs. n15 soil
n_plant_marine1<-ggplot(master_transect, aes(y=d15n_gash, x=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
n_plant_marine2<-ggplot(master_transect, aes(y=d15n_gash, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
n_plant_marine3<-ggplot(master_transect, aes(y=d15n_gash, x=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
n_plant_marine4<-ggplot(master_transect, aes(y=d15n_gash, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
n_plant_marine5<-ggplot(master_transect, aes(y=d15n_gash, x=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
n_plant_marine6<-ggplot(master_transect, aes(y=d15n_gash, x=fish_length))+geom_point()+geom_smooth(method="lm")
n_plant_marine7<-ggplot(master_transect, aes(y=d15n_gash, x=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")
plot_grid(n_plant_marine4, n_plant_marine5, n_plant_marine7, n_plant_marine3, n_plant_marine2, n_plant_marine1, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//d15n_plant_marine_corrected.png")




library(mgcv)
####GAM marine resources vs. n15n
n_marine_gam1<-ggplot(master_transect, aes(y=d15n, x=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam2<-ggplot(master_transect, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam3<-ggplot(master_transect, aes(y=d15n, x=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam4<-ggplot(master_transect, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam5<-ggplot(master_transect, aes(y=d15n, x=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam6<-ggplot(master_transect, aes(y=d15n, x=fish_length))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
plot_grid(n_marine_gam4, n_marine_gam5, n_marine_gam6, n_marine_gam3, n_marine_gam2, n_marine_gam1, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//d15n_marine_corrected_GAM.png")


#fish abundance
fish1<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=d15n))+geom_point()+geom_smooth(method="lm")
fish2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish3<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish4<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish13<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish19<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish20<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish1,fish2,fish3,fish4,fish13,fish19,fish20, ncol=4)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_abund_terrestrial.png")

#fish richness
fish5<-ggplot(master_transect, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish6<-ggplot(master_transect, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish7<-ggplot(master_transect, aes(x=fish_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish8<-ggplot(master_transect, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
fish14<-ggplot(master_transect, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish21<-ggplot(master_transect, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish22<-ggplot(master_transect, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish8,fish5,fish6,fish7,fish14,fish21,fish22, ncol=4)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_richness_terrestrial.png")

#fish biomass
fish9<-ggplot(master_transect, aes(x=log(fish_biomass_bym3), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish10<-ggplot(master_transect, aes(x=log(fish_biomass_bym3), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish11<-ggplot(master_transect, aes(x=log(fish_biomass_bym3), y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish12<-ggplot(master_transect, aes(x=log(fish_biomass_bym3), y=d15n))+geom_point()+geom_smooth(method="lm")
fish15<-ggplot(master_transect, aes(x=log(fish_biomass_bym3), y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish23<-ggplot(master_transect, aes(x=log(fish_biomass_bym3), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish24<-ggplot(master_transect, aes(x=log(fish_biomass_bym3), y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish12, fish9,fish10,fish11,fish15,fish23,fish24, ncol=4)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_biomass_bym3_terrestrial.png")

###fish richness
fish1<-ggplot(master_transect, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
fish2<-ggplot(master_transect, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish3<-ggplot(master_transect, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish4<-ggplot(master_transect, aes(x=fish_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish10<-ggplot(master_transect, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish12<-ggplot(master_transect, aes(x=fish_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
fish13<-ggplot(master_transect, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish14<-ggplot(master_transect, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
fish19<-ggplot(master_transect, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish20<-ggplot(master_transect, aes(x=fish_richness_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(fish1,fish2,fish3,fish4,fish10,fish12, fish13,fish14,fish19,fish20, ncol=5)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_richness_terrestrial.png", width=40, height=20, unit="cm")


###fish abundance
fish1<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=d15n))+geom_point()+geom_smooth(method="lm")
fish2<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish3<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish4<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish10<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish12<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=mammal_richness))+geom_point()+geom_smooth(method="lm")
fish13<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish14<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=tree_abundance))+geom_point()+geom_smooth(method="lm")
fish19<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish20<-ggplot(master_transect, aes(x=log(fish_abundance_bym3), y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(fish1,fish2,fish3,fish4,fish10,fish12, fish13,fish14,fish19,fish20, ncol=5)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_abundance_terrestrial.png", width=40, height=20, unit="cm")




###bycatch richness and abundance
bycatch1<-ggplot(master_transect, aes(x=bycatch_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
bycatch2<-ggplot(master_transect, aes(x=bycatch_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch3<-ggplot(master_transect, aes(x=bycatch_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch4<-ggplot(master_transect, aes(x=bycatch_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
bycatch10<-ggplot(master_transect, aes(x=bycatch_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
bycatch12<-ggplot(master_transect, aes(x=bycatch_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
bycatch13<-ggplot(master_transect, aes(x=bycatch_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch14<-ggplot(master_transect, aes(x=bycatch_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
bycatch19<-ggplot(master_transect, aes(x=bycatch_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch20<-ggplot(master_transect, aes(x=bycatch_richness_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(bycatch1,bycatch2,bycatch3,bycatch4,bycatch10,bycatch12, bycatch13,bycatch14,bycatch19,bycatch20, ncol=5)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//bycatch_richness_terrestrial.png", width=40, height=20, unit="cm")




bycatch5<-ggplot(master_transect, aes(x=bycatch_abundance_bym3, y=d15n))+geom_point()+geom_smooth(method="lm")
bycatch6<-ggplot(master_transect, aes(x=bycatch_abundance_bym3, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch7<-ggplot(master_transect, aes(x=bycatch_abundance_bym3, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch8<-ggplot(master_transect, aes(x=bycatch_abundance_bym3, y=habitat_het))+geom_point()+geom_smooth(method="lm")
bycatch9<-ggplot(master_transect, aes(x=bycatch_abundance_bym3, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
bycatch11<-ggplot(master_transect, aes(x=bycatch_abundance_bym3, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
bycatch15<-ggplot(master_transect, aes(x=bycatch_abundance_bym3, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch16<-ggplot(master_transect, aes(x=bycatch_abundance_bym3, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
bycatch17<-ggplot(master_transect, aes(x=bycatch_abundance_bym3, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch18<-ggplot(master_transect, aes(x=bycatch_abundance_bym3, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(bycatch5,bycatch6,bycatch7,bycatch8,bycatch9,bycatch11,bycatch15,bycatch16,bycatch17,bycatch18,ncol=5)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//bycatch_abundance_terrestrial.png", width=40, height=20, unit="cm")



# Plotting Marine resources vs. biogeography ----------------------------

#histogram of marine variab
hist_marine1<-ggplot(master_transect, aes(x=bycatch_abundance_bym3))+geom_density()
hist_marine2<-ggplot(master_transect, aes(x=bycatch_richness_corrected))+geom_density()
hist_marine3<-ggplot(master_transect, aes(x=log(fish_abundance_bym3)))+geom_density()
hist_marine4<-ggplot(master_transect, aes(x=fish_richness_corrected))+geom_density()
hist_marine5<-ggplot(master_transect, aes(x=log(fish_biomass_bym3)))+geom_density()
hist_marine6<-ggplot(master_transect, aes(x=fish_length))+geom_density()
hist_marine7<-ggplot(master_transect, aes(x=marine_richness_corrected))+geom_density()
hist_marine8<-ggplot(master_transect, aes(x=marine_abundance_bym3))+geom_density()
plot_grid(hist_marine4, hist_marine5, hist_marine6, hist_marine3, hist_marine2, hist_marine1, hist_marine7, ncol=4)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_biogeog//histogram_marine.png")



### Marine resources vs. Island area
lat_marine1<-ggplot(master_transect, aes(x=lat, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
lat_marine2<-ggplot(master_transect, aes(x=lat, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
lat_marine3<-ggplot(master_transect, aes(x=lat, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
lat_marine4<-ggplot(master_transect, aes(x=lat, y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
lat_marine5<-ggplot(master_transect, aes(x=lat, y=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
lat_marine6<-ggplot(master_transect, aes(x=lat, y=fish_length))+geom_point()+geom_smooth(method="lm")
lat_marine7<-ggplot(master_transect, aes(x=lat, y=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

plot_grid(lat_marine4, lat_marine5, lat_marine6, lat_marine3, lat_marine2, lat_marine1, lat_marine7, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_biogeog//Latitude_marine.png")


### Marine resources vs. Island area
A_marine1<-ggplot(master_transect, aes(x=log_Area, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
A_marine2<-ggplot(master_transect, aes(x=log_Area, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
A_marine3<-ggplot(master_transect, aes(x=log_Area, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
A_marine4<-ggplot(master_transect, aes(x=log_Area, y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
A_marine5<-ggplot(master_transect, aes(x=log_Area, y=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
A_marine6<-ggplot(master_transect, aes(x=log_Area, y=fish_length))+geom_point()+geom_smooth(method="lm")
A_marine7<-ggplot(master_transect, aes(x=log_Area, y=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

plot_grid(A_marine4, A_marine5, A_marine6, A_marine3, A_marine2, A_marine1, A_marine7, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_biogeog//Area_marine.png", width=40, height=40, unit="cm")



###Coverage of neighbouring land mass, LOW NEighb_250 = high exposure
neib1<-ggplot(master_transect, aes(x=Neighb_250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
neib2<-ggplot(master_transect, aes(x=Neighb_250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
neib4<- ggplot(master_transect, aes(x=Neighb_250, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
neib5<- ggplot(master_transect, aes(x=Neighb_250, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
neib6<-ggplot(master_transect, aes(x=Neighb_250, y=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
neib7<-ggplot(master_transect, aes(x=Neighb_250, y=fish_length))+geom_point()+geom_smooth(method="lm")
neib8<-ggplot(master_transect, aes(x=Neighb_250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

plot_grid(neib1,neib2,neib4,neib5,neib6,neib7,neib8, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_biogeog//Neighb250_marine.png")

##head(master_transect)
### Marine resources vs. Island area
DN_marine1<-ggplot(master_transect, aes(x=log(Dist_Near), y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
DN_marine2<-ggplot(master_transect, aes(x=log(Dist_Near), y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
DN_marine3<-ggplot(master_transect, aes(x=log(Dist_Near), y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
DN_marine4<-ggplot(master_transect, aes(x=log(Dist_Near), y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
DN_marine5<-ggplot(master_transect, aes(x=log(Dist_Near), y=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
DN_marine6<-ggplot(master_transect, aes(x=log(Dist_Near), y=fish_length))+geom_point()+geom_smooth(method="lm")
DN_marine7<-ggplot(master_transect, aes(x=log(Dist_Near), y=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

plot_grid(DN_marine4, DN_marine5, DN_marine6, DN_marine3, DN_marine2, DN_marine1, DN_marine7, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_biogeog//Dist_Near_marine.png", width=40, height=40, unit="cm")





###########3
# d15n bycatch ---------------------------------------------------------------
master_transect_zscores<-master_transect
master_transect_zscores$bycatch_richness_corrected<-scale(master_transect$bycatch_richness_corrected, center=TRUE, scale=TRUE)
master_transect_zscores$bycatch_richness_corrected.unscaled <-master_transect_zscores$bycatch_richness_corrected * attr(master_transect_zscores$bycatch_richness_corrected, 'scaled:scale') + attr(master_transect_zscores$bycatch_richness_corrected, 'scaled:center')
master_transect_zscores$bycatch_richness_corrected<-as.numeric(master_transect_zscores$bycatch_richness_corrected)
ggplot(master_transect_zscores, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")

#str(master_transect_zscores)

qqp(master_transect_zscores$d15n)
qqp(master_transect_zscores$d15n, "lnorm")

lm.d15n.bycatch<-lm(d15n ~ bycatch_richness_corrected, data=master_transect_zscores)
gam.lm.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = master_transect_zscores, select=TRUE, method="REML")
gam.loglink.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = master_transect_zscores, family = gaussian(link="log"), select=TRUE, method="REML")

gam.gamma.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = master_transect_zscores, family = Gamma, select=TRUE, method="REML")
gam.tweedie.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = master_transect_zscores, family = tw, select=TRUE, method="REML")


AICtab( lm.d15n.bycatch,  gam.lm.d15n.bycatch, gam.loglink.d15n.bycatch, gam.gamma.d15n.bycatch, gam.tweedie.d15n.bycatch)

#gam.lm.d15n.bycatch


plot(gam.gamma.d15n.bycatch, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
appraise(gam.gamma.d15n.bycatch)
qq_plot(gam.gamma.d15n.bycatch, method = 'simulate')
k.check(gam.gamma.d15n.bycatch)
summary(gam.gamma.d15n.bycatch)



fam.gam.d15n.bycatch <- family(gam.gamma.d15n.bycatch)
fam.gam.d15n.bycatch
#str(fam.gam.d15n.bycatch)
ilink.gam.d15n.bycatch<- fam.gam.d15n.bycatch$linkinv
ilink.gam.d15n.bycatch


mod.d15n.bycatch<-gam.gamma.d15n.bycatch
ndata.d15n.bycatch <- with(master_transect_zscores, data_frame(bycatch_richness_corrected = seq(min(bycatch_richness_corrected), max(bycatch_richness_corrected),length = 100)))


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

master_transect_zscores$bycatch_richness_corrected<-scale(master_transect$bycatch_richness_corrected, center=TRUE, scale=TRUE)

ndata.d15n.bycatch$bycatch_richness_corrected.unscaled<-ndata.d15n.bycatch$bycatch_richness_corrected * attr(master_transect_zscores$bycatch_richness_corrected, 'scaled:scale') + attr(master_transect_zscores$bycatch_richness_corrected, 'scaled:center')


# plot 
plt.d15n.bycatch <- ggplot(ndata.d15n.bycatch, aes(x = bycatch_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = master_transect_zscores)+
  xlab(expression("Bycatch richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.bycatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.bycatch
ggsave("C:Biodiversity idea//Plots//Transect//d15n_bycatch_400m.png")



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

master_transect_zscores<-master_transect
master_transect_zscores$marine_richness_corrected<-scale(master_transect$marine_richness_corrected, center=TRUE, scale=TRUE)
master_transect_zscores$marine_richness_corrected.unscaled <-master_transect_zscores$marine_richness_corrected * attr(master_transect_zscores$marine_richness_corrected, 'scaled:scale') + attr(master_transect_zscores$marine_richness_corrected, 'scaled:center')
master_transect_zscores$marine_richness_corrected<-as.numeric(master_transect_zscores$marine_richness_corrected)
ggplot(master_transect_zscores, aes(y=d15n, x=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

qqp(master_transect_zscores$d15n)
qqp(master_transect_zscores$d15n, "lnorm")

lm.d15n.marinecatch<-lm(d15n ~ marine_richness_corrected, data=master_transect_zscores)
gam.lm.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = master_transect_zscores, select=TRUE, method="REML")
gam.loglink.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = master_transect_zscores, family = gaussian(link="log"), select=TRUE, method="REML")

gam.gamma.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = master_transect_zscores, family = Gamma, select=TRUE, method="REML")
gam.tweedie.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = master_transect_zscores, family = tw, select=TRUE, method="REML")


AICtab( lm.d15n.marinecatch,  gam.lm.d15n.marinecatch, gam.loglink.d15n.marinecatch, gam.gamma.d15n.marinecatch, gam.tweedie.d15n.marinecatch)

#gam.lm.d15n.marinecatch


plot(gam.lm.d15n.marinecatch , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
appraise(gam.lm.d15n.marinecatch )
qq_plot(gam.lm.d15n.marinecatch , method = 'simulate')
k.check(gam.lm.d15n.marinecatch )
summary(gam.lm.d15n.marinecatch )



fam.gam.d15n.marinecatch <- family(gam.lm.d15n.marinecatch )
fam.gam.d15n.marinecatch
#str(fam.gam.d15n.marinecatch)
ilink.gam.d15n.marinecatch<- fam.gam.d15n.marinecatch$linkinv
ilink.gam.d15n.marinecatch


mod.d15n.marinecatch<-gam.lm.d15n.marinecatch 
ndata.d15n.marinecatch <- with(master_transect_zscores, data_frame(marine_richness_corrected = seq(min(marine_richness_corrected), max(marine_richness_corrected),length = 100)))


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

master_transect_zscores$marine_richness_corrected<-scale(master_transect$marine_richness_corrected, center=TRUE, scale=TRUE)

ndata.d15n.marinecatch$marine_richness_corrected.unscaled<-ndata.d15n.marinecatch$marine_richness_corrected * attr(master_transect_zscores$marine_richness_corrected, 'scaled:scale') + attr(master_transect_zscores$marine_richness_corrected, 'scaled:center')


# plot 
plt.d15n.marinecatch <- ggplot(ndata.d15n.marinecatch, aes(x = marine_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = master_transect_zscores)+
  xlab(expression("Marine catch richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.marinecatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.marinecatch
ggsave("C:Biodiversity idea//Plots//Transect//d15n_bycatch.png")


# d15n fish richness ------------------------------------------------------


master_transect_zscores<-master_transect
master_transect_zscores$fish_richness_corrected<-scale(master_transect$fish_richness_corrected, center=TRUE, scale=TRUE)
master_transect_zscores$fish_richness_corrected.unscaled <-master_transect_zscores$fish_richness_corrected * attr(master_transect_zscores$fish_richness_corrected, 'scaled:scale') + attr(master_transect_zscores$fish_richness_corrected, 'scaled:center')
master_transect_zscores$fish_richness_corrected<-as.numeric(master_transect_zscores$fish_richness_corrected)
ggplot(master_transect_zscores, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")

#str(master_transect_zscores)

qqp(master_transect_zscores$d15n)
qqp(master_transect_zscores$d15n, "lnorm")

gamma.12.fish_richness_corrected<-fitdi#str(master_transect$fish_richness_corrected+0.01, "gamma")
qqp(master_transect$fish_richness_corrected, "gamma", shape = gamma.12.fish_richness_corrected$estimate[[1]], rate = gamma.12.fish_richness_corrected$estimate[[2]])

lm.d15n.fish<-lm(d15n ~ fish_richness_corrected, data=master_transect_zscores)
gam.lm.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = master_transect_zscores, select=TRUE, method="REML")
gam.loglink.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = master_transect_zscores, family = gaussian(link="log"), select=TRUE, method="REML")
gam.gamma.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = master_transect_zscores, family = Gamma, select=TRUE, method="REML")
gam.tweedie.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = master_transect_zscores, family = tw, select=TRUE, method="REML")


AICtab( gam.gamma.d15n.fish,  lm.d15n.fish, gam.loglink.d15n.fish, gam.lm.d15n.fish, gam.tweedie.d15n.fish)

#gam.gam.gamma.d15n.fish


plot(gam.gamma.d15n.fish, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
appraise(gam.gamma.d15n.fish)
qq_plot(gam.gamma.d15n.fish, method = 'simulate')
k.check(gam.gamma.d15n.fish)
summary(gam.gamma.d15n.fish)



fam.gam.d15n.fish <- family(gam.gamma.d15n.fish)
fam.gam.d15n.fish
#str(fam.gam.d15n.fish)
ilink.gam.d15n.fish<- fam.gam.d15n.fish$linkinv
ilink.gam.d15n.fish


mod.d15n.fish<-gam.gamma.d15n.fish
ndata.d15n.fish <- with(master_transect_zscores, data_frame(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100)))


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

master_transect_zscores$fish_richness_corrected<-scale(master_transect$fish_richness_corrected, center=TRUE, scale=TRUE)
ndata.d15n.fish$fish_richness_corrected.unscaled<-ndata.d15n.fish$fish_richness_corrected * attr(master_transect_zscores$fish_richness_corrected, 'scaled:scale') + attr(master_transect_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.d15n.fish <- ggplot(ndata.d15n.fish, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = master_transect_zscores)+
  xlab(expression("fish richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.fish,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.fish
ggsave("C:Biodiversity idea//Plots//Transect//d15n_fish_400.png")





# Correlation -------------------------------------------------------------

which( colnames(master_transect)=="Exp_SHZN" )
which( colnames(master_transect)=="lat" )
which( colnames(master_transect)=="size.cat2" )
which( colnames(master_transect)=="notes" )

#str(master_transect)
corr_by_isl_selected_fish<-master_transect %>% select_if(is.numeric)
#str(corr_by_isl_selected_fish)


corr_by_isl_selected_fish_2 <- round(cor(corr_by_isl_selected_fish, use="pairwise.complete.obs"), 1)
##head(corr_by_isl_selected_fish_2[, 1:6])
p.mat_by_isl_selected_fish_2 <- cor_pmat(corr_by_isl_selected_fish)
##head(p.mat_by_isl_selected_fish_2[, 1:4])

ggcorrplot(corr_by_isl_selected_fish_2)
ggcorrplot(corr_by_isl_selected_fish_2, hc.order = TRUE, type = "lower",
           outline.col = "white")
ggcorrplot(corr_by_isl_selected_fish_2, hc.order = TRUE,
           type = "lower", p.mat = p.mat_by_isl_selected_fish_2)

ggcorrplot(corr_by_isl_selected_fish_2, hc.order = TRUE, type = "lower",
           lab = TRUE)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//ggcorrplot_marine_terrestrial.png",  width=40, height=40, unit="cm")

library(purrr)
library(ggcorrplot)
library(corrr)
library(PerformanceAnalytics)


corr_by_isl_selected_fish %>% correlate() %>%  network_plot(min_cor = 0.7)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//networkplot_marine_terrestrial_0.7.png")
