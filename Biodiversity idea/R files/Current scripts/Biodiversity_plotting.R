library(here)

fish_richness_merged_tran_isl<-read.csv("C:Biodiversity idea//Output files//fish_richness_merged_tran_isl.csv")
head(fish_richness_merged_tran_isl)


ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=bird.de, col=fish_biomass_bym3_cat))+
  geom_point()+geom_smooth(method="lm")


ggplot(fish_richness_merged_isl, aes(col=fish_biomass_bym3_cat_tran, y=tree_richness, x=log(Area)))+
  geom_point()+geom_smooth(method="lm")

### the problem with this is that the Area is by island not transect ..... 
#Could jitter points
# I think we need to do island-level stats for the Area. 



ggplot(fish_richness_merged_tran, aes(col=fish_biomass_bym3_cat_tran, y=tree_richness, x=log(Area)))+
  geom_point()+geom_smooth(method="lm")


ggplot(fish_richness_merged_tran_isl, aes(x=habitat_cover_1km, y=bird.density))+
  geom_point()+geom_smooth()


ggplot(fish_richness_merged_tran_isl, aes(x=HAB2000, y=bird.density))+
  geom_point()+geom_smooth()

ggplot(fish_richness_merged_tran_isl, aes(x=d15n, y=bird.density))+
  geom_point(aes(col=size.cat2))+geom_smooth()

ggplot(fish_richness_merged_tran_isl, aes(x=d34s, y=bird.density))+
  geom_point(aes(col=WAVE_EXPOSURE))+geom_smooth()




ggplot(fish_richness_merged_tran_isl, aes(x=eelgrass_cover_1km, y=fish_biomass_bym3_mean,col=node))+geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_tran_isl, aes(x=eelgrass_cover_1km, y=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")



ggplot(fish_richness_merged_tran_isl, aes(x=habitat_cover_1km, y=fish_demersal_biomass_bym3_mean, col=d34s))+
  geom_point()+geom_smooth(method="lm")+scale_colour_viridis()

ggplot(fish_richness_merged_tran_isl, aes(x=habitat_cover_1km, y=fish_demersal_biomass_bym3_mean, col=d15n))+
  geom_point()+geom_smooth(method="lm")+scale_colour_viridis()




# Determining best scale of comparison -----------------------------------------

###adding a change here
ggplot(fish_richness_merged_tran_isl, aes(x=as.numeric(WAVE_EXPOSURE), y=habitat_cover_1km, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")

ggplot(fish_richness_merged_tran_isl, aes(y=habitat_cover_1km, x=d15n, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(fish_richness_merged_tran_isl, aes(y=fish_biomass_bym3_mean, x=d34s, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl, aes(y=fish_biomass_bym3_mean, x=d15n, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")



ggplot(fish_richness_merged_tran_isl, aes(x=log(HAB2000), y=d34s, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl, aes(x=log(HAB2000), y=d15n, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(fish_richness_merged_tran_isl, aes(x=slope_mean, y=d15n, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl, aes(x=slope_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")

ggplot(fish_richness_merged_tran_isl, aes(x=SLOPE, y=d15n, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl, aes(x=SLOPE, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(fish_richness_merged_tran_isl, aes(x=slope_mean, y=d34s, col=size.cat2))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl, aes(x=slope_mean, y=d34s))+geom_point()+geom_smooth(aes(),method="lm")

ggplot(fish_richness_merged_tran_isl, aes(x=SLOPE, y=slope_mean, col=d34s))+geom_point()+geom_smooth(aes(),method="lm")+ geom_abline(intercept = 0, slope = 1)+scale_colour_viridis()


ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl, aes(x=marine_richness_bym3, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_bym3, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl, aes(x=combined_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(fish_richness_merged_tran_isl, aes(x=wrack_richness, y=eagles))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(fish_richness_merged_tran_isl, aes(x=log(fish_abundance_bym3+1), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")
ggplot(fish_richness_merged_tran_isl, aes(y=fish_biomass_bym3_sd, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(aes(),method="lm")

ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=d34s))+geom_point()+geom_smooth(aes(),method="gam")


ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=d15n, colour=size.cat2))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()

ggplot(fish_richness_merged_tran_isl, aes(col=log(Area), y=plant_richness, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis()



#### fish biomass
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=total_cover))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=shrub_cover))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=herb_cover))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=tree_abundance))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=sum_basal))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=NDVI_mean))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()


ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=shrub_cover))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=herb_cover))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=tree_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=sum_basal))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=NDVI_mean))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=insect_abs_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()+ylim(0,5000)
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=insect_herbivore_pitfall_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=total_cover))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=shrub_cover))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=herb_cover))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=sum_basal))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=insect_abs_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()+ylim(0,4000)
ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=insect_pitfall_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()+ylim(0,500)
ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=insect_beat_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=insect_pitfall_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=insect_detritivore_pitfall_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=insect_carnivore_pitfall_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=insect_herbivore_pitfall_av_abundance))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=bird.richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=log(site_mean_by_tran + 1), y=bird.richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=insect_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=log(site_mean_by_tran+1), y=insect_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


#head(fish_richness_merged_tran_isl)

ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=plant_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()

ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=tree_richness))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


#head(fish_richness_merged_tran_isl)



ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=bird.density))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=bird.density))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=marine_richness_corrected, y=bird.density))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()


ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()

ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=insect_abs_abundance))+geom_point()+geom_smooth(aes(),method="gam", formula = y ~ s(x))+scale_colour_viridis_d()


ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=bird.density))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()

data_subset3 <- fish_richness_merged_tran_isl[ , "fish_biomass_bym3_cat"]
fish_richness_merged_tran_isl_nona<- fish_richness_merged_tran_isl[complete.cases(data_subset3), ] 
ggplot(fish_richness_merged_tran_isl_nona, aes(col=fish_biomass_bym3_cat, y=bird.richness, x=log_Area))+geom_point()+geom_smooth(aes(),method="lm")+scale_colour_viridis_d()



ggplot(fish_richness_merged_tran_isl, aes(x=fish_biomass_bym3_mean, y=shrub_cover))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=shrub_cover))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=d15n, y=shrub_cover))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()


ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=log(plant_richness), colour=fish_biomass_bym3_cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=log(plant_richness), colour=d15n.cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()



ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=log(tree_richness), colour=d15n.cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()
ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=log(tree_richness), colour=fish_biomass_bym3_cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()


ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=log(insect_richness), colour=d15n.cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()

ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=log(insect_richness), colour=fish_biomass_bym3_cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()

ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=log(insect_richness), colour=fish_biomass_bym3_mean))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_c()

ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=insect_richness, colour=fish_biomass_bym3_cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()

ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=log(insect_richness), colour=d15n))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_c()



ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=d15n, colour=fish_biomass_bym3_mean))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis()
ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=d15n, colour=fish_biomass_bym3_cat))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_d()




ggplot(fish_richness_merged_tran_isl, aes(x=log(Area), y=d15n, colour=fish_richness_corrected))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis()


ggplot(fish_richness_merged_tran_isl, aes(x=fish_av_weight, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")


ggplot(fish_richness_merged_tran_isl, aes(x=fish_richness_corrected, y=d15n, col=PA_norml))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_c()
ggplot(fish_richness_merged_tran_isl, aes(x=log(fish_abundance_bym3+1), y=d15n, col=PA_norml))+geom_point()+geom_smooth(aes(),method="gam")+scale_colour_viridis_c()

ggplot(fish_richness_merged_tran_isl, aes(x=log(fish_abundance_bym3+1), y=d15n))+geom_point()+geom_smooth(aes(),method="gam")




#head(fish_richness_merged_tran_isl)

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
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_richness_n15_scale_by_day_volume.png", width=40, height=20, unit="cm")

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
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_biomass_n15_scale_by_day_volume.png", width=40, height=20, unit="cm")



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
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_richness_s_scale_by_day_volume.png", width=40, height=20, unit="cm")


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
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_biomass_s_scale_by_day_volume.png", width=40, height=20, unit="cm")

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
ggsave("C:Biodiversity idea//Plots//Transect//Chemistry//marine_richness_soilchem.png", width=40, height=20, unit="cm")


#salal chemistry 
gash_d15n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n_gash))+geom_point()+geom_smooth(method="lm")
gash_d13c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d13c_gash))+geom_point()+geom_smooth(method="lm")
gash_c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=c_gash))+geom_point()+geom_smooth(method="lm")
gash_n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=n_gash))+geom_point()+geom_smooth(method="lm")
gash_cn<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=cn_gash))+geom_point()+geom_smooth(method="lm")
gash_s<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=s_gash))+geom_point()+geom_smooth(method="lm")
plot_grid(gash_d15n, gash_d13c, gash_c, gash_n, gash_cn, gash_s,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Chemistry//marine_richness_gash.png", width=40, height=20, unit="cm")

#myanthemum chemistry
midi_d15n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n_midi))+geom_point()+geom_smooth(method="lm")
midi_d13c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d13c_midi))+geom_point()+geom_smooth(method="lm")
midi_c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=c_midi))+geom_point()+geom_smooth(method="lm")
midi_n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=n_midi))+geom_point()+geom_smooth(method="lm")
midi_cn<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=cn_midi))+geom_point()+geom_smooth(method="lm")
midi_s<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=s_midi))+geom_point()+geom_smooth(method="lm")
plot_grid(midi_d15n, midi_d13c, midi_c, midi_n, midi_cn, midi_s,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Chemistry//marine_richness_midi.png", width=40, height=20, unit="cm")

### beetle chemistry
beetles_d15n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n_beetles))+geom_point()+geom_smooth(method="lm")
beetles_d13c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d13c_beetles))+geom_point()+geom_smooth(method="lm")
beetles_c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=c_beetles))+geom_point()+geom_smooth(method="lm")
beetles_n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=n_beetles))+geom_point()+geom_smooth(method="lm")
beetles_cn<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=cn_beetles))+geom_point()+geom_smooth(method="lm")
plot_grid(beetles_d15n, beetles_d13c, beetles_c, beetles_n, beetles_cn,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Chemistry//marine_richness_beetles.png", width=40, height=20, unit="cm")

###weevil chemistry
weevils_d15n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d15n_weevils))+geom_point()+geom_smooth(method="lm")
weevils_d13c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=d13c_weevils))+geom_point()+geom_smooth(method="lm")
weevils_c<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=c_weevils))+geom_point()+geom_smooth(method="lm")
weevils_n<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=n_weevils))+geom_point()+geom_smooth(method="lm")
weevils_cn<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=cn_weevils))+geom_point()+geom_smooth(method="lm")
plot_grid(weevils_d15n, weevils_d13c, weevils_c, weevils_n, weevils_cn,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Chemistry//marine_richness_weevils.png", width=40, height=20, unit="cm")

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
ggsave("C:Biodiversity idea//Plots//Transect//Chemistry//marine_richness_isopods.png", width=40, height=20, unit="cm")


# Terrestrial ecology and marine richness ---------------------------------
##head(fish_richness_merged_tran_isl_300)

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


#head(fish_richness_merged_tran_isl_300)

#transect level
marine_plant<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=plant.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_pc1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")
marine_tree<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_treeabun<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
marine_insect<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_insectabund<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=log(insect_abs_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_plant,marine_pc1, marine_tree,marine_treeabun,marine_insect,marine_insectabund,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//marine_richness_terrestrial.png", width=40, height=20, unit="cm")


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
ggsave("C:Biodiversity idea//Plots//Compiled plots//Plants_fish.png", width=40, height=20, unit="cm")

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
ggsave("C:Biodiversity idea//Plots//Compiled plots//Plants_fish_1k.png", width=40, height=20, unit="cm")


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
ggsave("C:Biodiversity idea//Plots//Compiled plots//trees_fish.png", width=40, height=20, unit="cm")

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
ggsave("C:Biodiversity idea//Plots//Compiled plots//trees_fish_1k.png", width=40, height=20, unit="cm")



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
ggsave("C:Biodiversity idea//Plots//Compiled plots//insects_fish.png", width=40, height=20, unit="cm")

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
ggsave("C:Biodiversity idea//Plots//Compiled plots//insects_fish_1k.png", width=40, height=20, unit="cm")




###birds, fish richness
fish_mammal_richness<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_bird.richness<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()

fish_mammal_richness2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=mammal_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_bird.richness2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3+1), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()

plot_grid(fish_mammal_richness, fish_bird.richness,
          fish_mammal_richness2, fish_bird.richness2,  ncol=2)
ggsave("C:Biodiversity idea//Plots//Compiled plots//birds_fish.png", width=40, height=20, unit="cm")


###birds, fish richness
fish_mammal_richness<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_bird.richness<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()

fish_mammal_richness2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=mammal_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()
fish_bird.richness2<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=log(fish_abundance_bym3+1), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))+theme_classic()

plot_grid(fish_mammal_richness, fish_bird.richness,
          fish_mammal_richness2, fish_bird.richness2,  ncol=2)
ggsave("C:Biodiversity idea//Plots//Compiled plots//birds_fish_1k.png", width=40, height=20, unit="cm")















#just by fish richness
fish_plant<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=plant.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_pc1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")
fish_tree<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_treeabun<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
fish_insect<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_insectabund<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=log(insect_abs_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(fish_plant,fish_pc1, fish_tree,fish_treeabun,fish_insect,fish_insectabund,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_richness_terrestrial.png", width=40, height=20, unit="cm")

#just by fish richness
fish_plant<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=plant.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_pc1<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")
fish_tree<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_treeabun<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
fish_insect<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish_insectabund<-ggplot(fish_richness_merged_tran_isl_1k, aes(x=fish_richness_corrected, y=log(insect_abs_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(fish_plant,fish_pc1, fish_tree,fish_treeabun,fish_insect,fish_insectabund,ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_richness_terrestrial_1k.png", width=40, height=20, unit="cm")




###Just insects # wait until I get the beetle data - b/c can't do it w/o beetles/weevils
# marine_beetles<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
# marine_isopods<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=crustacea_richness))+geom_point()+geom_smooth(method="lm")
# ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=crustacea_abundance))+geom_point()+geom_smooth(method="lm")
# plot_grid(marine_plant,marine_pc1, marine_tree,marine_treeabun,marine_insect,marine_insectabund,ncol=3)
# ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//marine_richness_terrestrial.png", width=40, height=20, unit="cm")


# island level
marine_bird<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_habitathet<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
marine_NDVI<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
marine_mammal<-ggplot(fish_richness_merged_tran_isl_300, aes(x=marine_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
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
marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_HAB2000, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_HAB2000, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine8<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_HAB2000, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
marine9<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_HAB2000, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine10<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_HAB2000, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine11<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_HAB2000, y=log_site_sum_by_isl))+geom_point()+geom_smooth(method="lm")
plot_grid(marine6, marine8, marine11, marine7,marine9,marine10, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//hab2000_fish_bycatch_corrected.png")



marine_hab_1006<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(sum_100m), y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_1007<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(sum_100m), y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_1008<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(sum_100m), y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_1009<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(sum_100m), y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_10010<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(sum_100m), y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_10011<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(sum_100m), y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_1006, marine_hab_1008, marine_hab_10011, marine_hab_1007,marine_hab_1009,marine_hab_10010, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//hab100_fish_bycatch_corrected.png")

marine_hab_5006<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_500m, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_5007<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_500m, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_5008<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_500m, y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_5009<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_500m, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50010<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_500m, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50011<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_500m, y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_5006, marine_hab_5008, marine_hab_50011, marine_hab_5007,marine_hab_5009,marine_hab_50010, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//hab500_fish_bycatch_corrected.png")


marine_hab_5006<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_egarea250, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_5007<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_egarea250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_5008<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_egarea250, y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_5009<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_egarea250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50010<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_egarea250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
plot_grid(marine_hab_5006, marine_hab_5008, marine_hab_5007,marine_hab_5009,marine_hab_50010, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//egra250_fish_bycatch_corrected.png")


##head(fish_richness_merged_tran_isl_300)
marine_hab_kelp6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_kparea250, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_kelp7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_kparea250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_kelp8<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_kparea250, y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_kelp9<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_kparea250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_kelp10<-ggplot(fish_richness_merged_tran_isl_300, aes(x=MEAN_kparea250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
plot_grid(marine_hab_kelp6, marine_hab_kelp8, marine_hab_kelp7,marine_hab_kelp9,marine_hab_kelp10, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//habkelp_fish_bycatch_corrected.png")


marine_hab_2506<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_250m, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_2507<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_250m, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_2508<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_250m, y=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
marine_hab_2509<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_250m, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_25010<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_250m, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_25011<-ggplot(fish_richness_merged_tran_isl_300, aes(x=sum_250m, y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_2506, marine_hab_2508, marine_hab_25011, marine_hab_2507,marine_hab_2509,marine_hab_25010, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//hab250_fish_bycatch_corrected.png")


veg_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=bycatch_abundance_bym3))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=bycatch_richness_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=log(fish_abundance_bym3)))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=fish_richness_corrected))+geom_boxplot()+ theme(legend.position=c(0.9,0.90))+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=log(fish_biomass)))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=fish_length))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
plot_grid(veg_marine4, veg_marine5, veg_marine6, veg_marine3, veg_marine2, veg_marine1, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//resource_primary_intertidal_habitat.png")


subtidal_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=bycatch_abundance_bym3))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=bycatch_richness_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=log(fish_abundance_bym3)))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=fish_richness_corrected))+geom_boxplot()+ theme(legend.position=c(0.9,0.90))+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=log(fish_biomass)))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=fish_length))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
plot_grid(subtidal_marine4, subtidal_marine5, subtidal_marine6, subtidal_marine3, subtidal_marine2, subtidal_marine1, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_marine_var//resource_primary_subtidal_habitat.png")



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
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//d15n_marine_corrected.png", width=30, height=20, unit="cm")

s_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=log(bycatch_abundance_bym3+1)))+geom_point()+geom_smooth(method="gam")
s_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="gam")
s_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=log(fish_abundance_bym3+1)))+geom_point()+geom_smooth(method="gam")
s_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=fish_richness_corrected))+geom_point()+geom_smooth(method="gam")
s_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=log(fish_biomass_bym3+1)))+geom_point()+geom_smooth(method="gam")
s_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=fish_length))+geom_point()+geom_smooth(method="gam")
s_marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=marine_richness_corrected))+geom_point()+geom_smooth(method="gam")
s_marine8<-ggplot(fish_richness_merged_tran_isl_300, aes(y=s, x=fish_sd))+geom_point()+geom_smooth(method="gam")

plot_grid(s_marine4, s_marine5, s_marine7, s_marine3, s_marine2, s_marine1,s_marine6,s_marine8, ncol=4)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//s_marine_corrected.png", width=30, height=20, unit="cm")


# marine resources vs. n15 soil
n_plant_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
n_plant_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
n_plant_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
n_plant_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
n_plant_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
n_plant_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=fish_length))+geom_point()+geom_smooth(method="lm")
n_plant_marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n_gash, x=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")
plot_grid(n_plant_marine4, n_plant_marine5, n_plant_marine7, n_plant_marine3, n_plant_marine2, n_plant_marine1, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//d15n_plant_marine_corrected.png")




library(mgcv)
####GAM marine resources vs. n15n
n_marine_gam1<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam2<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam3<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam4<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam5<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam6<-ggplot(fish_richness_merged_tran_isl_300, aes(y=d15n, x=fish_length))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
plot_grid(n_marine_gam4, n_marine_gam5, n_marine_gam6, n_marine_gam3, n_marine_gam2, n_marine_gam1, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//d15n_marine_corrected_GAM.png")


#fish abundance
fish1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=d15n))+geom_point()+geom_smooth(method="lm")
fish2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish13<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish19<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish20<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_abundance_bym3), y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish1,fish2,fish3,fish4,fish13,fish19,fish20, ncol=4)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_abund_terrestrial.png")

#fish richness
fish5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish8<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
fish14<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish21<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish22<-ggplot(fish_richness_merged_tran_isl_300, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish8,fish5,fish6,fish7,fish14,fish21,fish22, ncol=4)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_richness_terrestrial.png")

#fish biomass
fish9<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish10<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish11<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish12<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=d15n))+geom_point()+geom_smooth(method="lm")
fish15<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish23<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish24<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(fish_biomass_bym3), y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish12, fish9,fish10,fish11,fish15,fish23,fish24, ncol=4)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_biomass_bym3_terrestrial.png")

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
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_richness_terrestrial.png", width=40, height=20, unit="cm")


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
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//fish_abundance_terrestrial.png", width=40, height=20, unit="cm")




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
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//bycatch_richness_terrestrial.png", width=40, height=20, unit="cm")




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
ggsave("C:Biodiversity idea//Plots//Transect//Resources_terr_var//bycatch_abundance_terrestrial.png", width=40, height=20, unit="cm")



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
ggsave("C:Biodiversity idea//Plots//Transect//Resources_biogeog//histogram_marine.png")



### Marine resources vs. Island area
lat_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
lat_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
lat_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
lat_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
lat_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
lat_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=fish_length))+geom_point()+geom_smooth(method="lm")
lat_marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=lat, y=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

plot_grid(lat_marine4, lat_marine5, lat_marine6, lat_marine3, lat_marine2, lat_marine1, lat_marine7, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_biogeog//Latitude_marine.png")


### Marine resources vs. Island area
A_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
A_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
A_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
A_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
A_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
A_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=fish_length))+geom_point()+geom_smooth(method="lm")
A_marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log_Area, y=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

plot_grid(A_marine4, A_marine5, A_marine6, A_marine3, A_marine2, A_marine1, A_marine7, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_biogeog//Area_marine.png", width=40, height=40, unit="cm")



###Coverage of neighbouring land mass, LOW NEighb_250 = high exposure
neib1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
neib2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
neib4<- ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
neib5<- ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
neib6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
neib7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=fish_length))+geom_point()+geom_smooth(method="lm")
neib8<-ggplot(fish_richness_merged_tran_isl_300, aes(x=Neighb_250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

plot_grid(neib1,neib2,neib4,neib5,neib6,neib7,neib8, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_biogeog//Neighb250_marine.png")

##head(fish_richness_merged_tran_isl_300)
### Marine resources vs. Island area
DN_marine1<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=bycatch_abundance_bym3))+geom_point()+geom_smooth(method="lm")
DN_marine2<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
DN_marine3<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=log(fish_abundance_bym3)))+geom_point()+geom_smooth(method="lm")
DN_marine4<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
DN_marine5<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=log(fish_biomass_bym3)))+geom_point()+geom_smooth(method="lm")
DN_marine6<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=fish_length))+geom_point()+geom_smooth(method="lm")
DN_marine7<-ggplot(fish_richness_merged_tran_isl_300, aes(x=log(Dist_Near), y=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

plot_grid(DN_marine4, DN_marine5, DN_marine6, DN_marine3, DN_marine2, DN_marine1, DN_marine7, ncol=3)
ggsave("C:Biodiversity idea//Plots//Transect//Resources_biogeog//Dist_Near_marine.png", width=40, height=40, unit="cm")





###########3
# d15n bycatch ---------------------------------------------------------------
fish_richness_merged_tran_isl_300_zscores<-fish_richness_merged_tran_isl_300
fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected<-scale(fish_richness_merged_tran_isl_300$bycatch_richness_corrected, center=TRUE, scale=TRUE)
fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected.unscaled <-fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected * attr(fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected, 'scaled:center')
fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected<-as.numeric(fish_richness_merged_tran_isl_300_zscores$bycatch_richness_corrected)
ggplot(fish_richness_merged_tran_isl_300_zscores, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")

#str(fish_richness_merged_tran_isl_300_zscores)

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
#str(fam.gam.d15n.bycatch)
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
#str(fam.gam.d15n.marinecatch)
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
ggsave("C:Biodiversity idea//Plots//Transect//d15n_bycatch.png")


# d15n fish richness ------------------------------------------------------


fish_richness_merged_tran_isl_300_zscores<-fish_richness_merged_tran_isl_300
fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected<-scale(fish_richness_merged_tran_isl_300$fish_richness_corrected, center=TRUE, scale=TRUE)
fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected.unscaled <-fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected * attr(fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected, 'scaled:center')
fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected<-as.numeric(fish_richness_merged_tran_isl_300_zscores$fish_richness_corrected)
ggplot(fish_richness_merged_tran_isl_300_zscores, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")

#str(fish_richness_merged_tran_isl_300_zscores)

qqp(fish_richness_merged_tran_isl_300_zscores$d15n)
qqp(fish_richness_merged_tran_isl_300_zscores$d15n, "lnorm")

gamma.12.fish_richness_corrected<-fitdi#str(fish_richness_merged_tran_isl_300$fish_richness_corrected+0.01, "gamma")
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
#str(fam.gam.d15n.fish)
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
ggsave("C:Biodiversity idea//Plots//Transect//d15n_fish_400.png")





# Correlation -------------------------------------------------------------

which( colnames(fish_richness_merged_tran_isl_300)=="Exp_SHZN" )
which( colnames(fish_richness_merged_tran_isl_300)=="lat" )
which( colnames(fish_richness_merged_tran_isl_300)=="size.cat2" )
which( colnames(fish_richness_merged_tran_isl_300)=="notes" )

#str(fish_richness_merged_tran_isl_300)
corr_by_isl_selected_fish<-fish_richness_merged_tran_isl_300 %>% select_if(is.numeric)
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
