install.packages("lavaan", dependencies = TRUE)
install.packages("ggm", dependencies = TRUE)

library(lavaan)

head(fish_richness_merged_tran_arch)

#pair down the 
fish_richness_merged_isl_colnames<-c( "fish_biomass_bym3_mean", "fish_bycatch_biomass", "MEAN_kparea2k", "MEAN_egarea2k",
                                     "SLOPE", "PA_norml", "WAVE_EXPOSURE", "Rock", "slope", "site_sum_by_isl", "ravens", "eagles") 
                                     
View(fish_richness_merged_tran_arch_2)  
#site_sum_by_isl, MEAN_kparea, MEAN_egarea need to be logged

fish_richness_merged_tran_arch_2$log_site_sum_by_isl <- log(fish_richness_merged_tran_arch_2$site_sum_by_isl+1)
fish_richness_merged_tran_arch_2$log_MEAN_kparea2k <- log(fish_richness_merged_tran_arch_2$MEAN_kparea2k+1)
fish_richness_merged_tran_arch_2$log_MEAN_egarea2k <- log(fish_richness_merged_tran_arch_2$MEAN_egarea2k+1)
fish_richness_merged_tran_arch_2$log_Rock<- log(fish_richness_merged_tran_arch_2$Rock+1)



N15_model<-'#latent variables as responses
            fish_biomass_bym3_mean + fish_bycatch_biomass + log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE + PA_norml + WAVE_EXPOSURE + log_Rock + slope ~ vector_pres
            
            fish_biomass_bym3_mean + fish_bycatch_biomass + log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE + PA_norml + WAVE_EXPOSURE + log_Rock + slope ~ human_pres
            
            fish_biomass_bym3_mean + fish_bycatch_biomass  + vector_pres + SLOPE  + WAVE_EXPOSURE + log_Rock + slope ~ marine_animal_biomass_shore
            
            log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + log_Rock + slope ~ algae_biomass_shore
            
            #correlations
            fish_biomass_bym3_mean ~~ fish_bycatch_biomass
            fish_biomass_bym3_mean ~~ log_MEAN_kparea2k
            fish_biomass_bym3_mean ~~ log_MEAN_egarea2k
            fish_bycatch_biomass ~~ log_MEAN_kparea2k
            fish_bycatch_biomass ~~ log_MEAN_egarea2k
            log_MEAN_kparea2k ~~ log_MEAN_egarea2k
            
            SLOPE ~~ PA_norml
            SLOPE ~~ WAVE_EXPOSURE
            SLOPE ~~ log_Rock
            SLOPE ~~ slope
            PA_norml ~~ WAVE_EXPOSURE
            PA_norml ~~ log_Rock
            PA_norml ~~ slope
            WAVE_EXPOSURE ~~ log_Rock
            WAVE_EXPOSURE ~~ slope
            log_Rock ~~ slope

            
            #latent variables measurement models
            vector_pres =~ ravens + eagles
            human_pres =~ midden_feature
            algae_biomass_shore =~ log_site_sum_by_isl
            human_pres =~ d15n
            algae_biomass_shore = ~d15n
            marine_animal_biomass_shore =~ d15n '


fit <- sem(N15_model, data=fish_richness_merged_tran_arch_2,  missing = "ML", se = "bootstrap")
summary(fit, fit.measures=TRUE)


varTable(fit)
parameterEstimates(fit)

var(fish_richness_merged_tran_arch_2$slope, na.rm=TRUE)


length(na.omit(fish_richness_merged_tran_arch_2$d15n))                    

?varTable()


N15_model_novector<-'#latent variables as responses

            fish_biomass_bym3_mean + fish_bycatch_biomass + log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE + PA_norml + WAVE_EXPOSURE + log_Rock + slope ~ human_pres
            
            fish_biomass_bym3_mean + fish_bycatch_biomass  + ravens + eagles + SLOPE  + WAVE_EXPOSURE + log_Rock + slope ~ marine_animal_biomass_shore
            
            log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + log_Rock + slope ~ algae_biomass_shore
            
            #correlations
            fish_biomass_bym3_mean ~~ fish_bycatch_biomass
            fish_biomass_bym3_mean ~~ log_MEAN_kparea2k
            fish_biomass_bym3_mean ~~ log_MEAN_egarea2k
            fish_bycatch_biomass ~~ log_MEAN_kparea2k
            fish_bycatch_biomass ~~ log_MEAN_egarea2k
            log_MEAN_kparea2k ~~ log_MEAN_egarea2k

            SLOPE ~~ PA_norml
            SLOPE ~~ WAVE_EXPOSURE
            SLOPE ~~ log_Rock
            SLOPE ~~ slope
            PA_norml ~~ WAVE_EXPOSURE
            PA_norml ~~ log_Rock
            PA_norml ~~ slope
            WAVE_EXPOSURE ~~ log_Rock
            WAVE_EXPOSURE ~~ slope
            log_Rock ~~ slope

            
            #latent variables measurement models
            human_pres =~ midden_feature
            algae_biomass_shore =~ log_site_sum_by_isl
            human_pres =~ d15n
            algae_biomass_shore = ~d15n
            marine_animal_biomass_shore =~ d15n '

fit2 <- sem(N15_model_novector, data=fish_richness_merged_tran_arch_2)
summary(fit2, fit.measures=TRUE)

residualsCor(fit2)
