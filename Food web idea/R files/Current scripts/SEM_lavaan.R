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
fish_richness_merged_tran_arch_2$midden_feature_sem[is.na(fish_richness_merged_tran_arch_2$midden_feature_sem)] <- 0



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
            human_pres =~ midden_feature_sem
            algae_biomass_shore =~ log_site_sum_by_isl
            human_pres =~ d15n
            algae_biomass_shore = ~d15n
            marine_animal_biomass_shore =~ d15n '


fit <- sem(N15_model, data=fish_richness_merged_tran_arch_2,  missing = "ML")
summary(fit, fit.measures=TRUE)





length(na.omit(fish_richness_merged_tran_arch_2$d15n))                    

?varTable()


N15_model_novector<-'#latent variables as responses

            fish_biomass_bym3_mean + fish_bycatch_biomass + log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE + PA_norml + WAVE_EXPOSURE + log_Rock + slope ~ human_pres
            
            fish_biomass_bym3_mean + fish_bycatch_biomass  + ravens + eagles + SLOPE  + WAVE_EXPOSURE + log_Rock + slope ~ marine_animal_biomass_shore
            
            log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + log_Rock + slope ~ algae_biomass_shore
            
            algae_biomass_shore + human_pres + marine_animal_biomass_shore ~ d15n
            
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
            human_pres =~ midden_feature_sem
            algae_biomass_shore =~ log_site_sum_by_isl
            human_pres =~ d15n
            algae_biomass_shore = ~d15n
            marine_animal_biomass_shore =~ d15n '

fit2 <- sem(N15_model_novector, data=fish_richness_merged_tran_arch_2)
summary(fit2, fit.measures=TRUE)

varTable(fit2)
parameterEstimates(fit2)

residualsCor(fit2)
nobs(fit2)
#returns the effective number of observations used whenfitting the model. In a multiple group analysis, this is the sum of all observations per group



######################## Alternative model with fish biomass as estimated ... might help with the problem of not many #s

N15_model_novector2<-'#latent variables as responses

            true_fish_biomass + true_invert_biomass  + SLOPE + PA_norml + WAVE_EXPOSURE + log_Rock + slope ~ human_pres
            
            true_fish_biomass + true_invert_biomass  + ravens + eagles + SLOPE  + WAVE_EXPOSURE + log_Rock + slope ~ marine_animal_biomass_shore
            
            log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + log_Rock + slope ~ algae_biomass_shore
            
            algae_biomass_shore + human_pres + marine_animal_biomass_shore ~ d15n
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ true_fish_biomass 
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ true_invert_biomass 
            
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
            human_pres =~ midden_feature_sem
            algae_biomass_shore =~ log_site_sum_by_isl
            human_pres =~ d15n
            algae_biomass_shore = ~d15n
            marine_animal_biomass_shore =~ d15n
            true_fish_biomass =~ fish_biomass_bym3_mean
            true_invert_biomass =~ fish_bycatch_biomass '

fit3 <- sem(N15_model_novector2, data=fish_richness_merged_tran_arch_2)
summary(fit3, fit.measures=TRUE)
varTable(fit3)
