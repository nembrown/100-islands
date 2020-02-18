# install.packages("lavaan", dependencies = TRUE)
# install.packages("ggm", dependencies = TRUE)

library(lavaan)
master_transect<-read.csv("C:Biodiversity idea//Output files//master_transect.csv")



# #pair down the 
sem_variables_names<-c( "fish_biomass_bym3_mean", "fish_bycatch_biomass", "MEAN_kparea2k", "MEAN_egarea2k",
                        "SLOPE", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope", "site_sum_by_isl", 
                        "ravens", "midden_feature_sem", "fish_feature_sem", "cult_imp_plant_richness", "d15n") 
                                     
master_transec_sem_subset<-master_transect[, colnames(master_transect) %in% sem_variables_names ]

cor(master_transec_sem_subset, use = "complete.obs")




N15_model<-'#latent variables as responses
            fish_biomass_bym3_mean + fish_bycatch_biomass + log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE + PA_norml + WAVE_EXPOSURE + log_Rock + slope  ~ vector_pres
            
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
            vector_pres =~ ravens + eagles + midi
            human_pres =~ midden_feature_sem + cult_imp_plant_richness + fish_feature_sem + d15n
            algae_biomass_shore =~ log_site_sum_by_isl + d15n
            marine_animal_biomass_shore =~ d15n '


fit <- sem(N15_model, data=master_transect,  missing = "ML")
summary(fit, fit.measures=TRUE)

varTable(fit)



length(na.omit(master_transect$d15n))                    

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

fit2 <- sem(N15_model_novector, data=master_transect)
summary(fit2, fit.measures=TRUE)

varTable(fit2)
parameterEstimates(fit2)

residualsCor(fit2)
nobs(fit2)
#returns the effective number of observations used whenfitting the model. In a multiple group analysis, this is the sum of all observations per group



######################## Alternative model with fish biomass as estimated ... might help with the problem of not many #s
### edited feb 14th
N15_model_novector2<-'#latent variables as responses

            #composite
            
            human_pres <~ fish_biomass_bym3_mean + fish_bycatch_biomass  + WAVE_EXPOSURE + log_Area
            
            marine_animal_biomass_shore <~ fish_biomass_bym3_mean + fish_bycatch_biomass + ravens  + WAVE_EXPOSURE
            
            algae_biomass_shore <~ log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + beachy_substrate + slope 
            
            
            
            algae_biomass_shore + human_pres + marine_animal_biomass_shore ~ d15n
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_biomass_bym3_mean
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_bycatch_biomass
            
            human_pres ~ marine_animal_biomass_shore
            
            #correlations
           # fish_biomass_bym3_mean ~~ fish_bycatch_biomass

            
            #latent variables measurement models
            human_pres =~ midden_feature_sem + fish_feature_sem + cult_imp_plant_richness + d15n
            algae_biomass_shore =~ log_site_sum_by_isl + d15n
            marine_animal_biomass_shore =~ d15n
                                                '

fit3 <- sem(N15_model_novector2, data=master_transect,  missing = "ML")
summary(fit3)
varTable(fit3)
lavTables(fit3)
print(modindices(fit3))

lavInspect(fit3, "cov.lv")

head(master_transect)


### piecewise fitting
N15_model_algae_only<-'#latent variables as responses
            
            log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + beachy_substrate + slope ~ algae_biomass_shore
            
            algae_biomass_shore ~ d15n
            
            #correlations
            # log_MEAN_kparea2k ~~ log_MEAN_egarea2k

            
            #latent variables measurement models
            algae_biomass_shore =~ log_site_sum_by_isl + d15n
                                                '

fit_algae <- sem(N15_model_algae_only, data=master_transect,  missing = "ML")
summary(fit_algae, fit.measures=TRUE)
varTable(fit_algae)
#This runs, and gives SE estimates so that is good
#The errors have to do with the variance/covariance matrix - can run "ridge" to help with this

################# marine animal biomass
N15_model_animal<-'#latent variables as responses

            fish_biomass_bym3_mean + fish_bycatch_biomass + ravens  + WAVE_EXPOSURE  ~ marine_animal_biomass_shore
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_biomass_bym3_mean
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_bycatch_biomass
            
            #correlations
            # fish_biomass_bym3_mean ~~ fish_bycatch_biomass
            # log_MEAN_kparea2k ~~ log_MEAN_egarea2k

            
            #latent variables measurement models
            marine_animal_biomass_shore =~ d15n
                                                '

fit_animal <- sem(N15_model_animal, data=master_transect,  missing = "ML")
summary(fit_animal, fit.measures=TRUE)

### This model also runs although has some problems... 
#it assumed marine nimal biomass IS 1:1 with d15n, because we have no other estimator
#same errors as above.... 


##### Human model
N15_model_human<-'#latent variables as responses

            fish_biomass_bym3_mean + fish_bycatch_biomass  + WAVE_EXPOSURE + log_Area ~ human_pres
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_biomass_bym3_mean
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_bycatch_biomass
            
            #correlations
             fish_biomass_bym3_mean ~~ fish_bycatch_biomass

            
            #latent variables measurement models
            human_pres =~ midden_feature_sem + fish_feature_sem + cult_imp_plant_richness + d15n
                                                '

fit_human <- sem(N15_model_human, data=master_transect,  std.lv=TRUE, std.ov=TRUE, missing="pairwise")
summary(fit_human, standardize=T)
coefs(fit_human, standardize=T )

lavaanify(N15_model_human)

?sem
