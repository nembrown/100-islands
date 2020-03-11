# install.packages("lavaan", dependencies = TRUE)
# install.packages("ggm", dependencies = TRUE)

library(lavaan)
master_transect<-read.csv("C:Biodiversity idea//Output files//master_transect.csv")
View(master_transect)


# #pair down the 
sem_variables_names<-c( "unq_tran","fish_biomass_bym3_mean", "bycatch_biomass_bym3_mean", "MEAN_kparea2k", "MEAN_egarea2k",
                        "SLOPE", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope", "site_sum_by_isl", 
                        "ravens", "midden_feature_sem", "fish_feature_sem", "cult_imp_plant_richness", "d15n",
                        "otter_pres_all", "marine_invert_pres_all", "fish_all") 
                                     
master_transec_sem_subset<-master_transect[, colnames(master_transect) %in% sem_variables_names ]

str(master_transec_sem_subset)


#cor(master_transec_sem_subset, use = "complete.obs")




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
### edited march 10
N15_model_novector2<-'#latent variables as responses

            #composite
            
            human_pres <~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean  + WAVE_EXPOSURE + log_Area
            
            marine_animal_biomass_shore <~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean + ravens + otter_pres_all  + WAVE_EXPOSURE + human_pres
            
            algae_biomass_shore <~ log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + beachy_substrate + slope 
            
            
            
           # algae_biomass_shore + human_pres + marine_animal_biomass_shore ~ d15n
            
            # log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_biomass_bym3_mean
            # 
            # log_MEAN_kparea2k + log_MEAN_egarea2k ~ bycatch_biomass_bym3_mean


            #correlations
           # fish_biomass_bym3_mean ~~ bycatch_biomass_bym3_mean

            
            #latent variables measurement models
            human_pres =~ midden_feature_sem + fish_feature_sem + cult_imp_plant_richness + d15n
            algae_biomass_shore =~ log_site_sum_by_isl + seaweed_all + d15n
            marine_animal_biomass_shore =~ marine_invert_pres_all + fish_all + d15n
                                                '

fit2 <- sem(N15_model_novector2, data=master_transect,missing="ML")
summary(fit2, fit.measures=TRUE)


fit4 <- sem(N15_model_novector2, data=master_transect,estimator = "PML",missing = "available.cases",std.lv=TRUE, fixed.x=FALSE, conditional.x=FALSE, test = "none")

fit5 <- sem(N15_model_novector2, data=master_transect,missing="pairwise", std.lv=TRUE, fixed.x=FALSE, conditional.x=FALSE)


# ordered=c("fish_all", "marine_invert_pres_all","midden_feature_sem","fish_feature_sem"),

#note: otter_pres can't be ordered bc it is exogenous.... 
#only order the endogenous (i.e. arrow going into) variables

#missing="pairwise", std.lv=TRUE, fixed.x=FALSE, conditional.x=FALSE
#stdv.lvn If TRUE, the metric of each latent variable is determined by fixing their variances to 1.0. 
#If FALSE, the metric of each latent variable is determined by fixing the factor loading 
#of the first indicator to 1.0. If there are multiple groups, std.lv = TRUE and "loadings" 
#is included in the group.label argument, then only the latent variances i of the first group 
#will be fixed to 1.0, while the latent variances of other groups are set free.




summary(fit4)
varTable(fit5)
lavTables(fit4)
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

            human_pres <~ fish_biomass_bym3_mean + fish_bycatch_biomass  + WAVE_EXPOSURE + log_Area
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_biomass_bym3_mean
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_bycatch_biomass
            
            #correlations
             #fish_biomass_bym3_mean ~~ fish_bycatch_biomass

            
            #latent variables measurement models
            human_pres =~ midden_feature_sem + fish_feature_sem + cult_imp_plant_richness + d15n
                                                '

fit_human <- sem(N15_model_human, data=master_transect,  std.lv=TRUE, std.ov=TRUE, missing="pairwise")
summary(fit_human, standardize=T, rsq=T)
coef(fit_human, standardize=T )

lavaanify(N15_model_human)
standardizedsolution(fit_human)

?sem
# 
# #getting composite scores from fixing first loading to 1
# 
# comp_formula2 <- '
# human_pres <~ 0.16*fish_biomass_bym3_mean + -19*fish_bycatch_biomass  + 0.40*WAVE_EXPOSURE + -0.40*log_Area
# 
# human_pres ~ d15n
# '
# 
# comp_model2 <- sem(comp_formula2, data=master_transect, fixed.x=F)
# 
# 
# 
# cover_model <- lm(d15n ~ fish_biomass_bym3_mean + fish_bycatch_biomass  + WAVE_EXPOSURE + log_Area ,master_transect)
# 
# summary(cover_model)
