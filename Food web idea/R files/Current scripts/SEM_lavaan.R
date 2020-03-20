
# load libraries and data -------------------------------------------------------
library(lavaan)
master_transect<-read.csv("C:Biodiversity idea//Output files//master_transect.csv")
head(master_transect)

# # pair down the variables
sem_variables_names<-c( "unq_tran","fish_biomass_bym3_mean", "bycatch_biomass_bym3_mean", "MEAN_kparea2k", "MEAN_egarea2k",
                        "SLOPE", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope", "site_sum_by_isl",
                        "ravens", "midden_feature_sem", "fish_feature_sem", "cult_imp_plant_richness", "d15n",
                        "otter_pres_all", "marine_invert_pres_all", "fish_all", "distance_to_any_arch", "distance_to_midden",
                        "distance_to_fish")

master_transec_sem_subset<-master_transect[, colnames(master_transect) %in% sem_variables_names]
str(master_transec_sem_subset)


# Simple non-categorical model  -------------------------------------------------------

N15_model_simple_nocat<-'
          
          human_pres ~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean  + WAVE_EXPOSURE + log_Area

          marine_animal_biomass_shore ~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean + ravens + otter_pres_all  + WAVE_EXPOSURE + human_pres

          algae_biomass_shore ~ log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + beachy_substrate
          
          d15n ~ a1*algae_biomass_shore + h1*human_pres + o1*marine_animal_biomass_shore

          log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_biomass_bym3_mean

          log_MEAN_kparea2k + log_MEAN_egarea2k ~ bycatch_biomass_bym3_mean


        #latent variables measurement models
        human_pres =~ distance_to_midden + distance_to_fish + cult_imp_plant_richness
        algae_biomass_shore =~ log_site_sum_by_isl + seaweed_all
        marine_animal_biomass_shore =~ marine_invert_pres_all + fish_all
        '

fit_simple_nocat <- sem(N15_model_simple_nocat, data=master_transect, missing="ML", std.lv=TRUE)
summary(fit_simple_nocat, fit.measures=TRUE)
varTable(fit_simple_nocat)






# Simple full model -------------------------------------------------------

N15_model_simple<-'
          
          human_pres ~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean  + WAVE_EXPOSURE + log_Area

          marine_animal_biomass_shore ~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean + ravens + otter_pres_all  + WAVE_EXPOSURE + human_pres

          algae_biomass_shore ~ log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + beachy_substrate
          
          d15n ~ a1*algae_biomass_shore + h1*human_pres + o1*marine_animal_biomass_shore

          log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_biomass_bym3_mean

          log_MEAN_kparea2k + log_MEAN_egarea2k ~ bycatch_biomass_bym3_mean


        #latent variables measurement models
        human_pres =~ midden_feature_sem + fish_feature_sem + cult_imp_plant_richness
        algae_biomass_shore =~ log_site_sum_by_isl + seaweed_all
        marine_animal_biomass_shore =~ marine_invert_pres_all + fish_all
        '

fit_simple <- sem(N15_model_simple, data=master_transect, missing="pairwise", fixed.x=FALSE, conditional.x=FALSE)
summary(fit_simple, fit.measures=TRUE)
varTable(fit_simple)


######################## 
N15_model_composite<-'#latent variables as responses

            #composite
            
            human_pres <~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean  + WAVE_EXPOSURE + log_Area
            
            marine_animal_biomass_shore <~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean + ravens + otter_pres_all  + WAVE_EXPOSURE + human_pres
            
            algae_biomass_shore <~ log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + beachy_substrate
            
            
            
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

fit5 <- sem(N15_model_composite, data=master_transect,missing="pairwise", std.lv=TRUE)
summary(fit5)


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



# Piecewise fitting -------------------------------------------------------

#Algal model only

N15_model_algae_only<-'#latent variables as responses
            
            algae_biomass_shore <~ log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + beachy_substrate + slope 
            a1*algae_biomass_shore ~ d15n

            
           #latent variables measurement models
            algae_biomass_shore =~ log_site_sum_by_isl + seaweed_all'
                                

fit_algae <- sem(N15_model_algae_only, data=master_transect)
summary(fit_algae, fit.measures=TRUE)
varTable(fit_algae)

# fit_algae_PML<-sem(N15_model_algae_only, data=master_transect,estimator = "PML",missing = "available.cases",std.lv=TRUE, fixed.x=FALSE, conditional.x=FALSE, test = "none")
# summary(fit_algae_PML, fit.measures=TRUE)

fit_algae_pairwise <- sem(N15_model_algae_only, data=master_transect,missing="pairwise", std.lv=TRUE)
summary(fit_algae_pairwise, fit.measures=TRUE)


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
