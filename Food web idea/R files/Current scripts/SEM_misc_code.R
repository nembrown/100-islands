
# Hierarchical model ------------------------------------------------------
#demo model
model <- '
        level: 1
            fw =~ y1 + y2 + y3
            fw ~ x1 + x2 + x3
        level: 2
            fb =~ y1 + y2 + y3
            fb ~ w1 + w2'
fit <- sem(model = model, data = Demo.twolevel, cluster = "cluster")
summary(fit)
semPaths(fit)

N15_model_hierarch_lvl<-'
        
        #transect level (between) only transect level stuff
        level: 1  
        
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k
        
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k
          
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.slope_degrees +  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k
          
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean 
        
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean 

        c.log_site_mean_by_tran ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.slope_degrees

        human_pres_trans ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE  + c.SLOPE_degrees 

        marine_animal_biomass_shore_trans ~ eagles + ravens + pres_otter  + human_pres_trans + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean

        c.d15n ~ a1*c.log_site_mean_by_tran + h1*human_pres_trans + o1*marine_animal_biomass_shore_trans + c.slope_degrees

        ### correlations not already accounted for in model
        pres_marine_invert ~~ c.log_bycatch_biomass_bym3_mean
        pres_fish ~~ c.log_fish_biomass_bym3_mean


        #latent variables measurement models
        human_pres_trans =~ c.distance_to_midden + c.distance_to_fish + c.cult_imp_plant_richness 
        marine_animal_biomass_shore_trans =~ pres_marine_invert + pres_fish 
        
        #island level (within)
        level: 2
        
        pres_otter ~   c.log_Area + c.PA_norml
        ravens ~   c.log_Area + c.PA_norml
        eagles ~   c.log_Area + c.PA_norml
        c.log_site_mean_by_tran ~ c.log_Area + c.PA_norml 
        c.d15n ~  c.log_Bog_area
        
        '


fit_simple_hierarch_imp <- semList(N15_model_hierarch_lvl, dataList=implist, cluster = "unq_isl")
summary(fit_simple_hierarch_imp )
#does not work. 0/20 imputed lists converged. 


fit_simple_hierarch_mice <- semList(N15_model_hierarch, dataList=imputed_transect, cluster = "unq_isl")
summary(fit_simple_hierarch_mice ) #0/21 datasets converged.
warnings()















# Piecewise fitting -------------------------------------------------------

#Algal model only

N15_model_algae_only<-'#latent variables as responses
            
            algae_biomass_shore <~ log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + beachy_substrate + slope 
            a1*algae_biomass_shore ~ d15n

            
           #latent variables measurement models
            algae_biomass_shore =~ log_site_mean_by_tran + seaweed_all'


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

            log_fish_biomass_bym3_mean + fish_bycatch_biomass + ravens  + WAVE_EXPOSURE  ~ marine_animal_biomass_shore
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ log_fish_biomass_bym3_mean
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_bycatch_biomass
            
            #correlations
            # log_fish_biomass_bym3_mean ~~ fish_bycatch_biomass
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

            human_pres <~ log_fish_biomass_bym3_mean + fish_bycatch_biomass  + WAVE_EXPOSURE + log_Area
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ log_fish_biomass_bym3_mean
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_bycatch_biomass
            
            #correlations
             #log_fish_biomass_bym3_mean ~~ fish_bycatch_biomass

            
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
# human_pres <~ 0.16*log_fish_biomass_bym3_mean + -19*fish_bycatch_biomass  + 0.40*WAVE_EXPOSURE + -0.40*log_Area
# 
# human_pres ~ d15n
# '
# 
# comp_model2 <- sem(comp_formula2, data=master_transect, fixed.x=F)
# 
# 
# 
# cover_model <- lm(d15n ~ log_fish_biomass_bym3_mean + fish_bycatch_biomass  + WAVE_EXPOSURE + log_Area ,master_transect)
# 
# summary(cover_model)







#think about centering variables later - if there are no interactions (currently how it's described, it shouldn't be a problem)
# # center variables, calculate interaction terms, ignore byproducts
# center_colmeans <- function(x) {
#     xcenter = colMeans(x)
#     x - rep(xcenter, rep.int(nrow(x), ncol(x)))
# }
# 
# colnames.to.centre<-c("log_fish_biomass_bym3_mean", "log_bycatch_biomass_bym3_mean", "MEAN_kparea2k", "MEAN_egarea2k",
#                       "SLOPE_degrees", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope_degrees", "midden_feature_sem", 
#                       "fish_feature_sem", "cult_imp_plant_richness", "d15n", "log_distance_to_any_arch", "log_distance_to_midden",
#                       "log_distance_to_fish", "PA_norml", "log_site_mean_by_tran", "log_MEAN_kparea2k", "log_MEAN_egarea2k", "log_Bog_area")
# length(colnames.to.centre)
# colnames.to.centre[1]
# new2.implist <- within(implist,{
#                                 c.log_fish_biomass_bym3_mean<-log_fish_biomass_bym3_mean - mean(log_fish_biomass_bym3_mean)
#                                 c.log_bycatch_biomass_bym3_mean<-log_bycatch_biomass_bym3_mean - mean(log_bycatch_biomass_bym3_mean)
#                                 c.MEAN_kparea2k<-MEAN_kparea2k - mean(MEAN_kparea2k)
#                                 c.MEAN_egarea2k<-MEAN_egarea2k - mean(MEAN_egarea2k)
#                                 c.SLOPE_degrees<-SLOPE_degrees - mean(SLOPE_degrees)
#                                 c.log_fish_biomass_bym3_mean<-log_fish_biomass_bym3_mean - mean(log_fish_biomass_bym3_mean)
#                                 c.log_fish_biomass_bym3_mean<-log_fish_biomass_bym3_mean - mean(log_fish_biomass_bym3_mean)
#                                 c.log_fish_biomass_bym3_mean<-log_fish_biomass_bym3_mean - mean(log_fish_biomass_bym3_mean)
# #                                 
# 
# 
# center_colmeans(implist[[1]])
# 
# new2.implist <- within(implist,{
#     M.SES <- mean(SES)
#     M.CognAbility <- mean(CognAbility)
#     C.SES <- SES - M.SES
#     C.CognAbility <- CognAbility - M.CognAbility
#     SES.CognAbility <- C.SES * C.CognAbility
# }, ignore=c("M.SES", "M.CognAbility"))
# 
# mean(colnames.to.centre)
# 
# implist[[1]]
# str(implist)
# hist(master_transec_sem_subset$d15n)
# hist(test.df$d15n)
# 
# test.df<-as.data.frame(new2.implist[1])
# head(test.df)



#plot(imp)
