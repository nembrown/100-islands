#new script to mirror SEM_lavaan but trying multiple imputation to fix the missingness problem. 

# load libraries and data -------------------------------------------------------
library(semTools)
library(mitml)
library(lavaan)
library(lavaan.survey)
library(semPlot)
library(MuMIn)
library(mice)
library(ggplot2)
library(mitools)
library(Amelia)

master_transect<-read.csv("C:Biodiversity idea//Output files//master_transect.csv")

## pair down the variables
sem_variables_names<-c("node", "unq_tran","unq_isl", "log_fish_biomass_bym3_mean", "log_bycatch_biomass_bym3_mean",
                       "SLOPE_degrees", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope_degrees",
                       "ravens", "cult_imp_plant_richness", "d15n", "distance_to_midden",
                       "distance_to_fish", "PA_norml", "log_site_mean_by_tran", "log_MEAN_kparea2k", "log_MEAN_egarea2k", "pres_otter", 
                       "pres_marine_invert", "pres_fish", "eagles", "log_Bog_area", "northing", "easting")

master_transec_sem_subset<-master_transect[, colnames(master_transect) %in% sem_variables_names]
master_transec_sem_subset$unq_tran<-factor(master_transec_sem_subset$unq_tran, ordered=TRUE)
master_transec_sem_subset$unq_isl<-factor(master_transec_sem_subset$unq_isl, ordered=TRUE)
master_transec_sem_subset_centered <- stdize(master_transec_sem_subset, 
                                             omit.cols = c("node", "unq_tran","unq_isl",  "beachy_substrate", "ravens",  "pres_otter",  
                                                           "pres_marine_invert", "pres_fish", "eagles", "northing", "easting"), 
                                             center = TRUE, scale = FALSE)
View(master_transec_sem_subset_centered)

master_transec_sem_subset_centered<-master_transec_sem_subset_centered[complete.cases(master_transec_sem_subset_centered$pres_otter), ]

master_transec_sem_subset_centered$c.log_Bog_area[is.na(master_transec_sem_subset_centered$c.log_Bog_area)] <- -1.042483
# master_transec_sem_subset_centered<-master_transec_sem_subset_centered[complete.cases(master_transec_sem_subset_centered$c.log_Bog_area), ]

# master_transec_sem_subset$beachy_substrate<-factor(master_transec_sem_subset$beachy_substrate, ordered=TRUE)
# master_transec_sem_subset$ravens<-factor(master_transec_sem_subset$ravens, ordered=TRUE)
# master_transec_sem_subset$eagles<-factor(master_transec_sem_subset$eagles, ordered=TRUE)
# master_transec_sem_subset$pres_marine_invert<-factor(master_transec_sem_subset$pres_marine_invert, ordered=TRUE)
# master_transec_sem_subset$pres_otter<-factor(master_transec_sem_subset$pres_otter, ordered=TRUE)
# master_transec_sem_subset$pres_fish<-factor(master_transec_sem_subset$pres_fish, ordered=TRUE)

summary(master_transec_sem_subset_centered)



#Multiple imputation using mimtl packacge which is two levels
fml <- list( c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean +
               c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.slope_degrees + c.log_site_mean_by_tran + c.cult_imp_plant_richness + c.d15n + c.distance_to_midden +
               c.distance_to_fish + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + pres_otter +
               pres_marine_invert + pres_fish  ~ 1 + (1|unq_isl) ,                                                 # Level 1
               c.log_Bog_area + c.log_Area +  c.PA_norml + ravens + eagles  ~ 1 )                                        # Level 2

imp <- jomoImpute(master_transec_sem_subset_centered, formula=fml, n.burn=5000, n.iter=250, m=20)
summary(imp)
#problems the model sees: 
plot(imp, trace="all", print="beta2", pos=c(1,1))
plot(imp, trace="all", print="beta", pos=c(1,11))
plot(imp, trace="all", print="psi", pos=c(6,6))
plot(imp, trace="all", print="sigma", pos=c(8,6))

implist <- mitmlComplete(imp, "all")

View(implist[[1]])
varTable((implist[[1]]))
#alternate way to do mult imp, which is single level.  

md.pattern(master_transec_sem_subset_centered)
imputed_transect <- mice(master_transec_sem_subset_centered, method='cart')

imputed_mitools <- imputationList(implist)


# Simple non-hierarchical model  -------------------------------------------------------
#Could combine this with survey.design or lavSpatialcorrect to get hierachical model .... but survey deisgn is more for pyschology, diffucult to specify

N15_model_simple_centered<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k
        
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k
          
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml+ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k
          
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml
        
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml

        c.log_site_mean_by_tran ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees

        human_pres ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.SLOPE_degrees + c.PA_norml

        marine_animal_biomass_shore ~ eagles + ravens + pres_otter  + human_pres + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean

        c.d15n ~ a1*c.log_site_mean_by_tran + h1*human_pres + o1*marine_animal_biomass_shore + c.slope_degrees + c.log_Bog_area

        ### correlations not already accounted for in model
        pres_marine_invert ~~ c.log_bycatch_biomass_bym3_mean
        pres_fish ~~ c.log_fish_biomass_bym3_mean


        #latent variables measurement models
        human_pres =~ c.distance_to_midden + c.distance_to_fish + c.cult_imp_plant_richness 
        marine_animal_biomass_shore =~ pres_marine_invert + pres_fish 
        '

fit_simple_centered_imp <- semList(N15_model_simple_centered, dataList=implist, std.lv=TRUE)
#this did not work. Solution not found ... only 5 converged, no std errors
summary(fit_simple_centered_imp)

#with mice
fit_simple_centered_mice <- semList(N15_model_simple_centered, dataList=imputed_transect, std.lv=TRUE)
summary(fit_simple_centered_mice) #0 converged

#With mice - but imputation happens in the model
runMI(N15_model_simple_centered, master_transec_sem_subset_centered, fun = "lavaan",m=20,  miArgs= list(method='cart'),
      miPackage = "mice", seed = 12345)

master_transec_sem_subset_centered_simp <- master_transec_sem_subset_centered %>% dplyr::select(-c("unq_tran", "unq_isl", "node"))

#With Amelia - but imputation happens in the model - this works with sem call instead of lavaan call
fit.mi<-runMI(N15_model_simple_centered, master_transec_sem_subset_centered, fun = "sem",m=20, miArgs=list(idvars = c("unq_tran", "unq_isl", "node")),
      miPackage = "Amelia", seed = 12345)
summary(fit.mi)

#
library(Amelia)
set.seed(12345)
HS.amelia <- amelia(master_transec_sem_subset_centered, m = 20, idvars = c("unq_tran", "unq_isl", "node"), p2s = FALSE)
imps <- HS.amelia$imputations



lavaan_fit_model<-sem(N15_model_simple_centered, meanstructure = TRUE, data=master_transec_sem_subset_centered)


# out2 <- runMI(N15_model_simple_centered, data = imps, fun="sem")
# out2

str(imps_2)
imps_2<-imputationList(imps)


# Survey.design -----------------------------------------------------------

#This doesn't work since implist can't go into survey design ... need another imputation method. 
#Survey design doesn't seem to work with mitools either 
design_imp_amelia<-svydesign(ids=~unq_isl, strata=~node, data=imps_2)
design_imp_mitml<-svydesign(ids=~unq_isl, strata=~node, data=imputed_mitools)

fit.adj<-lavaan.survey(lavaan.fit=lavaan_fit_model, survey.design = design_imp_mitml, estimator="MLM")
summary(fit.adj, standardized=T)
parameterEstimates(fit.adj)

pval.pFsum(fit.adj, survey.design = design_imp)


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
