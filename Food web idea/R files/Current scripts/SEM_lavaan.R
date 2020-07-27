#SEM using FIML method to deal with missing values. 

# load libraries and organize data -------------------------------------------------------
library(semTools)
library(mitml)
library(lavaan)
library(lavaan.survey)
library(semPlot)
library(MuMIn)
library(mice)
library(ggplot2)
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
head(master_transec_sem_subset_centered)

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

fit_simple_centered <- sem(N15_model_simple_centered, data=master_transec_sem_subset_centered, std.lv=TRUE, missing="ml.x")


# Survey.design -----------------------------------------------------------

#This doesn't work. model doesn't converge. 
design<-svydesign(ids=~unq_tran, strata=~unq_isl, data=master_transec_sem_subset_centered)
fit.adj<-lavaan.survey(lavaan.fit=fit_simple_centered, survey.design = design, estimator="ML")
summary(fit.adj, standardized=T)




# lavSpatialCorrect -------------------------------------------------------
#doesn't work b/c of missing values
source("C:Food web idea//R files//Current scripts//lavSpatialCorrectfunction.R")
lavSpatialCorrect(fit_simple_centered, master_transec_sem_subset_centered$northing, master_transec_sem_subset_centered$easting) 

#Do I need to correct for spatial autocorrelation? Yes. 
borRes <- as.data.frame(residuals(fit_simple_nocat , "casewise"))

#raw visualization of NDVI residuals
qplot(northing, easting, data=master_transec_sem_subset_centered, color=borRes$c.d15n, size=I(5)) +
    theme_bw(base_size=17) + 
    scale_color_gradient("d15N Residual", low="blue", high="yellow")



# Hierarchical model ------------------------------------------------------
N15_model_hierarch<-'
        
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


fit_simple_hierarch <- sem(N15_model_hierarch, data=master_transec_sem_subset_centered, cluster = "node", missing="FIML")
#doesn't converge b/c data needs to be complete, so doing listwise deletion




