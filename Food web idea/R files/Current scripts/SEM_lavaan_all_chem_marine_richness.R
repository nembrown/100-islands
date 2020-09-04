#new script to mirror SEM_lavaan but trying multiple imputation to fix the missingness problem. 

# load libraries and organize data -------------------------------------------------------
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
library(lavaanPlot)
library(tidySEM)

master_transect<-read.csv("C:Biodiversity idea//Output files//master_transect.csv")

head(master_transect)


## pair down the variables
sem_variables_names_veg_allchem<-c("node", "unq_tran","unq_isl", "log_fish_biomass_bym3_mean", "log_bycatch_biomass_bym3_mean",
                           "SLOPE_degrees", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope_degrees",
                           "ravens",  "d15n", "distance_to_midden",
                           "PA_norml", "log_site_mean_by_tran", "log_MEAN_kparea2k", "log_MEAN_egarea2k", "pres_otter", 
                           "pres_marine_invert", "pres_fish", "eagles", "log_Bog_area", "log_Dist_Near", "log_MEAN_rockarea2000" ,"elevation_max", 
                           "slope_isl", "CHM_mean_height", "habitat_het", "NDVI_mean", "d34s", "s", "n", "plant_richness")



master_transec_sem_subset_veg_allchem<-master_transect[, colnames(master_transect) %in% sem_variables_names_veg_allchem]
master_transec_sem_subset_veg_allchem$unq_tran<-factor(master_transec_sem_subset_veg_allchem$unq_tran, ordered=TRUE)
master_transec_sem_subset_veg_allchem$unq_isl<-factor(master_transec_sem_subset_veg_allchem$unq_isl, ordered=TRUE)
master_transec_sem_subset_veg_allchem$node<-factor(master_transec_sem_subset_veg_allchem$node)

master_transec_sem_subset_veg_allchem_centered <- stdize(master_transec_sem_subset_veg_allchem, 
                                                 omit.cols = c("node", "unq_tran","unq_isl",  "beachy_substrate", "ravens",  "pres_otter",  
                                                               "pres_marine_invert", "pres_fish", "eagles", "northing", "easting", "elevation_max"), 
                                                 center = TRUE, scale = FALSE)

head(master_transec_sem_subset_veg_allchem_centered)

#see structure of the data, which variables have missing data
summary(master_transec_sem_subset_veg_allchem_centered)
md.pattern(master_transec_sem_subset_veg_allchem_centered)


# Simple non-hierarchical model  -------------------------------------------------------
#Will combine this with survey.design to get hierachical model


N15_model_simple_centered_alt_veg_allchem<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
          
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE
          
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height

        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000

        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 

        pres_marine_invert ~ eagles + ravens + pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden

        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden

        c.d15n ~  w1*c.log_site_mean_by_tran + h1*c.distance_to_midden + pres_marine_invert + pres_fish + c.slope_degrees  + c.WAVE_EXPOSURE + pres_otter + ravens + eagles + elevation_max + c.slope_isl  + c.log_Bog_area + c.CHM_mean_height

        c.d34s ~ w1*c.log_site_mean_by_tran + h1*c.distance_to_midden + pres_marine_invert + pres_fish + c.slope_degrees  + c.WAVE_EXPOSURE + pres_otter + ravens + eagles + elevation_max + c.slope_isl  + c.log_Bog_area + c.CHM_mean_height
        
        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 

        c.plant_richness ~ pres_otter + c.d34s + c.d15n + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.slope_degrees'



# Multiple imputation -----------------------------------------------------
# We need to use multiple imputation because the methods to understand our multi-level data dont accept missing data


# Option 1 mitml ----------------------------------------------------------
#OPTION 1 Multiple imputation using mimtl packacge which is two levels - 
#this is important for the log_Bog_area variable whic is at level 2 (island level) but has missing values) 

#this is run the first time, but can also load it
fml.veg_allchem <- list( c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean +
                   c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.slope_degrees + c.log_site_mean_by_tran  +
                   c.d15n + c.distance_to_midden + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + pres_otter +
                   pres_marine_invert + pres_fish + c.log_MEAN_rockarea2000 + c.s + c.d34s + c.n  ~ 1 + (1|unq_isl) ,                                                 # Level 1
                 c.log_Bog_area + c.log_Area +  c.PA_norml + ravens + eagles + c.log_Dist_Near + elevation_max + c.slope_isl  + c.CHM_mean_height + c.habitat_het + c.NDVI_mean ~ 1 )                          # Level 2


imp.veg_allchem <- jomoImpute(master_transec_sem_subset_veg_allchem_centered, formula=fml.veg_allchem, n.burn=500, n.iter=1000, m=20)
summary(imp.veg_allchem)
write.mitml(imp.veg_allchem, "C:Food web idea//Data by person//Norah.data/imp.veg_allchem")

#load it
imp.veg_allchem <- read.mitml("C:Food web idea//Data by person//Norah.data/imp.veg_allchem")

#problems the model sees:
plot(imp.veg_allchem, trace="all", print="beta2", pos=c(1,7))
plot(imp.veg_allchem, trace="all", print="beta", pos=c(1,11))
plot(imp.veg_allchem, trace="all", print="psi", pos=c(17,6))
plot(imp.veg_allchem, trace="all", print="sigma", pos=c(4,2))

implist_veg_allchem <- mitmlComplete(imp.veg_allchem, "all")
#get it to talk to survey.design
imputed_mitml_veg_allchem <- imputationList(implist_veg_allchem)


# Option 2 Amelia ---------------------------------------------------------
set.seed(12345)
HS.amelia_veg_allchem <- amelia(master_transec_sem_subset_veg_allchem_centered, m = 20, idvars = c("unq_tran", "unq_isl", "node"), p2s = FALSE)
imps_veg_allchem <- HS.amelia_veg_allchem$imputations
imps_amelia_veg_allchem<-imputationList(imps_veg_allchem)

# Running model and adding survey design ----------------------------------

#run basic empty model then update with survey design
lavaan_fit_model_veg_allchem<-sem(N15_model_simple_centered_alt_veg_allchem, data=master_transec_sem_subset_veg_allchem_centered)
summary(lavaan_fit_model_veg_allchem)

varTable(lavaan_fit_model_veg_allchem)

# Survey.design with mitml and amelia - mice wasn't working 
design_imp_amelia_veg_allchem<-svydesign(ids=~unq_isl, strata=~node, data=imps_amelia_veg_allchem)
design_imp_mitml_veg_allchem<-svydesign(ids=~unq_isl, strata=~node, data=imputed_mitml_veg_allchem)

fit.adj.amelia.veg_allchem<-lavaan.survey(lavaan.fit=lavaan_fit_model_veg_allchem, survey.design = design_imp_amelia_veg_allchem, estimator="MLMVS")
summary(fit.adj.amelia.veg_allchem, standardized=T)

fit.adj.mitml.veg_allchem<-lavaan.survey(lavaan.fit=lavaan_fit_model_veg_allchem, survey.design = design_imp_mitml_veg_allchem, estimator="MLMVS")
 summary(fit.adj.mitml.veg_allchem, standardized=T)

 options(max.print=1000000)
#if model Sign can look at these: 
#plyr::arrange(modificationIndices(fit.adj.amelia.veg_allchem),mi, decreasing=TRUE)
#######

#semplot
grps_allchem<-list(Algae=c("c.log_MEAN_rockarea2000","c.log_site_mean_by_tran", "c.log_MEAN_kparea2k", "c.log_MEAN_egarea2k" ),
           Islchar=c("c.PA_norml", "c.SLOPE_degrees", "c.log_Area", "c.WAVE_EXPOSURE", "beachy_substrate", "c.slope_degrees","c.log_Bog_area", "c.log_Dist_Near", "elevation_max",   "c.slope_isl", "c.CHM_mean_height", "c.NDVI_mean"),
           Animalvect=c("marine_animal_biomass_shore","pres_otter", "pres_marine_invert", "pres_fish", "eagles","ravens" ),
           Humans=c("c.distance_to_midden"),
           Fish=c("c.log_fish_biomass_bym3_mean", "c.log_bycatch_biomass_bym3_mean"),
           outcome=c("c.d15n", "c.d34s", "plant_richness"))

colour_group=c("#51C5B4", "#C2B3A2", "#EC6D98", "#DEAFFD", "#00A4EE", "#CD8958")

nodelab_veg_allchem<-c("fish", "marine\ninvert","otter", "ravens", "eagles" ,"wrack", "midden","shell", 
                       "fish\nbones", "d15N\nsoil", "d34S\nsoil", "canopy\nheight","plant\nrichness", 
               "kelp", "eelgrass", "fucus", "sandy", "Area", "transect\nslope", "wiggly", "neighb","beach\nslope",  "wave\nexposure","elevation", 
               "island\nslope","Bog\narea","NDVI")


lay_names_veg_allchem<-get_layout(
                          "", "", "", "", "", "","","","","","","","","","","","","","","","","","","","","","","","","","","","", "","", "","","","", "","","","","", "","","","","", "plant\nrichness", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                          "","","","","","","","","","","","","","","","","","","","","","","","", "","", "","","","", "","","","d15N\nsoil",  "","","", "", "", "", "", "","","","", "","","","","","","","","","","","","","","","","","","","d34S\nsoil","","","","","","","","","","","","","","","","",
                          "wrack","","", "","","","","", "","","","","","", "","", "shell","","","","","","", "fish\nbones", "","", "","","","","","","","","","","","","","","","", "","","","","", "","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                          "","", "","","","","","","","","","", "eagles","","","","","","","ravens" ,"","","","","","","otter","","","","","","","","","","","","","","","","","","", "midden","","","","","","","","","","","","","","","","","","","", "","","","","","","","","","","","","","","","",
                          "","","","","","","","","","","","","","","marine\ninvert","","","","","","","","","","", "fish","","","","","","","","","","","", "","","","","","","","","","","","","","","","","","","","","","","canopy\nheight","","","","","","","","","","","","","","","","","","","","","",
                          "fucus","","","", "","","kelp","","","", "","","eelgrass","","","","","","","beach\nslope","","","","","","wave\nexposure","","","","", "sandy","","","","","","","", "transect\nslope","","","","","","NDVI","","","","","","wiggly","","","","","","Area","","","", "","","Bog\narea","","","","","", "neighb","","","","","", "elevation", "","","","","", "island\nslope", rows=6)

semPaths(fit.adj.mitml.veg_allchem, what="std",  layout=lay_names_veg_allchem, intercepts=FALSE, residuals=FALSE,
         groups=grps, exoVar = FALSE,  color=colour_group, esize=2, nodeLabels = nodelab_veg_allchem, legend=FALSE)

semPaths(fit.adj.mitml.veg_allchem, what="path",  layout=lay_names_veg_allchem, intercepts=FALSE, residuals=FALSE,
         groups=grps, exoVar = FALSE, color=colour_group, nodeLabels = nodelab_veg_allchem, legend=FALSE)


semPlot::semPaths(fit.adj.mitml.veg_allchem, "path",fade = F, residuals = F, intercepts=FALSE, label.cex=2, nCharNodes = 0, nodeLabels = 1:27)
semPlot::semPaths(fit.adj.mitml.veg_allchem, "path",fade = F, residuals = F, intercepts=FALSE, label.cex=2, nCharNodes = 0)

