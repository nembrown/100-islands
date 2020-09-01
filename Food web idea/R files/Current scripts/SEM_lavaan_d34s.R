

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
sem_variables_names_s<-c("node", "unq_tran","unq_isl", "log_fish_biomass_bym3_mean", "log_bycatch_biomass_bym3_mean",
                           "SLOPE_degrees", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope_degrees",
                           "ravens",  "d34s", "distance_to_midden",
                           "PA_norml", "log_site_mean_by_tran", "log_MEAN_kparea2k", "log_MEAN_egarea2k", "pres_otter", 
                           "pres_marine_invert", "pres_fish", "eagles", "log_Bog_area", "log_Dist_Near", "log_MEAN_rockarea2000" ,"elevation_max", 
                           "slope_isl", "CHM_mean_height", "habitat_het", "NDVI_mean")



master_transec_sem_subset_s<-master_transect[, colnames(master_transect) %in% sem_variables_names_s]
master_transec_sem_subset_s$unq_tran<-factor(master_transec_sem_subset_s$unq_tran, ordered=TRUE)
master_transec_sem_subset_s$unq_isl<-factor(master_transec_sem_subset_s$unq_isl, ordered=TRUE)
master_transec_sem_subset_s$node<-factor(master_transec_sem_subset_s$node)

master_transec_sem_subset_s_centered <- stdize(master_transec_sem_subset_s, 
                                                 omit.cols = c("node", "unq_tran","unq_isl",  "beachy_substrate", "ravens",  "pres_otter",  
                                                               "pres_marine_invert", "pres_fish", "eagles", "northing", "easting", "elevation_max"), 
                                                 center = TRUE, scale = FALSE)

head(master_transec_sem_subset_s_centered)

#see structure of the data, which variables have missing data
summary(master_transec_sem_subset_s_centered)
md.pattern(master_transec_sem_subset_s_centered)


# Simple non-hierarchical model  -------------------------------------------------------
#Will combine this with survey.design to get hierachical model


S34_model_simple_centered_alt_s<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
          
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE
          
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height

        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000

        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 

        pres_marine_invert ~ eagles + ravens + pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden

        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden

        c.d34s ~ w1*c.log_site_mean_by_tran + h1*c.distance_to_midden + pres_marine_invert + pres_fish + c.slope_degrees  + c.WAVE_EXPOSURE + pres_otter + ravens + eagles + elevation_max + c.slope_isl  + c.log_Bog_area + c.CHM_mean_height

        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 

'
# Multiple imputation -----------------------------------------------------
# We need to use multiple imputation because the methods to understand our multi-level data dont accept missing data


# Option 1 mitml ----------------------------------------------------------
#OPTION 1 Multiple imputation using mimtl packacge which is two levels - 
#this is important for the log_Bog_area variable whic is at level 2 (island level) but has missing values) 

#this is run the first time, but can also load it 
fml.s <- list( c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean +
                   c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.slope_degrees + c.log_site_mean_by_tran  + 
                   c.d34s + c.distance_to_midden + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + pres_otter +
                   pres_marine_invert + pres_fish + c.log_MEAN_rockarea2000  ~ 1 + (1|unq_isl) ,                                                 # Level 1
                 c.log_Bog_area + c.log_Area +  c.PA_norml + ravens + eagles + c.log_Dist_Near + elevation_max + c.slope_isl  + c.CHM_mean_height + c.habitat_het + c.NDVI_mean ~ 1 )                          # Level 2


imp.s <- jomoImpute(master_transec_sem_subset_s_centered, formula=fml.s, n.burn=500, n.iter=1000, m=20)
summary(imp.s)
write.mitml(imp.s, "C:Food web idea//Data by person//Norah.data/imp.s")

#load it 
imp.s <- read.mitml("C:Food web idea//Data by person//Norah.data/imp.s")

#problems the model sees: 
plot(imp.s, trace="all", print="beta2", pos=c(1,7))
plot(imp.s, trace="all", print="beta", pos=c(1,1))
plot(imp.s, trace="all", print="psi", pos=c(17,6))
plot(imp.s, trace="all", print="sigma", pos=c(4,2))

implist_s <- mitmlComplete(imp.s, "all")
#get it to talk to survey.design
imputed_mitml_s <- imputationList(implist_s)


# Option 2 Amelia ---------------------------------------------------------
set.seed(12345)
HS.amelia_s <- amelia(master_transec_sem_subset_s_centered, m = 20, idvars = c("unq_tran", "unq_isl", "node"), p2s = FALSE)
imps_s <- HS.amelia_s$imputations
imps_amelia_s<-imputationList(imps_s)

# Running model and adding survey design ----------------------------------

#run basic empty model then update with survey design
lavaan_fit_model_s<-sem(S34_model_simple_centered_alt_s)
summary(lavaan_fit_model_s)

varTable(lavaan_fit_model_s)

# Survey.design with mitml and amelia - mice wasn't working 
design_imp_amelia_s<-svydesign(ids=~unq_isl, strata=~node, data=imps_amelia_s)
design_imp_mitml_s<-svydesign(ids=~unq_isl, strata=~node, data=imputed_mitml_s)

fit.adj.amelia.s<-lavaan.survey(lavaan.fit=lavaan_fit_model_s, survey.design = design_imp_amelia_s, estimator="MLMVS")
summary(fit.adj.amelia.s, standardized=T)

fit.adj.mitml.s<-lavaan.survey(lavaan.fit=lavaan_fit_model_s, survey.design = design_imp_mitml_s, estimator="MLMVS")
summary(fit.adj.mitml.s, standardized=T)


#if model Sign can look at these: 
#plyr::arrange(modificationIndices(fit.adj.amelia.s),mi, decreasing=TRUE)
#######

#semplot
grps.s<-list(Algae=c("c.log_MEAN_rockarea2000","c.log_site_mean_by_tran", "c.log_MEAN_kparea2k", "c.log_MEAN_egarea2k" ),
           Islchar=c("c.PA_norml", "c.SLOPE_degrees", "c.log_Area", "c.WAVE_EXPOSURE", "beachy_substrate", "c.slope_degrees","c.log_Bog_area", "c.log_Dist_Near", "elevation_max",   "c.slope_isl", "c.CHM_mean_height", "c.NDVI_mean"),
           Animalvect=c("marine_animal_biomass_shore","pres_otter", "pres_marine_invert", "pres_fish", "eagles","ravens" ),
           Humans=c("c.distance_to_midden"),
           Fish=c("c.log_fish_biomass_bym3_mean", "c.log_bycatch_biomass_bym3_mean"),
           outcome=c("c.d34s"))

colour_group=c("#51C5B4", "#C2B3A2", "#EC6D98", "#DEAFFD", "#00A4EE", "#CD8958")

nodelab_s<-c("fish", "marine\ninvert","otter", "ravens", "eagles" ,"wrack", "midden","shell", "fish\nbones", "d34s\nsoil", "canopy\nheight", 
               "kelp", "eelgrass", "fucus", "sandy", "Area", "transect\nslope", "wiggly", "neighb","beach\nslope",  "wave\nexposure","elevation", 
               "island\nslope","Bog\narea","NDVI")

lay_names_s<-get_layout("", "", "", "", "", "","","","","","","","","","","","","","","","","","","","","","","","","","","","", "","", "","d34s\nsoil",   "","","", "","","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                          "wrack","","", "","","","","", "","","","","","", "","", "shell","","","","","","", "fish\nbones", "","", "","","","","","","","","","","","","","","","", "","","","","", "","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                          "","", "","","","","","","","","","", "eagles","","","","","","","ravens" ,"","","","","","","otter","","","","","","","","","","","","","","", "midden","","","","","","","","","","","","","","","","","","","","","","","", "","","","","","","","","","","","","","",
                          "","","","","","","","","","","","","","","marine\ninvert","","","","","","","","","","", "fish","","","","","","","","","","","", "","","","","","","","","","","","","","","","","","","","","","","canopy\nheight","","","","","","","","","","","","","","","","","","","",
                          "fucus","","","", "","","kelp","","","", "","","eelgrass","","","","","","","beach\nslope","","","","","wave\nexposure","","","","", "sandy","","","","","","","", "transect\nslope","","","","","","NDVI","","","","","","wiggly","","","","","","Area","","","", "","","Bog\narea","","","","","", "neighb","","","","","", "elevation", "","","","","", "island\nslope", rows=5)

semPaths(fit.adj.mitml.s, what="std",  layout=lay_names_s, intercepts=FALSE, residuals=FALSE,
         groups=grps.s, exoVar = FALSE,  color=colour_group, esize=2, nodeLabels = nodelab_s, legend=FALSE)

semPaths(fit.adj.mitml.s, what="path",  layout=lay_names_s, intercepts=FALSE, residuals=FALSE,
         groups=grps.s, exoVar = FALSE, color=colour_group, nodeLabels = nodelab_s, legend=FALSE)



semPlot::semPaths(fit.adj.mitml.s, "path",fade = F, residuals = F, intercepts=FALSE, label.cex=2, nCharNodes = 0, nodeLabels = 1:25)
semPlot::semPaths(fit.adj.mitml.s, "path",fade = F, residuals = F, intercepts=FALSE, label.cex=2, nCharNodes = 0)

AIC(fit.adj.mitml.s)
AIC(fit.adj.mitml.veg)
#s is a better fit? 
