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
library(tidyverse)

master_transect<-read.csv("C:Biodiversity idea//Output files//master_transect.csv")

head(master_transect)


## pair down the variables
sem_variables_names_richness<-c("node", "unq_tran","unq_isl", "log_fish_biomass_bym3_mean", "log_bycatch_biomass_bym3_mean",
                           "SLOPE_degrees", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope_degrees",
                           "ravens",  "d15n", "distance_to_midden",
                           "PA_norml", "log_site_mean_by_tran", "log_MEAN_kparea2k", "log_MEAN_egarea2k", "pres_otter", 
                           "pres_marine_invert", "pres_fish", "eagles", "log_Bog_area", "log_Dist_Near", "log_MEAN_rockarea2000" ,"elevation_max", 
                           "slope_isl", "CHM_mean_height", "plant_richness")



master_transec_sem_subset_richness<-master_transect[, colnames(master_transect) %in% sem_variables_names_richness]
master_transec_sem_subset_richness$unq_tran<-factor(master_transec_sem_subset_richness$unq_tran, ordered=TRUE)
master_transec_sem_subset_richness$unq_isl<-factor(master_transec_sem_subset_richness$unq_isl, ordered=TRUE)
master_transec_sem_subset_richness$node<-factor(master_transec_sem_subset_richness$node)

master_transec_sem_subset_richness_centered <- stdize(master_transec_sem_subset_richness, 
                                                 omit.cols = c("node", "unq_tran","unq_isl",  "beachy_substrate", "ravens",  "pres_otter",  
                                                               "pres_marine_invert", "pres_fish", "eagles", "northing", "easting", "elevation_max"), 
                                                 center = TRUE, scale = FALSE)

head(master_transec_sem_subset_richness_centered)

#see structure of the data, which variables have missing data
summary(master_transec_sem_subset_richness_centered)
md.pattern(master_transec_sem_subset_richness_centered)


# Simple non-hierarchical model  -------------------------------------------------------
#Will combine this with survey.design to get hierachical model


N15_model_simple_centered_alt_richness<-'
          
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

        c.CHM_mean_height ~ c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 

        c.plant_richness ~ pres_otter + c.d15n + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.slope_degrees + pres_marine_invert + pres_fish + c.distance_to_midden'



# Multiple imputation -----------------------------------------------------
# We need to use multiple imputation because the methods to understand our multi-level data dont accept missing data


# Option 1 mitml ----------------------------------------------------------
#OPTION 1 Multiple imputation using mimtl packacge which is two levels - 
#this is important for the log_Bog_area variable whic is at level 2 (island level) but has missing values) 

#this is run the first time, but can also load it
fml.richness <- list( c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean +
                   c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.slope_degrees + c.log_site_mean_by_tran  +
                   c.d15n + c.distance_to_midden + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + pres_otter +
                   pres_marine_invert + pres_fish + c.log_MEAN_rockarea2000   ~ 1 + (1|unq_isl) ,                                                 # Level 1
                 c.log_Bog_area + c.log_Area +  c.PA_norml + ravens + eagles + c.log_Dist_Near + elevation_max + c.slope_isl  + c.CHM_mean_height ~ 1 )                          # Level 2


imp.richness <- jomoImpute(master_transec_sem_subset_richness_centered, formula=fml.richness, n.burn=500, n.iter=1000, m=20)
summary(imp.richness)
write.mitml(imp.richness, "C:Food web idea//Data by person//Norah.data/imp.richness")

#load it
imp.richness <- read.mitml("C:Food web idea//Data by person//Norah.data/imp.richness")

#problems the model sees:
plot(imp.richness, trace="all", print="beta2", pos=c(1,7))
plot(imp.richness, trace="all", print="beta", pos=c(1,11))
plot(imp.richness, trace="all", print="psi", pos=c(17,6))
plot(imp.richness, trace="all", print="sigma", pos=c(4,2))

implist_richness <- mitmlComplete(imp.richness, "all")
#get it to talk to survey.design
imputed_mitml_richness <- imputationList(implist_richness)


# Option 2 Amelia ---------------------------------------------------------
set.seed(12345)
HS.amelia_richness <- amelia(master_transec_sem_subset_richness_centered, m = 20, idvars = c("unq_tran", "unq_isl", "node"), p2s = FALSE)
imps_richness <- HS.amelia_richness$imputations
imps_amelia_richness<-imputationList(imps_richness)

# Running model and adding survey design ----------------------------------

#run basic empty model then update with survey design
lavaan_fit_model_richness<-sem(N15_model_simple_centered_alt_richness, data=master_transec_sem_subset_richness_centered)
summary(lavaan_fit_model_richness)

varTable(lavaan_fit_model_richness)

# Survey.design with mitml and amelia - mice wasn't working 
design_imp_amelia_richness<-svydesign(ids=~unq_isl, strata=~node, data=imps_amelia_richness)
design_imp_mitml_richness<-svydesign(ids=~unq_isl, strata=~node, data=imputed_mitml_richness)

fit.adj.amelia.richness<-lavaan.survey(lavaan.fit=lavaan_fit_model_richness, survey.design = design_imp_amelia_richness, estimator="MLMVS")
summary(fit.adj.amelia.richness, standardized=T)

fit.adj.mitml.richness<-lavaan.survey(lavaan.fit=lavaan_fit_model_richness, survey.design = design_imp_mitml_richness, estimator="MLMVS")
 summary(fit.adj.mitml.richness, standardized=T)

 options(max.print=1000000)
#if model Sign can look at these: 
#plyr::arrange(modificationIndices(fit.adj.amelia.richness),mi, decreasing=TRUE)
#######

#semplot
grps_richness<-list(Algae=c("c.log_MEAN_rockarea2000","c.log_site_mean_by_tran", "c.log_MEAN_kparea2k", "c.log_MEAN_egarea2k" ),
           Islchar=c("c.PA_norml", "c.SLOPE_degrees", "c.log_Area", "c.WAVE_EXPOSURE", "beachy_substrate", "c.slope_degrees","c.log_Bog_area", "c.log_Dist_Near", "elevation_max",   "c.slope_isl"),
           Animalvect=c("pres_otter", "eagles","ravens", "c.distance_to_midden" ),
           plants=c("c.plant_richness", "c.CHM_mean_height", "c.NDVI_mean"),
           Fish=c("c.log_fish_biomass_bym3_mean", "c.log_bycatch_biomass_bym3_mean",  "pres_marine_invert", "pres_fish"),
           outcome=c("c.d15n"))

colour_group=c("#51C5B4", "#C2B3A2", "#EC6D98", "#539E59", "#00A4EE", "#CD8958")

nodelab_richness<-c("fish", "marine\ninvert","otter", "ravens", "eagles" ,"wrack", "distance\nmidden","shell", 
                       "fish\nbones", "d15N\nsoil",  "canopy\nheight","plant\nrichness", 
               "kelp", "eelgrass", "fucus", "sandy", "Area", "transect\nslope", "edge\neffects", "neighb","beach\nslope",  "wave\nexposure","elevation", 
               "island\nslope","Bog\narea")


lay_names_richness<-get_layout(
                          "", "", "", "", "", "","","","","","","","","","","","","","","","","","","","","","","","","","","","", "","", "","","","", "","","","","", "","","","","", "","","","","", "plant\nrichness","","","","","","","","","","","","","","","","","","","","","","","","","","",
                          "","","","","","","","","","","","","","","","","","","","","","","","", "","", "","","","", "","","","d15N\nsoil",  "","","", "", "", "", "", "","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                          "wrack","","", "","","","","", "","","","","","", "","", "shell","","","","","","", "fish\nbones", "","", "","","","","","","","","","","","","","","","", "","","","","", "","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                          "","", "","","","","","","","","","", "eagles","","","","","","","ravens" ,"","","","","","","otter","","","","","","","","","","","","","","","","","","", "distance\nmidden","","","","","","","","","","","","","","","","","","","", "","","","","","","","","","","","","","","","",
                          "","","","","","","","","","","","","","","marine\ninvert","","","","","","","","","","", "fish","","","","","","","","","","","", "","","","","","","","","","","","","","","","","","","","","","","canopy\nheight","","","","","","","","","","","","","","","","","","","","","",
                           "fucus","","","", "","","kelp","","","", "","","eelgrass","","","","","","","beach\nslope","","","","","","","wave\nexposure","","","","","","", "sandy","","","","","","","", "transect\nslope","","","","","","","","","edge\neffects","","","","","","Area","","","", "","","Bog\narea","","","","","", "neighb","","","","","", "elevation", "","","","","", "island\nslope", rows=6)

semPaths(fit.adj.mitml.richness, what="std",  layout=lay_names_richness, intercepts=FALSE, residuals=FALSE,
         groups=grps_richness, exoVar = FALSE,  color=colour_group, esize=2, nodeLabels = nodelab_richness, legend=FALSE)

semPaths(fit.adj.mitml.richness, what="path",  layout=lay_names_richness, intercepts=FALSE, residuals=FALSE,
         groups=grps, exoVar = FALSE, color=colour_group, nodeLabels = nodelab_richness, legend=FALSE)


fit.richness <- fit.adj.mitml.richness

lavaan::standardizedSolution(fit.richness) %>% dplyr::filter(!is.na(pvalue)) %>% arrange(desc(pvalue)) %>% mutate_if("is.numeric","round",3) %>% select(-ci.lower,-ci.upper,-z)

pvalue_cutoff <- 0.10

obj.richness <- semPlot:::semPlotModel(fit.richness)

# save a copy of the original, so we can compare it later and be sure we removed only what we intended to remove
original_Pars <- obj.richness@Pars

check_Pars <- obj.richness@Pars %>% dplyr::filter(!(edge %in% c("int","<->") | lhs == rhs)) # this is the list of paramater to sift thru
keep_Pars <- obj.richness@Pars %>% dplyr::filter(edge %in% c("int","<->") | lhs == rhs) # this is the list of paramater to keep asis
test_against <- lavaan::standardizedSolution(fit.richness) %>% dplyr::filter(pvalue < pvalue_cutoff, rhs != lhs)
test_against_rev <- test_against %>% dplyr::rename(rhs2 = lhs,   # for some reason, the rhs and lhs are reversed in the standardizedSolution() output, for some of the values
                                            lhs = rhs) %>% # I'll have to reverse it myself, and test against both orders
  dplyr::rename(rhs = rhs2)
checked_Pars <-
  check_Pars %>% semi_join(test_against, by = c("lhs", "rhs")) %>% bind_rows(
    check_Pars %>% semi_join(test_against_rev, by = c("lhs", "rhs"))
  )

obj.richness@Pars <- keep_Pars %>% bind_rows(checked_Pars)

#let's verify by looking at the list of the edges we removed from the obj.richnessect
anti_join(original_Pars,obj.richness@Pars)
# great, let's plot
semPlot::semPaths(obj.richness, "std",fade = F, residuals = F)


semPaths(obj.richness, what="std",  layout=lay_names_richness, intercepts=FALSE, residuals=FALSE,
         groups=grps_richness, exoVar = FALSE,  color=colour_group, esize=2, nodeLabels = nodelab_richness, legend=FALSE)


semPlot::semPaths(fit.adj.mitml.richness, "path",fade = F, residuals = F, intercepts=FALSE, label.cex=2, nCharNodes = 0, nodeLabels = 1:27)
semPlot::semPaths(fit.adj.mitml.richness, "path",fade = F, residuals = F, intercepts=FALSE, label.cex=2, nCharNodes = 0)

