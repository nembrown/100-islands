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

master_island<-read.csv("C:Biodiversity idea//Output files//master_island.csv")

head(master_island)


## pair down the variables
sem_variables_names_richness_island<-c("node", "unq_isl", "log_fish_biomass_bym3_mean", "log_bycatch_biomass_bym3_mean",
                           "SLOPE_degrees", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", 
                           "ravens",  "d15n", "distance_to_midden",
                           "PA_norml", "log_site_mean_by_isl", "log_MEAN_kparea2k", "log_MEAN_egarea2k", "otter_pres", 
                            "eagles", "log_Bog_area", "log_Dist_Near", "log_MEAN_rockarea2000" ,"elevation_max", 
                           "slope_isl", "CHM_mean_height", "plant_richness", "log_DistW_ML")



master_island_sem_subset_richness_island<-master_island[, colnames(master_island) %in% sem_variables_names_richness_island]
master_island_sem_subset_richness_island$unq_isl<-factor(master_island_sem_subset_richness_island$unq_isl, ordered=TRUE)
master_island_sem_subset_richness_island$node<-factor(master_island_sem_subset_richness_island$node)

master_island_sem_subset_richness_island_centered <- stdize(master_island_sem_subset_richness_island, 
                                                 omit.cols = c("node","unq_isl",  "beachy_substrate", "ravens",  "otter_pres",  
                                                               "pres_marine_invert", "pres_fish", "eagles", "northing", "easting", "elevation_max"), 
                                                 center = TRUE, scale = FALSE)

head(master_island_sem_subset_richness_island_centered)

#see structure of the data, which variables have missing data
summary(master_island_sem_subset_richness_island_centered)
md.pattern(master_island_sem_subset_richness_island_centered)

# Simple non-hierarchical model  -------------------------------------------------------
#Will combine this with survey.design to get hierachical model



N15_model_simple_centered_alt_richness_island<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
          
        otter_pres ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area  + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE
          
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height

        c.log_site_mean_by_isl ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml  + c.log_MEAN_rockarea2000  + c.slope_isl

        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area  + c.PA_norml + beachy_substrate + elevation_max 

        c.d15n ~  w1*c.log_site_mean_by_isl + h1*c.distance_to_midden + c.WAVE_EXPOSURE + otter_pres + ravens + eagles + elevation_max + c.slope_isl  + c.log_Bog_area + c.CHM_mean_height + c.log_Area

        c.CHM_mean_height ~ c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 
        
        c.plant_richness ~ otter_pres + c.d15n + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.distance_to_midden + c.log_DistW_ML
        
        c.PA_norml ~ c.log_Area '



# Multiple imputation -----------------------------------------------------


# # Amelia ---------------------------------------------------------
# set.seed(12345)
HS.amelia_richness_island <- amelia(master_island_sem_subset_richness_island_centered, m = 20, idvars = c( "unq_isl", "node"), p2s = FALSE)
imps_richness_island <- HS.amelia_richness_island$imputations
imps_amelia_richness_island<-imputationList(imps_richness_island)


# Running model four options----------------------------------

#run basic model with FIML
missing_fit_model_richness_island<-sem(N15_model_simple_centered_alt_richness_island, data=master_island_sem_subset_richness_island_centered, missing="fiml")
summary(missing_fit_model_richness_island, standardized=T)
fitMeasures(missing_fit_model_richness_island, c("cfi","rmsea","srmr"))

#run amelia imputation (single level) then then model
amelia_fit_model_richness_island<-semList(N15_model_simple_centered_alt_richness_island, dataList=imps_richness_island)
summary(amelia_fit_model_richness_island)
fitMeasures(amelia_fit_model_richness_island, c("cfi","rmsea","srmr"))

#run amelia and the model simultaenously using runMI
fit.mi.amelia<-runMI(N15_model_simple_centered_alt_richness_island,master_island_sem_subset_richness_island_centered, fun = "sem",m=20, miArgs=list(idvars = c("unq_isl", "node")),
                     miPackage = "Amelia", seed = 12345)
summary(fit.mi.amelia)
fitMeasures(fit.mi.amelia, c("cfi","rmsea","srmr"))

#Survey.design
lavaan_fit_model_richness_island<-sem(N15_model_simple_centered_alt_richness_island, data=master_island_sem_subset_richness_island_centered)
design_imp_amelia_richness_island<-svydesign(ids=~unq_isl, strata=~node, data=imps_amelia_richness_island)
fit.adj.richness_island<-lavaan.survey(lavaan.fit=lavaan_fit_model_richness_island, survey.design = design_imp_amelia_richness_island, estimator="MLMVS")
summary(fit.adj.richness_island, standardized=T)
fitMeasures(fit.adj.richness_island, c("cfi","rmsea","srmr"))

# #estimator="MLMVS"
# survey design doesn't work with Node - since not all nodes have enough to be considered

plyr::arrange(modificationIndices(fit.adj.richness_island),mi, decreasing=TRUE)
#######

#semplot
grps_richness_island<-list(Algae=c("c.log_MEAN_rockarea2000","c.log_site_mean_by_isl", "c.log_MEAN_kparea2k", "c.log_MEAN_egarea2k" ),
           Islchar=c("c.PA_norml", "c.SLOPE_degrees", "c.log_Area", "c.WAVE_EXPOSURE", "beachy_substrate", "c.log_Bog_area", "c.log_Dist_Near", "elevation_max",   "c.slope_isl", "c.log_DistW_ML"),
           Animalvect=c("otter_pres", "eagles","ravens", "c.distance_to_midden" ),
           plants=c("c.plant_richness", "c.CHM_mean_height", "c.NDVI_mean"),
           Fish=c("c.log_fish_biomass_bym3_mean", "c.log_bycatch_biomass_bym3_mean"),
           outcome=c("c.d15n"))

colour_group=c("#51C5B4", "#C2B3A2", "#EC6D98", "#539E59", "#00A4EE", "#CD8958")

nodelab_richness_island<-c("fish", "marine\ninvert","otter", "ravens", "eagles" ,"wrack", "distance\nmidden", "d15N\nsoil",  "canopy\nheight","plant\nrichness", 
                           "edge\neffects","kelp", "eelgrass", "fucus", "sandy", "Area", "neighb","beach\nslope",  "wave\nexposure","elevation",
               "island\nslope","Bog\narea", "Dist\nmainland")


lay_names_richness_island<-get_layout(
                          "", "", "", "", "", "","","","","","","","","","","","","","","","","","","","","","","","","","","","", "","", "","","","", "","","","","", "","","","","", "","","","","", "plant\nrichness","","","","","","","","","","","","","","","","","","","","","","","","","","",
                          "","","","","","","","","","","","","","","","","","","","","","","","", "","", "","","","", "","","","d15N\nsoil",  "","","", "", "", "", "", "","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                          "wrack","","", "","","","","", "","","","","","", "","", "","","","","","","", "", "","", "","","","","","","","","","","","","","","","", "","","","","", "","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                          "","", "","","","","","","","","","", "eagles","","","","","","","ravens" ,"","","","","","","otter","","","","","","","","","","","","", "distance\nmidden","","","","","","","","","","","","","","","","","","","","","","","","","", "","","","","","","","","","","","","","","","",
                          "","","","","","","","","","","","","","","marine\ninvert","","","","","","","","","","", "fish","","","","","","","","","","","", "","","","","","","","","","","","","","","","","","","","","","edge\neffects","","","","","","","","","canopy\nheight","","","","","","","","","","","","","",
                          "fucus","","","", "","","kelp","","","", "","","eelgrass","","","","","","","beach\nslope","","","","","","","wave\nexposure","","","","","","", "sandy","","","","","","","", "Dist\nmainland","","","","","","","","","","","","","","","Area","","","", "","","Bog\narea","","","","","", "neighb","","","","","", "elevation", "","","","","", "island\nslope", rows=6)

semPaths(lavaan_fit_model_richness_island, what="std",  layout=lay_names_richness_island, intercepts=FALSE, residuals=FALSE,
         groups=grps_richness_island, exoVar = FALSE,  color=colour_group, esize=2, nodeLabels = nodelab_richness_island, legend=FALSE)


semPaths(fit.adj.richness_island, what="std",  layout=lay_names_richness_island, intercepts=FALSE, residuals=FALSE,
         groups=grps_richness_island, exoVar = FALSE,  color=colour_group, esize=2, nodeLabels = nodelab_richness_island, legend=FALSE)



semPaths(fit.adj.richness_island, what="path",  layout=lay_names_richness_island, intercepts=FALSE, residuals=FALSE,
         groups=grps_richness_island, exoVar = FALSE,  color=colour_group, esize=2, nodeLabels = nodelab_richness_island, legend=FALSE)



fit_isl <- fit.adj.richness_island

lavaan::standardizedSolution(fit_isl) %>% dplyr::filter(!is.na(pvalue)) %>% arrange(desc(pvalue)) %>% mutate_if("is.numeric","round",3) %>% select(-ci.lower,-ci.upper,-z)

pvalue_cutoff <- 0.10

obj_isl <- semPlot:::semPlotModel(fit_isl)

# save a copy of the original, so we can compare it later and be sure we removed only what we intended to remove
original_Pars_isl <- obj_isl@Pars

check_Pars_isl <- obj_isl@Pars %>% dplyr::filter(!(edge %in% c("int","<->") | lhs == rhs)) # this is the list of paramater to sift thru
keep_Pars_isl <- obj_isl@Pars %>% dplyr::filter(edge %in% c("int","<->") | lhs == rhs) # this is the list of paramater to keep asis
test_against <- lavaan::standardizedSolution(fit_isl) %>% dplyr::filter(pvalue < pvalue_cutoff, rhs != lhs)
test_against_rev <- test_against %>% dplyr::rename(rhs2 = lhs, lhs = rhs) %>% dplyr::rename(rhs = rhs2)
checked_Pars_isl <-
  check_Pars_isl %>% semi_join(test_against, by = c("lhs", "rhs")) %>% bind_rows(
    check_Pars_isl %>% semi_join(test_against_rev, by = c("lhs", "rhs"))
  )

obj_isl@Pars <- keep_Pars_isl %>% bind_rows(checked_Pars_isl)

#let's verify by looking at the list of the edges we removed from the obj_islect
anti_join(original_Pars_isl,obj_isl@Pars)
# great, let's plot
#semPlot::semPaths(obj_isl, "std",fade = F, residuals = F)


semPaths(obj_isl, what="std",  layout=lay_names_richness_island, intercepts=FALSE, residuals=FALSE,
         groups=grps_richness_island, exoVar = FALSE,  color=colour_group, esize=2, nodeLabels = nodelab_richness_island, legend=FALSE)


semPlot::semPaths(fit.adj.richness_island, "path",fade = F, residuals = F, intercepts=FALSE, label.cex=2, nCharNodes = 0, nodeLabels = 1:23)
semPlot::semPaths(fit.adj.richness_island, "path",fade = F, residuals = F, intercepts=FALSE, label.cex=2, nCharNodes = 0)





####### Island without the richness part



## pair down the variables
sem_variables_names_island<-c("node", "unq_isl", "log_fish_biomass_bym3_mean", "log_bycatch_biomass_bym3_mean",
                                       "SLOPE_degrees", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", 
                                       "ravens",  "d15n", "distance_to_midden",
                                       "PA_norml", "log_site_mean_by_isl", "log_MEAN_kparea2k", "log_MEAN_egarea2k", "otter_pres", 
                                       "eagles", "log_Bog_area", "log_Dist_Near", "log_MEAN_rockarea2000" ,"elevation_max", 
                                       "slope_isl", "CHM_mean_height",  "log_DistW_ML")



master_island_sem_subset_island<-master_island[, colnames(master_island) %in% sem_variables_names_island]
master_island_sem_subset_island$unq_isl<-factor(master_island_sem_subset_island$unq_isl, ordered=TRUE)
master_island_sem_subset_island$node<-factor(master_island_sem_subset_island$node)

master_island_sem_subset_island_centered <- stdize(master_island_sem_subset_island, 
                                                            omit.cols = c("node","unq_isl",  "beachy_substrate", "ravens",  "otter_pres",  
                                                                          "pres_marine_invert", "pres_fish", "eagles", "northing", "easting", "elevation_max"), 
                                                            center = TRUE, scale = FALSE)

head(master_island_sem_subset_island_centered)

#see structure of the data, which variables have missing data
summary(master_island_sem_subset_island_centered)
md.pattern(master_island_sem_subset_island_centered)

# Simple non-hierarchical model  -------------------------------------------------------
#Will combine this with survey.design to get hierachical model



N15_model_simple_centered_alt_island<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
          
        otter_pres ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area  + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE
          
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height

        c.log_site_mean_by_isl ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml  + c.log_MEAN_rockarea2000  + c.slope_isl

        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area  + c.PA_norml + beachy_substrate + elevation_max 

        c.d15n ~  w1*c.log_site_mean_by_isl + h1*c.distance_to_midden + c.WAVE_EXPOSURE + otter_pres + ravens + eagles + elevation_max + c.slope_isl  + c.log_Bog_area + c.CHM_mean_height + c.log_Area

        c.CHM_mean_height ~ c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area  '



# Multiple imputation -----------------------------------------------------


# # Amelia ---------------------------------------------------------
# set.seed(12345)
HS.amelia_island <- amelia(master_island_sem_subset_island_centered, m = 20, idvars = c( "unq_isl", "node"), p2s = FALSE)
imps_island <- HS.amelia_island$imputations
imps_amelia_island<-imputationList(imps_island)


# Running model four options----------------------------------

#run basic model with FIML
missing_fit_model_island<-sem(N15_model_simple_centered_alt_island, data=master_island_sem_subset_island_centered, missing="fiml")
summary(missing_fit_model_island, standardized=T)
fitMeasures(missing_fit_model_island, c("cfi","rmsea","srmr"))

#run amelia imputation (single level) then then model
amelia_fit_model_island<-semList(N15_model_simple_centered_alt_island, dataList=imps_island)
summary(amelia_fit_model_island)
fitMeasures(amelia_fit_model_island, c("cfi","rmsea","srmr"))

#run amelia and the model simultaenously using runMI
fit.mi.amelia<-runMI(N15_model_simple_centered_alt_island,master_island_sem_subset_island_centered, fun = "sem",m=20, miArgs=list(idvars = c("unq_isl", "node")),
                     miPackage = "Amelia", seed = 12345)
summary(fit.mi.amelia)
fitMeasures(fit.mi.amelia, c("cfi","rmsea","srmr"))

#Survey.design
lavaan_fit_model_island<-sem(N15_model_simple_centered_alt_island, data=master_island_sem_subset_island_centered)
design_imp_amelia_island<-svydesign(ids=~unq_isl, strata=~node, data=imps_amelia_island)
fit.adj.island<-lavaan.survey(lavaan.fit=lavaan_fit_model_island, survey.design = design_imp_amelia_island, estimator="MLMVS")
summary(fit.adj.island, standardized=T)
fitMeasures(fit.adj.island, c("cfi","rmsea","srmr"))

# #estimator="MLMVS"
# survey design doesn't work with Node - since not all nodes have enough to be considered

plyr::arrange(modificationIndices(fit.adj.island),mi, decreasing=TRUE)
#######

#semplot
grps_island<-list(Algae=c("c.log_MEAN_rockarea2000","c.log_site_mean_by_isl", "c.log_MEAN_kparea2k", "c.log_MEAN_egarea2k" ),
                  Islchar=c("c.PA_norml", "c.SLOPE_degrees", "c.log_Area", "c.WAVE_EXPOSURE", "beachy_substrate", "c.log_Bog_area", "c.log_Dist_Near", "elevation_max",   "c.slope_isl"),
                  Animalvect=c("otter_pres", "eagles","ravens"),
                  Humans=c("c.distance_to_midden"),
                  Fish=c("c.log_fish_biomass_bym3_mean", "c.log_bycatch_biomass_bym3_mean"),
                  outcome=c("c.d15n"))

colour_group=c("#51C5B4", "#C2B3A2", "#EC6D98", "#DEAFFD", "#00A4EE", "#CD8958")

nodelab_island<-c("fish", "marine\ninvert","otter", "ravens", "eagles" ,"wrack", "distance\nmidden", "d15N\nsoil",  "canopy\nheight", 
                           "kelp", "eelgrass", "fucus", "sandy", "Area","edge\neffects", "neighb","beach\nslope",  "wave\nexposure","elevation",
                           "island\nslope","Bog\narea")


lay_names_island<-get_layout("", "", "", "", "", "","","","","","","","","","","","","","","","","","","","","","","","","","","","", "","", "","d15N\nsoil",   "","","", "","","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                              "wrack","","", "","","","","", "","","","","","", "","", "","","","","","","", "", "","", "","","","","","","","","","","","","","","","", "","","","","", "","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                              "","", "","","","","","","","","","", "eagles","","","","","","","ravens" ,"","","","","","","otter","","","","","","","","","","","","","","","","","","", "distance\nmidden","","","","","","","","","","","","","","","","","","","", "","","","","","","","","","","","","","",
                              "","","","","","","","","","","","","","","marine\ninvert","","","","","","","","","","", "fish","","","","","","","","","","","", "","","","","","","","","","","","","","","","","","","","","","","canopy\nheight","","","","","","","","","","","","","","","","","","","",
                              "fucus","","","", "","","kelp","","","", "","","eelgrass","","","","","","","beach\nslope","","","","","wave\nexposure","","","","", "sandy","","","","","","","", "","","","","","","","","","","","","edge\neffects","","","","","","Area","","","", "","","Bog\narea","","","","","", "neighb","","","","","", "elevation", "","","","","", "island\nslope", rows=5)

semPaths(lavaan_fit_model_island, what="std",  layout=lay_names_island, intercepts=FALSE, residuals=FALSE,
         groups=grps_island, exoCov = FALSE,  color=colour_group, esize=2, nodeLabels = nodelab_island, legend=FALSE,
         filetype="tiff", filename="Food web idea/Plots/SEM/SEM_dN15_isl_full", width=8, height=4)


semPaths(fit.adj.island, what="path",  layout=lay_names_island, intercepts=FALSE, residuals=FALSE,
         groups=grps_island, exoCov = FALSE,  color=colour_group, esize=2, nodeLabels = nodelab_island, legend=FALSE,
         filetype="tiff", filename="Food web idea/Plots/SEM/SEM_dN15_isl_grey", width=8, height=4)


fit_isl <- fit.adj.island

lavaan::standardizedSolution(fit_isl) %>% dplyr::filter(!is.na(pvalue)) %>% arrange(desc(pvalue)) %>% mutate_if("is.numeric","round",3) %>% select(-ci.lower,-ci.upper,-z)

pvalue_cutoff <- 0.10

obj_isl <- semPlot:::semPlotModel(fit_isl)

# save a copy of the original, so we can compare it later and be sure we removed only what we intended to remove
original_Pars_isl <- obj_isl@Pars

check_Pars_isl <- obj_isl@Pars %>% dplyr::filter(!(edge %in% c("int","<->") | lhs == rhs)) # this is the list of paramater to sift thru
keep_Pars_isl <- obj_isl@Pars %>% dplyr::filter(edge %in% c("int","<->") | lhs == rhs) # this is the list of paramater to keep asis
test_against <- lavaan::standardizedSolution(fit_isl) %>% dplyr::filter(pvalue < pvalue_cutoff, rhs != lhs)
test_against_rev <- test_against %>% dplyr::rename(rhs2 = lhs, lhs = rhs) %>% dplyr::rename(rhs = rhs2)
checked_Pars_isl <-
  check_Pars_isl %>% semi_join(test_against, by = c("lhs", "rhs")) %>% bind_rows(
    check_Pars_isl %>% semi_join(test_against_rev, by = c("lhs", "rhs"))
  )

obj_isl@Pars <- keep_Pars_isl %>% bind_rows(checked_Pars_isl)

#let's verify by looking at the list of the edges we removed from the obj_islect
anti_join(original_Pars_isl,obj_isl@Pars)
# great, let's plot
#semPlot::semPaths(obj_isl, "std",fade = F, residuals = F)


semPaths(obj_isl, what="std",  layout=lay_names_island, intercepts=FALSE, residuals=FALSE,
         groups=grps_island,  color=colour_group, esize=2, nodeLabels = nodelab_island, legend=FALSE,
         filetype="tiff", filename="Food web idea/Plots/SEM/SEM_dN15_isl_sig", width=8, height=4)



semPlot::semPaths(fit.adj.island, "path",fade = F, residuals = F, intercepts=FALSE, label.cex=2, nCharNodes = 0, nodeLabels = 1:21)
semPlot::semPaths(fit.adj.island, "path",fade = F, residuals = F, intercepts=FALSE, label.cex=2, nCharNodes = 0)




