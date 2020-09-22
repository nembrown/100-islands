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

plot(master_transect$log_Area ~ master_transect$PA_norml)
## pair down the variables
sem_variables_names_veg<-c("node", "unq_tran","unq_isl", "log_fish_biomass_bym3_mean", "log_bycatch_biomass_bym3_mean",
                       "SLOPE_degrees", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope_degrees",
                       "ravens",  "d15n", "distance_to_midden",
                       "PA_norml", "log_site_mean_by_tran", "log_MEAN_kparea2k", "log_MEAN_egarea2k", "pres_otter", 
                       "pres_marine_invert", "pres_fish", "eagles", "log_Bog_area", "log_Dist_Near", "log_MEAN_rockarea2000" ,"elevation_max", 
                       "slope_isl", "CHM_mean_height", "habitat_het", "NDVI_mean")



master_transec_sem_subset_veg<-master_transect[, colnames(master_transect) %in% sem_variables_names_veg]
master_transec_sem_subset_veg$unq_tran<-factor(master_transec_sem_subset_veg$unq_tran, ordered=TRUE)
master_transec_sem_subset_veg$unq_isl<-factor(master_transec_sem_subset_veg$unq_isl, ordered=TRUE)
master_transec_sem_subset_veg$node<-factor(master_transec_sem_subset_veg$node)

master_transec_sem_subset_veg_centered <- stdize(master_transec_sem_subset_veg, 
                                             omit.cols = c("node", "unq_tran","unq_isl",  "beachy_substrate", "ravens",  "pres_otter",  
                                                           "pres_marine_invert", "pres_fish", "eagles", "northing", "easting", "elevation_max"), 
                                             center = TRUE, scale = FALSE)

head(master_transec_sem_subset_veg_centered)

#see structure of the data, which variables have missing data
summary(master_transec_sem_subset_veg_centered)
md.pattern(master_transec_sem_subset_veg_centered)


# Simple non-hierarchical model  -------------------------------------------------------
#Will combine this with survey.design to get hierachical model


N15_model_simple_centered_alt_veg<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
          
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE + elevation_max
          
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height

        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000

        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 

        pres_marine_invert ~ eagles + ravens + pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden

        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden

        c.d15n ~ w1*c.log_site_mean_by_tran + h1*c.distance_to_midden + pres_marine_invert + pres_fish + c.slope_degrees  + c.WAVE_EXPOSURE + pres_otter + ravens + eagles + elevation_max + c.slope_isl  + c.log_Bog_area + c.CHM_mean_height

        c.CHM_mean_height ~ c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 

'
# Multiple imputation -----------------------------------------------------
# We need to use multiple imputation because the methods to understand our multi-level data dont accept missing data


# Option 1 mitml ----------------------------------------------------------
#OPTION 1 Multiple imputation using mimtl packacge which is two levels - 
#this is important for the log_Bog_area variable whic is at level 2 (island level) but has missing values) 

#this is run the first time, but can also load it 
fml.veg <- list( c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean +
               c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.slope_degrees + c.log_site_mean_by_tran  + 
               c.d15n + c.distance_to_midden + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + pres_otter +
               pres_marine_invert + pres_fish + c.log_MEAN_rockarea2000  ~ 1 + (1|unq_isl) ,                                                 # Level 1
               c.log_Bog_area + c.log_Area +  c.PA_norml + ravens + eagles + c.log_Dist_Near + elevation_max + c.slope_isl  + c.CHM_mean_height + c.habitat_het ~ 1 )                          # Level 2


imp.veg <- jomoImpute(master_transec_sem_subset_veg_centered, formula=fml.veg, n.burn=500, n.iter=1000, m=20)
summary(imp.veg)
write.mitml(imp.veg, "C:Food web idea//Data by person//Norah.data/imp.veg")

#load it 
imp.veg <- read.mitml("C:Food web idea//Data by person//Norah.data/imp.veg")

#problems the model sees: 
plot(imp.veg, trace="all", print="beta2", pos=c(1,7))
plot(imp.veg, trace="all", print="beta", pos=c(1,11))
plot(imp.veg, trace="all", print="psi", pos=c(17,6))
plot(imp.veg, trace="all", print="sigma", pos=c(4,2))

implist_veg <- mitmlComplete(imp.veg, "all")
#get it to talk to survey.design
imputed_mitml_veg <- imputationList(implist_veg)


# Option 2 Amelia ---------------------------------------------------------
set.seed(12345)
HS.amelia_veg <- amelia(master_transec_sem_subset_veg_centered, m = 20, idvars = c("unq_tran", "unq_isl", "node"), p2s = FALSE)
imps_veg <- HS.amelia_veg$imputations
imps_amelia_veg<-imputationList(imps_veg)

# Running model and adding survey design ----------------------------------

#run basic empty model then update with survey design
lavaan_fit_model_veg<-sem(N15_model_simple_centered_alt_veg, data=master_transec_sem_subset_veg_centered)
summary(lavaan_fit_model_veg)

varTable(lavaan_fit_model_veg)

# Survey.design with mitml and amelia - mice wasn't working 
design_imp_amelia_veg<-svydesign(ids=~unq_isl, strata=~node, data=imps_amelia_veg)
design_imp_mitml_veg<-svydesign(ids=~unq_isl, strata=~node, data=imputed_mitml_veg)

fit.adj.amelia.veg<-lavaan.survey(lavaan.fit=lavaan_fit_model_veg, survey.design = design_imp_amelia_veg, estimator="MLMVS")
summary(fit.adj.amelia.veg, standardized=T)

fit.adj.mitml.veg<-lavaan.survey(lavaan.fit=lavaan_fit_model_veg, survey.design = design_imp_mitml_veg, estimator="MLMVS")
summary(fit.adj.mitml.veg, standardized=T)

parest<-parameterestimates(fit.adj.mitml.veg)
View(parest)
#if model Sign can look at these: 
#plyr::arrange(modificationIndices(fit.adj.amelia.veg),mi, decreasing=TRUE)
#######

#semplot
grps<-list(Algae=c("c.log_MEAN_rockarea2000","c.log_site_mean_by_tran", "c.log_MEAN_kparea2k", "c.log_MEAN_egarea2k" ),
           Islchar=c("c.PA_norml", "c.SLOPE_degrees", "c.log_Area", "c.WAVE_EXPOSURE", "beachy_substrate", "c.slope_degrees","c.log_Bog_area", "c.log_Dist_Near", "elevation_max",   "c.slope_isl", "c.CHM_mean_height"),
           Animalvect=c("marine_animal_biomass_shore","pres_otter", "pres_marine_invert", "pres_fish", "eagles","ravens" ),
           Humans=c("c.distance_to_midden"),
           Fish=c("c.log_fish_biomass_bym3_mean", "c.log_bycatch_biomass_bym3_mean"),
           outcome=c("c.d15n"))

colour_group=c("#51C5B4", "#C2B3A2", "#EC6D98", "#DEAFFD", "#00A4EE", "#CD8958")

nodelab_veg<-c("fish", "marine\ninvert","otter", "ravens", "eagles" ,"wrack", "midden","shell", "fish\nbones", "d15N\nsoil", "canopy\nheight", 
            "kelp", "eelgrass", "fucus", "sandy", "Area", "transect\nslope", "edge\neffects", "neighb","beach\nslope",  "wave\nexposure","elevation", 
            "island\nslope","Bog\narea")

lay_names_veg<-get_layout("", "", "", "", "", "","","","","","","","","","","","","","","","","","","","","","","","","","","","", "","", "","d15N\nsoil",   "","","", "","","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                      "wrack","","", "","","","","", "","","","","","", "","", "shell","","","","","","", "fish\nbones", "","", "","","","","","","","","","","","","","","","", "","","","","", "","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                      "","", "","","","","","","","","","", "eagles","","","","","","","ravens" ,"","","","","","","otter","","","","","","","","","","","","","","","","","","", "midden","","","","","","","","","","","","","","","","","","","", "","","","","","","","","","","","","","",
                      "","","","","","","","","","","","","","","marine\ninvert","","","","","","","","","","", "fish","","","","","","","","","","","", "","","","","","","","","","","","","","","","","","","","","","","canopy\nheight","","","","","","","","","","","","","","","","","","","",
                      "fucus","","","", "","","kelp","","","", "","","eelgrass","","","","","","","beach\nslope","","","","","wave\nexposure","","","","", "sandy","","","","","","","", "transect\nslope","","","","","","","","","","","","edge\neffects","","","","","","Area","","","", "","","Bog\narea","","","","","", "neighb","","","","","", "elevation", "","","","","", "island\nslope", rows=5)

semPaths(fit.adj.mitml.veg, what="std",  layout=lay_names_veg, intercepts=FALSE, residuals=FALSE,
         groups=grps, color=colour_group, exoCov = FALSE, esize=2, nodeLabels = nodelab_veg, legend=FALSE, 
         filetype="tiff", filename="Food web idea/Plots/SEM/SEM_dN15_0m_full", width=8, height=4)

semPaths(fit.adj.mitml.veg, what="path",  layout=lay_names_veg, intercepts=FALSE, residuals=FALSE,
         groups=grps, color=colour_group, exoCov=FALSE, nodeLabels = nodelab_veg, legend=FALSE, 
         filetype="tiff", filename="Food web idea/Plots/SEM/SEM_dN15_0m_grey", width=8, height=4)

#This is code what I can use to show the residuals, exogenous variance or covariance
# graph<-semPaths(fit.adj.mitml.veg, what="std",  layout=lay_names_veg, intercepts=FALSE, residuals=TRUE,
#           groups=grps, color=colour_group, exoVar = TRUE,esize=2, nodeLabels = nodelab_veg, legend=FALSE,
#           filetype="tiff", filename="Food web idea/Plots/SEM/SEM_dN15_0m_full", width=8, height=4)
# graph$graphAttributes$Edges$curve<- ifelse(graph$Edgelist$bidir, -2, 0)
# plot(graph)
# 
# graph_grey<-semPaths(fit.adj.mitml.veg, what="path",  layout=lay_names_veg, intercepts=FALSE, residuals=TRUE,
#                 groups=grps, color=colour_group, exoVar=TRUE, esize=1, nodeLabels = nodelab_veg, legend=FALSE, 
#                 filetype="tiff", filename="Food web idea/Plots/SEM/SEM_dN15_0m_grey", width=8, height=4)
# graph_grey$graphAttributes$Edges$curve<- ifelse(graph_grey$Edgelist$bidir, -1, 0)
# plot(graph_grey)



#plot only significant paths
fit <- fit.adj.mitml.veg
lavaan::standardizedSolution(fit) %>% dplyr::filter(!is.na(pvalue)) %>% arrange(desc(pvalue)) %>% mutate_if("is.numeric","round",3) %>% select(-ci.lower,-ci.upper,-z)
pvalue_cutoff <- 0.05
obj <- semPlot:::semPlotModel(fit)

# save a copy of the original, so we can compare it later and be sure we removed only what we intended to remove
original_Pars <- obj@Pars
check_Pars <- obj@Pars %>% dplyr::filter(!(edge %in% c("int","<->") | lhs == rhs)) # this is the list of paramater to sift thru
keep_Pars <- obj@Pars %>% dplyr::filter(edge %in% c("int","<->") | lhs == rhs) # this is the list of paramater to keep asis
test_against <- lavaan::standardizedSolution(fit) %>% dplyr::filter(pvalue < pvalue_cutoff, rhs != lhs)
test_against_rev <- test_against %>% dplyr::rename(rhs2 = lhs,   # for some reason, the rhs and lhs are reversed in the standardizedSolution() output, for some of the values
                                            lhs = rhs) %>% # I'll have to reverse it myself, and test against both orders
        dplyr::rename(rhs = rhs2)
checked_Pars <-
        check_Pars %>% semi_join(test_against, by = c("lhs", "rhs")) %>% bind_rows(
                check_Pars %>% semi_join(test_against_rev, by = c("lhs", "rhs"))
        )

obj@Pars <- keep_Pars %>% bind_rows(checked_Pars)
anti_join(original_Pars,obj@Pars)

semPaths(obj, what="std",  layout=lay_names_veg, intercepts=FALSE, residuals=FALSE,          
         groups=grps,  color=colour_group, esize=2, nodeLabels = nodelab_veg, legend=FALSE,  
         filetype="tiff", filename="Food web idea/Plots/SEM/SEM_dN15_0m_sig", width=8, height=4)












######################## 40 M
sem_variables_names_veg_40m<-c("node", "unq_tran","unq_isl", "log_fish_biomass_bym3_mean", "log_bycatch_biomass_bym3_mean",
                           "SLOPE_degrees", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope_degrees",
                           "ravens",  "d15n_40m", "distance_to_midden",
                           "PA_norml", "log_site_mean_by_tran", "log_MEAN_kparea2k", "log_MEAN_egarea2k", "pres_otter", 
                           "pres_marine_invert", "pres_fish", "eagles", "log_Bog_area", "log_Dist_Near", "log_MEAN_rockarea2000" ,"elevation_max", 
                           "slope_isl", "CHM_mean_height", "habitat_het", "NDVI_mean")



master_transec_sem_subset_veg_40m<-master_transect[, colnames(master_transect) %in% sem_variables_names_veg_40m]
master_transec_sem_subset_veg_40m$unq_tran<-factor(master_transec_sem_subset_veg_40m$unq_tran, ordered=TRUE)
master_transec_sem_subset_veg_40m$unq_isl<-factor(master_transec_sem_subset_veg_40m$unq_isl, ordered=TRUE)
master_transec_sem_subset_veg_40m$node<-factor(master_transec_sem_subset_veg_40m$node)

master_transec_sem_subset_veg_40m_centered <- stdize(master_transec_sem_subset_veg_40m, 
                                                 omit.cols = c("node", "unq_tran","unq_isl",  "beachy_substrate", "ravens",  "pres_otter",  
                                                               "pres_marine_invert", "pres_fish", "eagles", "northing", "easting", "elevation_max"), 
                                                 center = TRUE, scale = FALSE)

head(master_transec_sem_subset_veg_40m_centered)

#see structure of the data, which variables have missing data
summary(master_transec_sem_subset_veg_40m_centered)
md.pattern(master_transec_sem_subset_veg_40m_centered)


# Simple non-hierarchical model  -------------------------------------------------------
#Will combine this with survey.design to get hierachical model


N15_model_simple_centered_alt_veg_40m<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
          
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE + elevation_max
          
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height

        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000

        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 

        pres_marine_invert ~ eagles + ravens + pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden

        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden

        c.d15n_40m ~ w1*c.log_site_mean_by_tran + h1*c.distance_to_midden + pres_marine_invert + pres_fish + c.slope_degrees  + c.WAVE_EXPOSURE + pres_otter + ravens + eagles + elevation_max + c.slope_isl  + c.log_Bog_area + c.CHM_mean_height

        c.CHM_mean_height ~ c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 

'
# Multiple imputation -----------------------------------------------------
# We need to use multiple imputation because the methods to understand our multi-level data dont accept missing data


# Option 1 mitml ----------------------------------------------------------
#OPTION 1 Multiple imputation using mimtl packacge which is two levels - 
#this is important for the log_Bog_area variable whic is at level 2 (island level) but has missing values) 

#this is run the first time, but can also load it 
fml.veg_40m <- list( c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean +
                         c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.slope_degrees + c.log_site_mean_by_tran  + 
                         c.d15n_40m + c.distance_to_midden + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + pres_otter +
                         pres_marine_invert + pres_fish + c.log_MEAN_rockarea2000  ~ 1 + (1|unq_isl) ,                                                 # Level 1
                 c.log_Bog_area + c.log_Area +  c.PA_norml + ravens + eagles + c.log_Dist_Near + elevation_max + c.slope_isl  + c.CHM_mean_height + c.habitat_het ~ 1 )                          # Level 2


imp.veg_40m <- jomoImpute(master_transec_sem_subset_veg_40m_centered, formula=fml.veg_40m, n.burn=500, n.iter=1000, m=20)
summary(imp.veg_40m)
write.mitml(imp.veg_40m, "C:Food web idea//Data by person//Norah.data/imp.veg_40m")

#load it 
imp.veg_40m <- read.mitml("C:Food web idea//Data by person//Norah.data/imp.veg_40m")

#problems the model sees: 
plot(imp.veg_40m, trace="all", print="beta2", pos=c(1,7))
plot(imp.veg_40m, trace="all", print="beta", pos=c(1,11))
plot(imp.veg_40m, trace="all", print="psi", pos=c(17,6))
plot(imp.veg_40m, trace="all", print="sigma", pos=c(4,2))

implist_veg_40m <- mitmlComplete(imp.veg_40m, "all")
#get it to talk to survey.design
imputed_mitml_veg_40m <- imputationList(implist_veg_40m)


# Option 2 Amelia ---------------------------------------------------------
set.seed(12345)
HS.amelia_veg_40m <- amelia(master_transec_sem_subset_veg_40m_centered, m = 20, idvars = c("unq_tran", "unq_isl", "node"), p2s = FALSE)
imps_veg_40m <- HS.amelia_veg_40m$imputations
imps_amelia_veg_40m<-imputationList(imps_veg_40m)

# Running model and adding survey design ----------------------------------

#run basic empty model then update with survey design
lavaan_fit_model_veg_40m<-sem(N15_model_simple_centered_alt_veg_40m, data=master_transec_sem_subset_veg_40m_centered)
summary(lavaan_fit_model_veg_40m)

varTable(lavaan_fit_model_veg_40m)

# Survey.design with mitml and amelia - mice wasn't working 
design_imp_amelia_veg_40m<-svydesign(ids=~unq_isl, strata=~node, data=imps_amelia_veg_40m)
design_imp_mitml_veg_40m<-svydesign(ids=~unq_isl, strata=~node, data=imputed_mitml_veg_40m)

fit.adj.amelia.veg_40m<-lavaan.survey(lavaan.fit=lavaan_fit_model_veg_40m, survey.design = design_imp_amelia_veg_40m, estimator="MLMVS")
summary(fit.adj.amelia.veg_40m, standardized=T)

fit.adj.mitml.veg_40m<-lavaan.survey(lavaan.fit=lavaan_fit_model_veg_40m, survey.design = design_imp_mitml_veg_40m, estimator="MLMVS")
summary(fit.adj.mitml.veg_40m, standardized=T)


#if model Sign can look at these: 
#plyr::arrange(modificationIndices(fit.adj.amelia.veg_40m),mi, decreasing=TRUE)
#######

#semplot
grps<-list(Algae=c("c.log_MEAN_rockarea2000","c.log_site_mean_by_tran", "c.log_MEAN_kparea2k", "c.log_MEAN_egarea2k" ),
           Islchar=c("c.PA_norml", "c.SLOPE_degrees", "c.log_Area", "c.WAVE_EXPOSURE", "beachy_substrate", "c.slope_degrees","c.log_Bog_area", "c.log_Dist_Near", "elevation_max",   "c.slope_isl", "c.CHM_mean_height"),
           Animalvect=c("marine_animal_biomass_shore","pres_otter", "pres_marine_invert", "pres_fish", "eagles","ravens" ),
           Humans=c("c.distance_to_midden"),
           Fish=c("c.log_fish_biomass_bym3_mean", "c.log_bycatch_biomass_bym3_mean"),
           outcome=c("c.d15n_40m"))

colour_group=c("#51C5B4", "#C2B3A2", "#EC6D98", "#DEAFFD", "#00A4EE", "#CD8958")

nodelab_veg_40m<-c("fish", "marine\ninvert","otter", "ravens", "eagles" ,"wrack", "midden","shell", "fish\nbones", "d15n_40m\nsoil", "canopy\nheight", 
               "kelp", "eelgrass", "fucus", "sandy", "Area", "transect\nslope", "edge\neffects", "neighb","beach\nslope",  "wave\nexposure","elevation", 
               "island\nslope","Bog\narea")

lay_names_veg_40m<-get_layout("", "", "", "", "", "","","","","","","","","","","","","","","","","","","","","","","","","","","","", "","", "","d15n_40m\nsoil",   "","","", "","","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                          "wrack","","", "","","","","", "","","","","","", "","", "shell","","","","","","", "fish\nbones", "","", "","","","","","","","","","","","","","","","", "","","","","", "","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                          "","", "","","","","","","","","","", "eagles","","","","","","","ravens" ,"","","","","","","otter","","","","","","","","","","","","","","","","","","", "midden","","","","","","","","","","","","","","","","","","","", "","","","","","","","","","","","","","",
                          "","","","","","","","","","","","","","","marine\ninvert","","","","","","","","","","", "fish","","","","","","","","","","","", "","","","","","","","","","","","","","","","","","","","","","","canopy\nheight","","","","","","","","","","","","","","","","","","","",
                          "fucus","","","", "","","kelp","","","", "","","eelgrass","","","","","","","beach\nslope","","","","","wave\nexposure","","","","", "sandy","","","","","","","", "transect\nslope","","","","","","","","","","","","edge\neffects","","","","","","Area","","","", "","","Bog\narea","","","","","", "neighb","","","","","", "elevation", "","","","","", "island\nslope", rows=5)

semPaths(fit.adj.mitml.veg_40m, what="std",  layout=lay_names_veg_40m, intercepts=FALSE, residuals=FALSE,
         groups=grps, color=colour_group, exoCov = FALSE, esize=2, nodeLabels = nodelab_veg_40m, legend=FALSE, 
         filetype="tiff", filename="Food web idea/Plots/SEM/SEM_dN15_40m_full", width=8, height=4)

semPaths(fit.adj.mitml.veg_40m, what="path",  layout=lay_names_veg_40m, intercepts=FALSE, residuals=FALSE,
         groups=grps, color=colour_group, exoCov=FALSE, nodeLabels = nodelab_veg_40m, legend=FALSE, 
         filetype="tiff", filename="Food web idea/Plots/SEM/SEM_dN15_40m_grey", width=8, height=4)


#plot only significant paths
fit_40m <- fit.adj.mitml.veg_40m
lavaan::standardizedSolution(fit_40m) %>% dplyr::filter(!is.na(pvalue)) %>% arrange(desc(pvalue)) %>% mutate_if("is.numeric","round",3) %>% select(-ci.lower,-ci.upper,-z)
pvalue_cutoff <- 0.05
obj_40m <- semPlot:::semPlotModel(fit_40m)

# save a copy of the original, so we can compare it later and be sure we removed only what we intended to remove
original_Pars <- obj_40m@Pars
check_Pars <- obj_40m@Pars %>% dplyr::filter(!(edge %in% c("int","<->") | lhs == rhs)) # this is the list of paramater to sift thru
keep_Pars <- obj_40m@Pars %>% dplyr::filter(edge %in% c("int","<->") | lhs == rhs) # this is the list of paramater to keep asis
test_against <- lavaan::standardizedSolution(fit_40m) %>% dplyr::filter(pvalue < pvalue_cutoff, rhs != lhs)
test_against_rev <- test_against %>% dplyr::rename(rhs2 = lhs,   # for some reason, the rhs and lhs are reversed in the standardizedSolution() output, for some of the values
                                                   lhs = rhs) %>% # I'll have to reverse it myself, and test against both orders
        dplyr::rename(rhs = rhs2)
checked_Pars <-
        check_Pars %>% semi_join(test_against, by = c("lhs", "rhs")) %>% bind_rows(
                check_Pars %>% semi_join(test_against_rev, by = c("lhs", "rhs"))
        )

obj_40m@Pars <- keep_Pars %>% bind_rows(checked_Pars)
anti_join(original_Pars,obj_40m@Pars)

semPaths(obj_40m, what="std",  layout=lay_names_veg_40m, intercepts=FALSE, residuals=FALSE,          
         groups=grps,  color=colour_group, esize=2, nodeLabels = nodelab_veg_40m, legend=FALSE,  
         filetype="tiff", filename="Food web idea/Plots/SEM/SEM_dN15_40m_sig", width=8, height=4)












############## Comparison models: 

N15_model_wrack<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
          
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE + elevation_max
          
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height

        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000

        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 

        pres_marine_invert ~ eagles + ravens + pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden

        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden

        c.d15n ~ w1*c.log_site_mean_by_tran + 0*c.distance_to_midden + 0*pres_marine_invert + 0*pres_fish + 0*c.slope_degrees  + 0*c.WAVE_EXPOSURE + 0*pres_otter + 0*ravens + 0*eagles + 0*elevation_max + 0*c.slope_isl  + 0*c.log_Bog_area + 0*c.CHM_mean_height

        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 

'
lavaan_fit_model_wrack<-sem(N15_model_wrack, data=master_transec_sem_subset_veg_centered)
fit.adj.mitml.wrack<-lavaan.survey(lavaan.fit=lavaan_fit_model_wrack, survey.design = design_imp_mitml_veg, estimator="MLMVS")
summary(fit.adj.mitml.wrack, standardized=T)


N15_model_human<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE + elevation_max
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000
        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 
        pres_marine_invert ~ eagles + ravens + pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden
        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden
        c.d15n ~ 0*c.log_site_mean_by_tran + c.distance_to_midden + 0*pres_marine_invert + 0*pres_fish + 0*c.slope_degrees  + 0*c.WAVE_EXPOSURE + 0*pres_otter + 0*ravens + 0*eagles + 0*elevation_max + 0*c.slope_isl  + 0*c.log_Bog_area + 0*c.CHM_mean_height
        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 
'
lavaan_fit_model_human<-sem(N15_model_human, data=master_transec_sem_subset_veg_centered)
fit.adj.mitml.human<-lavaan.survey(lavaan.fit=lavaan_fit_model_human, survey.design = design_imp_mitml_veg, estimator="MLMVS")
summary(fit.adj.mitml.human, standardized=T)

N15_model_otter<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE + elevation_max
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000
        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 
        pres_marine_invert ~ eagles + ravens + pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden
        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden
        c.d15n ~ 0*c.log_site_mean_by_tran + 0*c.distance_to_midden + 0*pres_marine_invert + 0*pres_fish + 0*c.slope_degrees  + 0*c.WAVE_EXPOSURE + pres_otter + 0*ravens + 0*eagles + 0*elevation_max + 0*c.slope_isl  + 0*c.log_Bog_area + 0*c.CHM_mean_height
        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 
'
lavaan_fit_model_otter<-sem(N15_model_otter, data=master_transec_sem_subset_veg_centered)
fit.adj.mitml.otter<-lavaan.survey(lavaan.fit=lavaan_fit_model_otter, survey.design = design_imp_mitml_veg, estimator="MLMVS")
summary(fit.adj.mitml.otter, standardized=T)


N15_model_otter_shell<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE + elevation_max
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000
        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 
        pres_marine_invert ~ 0*eagles + 0*ravens + pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden
        pres_fish ~ 0*eagles + 0*ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden
        c.d15n ~ 0*c.log_site_mean_by_tran + 0*c.distance_to_midden + pres_marine_invert + pres_fish + 0*c.slope_degrees  + 0*c.WAVE_EXPOSURE + pres_otter + 0*ravens + 0*eagles + 0*elevation_max + 0*c.slope_isl  + 0*c.log_Bog_area + 0*c.CHM_mean_height
        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 
'
lavaan_fit_model_otter_shell<-sem(N15_model_otter_shell, data=master_transec_sem_subset_veg_centered)
fit.adj.mitml.otter_shell<-lavaan.survey(lavaan.fit=lavaan_fit_model_otter_shell, survey.design = design_imp_mitml_veg, estimator="MLMVS")
summary(fit.adj.mitml.otter_shell, standardized=T)

N15_model_bird_otter_shell<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE + elevation_max
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000
        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 
        pres_marine_invert ~ eagles + ravens + pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden
        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden
        c.d15n ~ 0*c.log_site_mean_by_tran + 0*c.distance_to_midden + pres_marine_invert + pres_fish + 0*c.slope_degrees  + 0*c.WAVE_EXPOSURE + pres_otter + ravens + eagles + 0*elevation_max + 0*c.slope_isl  + 0*c.log_Bog_area + 0*c.CHM_mean_height
        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 
'
lavaan_fit_model_bird_otter_shell<-sem(N15_model_bird_otter_shell, data=master_transec_sem_subset_veg_centered)
fit.adj.mitml.bird_otter_shell<-lavaan.survey(lavaan.fit=lavaan_fit_model_bird_otter_shell, survey.design = design_imp_mitml_veg, estimator="MLMVS")
summary(fit.adj.mitml.bird_otter_shell, standardized=T)

N15_model_bird_shell<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE + elevation_max
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000
        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 
        pres_marine_invert ~ eagles + ravens + 0*pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden
        pres_fish ~ eagles + ravens + 0*pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden
        c.d15n ~ 0*c.log_site_mean_by_tran + 0*c.distance_to_midden + pres_marine_invert + pres_fish + 0*c.slope_degrees  + 0*c.WAVE_EXPOSURE + 0*pres_otter + ravens + eagles + 0*elevation_max + 0*c.slope_isl  + 0*c.log_Bog_area + 0*c.CHM_mean_height
        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 
'
lavaan_fit_model_bird_shell<-sem(N15_model_bird_shell, data=master_transec_sem_subset_veg_centered)
fit.adj.mitml.bird_shell<-lavaan.survey(lavaan.fit=lavaan_fit_model_bird_shell, survey.design = design_imp_mitml_veg, estimator="MLMVS")
summary(fit.adj.mitml.bird_shell, standardized=T)

N15_model_bird<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE + elevation_max
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000
        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 
        pres_marine_invert ~ eagles + ravens + pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden
        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden
        c.d15n ~ 0*c.log_site_mean_by_tran + 0*c.distance_to_midden +  0*pres_marine_invert +  0*pres_fish + 0*c.slope_degrees  + 0*c.WAVE_EXPOSURE + 0*pres_otter + ravens + eagles + 0*elevation_max + 0*c.slope_isl  + 0*c.log_Bog_area + 0*c.CHM_mean_height
        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 
'
lavaan_fit_model_bird<-sem(N15_model_bird, data=master_transec_sem_subset_veg_centered)
fit.adj.mitml.bird<-lavaan.survey(lavaan.fit=lavaan_fit_model_bird, survey.design = design_imp_mitml_veg, estimator="MLMVS")
summary(fit.adj.mitml.bird, standardized=T)


N15_model_biogeog<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE + elevation_max
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000
        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 
        pres_marine_invert ~ eagles + ravens + 0*pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden
        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden
        c.d15n ~ 0*c.log_site_mean_by_tran + 0*c.distance_to_midden + 0*pres_marine_invert + 0*pres_fish + c.slope_degrees  + c.WAVE_EXPOSURE + 0*pres_otter + 0*ravens + 0*eagles + elevation_max + c.slope_isl  + c.log_Bog_area + c.CHM_mean_height
        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 
'
lavaan_fit_model_biogeog<-sem(N15_model_biogeog, data=master_transec_sem_subset_veg_centered)
fit.adj.mitml.biogeog<-lavaan.survey(lavaan.fit=lavaan_fit_model_biogeog, survey.design = design_imp_mitml_veg, estimator="MLMVS")
summary(fit.adj.mitml.biogeog, standardized=T)

N15_model_vectors<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE + elevation_max
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000
        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 
        pres_marine_invert ~ eagles + ravens + pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden
        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden
        c.d15n ~ 0*c.log_site_mean_by_tran + c.distance_to_midden +  pres_marine_invert +  pres_fish + 0*c.slope_degrees  + 0*c.WAVE_EXPOSURE + pres_otter + ravens + eagles + 0*elevation_max + 0*c.slope_isl  + 0*c.log_Bog_area + 0*c.CHM_mean_height
        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 
'
lavaan_fit_model_vectors<-sem(N15_model_vectors, data=master_transec_sem_subset_veg_centered)
fit.adj.mitml.vectors<-lavaan.survey(lavaan.fit=lavaan_fit_model_vectors, survey.design = design_imp_mitml_veg, estimator="MLMVS")
summary(fit.adj.mitml.vectors, standardized=T)

N15_model_passive<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE + elevation_max
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000
        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 
        pres_marine_invert ~ eagles + ravens + 0*pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden
        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden
        c.d15n ~ c.log_site_mean_by_tran + 0*c.distance_to_midden + 0*pres_marine_invert + 0*pres_fish + c.slope_degrees  + c.WAVE_EXPOSURE + 0*pres_otter + 0*ravens + 0*eagles + elevation_max + c.slope_isl  + c.log_Bog_area + c.CHM_mean_height
        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 
'
lavaan_fit_model_passive<-sem(N15_model_passive, data=master_transec_sem_subset_veg_centered)
fit.adj.mitml.passive<-lavaan.survey(lavaan.fit=lavaan_fit_model_passive, survey.design = design_imp_mitml_veg, estimator="MLMVS")
summary(fit.adj.mitml.passive, standardized=T)


AIC(fit.adj.mitml.veg)
AIC(fit.adj.mitml.wrack)
AIC(fit.adj.mitml.human)
AIC(fit.adj.mitml.otter)
AIC(fit.adj.mitml.bird)
AIC(fit.adj.mitml.otter_shell)
AIC(fit.adj.mitml.bird_shell)
AIC(fit.adj.mitml.bird_otter_shell)
AIC(fit.adj.mitml.vectors)
AIC(fit.adj.mitml.passive)
AIC(fit.adj.mitml.biogeog)

lavTestLRT(fit.adj.mitml.veg, fit.adj.mitml.vectors)

