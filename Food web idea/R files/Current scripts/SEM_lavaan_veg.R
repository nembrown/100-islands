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
          
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees + c.WAVE_EXPOSURE
          
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height
        
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + elevation_max + c.CHM_mean_height

        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000

        c.distance_to_midden ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max 

        pres_marine_invert ~ eagles + ravens + pres_otter + c.log_bycatch_biomass_bym3_mean + c.distance_to_midden

        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.distance_to_midden

        c.d15n ~ w1*c.log_site_mean_by_tran + h1*c.distance_to_midden + pres_marine_invert + pres_fish + c.slope_degrees  + c.WAVE_EXPOSURE + pres_otter + ravens + eagles + elevation_max + c.slope_isl  + c.log_Bog_area + c.CHM_mean_height

        c.CHM_mean_height ~ c.NDVI_mean + c.log_Area + elevation_max + c.PA_norml + c.slope_isl + c.WAVE_EXPOSURE + c.log_Bog_area 

'




# ### error covariances not already accounted for in model (i.e. a similar processes underlying variables)
# #kelp/habitat/ocean process affects all of these:
# c.log_bycatch_biomass_bym3_mean ~~ c.log_fish_biomass_bym3_mean 
# 
# #I think the wrack process would also affect the marine debris that gets on shore 
# c.log_site_mean_by_tran ~~ pres_marine_invert + pres_fish
# 
# #similar drivers affect similar animal vectors
# eagles ~~ ravens 


#MI:
#wrack causes humans 


####need to add in ~~ between fish trap and fish ... etc ... but not sure if that' sokayu?? 


# Multiple imputation -----------------------------------------------------
# We need to use multiple imputation because the methods to understand our multi-level data dont accept missing data


# Option 1 mitml ----------------------------------------------------------
#OPTION 1 Multiple imputation using mimtl packacge which is two levels - 
#this is important for the log_Bog_area variable whic is at level 2 (island level) but has missing values) 


fml.veg <- list( c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean +
               c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.slope_degrees + c.log_site_mean_by_tran  + 
               c.d15n + c.distance_to_midden + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + pres_otter +
               pres_marine_invert + pres_fish + c.log_MEAN_rockarea2000  ~ 1 + (1|unq_isl) ,                                                 # Level 1
               c.log_Bog_area + c.log_Area +  c.PA_norml + ravens + eagles + c.log_Dist_Near + elevation_max + c.slope_isl  + c.CHM_mean_height + c.habitat_het + c.NDVI_mean ~ 1 )                          # Level 2


imp.veg <- jomoImpute(master_transec_sem_subset_veg_centered, formula=fml.veg, n.burn=500, n.iter=1000, m=20)
summary(imp.veg)

#problems the model sees: 
plot(imp.veg, trace="all", print="beta2", pos=c(1,7))
plot(imp.veg, trace="all", print="beta", pos=c(1,11))
plot(imp.veg, trace="all", print="psi", pos=c(17,6))
plot(imp.veg, trace="all", print="sigma", pos=c(4,2))

implist_veg <- mitmlComplete(imp.veg, "all")
#get it to talk to survey.design
imputed_mitml_veg <- imputationList(implist_veg)


# Option 3 Amelia ---------------------------------------------------------
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

inspect(fit.adj.mitml.veg, 'coverage')

standardizedSolution(fit.adj.mitml.veg)
rSquared = inspect(fit.adj.mitml.veg, "rsquare")
xVar = rSquared[1:(length(rSquared) - 1)]
# which should be excluded?
names(xVar[xVar < (mean(xVar) - (2 * sd(xVar)))])
# returns
character(0)
#use mitml because better adjustment - lower chi

#sometimes only amelia works.... 

plyr::arrange(modificationIndices(fit.adj.amelia.veg),mi, decreasing=TRUE)

mod.am<-as.data.frame(modificationIndices(fit.adj.amelia.veg))
str(mod.am)
mi_table<-plyr::arrange(mod.am, mi, decreasing=TRUE)
mi_table

mi_table2<-arrange(modificationIndices(fit.adj.amelia.veg),mi, decreasing=TRUE)
mi_table2

mi_table3<-arrange(modificationIndices(fit.adj.amelia.veg),mi, decreasing=TRUE)
mi_table3

fitMeasures(fit.adj.amelia.veg, c("cfi","rmsea","srmr"))

fitMeasures(fit.adj.mitml.veg)

#######

#semplot

grps<-list(Algae=c("c.log_MEAN_rockarea2000","c.log_site_mean_by_tran", "c.log_MEAN_kparea2k", "c.log_MEAN_egarea2k" ),
           Islchar=c("c.PA_norml", "c.SLOPE_degrees", "c.log_Area", "c.WAVE_EXPOSURE", "beachy_substrate", "c.slope_degrees","c.log_Bog_area", "c.log_Dist_Near", "elevation_max",   "c.slope_isl", "c.CHM_mean_height", "c.NDVI_mean"),
           Animalvect=c("marine_animal_biomass_shore","pres_otter", "pres_marine_invert", "pres_fish", "eagles","ravens" ),
           Humans=c("c.distance_to_midden"),
           Fish=c("c.log_fish_biomass_bym3_mean", "c.log_bycatch_biomass_bym3_mean"),
           outcome=c("c.d15n"))

#5152E0 #266DB8 #00A4EE #0AA5E2 #35CBBF #51C5B4 #A9BE93 #C2B3A2 #F78FB2 #F186AA #EC6D98

colour_group=c("#51C5B4", "#C2B3A2", "#EC6D98", "#DEAFFD", "#00A4EE", "#CD8958")

nodelab_veg<-c("fish", "marine\ninvert","otter", "ravens", "eagles" ,"wrack", "midden","shell", "fish\nbones", "d15N\nsoil", "canopy\nheight", 
            "kelp", "eelgrass", "fucus", "sandy", "Area", "transect\nslope", "wiggly", "neighb","beach\nslope",  "wave\nexposure","elevation", 
            "island\nslope","Bog\narea","NDVI")

lay_names_veg<-get_layout("", "", "", "", "", "","","","","","","","","","","","","","","","","","","","","","","","","","","","", "","", "","d15N\nsoil",   "","","", "","","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                      "wrack","","", "","","","","", "","","","","","", "","", "shell","","","","","","", "fish\nbones", "","", "","","","","","","","","","","","","","","","", "","","","","", "","","","", "","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",
                      "","", "","","","","","","","","","", "eagles","","","","","","","ravens" ,"","","","","","","otter","","","","","","","","","","","","","","","","","","", "midden","","","","","","","","","","","","","","","","","","","", "","","","","","","","","","","","","","",
                      "","","","","","","","","","","","","","","marine\ninvert","","","","","","","","","","", "fish","","","","","","","","","","","", "","","","","","","","","","","","","","","","","","","","","","","canopy\nheight","","","","","","","","","","","","","","","","","","","",
                      "fucus","","","", "","","kelp","","","", "","","eelgrass","","","","","","","beach\nslope","","","","","wave\nexposure","","","","", "sandy","","","","","","","", "transect\nslope","","","","","","NDVI","","","","","","wiggly","","","","","","Area","","","", "","","Bog\narea","","","","","", "neighb","","","","","", "elevation", "","","","","", "island\nslope", rows=5)

semPaths(fit.adj.mitml.veg, what="std",  layout=lay_names_veg, intercepts=FALSE, residuals=FALSE,
         groups=grps, exoVar = FALSE,  color=colour_group, esize=2, nodeLabels = nodelab_veg, legend=FALSE)

semPaths(fit.adj.mitml.veg, what="path",  layout=lay_names_veg, intercepts=FALSE, residuals=FALSE,
         groups=grps, exoVar = FALSE, color=colour_group, nodeLabels = nodelab_veg, legend=FALSE)


#############################
lavaan::parameterEstimates(fit.adj.mitml.veg) %>% dplyr::filter(!is.na(pvalue)) %>% arrange((pvalue)) %>% mutate_if("is.numeric","round",3) %>% dplyr::select(-ci.lower,-ci.upper,-z)
parameterEstimates(fit.adj.mitml.veg)
pvalue_cutoff <- 0.10
obj <- semPlot:::semPlotModel(fit.adj.mitml.veg)


# save a copy of the original, so we can compare it later and be sure we removed only what we intended to remove
original_Pars <- obj@Pars

check_Pars <- obj@Pars %>% dplyr::filter(!(edge %in% c("int","<->") | lhs == rhs)) # this is the list of paramater to sift thru
keep_Pars <- obj@Pars %>% dplyr::filter(edge %in% c("int","<->") | lhs == rhs) # this is the list of paramater to keep asis
test_against <- lavaan::parameterEstimates(fit.adj.mitml.veg) %>% dplyr::filter(pvalue < pvalue_cutoff)
test_against_rev <- test_against %>% rename(rhs2 = lhs,   # for some reason, the rhs and lhs are reversed in the standardizedSolution() output, for some of the values
                                            lhs = rhs) %>% # I'll have to reverse it myself, and test against both orders
  rename(rhs = rhs2)
checked_Pars <-
  check_Pars %>% semi_join(test_against, by = c("lhs", "rhs")) %>% bind_rows(
    check_Pars %>% semi_join(test_against_rev, by = c("lhs", "rhs"))
  )

obj@Pars <- keep_Pars %>% bind_rows(checked_Pars)

#let's verify by looking at the list of the edges we removed from the object
anti_join(original_Pars,obj@Pars)
#> Joining, by = c("label", "lhs", "edge", "rhs", "est", "std", "group", "fixed", "par")
#>   label  lhs edge rhs        est        std group fixed par
#> 1       gear   ~> mpg  0.1582792  0.0218978       FALSE   2
#> 2        cyl   ~> mpg -0.4956938 -0.1660012       FALSE   3

# great, let's plot
semPlot::semPaths(obj, "std",fade = F, residuals = F, intercepts=FALSE, nodeLabels = nodelab, layout=lay_names)


semPlot::semPaths(fit.adj.mitml.veg, "path",fade = F, residuals = F, intercepts=FALSE, label.cex=2, nCharNodes = 0, nodeLabels = 1:25)
semPlot::semPaths(fit.adj.mitml.veg, "path",fade = F, residuals = F, intercepts=FALSE, label.cex=2, nCharNodes = 0)

?semPlot::semPaths

semPaths(obj, what="std",  intercepts=FALSE, residuals=TRUE,
         groups=grps, layout=lay_alt, nCharNodes=0,  layoutSplit=TRUE, reorder=TRUE, 
         exoVar = FALSE,  pastel=TRUE, rainbowStart = 0.4, label.cex=2)

semPaths(fit.adj.mitml.veg, what="path",   residuals=FALSE,
         groups=grps, layout=lay_alt, nCharNodes=0,  layoutSplit=TRUE, reorder=TRUE, 
         exoVar = FALSE,  pastel=TRUE, rainbowStart = 0.4, label.cex=2, intercepts=FALSE)

?semPaths



####tidy SEM

graph_sem(model=fit.adj.amelia.veg, layout=lay_alt)

lay<-get_layout("", "", "", "", "", "", "c.d15n", "","", "", "",
                "c.log_site_mean_by_tran", "", "", "marine_animal_biomass_shore", "", "", "", "human_pres", "", "","",
                "", "", "",  "pres_marine_invert", "pres_fish","", "", "","cult_imp_plant_prop", "c.distance_to_midden","c.distance_to_fish",
                "","", "", "eagles","ravens" ,"pres_otter","","","","","", 
                "","","","c.log_fish_biomass_bym3_mean", "c.log_bycatch_biomass_bym3_mean","","","c.log_Bog_area","","","",
                "c.log_MEAN_rockarea2000", "c.log_MEAN_kparea2k", "c.log_MEAN_egarea2k", "c.PA_norml", "c.SLOPE_degrees", "c.log_Area", "c.WAVE_EXPOSURE", "beachy_substrate", "c.slope_degrees","c.log_Dist_Near", "elevation_max", rows=6)

lay_alt<-get_layout("", "", "", "", "", "","","","","","","","","","","","","","","","","","","","","","","", "","","","","","", "c.d15n", "","", "", "","","", "","","","","", "","",
                    "c.log_site_mean_by_tran","","", "","","","","", "","", "pres_marine_invert","","","","","","", "pres_fish", "","", "","","","", "","","","","","","","human_pres", "", "","","","","","","","", "","","","","","","",
                    "","", "","","","", "eagles","","","","","","","ravens" ,"","","","","","","pres_otter","","","","","","","","cult_imp_plant_prop","","","", "c.distance_to_midden","","","","c.distance_to_fish","","","","","","","","","", 
                    "","","","","","","","","c.log_fish_biomass_bym3_mean","","","","","","","","","","", "c.log_bycatch_biomass_bym3_mean","","","","","","","","","","","","","","","","","", "","","","","","","","","",
                    "c.log_MEAN_rockarea2000","","","", "","","c.log_MEAN_kparea2k","","","", "","","c.log_MEAN_egarea2k","","", "","","","","","","","","","","","","","c.SLOPE_degrees","c.WAVE_EXPOSURE", "beachy_substrate","","","","","","","", "c.slope_degrees","","c.PA_norml","c.log_Area",  "c.log_Dist_Near", "elevation_max",  "c.slope_isl","", "c.log_Bog_area", rows=5)

###lavaan plot
lavaanPlot(model = fit.adj.mitml.veg,
           node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE)


