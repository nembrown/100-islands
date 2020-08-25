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
sem_variables_names<-c("node", "unq_tran","unq_isl", "log_fish_biomass_bym3_mean", "log_bycatch_biomass_bym3_mean",
                       "SLOPE_degrees", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope_degrees",
                       "ravens", "cult_imp_plant_prop", "d15n", "distance_to_midden",
                       "distance_to_fish", "PA_norml", "log_site_mean_by_tran", "log_MEAN_kparea2k", "log_MEAN_egarea2k", "pres_otter", 
                       "pres_marine_invert", "pres_fish", "eagles", "log_Bog_area", "log_Dist_Near", "log_MEAN_rockarea2000" ,"elevation_max", 
                       "slope_isl", "CHM_mean_height")



master_transec_sem_subset<-master_transect[, colnames(master_transect) %in% sem_variables_names]
master_transec_sem_subset$unq_tran<-factor(master_transec_sem_subset$unq_tran, ordered=TRUE)
master_transec_sem_subset$unq_isl<-factor(master_transec_sem_subset$unq_isl, ordered=TRUE)
master_transec_sem_subset$node<-factor(master_transec_sem_subset$node)

master_transec_sem_subset_centered <- stdize(master_transec_sem_subset, 
                                             omit.cols = c("node", "unq_tran","unq_isl",  "beachy_substrate", "ravens",  "pres_otter",  
                                                           "pres_marine_invert", "pres_fish", "eagles", "northing", "easting", "cult_imp_plant_prop", "elevation_max"), 
                                             center = TRUE, scale = FALSE)

head(master_transec_sem_subset_centered)
# master_transec_sem_subset_centered<-master_transec_sem_subset_centered[complete.cases(master_transec_sem_subset_centered$pres_otter), ]
# master_transec_sem_subset_centered<-master_transec_sem_subset_centered[complete.cases(master_transec_sem_subset_centered$c.log_Bog_area), ]

# master_transec_sem_subset$beachy_substrate<-factor(master_transec_sem_subset$beachy_substrate, ordered=TRUE)
# master_transec_sem_subset$ravens<-factor(master_transec_sem_subset$ravens, ordered=TRUE)
# master_transec_sem_subset$eagles<-factor(master_transec_sem_subset$eagles, ordered=TRUE)
# master_transec_sem_subset$pres_marine_invert<-factor(master_transec_sem_subset$pres_marine_invert, ordered=TRUE)
# master_transec_sem_subset$pres_otter<-factor(master_transec_sem_subset$pres_otter, ordered=TRUE)
# master_transec_sem_subset $pres_fish<-factor(master_transec_sem_subset$pres_fish, ordered=TRUE)

#see structure of the data, which variables have missing data
summary(master_transec_sem_subset_centered)
md.pattern(master_transec_sem_subset_centered)


# Simple non-hierarchical model  -------------------------------------------------------
#Will combine this with survey.design to get hierachical model

N15_model_simple_centered<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
          
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near
          
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml
        
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml

        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000

        human_pres ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max

        marine_animal_biomass_shore ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean 

        c.d15n ~ a1*c.log_site_mean_by_tran + h1*human_pres + o1*marine_animal_biomass_shore + c.slope_degrees + c.log_Bog_area + c.WAVE_EXPOSURE + pres_otter + ravens + eagles + elevation_max

        c.log_Bog_area ~ c.log_Area + c.slope_degrees
        
        #latent variables measurement models
        human_pres =~ c.distance_to_midden + c.distance_to_fish + cult_imp_plant_prop 
        marine_animal_biomass_shore =~ pres_marine_invert + pres_fish 
        
        ### error covariances not already accounted for in model
        c.log_bycatch_biomass_bym3_mean ~~ c.log_fish_biomass_bym3_mean
        pres_marine_invert ~~ c.distance_to_midden
        pres_fish ~~ c.distance_to_fish
 
                                                    '

N15_model_simple_centered_alt<-'
          
        c.log_fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000
        
        c.log_bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate
          
        pres_otter ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml + c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + beachy_substrate + c.SLOPE_degrees
          
        ravens ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.CHM_mean_height
        
        eagles ~  c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.CHM_mean_height

        c.log_site_mean_by_tran ~  c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees + c.log_MEAN_rockarea2000

        human_pres ~ c.log_Dist_Near + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.log_MEAN_rockarea2000 + c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.slope_degrees + c.PA_norml + beachy_substrate + elevation_max

        pres_marine_invert ~ eagles + ravens + pres_otter + c.log_bycatch_biomass_bym3_mean 

        pres_fish ~ eagles + ravens + pres_otter + c.log_fish_biomass_bym3_mean 

        c.d15n ~ w1*c.log_site_mean_by_tran + h1*human_pres + pres_marine_invert + pres_fish + c.slope_degrees + c.log_Bog_area + c.WAVE_EXPOSURE + pres_otter + ravens + eagles + elevation_max + c.slope_isl  + c.CHM_mean_height

        #latent variables measurement models
        human_pres =~ c.distance_to_midden + c.distance_to_fish + cult_imp_plant_prop 

        ### error covariances not already accounted for in model
        c.log_bycatch_biomass_bym3_mean ~~ c.log_fish_biomass_bym3_mean
        pres_marine_invert ~~ c.distance_to_midden
        pres_fish ~~ c.distance_to_fish
        pres_marine_invert ~~ c.log_site_mean_by_tran
        pres_fish  ~~ c.log_site_mean_by_tran
                                                    '


#        c.log_bycatch_biomass_bym3_mean ~~ pres_marine_invert
#c.log_fish_biomass_bym3_mean ~~ pres_fish  

# Multiple imputation -----------------------------------------------------
# We need to use multiple imputation because the methods to understand our multi-level data dont accept missing data


# Option 1 mitml ----------------------------------------------------------
#OPTION 1 Multiple imputation using mimtl packacge which is two levels - 
#this is important for the log_Bog_area variable whic is at level 2 (island level) but has missing values) 
fml <- list( c.log_fish_biomass_bym3_mean + c.log_bycatch_biomass_bym3_mean +
               c.SLOPE_degrees  + c.WAVE_EXPOSURE + beachy_substrate + c.slope_degrees + c.log_site_mean_by_tran + cult_imp_plant_prop + 
               c.d15n + c.distance_to_midden +
               c.distance_to_fish + c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + pres_otter +
               pres_marine_invert + pres_fish + c.log_MEAN_rockarea2000  ~ 1 + (1|unq_isl) ,                                                 # Level 1
               c.log_Bog_area + c.log_Area +  c.PA_norml + ravens + eagles + c.log_Dist_Near + elevation_max + c.slope_isl  + c.CHM_mean_height ~ 1 )                          # Level 2


imp <- jomoImpute(master_transec_sem_subset_centered, formula=fml, n.burn=5000, n.iter=250, m=20)
summary(imp)

#problems the model sees: 
plot(imp, trace="all", print="beta2", pos=c(1,1))
plot(imp, trace="all", print="beta", pos=c(1,11))
plot(imp, trace="all", print="psi", pos=c(6,6))
plot(imp, trace="all", print="sigma", pos=c(8,6))

implist <- mitmlComplete(imp, "all")
#get it to talk to survey.design
imputed_mitml <- imputationList(implist)


# Option 2 mice ---------------------------------------------------------
?mice
#this is single level 
imputed_mice <- mice(master_transec_sem_subset_centered, method='cart',m=20)
#for surveydesign later:
imputed_mice_2<- imputationList(imputed_mice)
#i think cart is the wrong method but won't work with others


#With mice - but imputation happens in the model using the runMI function mitools - this only works with sem instead of lavaan call
fit.mi.mice<-runMI(N15_model_simple_centered, master_transec_sem_subset_centered, fun = "sem",m=20,  miArgs= list(method='cart'),
      miPackage = "mice", seed = 12345)
#this doesn't work - didn't converge

# Option 3 Amelia ---------------------------------------------------------
set.seed(12345)
HS.amelia <- amelia(master_transec_sem_subset_centered, m = 20, idvars = c("unq_tran", "unq_isl", "node"), p2s = FALSE)
imps <- HS.amelia$imputations
imps_amelia<-imputationList(imps)

#With Amelia - but imputation happens in the model - this works with sem call instead of lavaan call
fit.mi.amelia<-runMI(N15_model_simple_centered, master_transec_sem_subset_centered, fun = "sem",m=20, miArgs=list(idvars = c("unq_tran", "unq_isl", "node")),
      miPackage = "Amelia", seed = 12345)
summary(fit.mi.amelia)
#this works/runs

# Running model and adding survey design ----------------------------------

#run basic empty model then update with survey design
lavaan_fit_model<-sem(N15_model_simple_centered_alt, data=master_transec_sem_subset_centered, std.lv=TRUE)
summary(lavaan_fit_model)

varTable(lavaan_fit_model)
eigen(inspect(lavaan_fit_model, "cov.lv"))$values 

str(master_transec_sem_subset_centered)
imps_str<-as.data.frame(imps[[1]])
imps_cov<-cov(imps_str[,-c(1,2,14)])
View(imps_cov)

imps_cov<-as.matrix(imps_cov)
det(imps_cov)
solve(imps_cov)

# Survey.design with mitml and amelia - mice wasn't working 
design_imp_amelia<-svydesign(ids=~unq_isl, strata=~node, data=imps_amelia)
design_imp_mitml<-svydesign(ids=~unq_isl, strata=~node, data=imputed_mitml)

fit.adj.amelia<-lavaan.survey(lavaan.fit=lavaan_fit_model, survey.design = design_imp_amelia, estimator="MLMVS")
summary(fit.adj.amelia, standardized=T)

fit.adj.mitml<-lavaan.survey(lavaan.fit=lavaan_fit_model, survey.design = design_imp_mitml, estimator="MLMVS")
summary(fit.adj.mitml, standardized=T)

inspect(fit.adj.mitml, 'coverage')

standardizedSolution(fit.adj.mitml)

#use mitml because better adjustment - lower chi

#sometimes only amelia works.... 

plyr::arrange(modificationIndices(fit.adj.amelia),mi, decreasing=TRUE)

mod.am<-as.data.frame(modificationIndices(fit.adj.amelia))
str(mod.am)
mi_table<-plyr::arrange(mod.am, mi, decreasing=TRUE)
mi_table

mi_table2<-arrange(modificationIndices(fit.adj.amelia),mi, decreasing=TRUE)
mi_table2

mi_table3<-arrange(modificationIndices(fit.adj.amelia),mi, decreasing=TRUE)
mi_table3


#semplot

semPaths(fit.adj.amelia, what="std",  intercepts=FALSE, residuals=FALSE,
         groups=grps, layout=lay_alt, nCharNodes=0,  layoutSplit=TRUE, reorder=TRUE, 
         exoVar = FALSE,  pastel=TRUE, rainbowStart = 0.4, label.cex=2)


grps<-list(Algae=c("c.log_MEAN_rockarea2000","c.log_site_mean_by_tran", "c.log_MEAN_kparea2k", "c.log_MEAN_egarea2k" ),
           Islchar=c("c.PA_norml", "c.SLOPE_degrees", "c.log_Area", "c.WAVE_EXPOSURE", "beachy_substrate", "c.slope_degrees","c.log_Bog_area", "c.log_Dist_Near", "elevation_max",   "c.slope_isl", "c.CHM_mean_height"),
           Animalvect=c("marine_animal_biomass_shore","pres_otter", "pres_marine_invert", "pres_fish", "eagles","ravens" ),
           Humans=c("human_pres","cult_imp_plant_prop", "c.distance_to_midden","c.distance_to_fish"),
           Fish=c("c.log_fish_biomass_bym3_mean", "c.log_bycatch_biomass_bym3_mean"),
           outcome=c("c.d15n"))

nodelabels<-c("c.log_MEAN_rockarea2000"="Fucus","c.log_site_mean_by_tran"="Wrack", "c.log_MEAN_kparea2k"="Kelp", "c.log_MEAN_egarea2k"="eelgrass",
"c.PA_norml" = "wigglyness", "c.SLOPE_degrees"="beach_slope", "c.log_Area"="Isl_Area", "c.WAVE_EXPOSURE"="Exposure", "c.slope_degrees"="land_slope","c.log_Bog_area"="Bog_area", "c.log_Dist_Near"="Nearest_neighb",
"marine_animal_biomass_shore"="marine_debris","pres_otter"="otters", "pres_marine_invert"="marine_inverts", "pres_fish"="fish_shore", 
"cult_imp_plant_prop"="cultural_plants", "c.distance_to_midden"="midden","c.distance_to_fish"="fish_trap",
"c.log_fish_biomass_bym3_mean"="fish_biomass", "c.log_bycatch_biomass_bym3_mean"="invert_biomass","c.d15n"="d15N_soil", "eagles"="eagles","ravens"="ravens", "elevation_max"="isl_elevation", "human_pres"="human_pres", "beachy_substrate"="beach")

nodelabels<-c("", "", "", "", "", "", "d15n", "","", "", "",
  "wrack", "", "", "marine_debris", "", "", "", "human_pres", "", "","",
  "", "", "",  "pres_marine_invert", "pres_fish","", "", "","cultural_plants", "midden","fish_trap",
  "","", "", "eagles","ravens" ,"otter","","","","","", 
  "","","","fish_biomass", "bycatch_biomass","","","Bog_area","","","",
  "fucus", "kelp", "eelgrass", "PA_norml", "beach_slope", "Area", "WAVE_EXPOSURE", "beach", "slope_terr","Nearest_neighb", "elevation_max")



matrix_nodes<-cbind(c(-1,-1,0,0,-1,1), c(1,0,-1,1,-1,0))
rownames(matrix_nodes)<-c("Algae", "Islchar", "Animalvect", "Humans", "Fish", "outcome")
colnames(matrix_nodes)<-c("x", "y")

matrix_2<-c(1,0,0,0,0,2,0,4,0,6,5,0,3,0,0)

matrix_2<-c("Algae",0,0,0,0,"Islchar",0,"Humans",0,"outcome","Fish",0,"Animalvect",0,0)

###lavaan plot
lavaanPlot(model = fit.adj.mitml,
           node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE)

?lavaanPlot

####tidy SEM

graph_sem(model=fit.adj.amelia, layout=lay_alt)

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
                "c.log_MEAN_rockarea2000","","","", "","","c.log_MEAN_kparea2k","","","", "","","c.log_MEAN_egarea2k","","", "","","","","","","","","","","","","","c.SLOPE_degrees","c.WAVE_EXPOSURE", "beachy_substrate","","","","","","","", "c.slope_degrees","","c.PA_norml","c.log_Area",  "c.log_Dist_Near", "elevation_max",  "c.slope_isl", "c.CHM_mean_height","c.log_Bog_area", rows=5)


 

