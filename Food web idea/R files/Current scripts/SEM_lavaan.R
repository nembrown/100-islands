# updated since taking the workshop

# load libraries and data -------------------------------------------------------
library(semTools)
library(mitml)
library(lavaan)
library(lavaan.survey)
library(semPlot)
library(MuMIn)
library(mice)
library(ggplot2)
master_transect<-read.csv("C:Biodiversity idea//Output files//master_transect.csv")
# View(master_transect)

# # pair down the variables
sem_variables_names<-c( "unq_tran","unq_isl", "fish_biomass_bym3_mean", "bycatch_biomass_bym3_mean",
                        "SLOPE_degrees", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope_degrees",
                        "ravens", "cult_imp_plant_richness", "d15n", "distance_to_midden",
                        "distance_to_fish", "PA_norml", "log_site_mean_by_tran", "log_MEAN_kparea2k", "log_MEAN_egarea2k", "pres_otter", 
                        "pres_marine_invert", "pres_fish", "eagles", "log_Bog_area", "northing", "easting")

master_transec_sem_subset<-master_transect[, colnames(master_transect) %in% sem_variables_names]
str(master_transec_sem_subset)
master_transec_sem_subset$unq_tran<-as.factor(master_transec_sem_subset$unq_tran)
master_transec_sem_subset$unq_isl<-as.factor(master_transec_sem_subset$unq_isl)

summary(master_transec_sem_subset)

View(master_transect)


fml <- list( fish_biomass_bym3_mean + bycatch_biomass_bym3_mean +
             SLOPE_degrees  + WAVE_EXPOSURE + beachy_substrate + slope_degrees + log_site_mean_by_tran + cult_imp_plant_richness + d15n + distance_to_midden +
             distance_to_fish + log_MEAN_kparea2k + log_MEAN_egarea2k + pres_otter +
             pres_marine_invert + pres_fish  ~ 1 + (1|unq_isl) ,                                                 # Level 1
             log_Bog_area + log_Area +  PA_norml + ravens + eagles  ~ 1 )                                        # Level 2

imp <- jomoImpute(master_transec_sem_subset, formula=fml, n.burn=5000, n.iter=250, m=20)
summary(imp)
#problems the model sees: 
plot(imp, trace="all", print="beta", pos=c(1,15))
plot(imp, trace="all", print="beta", pos=c(1,1))
plot(imp, trace="all", print="psi", pos=c(8,3))
plot(imp, trace="all", print="sigma", pos=c(13,5))

implist <- mitmlComplete(imp, "all")

View(implist[[1]])
#think about centering variables later - if there are no interactions (currently how it's described, it shouldn't be a problem)
# # center variables, calculate interaction terms, ignore byproducts
# center_colmeans <- function(x) {
#     xcenter = colMeans(x)
#     x - rep(xcenter, rep.int(nrow(x), ncol(x)))
# }
# 
# colnames.to.centre<-c("fish_biomass_bym3_mean", "bycatch_biomass_bym3_mean", "MEAN_kparea2k", "MEAN_egarea2k",
#                       "SLOPE_degrees", "log_Area", "WAVE_EXPOSURE", "beachy_substrate", "slope_degrees", "midden_feature_sem", 
#                       "fish_feature_sem", "cult_imp_plant_richness", "d15n", "distance_to_any_arch", "distance_to_midden",
#                       "distance_to_fish", "PA_norml", "log_site_mean_by_tran", "log_MEAN_kparea2k", "log_MEAN_egarea2k", "log_Bog_area")
# length(colnames.to.centre)
# colnames.to.centre[1]
# new2.implist <- within(implist,{
#                                 c.fish_biomass_bym3_mean<-fish_biomass_bym3_mean - mean(fish_biomass_bym3_mean)
#                                 c.bycatch_biomass_bym3_mean<-bycatch_biomass_bym3_mean - mean(bycatch_biomass_bym3_mean)
#                                 c.MEAN_kparea2k<-MEAN_kparea2k - mean(MEAN_kparea2k)
#                                 c.MEAN_egarea2k<-MEAN_egarea2k - mean(MEAN_egarea2k)
#                                 c.SLOPE_degrees<-SLOPE_degrees - mean(SLOPE_degrees)
#                                 c.fish_biomass_bym3_mean<-fish_biomass_bym3_mean - mean(fish_biomass_bym3_mean)
#                                 c.fish_biomass_bym3_mean<-fish_biomass_bym3_mean - mean(fish_biomass_bym3_mean)
#                                 c.fish_biomass_bym3_mean<-fish_biomass_bym3_mean - mean(fish_biomass_bym3_mean)
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

# md.pattern(master_transec_sem_subset_centered)
# imputed_transect <- mice(master_transec_sem_subset_centered, m=5, method = 'pmm', seed = 101)



master_transec_sem_subset_centered <- stdize(master_transec_sem_subset, omit.cols = c("northing", "easting","unq_tran","unq_isl", "prop_otter","pres_otter", "pres_marine_invert", "pres_fish", "ravens", "eagles"), center = TRUE, scale = FALSE)
head(master_transec_sem_subset_centered)
master_transec_sem_subset_centered_slim<-master_transec_sem_subset_centered[,-c(6:7)]

##3 I think I need to do this at the island levell to reduce the # of missing values.... ??
#what about PANOrml and distance to neighbours .... 


# Simple non-categorical model  -------------------------------------------------------

N15_model_simple_nocat<-'
          
        c.fish_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k
        
        c.bycatch_biomass_bym3_mean ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k
          
        pres_otter ~  c.fish_biomass_bym3_mean + c.bycatch_biomass_bym3_mean + c.log_Area + c.slope_degrees + c.PA_norml+ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k
          
        ravens ~  c.fish_biomass_bym3_mean + c.bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml
        
        eagles ~  c.fish_biomass_bym3_mean + c.bycatch_biomass_bym3_mean + c.log_Area + c.PA_norml

        c.log_site_mean_by_tran ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.SLOPE_degrees  + c.WAVE_EXPOSURE + c.beachy_substrate + c.log_Area + c.PA_norml + c.slope_degrees

        c.human_pres ~ c.log_MEAN_kparea2k + c.log_MEAN_egarea2k + c.fish_biomass_bym3_mean + c.bycatch_biomass_bym3_mean  + c.WAVE_EXPOSURE + c.log_Area + c.SLOPE_degrees + c.PA_norml

        c.marine_animal_biomass_shore ~ eagles + ravens + pres_otter  + c.human_pres + c.fish_biomass_bym3_mean + c.bycatch_biomass_bym3_mean

        c.d15n ~ a1*c.log_site_mean_by_tran + h1*c.human_pres + o1*c.marine_animal_biomass_shore + c.slope_degrees + c.log_Bog_area

        ### correlations not already accounted for in model
        pres_marine_invert ~~ c.bycatch_biomass_bym3_mean
        pres_fish ~~ c.fish_biomass_bym3_mean


        #latent variables measurement models
        c.human_pres =~ c.distance_to_midden + c.distance_to_fish + c.cult_imp_plant_richness 
        c.marine_animal_biomass_shore =~ pres_marine_invert + pres_fish 
        '

fit_simple_nocat <- sem(N15_model_simple_nocat, data=master_transec_sem_subset_centered, std.lv=TRUE, missing="ml.x")

design<-svydesign(ids=~unq_isl, nest=TRUE, data=master_transec_sem_subset_centered)
        
fit.adj<-lavaan.survey(lavaan.fit=fit_simple_nocat, survey.design = design, estimator="ML")

summary(fit_simple_nocat, standardized=T)

summary(fit_simple_nocat, standardized=T)

lavSpatialCorrect(fit_simple_nocat, master_transec_sem_subset_centered$northing, master_transec_sem_subset_centered$easting)

library(ape)

borRes <- as.data.frame(residuals(fit_simple_nocat , "casewise"))

#raw visualization of NDVI residuals
qplot(northing, easting, data=master_transec_sem_subset_centered, color=borRes$c.d15n, size=I(5)) +
    theme_bw(base_size=17) + 
    scale_color_gradient("d15N Residual", low="blue", high="yellow")


library(ape)
distMat <- as.matrix(dist(
    cbind(master_transec_sem_subset_centered$northing, master_transec_sem_subset_centered$easting)))

distsInv <- 1/distMat
diag(distsInv) <- 0
mi.ndvi <- Moran.I(borRes$c.d15n, distsInv, na.rm=TRUE)
mi.ndvi$observed
[1] 0.08014145
$expected
[1] -0.001879699
$sd
[1] 0.003986118
$p.value
[1] 0

modindices(fit_simple_nocat, sort.=TRUE, minimum.value=10)
coef(fit_simple_nocat )

#std.lv=TRUE, missing="ml"
#+ PA_norml

semPaths(fit_simple_nocat, whatLabels="est", intercepts="FALSE", layout="tree2")

varTable(fit_simple_nocat)
lavInspect(fit_simple_nocat, "cov.lv")

library(MVN)
source("./fitted_lavaan.R")
dist_resid <- residuals_lavaan(fit_simple_nocat)
mvn(dist_resid, mvnTest="mardia", univariatePlot = "qqplot")
####### doesn't work with latent

library(mvnormtest)
fitdata <- inspect(fit_simple_nocat, "data")
mshapiro.test(t(fitdata))
#doesn't work with latent i dont think


###############
# implist

N15_model_hierarch<-'
        
        #transect level (between) only transect level stuff
        level: 1  
        
        fish_biomass_bym3_mean ~ log_MEAN_kparea2k + log_MEAN_egarea2k
        
        bycatch_biomass_bym3_mean ~ log_MEAN_kparea2k + log_MEAN_egarea2k
          
        pres_otter ~  fish_biomass_bym3_mean + bycatch_biomass_bym3_mean + slope_degrees +  log_MEAN_kparea2k + log_MEAN_egarea2k
          
        ravens ~  fish_biomass_bym3_mean + bycatch_biomass_bym3_mean 
        
        eagles ~  fish_biomass_bym3_mean + bycatch_biomass_bym3_mean 

        log_site_mean_by_tran ~ log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE_degrees  + WAVE_EXPOSURE + beachy_substrate + slope_degrees

        human_pres_trans ~ log_MEAN_kparea2k + log_MEAN_egarea2k + fish_biomass_bym3_mean + bycatch_biomass_bym3_mean  + WAVE_EXPOSURE  + SLOPE_degrees 

        marine_animal_biomass_shore_trans ~ eagles + ravens + pres_otter  + human_pres_trans + fish_biomass_bym3_mean + bycatch_biomass_bym3_mean

        d15n ~ a1*log_site_mean_by_tran + h1*human_pres_trans + o1*marine_animal_biomass_shore_trans + slope_degrees

        ### correlations not already accounted for in model
        pres_marine_invert ~~ bycatch_biomass_bym3_mean
        pres_fish ~~ fish_biomass_bym3_mean


        #latent variables measurement models
        human_pres_trans =~ distance_to_midden + distance_to_fish + cult_imp_plant_richness 
        marine_animal_biomass_shore_trans =~ pres_marine_invert + pres_fish 
        
        #island level (within)
        level: 2
        
        pres_otter ~   log_Area + PA_norml
        ravens ~   log_Area + PA_norml
        eagles ~   log_Area + PA_norml
        log_site_mean_by_tran ~ log_Area + PA_norml 
        d15n ~  log_Bog_area
        
        '

fit_simple_hierarch <- semList(N15_model_hierarch, dataList=implist, cluster = "unq_isl", verbose = TRUE, optim.method = "em")
summary(fit_simple_hierarch)
modindices(fit_simple_nocat, sort.=TRUE, minimum.value=10)

diag(1,5)
diag(1,3)

#####


N15_model_simple_nocat_alt<-'
          
          human_pres ~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean + log_DistW_ML + Neighb_250 + beachy_substrate 

          marine_animal_biomass_shore ~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean + ravens + otter_pres_all 

          algae_biomass_shore ~ log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + beachy_substrate + Neighb_250
          
          log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_biomass_bym3_mean
          
          log_MEAN_kparea2k + log_MEAN_egarea2k ~ bycatch_biomass_bym3_mean
 
          d15n ~ a1*algae_biomass_shore + h1*human_pres + o1*marine_animal_biomass_shore


        #latent variables measurement models
        human_pres =~ distance_to_midden  + cult_imp_plant_richness 
        algae_biomass_shore =~ log_site_mean_by_tran + seaweed_all
        marine_animal_biomass_shore =~ marine_invert_pres_all + fish_all
        '

fit_simple_nocat_alt <- sem(N15_model_simple_nocat_alt, data=master_transect, missing="ml.x", std.lv=TRUE)
summary(fit_simple_nocat_alt, fit.measures=TRUE)
varTable(fit_simple_nocat_alt)

modI<-modificationIndices(fit_simple_nocat_alt, standardized=F)
modI[modI$mi<3,]


# Simple full model -------------------------------------------------------

N15_model_simple<-'
          
          human_pres ~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean  + WAVE_EXPOSURE + log_Area

          marine_animal_biomass_shore ~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean + ravens + otter_pres_all  + WAVE_EXPOSURE + human_pres

          algae_biomass_shore ~ log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + beachy_substrate
          
          d15n ~ a1*algae_biomass_shore + h1*human_pres + o1*marine_animal_biomass_shore

          log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_biomass_bym3_mean

          log_MEAN_kparea2k + log_MEAN_egarea2k ~ bycatch_biomass_bym3_mean


        #latent variables measurement models
        human_pres =~ midden_feature_sem + fish_feature_sem + cult_imp_plant_richness
        algae_biomass_shore =~ log_site_mean_by_tran + seaweed_all
        marine_animal_biomass_shore =~ marine_invert_pres_all + fish_all
        '

fit_simple <- sem(N15_model_simple, data=master_transect, missing="pairwise", fixed.x=FALSE, conditional.x=FALSE)
summary(fit_simple, fit.measures=TRUE)
varTable(fit_simple)


######################## 
N15_model_composite<-'#latent variables as responses

            #composite
            
            human_pres <~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean  + WAVE_EXPOSURE + log_Area
            
            marine_animal_biomass_shore <~ fish_biomass_bym3_mean + bycatch_biomass_bym3_mean + ravens + otter_pres_all  + WAVE_EXPOSURE + human_pres
            
            algae_biomass_shore <~ log_MEAN_kparea2k + log_MEAN_egarea2k + SLOPE  + WAVE_EXPOSURE + beachy_substrate
            
            
            
           # algae_biomass_shore + human_pres + marine_animal_biomass_shore ~ d15n
            
            # log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_biomass_bym3_mean
            # 
            # log_MEAN_kparea2k + log_MEAN_egarea2k ~ bycatch_biomass_bym3_mean


            #correlations
           # fish_biomass_bym3_mean ~~ bycatch_biomass_bym3_mean

            
            #latent variables measurement models
            human_pres =~ midden_feature_sem + fish_feature_sem + cult_imp_plant_richness + d15n
            algae_biomass_shore =~ log_site_mean_by_tran + seaweed_all + d15n
            marine_animal_biomass_shore =~ marine_invert_pres_all + fish_all + d15n
                                                '

fit2 <- sem(N15_model_novector2, data=master_transect,missing="ML")
summary(fit2, fit.measures=TRUE)


fit4 <- sem(N15_model_novector2, data=master_transect,estimator = "PML",missing = "available.cases",std.lv=TRUE, fixed.x=FALSE, conditional.x=FALSE, test = "none")

fit5 <- sem(N15_model_composite, data=master_transect,missing="pairwise", std.lv=TRUE)
summary(fit5)


# ordered=c("fish_all", "marine_invert_pres_all","midden_feature_sem","fish_feature_sem"),

#note: otter_pres can't be ordered bc it is exogenous.... 
#only order the endogenous (i.e. arrow going into) variables

#missing="pairwise", std.lv=TRUE, fixed.x=FALSE, conditional.x=FALSE
#stdv.lvn If TRUE, the metric of each latent variable is determined by fixing their variances to 1.0. 
#If FALSE, the metric of each latent variable is determined by fixing the factor loading 
#of the first indicator to 1.0. If there are multiple groups, std.lv = TRUE and "loadings" 
#is included in the group.label argument, then only the latent variances i of the first group 
#will be fixed to 1.0, while the latent variances of other groups are set free.




summary(fit4)
varTable(fit5)
lavTables(fit4)
print(modindices(fit3))

lavInspect(fit3, "cov.lv")

head(master_transect)



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

            fish_biomass_bym3_mean + fish_bycatch_biomass + ravens  + WAVE_EXPOSURE  ~ marine_animal_biomass_shore
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_biomass_bym3_mean
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_bycatch_biomass
            
            #correlations
            # fish_biomass_bym3_mean ~~ fish_bycatch_biomass
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

            human_pres <~ fish_biomass_bym3_mean + fish_bycatch_biomass  + WAVE_EXPOSURE + log_Area
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_biomass_bym3_mean
            
            log_MEAN_kparea2k + log_MEAN_egarea2k ~ fish_bycatch_biomass
            
            #correlations
             #fish_biomass_bym3_mean ~~ fish_bycatch_biomass

            
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
# human_pres <~ 0.16*fish_biomass_bym3_mean + -19*fish_bycatch_biomass  + 0.40*WAVE_EXPOSURE + -0.40*log_Area
# 
# human_pres ~ d15n
# '
# 
# comp_model2 <- sem(comp_formula2, data=master_transect, fixed.x=F)
# 
# 
# 
# cover_model <- lm(d15n ~ fish_biomass_bym3_mean + fish_bycatch_biomass  + WAVE_EXPOSURE + log_Area ,master_transect)
# 
# summary(cover_model)
