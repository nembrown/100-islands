setwd("C:/Users/Norah/Dropbox/Projects/100-islands/Biodiversity idea")

# install.packages("devtools")
# devtools::install_github("cardiomoon/ggiraphExtra")
# install.packages("installr")
library(installr) # install+load installr

# updateR()
library(ggiraphExtra)
library(bbmle) 
library(glmmTMB)
library(grid)
library(glmmADMB)
library(betareg)
library(lmtest)
library(fitdistrplus)
library(visreg)
library(lme4)
library(coefplot)
library(arm)
library(lmerTest)
library(boot)
library(MASS)
require(scales)
library(car)
library(knitr)
library(tidyverse)
library(kableExtra)
library(multcomp)
library(arm) ## for sim()
library(descr)  ## for LogRegR2
require(reshape2)
library(DHARMa)
library(gap)
library(qrnn)
library(mgcv)
library(colorspace)
library(gratia)
library(cowplot)
library(gtable)
library(gridExtra)
library(gamm4)

#install.packages('TMB', type = 'source')

# Reading data -------------------------------------------------------------------------
#Reading in the data and making scaled
fish_stats<-read.csv("C:Output files//fish_richness_merged_tran_isl.csv")
head(fish_stats)
fish_stats<-fish_stats[,-1]
# fish_stats<-fish_stats %>% filter(Distance < 1)
fish_stats_zscores$d15n<-as.numeric(fish_stats_zscores$d15n)

data_subset3 <- fish_stats[ , "fish_biomass_bym3_cat"]
fish_stats<- fish_stats[complete.cases(data_subset3), ] 


fish_stats$fish_abundance_bym3_log<-log(fish_stats$fish_abundance_bym3+1)
fish_stats<- fish_stats[complete.cases(fish_stats$fish_richness_corrected), ] 
fish_stats<- fish_stats[complete.cases(fish_stats$fish_abundance_bym3), ] 
fish_stats<- fish_stats[complete.cases(fish_stats$plant.richness), ] 

fish_stats_zscores<-fish_stats
fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)
fish_stats_zscores$fish_richness_corrected.unscaled <-fish_stats_zscores$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')
fish_stats_zscores$fish_richness_corrected<-as.numeric(fish_stats_zscores$fish_richness_corrected)

fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)
fish_stats_zscores$fish_biomass_bym3_mean.unscaled <-fish_stats_zscores$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')
fish_stats_zscores$fish_biomass_bym3_mean<-as.numeric(fish_stats_zscores$fish_biomass_bym3_mean)


fish_stats_zscores$fish_abundance_bym3_log<-scale(fish_stats$fish_abundance_bym3_log, center=TRUE, scale=TRUE)
fish_stats_zscores$fish_abundance_bym3_log.unscaled <-fish_stats_zscores$fish_abundance_bym3_log * attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:center')
fish_stats_zscores$fish_abundance_bym3_log<-as.numeric(fish_stats_zscores$fish_abundance_bym3_log)

fish_stats_zscores$fish_abundance_bym3<-scale(fish_stats$fish_abundance_bym3, center=TRUE, scale=TRUE)
fish_stats_zscores$fish_abundance_bym3.unscaled <-fish_stats_zscores$fish_abundance_bym3 * attr(fish_stats_zscores$fish_abundance_bym3, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3, 'scaled:center')
fish_stats_zscores$fish_abundance_bym3<-as.numeric(fish_stats_zscores$fish_abundance_bym3)


fish_stats_zscores_cat<-fish_stats_zscores %>% filter(fish_biomass_bym3_cat != "med fish biomass")
fish_stats_cat<-fish_stats %>% filter(fish_biomass_bym3_cat != "med fish biomass")

head(fish_stats_zscores_cat)
fish_stats_zscores_cat$fish_biomass_bym3_cat<-factor(fish_stats_zscores_cat$fish_biomass_bym3_cat)
levels(fish_stats_zscores_cat$fish_biomass_bym3_cat)

fish_stats_zscores_cat$log_Area<-scale(fish_stats_cat$log_Area, center=TRUE, scale=TRUE)
fish_stats_zscores_cat$log_Area.unscaled <-fish_stats_zscores_cat$log_Area * attr(fish_stats_zscores_cat$log_Area, 'scaled:scale') + attr(fish_stats_zscores_cat$log_Area, 'scaled:center')
fish_stats_zscores_cat$log_Area<-as.numeric(fish_stats_zscores_cat$log_Area)


# Shrub cover (total) vs. fish biomass ----------
ggplot(fish_stats_zscores, aes(y=shrub_cover, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$shrub_cover)

qqp(log(fish_stats_zscores$shrub_cover+1))

qqp(fish_stats_zscores$shrub_cover, "lnorm")

gamma.12.evenness<-fitdistr(fish_stats_zscores$shrub_cover+0.01, "gamma")
qqp(fish_stats_zscores$shrub_cover, "gamma", shape = gamma.12.evenness$estimate[[1]], rate = gamma.12.evenness$estimate[[2]])


# 
# #this is the right specification 
# lmer.shrub_cover.fishbiomass<-lmer(shrub_cover ~ fish_biomass_bym3_mean + (1|unq_isl) + (1|site), data=fish_stats_zscores, na.action=na.omit)
# glmm.shrub_cover.fishbiomass<-glmmTMB((shrub_cover) ~ fish_biomass_bym3_mean + (1|unq_isl)+ (1|site), data=fish_stats_zscores, family="gaussian", na.action=na.omit)
# 
# gamm4.shrub_cover.fishbiomass<- gamm4(shrub_cover ~ s(fish_biomass_bym3_mean), random=~(1|unq_isl)+ (1|site), data=fish_stats_zscores, family="gaussian")
# 
# gamm.shrub_cover.fishbiomass<- gamm(shrub_cover ~ s(fish_biomass_bym3_mean), random=~(1|unq_isl)+ (1|site), data=fish_stats_zscores, family="gaussian")
# 
# 
# gam.shrub_cover.fishbiomass<- gamm(shrub_cover ~ s(fish_biomass_bym3_mean) + s(unq_isl, bs="re")+s(site,bs="re"), data=fish_stats_zscores, method="REML")
# gam.shrub_cover.fishbiomass_3<- gam(shrub_cover ~ s(fish_biomass_bym3_mean, unq_isl, bs="re")+s(fish_biomass_bym3_mean,site,bs="re"), data=fish_stats_zscores, method="REML")
# gam.shrub_cover.fishbiomass_2<- gam(shrub_cover ~ s(fish_biomass_bym3_mean, unq_isl, bs="re")+s(site,bs="re"), data=fish_stats_zscores, method="REML")
# 
# AICtab(gam.shrub_cover.fishbiomass_2, gam.shrub_cover.fishbiomass,gam.shrub_cover.fishbiomass_3, glmm.shrub_cover.fishbiomass, lmer.shrub_cover.fishbiomass, gamm.shrub_cover.fishbiomass$mer)
# #or gam just specifying ransom effects.... as smooth
# 
# 
# AICtab(glmm.shrub_cover.fishbiomass, lmer.shrub_cover.fishbiomass, gamm.shrub_cover.fishbiomass$mer)
# 
# summary(gam.shrub_cover.fishbiomass)
# 
# plot(gam.shrub_cover.fishbiomass, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
# appraise(gam.shrub_cover.fishbiomass)
# qq_plot(gam.shrub_cover.fishbiomass, method = 'simulate')
# k.check(gam.shrub_cover.fishbiomass)
# 
# 
# # fam.gam.shrub_cover.fishbiomass <- family(gam.shrub_cover.fishbiomass)
# # fam.gam.shrub_cover.fishbiomass 
# # str(fam.gam.shrub_cover.fishbiomass )
# # ilink.gam.shrub_cover.fishbiomass <- fam.gam.shrub_cover.fishbiomass$linkinv
# # ilink.gam.shrub_cover.fishbiomass
# # 
# # Extracting coefficients and plotting
# want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
# mod.shrub_cover.fishbiomass<-gam.shrub_cover.fishbiomass
# ndata.shrub_cover.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
#                                                                unq_isl = unq_isl[want], site = site[want]))
# 
# ## add the fitted values by predicting from the model for the new data
# ndata.shrub_cover.fishbiomass <- add_column(ndata.shrub_cover.fishbiomass, fit = predict(mod.shrub_cover.fishbiomass, newdata = ndata.shrub_cover.fishbiomass, level=0))
#  ndata.shrub_cover.fishbiomass <- bind_cols(ndata.shrub_cover.fishbiomass, setNames(as_tibble(predict(mod.shrub_cover.fishbiomass, ndata.shrub_cover.fishbiomass,level=0, se.fit = TRUE)[1:2]),
#                                                     c('fit_link','se_link')))
# 
# 
# ndata.shrub_cover.fishbiomass <- mutate(ndata.shrub_cover.fishbiomass,
#                         fit_resp  = ilink.gam.shrub_cover.fishbiomass(fit_link),
#                         right_upr = ilink.gam.shrub_cover.fishbiomass(fit_link + (2 * se_link)),
#                         right_lwr = ilink.gam.shrub_cover.fishbiomass(fit_link - (2 * se_link)))
# 
# 
# fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)
# 
# ndata.shrub_cover.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.shrub_cover.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')

#lmer.shrub_cover.fishbiomass<-lmer(shrub_cover ~ fish_biomass_bym3_mean + (1|unq_tran) + (1|site), data=fish_stats_zscores, na.action=na.omit)
# lmer.shrub_cover.fishbiomass<-lmer(shrub_cover ~ fish_biomass_bym3_mean + (1|unq_isl/unq_tran) + (1|site), data=fish_stats_zscores, na.action=na.omit)
# glmm.shrub_cover.fishbiomass<-glmmTMB((shrub_cover+0.01) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="Gamma")
# warnings()

lme.shrub_cover.fishbiomass<-lme(shrub_cover ~ fish_biomass_bym3_mean, random=~1|unq_isl/unq_tran, data=fish_stats_zscores, na.action=na.omit)
# lme.shrub_cover.fishbiomass_2<-lme(shrub_cover ~ fish_biomass_bym3_mean, random=~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)



AICtab(lme.shrub_cover.fishbiomass_2, lme.shrub_cover.fishbiomass, glmm.shrub_cover.fishbiomass)

summary(lme.shrub_cover.fishbiomass)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.shrub_cover.fishbiomass,type=c("p","smooth")),
             plot(lme.shrub_cover.fishbiomass,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.shrub_cover.fishbiomass,resid(.,type="pearson")~fish_biomass_bym3_mean,
                  type=c("p","smooth")))
# Extracting coefficients and plotting
  want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
  mod.shrub_cover.fishbiomass<-lme.shrub_cover.fishbiomass 
  ndata.shrub_cover.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
                                                                  unq_isl = unq_isl[want]))
  
  ## add the fitted values by predicting from the model for the new data
  ndata.shrub_cover.fishbiomass <- add_column(ndata.shrub_cover.fishbiomass, fit = predict(mod.shrub_cover.fishbiomass, newdata = ndata.shrub_cover.fishbiomass, level=0))
  
  ###for lmes: from bolker: 
  #http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
  Designmat <- model.matrix(formula(mod.shrub_cover.fishbiomass)[-2], ndata.shrub_cover.fishbiomass)
  predvar <- diag(Designmat %*% vcov(mod.shrub_cover.fishbiomass) %*% t(Designmat)) 
  
  ndata.shrub_cover.fishbiomass$SE <- sqrt(predvar) 
  ndata.shrub_cover.fishbiomass$SE2 <- sqrt(predvar+mod.shrub_cover.fishbiomass$sigma^2)
  
  fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)
  
  ndata.shrub_cover.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.shrub_cover.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')
  
  
  # plot 
  plt.shrub_cover.fishbiomass <- ggplot(ndata.shrub_cover.fishbiomass, aes(x = fish_biomass_bym3_mean.unscaled, y = fit)) + 
    theme_classic()+
    geom_line(size=1.5, aes()) +
    geom_point(aes(y =shrub_cover), size=3, data = fish_stats_zscores)+
    xlab(expression("Fish biomass (g per m3)")) + ylab("Shrub cover (%)")+  
    scale_shape_manual(values=c(19))+
    geom_ribbon(data = ndata.shrub_cover.fishbiomass,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
    theme(legend.position="none")
  plt.shrub_cover.fishbiomass
  ggsave("C:Plots//Model-fitted//LME_shrub_cover_fish_biomass.png")


# Herb cover (total) vs. fish biomass ----------
ggplot(fish_stats_zscores, aes(y=herb_cover, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$herb_cover)
qqp(fish_stats_zscores$herb_cover, "lnorm")

# lme.herb_cover.fishbiomass<-lme(herb_cover ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lme.herb_cover.fishbiomass<-lme(herb_cover ~ fish_biomass_bym3_mean, random= ~1|unq_isl/unq_tran, data=fish_stats_zscores, na.action=na.omit)
# lme.herb_cover.fishbiomass_log<-lme(log(herb_cover+1) ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)


#glmm.herb_cover.fishbiomass<-glmmTMB((herb_cover+0.01) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(lme.herb_cover.fishbiomass, lme.herb_cover.fishbiomass_log)

summary(lme.herb_cover.fishbiomass)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.herb_cover.fishbiomass,type=c("p","smooth")),
             plot(lme.herb_cover.fishbiomass,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.herb_cover.fishbiomass,resid(.,type="pearson")~fish_biomass_bym3_mean,
                  type=c("p","smooth")),
             qqnorm(lme.herb_cover.fishbiomass,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.herb_cover.fishbiomass<-lme.herb_cover.fishbiomass 
ndata.herb_cover.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
                                                                 unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.herb_cover.fishbiomass <- add_column(ndata.herb_cover.fishbiomass, fit = predict(mod.herb_cover.fishbiomass, newdata = ndata.herb_cover.fishbiomass, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.herb_cover.fishbiomass)[-2], ndata.herb_cover.fishbiomass)
predvar <- diag(Designmat %*% vcov(mod.herb_cover.fishbiomass) %*% t(Designmat)) 

ndata.herb_cover.fishbiomass$SE <- sqrt(predvar) 
ndata.herb_cover.fishbiomass$SE2 <- sqrt(predvar+mod.herb_cover.fishbiomass$sigma^2)

fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)

ndata.herb_cover.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.herb_cover.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')


# plot 
plt.herb_cover.fishbiomass <- ggplot(ndata.herb_cover.fishbiomass, aes(x = fish_biomass_bym3_mean.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =herb_cover), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish biomass (g per m3)")) + ylab("Herb cover (%)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.herb_cover.fishbiomass,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.herb_cover.fishbiomass
ggsave("C:Plots//Model-fitted//LME_herb_cover_fish_biomass.png")

ggplot(fish_stats_zscores, aes(y=herb_cover, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$herb_cover)
qqp(fish_stats_zscores$herb_cover, "lnorm")


#Total plant cover -----
# lme.total_cover.fishbiomass<-lme(total_cover ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lme.total_cover.fishbiomass<-lme(total_cover ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# lme.total_cover.fishbiomass_log<-lme(log(total_cover+1) ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)


#glmm.total_cover.fishbiomass<-glmmTMB((total_cover+0.01) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(lme.total_cover.fishbiomass, lme.total_cover.fishbiomass_log)

summary(lme.total_cover.fishbiomass)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.total_cover.fishbiomass,type=c("p","smooth")),
             plot(lme.total_cover.fishbiomass,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.total_cover.fishbiomass,resid(.,type="pearson")~fish_biomass_bym3_mean,
                  type=c("p","smooth")),
             qqnorm(lme.total_cover.fishbiomass,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.total_cover.fishbiomass<-lme.total_cover.fishbiomass 
ndata.total_cover.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
                                                                unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.total_cover.fishbiomass <- add_column(ndata.total_cover.fishbiomass, fit = predict(mod.total_cover.fishbiomass, newdata = ndata.total_cover.fishbiomass, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.total_cover.fishbiomass)[-2], ndata.total_cover.fishbiomass)
predvar <- diag(Designmat %*% vcov(mod.total_cover.fishbiomass) %*% t(Designmat)) 

ndata.total_cover.fishbiomass$SE <- sqrt(predvar) 
ndata.total_cover.fishbiomass$SE2 <- sqrt(predvar+mod.total_cover.fishbiomass$sigma^2)

fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)

ndata.total_cover.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.total_cover.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')


# plot 
plt.total_cover.fishbiomass <- ggplot(ndata.total_cover.fishbiomass, aes(x = fish_biomass_bym3_mean.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =total_cover), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish biomass (g per m3)")) + ylab("Plant cover (%)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.total_cover.fishbiomass,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.total_cover.fishbiomass
ggsave("C:Plots//Model-fitted//LME_total_cover_fish_biomass.png")



#NDVI mean fish biomass ----
# lme.NDVI_mean.fishbiomass<-lme(NDVI_mean ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lm.NDVI_mean.fishbiomass<-lm(NDVI_mean ~ fish_biomass_bym3_mean, data=fish_stats_zscores)

# lme.NDVI_mean.fishbiomass_log<-lme(log(NDVI_mean+1) ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)


#glmm.NDVI_mean.fishbiomass<-glmmTMB((NDVI_mean+0.01) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(lme.NDVI_mean.fishbiomass, lme.NDVI_mean.fishbiomass_log)

summary(lm.NDVI_mean.fishbiomass)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.NDVI_mean.fishbiomass,type=c("p","smooth")),
             plot(lme.NDVI_mean.fishbiomass,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.NDVI_mean.fishbiomass,resid(.,type="pearson")~fish_biomass_bym3_mean,
                  type=c("p","smooth")),
             qqnorm(lme.NDVI_mean.fishbiomass,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))
# 
# # Extracting coefficients and plotting
# want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
# mod.NDVI_mean.fishbiomass<-lme.NDVI_mean.fishbiomass 
# ndata.NDVI_mean.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
#                                                                  unq_isl = unq_isl[want]))
# 
# ## add the fitted values by predicting from the model for the new data
# ndata.NDVI_mean.fishbiomass <- add_column(ndata.NDVI_mean.fishbiomass, fit = predict(mod.NDVI_mean.fishbiomass, newdata = ndata.NDVI_mean.fishbiomass, level=0))
# 
# ###for lmes: from bolker: 
# #http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
# Designmat <- model.matrix(formula(mod.NDVI_mean.fishbiomass)[-2], ndata.NDVI_mean.fishbiomass)
# predvar <- diag(Designmat %*% vcov(mod.NDVI_mean.fishbiomass) %*% t(Designmat)) 
# 
# ndata.NDVI_mean.fishbiomass$SE <- sqrt(predvar) 
# ndata.NDVI_mean.fishbiomass$SE2 <- sqrt(predvar+mod.NDVI_mean.fishbiomass$sigma^2)
# 
# fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)
# 
# ndata.NDVI_mean.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.NDVI_mean.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')


# plot 
plt.NDVI_mean.fishbiomass <- ggplot(fish_stats_zscores, aes(x = fish_biomass_bym3_mean.unscaled, y = NDVI_mean)) + 
  theme_classic()+
  geom_point( size=3)+
  xlab(expression("Fish biomass (g per m3)")) + ylab("Normalized Difference Vegetation Index (NDVI)")+  
  scale_shape_manual(values=c(19))+
  geom_smooth(method="lm", col="black", alpha=0.10, size=1.5)+
  theme(legend.position="none")
plt.NDVI_mean.fishbiomass
ggsave("C:Plots//Model-fitted//LME_NDVI_mean_fish_biomass.png")



# Tree abundance vs. fish biomass ----------
ggplot(fish_stats_zscores, aes(y=tree_abundance, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$tree_abundance)
qqp(fish_stats_zscores$tree_abundance, "lnorm")

lme.tree_abundance.fishbiomass<-lme(tree_abundance ~ fish_biomass_bym3_mean, random= ~1|unq_isl/unq_tran, data=fish_stats_zscores, na.action=na.omit)
# lme.tree_abundance.fishbiomass_log<-lme(log(tree_abundance+1) ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# 
# glmm.tree_abundance.fishbiomass_poisson<-glmmTMB((tree_abundance) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="poisson", na.action=na.omit)
# glmm.tree_abundance.fishbiomass<-glmmTMB((tree_abundance+0.01) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
# AICtab(glmm.tree_abundance.fishbiomass_poisson, glmm.tree_abundance.fishbiomass, lme.tree_abundance.fishbiomass, lme.tree_abundance.fishbiomass_log)

summary(lme.tree_abundance.fishbiomass)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.tree_abundance.fishbiomass,type=c("p","smooth")),
             plot(lme.tree_abundance.fishbiomass,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.tree_abundance.fishbiomass,resid(.,type="pearson")~fish_biomass_bym3_mean,
                  type=c("p","smooth")),
             qqnorm(lme.tree_abundance.fishbiomass,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.tree_abundance.fishbiomass<-lme.tree_abundance.fishbiomass 
ndata.tree_abundance.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
                                                                unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.tree_abundance.fishbiomass <- add_column(ndata.tree_abundance.fishbiomass, fit = predict(mod.tree_abundance.fishbiomass, newdata = ndata.tree_abundance.fishbiomass, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.tree_abundance.fishbiomass)[-2], ndata.tree_abundance.fishbiomass)
predvar <- diag(Designmat %*% vcov(mod.tree_abundance.fishbiomass) %*% t(Designmat)) 

ndata.tree_abundance.fishbiomass$SE <- sqrt(predvar) 
ndata.tree_abundance.fishbiomass$SE2 <- sqrt(predvar+mod.tree_abundance.fishbiomass$sigma^2)

fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)

ndata.tree_abundance.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.tree_abundance.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')


# plot 
plt.tree_abundance.fishbiomass <- ggplot(ndata.tree_abundance.fishbiomass, aes(x = fish_biomass_bym3_mean.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =tree_abundance), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish biomass (g per m3)")) + ylab("Tree density")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.tree_abundance.fishbiomass,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.tree_abundance.fishbiomass
ggsave("C:Plots//Model-fitted//LME_tree_abundance_fish_biomass.png")





# Sum_basal vs. fish biomass ----------
ggplot(fish_stats_zscores, aes(y=sum_basal, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$sum_basal)
qqp(fish_stats_zscores$sum_basal, "lnorm")

lme.sum_basal.fishbiomass<-lme(sum_basal ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# lme.sum_basal.fishbiomass_log<-lme(log(sum_basal+1) ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# 
# glmm.sum_basal.fishbiomass<-glmmTMB((sum_basal+0.01) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
# 
# AICtab( lme.sum_basal.fishbiomass, lme.sum_basal.fishbiomass_log, glmm.sum_basal.fishbiomass)

summary(lme.sum_basal.fishbiomass)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.sum_basal.fishbiomass,type=c("p","smooth")),
             plot(lme.sum_basal.fishbiomass,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.sum_basal.fishbiomass,resid(.,type="pearson")~fish_biomass_bym3_mean,
                  type=c("p","smooth")),
             qqnorm(lme.sum_basal.fishbiomass,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.sum_basal.fishbiomass<-lme.sum_basal.fishbiomass 
ndata.sum_basal.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
                                                                    unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.sum_basal.fishbiomass <- add_column(ndata.sum_basal.fishbiomass, fit = predict(mod.sum_basal.fishbiomass, newdata = ndata.sum_basal.fishbiomass, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.sum_basal.fishbiomass)[-2], ndata.sum_basal.fishbiomass)
predvar <- diag(Designmat %*% vcov(mod.sum_basal.fishbiomass) %*% t(Designmat)) 

ndata.sum_basal.fishbiomass$SE <- sqrt(predvar) 
ndata.sum_basal.fishbiomass$SE2 <- sqrt(predvar+mod.sum_basal.fishbiomass$sigma^2)

fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)

ndata.sum_basal.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.sum_basal.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')


# plot 
plt.sum_basal.fishbiomass <- ggplot(ndata.sum_basal.fishbiomass, aes(x = fish_biomass_bym3_mean.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =sum_basal), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish biomass (g per m3)")) + ylab("Summed tree basal area")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.sum_basal.fishbiomass,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.sum_basal.fishbiomass
ggsave("C:Plots//Model-fitted//LME_sum_basal_fish_biomass.png")

# bird.density vs. fish biomass ----------
ggplot(fish_stats_zscores, aes(y=bird.density, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$bird.density)
qqp(fish_stats_zscores$bird.density, "lnorm")

lm.bird.density.fishbiomass<-lm(bird.density ~ fish_biomass_bym3_mean, data=fish_stats_zscores, na.action=na.omit)
summary(lm.bird.density.fishbiomass)


# plot 
plt.bird.density.fishbiomass <- ggplot(fish_stats_zscores, aes(x = fish_biomass_bym3_mean.unscaled, y = bird.density)) + 
  theme_classic()+
  geom_point(size=3)+
  xlab(expression("Fish biomass (g per m3)")) + ylab("Bird density (#/hectare)")+  
  scale_shape_manual(values=c(19))+
  geom_smooth(size=1.5, col="black", alpha=0.15, method="lm")+
  theme(legend.position="none")
plt.bird.density.fishbiomass
ggsave("C:Plots//Model-fitted//LM_bird.density_fish_biomass.png", width=15, height=10, units="cm")

# bird.richness vs. Area ----------
ggplot(fish_stats_zscores_cat, aes(y=bird.richness, x=log_Area))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores_cat$bird.richness)
qqp(fish_stats_zscores_cat$bird.richness, "lnorm")

lm.bird.richness.Area<-lm(log(bird.richness) ~ log_Area*fish_biomass_bym3_cat, data=fish_stats_zscores_cat, na.action=na.omit)
summary(lm.bird.richness.Area)


# plot 
colorset_richness = c("low fish biomass"="black" , "high fish biomass" ="#1baff5" )

plt.bird.richness.Area <- ggplot(fish_stats_zscores_cat, aes(x = log_Area.unscaled, y = log(bird.richness) ,col=fish_biomass_bym3_cat)) + 
  theme_classic()+
  geom_point(size=3)+
  scale_colour_manual(values=colorset_richness)+ scale_fill_manual(values=colorset_richness)+
  xlab(expression("Island Area (log)")) + ylab("Bird richness (log)")+  
  scale_shape_manual(values=c(19))+
  geom_smooth(aes(col=fish_biomass_bym3_cat, fill=fish_biomass_bym3_cat),size=1.5, alpha=0.2, method="lm")+
  theme(legend.position="none")
plt.bird.richness.Area
ggsave("C:Plots//Model-fitted//LME_bird.richness_Area.png", width=10, height=10, units="cm")



####

# tree_richness vs. Area ----------
ggplot(fish_stats_zscores_cat, aes(y=tree_richness, x=log_Area))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores_cat$tree_richness)
qqp(fish_stats_zscores_cat$tree_richness, "lnorm")

lm.tree_richness.Area<-lm(log(tree_richness) ~ log_Area*fish_biomass_bym3_cat, data=fish_stats_zscores_cat, na.action=na.omit)
summary(lm.tree_richness.Area)


# plot 
colorset_richness = c("low fish biomass"="black" , "high fish biomass" ="#1baff5" )

plt.tree_richness.Area <- ggplot(fish_stats_zscores_cat, aes(x = log_Area.unscaled, y = log(tree_richness) ,col=fish_biomass_bym3_cat)) + 
  theme_classic()+
  geom_point(size=3)+
  scale_colour_manual(values=colorset_richness)+ scale_fill_manual(values=colorset_richness)+
  xlab(expression("Island Area (log)")) + ylab("Tree richness (log)")+  
  scale_shape_manual(values=c(19))+
  geom_smooth(aes(col=fish_biomass_bym3_cat, fill=fish_biomass_bym3_cat),size=1.5, alpha=0.2, method="lm")+
  theme(legend.position="none")
plt.tree_richness.Area
ggsave("C:Plots//Model-fitted//LME_tree_richness_Area.png")

# mammal_richness vs. Area ----------
ggplot(fish_stats_zscores_cat, aes(y=mammal_richness, x=log_Area))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores_cat$mammal_richness)
qqp(fish_stats_zscores_cat$mammal_richness, "lnorm")

lm.mammal_richness.Area<-lm(log(mammal_richness+1) ~ log_Area*fish_biomass_bym3_cat, data=fish_stats_zscores_cat, na.action=na.omit)
summary(lm.mammal_richness.Area)


# plot 
colorset_richness = c("low fish biomass"="black" , "high fish biomass" ="#1baff5" )

plt.mammal_richness.Area <- ggplot(fish_stats_zscores_cat, aes(x = log_Area, y = log(mammal_richness+1) ,col=fish_biomass_bym3_cat)) + 
  theme_classic()+
  geom_point(size=3)+
  scale_colour_manual(values=colorset_richness)+ scale_fill_manual(values=colorset_richness)+
  xlab(expression("Island Area (log)")) + ylab("mammal richness (log)")+  
  scale_shape_manual(values=c(19))+
  geom_smooth(aes(col=fish_biomass_bym3_cat, fill=fish_biomass_bym3_cat),size=1.5, alpha=0.2, method="lm")+
  theme(legend.position="none")
plt.mammal_richness.Area
ggsave("C:Plots//Model-fitted//LME_mammal_richness_Area.png")


# total_richness vs. Area ----------
ggplot(fish_stats_zscores_cat, aes(y=total_richness, x=log_Area))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores_cat$total_richness)
qqp(fish_stats_zscores_cat$total_richness, "lnorm")

lm.total_richness.Area<-lm(log(total_richness) ~ log_Area*fish_biomass_bym3_cat, data=fish_stats_zscores_cat, na.action=na.omit)
summary(lm.total_richness.Area)


# plot 
colorset_richness = c("low fish biomass"="black" , "high fish biomass" ="#1baff5" )

plt.total_richness.Area <- ggplot(fish_stats_zscores_cat, aes(x = log_Area, y = log(total_richness) ,col=fish_biomass_bym3_cat)) + 
  theme_classic()+
  geom_point(size=3)+
  scale_colour_manual(values=colorset_richness)+ scale_fill_manual(values=colorset_richness)+
  xlab(expression("Island Area (log)")) + ylab("total richness (log)")+  
  scale_shape_manual(values=c(19))+
  geom_smooth(aes(col=fish_biomass_bym3_cat, fill=fish_biomass_bym3_cat),size=1.5, alpha=0.2, method="lm")+
  theme(legend.position="none")
plt.total_richness.Area
ggsave("C:Plots//Model-fitted//LME_total_richness_Area.png")


# bird.density vs. fish richness ----------
ggplot(fish_stats_zscores, aes(y=bird.density, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$bird.density)
qqp(fish_stats_zscores$bird.density, "lnorm")

lm.bird.density.fishcatch<-lm(bird.density ~ fish_richness_corrected, data=fish_stats_zscores, na.action=na.omit)
# lme.bird.density.fishcatch_log<-lme(log(bird.density+1) ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# 
# glmm.bird.density.fishcatch<-glmmTMB((bird.density+0.01) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
# 
# AICtab( lme.bird.density.fishcatch, lme.bird.density.fishcatch_log, glmm.bird.density.fishcatch)

summary(lm.bird.density.fishcatch)


# plot 
plt.bird.density.fishcatch <- ggplot(fish_stats_zscores, aes(x = fish_richness_corrected.unscaled, y = bird.density)) + 
  theme_classic()+
  geom_point(size=3)+
  xlab(expression("Fish richness per 100 m3")) + ylab("Bird density (#/hectare)")+  
  scale_shape_manual(values=c(19))+
  geom_smooth(size=1.5, col="black", alpha=0.2, method="lm")+
  theme(legend.position="none")
plt.bird.density.fishcatch
ggsave("C:Plots//Model-fitted//LME_bird.density_fish_richness.png")


ggplot(fish_stats_zscores, aes(y=insect_detritivore_beat_av_abundance, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")

qqp(fish_stats_zscores$insect_detritivore_beat_av_abundance)
qqp(fish_stats_zscores$insect_detritivore_beat_av_abundance, "lnorm")

lme.insect_detritivore_beat_av_abundance.fishrichness<-lme(insect_detritivore_beat_av_abundance ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# lme.insect_detritivore_beat_av_abundance.fishrichness_log<-lme(log(insect_detritivore_beat_av_abundance+1) ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# 
# glmm.insect_detritivore_beat_av_abundance.fishrichness<-glmmTMB((insect_detritivore_beat_av_abundance+0.01) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
# 
# AICtab( lme.insect_detritivore_beat_av_abundance.fishrichness, lme.insect_detritivore_beat_av_abundance.fishrichness_log, glmm.insect_detritivore_beat_av_abundance.fishrichness)

summary(lme.insect_detritivore_beat_av_abundance.fishrichness)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.insect_detritivore_beat_av_abundance.fishrichness,type=c("p","smooth")),
             plot(lme.insect_detritivore_beat_av_abundance.fishrichness,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.insect_detritivore_beat_av_abundance.fishrichness,resid(.,type="pearson")~fish_richness_corrected,
                  type=c("p","smooth")),
             qqnorm(lme.insect_detritivore_beat_av_abundance.fishrichness,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.insect_detritivore_beat_av_abundance.fishrichness<-lme.insect_detritivore_beat_av_abundance.fishrichness 
ndata.insect_detritivore_beat_av_abundance.fishrichness <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
                                                                                          unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.insect_detritivore_beat_av_abundance.fishrichness <- add_column(ndata.insect_detritivore_beat_av_abundance.fishrichness, fit = predict(mod.insect_detritivore_beat_av_abundance.fishrichness, newdata = ndata.insect_detritivore_beat_av_abundance.fishrichness, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.insect_detritivore_beat_av_abundance.fishrichness)[-2], ndata.insect_detritivore_beat_av_abundance.fishrichness)
predvar <- diag(Designmat %*% vcov(mod.insect_detritivore_beat_av_abundance.fishrichness) %*% t(Designmat)) 

ndata.insect_detritivore_beat_av_abundance.fishrichness$SE <- sqrt(predvar) 
ndata.insect_detritivore_beat_av_abundance.fishrichness$SE2 <- sqrt(predvar+mod.insect_detritivore_beat_av_abundance.fishrichness$sigma^2)

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.insect_detritivore_beat_av_abundance.fishrichness$fish_richness_corrected.unscaled<-ndata.insect_detritivore_beat_av_abundance.fishrichness$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.insect_detritivore_beat_av_abundance.fishrichness <- ggplot(ndata.insect_detritivore_beat_av_abundance.fishrichness, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =insect_detritivore_beat_av_abundance), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish biomass (g per m3)")) + ylab("Insect detritivore density (#/beat)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.insect_detritivore_beat_av_abundance.fishrichness,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.insect_detritivore_beat_av_abundance.fishrichness
ggsave("C:Plots//Model-fitted//LME_insect_detritivore_beat_av_abundance_fish_biomass.png")



# insect_detritivore_beat_av_abundance vs. fish biomass ----------
ggplot(fish_stats_zscores, aes(y=insect_detritivore_beat_av_abundance, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")

qqp(fish_stats_zscores$insect_detritivore_beat_av_abundance)
qqp(fish_stats_zscores$insect_detritivore_beat_av_abundance, "lnorm")

lme.insect_detritivore_beat_av_abundance.fishbiomass<-lme(insect_detritivore_beat_av_abundance ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# lme.insect_detritivore_beat_av_abundance.fishbiomass_log<-lme(log(insect_detritivore_beat_av_abundance+1) ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# 
# glmm.insect_detritivore_beat_av_abundance.fishbiomass<-glmmTMB((insect_detritivore_beat_av_abundance+0.01) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
# 
# AICtab( lme.insect_detritivore_beat_av_abundance.fishbiomass, lme.insect_detritivore_beat_av_abundance.fishbiomass_log, glmm.insect_detritivore_beat_av_abundance.fishbiomass)

summary(lme.insect_detritivore_beat_av_abundance.fishbiomass)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.insect_detritivore_beat_av_abundance.fishbiomass,type=c("p","smooth")),
             plot(lme.insect_detritivore_beat_av_abundance.fishbiomass,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.insect_detritivore_beat_av_abundance.fishbiomass,resid(.,type="pearson")~fish_biomass_bym3_mean,
                  type=c("p","smooth")),
             qqnorm(lme.insect_detritivore_beat_av_abundance.fishbiomass,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.insect_detritivore_beat_av_abundance.fishbiomass<-lme.insect_detritivore_beat_av_abundance.fishbiomass 
ndata.insect_detritivore_beat_av_abundance.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
                                                               unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.insect_detritivore_beat_av_abundance.fishbiomass <- add_column(ndata.insect_detritivore_beat_av_abundance.fishbiomass, fit = predict(mod.insect_detritivore_beat_av_abundance.fishbiomass, newdata = ndata.insect_detritivore_beat_av_abundance.fishbiomass, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.insect_detritivore_beat_av_abundance.fishbiomass)[-2], ndata.insect_detritivore_beat_av_abundance.fishbiomass)
predvar <- diag(Designmat %*% vcov(mod.insect_detritivore_beat_av_abundance.fishbiomass) %*% t(Designmat)) 

ndata.insect_detritivore_beat_av_abundance.fishbiomass$SE <- sqrt(predvar) 
ndata.insect_detritivore_beat_av_abundance.fishbiomass$SE2 <- sqrt(predvar+mod.insect_detritivore_beat_av_abundance.fishbiomass$sigma^2)

fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)

ndata.insect_detritivore_beat_av_abundance.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.insect_detritivore_beat_av_abundance.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')


# plot 
plt.insect_detritivore_beat_av_abundance.fishbiomass <- ggplot(ndata.insect_detritivore_beat_av_abundance.fishbiomass, aes(x = fish_biomass_bym3_mean.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =insect_detritivore_beat_av_abundance), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish biomass (g per m3)")) + ylab("Insect detritivore density (#/beat)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.insect_detritivore_beat_av_abundance.fishbiomass,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.insect_detritivore_beat_av_abundance.fishbiomass
ggsave("C:Plots//Model-fitted//LME_insect_detritivore_beat_av_abundance_fish_biomass.png")

# insect_carnivore_beat_av_abundance vs. fish biomass ----------
ggplot(fish_stats_zscores, aes(y=insect_carnivore_beat_av_abundance, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$insect_carnivore_beat_av_abundance)
qqp(fish_stats_zscores$insect_carnivore_beat_av_abundance, "lnorm")

lme.insect_carnivore_beat_av_abundance.fishbiomass<-lme(insect_carnivore_beat_av_abundance ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# lme.insect_carnivore_beat_av_abundance.fishbiomass_log<-lme(log(insect_carnivore_beat_av_abundance+1) ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# 
# glmm.insect_carnivore_beat_av_abundance.fishbiomass<-glmmTMB((insect_carnivore_beat_av_abundance+0.01) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
# 
# AICtab( lme.insect_carnivore_beat_av_abundance.fishbiomass, lme.insect_carnivore_beat_av_abundance.fishbiomass_log, glmm.insect_carnivore_beat_av_abundance.fishbiomass)

summary(lme.insect_carnivore_beat_av_abundance.fishbiomass)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.insect_carnivore_beat_av_abundance.fishbiomass,type=c("p","smooth")),
             plot(lme.insect_carnivore_beat_av_abundance.fishbiomass,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.insect_carnivore_beat_av_abundance.fishbiomass,resid(.,type="pearson")~fish_biomass_bym3_mean,
                  type=c("p","smooth")),
             qqnorm(lme.insect_carnivore_beat_av_abundance.fishbiomass,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.insect_carnivore_beat_av_abundance.fishbiomass<-lme.insect_carnivore_beat_av_abundance.fishbiomass 
ndata.insect_carnivore_beat_av_abundance.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
                                                                                             unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.insect_carnivore_beat_av_abundance.fishbiomass <- add_column(ndata.insect_carnivore_beat_av_abundance.fishbiomass, fit = predict(mod.insect_carnivore_beat_av_abundance.fishbiomass, newdata = ndata.insect_carnivore_beat_av_abundance.fishbiomass, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.insect_carnivore_beat_av_abundance.fishbiomass)[-2], ndata.insect_carnivore_beat_av_abundance.fishbiomass)
predvar <- diag(Designmat %*% vcov(mod.insect_carnivore_beat_av_abundance.fishbiomass) %*% t(Designmat)) 

ndata.insect_carnivore_beat_av_abundance.fishbiomass$SE <- sqrt(predvar) 
ndata.insect_carnivore_beat_av_abundance.fishbiomass$SE2 <- sqrt(predvar+mod.insect_carnivore_beat_av_abundance.fishbiomass$sigma^2)

fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)

ndata.insect_carnivore_beat_av_abundance.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.insect_carnivore_beat_av_abundance.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')


# plot
plt.insect_carnivore_beat_av_abundance.fishbiomass <- ggplot(ndata.insect_carnivore_beat_av_abundance.fishbiomass, aes(x = fish_biomass_bym3_mean.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =insect_carnivore_beat_av_abundance), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish biomass (g per m3)")) + ylab("Insect carnivore density (#/beat)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.insect_carnivore_beat_av_abundance.fishbiomass,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.insect_carnivore_beat_av_abundance.fishbiomass
ggsave("C:Plots//Model-fitted//LME_insect_carnivore_beat_av_abundance_fish_biomass.png")



# birdfood ----------------------------------------------------------------
# birdfood insects vs. fish biomass  
head(fish_stats_zscores)
lme.insect_birdfood_beat_av_abundance.fishbiomass<-lme(insect_birdfood_beat_av_abundance ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# lme.insect_birdfood_beat_av_abundance.fishbiomass_log<-lme(log(insect_birdfood_beat_av_abundance+1) ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# 
# glmm.insect_birdfood_beat_av_abundance.fishbiomass<-glmmTMB((insect_birdfood_beat_av_abundance+0.01) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
# 
# AICtab( lme.insect_birdfood_beat_av_abundance.fishbiomass, lme.insect_birdfood_beat_av_abundance.fishbiomass_log, glmm.insect_birdfood_beat_av_abundance.fishbiomass)


summary(lme.insect_birdfood_beat_av_abundance.fishbiomass)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.insect_birdfood_beat_av_abundance.fishbiomass,type=c("p","smooth")),
             plot(lme.insect_birdfood_beat_av_abundance.fishbiomass,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.insect_birdfood_beat_av_abundance.fishbiomass,resid(.,type="pearson")~fish_biomass_bym3_mean,
                  type=c("p","smooth")),
             qqnorm(lme.insect_birdfood_beat_av_abundance.fishbiomass,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.insect_birdfood_beat_av_abundance.fishbiomass<-lme.insect_birdfood_beat_av_abundance.fishbiomass 
ndata.insect_birdfood_beat_av_abundance.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
                                                                                        unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.insect_birdfood_beat_av_abundance.fishbiomass <- add_column(ndata.insect_birdfood_beat_av_abundance.fishbiomass, fit = predict(mod.insect_birdfood_beat_av_abundance.fishbiomass, newdata = ndata.insect_birdfood_beat_av_abundance.fishbiomass, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.insect_birdfood_beat_av_abundance.fishbiomass)[-2], ndata.insect_birdfood_beat_av_abundance.fishbiomass)
predvar <- diag(Designmat %*% vcov(mod.insect_birdfood_beat_av_abundance.fishbiomass) %*% t(Designmat)) 

ndata.insect_birdfood_beat_av_abundance.fishbiomass$SE <- sqrt(predvar) 
ndata.insect_birdfood_beat_av_abundance.fishbiomass$SE2 <- sqrt(predvar+mod.insect_birdfood_beat_av_abundance.fishbiomass$sigma^2)

fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)

ndata.insect_birdfood_beat_av_abundance.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.insect_birdfood_beat_av_abundance.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')


# plot
plt.insect_birdfood_beat_av_abundance.fishbiomass <- ggplot(ndata.insect_birdfood_beat_av_abundance.fishbiomass, aes(x = fish_biomass_bym3_mean.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =insect_birdfood_beat_av_abundance), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish biomass (g per m3)")) + ylab("Insect birdfood density (#/beat)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.insect_birdfood_beat_av_abundance.fishbiomass,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.insect_birdfood_beat_av_abundance.fishbiomass
ggsave("C:Plots//Model-fitted//LME_insect_birdfood_beat_av_abundance_fish_biomass.png")

# insect_birdfood_richness vs. Area----------
ggplot(fish_stats_zscores_cat, aes(y=insect_birdfood_richness, x=log_Area))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores_cat$insect_birdfood_richness)
qqp(fish_stats_zscores_cat$insect_birdfood_richness, "lnorm")

lme.insect_birdfood_richness.Area<-lme(log(insect_birdfood_richness) ~ log_Area*fish_biomass_bym3_cat, random= ~1|unq_isl, data=fish_stats_zscores_cat, na.action=na.omit)
summary(lme.insect_birdfood_richness.Area)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.insect_birdfood_richness.Area,type=c("p","smooth")),
             plot(lme.insect_birdfood_richness.Area,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores_cat$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.insect_birdfood_richness.Area,resid(.,type="pearson")~log_Area,
                  type=c("p","smooth")),
             qqnorm(lme.insect_birdfood_richness.Area,abline=c(0,1),
                    col=ifelse(fish_stats_zscores_cat$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting

fm1_birdfood<-lme.insect_birdfood_richness.Area
newdat_birdfood <- expand.grid(log_Area = seq(min(fish_stats_zscores_cat$log_Area), max(fish_stats_zscores_cat$log_Area),length = 100),
                                fish_biomass_bym3_cat=c("low fish biomass", "high fish biomass"))
newdat_birdfood$pred <- predict(fm1_birdfood, newdat_birdfood, level = 0)


Designmat_birdfood <- model.matrix(formula(fm1_birdfood)[-2], newdat_birdfood)
predvar_birdfood <- diag(Designmat_birdfood %*% vcov(fm1_birdfood) %*% t(Designmat_birdfood)) 
newdat_birdfood$SE <- sqrt(predvar_birdfood) 
newdat_birdfood$SE2 <- sqrt(predvar_birdfood+fm1_birdfood$sigma^2)

fish_stats_zscores_cat$log_Area<-scale(fish_stats_cat$log_Area, center=TRUE, scale=TRUE)

newdat_birdfood$log_Area.unscaled<-newdat$log_Area * attr(fish_stats_zscores_cat$log_Area, 'scaled:scale') + attr(fish_stats_zscores_cat$log_Area, 'scaled:center')


# plot
colorset_richness = c("low fish biomass"="black" , "high fish biomass" ="#1baff5" )
plt.insect_birdfood_richness.Area <- ggplot(newdat_birdfood, aes(x = log_Area.unscaled, y = pred, colour=fish_biomass_bym3_cat)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  scale_colour_manual(values=colorset_richness)+ scale_fill_manual(values=colorset_richness)+
  geom_point(aes(y =log(insect_birdfood_richness)), size=3, data = fish_stats_zscores_cat)+
  xlab(expression("Island Area (log)")) + ylab("Insect birdfood richness (log)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = newdat_birdfood,aes(ymin = pred - 2*SE, ymax =  pred+ 2*SE, fill=fish_biomass_bym3_cat, colour=fish_biomass_bym3_cat), alpha = 0.10, colour = NA)+
  theme(legend.position="none")
plt.insect_birdfood_richness.Area
ggsave("C:Plots//Model-fitted//LME_insect_birdfood_richness_Area.png")


# insect_carnivore_richness vs. Area----------
ggplot(fish_stats_zscores_cat, aes(y=insect_carnivore_richness, x=log_Area))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores_cat$insect_carnivore_richness)
qqp(fish_stats_zscores_cat$insect_carnivore_richness, "lnorm")

lme.insect_carnivore_richness.Area<-lme(log(insect_carnivore_richness) ~ log_Area*fish_biomass_bym3_cat, random= ~1|unq_isl, data=fish_stats_zscores_cat, na.action=na.omit)
summary(lme.insect_carnivore_richness.Area)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.insect_carnivore_richness.Area,type=c("p","smooth")),
             plot(lme.insect_carnivore_richness.Area,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores_cat$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.insect_carnivore_richness.Area,resid(.,type="pearson")~log_Area,
                  type=c("p","smooth")),
             qqnorm(lme.insect_carnivore_richness.Area,abline=c(0,1),
                    col=ifelse(fish_stats_zscores_cat$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting

fm1_carnivore<-lme.insect_carnivore_richness.Area
newdat_carnivore <- expand.grid(log_Area = seq(min(fish_stats_zscores_cat$log_Area), max(fish_stats_zscores_cat$log_Area),length = 100),
                      fish_biomass_bym3_cat=c("low fish biomass", "high fish biomass"))
newdat_carnivore$pred <- predict(fm1_carnivore, newdat_carnivore, level = 0)


Designmat_carnivore <- model.matrix(formula(fm1_carnivore)[-2], newdat_carnivore)
predvar_carnivore <- diag(Designmat_carnivore %*% vcov(fm1_carnivore) %*% t(Designmat_carnivore)) 
newdat_carnivore$SE <- sqrt(predvar_carnivore) 
newdat_carnivore$SE2 <- sqrt(predvar_carnivore+fm1_carnivore$sigma^2)

fish_stats_zscores_cat$log_Area<-scale(fish_stats_cat$log_Area, center=TRUE, scale=TRUE)

newdat_carnivore$log_Area.unscaled<-newdat$log_Area * attr(fish_stats_zscores_cat$log_Area, 'scaled:scale') + attr(fish_stats_zscores_cat$log_Area, 'scaled:center')


# plot
colorset_richness = c("low fish biomass"="black" , "high fish biomass" ="#1baff5" )
plt.insect_carnivore_richness.Area <- ggplot(newdat_carnivore, aes(x = log_Area.unscaled, y = pred, colour=fish_biomass_bym3_cat)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  scale_colour_manual(values=colorset_richness)+ scale_fill_manual(values=colorset_richness)+
  geom_point(aes(y =log(insect_carnivore_richness)), size=3, data = fish_stats_zscores_cat)+
  xlab(expression("Island Area (log)")) + ylab("Insect carnivore richness (log)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = newdat_carnivore,aes(ymin = pred - 2*SE, ymax =  pred+ 2*SE, fill=fish_biomass_bym3_cat, colour=fish_biomass_bym3_cat), alpha = 0.10, colour = NA)+
  theme(legend.position="none")
plt.insect_carnivore_richness.Area
ggsave("C:Plots//Model-fitted//LME_insect_carnivore_richness_Area.png")

# insect_detritivore_richness vs. Area----------
ggplot(fish_stats_zscores_cat, aes(y=insect_detritivore_richness, x=log_Area))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores_cat$insect_detritivore_richness)
qqp(fish_stats_zscores_cat$insect_detritivore_richness, "lnorm")

lme.insect_detritivore_richness.Area<-lme(log(insect_detritivore_richness) ~ log_Area*fish_biomass_bym3_cat, random= ~1|unq_isl, data=fish_stats_zscores_cat, na.action=na.omit)
summary(lme.insect_detritivore_richness.Area)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.insect_detritivore_richness.Area,type=c("p","smooth")),
             plot(lme.insect_detritivore_richness.Area,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores_cat$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.insect_detritivore_richness.Area,resid(.,type="pearson")~log_Area,
                  type=c("p","smooth")),
             qqnorm(lme.insect_detritivore_richness.Area,abline=c(0,1),
                    col=ifelse(fish_stats_zscores_cat$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting
fm1_detritivore<-lme.insect_detritivore_richness.Area
newdat_detritivore <- expand.grid(log_Area = seq(min(fish_stats_zscores_cat$log_Area), max(fish_stats_zscores_cat$log_Area),length = 100),
                                fish_biomass_bym3_cat=c("low fish biomass", "high fish biomass"))
newdat_detritivore$pred <- predict(fm1_detritivore, newdat_detritivore, level = 0)


Designmat_detritivore <- model.matrix(formula(fm1_detritivore)[-2], newdat_detritivore)
predvar_detritivore <- diag(Designmat_detritivore %*% vcov(fm1_detritivore) %*% t(Designmat_detritivore)) 
newdat_detritivore$SE <- sqrt(predvar_detritivore) 
newdat_detritivore$SE2 <- sqrt(predvar_detritivore+fm1_detritivore$sigma^2)

fish_stats_zscores_cat$log_Area<-scale(fish_stats_cat$log_Area, center=TRUE, scale=TRUE)

newdat_detritivore$log_Area.unscaled<-newdat$log_Area * attr(fish_stats_zscores_cat$log_Area, 'scaled:scale') + attr(fish_stats_zscores_cat$log_Area, 'scaled:center')


# plot
colorset_richness = c("low fish biomass"="black" , "high fish biomass" ="#1baff5" )
plt.insect_detritivore_richness.Area <- ggplot(newdat_detritivore, aes(x = log_Area.unscaled, y = pred, colour=fish_biomass_bym3_cat)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  scale_colour_manual(values=colorset_richness)+ scale_fill_manual(values=colorset_richness)+
  geom_point(aes(y =log(insect_detritivore_richness)), size=3, data = fish_stats_zscores_cat)+
  xlab(expression("Island Area (log)")) + ylab("Insect detritivore richness (log)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = newdat_detritivore,aes(ymin = pred - 2*SE, ymax =  pred+ 2*SE, fill=fish_biomass_bym3_cat, colour=fish_biomass_bym3_cat), alpha = 0.10, colour = NA)+
  theme(legend.position="none")
plt.insect_detritivore_richness.Area
ggsave("C:Plots//Model-fitted//LME_insect_detritivore_richness_Area.png")


# insect_herbivore_richness vs. Area----------
ggplot(fish_stats_zscores_cat, aes(y=insect_herbivore_richness, x=log_Area))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores_cat$insect_herbivore_richness)
qqp(fish_stats_zscores_cat$insect_herbivore_richness, "lnorm")

lme.insect_herbivore_richness.Area<-lme(log(insect_herbivore_richness) ~ log_Area*fish_biomass_bym3_cat, random= ~1|unq_isl, data=fish_stats_zscores_cat, na.action=na.omit)
summary(lme.insect_herbivore_richness.Area)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.insect_herbivore_richness.Area,type=c("p","smooth")),
             plot(lme.insect_herbivore_richness.Area,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores_cat$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.insect_herbivore_richness.Area,resid(.,type="pearson")~log_Area,
                  type=c("p","smooth")),
             qqnorm(lme.insect_herbivore_richness.Area,abline=c(0,1),
                    col=ifelse(fish_stats_zscores_cat$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting
fm1_herbivore<-lme.insect_herbivore_richness.Area
newdat_herbivore <- expand.grid(log_Area = seq(min(fish_stats_zscores_cat$log_Area), max(fish_stats_zscores_cat$log_Area),length = 100),
                                fish_biomass_bym3_cat=c("low fish biomass", "high fish biomass"))
newdat_herbivore$pred <- predict(fm1_herbivore, newdat_herbivore, level = 0)


Designmat_herbivore <- model.matrix(formula(fm1_herbivore)[-2], newdat_herbivore)
predvar_herbivore <- diag(Designmat_herbivore %*% vcov(fm1_herbivore) %*% t(Designmat_herbivore)) 
newdat_herbivore$SE <- sqrt(predvar_herbivore) 
newdat_herbivore$SE2 <- sqrt(predvar_herbivore+fm1_herbivore$sigma^2)

fish_stats_zscores_cat$log_Area<-scale(fish_stats_cat$log_Area, center=TRUE, scale=TRUE)

newdat_herbivore$log_Area.unscaled<-newdat$log_Area * attr(fish_stats_zscores_cat$log_Area, 'scaled:scale') + attr(fish_stats_zscores_cat$log_Area, 'scaled:center')


# plot
colorset_richness = c("low fish biomass"="black" , "high fish biomass" ="#1baff5" )
plt.insect_herbivore_richness.Area <- ggplot(newdat_herbivore, aes(x = log_Area.unscaled, y = pred, colour=fish_biomass_bym3_cat)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  scale_colour_manual(values=colorset_richness)+ scale_fill_manual(values=colorset_richness)+
  geom_point(aes(y =log(insect_herbivore_richness)), size=3, data = fish_stats_zscores_cat)+
  xlab(expression("Island Area (log)")) + ylab("Insect herbivore richness (log)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = newdat_herbivore,aes(ymin = pred - 2*SE, ymax =  pred+ 2*SE, fill=fish_biomass_bym3_cat, colour=fish_biomass_bym3_cat), alpha = 0.10, colour = NA)+
  theme(legend.position="none")
plt.insect_herbivore_richness.Area
ggsave("C:Plots//Model-fitted//LME_insect_herbivore_richness_Area.png")

# insect_richness vs. Area----------
ggplot(fish_stats_zscores_cat, aes(y=insect_richness, x=log_Area))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores_cat$insect_richness)
qqp(fish_stats_zscores_cat$insect_richness, "lnorm")

lme.insect_richness.Area<-lme(log(insect_richness) ~ log_Area*fish_biomass_bym3_cat, random= ~1|unq_isl, data=fish_stats_zscores_cat, na.action=na.omit)
summary(lme.insect_richness.Area)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.insect_richness.Area,type=c("p","smooth")),
             plot(lme.insect_richness.Area,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores_cat$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.insect_richness.Area,resid(.,type="pearson")~log_Area,
                  type=c("p","smooth")),
             qqnorm(lme.insect_richness.Area,abline=c(0,1),
                    col=ifelse(fish_stats_zscores_cat$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting

fm1_insect<-lme.insect_richness.Area
newdat_insect <- expand.grid(log_Area = seq(min(fish_stats_zscores_cat$log_Area), max(fish_stats_zscores_cat$log_Area),length = 100),
                                fish_biomass_bym3_cat=c("low fish biomass", "high fish biomass"))
newdat_insect$pred <- predict(fm1_insect, newdat_insect, level = 0)


Designmat_insect <- model.matrix(formula(fm1_insect)[-2], newdat_insect)
predvar_insect <- diag(Designmat_insect %*% vcov(fm1_insect) %*% t(Designmat_insect)) 
newdat_insect$SE <- sqrt(predvar_insect) 
newdat_insect$SE2 <- sqrt(predvar_insect+fm1_insect$sigma^2)

fish_stats_zscores_cat$log_Area<-scale(fish_stats_cat$log_Area, center=TRUE, scale=TRUE)

newdat_insect$log_Area.unscaled<-newdat$log_Area * attr(fish_stats_zscores_cat$log_Area, 'scaled:scale') + attr(fish_stats_zscores_cat$log_Area, 'scaled:center')


# plot
colorset_richness = c("low fish biomass"="black" , "high fish biomass" ="#1baff5" )
plt.insect_richness.Area <- ggplot(newdat_insect, aes(x = log_Area.unscaled, y = pred, colour=fish_biomass_bym3_cat)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  scale_colour_manual(values=colorset_richness)+ scale_fill_manual(values=colorset_richness)+
  geom_point(aes(y =log(insect_richness)), size=3, data = fish_stats_zscores_cat)+
  xlab(expression("Island Area (log)")) + ylab("Insect richness (log)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = newdat_insect,aes(ymin = pred - 2*SE, ymax =  pred+ 2*SE, fill=fish_biomass_bym3_cat, colour=fish_biomass_bym3_cat), alpha = 0.10, colour = NA)+
  theme(legend.position="none")
plt.insect_richness.Area
ggsave("C:Plots//Model-fitted//LME_insect_richness_Area.png")

# plant_richness vs. Area----------
ggplot(fish_stats_zscores_cat, aes(y=plant_richness, x=log_Area))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores_cat$plant_richness)
qqp(fish_stats_zscores_cat$plant_richness, "lnorm")

lme.plant_richness.Area<-lme(log(plant_richness+1) ~ log_Area*fish_biomass_bym3_cat, random= ~1|unq_isl, data=fish_stats_zscores_cat, na.action=na.omit)
summary(lme.plant_richness.Area)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.plant_richness.Area,type=c("p","smooth")),
             plot(lme.plant_richness.Area,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores_cat$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.plant_richness.Area,resid(.,type="pearson")~log_Area,
                  type=c("p","smooth")),
             qqnorm(lme.plant_richness.Area,abline=c(0,1),
                    col=ifelse(fish_stats_zscores_cat$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting

fm1_plant<-lme.plant_richness.Area
newdat_plant <- expand.grid(log_Area = seq(min(fish_stats_zscores_cat$log_Area), max(fish_stats_zscores_cat$log_Area),length = 100),
                             fish_biomass_bym3_cat=c("low fish biomass", "high fish biomass"))
newdat_plant$pred <- predict(fm1_plant, newdat_plant, level = 0)


Designmat_plant <- model.matrix(formula(fm1_plant)[-2], newdat_plant)
predvar_plant <- diag(Designmat_plant %*% vcov(fm1_plant) %*% t(Designmat_plant)) 
newdat_plant$SE <- sqrt(predvar_plant) 
newdat_plant$SE2 <- sqrt(predvar_plant+fm1_plant$sigma^2)

fish_stats_zscores_cat$log_Area<-scale(fish_stats_cat$log_Area, center=TRUE, scale=TRUE)

newdat_plant$log_Area.unscaled<-newdat$log_Area * attr(fish_stats_zscores_cat$log_Area, 'scaled:scale') + attr(fish_stats_zscores_cat$log_Area, 'scaled:center')


# plot
colorset_richness = c("low fish biomass"="black" , "high fish biomass" ="#1baff5" )
plt.plant_richness.Area <- ggplot(newdat_plant, aes(x = log_Area.unscaled, y = pred, colour=fish_biomass_bym3_cat)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  scale_colour_manual(values=colorset_richness)+ scale_fill_manual(values=colorset_richness)+
  geom_point(aes(y =log(plant_richness+1)), size=3, data = fish_stats_zscores_cat)+
  xlab(expression("Island Area (log)")) + ylab("Plant richness (log)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = newdat_plant,aes(ymin = pred - 2*SE, ymax =  pred+ 2*SE, fill=fish_biomass_bym3_cat, colour=fish_biomass_bym3_cat), alpha = 0.10, colour = NA)+
  theme(legend.position="none")
plt.plant_richness.Area
ggsave("C:Plots//Model-fitted//LME_plant_richness_Area.png")


# insect_herbivore_beat_av_abundance vs. fish biomass ----------
ggplot(fish_stats_zscores, aes(y=insect_herbivore_beat_av_abundance, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$insect_herbivore_beat_av_abundance)
qqp(fish_stats_zscores$insect_herbivore_beat_av_abundance, "lnorm")

lme.insect_herbivore_beat_av_abundance.fishbiomass<-lme(insect_herbivore_beat_av_abundance ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# lme.insect_herbivore_beat_av_abundance.fishbiomass_log<-lme(log(insect_herbivore_beat_av_abundance+1) ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# 
# glmm.insect_herbivore_beat_av_abundance.fishbiomass<-glmmTMB((insect_herbivore_beat_av_abundance+0.01) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
# 
# AICtab( lme.insect_herbivore_beat_av_abundance.fishbiomass, lme.insect_herbivore_beat_av_abundance.fishbiomass_log, glmm.insect_herbivore_beat_av_abundance.fishbiomass)

summary(lme.insect_herbivore_beat_av_abundance.fishbiomass)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.insect_herbivore_beat_av_abundance.fishbiomass,type=c("p","smooth")),
             plot(lme.insect_herbivore_beat_av_abundance.fishbiomass,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.insect_herbivore_beat_av_abundance.fishbiomass,resid(.,type="pearson")~fish_biomass_bym3_mean,
                  type=c("p","smooth")),
             qqnorm(lme.insect_herbivore_beat_av_abundance.fishbiomass,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.insect_herbivore_beat_av_abundance.fishbiomass<-lme.insect_herbivore_beat_av_abundance.fishbiomass 
ndata.insect_herbivore_beat_av_abundance.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
                                                                                             unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.insect_herbivore_beat_av_abundance.fishbiomass <- add_column(ndata.insect_herbivore_beat_av_abundance.fishbiomass, fit = predict(mod.insect_herbivore_beat_av_abundance.fishbiomass, newdata = ndata.insect_herbivore_beat_av_abundance.fishbiomass, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.insect_herbivore_beat_av_abundance.fishbiomass)[-2], ndata.insect_herbivore_beat_av_abundance.fishbiomass)
predvar <- diag(Designmat %*% vcov(mod.insect_herbivore_beat_av_abundance.fishbiomass) %*% t(Designmat)) 

ndata.insect_herbivore_beat_av_abundance.fishbiomass$SE <- sqrt(predvar) 
ndata.insect_herbivore_beat_av_abundance.fishbiomass$SE2 <- sqrt(predvar+mod.insect_herbivore_beat_av_abundance.fishbiomass$sigma^2)

fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)

ndata.insect_herbivore_beat_av_abundance.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.insect_herbivore_beat_av_abundance.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')


# plot 
plt.insect_herbivore_beat_av_abundance.fishbiomass <- ggplot(ndata.insect_herbivore_beat_av_abundance.fishbiomass, aes(x = fish_biomass_bym3_mean.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =insect_herbivore_beat_av_abundance), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish biomass (g per m3)")) + ylab("Insect herbivore density (#/beat)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.insect_herbivore_beat_av_abundance.fishbiomass,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.insect_herbivore_beat_av_abundance.fishbiomass
ggsave("C:Plots//Model-fitted//LME_insect_herbivore_beat_av_abundance_fish_biomass.png")


# insect_beat_av_abundance vs. fish biomass ----------
ggplot(fish_stats_zscores, aes(y=insect_beat_av_abundance, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$insect_beat_av_abundance)
qqp(fish_stats_zscores$insect_beat_av_abundance, "lnorm")

lme.insect_beat_av_abundance.fishbiomass<-lme(insect_beat_av_abundance ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# lme.insect_beat_av_abundance.fishbiomass_log<-lme(log(insect_beat_av_abundance+1) ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# 
# glmm.insect_beat_av_abundance.fishbiomass<-glmmTMB((insect_beat_av_abundance+0.01) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
# 
# AICtab( lme.insect_beat_av_abundance.fishbiomass, lme.insect_beat_av_abundance.fishbiomass_log, glmm.insect_beat_av_abundance.fishbiomass)

summary(lme.insect_beat_av_abundance.fishbiomass)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.insect_beat_av_abundance.fishbiomass,type=c("p","smooth")),
             plot(lme.insect_beat_av_abundance.fishbiomass,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.insect_beat_av_abundance.fishbiomass,resid(.,type="pearson")~fish_biomass_bym3_mean,
                  type=c("p","smooth")),
             qqnorm(lme.insect_beat_av_abundance.fishbiomass,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))

# Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.insect_beat_av_abundance.fishbiomass<-lme.insect_beat_av_abundance.fishbiomass 
ndata.insect_beat_av_abundance.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
                                                                                 unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.insect_beat_av_abundance.fishbiomass <- add_column(ndata.insect_beat_av_abundance.fishbiomass, fit = predict(mod.insect_beat_av_abundance.fishbiomass, newdata = ndata.insect_beat_av_abundance.fishbiomass, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.insect_beat_av_abundance.fishbiomass)[-2], ndata.insect_beat_av_abundance.fishbiomass)
predvar <- diag(Designmat %*% vcov(mod.insect_beat_av_abundance.fishbiomass) %*% t(Designmat)) 

ndata.insect_beat_av_abundance.fishbiomass$SE <- sqrt(predvar) 
ndata.insect_beat_av_abundance.fishbiomass$SE2 <- sqrt(predvar+mod.insect_beat_av_abundance.fishbiomass$sigma^2)

fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)

ndata.insect_beat_av_abundance.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.insect_beat_av_abundance.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')


# plot 
plt.insect_beat_av_abundance.fishbiomass <- ggplot(ndata.insect_beat_av_abundance.fishbiomass, aes(x = fish_biomass_bym3_mean.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =insect_beat_av_abundance), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish biomass (g per m3)")) + ylab("Insect density (#/beat)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.insect_beat_av_abundance.fishbiomass,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.insect_beat_av_abundance.fishbiomass
ggsave("C:Plots//Model-fitted//LME_insect_beat_av_abundance_fish_biomass.png")



# Shrub (total) vs. fish richness -----------------------------------------------------
#visualize different distributions
ggplot(fish_stats_zscores, aes(y=shrub_cover, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$shrub_cover)
qqp(fish_stats_zscores$shrub_cover, "lnorm")



#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.shrub_cover.fishcatch<-lme(shrub_cover ~ fish_richness_corrected, random= ~1|unq_isl/unq_tran, data=fish_stats_zscores, na.action=na.omit)
# glmm.shrub_cover.fishcatch<-glmmTMB((shrub_cover+0.01) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
# AICtab(glmm.shrub_cover.fishcatch, lme.shrub_cover.fishcatch)

summary(lme.shrub_cover.fishcatch)

#dwplot(list(glmmTMB=lme.shrub_cover.fishcatch,lmer=lmer.shrub_cover.fishcatch),by_2sd=TRUE)

colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.shrub_cover.fishcatch,type=c("p","smooth")),
             plot(lme.shrub_cover.fishcatch,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.shrub_cover.fishcatch,resid(.,type="pearson")~fish_richness_corrected,
                  type=c("p","smooth")),
             qqnorm(lme.shrub_cover.fishcatch,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))


## Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.shrub_cover.fishcatch<-lme.shrub_cover.fishcatch 
ndata.shrub_cover.fishcatch <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
                                                               unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.shrub_cover.fishcatch <- add_column(ndata.shrub_cover.fishcatch, fit = predict(mod.shrub_cover.fishcatch, newdata = ndata.shrub_cover.fishcatch, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.shrub_cover.fishcatch)[-2], ndata.shrub_cover.fishcatch)
predvar <- diag(Designmat %*% vcov(mod.shrub_cover.fishcatch) %*% t(Designmat)) 

ndata.shrub_cover.fishcatch$SE <- sqrt(predvar) 
ndata.shrub_cover.fishcatch$SE2 <- sqrt(predvar+mod.shrub_cover.fishcatch$sigma^2)

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.shrub_cover.fishcatch$fish_richness_corrected.unscaled<-ndata.shrub_cover.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.shrub_cover.fishcatch <- ggplot(ndata.shrub_cover.fishcatch, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(shrub_cover)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish richness per 100 m3")) + ylab("Shrub cover")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.shrub_cover.fishcatch,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.shrub_cover.fishcatch
ggsave("C:Plots//Model-fitted//LME_shrub_cover_fish_catch.png")


# NDVI vs. fish richness -----------------------------------------------------
#visualize different distributions
ggplot(fish_stats_zscores, aes(y=NDVI_mean, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$NDVI_mean)
qqp(fish_stats_zscores$NDVI_mean, "lnorm")



#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lm.NDVI_mean.fishcatch<-lm(NDVI_mean ~ fish_richness_corrected, data=fish_stats_zscores)
glmm.NDVI_mean.fishcatch<-glmmTMB((NDVI_mean+0.01) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
AICtab(glmm.NDVI_mean.fishcatch, lme.NDVI_mean.fishcatch)

summary(lm.NDVI_mean.fishcatch)

#dwplot(list(glmmTMB=lm.NDVI_mean.fishcatch,lmer=lmer.NDVI_mean.fishcatch),by_2sd=TRUE)

colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lm.NDVI_mean.fishcatch,type=c("p","smooth")),
             plot(lm.NDVI_mean.fishcatch,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lm.NDVI_mean.fishcatch,resid(.,type="pearson")~fish_richness_corrected,
                  type=c("p","smooth")),
             qqnorm(lm.NDVI_mean.fishcatch,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))

# 
# ## Extracting coefficients and plotting
# want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
# mod.NDVI_mean.fishcatch<-lm.NDVI_mean.fishcatch 
# ndata.NDVI_mean.fishcatch <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
#                                                                unq_isl = unq_isl[want]))
# 
# ## add the fitted values by predicting from the model for the new data
# ndata.NDVI_mean.fishcatch <- add_column(ndata.NDVI_mean.fishcatch, fit = predict(mod.NDVI_mean.fishcatch, newdata = ndata.NDVI_mean.fishcatch))
# 
# ###for lmes: from bolker: 
# #http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
# Designmat <- model.matrix(formula(mod.NDVI_mean.fishcatch)[-2], ndata.NDVI_mean.fishcatch)
# predvar <- diag(Designmat %*% vcov(mod.NDVI_mean.fishcatch) %*% t(Designmat)) 
# 
# ndata.NDVI_mean.fishcatch$SE <- sqrt(predvar) 
# ndata.NDVI_mean.fishcatch$SE2 <- sqrt(predvar+mod.NDVI_mean.fishcatch$sigma^2)
# 
# fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)
# 
# ndata.NDVI_mean.fishcatch$fish_richness_corrected.unscaled<-ndata.NDVI_mean.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')
# 

# plot 
plt.NDVI_mean.fishcatch <- ggplot(fish_stats_zscores, aes(x = fish_richness_corrected.unscaled, y = NDVI_mean)) + 
  theme_classic()+
  geom_point( size=3)+
  xlab(expression("Fish richness per 100 m3")) + ylab("NDVI_mean")+  
  scale_shape_manual(values=c(19))+
  geom_smooth(method="lm", alpha = 0.10, col="black", size=1.5)+
  theme(legend.position="none")
plt.NDVI_mean.fishcatch
ggsave("C:Plots//Model-fitted//LME_NDVI_mean_fish_catch.png")


#Sum basal vs. fish richness -----------------------------------------------------
#visualize different distributions
ggplot(fish_stats_zscores, aes(y=sum_basal, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$sum_basal)
qqp(fish_stats_zscores$sum_basal, "lnorm")



#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.sum_basal.fishcatch<-lme(sum_basal ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
glmm.sum_basal.fishcatch<-glmmTMB((sum_basal+0.01) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
AICtab(glmm.sum_basal.fishcatch, lme.sum_basal.fishcatch)

summary(lme.sum_basal.fishcatch)

#dwplot(list(glmmTMB=lme.sum_basal.fishcatch,lmer=lmer.sum_basal.fishcatch),by_2sd=TRUE)

colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.sum_basal.fishcatch,type=c("p","smooth")),
             plot(lme.sum_basal.fishcatch,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.sum_basal.fishcatch,resid(.,type="pearson")~fish_richness_corrected,
                  type=c("p","smooth")),
             qqnorm(lme.sum_basal.fishcatch,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))


## Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.sum_basal.fishcatch<-lme.sum_basal.fishcatch 
ndata.sum_basal.fishcatch <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
                                                               unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.sum_basal.fishcatch <- add_column(ndata.sum_basal.fishcatch, fit = predict(mod.sum_basal.fishcatch, newdata = ndata.sum_basal.fishcatch, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.sum_basal.fishcatch)[-2], ndata.sum_basal.fishcatch)
predvar <- diag(Designmat %*% vcov(mod.sum_basal.fishcatch) %*% t(Designmat)) 

ndata.sum_basal.fishcatch$SE <- sqrt(predvar) 
ndata.sum_basal.fishcatch$SE2 <- sqrt(predvar+mod.sum_basal.fishcatch$sigma^2)

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.sum_basal.fishcatch$fish_richness_corrected.unscaled<-ndata.sum_basal.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.sum_basal.fishcatch <- ggplot(ndata.sum_basal.fishcatch, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(sum_basal)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish richness per 100 m3")) + ylab("Summed tree basal area")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.sum_basal.fishcatch,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.sum_basal.fishcatch
ggsave("C:Plots//Model-fitted//LME_sum_basal_fish_catch.png")

#Sum basal vs. fish richness -----------------------------------------------------
#visualize different distributions
ggplot(fish_stats_zscores, aes(y=insect_pitfall_av_abundance, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$insect_pitfall_av_abundance)
qqp(fish_stats_zscores$insect_pitfall_av_abundance, "lnorm")



#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.insect_pitfall_av_abundance.fishcatch<-lme(insect_pitfall_av_abundance ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
glmm.insect_pitfall_av_abundance.fishcatch<-glmmTMB((insect_pitfall_av_abundance+0.01) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
AICtab(glmm.insect_pitfall_av_abundance.fishcatch, lme.insect_pitfall_av_abundance.fishcatch)

summary(lme.insect_pitfall_av_abundance.fishcatch)

#dwplot(list(glmmTMB=lme.insect_pitfall_av_abundance.fishcatch,lmer=lmer.insect_pitfall_av_abundance.fishcatch),by_2sd=TRUE)

colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.insect_pitfall_av_abundance.fishcatch,type=c("p","smooth")),
             plot(lme.insect_pitfall_av_abundance.fishcatch,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.insect_pitfall_av_abundance.fishcatch,resid(.,type="pearson")~fish_richness_corrected,
                  type=c("p","smooth")),
             qqnorm(lme.insect_pitfall_av_abundance.fishcatch,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))


## Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.insect_pitfall_av_abundance.fishcatch<-lme.insect_pitfall_av_abundance.fishcatch 
ndata.insect_pitfall_av_abundance.fishcatch <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
                                                             unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.insect_pitfall_av_abundance.fishcatch <- add_column(ndata.insect_pitfall_av_abundance.fishcatch, fit = predict(mod.insect_pitfall_av_abundance.fishcatch, newdata = ndata.insect_pitfall_av_abundance.fishcatch, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.insect_pitfall_av_abundance.fishcatch)[-2], ndata.insect_pitfall_av_abundance.fishcatch)
predvar <- diag(Designmat %*% vcov(mod.insect_pitfall_av_abundance.fishcatch) %*% t(Designmat)) 

ndata.insect_pitfall_av_abundance.fishcatch$SE <- sqrt(predvar) 
ndata.insect_pitfall_av_abundance.fishcatch$SE2 <- sqrt(predvar+mod.insect_pitfall_av_abundance.fishcatch$sigma^2)

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.insect_pitfall_av_abundance.fishcatch$fish_richness_corrected.unscaled<-ndata.insect_pitfall_av_abundance.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.insect_pitfall_av_abundance.fishcatch <- ggplot(ndata.insect_pitfall_av_abundance.fishcatch, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(insect_pitfall_av_abundance)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish richness per 100 m3")) + ylab("Insect density (#/pitfall)")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.insect_pitfall_av_abundance.fishcatch,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.insect_pitfall_av_abundance.fishcatch
ggsave("C:Plots//Model-fitted//LME_insect_pitfall_av_abundance_fish_catch.png")


# Total cover vs. fish richness -----------------------------------------------------
#visualize different distributions
ggplot(fish_stats_zscores, aes(y=total_cover, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$total_cover)
qqp(fish_stats_zscores$total_cover, "lnorm")



#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.total_cover.fishcatch<-lme(total_cover ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
# glmm.total_cover.fishcatch<-glmmTMB((total_cover+0.01) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
# 
# AICtab(glmm.total_cover.fishcatch, lme.total_cover.fishcatch)
# 
summary(lme.total_cover.fishcatch)

#dwplot(list(glmmTMB=lme.total_cover.fishcatch,lmer=lmer.total_cover.fishcatch),by_2sd=TRUE)

colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.total_cover.fishcatch,type=c("p","smooth")),
             plot(lme.total_cover.fishcatch,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.total_cover.fishcatch,resid(.,type="pearson")~fish_richness_corrected,
                  type=c("p","smooth")),
             qqnorm(lme.total_cover.fishcatch,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))


## Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.total_cover.fishcatch<-lme.total_cover.fishcatch 
ndata.total_cover.fishcatch <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
                                                                  unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.total_cover.fishcatch <- add_column(ndata.total_cover.fishcatch, fit = predict(mod.total_cover.fishcatch, newdata = ndata.total_cover.fishcatch, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.total_cover.fishcatch)[-2], ndata.total_cover.fishcatch)
predvar <- diag(Designmat %*% vcov(mod.total_cover.fishcatch) %*% t(Designmat)) 

ndata.total_cover.fishcatch$SE <- sqrt(predvar) 
ndata.total_cover.fishcatch$SE2 <- sqrt(predvar+mod.total_cover.fishcatch$sigma^2)

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.total_cover.fishcatch$fish_richness_corrected.unscaled<-ndata.total_cover.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.total_cover.fishcatch <- ggplot(ndata.total_cover.fishcatch, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(total_cover)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish richness per 100 m3")) + ylab("Plant total cover")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.total_cover.fishcatch,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.total_cover.fishcatch
ggsave("C:Plots//Model-fitted//LME_Poisson_total_cover_fish_catch.png")



# Plant cover vs. fish abundance -------------------------------------------

#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.shrub_cover.fish_abundance<-lme(shrub_cover ~ fish_abundance_bym3, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lme.shrub_cover.fish_abundance_log<-lme(shrub_cover ~ fish_abundance_bym3_log, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)

glmm.shrub_cover.fish_abundance<-glmmTMB((shrub_cover+0.01) ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
glmm.shrub_cover.fish_abundance_log<-glmmTMB((shrub_cover+0.01) ~ fish_abundance_bym3_log + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(glmm.shrub_cover.fish_abundance_log, glmm.shrub_cover.fish_abundance, lme.shrub_cover.fish_abundance, lme.shrub_cover.fish_abundance_log)

summary(lme.shrub_cover.fish_abundance_log)

colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.shrub_cover.fish_abundance_log,type=c("p","smooth")),
             plot(lme.shrub_cover.fish_abundance_log,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.shrub_cover.fish_abundance_log,resid(.,type="pearson")~fish_abundance_bym3_log,
                  type=c("p","smooth")),
             qqnorm(lme.shrub_cover.fish_abundance_log,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))


## Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.shrub_cover.fish_abundance<-lme.shrub_cover.fish_abundance_log
ndata.shrub_cover.fish_abundance <- with(fish_stats_zscores, tibble(fish_abundance_bym3_log = seq(min(fish_abundance_bym3_log), max(fish_abundance_bym3_log),length = 100),
                                                                       unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data
ndata.shrub_cover.fish_abundance <- add_column(ndata.shrub_cover.fish_abundance, fit = predict(mod.shrub_cover.fish_abundance, newdata = ndata.shrub_cover.fish_abundance, type = 'response', level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.shrub_cover.fish_abundance)[-2], ndata.shrub_cover.fish_abundance)
predvar <- diag(Designmat %*% vcov(mod.shrub_cover.fish_abundance) %*% t(Designmat)) 

ndata.shrub_cover.fish_abundance$SE <- sqrt(predvar) 
ndata.shrub_cover.fish_abundance$SE2 <- sqrt(predvar+mod.shrub_cover.fish_abundance$sigma^2)

fish_stats_zscores$fish_abundance_bym3_log<-scale(fish_stats$fish_abundance_bym3_log, center=TRUE, scale=TRUE)

ndata.shrub_cover.fish_abundance$fish_abundance_bym3_log.unscaled<-ndata.shrub_cover.fish_abundance$fish_abundance_bym3_log * attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:center')



# plot 
plt.shrub_cover.fish_abundance <- ggplot(ndata.shrub_cover.fish_abundance, aes(x = fish_abundance_bym3_log.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(shrub_cover)), size=3, data = fish_stats_zscores)+
  xlab(expression("Log fish abundance per m3")) + ylab("Plant cover")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.shrub_cover.fish_abundance,aes(ymin = fit - 2*SE, ymax = fit+2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.shrub_cover.fish_abundance
ggsave("C:Plots//Model-fitted//LME_shrub_cover_fish_abundance.png")



# Plant evenness vs. fish richness ----------------------------------------------------------
##this one differs between 1km and 0.3km ... so I made the 300 m file


#visualize different distributions
ggplot(fish_stats_zscores, aes(y=plant_evenness, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$plant_evenness)
qqp(fish_stats_zscores$plant_evenness, "lnorm")

poisson.fish<-fitdistr(fish_stats_zscores$plant_evenness, "Poisson")
qqp(fish_stats_zscores$plant_evenness, "pois", lambda=poisson.fish$estimate[[1]])



#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.plant_evenness.fishcatch<-lme(plant_evenness ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)

glmm.plant_evenness.fishcatch<-glmmTMB((plant_evenness) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="binomial", na.action=na.omit)
glmm.plant_evenness.fishcatch_gam<-glmmTMB((plant_evenness+0.01) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(glmm.plant_evenness.fishcatch, lme.plant_evenness.fishcatch, glmm.plant_evenness.fishcatch_gam)

summary(lme.plant_evenness.fishcatch)

colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.plant_evenness.fishcatch,type=c("p","smooth")),
             plot(lme.plant_evenness.fishcatch,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.plant_evenness.fishcatch,resid(.,type="pearson")~fish_richness_corrected,
                  type=c("p","smooth")),
             qqnorm(lme.plant_evenness.fishcatch,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))


## Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.plant_evenness.fishcatch<-lme.plant_evenness.fishcatch 
ndata.plant_evenness.fishcatch <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
                                                               unq_isl = unq_isl[want]))

## add the fitted values by predicting from the model for the new data
ndata.plant_evenness.fishcatch <- add_column(ndata.plant_evenness.fishcatch, fit = predict(mod.plant_evenness.fishcatch, newdata = ndata.plant_evenness.fishcatch, level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.plant_evenness.fishcatch)[-2], ndata.plant_evenness.fishcatch)
predvar <- diag(Designmat %*% vcov(mod.plant_evenness.fishcatch) %*% t(Designmat)) 

ndata.plant_evenness.fishcatch$SE <- sqrt(predvar) 
ndata.plant_evenness.fishcatch$SE2 <- sqrt(predvar+mod.plant_evenness.fishcatch$sigma^2)

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.plant_evenness.fishcatch$fish_richness_corrected.unscaled<-ndata.plant_evenness.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.plant_evenness.fishcatch <- ggplot(ndata.plant_evenness.fishcatch, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(plant_evenness)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish richness per 100 m3")) + ylab("Plant evenness")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.plant_evenness.fishcatch,aes(ymin = fit - 2*SE, ymax =  fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.plant_evenness.fishcatch
ggsave("C:Plots//Model-fitted//LME_Poisson_plant_evenness_fish_catch.png")



# Plant evenness vs. fish abundance ---------------------------------------

###different 300 and 1km

#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.plant_evenness.fish_abundance<-lme(plant_evenness ~ fish_abundance_bym3, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lme.plant_evenness.fish_abundance_log<-lme(plant_evenness ~ fish_abundance_bym3_log, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)

glmm.plant_evenness.fish_abundance<-glmmTMB((plant_evenness) ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, family="binomial", na.action=na.omit)
glmm.plant_evenness.fish_abundance_log<-glmmTMB((plant_evenness) ~ fish_abundance_bym3_log + (1|unq_isl), data=fish_stats_zscores, family="binomial", na.action=na.omit)

glmm.plant_evenness.fish_abundance_gamma<-glmmTMB((plant_evenness+0.01) ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
glmm.plant_evenness.fish_abundance_log_gamma<-glmmTMB((plant_evenness+0.01) ~ fish_abundance_bym3_log + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(glmm.plant_evenness.fish_abundance_gamma, glmm.plant_evenness.fish_abundance_log_gamma, glmm.plant_evenness.fish_abundance_log, glmm.plant_evenness.fish_abundance, lme.plant_evenness.fish_abundance, lme.plant_evenness.fish_abundance_log)

summary(lme.plant_evenness.fish_abundance)


colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.plant_evenness.fish_abundance,type=c("p","smooth")),
             plot(lme.plant_evenness.fish_abundance,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.plant_evenness.fish_abundance,resid(.,type="pearson")~fish_abundance_bym3,
                  type=c("p","smooth")),
             qqnorm(lme.plant_evenness.fish_abundance,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))


## Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.plant_evenness.fish_abundance<-lme.plant_evenness.fish_abundance
ndata.plant_evenness.fish_abundance <- with(fish_stats_zscores, tibble(fish_abundance_bym3= seq(min(fish_abundance_bym3), max(fish_abundance_bym3),length = 100),
                                                                      unq_isl = unq_isl[want]))
ndata.plant_evenness.fish_abundance <- add_column(ndata.plant_evenness.fish_abundance, fit = predict(mod.plant_evenness.fish_abundance, newdata = ndata.plant_evenness.fish_abundance, type = 'response', level=0))


###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.plant_evenness.fish_abundance)[-2], ndata.plant_evenness.fish_abundance)
predvar <- diag(Designmat %*% vcov(mod.plant_evenness.fish_abundance) %*% t(Designmat)) 

ndata.plant_evenness.fish_abundance$SE <- sqrt(predvar) 
ndata.plant_evenness.fish_abundance$SE2 <- sqrt(predvar+mod.plant_evenness.fish_abundance$sigma^2)

fish_stats_zscores$fish_abundance_bym3<-scale(fish_stats$fish_abundance_bym3, center=TRUE, scale=TRUE)

ndata.plant_evenness.fish_abundance$fish_abundance_bym3.unscaled<-ndata.plant_evenness.fish_abundance$fish_abundance_bym3 * attr(fish_stats_zscores$fish_abundance_bym3, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3, 'scaled:center')

# plot 
plt.plant_evenness.fish_abundance <- ggplot(ndata.plant_evenness.fish_abundance, aes(x = fish_abundance_bym3.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(plant_evenness)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish abundance per m3")) + ylab("Plant_evenness")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.plant_evenness.fish_abundance,aes(ymin = fit - 2*SE, ymax = fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.plant_evenness.fish_abundance
ggsave("C:Plots//Model-fitted//GLMM_Poisson_plant_evenness_fish_abundance.png")



# Plant richness vs. fish richness ----------------------------------------------------------

#visualize different distributions
ggplot(fish_stats_zscores, aes(y=plant_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$plant_richness)
qqp(fish_stats_zscores$plant_richness, "lnorm")

poisson.fish<-fitdistr(fish_stats_zscores$plant_richness, "Poisson")
qqp(fish_stats_zscores$plant_richness, "pois", lambda=poisson.fish$estimate[[1]])

View(fish_stats_zscores)

#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.plant_richness.fishcatch<-lme(plant_richness ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)

glmm.plant_richness.fishcatch<-glmmTMB((plant_richness) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="poisson", na.action=na.omit)
glmm.plant_richness.fishcatch.admb<-glmmadmb((plant_richness) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="poisson")

AICtab( glmm.plant_richness.fishcatch, lme.plant_richness.fishcatch, glmm.plant_richness.fishcatch.admb)

summary(glmm.plant_richness.fishcatch)

#dwplot(list(glmmTMB=glmm.plant_richness.fishcatch,lmer=lmer.plant_richness.fishcatch),by_2sd=TRUE)

## Visualizing glmm residuals with Dharma package
simulationOutput <- simulateResiduals(fittedModel = glmm.plant_richness.fishcatch)
plot(simulationOutput)
testZeroInflation(simulationOutput)
plot(simulationOutput, quantreg = T)
#so our model is not great still.... 

# Plot the residuals against island level/ transect
augDat <- data.frame(fish_stats_zscores,resid=residuals(glmm.plant_richness.fishcatch,type="pearson"),
                     fitted=fitted(glmm.plant_richness.fishcatch))
ggplot(augDat,aes(x=unq_isl,y=resid))+geom_boxplot()+coord_flip()



## Extracting coefficients and plotting
fam.glmm.plant_richness.fishcatch <- family(glmm.plant_richness.fishcatch )
fam.glmm.plant_richness.fishcatch
ilink.glmm.plant_richness.fishcatch<- fam.glmm.plant_richness.fishcatch$linkinv
ilink.glmm.plant_richness.fishcatch

want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.plant_richness.fishcatch<-glmm.plant_richness.fishcatch 
ndata.plant_richness.fishcatch <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
                                                        unq_isl = unq_isl[want]))


#from http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
## design matrix (fixed effects)
library(glmmADMB)
glmm.plant_richness.fishcatch_admb<-glmmadmb(plant_richness ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="poisson")

fm2<-glmm.plant_richness.fishcatch_admb
newdat<-ndata.plant_richness.fishcatch
## design matrix (fixed effects)
mm <- model.matrix(delete.response(terms(fm2)),newdat)
## linear predictor (for GLMMs, back-transform this with the
##  inverse link function (e.g. plogis() for binomial, beta;
##  exp() for Poisson, negative binomial
newdat$fit_link <- drop(mm %*% fixef(fm2))
predvar <- diag(mm %*% vcov(fm2) %*% t(mm))
newdat$se_link <- sqrt(predvar) 



# ## add the fitted values by predicting from the model for the new data
# ndata.plant_richness.fishcatch <- add_column(ndata.plant_richness.fishcatch, fit = predict(mod.plant_richness.fishcatch, newdata = ndata.plant_richness.fishcatch, type = 'response', level=0))
# 
# ndata.plant_richness.fishcatch <- bind_cols(ndata.plant_richness.fishcatch, setNames(as_tibble(predict(mod.plant_richness.fishcatch, ndata.plant_richness.fishcatch, level=0, se.fit = TRUE)[1:2]),
#                                                                  c('fit_link','se_link')))
ndata.plant_richness.fishcatch<-newdat
## create the interval and backtransform

ndata.plant_richness.fishcatch <- mutate(ndata.plant_richness.fishcatch,
                               fit_resp  = ilink.glmm.plant_richness.fishcatch(fit_link),
                               right_upr = ilink.glmm.plant_richness.fishcatch(fit_link + (2 * se_link)),
                               right_lwr = ilink.glmm.plant_richness.fishcatch(fit_link - (2 * se_link)))

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.plant_richness.fishcatch$fish_richness_corrected.unscaled<-ndata.plant_richness.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.plant_richness.fishcatch <- ggplot(ndata.plant_richness.fishcatch, aes(x = fish_richness_corrected.unscaled, y = fit_resp)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(plant_richness)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish richness per 100 m3")) + ylab("Plant richness")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.plant_richness.fishcatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.plant_richness.fishcatch
ggsave("C:Plots//Model-fitted//GLMM_Poisson_plant_richness_fish_catch.png")


##### gg predict is not for mixed models
require(ggiraph)
require(ggiraphExtra)
require(plyr)
require(moonBook)  
ggPredict(glmm.plant_richness.fishcatch, se=TRUE)




# Plant richness vs. fish abundance ---------------------------------------


#visualize different distributions
ggplot(fish_stats_zscores, aes(y=plant_richness, x=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$plant_richness)
qqp(fish_stats_zscores$plant_richness, "lnorm")
#normal and Gamma both good, have a few obs. outside 


#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.plant_richness.fish_abundance<-lme(plant_richness ~ fish_abundance_bym3, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lme.plant_richness.fish_abundance_log<-lme(plant_richness ~ fish_abundance_bym3_log, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)

glmm.plant_richness.fish_abundance<-glmmTMB((plant_richness) ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, family="poisson", na.action=na.omit)
glmm.plant_richness.fish_abundance_log<-glmmTMB((plant_richness) ~ fish_abundance_bym3_log + (1|unq_isl), data=fish_stats_zscores, family="poisson", na.action=na.omit)

AICtab(glmm.plant_richness.fish_abundance_log, glmm.plant_richness.fish_abundance, lme.plant_richness.fish_abundance, lme.plant_richness.fish_abundance_log)

summary(glmm.plant_richness.fish_abundance_log)

## Visualizing glmm residuals with Dharma package
simulationOutput <- simulateResiduals(fittedModel = glmm.plant_richness.fish_abundance_log)
plot(simulationOutput)
testZeroInflation(simulationOutput)
plot(simulationOutput, quantreg = T)
#so our model is not great still.... 

# Plot the residuals against island level/ transect
augDat <- data.frame(fish_stats_zscores,resid=residuals(glmm.plant_richness.fish_abundance,type="pearson"),
                     fitted=fitted(glmm.plant_richness.fish_abundance))
ggplot(augDat,aes(x=unq_isl,y=resid))+geom_boxplot()+coord_flip()



## Extracting coefficients and plotting
fam.glmm.plant_richness.fish_abundance <- family(glmm.plant_richness.fish_abundance_log)
fam.glmm.plant_richness.fish_abundance
str(fam.glmm.plant_richness.fish_abundance)
ilink.glmm.plant_richness.fish_abundance<- fam.glmm.plant_richness.fish_abundance$linkinv
ilink.glmm.plant_richness.fish_abundance

want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.plant_richness.fish_abundance<-glmm.plant_richness.fish_abundance_log 
ndata.plant_richness.fish_abundance <- with(fish_stats_zscores, tibble(fish_abundance_bym3_log = seq(min(fish_abundance_bym3_log), max(fish_abundance_bym3_log),length = 100),
                                                             unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data

fm2<-glmm.plant_richness.fish_abundance_log 
newdat<-ndata.plant_richness.fish_abundance 
fixef(fm2)
matrix.fixef<-fixef(fm2)
str(matrix.fixef)
matrix.fixef$cond

vcov.fm2<-vcov(fm2)
str(vcov.fm2)
vcov.fm2$cond

mm <- model.matrix(delete.response(terms(fm2)),newdat)
newdat$fit_link <- drop(mm %*% matrix.fixef$cond)
predvar <- diag(mm %*% vcov.fm2$cond %*% t(mm))
newdat$se_link <- sqrt(predvar) 

ndata.plant_richness.fish_abundance<-newdat
## create the interval and backtransform

ndata.plant_richness.fish_abundance <- mutate(ndata.plant_richness.fish_abundance,
                                    fit_resp  = ilink.glmm.plant_richness.fish_abundance(fit_link),
                                    right_upr = ilink.glmm.plant_richness.fish_abundance(fit_link + (2 * se_link)),
                                    right_lwr = ilink.glmm.plant_richness.fish_abundance(fit_link - (2 * se_link)))

fish_stats_zscores$fish_abundance_bym3_log<-scale(fish_stats$fish_abundance_bym3_log, center=TRUE, scale=TRUE)

ndata.plant_richness.fish_abundance$fish_abundance_bym3_log.unscaled<-ndata.plant_richness.fish_abundance$fish_abundance_bym3_log * attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:center')


# plot 
plt.plant_richness.fish_abundance <- ggplot(ndata.plant_richness.fish_abundance, aes(x = fish_abundance_bym3_log.unscaled, y = fit_resp)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(plant_richness)), size=3, data = fish_stats_zscores)+
  xlab(expression("Log fish abundance per m3")) + ylab("Plant richness")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.plant_richness.fish_abundance,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.plant_richness.fish_abundance
ggsave("C:Plots//Model-fitted//GLMM_Poisson_plant_richness_fish_abundance.png")


# Tree richness vs. fish richness -----------------------------------------------------------

#visualize different distributions
ggplot(fish_stats_zscores, aes(y=tree_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$tree_richness)
qqp(fish_stats_zscores$tree_richness, "lnorm")

poisson.fish<-fitdistr(fish_stats_zscores$tree_richness, "Poisson")
qqp(fish_stats_zscores$tree_richness, "pois", lambda=poisson.fish$estimate[[1]])



#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.tree_richness.fishcatch<-lme(tree_richness ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lmer.tree_richness.fishcatch<-lmer(tree_richness ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, na.action=na.omit)

glmm.tree_richness.fishcatch<-glmmTMB((tree_richness) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="poisson", na.action=na.omit)
#glmm.1.tree_richness.fishcatch<-glmmTMB((tree_richness+1) ~ fish_richness_corrected + (1+fish_richness_corrected|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(lmer.tree_richness.fishcatch, glmm.tree_richness.fishcatch, lme.tree_richness.fishcatch)

summary(lme.tree_richness.fishcatch)

colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.tree_richness.fishcatch,type=c("p","smooth")),
             plot(lme.tree_richness.fishcatch,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.tree_richness.fishcatch,resid(.,type="pearson")~log(fish_richness_corrected+1),
                  type=c("p","smooth")),
             qqnorm(lme.tree_richness.fishcatch,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))


## Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.tree_richness.fishcatch<-lme.tree_richness.fishcatch
ndata.tree_richness.fishcatch <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
                                                                    unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data
ndata.tree_richness.fishcatch <- add_column(ndata.tree_richness.fishcatch, fit = predict(mod.tree_richness.fishcatch, newdata = ndata.tree_richness.fishcatch, type = 'response', level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.tree_richness.fishcatch)[-2], ndata.tree_richness.fishcatch)
predvar <- diag(Designmat %*% vcov(mod.tree_richness.fishcatch) %*% t(Designmat)) 

ndata.tree_richness.fishcatch$SE <- sqrt(predvar) 
ndata.tree_richness.fishcatch$SE2 <- sqrt(predvar+mod.tree_richness.fishcatch$sigma^2)

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.tree_richness.fishcatch$fish_richness_corrected.unscaled<-ndata.tree_richness.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')



# plot 
plt.tree_richness.fishcatch <- ggplot(ndata.tree_richness.fishcatch, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(tree_richness)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish richness per 100 m3")) + ylab("Tree richness")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.tree_richness.fishcatch,aes(ymin = fit - 2*SE, ymax = fit + 2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.tree_richness.fishcatch
ggsave("C:Plots//Model-fitted//GLMM_Poisson_tree_richness_fish_catch.png")



# Tree richness vs. Fish abundance ----------------------------------------
#visualize different distributions
ggplot(fish_stats_zscores, aes(y=tree_richness, x=fish_abundance_bym3_log))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$tree_richness)
qqp(fish_stats_zscores$tree_richness, "lnorm")
#normal and Gamma both good, have a few obs. outside 


#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.tree_richness.fish_abundance<-lme(tree_richness ~ fish_abundance_bym3, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lme.tree_richness.fish_abundance_log<-lme(tree_richness ~ fish_abundance_bym3_log, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)

lmer.tree_richness.fish_abundance<-lmer(tree_richness ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, na.action=na.omit)
lmer.tree_richness.fish_abundance_log<-lmer(tree_richness ~ fish_abundance_bym3_log +  (1+fish_abundance_bym3|unq_isl), data=fish_stats_zscores, na.action=na.omit)

glmm.tree_richness.fish_abundance<-glmmTMB((tree_richness) ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, family="poisson", na.action=na.omit)
glmm.tree_richness.fish_abundance_log<-glmmTMB((tree_richness) ~ fish_abundance_bym3_log + (1|unq_isl), data=fish_stats_zscores, family="poisson", na.action=na.omit)

AICtab(lmer.tree_richness.fish_abundance, lmer.tree_richness.fish_abundance_log, glmm.tree_richness.fish_abundance_log, glmm.tree_richness.fish_abundance, lme.tree_richness.fish_abundance, lme.tree_richness.fish_abundance_log)

summary(lme.tree_richness.fish_abundance_log)

colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.tree_richness.fish_abundance_log,type=c("p","smooth")),
             plot(lme.tree_richness.fish_abundance_log,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.tree_richness.fish_abundance_log,resid(.,type="pearson")~fish_abundance_bym3_log,
                  type=c("p","smooth")),
             qqnorm(lme.tree_richness.fish_abundance_log,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))


## Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.tree_richness.fish_abundance<-lme.tree_richness.fish_abundance_log
ndata.tree_richness.fish_abundance <- with(fish_stats_zscores, tibble(fish_abundance_bym3_log = seq(min(fish_abundance_bym3_log), max(fish_abundance_bym3_log),length = 100),
                                                                    unq_isl = unq_isl[want]))
ndata.tree_richness.fish_abundance <- add_column(ndata.tree_richness.fish_abundance, fit = predict(mod.tree_richness.fish_abundance, newdata = ndata.tree_richness.fish_abundance, type = 'response', level=0))


###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.tree_richness.fish_abundance)[-2], ndata.tree_richness.fish_abundance)
predvar <- diag(Designmat %*% vcov(mod.tree_richness.fish_abundance) %*% t(Designmat)) 

ndata.tree_richness.fish_abundance$SE <- sqrt(predvar) 
ndata.tree_richness.fish_abundance$SE2 <- sqrt(predvar+mod.tree_richness.fish_abundance$sigma^2)

fish_stats_zscores$fish_abundance_bym3_log<-scale(fish_stats$fish_abundance_bym3_log, center=TRUE, scale=TRUE)

ndata.tree_richness.fish_abundance$fish_abundance_bym3_log.unscaled<-ndata.tree_richness.fish_abundance$fish_abundance_bym3_log * attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:center')

# plot 
plt.tree_richness.fish_abundance <- ggplot(ndata.tree_richness.fish_abundance, aes(x = fish_abundance_bym3_log.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(tree_richness)), size=3, data = fish_stats_zscores)+
  xlab(expression("Log fish abundance per m3")) + ylab("Tree richness")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.tree_richness.fish_abundance,aes(ymin = fit - 2*SE2, ymax = fit + 2*SE2), alpha = 0.10)+
  theme(legend.position="none")
plt.tree_richness.fish_abundance
ggsave("C:Plots//Model-fitted//GLMM_Poisson_tree_richness_fish_abundance.png")

# Beachseine only data ----------------------------------------------------

beachseine_stats<-read.csv("C:Output files//fish_bycatch_richness_merged_tran_year.csv")

beachseine_stats<-beachseine_stats[,-1]
beachseine_stats$fish_abundance_bym3_log<-log(beachseine_stats$fish_richness_bym3+1)
beachseine_stats_zscores<-beachseine_stats

beachseine_stats_zscores$fish_abundance_bym3_log<-scale(beachseine_stats$fish_abundance_bym3_log, center=TRUE, scale=TRUE)
beachseine_stats_zscores$fish_abundance_bym3_log.unscaled <-beachseine_stats$fish_abundance_bym3_log * attr(beachseine_stats_zscores$fish_abundance_bym3_log, 'scaled:scale') + attr(beachseine_stats_zscores$fish_abundance_bym3_log, 'scaled:center')

beachseine_stats_zscores$fish_abundance_bym3<-scale(beachseine_stats$fish_abundance_bym3, center=TRUE, scale=TRUE)
beachseine_stats_zscores$fish_abundance_bym3.unscaled <-beachseine_stats_zscores$fish_abundance_bym3 * attr(beachseine_stats_zscores$fish_abundance_bym3, 'scaled:scale') + attr(beachseine_stats_zscores$fish_abundance_bym3, 'scaled:center')

#visualize different distributions
ggplot(beachseine_stats_zscores, aes(x=fish_abundance_bym3_log, y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(beachseine_stats_zscores$fish_richness_corrected)
qqp(beachseine_stats_zscores$fish_richness_corrected, "lnorm")
gamma.12.fish_richness_corrected<-fitdistr(beachseine_stats_zscores$fish_richness_corrected+1, "gamma")
qqp(beachseine_stats$fish_richness_corrected, "gamma", shape = gamma.12.fish_richness_corrected$estimate[[1]], rate = gamma.12.fish_richness_corrected$estimate[[2]])
#normal and Gamma both good, have a few obs. outside 


#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lm.fish_speciesvabund<-lm(fish_richness_corrected ~ fish_abundance_bym3, data=beachseine_stats_zscores)
lm.fish_speciesvabund_log<-lm(fish_richness_corrected ~ as.vector(fish_abundance_bym3_log), data=beachseine_stats_zscores)

glm.fish_speciesvabund<-glm(fish_richness_corrected ~ fish_abundance_bym3, data=beachseine_stats_zscores, family="poisson")
glm.fish_speciesvabund_log<-glm((fish_richness_corrected) ~ fish_abundance_bym3_log, data=beachseine_stats_zscores, family="poisson", na.action=na.omit)

AICtab(glm.fish_speciesvabund, glm.fish_speciesvabund_log, lm.fish_speciesvabund_log, lm.fish_speciesvabund)

summary(lm.fish_speciesvabund_log)

#dwplot(list(glmmTMB=glmm.fish_speciesvabund,lmer=lmer.fish_speciesvabund),by_2sd=TRUE)

plot(lm.fish_speciesvabund_log)

r2.corr.mer(lm.fish_speciesvabund_log)

## Extracting coefficients and plotting
fam.lm.fish_speciesvabund_log <- family(lm.fish_speciesvabund_log)
fam.lm.fish_speciesvabund_log
str(fam.lm.fish_speciesvabund_log)
ilink.lm.fish_speciesvabund_log<- fam.lm.fish_speciesvabund_log$linkinv
ilink.lm.fish_speciesvabund_log

want <- seq(1, nrow(beachseine_stats_zscores), length.out = 100)
ndata.fish_speciesvabund<- with(beachseine_stats_zscores, tibble(fish_abundance_bym3_log = seq(min(fish_abundance_bym3_log), max(fish_abundance_bym3_log),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.fish_speciesvabund<- add_column(ndata.fish_speciesvabund, fit = predict(lm.fish_speciesvabund_log, newdata = ndata.fish_speciesvabund, type = 'response'))

ndata.fish_speciesvabund<- bind_cols(ndata.fish_speciesvabund, setNames(as_tibble(predict(lm.fish_speciesvabund_log, ndata.fish_speciesvabund, se.fit = TRUE)[1:2]),
                                                                 c('fit_link','se_link')))

## create the interval and backtransform

ndata.fish_speciesvabund<- mutate(ndata.fish_speciesvabund,
                               fit_resp  = ilink.lm.fish_speciesvabund(fit_link),
                               right_upr = ilink.lm.fish_speciesvabund(fit_link + (2 * se_link)),
                               right_lwr = ilink.lm.fish_speciesvabund(fit_link - (2 * se_link)))

str(beachseine_stats_zscores)
beachseine_stats_zscores$fish_abundance_bym3_log<-scale(beachseine_stats$fish_abundance_bym3_log, center=TRUE, scale=TRUE)

ndata.fish_speciesvabund$fish_abundance_bym3_log.unscaled<-ndata.fish_speciesvabund$fish_abundance_bym3_log * attr(beachseine_stats_zscores$fish_abundance_bym3_log, 'scaled:scale') + attr(beachseine_stats_zscores$fish_abundance_bym3_log, 'scaled:center')

# plot 
plt.fish_speciesvabund <- ggplot(ndata.fish_speciesvabund, aes(x = fish_abundance_bym3_log, y = fit_resp)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(fish_richness_corrected)), size=3, data = beachseine_stats_zscores)+
  xlab(expression("Log Fish abundance per m3")) + ylab("Fish richness per 100m3")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.fish_speciesvabund,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.fish_speciesvabund
ggsave("C:Plots//Model-fitted//LM_fish_speciesvabund.png")

# Fish biomass and N15 ---------------------------------------------------

#visualize different distributions
ggplot(fish_stats_zscores, aes(y=d15n, x=fish_biomass_bym3_mean))+geom_point()+geom_smooth(method="lm")+theme_classic()
qqp(fish_stats_zscores$d15n)
qqp(fish_stats_zscores$d15n, "lnorm")
gamma.12.fish_biomass_bym3_mean<-fitdistr(fish_stats_zscores$d15n+1, "gamma")
qqp(fish_stats$fish_biomass_bym3_mean, "gamma", shape = gamma.12.fish_biomass_bym3_mean$estimate[[1]], rate = gamma.12.fish_biomass_bym3_mean$estimate[[2]])
#normal and Gamma both good, have a few obs. outside 


#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.d15n.fishbiomass<-lme(d15n ~ fish_biomass_bym3_mean, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
#lme.1.d15n.fishbiomass<-lme(d15n ~ fish_biomass_bym3_mean, random= ~1+fish_biomass_bym3_mean|unq_isl, data=fish_stats_zscores, na.action=na.omit)


glmm.d15n.fishbiomass<-glmmTMB((d15n+1.5) ~ fish_biomass_bym3_mean + (1|unq_isl), data=fish_stats_zscores, family="Gamma")

AICtab(glmm.d15n.fishbiomass, lme.d15n.fishbiomass)

summary(glmm.d15n.fishbiomass)

r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

r2.corr.mer(glmm.d15n.fishbiomass)


## Visualizing glmm residuals with Dharma package
simulationOutput <- simulateResiduals(fittedModel = glmm.d15n.fishbiomass)
plot(simulationOutput)
testZeroInflation(simulationOutput)
plot(simulationOutput, quantreg = T)
#so our model is not great still.... 

# Plot the residuals against island level/ transect
augDat <- data.frame(fish_stats_zscores,resid=residuals(glmm.d15n.fishbiomass,type="pearson"),
                     fitted=fitted(glmm.d15n.fishbiomass))
ggplot(augDat,aes(x=unq_tran,y=resid))+geom_boxplot()+coord_flip()



## Extracting coefficients and plotting
fam.glmm.d15n.fishbiomass <- family(glmm.d15n.fishbiomass)
fam.glmm.d15n.fishbiomass
str(fam.glmm.d15n.fishbiomass)
ilink.glmm.d15n.fishbiomass<- fam.glmm.d15n.fishbiomass$linkinv
ilink.glmm.d15n.fishbiomass

want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.d15n.fishbiomass<-glmm.d15n.fishbiomass
ndata.d15n.fishbiomass <- with(fish_stats_zscores, tibble(fish_biomass_bym3_mean = seq(min(fish_biomass_bym3_mean), max(fish_biomass_bym3_mean),length = 100),
                                                        unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data
ndata.d15n.fishbiomass <- add_column(ndata.d15n.fishbiomass, fit = predict(mod.d15n.fishbiomass, newdata = ndata.d15n.fishbiomass, type = 'response', level=0))
ndata.d15n.fishbiomass <- bind_cols(ndata.d15n.fishbiomass, setNames(as_tibble(predict(mod.d15n.fishbiomass, ndata.d15n.fishbiomass, level=0, se.fit = TRUE)[1:2]),
                                                                 c('fit_link','se_link')))

## create the interval and backtransform

## add the fitted values by predicting from the model for the new data

fm2<-glmm.d15n.fishbiomass
newdat<-ndata.d15n.fishbiomass 
fixef(fm2)
matrix.fixef<-fixef(fm2)
str(matrix.fixef)
matrix.fixef$cond

vcov.fm2<-vcov(fm2)
str(vcov.fm2)
vcov.fm2$cond

mm <- model.matrix(delete.response(terms(fm2)),newdat)
newdat$fit_link <- drop(mm %*% matrix.fixef$cond)
predvar <- diag(mm %*% vcov.fm2$cond %*% t(mm))
newdat$se_link <- sqrt(predvar) 

ndata.d15n.fishbiomass<-newdat
## create the interval and backtransform

ndata.d15n.fishbiomass <- mutate(ndata.d15n.fishbiomass,
                               fit_resp  = ilink.glmm.d15n.fishbiomass(fit_link),
                               right_upr = ilink.glmm.d15n.fishbiomass(fit_link + (2 * se_link)),
                               right_lwr = ilink.glmm.d15n.fishbiomass(fit_link - (2 * se_link)))

fish_stats_zscores$fish_biomass_bym3_mean<-scale(fish_stats$fish_biomass_bym3_mean, center=TRUE, scale=TRUE)

ndata.d15n.fishbiomass$fish_biomass_bym3_mean.unscaled<-ndata.d15n.fishbiomass$fish_biomass_bym3_mean * attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:scale') + attr(fish_stats_zscores$fish_biomass_bym3_mean, 'scaled:center')


# plot 
plt.d15n.fishbiomass <- ggplot(ndata.d15n.fishbiomass, aes(x = fish_biomass_bym3_mean.unscaled, y = fit_resp)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n+1.5)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish biomass g per m3")) + ylab("Soil d15n at shoreline")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.fishbiomass,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.fishbiomass
ggsave("C:Plots//Model-fitted//GLMM_Gamma_d15n_fishbiomass.png")


# Fish richness and N15 ---------------------------------------------------

#visualize different distributions
ggplot(fish_stats_zscores, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")+theme_classic()
qqp(fish_stats_zscores$d15n)
qqp(fish_stats_zscores$d15n, "lnorm")
gamma.12.fish_richness_corrected<-fitdistr(fish_stats_zscores$d15n+1, "gamma")
qqp(fish_stats$fish_richness_corrected, "gamma", shape = gamma.12.fish_richness_corrected$estimate[[1]], rate = gamma.12.fish_richness_corrected$estimate[[2]])
#normal and Gamma both good, have a few obs. outside 


#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.d15n.fishcatch<-lme(d15n ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
#lme.1.d15n.fishcatch<-lme(d15n ~ fish_richness_corrected, random= ~1+fish_richness_corrected|unq_isl, data=fish_stats_zscores, na.action=na.omit)


glmm.d15n.fishcatch<-glmmTMB((d15n+1.5) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma")
glmm.d15n.fishcatch_admb<-glmmadmb((d15n+1.5) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma")


AICtab(glmm.d15n.fishcatch, lme.d15n.fishcatch)

summary(glmm.d15n.fishcatch)

r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

r2.corr.mer(glmm.d15n.fishcatch)


## Visualizing glmm residuals with Dharma package
simulationOutput <- simulateResiduals(fittedModel = glmm.d15n.fishcatch)
plot(simulationOutput)
testZeroInflation(simulationOutput)
plot(simulationOutput, quantreg = T)
#so our model is not great still.... 

# Plot the residuals against island level/ transect
augDat <- data.frame(fish_stats_zscores,resid=residuals(glmm.d15n.fishcatch,type="pearson"),
                     fitted=fitted(glmm.d15n.fishcatch))
ggplot(augDat,aes(x=unq_tran,y=resid))+geom_boxplot()+coord_flip()



## Extracting coefficients and plotting
fam.glmm.d15n.fishcatch <- family(glmm.d15n.fishcatch)
fam.glmm.d15n.fishcatch
str(fam.glmm.d15n.fishcatch)
ilink.glmm.d15n.fishcatch<- fam.glmm.d15n.fishcatch$linkinv
ilink.glmm.d15n.fishcatch

want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.d15n.fishcatch<-glmm.d15n.fishcatch
ndata.d15n.fishcatch <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
                                                        unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data
ndata.d15n.fishcatch <- add_column(ndata.d15n.fishcatch, fit = predict(mod.d15n.fishcatch, newdata = ndata.d15n.fishcatch, type = 'response', level=0))
ndata.d15n.fishcatch <- bind_cols(ndata.d15n.fishcatch, setNames(as_tibble(predict(mod.d15n.fishcatch, ndata.d15n.fishcatch, level=0, se.fit = TRUE)[1:2]),
                                                                           c('fit_link','se_link')))

## create the interval and backtransform

## add the fitted values by predicting from the model for the new data

fm2<-glmm.d15n.fishcatch
newdat<-ndata.d15n.fishcatch 
fixef(fm2)
matrix.fixef<-fixef(fm2)
str(matrix.fixef)
matrix.fixef$cond

vcov.fm2<-vcov(fm2)
str(vcov.fm2)
vcov.fm2$cond

mm <- model.matrix(delete.response(terms(fm2)),newdat)
newdat$fit_link <- drop(mm %*% matrix.fixef$cond)
predvar <- diag(mm %*% vcov.fm2$cond %*% t(mm))
newdat$se_link <- sqrt(predvar) 
                                                            
ndata.d15n.fishcatch<-newdat
## create the interval and backtransform

ndata.d15n.fishcatch <- mutate(ndata.d15n.fishcatch,
                                 fit_resp  = ilink.glmm.d15n.fishcatch(fit_link),
                                 right_upr = ilink.glmm.d15n.fishcatch(fit_link + (2 * se_link)),
                                 right_lwr = ilink.glmm.d15n.fishcatch(fit_link - (2 * se_link)))

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.d15n.fishcatch$fish_richness_corrected.unscaled<-ndata.d15n.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.d15n.fishcatch <- ggplot(ndata.d15n.fishcatch, aes(x = fish_richness_corrected.unscaled, y = fit_resp)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n+1.5)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish richness per 100 m3")) + ylab("Soil d15n at shoreline")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.fishcatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.fishcatch
ggsave("C:Plots//Model-fitted//GLMM_Gamma_d15n_fish_catch.png")


# Fish abundance and N15 ----------------------------------------------------------
#visualize different distributions
ggplot(fish_stats_zscores, aes(y=d15n, x=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$d15n)
qqp(fish_stats_zscores$d15n, "lnorm")
gamma.12.fish_abundance_bym3<-fitdistr(fish_stats_zscores$d15n+1, "gamma")
qqp(fish_stats$fish_abundance_bym3, "gamma", shape = gamma.12.fish_abundance_bym3$estimate[[1]], rate = gamma.12.fish_abundance_bym3$estimate[[2]])
#normal and Gamma both good, have a few obs. outside 


#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.d15n.fish_abundance<-lme(d15n ~ fish_abundance_bym3, random= ~1|unq_isl, data=fish_stats_zscores)
lme.d15n.fish_abundance_log<-lme(d15n ~ fish_abundance_bym3_log, random= ~1|unq_isl, data=fish_stats_zscores)

glmm.d15n.fish_abundance<-glmmTMB((d15n+1.5) ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, family="Gamma")
glmm.d15n.fish_abundance_log<-glmmTMB((d15n+1.5) ~ fish_abundance_bym3_log + (1|unq_isl), data=fish_stats_zscores, family="Gamma")

AICtab(glmm.d15n.fish_abundance_log, glmm.d15n.fish_abundance, lme.d15n.fish_abundance, lme.d15n.fish_abundance_log)

summary(glmm.d15n.fish_abundance_log)

#dwplot(list(glmmTMB=glmm.d15n.fish_abundance,lmer=lmer.d15n.fish_abundance),by_2sd=TRUE)
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

r2.corr.mer(glmm.d15n.fish_abundance_log)


## Visualizing glmm residuals with Dharma package
simulationOutput <- simulateResiduals(fittedModel = glmm.d15n.fish_abundance_log)
plot(simulationOutput)
testZeroInflation(simulationOutput)
plot(simulationOutput, quantreg = T)
#so our model is not great still.... 

# Plot the residuals against island level/ transect
augDat <- data.frame(fish_stats_zscores,resid=residuals(glmm.d15n.fish_abundance,type="pearson"),
                     fitted=fitted(glmm.d15n.fish_abundance))
ggplot(augDat,aes(x=unq_isl,y=resid))+geom_boxplot()+coord_flip()



## Extracting coefficients and plotting
fam.glmm.d15n.fish_abundance <- family(glmm.d15n.fish_abundance_log)
fam.glmm.d15n.fish_abundance
str(fam.glmm.d15n.fish_abundance)
ilink.glmm.d15n.fish_abundance<- fam.glmm.d15n.fish_abundance$linkinv
ilink.glmm.d15n.fish_abundance

want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.d15n.fish_abundance<-glmm.d15n.fish_abundance_log 
ndata.d15n.fish_abundance <- with(fish_stats_zscores, tibble(fish_abundance_bym3_log = seq(min(fish_abundance_bym3_log), max(fish_abundance_bym3_log),length = 100),
                                                        unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data

fm2<-glmm.d15n.fish_abundance_log
newdat<-ndata.d15n.fish_abundance 
fixef(fm2)
matrix.fixef<-fixef(fm2)
str(matrix.fixef)
matrix.fixef$cond

vcov.fm2<-vcov(fm2)
str(vcov.fm2)
vcov.fm2$cond

mm <- model.matrix(delete.response(terms(fm2)),newdat)
newdat$fit_link <- drop(mm %*% matrix.fixef$cond)
predvar <- diag(mm %*% vcov.fm2$cond %*% t(mm))
newdat$se_link <- sqrt(predvar) 

ndata.d15n.fish_abundance<-newdat

## create the interval and backtransform

ndata.d15n.fish_abundance <- mutate(ndata.d15n.fish_abundance,
                               fit_resp  = ilink.glmm.d15n.fish_abundance(fit_link),
                               right_upr = ilink.glmm.d15n.fish_abundance(fit_link + (2 * se_link)),
                               right_lwr = ilink.glmm.d15n.fish_abundance(fit_link - (2 * se_link)))

fish_stats_zscores$fish_abundance_bym3_log<-scale(fish_stats$fish_abundance_bym3_log, center=TRUE, scale=TRUE)

ndata.d15n.fish_abundance$fish_abundance_bym3_log.unscaled<-ndata.d15n.fish_abundance$fish_abundance_bym3_log * attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:center')


# plot 
plt.d15n.fish_abundance <- ggplot(ndata.d15n.fish_abundance, aes(x = fish_abundance_bym3_log.unscaled, y = fit_resp)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n+1.5)), size=3, data = fish_stats_zscores)+
  xlab(expression("Log fish abundance per m3")) + ylab("Soil d15n at shoreline")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.fish_abundance,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.fish_abundance
ggsave("C:Plots//Model-fitted//GLMM_Gamma_d15n_fish_abundance.png")



# Combining Plots ----------------------------------------------------
library(cowplot)

plot_grid(plt.bird.density.fishbiomass, plt.bird.density.fishcatch,ncol=2, align='v', axis = 'l')

soil_plots<-plot_grid(plt.d15n.fishcatch, plt.d15n.fish_abundance, plt.fish_speciesvabund, ncol=3, align='v', axis = 'l')
soil_plots
                 
ggplot2::ggsave("C:Plots//Model-fitted//soil_plots.png", width=60, height=20, units="cm")
                          

plant_plots<-plot_grid(plt.plant_richness.fishcatch, plt.plant_evenness.fishcatch, plt.shrub_cover.fishcatch,
                       plt.plant_richness.fish_abundance, plt.plant_evenness.fish_abundance, plt.shrub_cover.fish_abundance , ncol=3, align='v', axis = 'l')
plant_plots

ggplot2::ggsave("C:Plots//Model-fitted//plant_plots.png", width=60, height=40, units="cm")

plant_plots_cover<-plot_grid(plt.total_cover.fishbiomass, plt.sum_basal.fishbiomass, plt.NDVI_mean.fishbiomass,
                        ncol=3, align='v', axis = 'l')
plant_plots_cover

ggplot2::ggsave("C:Plots//Model-fitted//plant_plots_cover.png", width=30, height=10, units="cm")


plant_plots_cover_richness<-plot_grid(plt.total_cover.fishcatch, plt.sum_basal.fishcatch, plt.NDVI_mean.fishcatch,
                             ncol=3, align='v', axis = 'l')
plant_plots_cover_richness
ggplot2::ggsave("C:Plots//Model-fitted//plant_plots_cover_richness.png", width=30, height=10, units="cm")


insect_plots_cover<-plot_grid(plt.insect_detritivore_beat_av_abundance.fishbiomass,plt.insect_herbivore_beat_av_abundance.fishbiomass, plt.insect_carnivore_beat_av_abundance.fishbiomass,
                             ncol=3, align='v', axis = 'l')
insect_plots_cover
ggplot2::ggsave("C:Plots//Model-fitted//insect_plots_cover.png", width=30, height=10, units="cm")


insect_plots_Area<-plot_grid(plt.insect_detritivore_richness.Area,plt.insect_herbivore_richness.Area, plt.insect_carnivore_richness.Area,
                              ncol=3, align='v', axis = 'l')
insect_plots_Area
ggplot2::ggsave("C:Plots//Model-fitted//insect_plots_Area.png", width=30, height=10, units="cm")



plant_plots_Area<-plot_grid(plt.plant_richness.Area, plt.tree_richness.Area,
                                      ncol=2, align='v', axis = 'l')
plant_plots_Area
ggplot2::ggsave("C:Plots//Model-fitted//plant_plots_Area.png", width=20, height=10, units="cm")



# Marine catch vs. d15n ---------------------------------------------------


gamma.12.marine_richness_corrected<-fitdistr(fish_stats$marine_richness_corrected+0.01, "gamma")
qqp(fish_stats$fish_richness_corrected, "gamma", shape = gamma.12.marine_richness_corrected$estimate[[1]], rate = gamma.12.marine_richness_corrected$estimate[[2]])


#suite of models
lm.d15n.marinecatch<-lm(d15n ~ marine_richness_corrected, data=fish_stats_zscores)
glm.d15n.marinecatch<-glm(d15n ~ marine_richness_corrected, data=fish_stats_zscores, family="Gamma")

AICtab( glm.d15n.marinecatch, lm.d15n.marinecatch)

plot(glm.d15n.marinecatch)
summary(glm.d15n.marinecatch)

fam.glmm.d15n.marinecatch <- family(glm.d15n.marinecatch )
fam.glmm.d15n.marinecatch
str(fam.glmm.d15n.marinecatch)
ilink.glmm.d15n.marinecatch<- fam.glmm.d15n.marinecatch$linkinv
ilink.glmm.d15n.marinecatch


mod.d15n.marinecatch<-glm.d15n.marinecatch 
ndata.d15n.marinecatch <- with(fish_stats_zscores, data_frame(marine_richness_corrected = seq(min(marine_richness_corrected), max(marine_richness_corrected),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.d15n.marinecatch <- add_column(ndata.d15n.marinecatch, fit = predict(mod.d15n.marinecatch, newdata = ndata.d15n.marinecatch, type = 'response'))

predict(mod.d15n.marinecatch, newdata = ndata.d15n.marinecatch, type = 'response')
ndata.d15n.marinecatch <- bind_cols(ndata.d15n.marinecatch, setNames(as_tibble(predict(mod.d15n.marinecatch, ndata.d15n.marinecatch, se.fit = TRUE)[1:2]),
                                                                     c('fit_link','se_link')))

## create the interval and backtransform

ndata.d15n.marinecatch <- mutate(ndata.d15n.marinecatch,
                                 fit_resp  = ilink.glmm.d15n.marinecatch(fit_link),
                                 right_upr = ilink.glmm.d15n.marinecatch(fit_link + (2 * se_link)),
                                 right_lwr = ilink.glmm.d15n.marinecatch(fit_link - (2 * se_link)))

fish_stats_zscores$marine_richness_corrected<-scale(fish_stats$marine_richness_corrected, center=TRUE, scale=TRUE)

ndata.d15n.marinecatch$marine_richness_corrected.unscaled<-ndata.d15n.marinecatch$marine_richness_corrected * attr(fish_stats_zscores$marine_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$marine_richness_corrected, 'scaled:center')


# plot 
plt.d15n.marinecatch <- ggplot(ndata.d15n.marinecatch, aes(x = marine_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = fish_stats_zscores)+
  xlab(expression("Marine catch richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.marinecatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.marinecatch
ggsave("C:Plots//Transect//GLM_Gamma_d15n_marine_catch.png")


# Marine catch and 13c ----------------------------------------------------

#### marine total catch and n15
#visualize
ggplot(fish_stats_zscores, aes(y=d13c, x=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$d13c)
qqp(fish_stats_zscores$d13c, "lnorm")


#unscale the x axis
fish_stats_zscores<-fish_stats
fish_stats_zscores$marine_richness_corrected<-scale(fish_stats$marine_richness_corrected, center=TRUE, scale=TRUE)
fish_stats_zscores$marine_richness_corrected.unscaled <-fish_stats_zscores$marine_richness_corrected * attr(fish_stats_zscores$marine_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$marine_richness_corrected, 'scaled:center')
fish_stats_zscores$marine_richness_corrected<-as.numeric(fish_stats_zscores$marine_richness_corrected)


#suite of models
lm.d13c.marinecatch<-lm(d13c ~ marine_richness_corrected, data=fish_stats_zscores)
plot(lm.d13c.marinecatch)
glm.d13c.marinecatch<-glm(d13c ~ marine_richness_corrected, data=fish_stats_zscores, family=gaussian(link="log"))

#gamma does

fam.glmm.d13c.marinecatch <- family(gam.lm.d13c.marinecatch )
fam.glmm.d13c.marinecatch
str(fam.glmm.d13c.marinecatch)
ilink.glmm.d13c.marinecatch<- fam.glmm.d13c.marinecatch$linkinv
ilink.glmm.d13c.marinecatch


mod.d13c.marinecatch<-gam.lm.d13c.marinecatch 
ndata.d13c.marinecatch <- with(fish_stats_zscores, data_frame(marine_richness_corrected = seq(min(marine_richness_corrected), max(marine_richness_corrected),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.d13c.marinecatch <- add_column(ndata.d13c.marinecatch, fit = predict(mod.d13c.marinecatch, newdata = ndata.d13c.marinecatch, type = 'response'))

predict(mod.d13c.marinecatch, newdata = ndata.d13c.marinecatch, type = 'response')
ndata.d13c.marinecatch <- bind_cols(ndata.d13c.marinecatch, setNames(as_tibble(predict(mod.d13c.marinecatch, ndata.d13c.marinecatch, se.fit = TRUE)[1:2]),
                                                                     c('fit_link','se_link')))

## create the interval and backtransform

ndata.d13c.marinecatch <- mutate(ndata.d13c.marinecatch,
                                 fit_resp  = ilink.glmm.d13c.marinecatch(fit_link),
                                 right_upr = ilink.glmm.d13c.marinecatch(fit_link + (2 * se_link)),
                                 right_lwr = ilink.glmm.d13c.marinecatch(fit_link - (2 * se_link)))

fish_stats_zscores$marine_richness_corrected<-scale(fish_stats$marine_richness_corrected, center=TRUE, scale=TRUE)

ndata.d13c.marinecatch$marine_richness_corrected.unscaled<-ndata.d13c.marinecatch$marine_richness_corrected * attr(fish_stats_zscores$marine_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$marine_richness_corrected, 'scaled:center')


# plot 
plt.d13c.marinecatch <- ggplot(ndata.d13c.marinecatch, aes(x = marine_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d13c)), size=3, data = fish_stats_zscores)+
  xlab(expression("Marine catch richness per m2")) + ylab("d13c")+  
  scale_shape_manual(values=c(19))+xlim(0,40)+
  geom_ribbon(data = ndata.d13c.marinecatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d13c.marinecatch
ggsave("C:Plots//Transect//GAM_lm_d13c_marine_catch.png")


