setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Biodiversity idea")


library(tidyr)
library(bbmle) 
library(glmmTMB)
library(doBy)
library(plyr)
require(dplyr)
library(ggplot2) 
library(doBy)
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
library(ggplot2)
library(grid)
library(DHARMa)
library(gap)
library(qrnn)
library(mgcv)
library(colorspace)
library(gratia)
library(cowplot)
library(gtable)
library(gridExtra)





# Plant richness ----------------------------------------------------------
fish_stats<-read.csv("C:Output files//fish_richness_merged_tran_isl.csv")
head(fish_stats)
fish_stats<-fish_stats[,-1]
fish_stats<-fish_stats %>% filter(Distance < .25)
fish_stats<- fish_stats[complete.cases(fish_stats$fish_richness_corrected), ] 
fish_stats<- fish_stats[complete.cases(fish_stats$plant_richness), ] 
fish_stats_zscores<-fish_stats
fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)
fish_stats_zscores$fish_richness_corrected.unscaled <-fish_stats_zscores$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')
fish_stats_zscores$fish_richness_corrected<-as.numeric(fish_stats_zscores$fish_richness_corrected)



#visualize different distributions
ggplot(fish_stats_zscores, aes(y=plant_richness, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$plant_richness)
qqp(fish_stats_zscores$plant_richness, "lnorm")

poisson.fish<-fitdistr(fish_stats_zscores$plant_richness, "Poisson")
qqp(fish_stats_zscores$plant_richness, "pois", lambda=poisson.fish$estimate[[1]])



#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.plant_richness.fishcatch<-lme(plant_richness ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lmer.plant_richness.fishcatch<-lmer(plant_richness ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, na.action=na.omit)
lmer.1.plant_richness.fishcatch<-lmer(plant_richness ~ fish_richness_corrected +  (1+fish_richness_corrected|unq_isl), data=fish_stats_zscores, na.action=na.omit)

glmm.plant_richness.fishcatch<-glmmTMB((plant_richness) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="poisson", na.action=na.omit)
#glmm.1.plant_richness.fishcatch<-glmmTMB((plant_richness+1) ~ fish_richness_corrected + (1+fish_richness_corrected|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(lmer.plant_richness.fishcatch, lmer.1.plant_richness.fishcatch,  glmm.plant_richness.fishcatch, lme.plant_richness.fishcatch)

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
str(fam.glmm.plant_richness.fishcatch)
ilink.glmm.plant_richness.fishcatch<- fam.glmm.plant_richness.fishcatch$linkinv
ilink.glmm.plant_richness.fishcatch

want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.plant_richness.fishcatch<-glmm.plant_richness.fishcatch 
ndata.plant_richness.fishcatch <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
                                                        unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data
ndata.plant_richness.fishcatch <- add_column(ndata.plant_richness.fishcatch, fit = predict(mod.plant_richness.fishcatch, newdata = ndata.plant_richness.fishcatch, type = 'response'))

predict(mod.plant_richness.fishcatch, newdata = ndata.plant_richness.fishcatch, type = 'response')
ndata.plant_richness.fishcatch <- bind_cols(ndata.plant_richness.fishcatch, setNames(as_tibble(predict(mod.plant_richness.fishcatch, ndata.plant_richness.fishcatch, se.fit = TRUE)[1:2]),
                                                                 c('fit_link','se_link')))

## create the interval and backtransform

ndata.plant_richness.fishcatch <- mutate(ndata.plant_richness.fishcatch,
                               fit_resp  = ilink.glmm.plant_richness.fishcatch(fit_link),
                               right_upr = ilink.glmm.plant_richness.fishcatch(fit_link + (2 * se_link)),
                               right_lwr = ilink.glmm.plant_richness.fishcatch(fit_link - (2 * se_link)))

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.plant_richness.fishcatch$fish_richness_corrected.unscaled<-ndata.plant_richness.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')

View(ndata.plant_richness.fishcatch)
# plot 
plt.plant_richness.fishcatch <- ggplot(ndata.plant_richness.fishcatch, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(plant_richness)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish richness per 100 m3")) + ylab("Soil plant_richness at shoreline")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.plant_richness.fishcatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.plant_richness.fishcatch
ggsave("C:Plots//Model-fitted//GLMM_Poisson_plant_richness_fish_catch.png")


# Beachseine only data ----------------------------------------------------


beachseine_stats<-read.csv("C:Output files//fish_bycatch_richness_merged_tran_year.csv")

beachseine_stats<-beachseine_stats[,-1]
beachseine_stats<- beachseine_stats[complete.cases(beachseine_stats$fish_abundance_bym3), ] 
beachseine_stats_zscores<-beachseine_stats
beachseine_stats_zscores$fish_abundance_bym3<-scale(beachseine_stats$fish_abundance_bym3, center=TRUE, scale=TRUE)
beachseine_stats_zscores$fish_abundance_bym3.unscaled <-beachseine_stats_zscores$fish_abundance_bym3 * attr(beachseine_stats_zscores$fish_abundance_bym3, 'scaled:scale') + attr(beachseine_stats_zscores$fish_abundance_bym3, 'scaled:center')
beachseine_stats_zscores$fish_abundance_bym3<-as.numeric(beachseine_stats_zscores$fish_abundance_bym3)



#visualize different distributions
ggplot(beachseine_stats_zscores, aes(x=log(fish_abundance_bym3+1), y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(beachseine_stats_zscores$fish_richness_corrected)
qqp(beachseine_stats_zscores$fish_richness_corrected, "lnorm")
gamma.12.fish_richness_corrected<-fitdistr(beachseine_stats_zscores$fish_richness_corrected+1, "gamma")
qqp(beachseine_stats$fish_richness_corrected, "gamma", shape = gamma.12.fish_richness_corrected$estimate[[1]], rate = gamma.12.fish_richness_corrected$estimate[[2]])
#normal and Gamma both good, have a few obs. outside 


#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lm.fish_speciesvabund<-lm(fish_richness_corrected ~ fish_abundance_bym3, data=beachseine_stats_zscores, na.action=na.omit)
lm.fish_speciesvabund_log<-lm(fish_richness_corrected ~ log(fish_abundance_bym3+1), data=beachseine_stats_zscores, na.action=na.omit)

glm.fish_speciesvabund<-glm(fish_richness_corrected ~ fish_abundance_bym3, data=beachseine_stats_zscores, family="poisson")
glm.fish_speciesvabund_log<-glm((fish_richness_corrected) ~ log(fish_abundance_bym3+1), data=beachseine_stats_zscores, family="poisson", na.action=na.omit)

AICtab(glm.fish_speciesvabund, glm.fish_speciesvabund_log, lm.fish_speciesvabund_log, lm.fish_speciesvabund)

summary(lm.fish_speciesvabund_log)

#dwplot(list(glmmTMB=glmm.fish_speciesvabund,lmer=lmer.fish_speciesvabund),by_2sd=TRUE)

plot(lm.fish_speciesvabund_log)


## Extracting coefficients and plotting
fam.glmm.fish_speciesvabund <- family(lm.fish_speciesvabund_log )
fam.glmm.fish_speciesvabund
str(fam.glmm.fish_speciesvabund)
ilink.glmm.fish_speciesvabund<- fam.glmm.fish_speciesvabund$linkinv
ilink.glmm.fish_speciesvabund

want <- seq(1, nrow(beachseine_stats_zscores), length.out = 100)
mod.fish_speciesvabund<-lm.fish_speciesvabund_log
ndata.fish_speciesvabund<- with(beachseine_stats_zscores, tibble(fish_abundance_bym3 = seq(min(fish_abundance_bym3), max(fish_abundance_bym3),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.fish_speciesvabund<- add_column(ndata.fish_speciesvabund, fit = predict(mod.fish_speciesvabund, newdata = ndata.fish_speciesvabund, type = 'response'))

predict(mod.fish_speciesvabund, newdata = ndata.fish_speciesvabund, type = 'response')
ndata.fish_speciesvabund<- bind_cols(ndata.fish_speciesvabund, setNames(as_tibble(predict(mod.fish_speciesvabund, ndata.fish_speciesvabund, se.fit = TRUE)[1:2]),
                                                                 c('fit_link','se_link')))

## create the interval and backtransform

ndata.fish_speciesvabund<- mutate(ndata.fish_speciesvabund,
                               fit_resp  = ilink.glmm.fish_speciesvabund(fit_link),
                               right_upr = ilink.glmm.fish_speciesvabund(fit_link + (2 * se_link)),
                               right_lwr = ilink.glmm.fish_speciesvabund(fit_link - (2 * se_link)))

beachseine_stats_zscores$fish_abundance_bym3<-scale(beachseine_stats$fish_abundance_bym3, center=TRUE, scale=TRUE)

ndata.fish_speciesvabund$fish_abundance_bym3.unscaled<-ndata.fish_speciesvabund$fish_abundance_bym3 * attr(beachseine_stats_zscores$fish_abundance_bym3, 'scaled:scale') + attr(beachseine_stats_zscores$fish_abundance_bym3, 'scaled:center')


# plot 
plt.fish_speciesvabund <- ggplot(ndata.fish_speciesvabund, aes(x = log(fish_abundance_bym3.unscaled+1), y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(fish_richness_corrected)), size=3, data = beachseine_stats_zscores)+
  xlab(expression("Log Fish abundance per m3")) + ylab("Fish richness per 100m3")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.fish_speciesvabund,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.fish_speciesvabund
ggsave("C:Plots//Model-fitted//LM_fish_speciesvabund.png")



# Fish richness and N15 ---------------------------------------------------
fish_stats<-read.csv("C:Output files//fish_richness_merged_tran_isl.csv")
head(fish_stats)
fish_stats<-fish_stats[,-1]
fish_stats<-fish_stats %>% filter(Distance < .25)
fish_stats<- fish_stats[complete.cases(fish_stats$fish_richness_corrected), ] 
fish_stats_zscores<-fish_stats
fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)
fish_stats_zscores$fish_richness_corrected.unscaled <-fish_stats_zscores$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')
fish_stats_zscores$fish_richness_corrected<-as.numeric(fish_stats_zscores$fish_richness_corrected)



#visualize different distributions
ggplot(fish_stats_zscores, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$d15n)
qqp(fish_stats_zscores$d15n, "lnorm")
gamma.12.fish_richness_corrected<-fitdistr(fish_stats_zscores$d15n+1, "gamma")
qqp(fish_stats$fish_richness_corrected, "gamma", shape = gamma.12.fish_richness_corrected$estimate[[1]], rate = gamma.12.fish_richness_corrected$estimate[[2]])
#normal and Gamma both good, have a few obs. outside 


#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.d15n.fishcatch<-lme(d15n ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
#lme.1.d15n.fishcatch<-lme(d15n ~ fish_richness_corrected, random= ~1+fish_richness_corrected|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lmer.d15n.fishcatch<-lmer(d15n ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, na.action=na.omit)
lmer.1.d15n.fishcatch<-lmer(d15n ~ fish_richness_corrected +  (1+fish_richness_corrected|unq_isl), data=fish_stats_zscores, na.action=na.omit)
glmm.d15n.fishcatch<-glmmTMB((d15n) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
#glmm.1.d15n.fishcatch<-glmmTMB((d15n+1) ~ fish_richness_corrected + (1+fish_richness_corrected|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(lmer.d15n.fishcatch, lmer.1.d15n.fishcatch,  glmm.d15n.fishcatch, lme.d15n.fishcatch)

summary(glmm.d15n.fishcatch)

#dwplot(list(glmmTMB=glmm.d15n.fishcatch,lmer=lmer.d15n.fishcatch),by_2sd=TRUE)

colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.d15n.fishcatch,type=c("p","smooth")),
             plot(lme.d15n.fishcatch,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.d15n.fishcatch,resid(.,type="pearson")~fish_richness_corrected,
                  type=c("p","smooth")),
             qqnorm(lme.d15n.fishcatch,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))


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
fam.glmm.d15n.fishcatch <- family(glmm.d15n.fishcatch )
fam.glmm.d15n.fishcatch
str(fam.glmm.d15n.fishcatch)
ilink.glmm.d15n.fishcatch<- fam.glmm.d15n.fishcatch$linkinv
ilink.glmm.d15n.fishcatch

want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.d15n.fishcatch<-glmm.d15n.fishcatch 
ndata.d15n.fishcatch <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
                                                        unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data
ndata.d15n.fishcatch <- add_column(ndata.d15n.fishcatch, fit = predict(mod.d15n.fishcatch, newdata = ndata.d15n.fishcatch, type = 'response'))

predict(mod.d15n.fishcatch, newdata = ndata.d15n.fishcatch, type = 'response')
ndata.d15n.fishcatch <- bind_cols(ndata.d15n.fishcatch, setNames(as_tibble(predict(mod.d15n.fishcatch, ndata.d15n.fishcatch, se.fit = TRUE)[1:2]),
                                                                     c('fit_link','se_link')))

## create the interval and backtransform

ndata.d15n.fishcatch <- mutate(ndata.d15n.fishcatch,
                                 fit_resp  = ilink.glmm.d15n.fishcatch(fit_link),
                                 right_upr = ilink.glmm.d15n.fishcatch(fit_link + (2 * se_link)),
                                 right_lwr = ilink.glmm.d15n.fishcatch(fit_link - (2 * se_link)))

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.d15n.fishcatch$fish_richness_corrected.unscaled<-ndata.d15n.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.d15n.fishcatch <- ggplot(ndata.d15n.fishcatch, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish richness per 100 m3")) + ylab("Soil d15n at shoreline")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.fishcatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.fishcatch
ggsave("C:Plots//Model-fitted//GLMM_Gamma_d15n_fish_catch.png")


# Fish abundance ----------------------------------------------------------
fish_stats_zscores$fish_abundance_bym3<-scale(fish_stats$fish_abundance_bym3, center=TRUE, scale=TRUE)
fish_stats_zscores$fish_abundance_bym3.unscaled <-fish_stats_zscores$fish_abundance_bym3 * attr(fish_stats_zscores$fish_abundance_bym3, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3, 'scaled:center')
fish_stats_zscores$fish_abundance_bym3<-as.numeric(fish_stats_zscores$fish_abundance_bym3)



#visualize different distributions
ggplot(fish_stats_zscores, aes(y=d15n, x=fish_abundance_bym3))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$d15n)
qqp(fish_stats_zscores$d15n, "lnorm")
gamma.12.fish_abundance_bym3<-fitdistr(fish_stats_zscores$d15n+1, "gamma")
qqp(fish_stats$fish_abundance_bym3, "gamma", shape = gamma.12.fish_abundance_bym3$estimate[[1]], rate = gamma.12.fish_abundance_bym3$estimate[[2]])
#normal and Gamma both good, have a few obs. outside 


#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.d15n.fish_abundance<-lme(d15n ~ fish_abundance_bym3, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lme.d15n.fish_abundance_log<-lme(d15n ~ log(fish_abundance_bym3+1), random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)

lmer.d15n.fish_abundance<-lmer(d15n ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, na.action=na.omit)
lmer.d15n.fish_abundance_log<-lmer(d15n ~ log(fish_abundance_bym3+1) +  (1+fish_abundance_bym3|unq_isl), data=fish_stats_zscores, na.action=na.omit)

glmm.d15n.fish_abundance<-glmmTMB((d15n) ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
glmm.d15n.fish_abundance_log<-glmmTMB((d15n) ~ log(fish_abundance_bym3+1) + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(lmer.d15n.fish_abundance, lmer.d15n.fish_abundance_log, glmm.d15n.fish_abundance_log, glmm.d15n.fish_abundance, lme.d15n.fish_abundance, lme.d15n.fish_abundance_log)

summary(glmm.d15n.fish_abundance_log)

#dwplot(list(glmmTMB=glmm.d15n.fish_abundance,lmer=lmer.d15n.fish_abundance),by_2sd=TRUE)

colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.d15n.fish_abundance,type=c("p","smooth")),
             plot(lme.d15n.fish_abundance,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.d15n.fish_abundance,resid(.,type="pearson")~fish_abundance_bym3,
                  type=c("p","smooth")),
             qqnorm(lme.d15n.fish_abundance,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))


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
fam.glmm.d15n.fish_abundance <- family(glmm.d15n.fish_abundance_log )
fam.glmm.d15n.fish_abundance
str(fam.glmm.d15n.fish_abundance)
ilink.glmm.d15n.fish_abundance<- fam.glmm.d15n.fish_abundance$linkinv
ilink.glmm.d15n.fish_abundance

want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.d15n.fish_abundance<-glmm.d15n.fish_abundance_log 
ndata.d15n.fish_abundance <- with(fish_stats_zscores, tibble(fish_abundance_bym3 = seq(min(fish_abundance_bym3), max(fish_abundance_bym3),length = 100),
                                                        unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data
ndata.d15n.fish_abundance <- add_column(ndata.d15n.fish_abundance, fit = predict(mod.d15n.fish_abundance, newdata = ndata.d15n.fish_abundance, type = 'response'))

predict(mod.d15n.fish_abundance, newdata = ndata.d15n.fish_abundance, type = 'response')
ndata.d15n.fish_abundance <- bind_cols(ndata.d15n.fish_abundance, setNames(as_tibble(predict(mod.d15n.fish_abundance, ndata.d15n.fish_abundance, se.fit = TRUE)[1:2]),
                                                                 c('fit_link','se_link')))

## create the interval and backtransform

ndata.d15n.fish_abundance <- mutate(ndata.d15n.fish_abundance,
                               fit_resp  = ilink.glmm.d15n.fish_abundance(fit_link),
                               right_upr = ilink.glmm.d15n.fish_abundance(fit_link + (2 * se_link)),
                               right_lwr = ilink.glmm.d15n.fish_abundance(fit_link - (2 * se_link)))

fish_stats_zscores$fish_abundance_bym3<-scale(fish_stats$fish_abundance_bym3, center=TRUE, scale=TRUE)

ndata.d15n.fish_abundance$fish_abundance_bym3.unscaled<-ndata.d15n.fish_abundance$fish_abundance_bym3 * attr(fish_stats_zscores$fish_abundance_bym3, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3, 'scaled:center')


# plot 
plt.d15n.fish_abundance <- ggplot(ndata.d15n.fish_abundance, aes(x = log(fish_abundance_bym3.unscaled+1), y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = fish_stats_zscores)+
  xlab(expression("Log fish abundance per m3")) + ylab("Soil d15n at shoreline")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.fish_abundance,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.fish_abundance
ggsave("C:Plots//Model-fitted//GLMM_Gamma_d15n_fish_abundance.png")



# Combining Soil Plots ----------------------------------------------------
library(cowplot)
soil_plots<-plot_grid(plt.d15n.fishcatch, plt.d15n.fish_abundance, plt.fish_speciesvabund, ncol=3, align='v', axis = 'l')
soil_plots
                 
ggplot2::ggsave("C:Plots//Model-fitted//soil_plots.png", width=60, height=20, units="cm")
                          
                          
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


