setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Biodiversity idea")

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


#install.packages('TMB', type = 'source')

# Reading data -------------------------------------------------------------------------
#Reading in the data and making scaled
fish_stats<-read.csv("C:Output files//fish_richness_merged_tran_isl.csv")
str(fish_stats)
fish_stats<-fish_stats[,-1]
fish_stats<-fish_stats %>% filter(Distance < 1)
fish_stats_zscores$d15n<-as.numeric(fish_stats_zscores$d15n)

fish_stats$fish_abundance_bym3_log<-log(fish_stats$fish_abundance_bym3+1)
fish_stats<- fish_stats[complete.cases(fish_stats$fish_richness_corrected), ] 
fish_stats<- fish_stats[complete.cases(fish_stats$fish_abundance_bym3), ] 
fish_stats<- fish_stats[complete.cases(fish_stats$plant.richness), ] 

fish_stats_zscores<-fish_stats
fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)
fish_stats_zscores$fish_richness_corrected.unscaled <-fish_stats_zscores$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')
fish_stats_zscores$fish_richness_corrected<-as.numeric(fish_stats_zscores$fish_richness_corrected)


fish_stats_zscores$fish_abundance_bym3_log<-scale(fish_stats$fish_abundance_bym3_log, center=TRUE, scale=TRUE)
fish_stats_zscores$fish_abundance_bym3_log.unscaled <-fish_stats_zscores$fish_abundance_bym3_log * attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:center')
fish_stats_zscores$fish_abundance_bym3_log<-as.numeric(fish_stats_zscores$fish_abundance_bym3_log)

fish_stats_zscores$fish_abundance_bym3<-scale(fish_stats$fish_abundance_bym3, center=TRUE, scale=TRUE)
fish_stats_zscores$fish_abundance_bym3.unscaled <-fish_stats_zscores$fish_abundance_bym3 * attr(fish_stats_zscores$fish_abundance_bym3, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3, 'scaled:center')
fish_stats_zscores$fish_abundance_bym3<-as.numeric(fish_stats_zscores$fish_abundance_bym3)


# Plant cover (total) vs. fish richness -----------------------------------------------------
#visualize different distributions
ggplot(fish_stats_zscores, aes(y=total_cover, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
qqp(fish_stats_zscores$total_cover)
qqp(fish_stats_zscores$total_cover, "lnorm")



#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.total_cover.fishcatch<-lme(total_cover ~ fish_richness_corrected, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
glmm.total_cover.fishcatch<-glmmTMB((total_cover+0.01) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(glmm.total_cover.fishcatch, lme.total_cover.fishcatch)

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
lme.total_cover.fish_abundance<-lme(total_cover ~ fish_abundance_bym3, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lme.total_cover.fish_abundance_log<-lme(total_cover ~ fish_abundance_bym3_log, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)

glmm.total_cover.fish_abundance<-glmmTMB((total_cover+0.01) ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
glmm.total_cover.fish_abundance_log<-glmmTMB((total_cover+0.01) ~ fish_abundance_bym3_log + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(glmm.total_cover.fish_abundance_log, glmm.total_cover.fish_abundance, lme.total_cover.fish_abundance, lme.total_cover.fish_abundance_log)

summary(lme.total_cover.fish_abundance_log)

colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(lme.total_cover.fish_abundance_log,type=c("p","smooth")),
             plot(lme.total_cover.fish_abundance_log,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(lme.total_cover.fish_abundance_log,resid(.,type="pearson")~fish_abundance_bym3_log,
                  type=c("p","smooth")),
             qqnorm(lme.total_cover.fish_abundance_log,abline=c(0,1),
                    col=ifelse(fish_stats_zscores$unq_isl=="CV04",colvec[1],colvec[2])))


## Extracting coefficients and plotting
want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.total_cover.fish_abundance<-lme.total_cover.fish_abundance_log
ndata.total_cover.fish_abundance <- with(fish_stats_zscores, tibble(fish_abundance_bym3_log = seq(min(fish_abundance_bym3_log), max(fish_abundance_bym3_log),length = 100),
                                                                       unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data
ndata.total_cover.fish_abundance <- add_column(ndata.total_cover.fish_abundance, fit = predict(mod.total_cover.fish_abundance, newdata = ndata.total_cover.fish_abundance, type = 'response', level=0))

###for lmes: from bolker: 
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
Designmat <- model.matrix(formula(mod.total_cover.fish_abundance)[-2], ndata.total_cover.fish_abundance)
predvar <- diag(Designmat %*% vcov(mod.total_cover.fish_abundance) %*% t(Designmat)) 

ndata.total_cover.fish_abundance$SE <- sqrt(predvar) 
ndata.total_cover.fish_abundance$SE2 <- sqrt(predvar+mod.total_cover.fish_abundance$sigma^2)

fish_stats_zscores$fish_abundance_bym3_log<-scale(fish_stats$fish_abundance_bym3_log, center=TRUE, scale=TRUE)

ndata.total_cover.fish_abundance$fish_abundance_bym3_log.unscaled<-ndata.total_cover.fish_abundance$fish_abundance_bym3_log * attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:center')



# plot 
plt.total_cover.fish_abundance <- ggplot(ndata.total_cover.fish_abundance, aes(x = fish_abundance_bym3_log.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(total_cover)), size=3, data = fish_stats_zscores)+
  xlab(expression("Log fish abundance per m3")) + ylab("Plant cover")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.total_cover.fish_abundance,aes(ymin = fit - 2*SE, ymax = fit+2*SE), alpha = 0.10)+
  theme(legend.position="none")
plt.total_cover.fish_abundance
ggsave("C:Plots//Model-fitted//LME_total_cover_fish_abundance.png")



# Plant evenness vs. fish richness ----------------------------------------------------------
##this one differs between 1km and 0.3km ... 


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

summary(glmm.plant_evenness.fishcatch_gam)



## Visualizing glmm residuals with Dharma package
simulationOutput <- simulateResiduals(fittedModel = glmm.plant_evenness.fishcatch_gam)
plot(simulationOutput)
testZeroInflation(simulationOutput)
plot(simulationOutput, quantreg = T)

# Plot the residuals against island level
augDat <- data.frame(fish_stats_zscores,resid=residuals(glmm.plant_evenness.fish_abundance,type="pearson"),
                     fitted=fitted(glmm.plant_evenness.fish_abundance))
ggplot(augDat,aes(x=unq_isl,y=resid))+geom_boxplot()+coord_flip()


## Extracting coefficients and plotting
fam.glmm.plant_evenness.fishcatch_gam <- family(glmm.plant_evenness.fishcatch_gam)
fam.glmm.plant_evenness.fishcatch_gam
str(fam.glmm.plant_evenness.fishcatch_gam)
ilink.glmm.plant_evenness.fishcatch_gam<- fam.glmm.plant_evenness.fishcatch_gam$linkinv
ilink.glmm.plant_evenness.fishcatch_gam

want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.plant_evenness.fishcatch<-glmm.plant_evenness.fishcatch_gam 
ndata.plant_evenness.fishcatch <- with(fish_stats_zscores, tibble(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100),
                                                                  unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data
ndata.plant_evenness.fishcatch <- add_column(ndata.plant_evenness.fishcatch, fit = predict(mod.plant_evenness.fishcatch, newdata = ndata.plant_evenness.fishcatch, type = 'response', level=0))
ndata.plant_evenness.fishcatch <- bind_cols(ndata.plant_evenness.fishcatch, setNames(as_tibble(predict(mod.plant_evenness.fishcatch, ndata.plant_evenness.fishcatch, level=0, se.fit = TRUE)[1:2]),
                                                                                     c('fit_link','se_link')))

## create the interval and backtransform

ndata.plant_evenness.fishcatch <- mutate(ndata.plant_evenness.fishcatch,
                                         fit_resp  = ilink.glmm.plant_evenness.fishcatch_gam(fit_link),
                                         right_upr = ilink.glmm.plant_evenness.fishcatch_gam(fit_link + (2 * se_link)),
                                         right_lwr = ilink.glmm.plant_evenness.fishcatch_gam(fit_link - (2 * se_link)))

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.plant_evenness.fishcatch$fish_richness_corrected.unscaled<-ndata.plant_evenness.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.plant_evenness.fishcatch <- ggplot(ndata.plant_evenness.fishcatch, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(plant_evenness)), size=3, data = fish_stats_zscores)+
  xlab(expression("Fish richness per 100 m3")) + ylab("Plant evenness")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.plant_evenness.fishcatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.plant_evenness.fishcatch
ggsave("C:Plots//Model-fitted//GLMM_Poisson_plant_evenness_fish_catch.png")



# Plant evenness vs. fish abundance ---------------------------------------

#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lme.plant_evenness.fish_abundance<-lme(plant_evenness ~ fish_abundance_bym3, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)
lme.plant_evenness.fish_abundance_log<-lme(plant_evenness ~ fish_abundance_bym3_log, random= ~1|unq_isl, data=fish_stats_zscores, na.action=na.omit)

glmm.plant_evenness.fish_abundance<-glmmTMB((plant_evenness) ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, family="binomial", na.action=na.omit)
glmm.plant_evenness.fish_abundance_log<-glmmTMB((plant_evenness) ~ fish_abundance_bym3_log + (1|unq_isl), data=fish_stats_zscores, family="binomial", na.action=na.omit)

glmm.plant_evenness.fish_abundance_gamma<-glmmTMB((plant_evenness+0.01) ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)
glmm.plant_evenness.fish_abundance_log_gamma<-glmmTMB((plant_evenness+0.01) ~ fish_abundance_bym3_log + (1|unq_isl), data=fish_stats_zscores, family="Gamma", na.action=na.omit)

AICtab(glmm.plant_evenness.fish_abundance_gamma, glmm.plant_evenness.fish_abundance_log_gamma, glmm.plant_evenness.fish_abundance_log, glmm.plant_evenness.fish_abundance, lme.plant_evenness.fish_abundance, lme.plant_evenness.fish_abundance_log)

summary(glmm.plant_evenness.fish_abundance_log_gamma)


## Visualizing glmm residuals with Dharma package
simulationOutput <- simulateResiduals(fittedModel = glmm.plant_evenness.fish_abundance_log_gamma)
plot(simulationOutput)
testZeroInflation(simulationOutput)
plot(simulationOutput, quantreg = T)

# Plot the residuals against island level
augDat <- data.frame(fish_stats_zscores,resid=residuals(glmm.plant_evenness.fish_abundance,type="pearson"),
                     fitted=fitted(glmm.plant_evenness.fish_abundance))
ggplot(augDat,aes(x=unq_isl,y=resid))+geom_boxplot()+coord_flip()



## Extracting coefficients and plotting
fam.glmm.plant_evenness.fish_abundance_log_gamma <- family(glmm.plant_evenness.fish_abundance_log_gamma)
fam.glmm.plant_evenness.fish_abundance_log_gamma
str(fam.glmm.plant_evenness.fish_abundance_log_gamma)
ilink.glmm.plant_evenness.fish_abundance_log_gamma<- fam.glmm.plant_evenness.fish_abundance_log_gamma$linkinv
ilink.glmm.plant_evenness.fish_abundance_log_gamma

want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.plant_evenness.fish_abundance<-glmm.plant_evenness.fish_abundance_log_gamma 
ndata.plant_evenness.fish_abundance <- with(fish_stats_zscores, tibble(fish_abundance_bym3_log = seq(min(fish_abundance_bym3_log), max(fish_abundance_bym3_log),length = 100),
                                                                  unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data
ndata.plant_evenness.fish_abundance <- add_column(ndata.plant_evenness.fish_abundance, fit = predict(mod.plant_evenness.fish_abundance, newdata = ndata.plant_evenness.fish_abundance, type = 'response', level=0))
ndata.plant_evenness.fish_abundance <- bind_cols(ndata.plant_evenness.fish_abundance, setNames(as_tibble(predict(mod.plant_evenness.fish_abundance, ndata.plant_evenness.fish_abundance, level=0, se.fit = TRUE)[1:2]),
                                                                                     c('fit_link','se_link')))

## create the interval and backtransform

ndata.plant_evenness.fish_abundance <- mutate(ndata.plant_evenness.fish_abundance,
                                         fit_resp  = ilink.glmm.plant_evenness.fish_abundance_log_gamma(fit_link),
                                         right_upr = ilink.glmm.plant_evenness.fish_abundance_log_gamma(fit_link + (2 * se_link)),
                                         right_lwr = ilink.glmm.plant_evenness.fish_abundance_log_gamma(fit_link - (2 * se_link)))

fish_stats_zscores$fish_abundance_bym3<-scale(fish_stats$fish_abundance_bym3_log, center=TRUE, scale=TRUE)

ndata.plant_evenness.fish_abundance$fish_abundance_bym3_log.unscaled<-ndata.plant_evenness.fish_abundance$fish_abundance_bym3_log * attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3_log, 'scaled:center')


# plot 
plt.plant_evenness.fish_abundance <- ggplot(ndata.plant_evenness.fish_abundance, aes(x = fish_abundance_bym3_log.unscaled, y = fit)) + 
  theme_classic()+ylim(0,1)+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(plant_evenness)), size=3, data = fish_stats_zscores)+
  xlab(expression("Log fish abundance per m3")) + ylab("Plant evenness")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.plant_evenness.fish_abundance,aes(ymin =right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.plant_evenness.fish_abundance
ggsave("C:Plots//Model-fitted//LME_plant_evenness_fish_abundance.png")



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

lmer.plant_richness.fish_abundance<-lmer(plant_richness ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, na.action=na.omit)
lmer.plant_richness.fish_abundance_log<-lmer(plant_richness ~ fish_abundance_bym3_log +  (1+fish_abundance_bym3|unq_isl), data=fish_stats_zscores, na.action=na.omit)

glmm.plant_richness.fish_abundance<-glmmTMB((plant_richness) ~ fish_abundance_bym3 + (1|unq_isl), data=fish_stats_zscores, family="poisson", na.action=na.omit)
glmm.plant_richness.fish_abundance_log<-glmmTMB((plant_richness) ~ fish_abundance_bym3_log + (1|unq_isl), data=fish_stats_zscores, family="poisson", na.action=na.omit)

AICtab(lmer.plant_richness.fish_abundance, lmer.plant_richness.fish_abundance_log, glmm.plant_richness.fish_abundance_log, glmm.plant_richness.fish_abundance, lme.plant_richness.fish_abundance, lme.plant_richness.fish_abundance_log)

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
fam.glmm.plant_richness.fish_abundance <- family(glmm.plant_richness.fish_abundance_log )
fam.glmm.plant_richness.fish_abundance
str(fam.glmm.plant_richness.fish_abundance)
ilink.glmm.plant_richness.fish_abundance<- fam.glmm.plant_richness.fish_abundance$linkinv
ilink.glmm.plant_richness.fish_abundance

want <- seq(1, nrow(fish_stats_zscores), length.out = 100)
mod.plant_richness.fish_abundance<-glmm.plant_richness.fish_abundance_log 
ndata.plant_richness.fish_abundance <- with(fish_stats_zscores, tibble(fish_abundance_bym3 = seq(min(fish_abundance_bym3), max(fish_abundance_bym3),length = 100),
                                                             unq_isl = unq_isl[want]))


## add the fitted values by predicting from the model for the new data
ndata.plant_richness.fish_abundance <- add_column(ndata.plant_richness.fish_abundance, fit = predict(mod.plant_richness.fish_abundance, newdata = ndata.plant_richness.fish_abundance, type = 'response'))

predict(mod.plant_richness.fish_abundance, newdata = ndata.plant_richness.fish_abundance, type = 'response')
ndata.plant_richness.fish_abundance <- bind_cols(ndata.plant_richness.fish_abundance, setNames(as_tibble(predict(mod.plant_richness.fish_abundance, ndata.plant_richness.fish_abundance, se.fit = TRUE)[1:2]),
                                                                           c('fit_link','se_link')))

## create the interval and backtransform

ndata.plant_richness.fish_abundance <- mutate(ndata.plant_richness.fish_abundance,
                                    fit_resp  = ilink.glmm.plant_richness.fish_abundance(fit_link),
                                    right_upr = ilink.glmm.plant_richness.fish_abundance(fit_link + (2 * se_link)),
                                    right_lwr = ilink.glmm.plant_richness.fish_abundance(fit_link - (2 * se_link)))

fish_stats_zscores$fish_abundance_bym3<-scale(fish_stats$fish_abundance_bym3, center=TRUE, scale=TRUE)

ndata.plant_richness.fish_abundance$fish_abundance_bym3.unscaled<-ndata.plant_richness.fish_abundance$fish_abundance_bym3 * attr(fish_stats_zscores$fish_abundance_bym3, 'scaled:scale') + attr(fish_stats_zscores$fish_abundance_bym3, 'scaled:center')


# plot 
plt.plant_richness.fish_abundance <- ggplot(ndata.plant_richness.fish_abundance, aes(x = log(fish_abundance_bym3.unscaled+1), y = fit)) + 
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


AICtab(lmer.d15n.fishcatch, lmer.1.d15n.fishcatch,  glmm.d15n.fishcatch, lme.d15n.fishcatch, glmm.d15n.fishcatch_admb)

summary(glmm.d15n.fishcatch_admb)


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
soil_plots<-plot_grid(plt.d15n.fishcatch, plt.d15n.fish_abundance, plt.fish_speciesvabund, ncol=3, align='v', axis = 'l')
soil_plots
                 
ggplot2::ggsave("C:Plots//Model-fitted//soil_plots.png", width=60, height=20, units="cm")
                          

plant_plots<-plot_grid(plt.plant_richness.fishcatch, plt.plant_evenness.fishcatch, plt.total_cover.fishcatch,
                       plt.plant_richness.fish_abundance, plt.plant_evenness.fish_abundance, plt.total_cover.fish_abundance , ncol=3, align='v', axis = 'l')
plant_plots

ggplot2::ggsave("C:Plots//Model-fitted//plant_plots.png", width=60, height=40, units="cm")


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


