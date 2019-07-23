setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Biodiversity idea")
fish_stats<-read.csv("C:Output files//fish_richness_merged_tran_isl.csv")
head(fish_stats)
fish_stats<-fish_stats[,-1]


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




# Fish richness and N15 ---------------------------------------------------
fish_stats<-fish_stats %>% filter(Distance < 1)
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
#normal is the best visually 

View(fish_stats_zscores)
#suite of models, we need to have unq_isl as a random effect, therefore mixed effects models
lmer.d15n.fishcatch<-lmer(d15n ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores)
lmer.1.d15n.fishcatch<-lmer(d15n ~ fish_richness_corrected +  (1+fish_richness_corrected|unq_isl), data=fish_stats_zscores)

glmm.d15n.fishcatch<-glmmTMB((d15n+1) ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma")
glmm.1.d15n.fishcatch<-glmmTMB((d15n+1) ~ fish_richness_corrected + (1+fish_richness_corrected|unq_isl), data=fish_stats_zscores, family="Gamma")

AICtab(lmer.d15n.fishcatch)


glmm.d15n.fishcatch<-glmmTMB(d15n ~ fish_richness_corrected + (1|unq_isl), data=fish_stats_zscores, family="Gamma")

AICtab( glmm.d15n.fishcatch, lme.d15n.fishcatch)

plot(lme.d15n.fishcatch)
plot(lmer.d15n.fishcatch)
plot(lme.slope.d15n.fishcatch)



summary(lme.d15n.fishcatch)
Anova(lme.d15n.fishcatch)



fam.gam.d15n.fishcatch <- family(glm.d15n.fishcatch )
fam.gam.d15n.fishcatch
str(fam.gam.d15n.fishcatch)
ilink.gam.d15n.fishcatch<- fam.gam.d15n.fishcatch$linkinv
ilink.gam.d15n.fishcatch


mod.d15n.fishcatch<-glm.d15n.fishcatch 
ndata.d15n.fishcatch <- with(fish_stats_zscores, data_frame(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.d15n.fishcatch <- add_column(ndata.d15n.fishcatch, fit = predict(mod.d15n.fishcatch, newdata = ndata.d15n.fishcatch, type = 'response'))

predict(mod.d15n.fishcatch, newdata = ndata.d15n.fishcatch, type = 'response')
ndata.d15n.fishcatch <- bind_cols(ndata.d15n.fishcatch, setNames(as_tibble(predict(mod.d15n.fishcatch, ndata.d15n.fishcatch, se.fit = TRUE)[1:2]),
                                                                     c('fit_link','se_link')))

## create the interval and backtransform

ndata.d15n.fishcatch <- mutate(ndata.d15n.fishcatch,
                                 fit_resp  = ilink.gam.d15n.fishcatch(fit_link),
                                 right_upr = ilink.gam.d15n.fishcatch(fit_link + (2 * se_link)),
                                 right_lwr = ilink.gam.d15n.fishcatch(fit_link - (2 * se_link)))

fish_stats_zscores$fish_richness_corrected<-scale(fish_stats$fish_richness_corrected, center=TRUE, scale=TRUE)

ndata.d15n.fishcatch$fish_richness_corrected.unscaled<-ndata.d15n.fishcatch$fish_richness_corrected * attr(fish_stats_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_stats_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.d15n.fishcatch <- ggplot(ndata.d15n.fishcatch, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = fish_stats_zscores)+
  xlab(expression("fish catch richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.fishcatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.fishcatch
ggsave("C:Plots//Transect//GLM_Gamma_d15n_fish_catch.png")


# Marine catch vs. d15n ---------------------------------------------------


gamma.12.marine_richness_corrected<-fitdistr(fish_stats$marine_richness_corrected+0.01, "gamma")
qqp(fish_stats$fish_richness_corrected, "gamma", shape = gamma.12.marine_richness_corrected$estimate[[1]], rate = gamma.12.marine_richness_corrected$estimate[[2]])


#suite of models
lm.d15n.marinecatch<-lm(d15n ~ marine_richness_corrected, data=fish_stats_zscores)
glm.d15n.marinecatch<-glm(d15n ~ marine_richness_corrected, data=fish_stats_zscores, family="Gamma")

AICtab( glm.d15n.marinecatch, lm.d15n.marinecatch)

plot(glm.d15n.marinecatch)
summary(glm.d15n.marinecatch)

fam.gam.d15n.marinecatch <- family(glm.d15n.marinecatch )
fam.gam.d15n.marinecatch
str(fam.gam.d15n.marinecatch)
ilink.gam.d15n.marinecatch<- fam.gam.d15n.marinecatch$linkinv
ilink.gam.d15n.marinecatch


mod.d15n.marinecatch<-glm.d15n.marinecatch 
ndata.d15n.marinecatch <- with(fish_stats_zscores, data_frame(marine_richness_corrected = seq(min(marine_richness_corrected), max(marine_richness_corrected),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.d15n.marinecatch <- add_column(ndata.d15n.marinecatch, fit = predict(mod.d15n.marinecatch, newdata = ndata.d15n.marinecatch, type = 'response'))

predict(mod.d15n.marinecatch, newdata = ndata.d15n.marinecatch, type = 'response')
ndata.d15n.marinecatch <- bind_cols(ndata.d15n.marinecatch, setNames(as_tibble(predict(mod.d15n.marinecatch, ndata.d15n.marinecatch, se.fit = TRUE)[1:2]),
                                                                     c('fit_link','se_link')))

## create the interval and backtransform

ndata.d15n.marinecatch <- mutate(ndata.d15n.marinecatch,
                                 fit_resp  = ilink.gam.d15n.marinecatch(fit_link),
                                 right_upr = ilink.gam.d15n.marinecatch(fit_link + (2 * se_link)),
                                 right_lwr = ilink.gam.d15n.marinecatch(fit_link - (2 * se_link)))

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

fam.gam.d13c.marinecatch <- family(gam.lm.d13c.marinecatch )
fam.gam.d13c.marinecatch
str(fam.gam.d13c.marinecatch)
ilink.gam.d13c.marinecatch<- fam.gam.d13c.marinecatch$linkinv
ilink.gam.d13c.marinecatch


mod.d13c.marinecatch<-gam.lm.d13c.marinecatch 
ndata.d13c.marinecatch <- with(fish_stats_zscores, data_frame(marine_richness_corrected = seq(min(marine_richness_corrected), max(marine_richness_corrected),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.d13c.marinecatch <- add_column(ndata.d13c.marinecatch, fit = predict(mod.d13c.marinecatch, newdata = ndata.d13c.marinecatch, type = 'response'))

predict(mod.d13c.marinecatch, newdata = ndata.d13c.marinecatch, type = 'response')
ndata.d13c.marinecatch <- bind_cols(ndata.d13c.marinecatch, setNames(as_tibble(predict(mod.d13c.marinecatch, ndata.d13c.marinecatch, se.fit = TRUE)[1:2]),
                                                                     c('fit_link','se_link')))

## create the interval and backtransform

ndata.d13c.marinecatch <- mutate(ndata.d13c.marinecatch,
                                 fit_resp  = ilink.gam.d13c.marinecatch(fit_link),
                                 right_upr = ilink.gam.d13c.marinecatch(fit_link + (2 * se_link)),
                                 right_lwr = ilink.gam.d13c.marinecatch(fit_link - (2 * se_link)))

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


