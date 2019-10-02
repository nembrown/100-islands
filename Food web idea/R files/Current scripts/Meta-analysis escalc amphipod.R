library(metafor)
setwd("C:/Users/Norah/Dropbox/Projects/100 Islands/Modelling practice/Deb.Data")


meta.amphipod.C<- read.csv("c:amphipod_discrimination_C.csv",stringsAsFactors = FALSE, na.strings = c("NA","") )
head(meta.amphipod.C)



#### Calculate effect sizes
meta.amphipod.C2<-escalc(measure="MD",m1i=Mean_tissue, m2i=Mean_diet, sd1i=SD_tissue,sd2i=SD_diet,n1i=N_tissue,n2i=N_diet, data=meta.amphipod.C,var.names=c("MD","MD_var"), digits=4)

head(meta.amphipod.C2)



mod.amphipod.C <- rma(MD, MD_var, data=meta.amphipod.C2)
summary(mod.amphipod.C)

#mod.amphipod.C<- rma.mv(MD, MD_var, data=meta.amphipod.C2, random=list(~1|Paper_no))
#summary(mod.amphipod.C)

funnel(mod.amphipod.C)
regtest(mod.amphipod.C,model="rma",predictor="sei")
funnel(trimfill(mod.amphipod.C, side="right"))




baujat(mod.amphipod.C)


### calculate influence diagnostics
inf <- influence(mod.amphipod.C)
### plot the influence diagnostics
plot(inf, layout=c(8,1))


rs.abund.wood <- rstandard(mod.amphipod.C)
hat.abund.wood <- hatvalues(mod.amphipod.C)/mean(hatvalues(mod.amphipod.C))
plot(hat.abund.wood, rs.abund.wood$resid, ylim = c(-4.0,4))
text(hat.abund.wood, rs.abund.wood$resid, labels = meta.amphipod.C2$Paper_no, cex= 1, pos = 2)
abline(h = -3)
abline(h = 3)
abline( v = 2)
#### should compare without Crook, Comeau, Hettinger outliers model fit







############# GGPLOT using summary data for each Food.supply level
library(ggplot2)

######## PLOTTING WITH A MODEL


summary(mod.1.growth)
head(mod.1.growth)
library(plyr)
library(dplyr)


cdata.MD.amphipod.C<-meta.amphipod.C2 %>% summarise(mean=mean(MD))
cdata.MD.amphipod.C_var<-meta.amphipod.C2 %>% summarise(mean=mean(MD_var))
head(cdata.MD.amphipod.C_var)
cdata.MD.amphipod.C$var<-cdata.MD.amphipod.C_var$mean
head(cdata.MD.amphipod.C)
cdata.MD.amphipod.C2<-cbind(cdata.MD.amphipod.C, mod.amphipod.C$ci.lb, mod.amphipod.C$ci.ub)

colnames(cdata.MD.amphipod.C2)<- c("mean","var", "lower", "upper")
head(cdata.MD.amphipod.C2)


plot.mod.amphipod.C <-ggplot(cdata.MD.amphipod.C,aes(x=1,y=mod.amphipod.C $b,ymax=(mod.amphipod.C $ci.ub),ymin=(mod.amphipod.C $ci.lb),size=2))
plot.mod.amphipod.C <-plot.mod.amphipod.C +geom_pointrange(size=1)
plot.mod.amphipod.C <-plot.mod.amphipod.C  #+coord_flip()
plot.mod.amphipod.C <-plot.mod.amphipod.C +geom_hline(aes(x=0, yintercept=0), lty=2,size=1) 
plot.mod.amphipod.C <-plot.mod.amphipod.C +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ theme_bw()+theme(legend.position = "none")
plot.mod.amphipod.C <-plot.mod.amphipod.C + xlab('        Food supply') +ylab ('TEF d13C amphipods')
plot.mod.amphipod.C <-plot.mod.amphipod.C +theme(axis.text.x = element_text(size = 16, colour = 'black')) +theme(axis.text.y = element_text(size = 16, colour = 'black'))
plot.mod.amphipod.C <-plot.mod.amphipod.C +theme(axis.title.x = element_text(size = 20, colour = 'black'))+theme(axis.title.y = element_text(size = 20, colour = 'black'))
plot.mod.amphipod.C 




# N amphipod -------------------------------------------------------------


meta.amphipod.N<- read.csv("c:amphipod_discrimination_N.csv",stringsAsFactors = FALSE, na.strings = c("NA","") )
head(meta.amphipod.N)


#### Calculate effect sizes
meta.amphipod.N2<-escalc(measure="MD",m1i=Mean_tissue, m2i=Mean_diet, sd1i=SD_tissue,sd2i=SD_diet,n1i=N_tissue,n2i=N_diet, data=meta.amphipod.N,var.names=c("MD","MD_var"), digits=4)

head(meta.amphipod.N2)



mod.amphipod.N <- rma(MD, MD_var, data=meta.amphipod.N2)
summary(mod.amphipod.N)

#mod.amphipod.N<- rma.mv(MD, MD_var, data=meta.amphipod.N2, random=list(~1|Paper_no))
#summary(mod.amphipod.N)

funnel(mod.amphipod.N)
regtest(mod.amphipod.N,model="rma",predictor="sei")
funnel(trimfill(mod.amphipod.N, side="right"))




baujat(mod.amphipod.N)


### calculate influence diagnostics
inf <- influence(mod.amphipod.N)
### plot the influence diagnostics
plot(inf, layout=c(8,1))


rs.abund.wood <- rstandard(mod.amphipod.N)
hat.abund.wood <- hatvalues(mod.amphipod.N)/mean(hatvalues(mod.amphipod.N))
plot(hat.abund.wood, rs.abund.wood$resid, ylim = c(-4.0,4))
text(hat.abund.wood, rs.abund.wood$resid, labels = meta.amphipod.N2$Paper_no, cex= 1, pos = 2)
abline(h = -3)
abline(h = 3)
abline( v = 2)
#### should compare without Crook, Comeau, Hettinger outliers model fit







############# GGPLOT using summary data for each Food.supply level
library(ggplot2)

######## PLOTTING WITH A MODEL


summary(mod.1.growth)
head(mod.1.growth)
library(plyr)
library(dplyr)


cdata.MD.amphipod<-meta.amphipod.N2 %>% summarise(mean=mean(MD))
cdata.MD.amphipod_var<-meta.amphipod.N2 %>% summarise(mean=mean(MD_var))
head(cdata.MD.amphipod_var)
cdata.MD.amphipod$var<-cdata.MD.amphipod_var$mean
head(cdata.MD.amphipod)
cdata.MD.amphipod2<-cbind(cdata.MD.amphipod, mod.amphipod.N$ci.lb, mod.amphipod.N$ci.ub)

colnames(cdata.MD.amphipod2)<- c("mean","var", "lower", "upper")
head(cdata.MD.amphipod2)


plot.mod.amphipod.N <-ggplot(cdata.MD.amphipod,aes(x=1,y=mod.amphipod.N $b,ymax=(mod.amphipod.N $ci.ub),ymin=(mod.amphipod.N $ci.lb),size=2))
plot.mod.amphipod.N <-plot.mod.amphipod.N +geom_pointrange(size=1)
plot.mod.amphipod.N <-plot.mod.amphipod.N  #+coord_flip()
plot.mod.amphipod.N <-plot.mod.amphipod.N +geom_hline(aes(x=0, yintercept=0), lty=2,size=1) 
plot.mod.amphipod.N <-plot.mod.amphipod.N +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ theme_bw()+theme(legend.position = "none")
plot.mod.amphipod.N <-plot.mod.amphipod.N +ylab ('TEF d15N amphipods')
plot.mod.amphipod.N <-plot.mod.amphipod.N +theme(axis.text.x = element_text(size = 16, colour = 'black')) +theme(axis.text.y = element_text(size = 16, colour = 'black'))
plot.mod.amphipod.N <-plot.mod.amphipod.N +theme(axis.title.x = element_text(size = 20, colour = 'black'))+theme(axis.title.y = element_text(size = 20, colour = 'black'))
plot.mod.amphipod.N 




