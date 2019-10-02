library(metafor)
setwd("C:/Users/Norah/Dropbox/Projects/100 Islands/Modelling practice/Deb.data")


meta.herbivore.C<- read.csv("c:herbivore_discrimination_C.csv",stringsAsFactors = FALSE, na.strings = c("NA","") )
head(meta.herbivore.C)

?escalc
#### Calculate effect sizes
meta.herbivore.C2<-escalc(measure="MD",m2i=Mean_tissue, m1i=Mean_diet, sd2i=SD_tissue,sd1i=SD_diet,n2i=N_tissue,n1i=N_diet, data=meta.herbivore.C,var.names=c("MD","MD_var"), digits=4)

head(meta.herbivore.C2)



mod.herbivore.C <- rma(MD, MD_var, data=meta.herbivore.C2)
summary(mod.herbivore.C)

#mod.herbivore.C<- rma.mv(MD, MD_var, data=meta.herbivore.C2, random=list(~1|Paper_no))
#summary(mod.herbivore.C)

funnel(mod.herbivore.C)
regtest(mod.herbivore.C,model="rma",predictor="sei")
funnel(trimfill(mod.herbivore.C, side="right"))




baujat(mod.herbivore.C)


### calculate influence diagnostics
inf <- influence(mod.herbivore.C)
### plot the influence diagnostics
plot(inf, layout=c(8,1))


rs.abund.wood <- rstandard(mod.herbivore.C)
hat.abund.wood <- hatvalues(mod.herbivore.C)/mean(hatvalues(mod.herbivore.C))
plot(hat.abund.wood, rs.abund.wood$resid, ylim = c(-4.0,4))
text(hat.abund.wood, rs.abund.wood$resid, labels = meta.herbivore.C2$Paper_no, cex= 1, pos = 2)
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


cdata.MD.herbivore.C<-meta.herbivore.C2 %>% summarise(mean=mean(MD))
cdata.MD.herbivore.C_var<-meta.herbivore.C2 %>% summarise(mean=mean(MD_var))
head(cdata.MD.herbivore.C_var)
cdata.MD.herbivore.C$var<-cdata.MD.herbivore.C_var$mean
head(cdata.MD.herbivore.C)
cdata.MD.herbivore.C2<-cbind(cdata.MD.herbivore.C, mod.herbivore.C$ci.lb, mod.herbivore.C$ci.ub)

colnames(cdata.MD.herbivore.C2)<- c("mean","var", "lower", "upper")
head(cdata.MD.herbivore.C2)


plot.mod.herbivore.C <-ggplot(cdata.MD.herbivore.C,aes(x=1,y=mod.herbivore.C $b,ymax=(mod.herbivore.C $ci.ub),ymin=(mod.herbivore.C $ci.lb),size=2))
plot.mod.herbivore.C <-plot.mod.herbivore.C +geom_pointrange(size=1)
plot.mod.herbivore.C <-plot.mod.herbivore.C  #+coord_flip()
plot.mod.herbivore.C <-plot.mod.herbivore.C +geom_hline(aes(x=0, yintercept=0), lty=2,size=1) 
plot.mod.herbivore.C <-plot.mod.herbivore.C +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ theme_bw()+theme(legend.position = "none")
plot.mod.herbivore.C <-plot.mod.herbivore.C + xlab('        Food supply') +ylab ('TEF d13C Herbivores')
plot.mod.herbivore.C <-plot.mod.herbivore.C +theme(axis.text.x = element_text(size = 16, colour = 'black')) +theme(axis.text.y = element_text(size = 16, colour = 'black'))
plot.mod.herbivore.C <-plot.mod.herbivore.C +theme(axis.title.x = element_text(size = 20, colour = 'black'))+theme(axis.title.y = element_text(size = 20, colour = 'black'))
plot.mod.herbivore.C 




# N herbivore -------------------------------------------------------------


meta.herbivore.N<- read.csv("c:herbivore_discrimination_N.csv",stringsAsFactors = FALSE, na.strings = c("NA","") )
head(meta.herbivore.N)


#### Calculate effect sizes
meta.herbivore.N2<-escalc(measure="MD",m1i=Mean_tissue, m2i=Mean_diet, sd1i=SD_tissue,sd2i=SD_diet,n1i=N_tissue,n2i=N_diet, data=meta.herbivore.N,var.names=c("MD","MD_var"), digits=4)

head(meta.herbivore.N2)



mod.herbivore.N <- rma(MD, MD_var, data=meta.herbivore.N2)
summary(mod.herbivore.N)

#mod.herbivore.N<- rma.mv(MD, MD_var, data=meta.herbivore.N2, random=list(~1|Paper_no))
#summary(mod.herbivore.N)

funnel(mod.herbivore.N)
regtest(mod.herbivore.N,model="rma",predictor="sei")
funnel(trimfill(mod.herbivore.N, side="right"))




baujat(mod.herbivore.N)


### calculate influence diagnostics
inf <- influence(mod.herbivore.N)
### plot the influence diagnostics
plot(inf, layout=c(8,1))


rs.abund.wood <- rstandard(mod.herbivore.N)
hat.abund.wood <- hatvalues(mod.herbivore.N)/mean(hatvalues(mod.herbivore.N))
plot(hat.abund.wood, rs.abund.wood$resid, ylim = c(-4.0,4))
text(hat.abund.wood, rs.abund.wood$resid, labels = meta.herbivore.N2$Paper_no, cex= 1, pos = 2)
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


cdata.MD.herbivore<-meta.herbivore.N2 %>% summarise(mean=mean(MD))
cdata.MD.herbivore_var<-meta.herbivore.N2 %>% summarise(mean=mean(MD_var))
head(cdata.MD.herbivore_var)
cdata.MD.herbivore$var<-cdata.MD.herbivore_var$mean
head(cdata.MD.herbivore)
cdata.MD.herbivore2<-cbind(cdata.MD.herbivore, mod.herbivore.N$ci.lb, mod.herbivore.N$ci.ub)

colnames(cdata.MD.herbivore2)<- c("mean","var", "lower", "upper")
head(cdata.MD.herbivore2)


plot.mod.herbivore.N <-ggplot(cdata.MD.herbivore,aes(x=1,y=mod.herbivore.N $b,ymax=(mod.herbivore.N $ci.ub),ymin=(mod.herbivore.N $ci.lb),size=2))
plot.mod.herbivore.N <-plot.mod.herbivore.N +geom_pointrange(size=1)
plot.mod.herbivore.N <-plot.mod.herbivore.N  #+coord_flip()
plot.mod.herbivore.N <-plot.mod.herbivore.N +geom_hline(aes(x=0, yintercept=0), lty=2,size=1) 
plot.mod.herbivore.N <-plot.mod.herbivore.N +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ theme_bw()+theme(legend.position = "none")
plot.mod.herbivore.N <-plot.mod.herbivore.N +ylab ('TEF d15N Herbivores')
plot.mod.herbivore.N <-plot.mod.herbivore.N +theme(axis.text.x = element_text(size = 16, colour = 'black')) +theme(axis.text.y = element_text(size = 16, colour = 'black'))
plot.mod.herbivore.N <-plot.mod.herbivore.N +theme(axis.title.x = element_text(size = 20, colour = 'black'))+theme(axis.title.y = element_text(size = 20, colour = 'black'))
plot.mod.herbivore.N 




