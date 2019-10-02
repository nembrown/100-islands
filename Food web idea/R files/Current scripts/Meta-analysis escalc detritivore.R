library(metafor)
setwd("C:/Users/Norah/Dropbox/Projects/100 Islands/Modelling practice/Deb.data")


meta.detritivore.C<- read.csv("c:detritivore_discrimination_C.csv",stringsAsFactors = FALSE, na.strings = c("NA","") )
head(meta.detritivore.C)


#### Calculate effect sizes
meta.detritivore.C2<-escalc(measure="MD",m1i=Mean_tissue, m2i=Mean_diet, sd1i=SD_tissue,sd2i=SD_diet,n1i=N_tissue,n2i=N_diet, data=meta.detritivore.C,var.names=c("MD","MD_var"), digits=4)

head(meta.detritivore.C2)



mod.detritivore.C <- rma(MD, MD_var, data=meta.detritivore.C2)
summary(mod.detritivore.C)

#mod.detritivore.C<- rma.mv(MD, MD_var, data=meta.detritivore.C2, random=list(~1|Paper_no))
#summary(mod.detritivore.C)

funnel(mod.detritivore.C)
regtest(mod.detritivore.C,model="rma",predictor="sei")
funnel(trimfill(mod.detritivore.C, side="right"))




baujat(mod.detritivore.C)


### calculate influence diagnostics
inf <- influence(mod.detritivore.C)
### plot the influence diagnostics
plot(inf, layout=c(8,1))


rs.abund.wood <- rstandard(mod.detritivore.C)
hat.abund.wood <- hatvalues(mod.detritivore.C)/mean(hatvalues(mod.detritivore.C))
plot(hat.abund.wood, rs.abund.wood$resid, ylim = c(-4.0,4))
text(hat.abund.wood, rs.abund.wood$resid, labels = meta.detritivore.C2$Paper_no, cex= 1, pos = 2)
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


cdata.MD.detritivore.C<-meta.detritivore.C2 %>% summarise(mean=mean(MD))
cdata.MD.detritivore.C_var<-meta.detritivore.C2 %>% summarise(mean=mean(MD_var))
head(cdata.MD.detritivore.C_var)
cdata.MD.detritivore.C$var<-cdata.MD.detritivore.C_var$mean
head(cdata.MD.detritivore.C)
cdata.MD.detritivore.C2<-cbind(cdata.MD.detritivore.C, mod.detritivore.C$ci.lb, mod.detritivore.C$ci.ub)

colnames(cdata.MD.detritivore.C2)<- c("mean","var", "lower", "upper")
head(cdata.MD.detritivore.C2)


plot.mod.detritivore.C <-ggplot(cdata.MD.detritivore.C,aes(x=1,y=mod.detritivore.C $b,ymax=(mod.detritivore.C $ci.ub),ymin=(mod.detritivore.C $ci.lb),size=2))
plot.mod.detritivore.C <-plot.mod.detritivore.C +geom_pointrange(size=1)
plot.mod.detritivore.C <-plot.mod.detritivore.C  #+coord_flip()
plot.mod.detritivore.C <-plot.mod.detritivore.C +geom_hline(aes(x=0, yintercept=0), lty=2,size=1) 
plot.mod.detritivore.C <-plot.mod.detritivore.C +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ theme_bw()+theme(legend.position = "none")
plot.mod.detritivore.C <-plot.mod.detritivore.C + xlab('        Food supply') +ylab ('TEF d13C detritivores')
plot.mod.detritivore.C <-plot.mod.detritivore.C +theme(axis.text.x = element_text(size = 16, colour = 'black')) +theme(axis.text.y = element_text(size = 16, colour = 'black'))
plot.mod.detritivore.C <-plot.mod.detritivore.C +theme(axis.title.x = element_text(size = 20, colour = 'black'))+theme(axis.title.y = element_text(size = 20, colour = 'black'))
plot.mod.detritivore.C 




# N detritivore -------------------------------------------------------------


meta.detritivore.N<- read.csv("c:detritivore_discrimination_N.csv",stringsAsFactors = FALSE, na.strings = c("NA","") )
head(meta.detritivore.N)


#### Calculate effect sizes
meta.detritivore.N2<-escalc(measure="MD",m1i=Mean_tissue, m2i=Mean_diet, sd1i=SD_tissue,sd2i=SD_diet,n1i=N_tissue,n2i=N_diet, data=meta.detritivore.N,var.names=c("MD","MD_var"), digits=4)

head(meta.detritivore.N2)



mod.detritivore.N <- rma(MD, MD_var, data=meta.detritivore.N2)
summary(mod.detritivore.N)

#mod.detritivore.N<- rma.mv(MD, MD_var, data=meta.detritivore.N2, random=list(~1|Paper_no))
#summary(mod.detritivore.N)

funnel(mod.detritivore.N)
regtest(mod.detritivore.N,model="rma",predictor="sei")
funnel(trimfill(mod.detritivore.N, side="right"))




baujat(mod.detritivore.N)


### calculate influence diagnostics
inf <- influence(mod.detritivore.N)
### plot the influence diagnostics
plot(inf, layout=c(8,1))


rs.abund.wood <- rstandard(mod.detritivore.N)
hat.abund.wood <- hatvalues(mod.detritivore.N)/mean(hatvalues(mod.detritivore.N))
plot(hat.abund.wood, rs.abund.wood$resid, ylim = c(-4.0,4))
text(hat.abund.wood, rs.abund.wood$resid, labels = meta.detritivore.N2$Paper_no, cex= 1, pos = 2)
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


cdata.MD.detritivore<-meta.detritivore.N2 %>% summarise(mean=mean(MD))
cdata.MD.detritivore_var<-meta.detritivore.N2 %>% summarise(mean=mean(MD_var))
head(cdata.MD.detritivore_var)
cdata.MD.detritivore$var<-cdata.MD.detritivore_var$mean
head(cdata.MD.detritivore)
cdata.MD.detritivore2<-cbind(cdata.MD.detritivore, mod.detritivore.N$ci.lb, mod.detritivore.N$ci.ub)

colnames(cdata.MD.detritivore2)<- c("mean","var", "lower", "upper")
head(cdata.MD.detritivore2)


plot.mod.detritivore.N <-ggplot(cdata.MD.detritivore,aes(x=1,y=mod.detritivore.N $b,ymax=(mod.detritivore.N $ci.ub),ymin=(mod.detritivore.N $ci.lb),size=2))
plot.mod.detritivore.N <-plot.mod.detritivore.N +geom_pointrange(size=1)
plot.mod.detritivore.N <-plot.mod.detritivore.N  #+coord_flip()
plot.mod.detritivore.N <-plot.mod.detritivore.N +geom_hline(aes(x=0, yintercept=0), lty=2,size=1) 
plot.mod.detritivore.N <-plot.mod.detritivore.N +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ theme_bw()+theme(legend.position = "none")
plot.mod.detritivore.N <-plot.mod.detritivore.N +ylab ('TEF d15N detritivores')
plot.mod.detritivore.N <-plot.mod.detritivore.N +theme(axis.text.x = element_text(size = 16, colour = 'black')) +theme(axis.text.y = element_text(size = 16, colour = 'black'))
plot.mod.detritivore.N <-plot.mod.detritivore.N +theme(axis.title.x = element_text(size = 20, colour = 'black'))+theme(axis.title.y = element_text(size = 20, colour = 'black'))
plot.mod.detritivore.N 




