setwd("C:/Users/Norah/Dropbox/Projects/100-islands/Food web idea/Data by person/Norah.data")


library(metafor)


meta.bird.C<- read.csv("c:bird_discrimination_C_feather-derived.csv",stringsAsFactors = FALSE, na.strings = c("NA","") )
head(meta.bird.C)


#### Calculate effect sizes
meta.bird.C2<-escalc(measure="MD",m1i=Mean_tissue, m2i=Mean_diet, sd1i=SD_tissue,sd2i=SD_diet,n1i=N_tissue,n2i=N_diet, data=meta.bird.C,var.names=c("MD","MD_var"), digits=4)

head(meta.bird.C2)



mod.bird.C_overall <- rma(MD, MD_var, data=meta.bird.C2)
summary(mod.bird.C_overall)

mod.bird.C<- rma.mv(MD, MD_var, mods = ~factor(Species_applied) - 1, data=meta.bird.C2, random=list(~1|Paper_no))
summary(mod.bird.C)

funnel(mod.bird.C)
regtest(mod.bird.C,model="rma",predictor="sei")
funnel(trimfill(mod.bird.C, side="right"))




baujat(mod.bird.C)


### calculate influence diagnostics
inf <- influence(mod.bird.C)
### plot the influence diagnostics
plot(inf, layout=c(8,1))


rs.abund.wood <- rstandard(mod.bird.C)
hat.abund.wood <- hatvalues(mod.bird.C)/mean(hatvalues(mod.bird.C))
plot(hat.abund.wood, rs.abund.wood$resid, ylim = c(-4.0,4))
text(hat.abund.wood, rs.abund.wood$resid, labels = meta.bird.C2$Paper_no, cex= 1, pos = 2)
abline(h = -3)
abline(h = 3)
abline( v = 2)
#### should compare without Crook, Comeau, Hettinger outliers model fit







############# GGPLOT using summary data for each Food.supply level
library(ggplot2)

######## PLOTTING WITH A MODEL


library(plyr)
library(dplyr)


cdata.MD.bird.C<-meta.bird.C2 %>% group_by(Species_applied) %>% summarise(mean=mean(MD))
cdata.MD.bird.C_var<-meta.bird.C2%>% group_by(Species_applied) %>% summarise(mean=mean(MD_var))
head(cdata.MD.bird.C_var)
cdata.MD.bird.C$var<-cdata.MD.bird.C_var$mean
head(cdata.MD.bird.C)



cdata.MD.bird.C2<-cbind(cdata.MD.bird.C, mod.bird.C$ci.lb, mod.bird.C$ci.ub)
colnames(cdata.MD.bird.C2)<- c("Species", "mean","var", "lower", "upper")
head(cdata.MD.bird.C2)


plot.mod.bird.C <-ggplot(cdata.MD.bird.C,aes(x=Species_applied,y=mod.bird.C$b,ymax=(mod.bird.C$ci.ub),ymin=(mod.bird.C$ci.lb),size=2))
plot.mod.bird.C <-plot.mod.bird.C +geom_pointrange(size=1)
plot.mod.bird.C <-plot.mod.bird.C  #+coord_flip()
plot.mod.bird.C <-plot.mod.bird.C +geom_hline(aes(yintercept=0), lty=2,size=1) 
plot.mod.bird.C <-plot.mod.bird.C +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ theme_bw()+theme(legend.position = "none")
plot.mod.bird.C <-plot.mod.bird.C + xlab('Species_applied') +ylab ('TEF d13C birds')
plot.mod.bird.C <-plot.mod.bird.C +theme(axis.text.x = element_text(size = 16, colour = 'black')) +theme(axis.text.y = element_text(size = 16, colour = 'black'))
plot.mod.bird.C <-plot.mod.bird.C +theme(axis.title.x = element_text(size = 20, colour = 'black'))+theme(axis.title.y = element_text(size = 20, colour = 'black'))
plot.mod.bird.C 

model_output_C<-cbind( mod.bird.C$b, mod.bird.C$ci.lb, mod.bird.C$ci.ub)
colnames(model_output_C)<- c("model_estimate", "lower", "upper")
head(model_output_C)
write.csv(model_output_C, "C:bird_discrimination_model_output_C.csv")


model_output_C_overall<-cbind( mod.bird.C_overall$b, mod.bird.C_overall$ci.lb, mod.bird.C_overall$ci.ub)
colnames(model_output_C_overall)<- c("model_estimate", "lower", "upper")
head(model_output_C_overall)
write.csv(model_output_C_overall, "C:bird_discrimination_model_output_C_overall.csv")



# N bird -------------------------------------------------------------


meta.bird.N<- read.csv("c:bird_discrimination_N_feather-derived.csv",stringsAsFactors = FALSE, na.strings = c("NA","") )
head(meta.bird.N)


#### Calculate effect sizes
meta.bird.N2<-escalc(measure="MD",m1i=Mean_tissue, m2i=Mean_diet, sd1i=SD_tissue,sd2i=SD_diet,n1i=N_tissue,n2i=N_diet, data=meta.bird.N,var.names=c("MD","MD_var"), digits=4)

head(meta.bird.N2)



mod.bird.N_overall <- rma(MD, MD_var, data=meta.bird.N2)
summary(mod.bird.N_overall)

mod.bird.N<- rma.mv(MD, MD_var, mods = ~factor(Species_applied) - 1, data=meta.bird.N2, random=list(~1|Paper_no))
summary(mod.bird.N)

funnel(mod.bird.N)
regtest(mod.bird.N,model="rma",predictor="sei")
funnel(trimfill(mod.bird.N, side="right"))




baujat(mod.bird.N)


### calculate influence diagnostics
inf <- influence(mod.bird.N)
### plot the influence diagnostics
plot(inf, layout=c(8,1))


rs.abund.wood <- rstandard(mod.bird.N)
hat.abund.wood <- hatvalues(mod.bird.N)/mean(hatvalues(mod.bird.N))
plot(hat.abund.wood, rs.abund.wood$resid, ylim = c(-4.0,4))
text(hat.abund.wood, rs.abund.wood$resid, labels = meta.bird.N2$Paper_no, cex= 1, pos = 2)
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


cdata.MD.bird<-meta.bird.N2 %>% group_by(Species_applied) %>%  summarise(mean=mean(MD))
cdata.MD.bird_var<-meta.bird.N2 %>% group_by %>% summarise(mean=mean(MD_var))
head(cdata.MD.bird_var)
cdata.MD.bird$var<-cdata.MD.bird_var$mean
head(cdata.MD.bird)

cdata.MD.bird2<-cbind(cdata.MD.bird, mod.bird.N$ci.lb, mod.bird.N$ci.ub)
colnames(cdata.MD.bird2)<- c("Species", "mean","var", "lower", "upper")
head(cdata.MD.bird2)


plot.mod.bird.N <-ggplot(cdata.MD.bird,aes(x=Species_applied,y=mod.bird.N $b,ymax=(mod.bird.N $ci.ub),ymin=(mod.bird.N $ci.lb),size=2))
plot.mod.bird.N <-plot.mod.bird.N +geom_pointrange(size=1)
plot.mod.bird.N <-plot.mod.bird.N  #+coord_flip()
plot.mod.bird.N <-plot.mod.bird.N +geom_hline(aes( yintercept=0), lty=2,size=1) 
plot.mod.bird.N <-plot.mod.bird.N +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ theme_bw()+theme(legend.position = "none")
plot.mod.bird.N <-plot.mod.bird.N +ylab ('TEF d15N birds') + xlab('Species_applied')
plot.mod.bird.N <-plot.mod.bird.N +theme(axis.text.x = element_text(size = 16, colour = 'black')) +theme(axis.text.y = element_text(size = 16, colour = 'black'))
plot.mod.bird.N <-plot.mod.bird.N +theme(axis.title.x = element_text(size = 20, colour = 'black'))+theme(axis.title.y = element_text(size = 20, colour = 'black'))
plot.mod.bird.N 

model_output_N<-cbind( mod.bird.N$b, mod.bird.N$ci.lb, mod.bird.N$ci.ub)
colnames(model_output_N)<- c("model_estimate", "lower", "upper")
head(model_output_N)

write.csv(model_output_N, "C:bird_discrimination_model_output_N.csv")

model_output_N_overall<-cbind( mod.bird.N_overall$b, mod.bird.N_overall$ci.lb, mod.bird.N_overall$ci.ub)
colnames(model_output_N_overall)<- c("model_estimate", "lower", "upper")
head(model_output_N_overall)
write.csv(model_output_N_overall, "C:bird_discrimination_model_output_N_overall.csv")


