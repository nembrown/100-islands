setwd("C:/Users/norahbrown/Dropbox/Projects/100 islands/Modelling practice")
#change to norahbrown if on work computer

#read in necessary packages
library(tidyr)
library(plyr)
library(dplyr)
library(doBy)
library(ggplot2)
library(car)
library(fitdistrplus)
library(tidyverse)

library(purrr)
library(ggcorrplot)
library(corrr)
library(PerformanceAnalytics)

library(vegan)
library(betapart)
library(bipartite)
library(viridis)
library(cowplot)



head(by_isl_master)

bird_summary<-read.csv("C:Deb.data//bird-summary.csv")
head(bird_summary)
names(bird_summary)[1]<-"unq_isl"
names(bird_summary)[6]<-"bird.richness"
names(bird_summary)[4]<-"bird.density"
bird_summary$bird.evenness<-bird_summary$evenness/(log(bird_summary$bird.richness))
head(bird_summary)


bird_traits<-read.csv("C:Pat.data//birdtrait_May19.csv")
head(bird_traits)


shoredist.deb<-read.csv("C:Deb.data//shoredist.csv")
#this is the point count to distance to shore data

pointcounts<-read.csv("C:Deb.data//pointcounts.csv")
head(pointcounts)
pointcounts$pcid<-gsub(" ", "", pointcounts$pcid, fixed = TRUE)
head(pointcounts)
names(pointcounts)[2]<-"unq_isl"

birdmat_raw<-read.csv("C:Deb.data//birdmat_raw_abund.csv")


birdabund<-read.csv("C:Deb.data//birdabund_taxanames.csv")
head(birdabund)
names(birdabund)[5]<-"Phylogeny_Sci"
names(birdabund)[1]<-"unq_isl"

bird_trophic <- merge(birdabund, bird_traits, by="Phylogeny_Sci", all=TRUE)
head(bird_trophic)

bird_trophic <- bird_trophic %>% group_by(unq_isl, Phylogeny_Sci)

bird_trophic$bird.biomass<-(bird_trophic$n)*(bird_trophic$BodyMass.Value)
bird_trophic$bird.biomass.per.ha<-(bird_trophic$bird.biomass/(bird_trophic$buf50_area))*10000
``
which( colnames(bird_trophic)=="bird.biomass" )
which( colnames(bird_trophic)=="bird.biomass.per.ha" )

bird_trophic_by_isl <-bird_trophic[, c(2,4,102,103)] %>% group_by(unq_isl) %>% summarise_if(is.numeric, sum, na.rm=TRUE)
head(bird_trophic_by_isl)

## this is # of INDIVIDUALS in each group across each island   
bird_trophic_diet<- bird_trophic %>% group_by(unq_isl, Diet.5Cat) %>% tally() %>% spread(Diet.5Cat, nn)
  
bird_trophic_by_isl<- merge(bird_trophic_by_isl, bird_trophic_diet, by="unq_isl")
head(bird_trophic_by_isl)

bird_trophic_by_isl_by_species<-bird_trophic %>% group_by(unq_isl,Phylogeny_Sci,Diet.5Cat) %>% tally() %>% mutate(nn= ifelse(nn>0,1,0)) %>% 
  group_by(unq_isl, Diet.5Cat)%>% summarise_if(is.numeric, sum, na.rm=TRUE) %>% spread(Diet.5Cat, nn)
names(bird_trophic_by_isl_by_species)[2]<-"FruiNect_spp"
names(bird_trophic_by_isl_by_species)[3]<-"Invertebrate_spp"
names(bird_trophic_by_isl_by_species)[4]<-"Omnivore_spp"
names(bird_trophic_by_isl_by_species)[5]<-"PlantSeed_spp"
names(bird_trophic_by_isl_by_species)[6]<-"VertFishScav_spp"
head(bird_trophic_by_isl_by_species)


bird_trophic_by_isl<- merge(bird_trophic_by_isl, bird_trophic_by_isl_by_species, by="unq_isl")
head(bird_trophic_by_isl)
names(bird_trophic_by_isl)[2]<-"num.bird.isl"

bird_trophic_by_isl<- merge(bird_trophic_by_isl, bird_summary, by="unq_isl")
head(bird_trophic_by_isl)

head(by_isl_master)

bird_with_isl_master<-merge(by_isl_master, bird_trophic_by_isl, by="unq_isl")
head(bird_with_isl_master)

bird_with_isl_master<-merge(bird_with_isl_master, bird_isotopes, by="unq_isl")
head(bird_with_isl_master)



ggplot(bird_with_isl_master, aes(y=bird.density, x=d15n.x))+geom_point()+geom_smooth(method="lm")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
ggplot(bird_with_isl_master, aes(y=bird.evenness, x=d15n.x))+geom_point()+geom_smooth(method="lm")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
ggplot(bird_with_isl_master, aes(y=bird.biomass.per.ha, x=log_Area))+geom_point()+geom_smooth(method="lm")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
ggplot(bird_with_isl_master, aes(y=bird.biomass.per.ha, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))


soil_trait1<-ggplot(bird_with_isl_master, aes(y=Invertebrate_spp/obs.n.species, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait2<-ggplot(bird_with_isl_master, aes(y=FruiNect_spp/obs.n.species, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait3<-ggplot(bird_with_isl_master, aes(y=Omnivore_spp/obs.n.species, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait4<-ggplot(bird_with_isl_master, aes(y=PlantSeed_spp/obs.n.species, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait5<-ggplot(bird_with_isl_master, aes(y=VertFishScav_spp/obs.n.species, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait6<-ggplot(bird_with_isl_master, aes(y=Invertebrate/n.birds.50, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait7<-ggplot(bird_with_isl_master, aes(y=FruiNect/n.birds.50, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait8<-ggplot(bird_with_isl_master, aes(y=Omnivore/n.birds.50, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait9<-ggplot(bird_with_isl_master, aes(y=PlantSeed/n.birds.50, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait10<-ggplot(bird_with_isl_master, aes(y=VertFishScav/n.birds.50, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))


plot_grid(soil_trait1, soil_trait2, soil_trait3, soil_trait4, soil_trait5, soil_trait6,soil_trait7,soil_trait8,soil_trait9,soil_trait10, ncol=5)
ggsave("C:Plots//Richness_nut//Trait_soil_n15.png", width=30, height=20, unit="cm")



ggplot(bird_with_isl_master, aes(y=Invertebrate/n.birds.50, x=log(bird.biomass.per.ha)))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
ggplot(bird_with_isl_master, aes(y=FruiNect_spp, x=d15n))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))


trait1<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=Invertebrate_spp/obs.n.species, x=d15n.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
trait2<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=FruiNect_spp/obs.n.species, x=d15n.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait3<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=Omnivore_spp/obs.n.species, x=d15n.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait4<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=PlantSeed_spp/obs.n.species, x=d15n.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait5<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=VertFishScav_spp/obs.n.species, x=d15n.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait6<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=Invertebrate/n.birds.50, x=d15n.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait7<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=FruiNect/n.birds.50, x=d15n.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait8<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=Omnivore/n.birds.50, x=d15n.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait9<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=PlantSeed/n.birds.50, x=d15n.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait10<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=VertFishScav/n.birds.50, x=d15n.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")


plot_grid(trait1, trait2, trait3, trait4, trait5, trait6,trait7,trait8,trait9,trait10, ncol=5)
ggsave("C:Plots//Richness_nut//Trait_bird_n15.png", width=30, height=20, unit="cm")



trait1<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=Invertebrate_spp/obs.n.species, x=d13c.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
trait2<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=FruiNect_spp/obs.n.species, x=d13c.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait3<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=Omnivore_spp/obs.n.species, x=d13c.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait4<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=PlantSeed_spp/obs.n.species, x=d13c.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait5<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=VertFishScav_spp/obs.n.species, x=d13c.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait6<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=Invertebrate/n.birds.50, x=d13c.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait7<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=FruiNect/n.birds.50, x=d13c.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait8<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=Omnivore/n.birds.50, x=d13c.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait9<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=PlantSeed/n.birds.50, x=d13c.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
trait10<-ggplot(bird_with_isl_master, aes(fill=group, col=group, y=VertFishScav/n.birds.50, x=d13c.y))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")


plot_grid(trait1, trait2, trait3, trait4, trait5, trait6,trait7,trait8,trait9,trait10, ncol=5)

soil_trait1<-ggplot(bird_with_isl_master, aes(y=Invertebrate_spp/obs.n.species, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait2<-ggplot(bird_with_isl_master, aes(y=FruiNect_spp/obs.n.species, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait3<-ggplot(bird_with_isl_master, aes(y=Omnivore_spp/obs.n.species, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait4<-ggplot(bird_with_isl_master, aes(y=PlantSeed_spp/obs.n.species, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait5<-ggplot(bird_with_isl_master, aes(y=VertFishScav_spp/obs.n.species, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait6<-ggplot(bird_with_isl_master, aes(y=Invertebrate/n.birds.50, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait7<-ggplot(bird_with_isl_master, aes(y=FruiNect/n.birds.50, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait8<-ggplot(bird_with_isl_master, aes(y=Omnivore/n.birds.50, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait9<-ggplot(bird_with_isl_master, aes(y=PlantSeed/n.birds.50, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
soil_trait10<-ggplot(bird_with_isl_master, aes(y=VertFishScav/n.birds.50, x=d15n.x))+geom_point()+geom_smooth(method="gam")+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))


plot_grid(soil_trait1, soil_trait2, soil_trait3, soil_trait4, soil_trait5, soil_trait6,soil_trait7,soil_trait8,soil_trait9,soil_trait10, ncol=5)

