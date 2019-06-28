setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Modelling practice")
#change to norahbrown if on work computer

library(tidyr)
library(plyr)
library(dplyr)
library(doBy)
library(ggplot2)




wrack.sara<-read.csv("C:Sara's data//wrack.isotopes.cn.csv")
wrack.key<-read.csv("C:Sara's data//wrack.key.csv")
ang.seaweed<-read.csv("C:Ang's data//chokedpass_macrophytes_AMO2015.csv", header=TRUE, sep=",")

#seaweed
names(ang.seaweed)[8]<-"d13c"
names(ang.seaweed)[9]<-"d15n"
ang.seaweed$Dryness<-"L"
ang.seaweed$person<-"Ang"
names(wrack.sara)[2]<-"d13c"
names(wrack.sara)[5]<-"d15n"
wrack.sara$group<-"wrack"
wrack.merge<-merge(wrack.key, wrack.sara, by="Sample.ID")
names(wrack.merge)[3]<-"Node"
wrack.merge$person<-"Sara"


#seaweed plotting
head(wrack.merge)
head(ang.seaweed)
all.seaweed<-rbind(wrack.merge[,c(8,9,16,19,32,4)], ang.seaweed[,c(3,11,8,9,12,5)])
write.csv(all.seaweed, "C:Sara's data/all.seaweed.csv")


ggplot((filter(all.seaweed, species=="zostera marina")), aes(x=d13c, y=d15n))  + geom_point(aes(col=site, shape=Dryness), size=2)+scale_shape_manual(values=c(16,2))+stat_ellipse(aes(colour=site))
ggplot((filter(all.seaweed, species=="nereocystis luetkeana")), aes(x=d13c, y=d15n))  + geom_point(aes(col=site, shape=Dryness), size=2)+scale_shape_manual(values=c(16,2))
ggplot((filter(all.seaweed, species=="fucus disticus")), aes(x=d13c, y=d15n))  + geom_point(aes(col=site, shape=Dryness), size=2)+scale_shape_manual(values=c(16,2))
ggplot((filter(all.seaweed, species=="")), aes(x=d13c, y=d15n))  + geom_point(aes(col=site, shape=Dryness), size=2)+scale_shape_manual(values=c(16,2))



ggplot(all.seaweed, aes(x=d13c, y=d15n))  + geom_point(aes(col=species, shape=Dryness), size=2)+scale_shape_manual(values=c(16,2))
ggsave("C:Sara's data//seaweed by species iso.png")


ggplot(all.seaweed, aes(x=d13c, y=d15n))  + geom_point(aes(col=species, shape=person), size=2)+scale_shape_manual(values=c(16,2))
ggsave("C:Sara's data//seaweed by species and person.png")



ggplot(all.seaweed, aes(x=d13c, y=d15n))  + geom_point(aes(col=person, shape=Dryness), size=2)+scale_shape_manual(values=c(16,2))+stat_ellipse(aes(colour=person))
ggsave("C:Sara's data//sara vs. ang seaweed iso.png")

ggplot(filter(all.seaweed, person=="Sara"), aes(x=d13c, y=d15n))  + geom_point(aes(col=Dryness, shape=Dryness), size=2)+scale_shape_manual(values=c(16,2))+stat_ellipse(aes(colour=Dryness))
ggsave("C:Sara's data//sara wrack vs. seaweed.png")

ggplot(filter(all.seaweed, person=="Sara"), aes(x=d13c, y=d15n))  + geom_point(aes(col=species, shape=Dryness), size=2)+scale_shape_manual(values=c(16,2))+stat_ellipse(aes(colour=species))
ggsave("C:Sara's data//sara by species.png")

ggplot((filter(all.seaweed, species=="zostera marina", person=="Sara")), aes(x=d13c, y=d15n))  + geom_point(aes(col=site, shape=Dryness), size=2)+scale_shape_manual(values=c(16,2))+stat_ellipse(aes(colour=site))
ggsave("C:Sara's data//sara zostera by site.png")

ggplot((filter(all.seaweed, species=="fucus disticus", person=="Sara")), aes(x=d13c, y=d15n))  + geom_point(aes(col=site, shape=Dryness), size=2)+scale_shape_manual(values=c(16,2))+stat_ellipse(aes(colour=site))
ggsave("C:Sara's data//sara fucus by site.png")

ggplot((filter(all.seaweed, species=="nereocystis luetkeana", person=="Sara")), aes(x=d13c, y=d15n))  + geom_point(aes(col=site, shape=Dryness), size=2)+scale_shape_manual(values=c(16,2))+stat_ellipse(aes(colour=site))
ggsave("C:Sara's data//sara nereo by site.png")

ggplot((filter(all.seaweed, species=="phyllospadix", person=="Sara")), aes(x=d13c, y=d15n))  + geom_point(aes(col=site, shape=Dryness), size=2)+scale_shape_manual(values=c(16,2))+stat_ellipse(aes(colour=site))
ggsave("C:Sara's data//sara phyllo by site.png")


ggplot(filter(all.seaweed, person=="Sara"), aes(x=factor(species), y=d13c, fill=factor(species)))+geom_boxplot()
ggsave("C:Sara's data//d13c sara by species colour.png")

ggplot(filter(all.seaweed, person=="Sara"), aes(x=factor(species), y=d13c, fill=factor(Dryness)))+geom_boxplot()
ggsave("C:Sara's data//d13c sara by species.png")

ggplot(filter(all.seaweed, person=="Ang"), aes(x=factor(species), y=d13c, fill=factor(species)))+geom_boxplot()
ggsave("C:Sara's data//d13c ang by species.png")

ggplot(filter(all.seaweed, person=="Sara"), aes(x=factor(species), y=d15n, fill=factor(Dryness)))+geom_boxplot()
ggsave("C:Sara's data//d15n sara by species.png")

ggplot(filter(all.seaweed, person=="Ang"), aes(x=factor(species), y=d15n, fill=factor(species)))+geom_boxplot()
ggsave("C:Sara's data//d15n ang by species.png")



#sulfur in wrack
wrack.sara.s<-read.csv("C:Sara's data//wrack.isotopes.s.csv")
head(wrack.sara.s)
wrack.key.s<-read.csv("C:Sara's data//wrack.key.s.csv")
head(wrack.key.s)

names(wrack.sara.s)[3]<-"d34S"

wrack.merge.s<-merge(wrack.key.s, wrack.sara.s, by="Sample.ID")
head(wrack.merge.s)

ggplot(wrack.merge.s, aes(x=factor(SPECIES), y=d34S, fill=factor(SPECIES)))+geom_boxplot()





