setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Modelling practice")
#change to norahbrown if on work computer

library(tidyr)
library(plyr)
library(dplyr)
library(doBy)
library(ggplot2)

#loading Anne's data
anne.isotopes<-read.csv("C:Anne's data//Solomon_centralcoast_isotope.csv", header=T, sep=",", stringsAsFactors=FALSE)

#anne.coordinates<-read.csv("C:Anne's data//Solomon_centralcoast_coordinates.csv", sep=",", stringsAsFactors=FALSE)
#head(anne.coordinates)
#anne.isotopes<-merge(anne.isotopes, anne.coordinates, by="Site")

head(anne.isotopes)
str(anne.isotopes)
anne.isotopes$d15N<-as.numeric(anne.isotopes$d15N)
anne.isotopes$Latitude<-as.numeric(anne.isotopes$Latitude)
anne.isotopes$Longitude<-as.numeric(anne.isotopes$Longitude)


ggplot(anne.isotopes, aes(x=d13C, y=d15N)) + geom_point(aes(col=common.name))+stat_ellipse(aes(color=common.name))

ggplot(anne.isotopes, aes(x=d13C, y=d15N)) + geom_point(aes(col=species.code))+stat_ellipse(aes(color=species.code))



ggplot(filter(anne.isotopes, sample.class!="macroalgae"), aes(x=d13C, y=d15N)) + geom_point(aes(col=common.name))+stat_ellipse(aes(color=common.name))



ggplot(filter(anne.isotopes, sample.class!="macroalgae"), aes(x=d13C, y=d15N)) + geom_point(aes(col=sample.class))+stat_ellipse(aes(color=sample.class))




ggplot(filter(anne.isotopes, sample.class=="fish"), aes(x=d13C, y=d15N)) + geom_point(aes(col=Site))

ggplot(filter(anne.isotopes, sample.class=="bivalve"), aes(x=d13C, y=d15N)) + geom_point(aes(col=Site))


ggplot(filter(anne.isotopes, sample.class!="macroalgae"), aes(x = sample.class,y=d15N)) + geom_boxplot(aes(col=sample.class))


ggplot(anne.isotopes, aes(x =Latitude,y=d15N, col=sample.class)) + geom_point()+geom_smooth(method="lm")



#### otter
otter_chris<-read.csv("C:Chris.data//otter_sia.csv")
names(otter_chris)[6]<-"d13C"
names(otter_chris)[8]<-"d15N"
head(otter_chris)
otter_chris_onsite_soil<-otter_chris[otter_chris$group=="AC" & otter_chris$SIASample=="Soil",]


ggplot(filter(otter_chris, SIASample=="Soil"), aes(x=d13C, y=d15N)) + geom_point(aes(col=Region))+stat_ellipse(aes(color=Region))



