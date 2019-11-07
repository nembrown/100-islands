library(here)
soil_s<-read.csv("C:Food web idea/Data by person/Norah.data/soil_s.csv")


s()# load packages ----------------------------------------------------------
library(rlang)
library(tidyr)
library(vegan)
library(ggplot2)
library(ggcorrplot)
library(doBy)
library(plyr)
library(dplyr)
library(doBy)
library(cowplot)
library(viridis)
library(matrixStats)
library(tidyverse)

head(soil_s)


soil_owen_deb<-read.csv("C:Food web idea//Data by person//Norah.data//soil_owen_deb.csv")
soil_owen_deb<-soil_owen_deb[,-1]
head(soil_owen_deb)

soil_owen_deb_s<-merge(soil_owen_deb, soil_s, by="unq_plot")
head(soil_owen_deb_s)

island_data_wiebe<-read.csv("C:Food web idea//Data by person//Pat.data//Islands_Master_Vegetation2017.csv", header=TRUE, sep=",")
head(island_data_wiebe)
names(island_data_wiebe)[1]<-"unq_isl"

soil_owen_deb_s<-merge(soil_owen_deb_s, island_data_wiebe, by="unq_isl")
head(soil_owen_deb_s)

#Label small islands as those with Area less than 6000m2
xs2=quantile(na.omit(soil_owen_deb_s$Area),c(0,1/2,1))
xs2

labels2 <- c("small", "large")
soil_owen_deb_s<- soil_owen_deb_s%>% mutate(Area_cat = cut(Area, xs2, labels = labels2))

levels_shore_dist <- c(-Inf,39,100, Inf)
labels3 <- c("0-40", "40-100", "100 plus")
soil_owen_deb_s<- soil_owen_deb_s%>% mutate(shore_cat = cut(shore_dist, levels_shore_dist, labels = labels3))





ggplot(soil_owen_deb_s,aes(y=d15n, x=d34s))+geom_point()+geom_smooth(method="lm")

ggplot(soil_owen_deb_s,aes(y=d15n, x=d34s, col=Area_cat))+geom_point()+geom_smooth(method="lm")
ggplot(soil_owen_deb_s,aes(y=d15n, x=d34s, col=slope_mean))+geom_point()
ggplot(soil_owen_deb_s,aes( y=d15n, x=n))+geom_point()+geom_smooth(method="lm")


ggplot(soil_owen_deb_s,aes( y=d15n, x=d34s))+geom_point()+geom_smooth(method="lm")



ggplot(soil_owen_deb_s,aes(y=d13c, x=d34s))+geom_point()+geom_smooth(method="lm")

ggplot(soil_owen_deb_s,aes(x=shore_dist, y=d34s))+geom_point()+geom_smooth(method="lm")
ggplot(soil_owen_deb_s,aes(x=shore_dist, y=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(soil_owen_deb_s,aes(x=shore_dist, y=s))+geom_point()+geom_smooth(method="lm")


ggplot(soil_owen_deb_s,aes(x=s, y=d34s, col=Area_cat))+geom_point()+geom_smooth(method="gam")

ggplot(soil_owen_deb_s,aes(x=log(Area), y=d34s, col=shore_dist))+geom_point()+geom_smooth(method="lm")

