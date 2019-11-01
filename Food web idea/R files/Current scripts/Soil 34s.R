library(here)
soil_s<-read.csv("C:Food web idea/Data by person/Norah.data/soil_s.csv")


# load packages ----------------------------------------------------------
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
levels <- c(-Inf, 6000, Inf)
xs2=quantile(na.omit(soil_owen_deb_s$Area),c(0,1/2,1))
xs2

labels2 <- c("small", "large")
soil_owen_deb_s<- soil_owen_deb_s%>% mutate(Area_cat = cut(Area, xs2, labels = labels2))





ggplot(soil_owen_deb_s,aes(y=d15n, x=d34s))+geom_point()+geom_smooth(method="lm")

ggplot(soil_owen_deb_s,aes(y=d15n, x=d34s, col=Area_cat))+geom_point()+geom_smooth(method="lm")


ggplot(soil_owen_deb_s,aes(y=d13c, x=d34s))+geom_point()+geom_smooth(method="lm")

ggplot(soil_owen_deb_s,aes(x=shore_dist, y=d34s))+geom_point()+geom_smooth(method="lm")
ggplot(soil_owen_deb_s,aes(x=shore_dist, y=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(soil_owen_deb_s,aes(x=shore_dist, y=s))+geom_point()+geom_smooth(method="lm")
ggplot(soil_owen_deb_s,aes(x=s, y=d34s))+geom_point()+geom_smooth(method="lm")



# subsample_owen_soils<-read.csv("C:Owen's data/subsample_owen_soil.csv")
# head(subsample_owen_soils)
# 
# 
# subsample_owen_soils_s<-merge(subsample_owen_soils, soil_s, by="unq_plot", all=TRUE)
# head(subsample_owen_soils_s)
# 
# ggplot(subsample_owen_soils_s,aes(x=s, y=d34s, colour=node))+geom_point()
# ggplot(subsample_owen_soils_s,aes(y=d34s, x=lat, colour=node, size=4))+geom_point()
# ggplot(subsample_owen_soils_s,aes(y=d15n, x=n, colour=node, size=1))+geom_point()
