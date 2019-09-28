setwd("C:/Users/Norah/Dropbox/Projects/100-islands/Food web idea/Data by person")
soil_s<-read.csv("C:Norah.data/soil_s.csv")


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

subsample_owen_soils<-read.csv("C:Owen's data/subsample_owen_soil.csv")
head(subsample_owen_soils)


subsample_owen_soils_s<-merge(subsample_owen_soils, soil_s, by="unq_plot", all=TRUE)
head(subsample_owen_soils_s)

ggplot(subsample_owen_soils_s,aes(y=d15n, x=d34s, colour=node, size=4))+geom_point()
ggplot(subsample_owen_soils_s,aes(y=d34s, x=shore_dist, colour=node, size=4))+geom_point()



ggplot(subsample_owen_soils_s,aes(y=d15n, x=n, colour=node, size=2))+geom_point()
