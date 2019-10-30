library(here)

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

barnacles_flies<-read.csv("C:Food web idea/Data by person/Norah.data/barnacles_flies.csv")



head(barnacles_flies)


ggplot(barnacles_flies,aes(y=d15n, x=d13c, colour=Species, pch=Beach, size=3))+geom_point()+theme_bw()

