setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Modelling practice")
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



#loading all the data

sara_composition<-read.csv("c:Sara's data//sara_composition.csv", header=TRUE, sep=",")
head(sara_composition)


sara_composition_means <- sara_composition %>%  group_by(unq_isl)%>% summarise_each(funs(mean))

str(sara_composition_means)

sara_composition_means$wrack_richness<-specnumber(sara_composition_means[,7:51])
head(sara_composition_means)
