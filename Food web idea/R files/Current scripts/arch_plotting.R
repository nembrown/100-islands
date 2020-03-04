##loading necesasry libraries

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
library(cowplot)


#This is Margot's data joined to Duncan McLaren's data - i.e. no 100 islands - reads from ben_fish_cleaning file
fish_arch<-read.csv("C:Biodiversity idea//Output files//fish_bycatch_richness_merged_tran_year_arch.csv")
head(fish_arch)
#Are the position of fish traps related to fish biomass? 
ggplot(fish_arch, aes(x=fish_feature, y=fish_biomass_bym3_mean, col=fish_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=fish_feature, y=fish_pelagic_biomass_bym3_mean, col=fish_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=fish_feature, y=fish_demersal_biomass_bym3_mean, col=fish_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=fish_feature, y=fish_av_weight, col=fish_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=fish_feature, y=fish_length, col=fish_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=fish_feature, y=bycatch_biomass_bym3_mean, col=fish_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=fish_feature, y=fish_richness_corrected, col=fish_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=fish_feature, y=bycatch_richness_corrected, col=fish_feature))+
  geom_boxplot()+geom_jitter(width=0.2)



#Problem: there's only 10 fish traps within a km of beachseine site, try with middens
ggplot(fish_arch, aes(x=midden_feature, y=fish_biomass_bym3_mean, col=midden_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=midden_feature, y=fish_pelagic_biomass_bym3_mean, col=midden_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=midden_feature, y=fish_demersal_biomass_bym3_mean, col=midden_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=midden_feature, y=fish_av_weight, col=midden_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=midden_feature, y=fish_length, col=midden_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=midden_feature, y=bycatch_biomass_bym3_mean, col=midden_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=midden_feature, y=fish_richness_corrected, col=midden_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

ggplot(fish_arch, aes(x=midden_feature, y=bycatch_richness_corrected, col=midden_feature))+
  geom_boxplot()+geom_jitter(width=0.2)


###THIS ONE
ggplot(fish_arch, aes(x=midden_feature, y=marine_richness_bym3, col=midden_feature))+
  geom_boxplot()+geom_jitter(width=0.2)

