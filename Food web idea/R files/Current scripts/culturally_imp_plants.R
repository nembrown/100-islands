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

#this loads data from "Habitation data" R script

longform_plant_percentcover<-read.csv("C:Food web idea//Data by person//Kalina.data/Deb_Owen_veg_combined_complete_filled.csv", header=TRUE, sep=",")
View(longform_plant_percentcover)

longform_plant_percentcover_owen <- longform_plant_percentcover %>% filter(person=="Owen")
head(longform_plant_percentcover_owen)
longform_plant_percentcover_owen$cover<-as.numeric(longform_plant_percentcover_owen$cover)



## CULTURALLY IMPORTANT SPECIES LIST
# Create column - cult important (yes/no), then merge
cult.imp.sp <- c("rhgr",
                 "vevi",
                 "frca",
                 "lyam",
                 "midi",
                 "pogl",
                 "pomu",
                 "ptaq",
                 "cosp",
                 "laja",
                 "goob",
                 "drro",
                 "poan",
                 "epan",
                 "amal",
                 "gash",
                 "rusp",
                 "vaov",
                 "vapa",
                 "ribr",
                 "rupa",
                 "rila",
                 "rhpu",
                 "loin",
                 "rupe",
                 "opho",
                 "ronu",
                 "thpl",
                 "tshe",
                 "chno",
                 "alru",
                 "mafu",
                 "pisi",
                 "tabr",
                 "pico")
# Creating new dataframe, filter by cult sp
plant_data_cult <- longform_plant_percentcover_owen %>% filter(species %in% cult.imp.sp)



plant_data_cult_wide <- plant_data_cult[,c(1:8)] %>% 
  group_by(unq_tran,species) %>% 
  summarise(cover_mean = mean(cover, na.rm=TRUE)) %>% 
  spread(species, cover_mean)%>%  replace(is.na(.), 0)

head(plant_data_cult_wide)

# Create new DF for richness by node
plant_data_cult_richness <- plant_data_cult_wide %>% dplyr::select(unq_tran)
# Add new col richness, dont wanna count node for sp (-1), will give 1 sp number per node
plant_data_cult_richness$cult_imp_plant_richness <- specnumber(plant_data_cult_wide[,-1])
# Order nodes by richness

View(plant_data_cult_richness)

write.csv(plant_data_cult_richness, "C:Biodiversity idea//Output files//plant_data_cult_richness.csv", row.names=FALSE)

