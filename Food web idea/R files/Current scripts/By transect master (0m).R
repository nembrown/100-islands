library(here)
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






# Loading soil data -------------------------------------------------------


#####OWEN
#owen's isotope data by plot
soil_clean<-read.csv("C:Food web idea//Food web idea//Data by person//Owen's data//soil_clean.csv", header=TRUE, sep=",")
head(soil_clean)
length((soil_clean$unq_plot))

#duplicated plots: 
soil_clean[duplicated(soil_clean$unq_plot),]
#let's keep them in for now

names(soil_clean)[6]<-"d13c"
names(soil_clean)[7]<-"d15n"

head(soil_clean)

#Owen's key data
owen_key<-read.csv("C:Food web idea//Food web idea//Data by person//Owen's data//key_mod_2019.csv", header=TRUE, sep=",")
head(owen_key)
length(unique(owen_key$unq_tran))

#Owen's plot-level soil info - moisture, slope etc
hakai_plot<-read.csv("C:Food web idea//Data by person//Owen's data//hakai_plot.csv", header=TRUE, sep=",")
names(hakai_plot)[3]<-"plant.richness"
head(hakai_plot)

owen_key_expanded<-merge(owen_key, hakai_plot, by="unq_plot", all=TRUE)
head(owen_key_expanded)
length(unique(owen_key_expanded$unq_tran))

#Add in the GPS coordinates
owen_coords<-read.csv("C:Food web idea//Food web idea//Data by person//Becky.data//ofwi_tran_coords_mod_3.csv", header=TRUE, sep=",")
head(owen_coords)
owen_coords<-owen_coords[,c(1:9)]
head(owen_coords)

owen_coords$unq_tran<- paste(owen_coords$unq_isl,owen_coords$TRANSECT)
owen_coords$unq_tran<-gsub(" ", "", owen_coords$unq_tran, fixed = TRUE)

owen_coords<-owen_coords[,c(3,4, 10)]
head(owen_coords)
names(owen_coords)[1]<-"easting"
names(owen_coords)[2]<-"northing"

owen_key_expanded<-merge(owen_key_expanded, owen_coords, by="unq_tran", all=TRUE)
head(owen_key_expanded)

head(soil_clean)

#put isotope data together with the key
soil_merge<-merge(soil_clean, owen_key_expanded, by="unq_plot")
head(soil_merge)


write.csv(soil_merge, "C:Food web idea\\Data by person\\Norah.data\\soil_merge.csv")


#
### add in d34s here
soil_s<-read.csv("C:Food web idea/Data by person/Norah.data/soil_s.csv")
head(soil_s)

soil_merge_s<-merge(soil_merge, soil_s, by="unq_plot", all=TRUE)
head(soil_merge_s)


# ggplot(soil_merge_s, aes(x=d34s, y=d15n, col=shore_dist))+geom_point()

soil_merge_0m <- soil_merge_s %>% filter(shore_dist == 0)
soil_merge_0m<-soil_merge_0m[,-c(8,10, 11, 13, 14, 15)]

head(soil_merge_0m)
# ggplot(soil_merge_0m, aes(x=d34s, y=d15n))+geom_point()


write.csv(soil_merge_0m, "C:Food web idea\\Data by person\\Norah.data\\soil_merge_0m.csv", row.names=FALSE)


# Adding plant cover and richnes sshoreline -----------------------------------------

#this loads data from "Habitation data" R script

longform_plant_percentcover<-read.csv("C:Food web idea//Data by person//Kalina.data/Deb_Owen_veg_combined_complete_filled.csv", header=TRUE, sep=",")
longform_plant_percentcover<-longform_plant_percentcover[,-c(1)]

#longform_plant_percentcover$unq_tran<-strtrim(longform_plant_percentcover$unq_tran, 5)

longform_plant_percentcover_owen <- longform_plant_percentcover %>% filter(person=="Owen")
longform_plant_percentcover_owen_0m<-longform_plant_percentcover_owen %>% filter(shore_dist=="0")
head(longform_plant_percentcover_owen_0m)

length(unique(longform_plant_percentcover_owen_0m$unq_plot))
length(unique(longform_plant_percentcover_owen_0m_shrub$unq_plot))


longform_plant_percentcover2_tran_0m <- longform_plant_percentcover_owen_0m[,c(1:7)] %>% 
                                       group_by(unq_plot,species) %>% 
                                       spread(species, cover)%>%  replace(is.na(.), 0)


longform_plant_percentcover_owen_0m_shrub<- longform_plant_percentcover_owen_0m %>% filter(herb_shrub=="shrub")
longform_plant_percentcover_owen_0m_herb<- longform_plant_percentcover_owen_0m %>% filter(herb_shrub=="herb")

longform_plant_percentcover2_tran_0m_shrub <- longform_plant_percentcover_owen_0m_shrub[,c(1:7)] %>% 
  group_by(unq_plot,species) %>% 
  spread(species, cover)%>%  replace(is.na(.), 0)

longform_plant_percentcover2_tran_0m_herb <- longform_plant_percentcover_owen_0m_herb[,c(1:7)] %>% 
  group_by(unq_plot,species) %>% 
  spread(species, cover)%>%  replace(is.na(.), 0)

longform_plant_percentcover_species_tran_0m<-longform_plant_percentcover2_tran_0m
head(longform_plant_percentcover_species_tran_0m)

which( colnames(longform_plant_percentcover2_tran_0m)=="gash" )
which( colnames(longform_plant_percentcover2_tran_0m)=="midi" )


longform_plant_percentcover3_tran_0m<-longform_plant_percentcover2_tran_0m[,c(1,4,41,56)]
head(longform_plant_percentcover3_tran_0m)
longform_plant_percentcover3_tran_0m$plant_richness<-specnumber(longform_plant_percentcover_species_tran_0m[,-c(1:5)])
longform_plant_percentcover3_tran_0m$plant_shannon.diversity<-diversity(longform_plant_percentcover_species_tran_0m[,-c(1:5)], index="shannon")
longform_plant_percentcover3_tran_0m$plant_evenness<-longform_plant_percentcover3_tran_0m$plant_shannon.diversity/(log(longform_plant_percentcover3_tran_0m$plant_richness))
longform_plant_percentcover3_tran_0m$total_cover<-rowSums(longform_plant_percentcover_species_tran_0m[,-c(1:5)], na.rm=TRUE)

longform_plant_percentcover3_tran_0m$shrub_richness<-specnumber(longform_plant_percentcover2_tran_0m_shrub[,-c(1:5)])
longform_plant_percentcover3_tran_0m$shrub_cover<-rowSums(longform_plant_percentcover2_tran_0m_shrub[,-c(1:5)], na.rm=TRUE)
longform_plant_percentcover3_tran_0m$herb_richness<-specnumber(longform_plant_percentcover2_tran_0m_herb[,-c(1:5)])
longform_plant_percentcover3_tran_0m$herb_cover<-rowSums(longform_plant_percentcover2_tran_0m_herb[,-c(1:5)], na.rm=TRUE)

longform_plant_percentcover3_tran_0m$unq_tran<-strtrim(longform_plant_percentcover3_tran_0m$unq_tran, 5)

which( colnames(longform_plant_percentcover3_tran_0m)=="unq_tran" )


head(soil_merge_0m)

habitat_veg_soil_by_tran_0m<-merge(soil_merge_0m, longform_plant_percentcover3_tran_0m[,-2], by="unq_plot", all=TRUE)
head(habitat_veg_soil_by_tran_0m)


# Vegetation isotopes -----------------------------------------------------


owen.veg_tran<-read.csv("C:Food web idea//Data by person\\Owen's data\\foliar_clean_sorted_merge_meta.csv")
head(owen.veg_tran)
owen.veg_tran<-owen.veg_tran[,-1]

owen.veg_tran_0m<-owen.veg_tran %>% filter(shore_dist=="0")

owen.veg_tran_0m_gash <- owen.veg_tran_0m %>% filter(species == "gash")
owen.veg_tran_0m_midi <- owen.veg_tran_0m %>% filter(species == "midi")
head(owen.veg_tran_0m_gash)

names(owen.veg_tran_0m_gash)[3]<-"n_gash"
names(owen.veg_tran_0m_gash)[4]<-"c_gash"
names(owen.veg_tran_0m_gash)[5]<-"cn_gash"
names(owen.veg_tran_0m_gash)[6]<-"s_gash"
names(owen.veg_tran_0m_gash)[7]<-"d13c_gash"
names(owen.veg_tran_0m_gash)[8]<-"d15n_gash"

names(owen.veg_tran_0m_midi)[3]<-"n_midi"
names(owen.veg_tran_0m_midi)[4]<-"c_midi"
names(owen.veg_tran_0m_midi)[5]<-"cn_midi"
names(owen.veg_tran_0m_midi)[6]<-"s_midi"
names(owen.veg_tran_0m_midi)[7]<-"d13c_midi"
names(owen.veg_tran_0m_midi)[8]<-"d15n_midi"

owen.veg_tran_0m_midi$transect<-strtrim(owen.veg_tran_0m_midi$transect, 1)
owen.veg_tran_0m_midi$unq_tran<- paste(owen.veg_tran_0m_midi$unq_isl,owen.veg_tran_0m_midi$transect)
owen.veg_tran_0m_midi$unq_tran<-gsub(" ", "", owen.veg_tran_0m_midi$unq_tran, fixed = TRUE)

owen.veg_tran_0m_gash$transect<-strtrim(owen.veg_tran_0m_gash$transect, 1)
owen.veg_tran_0m_gash$unq_tran<- paste(owen.veg_tran_0m_gash$unq_isl,owen.veg_tran_0m_gash$transect)
owen.veg_tran_0m_gash$unq_tran<-gsub(" ", "", owen.veg_tran_0m_gash$unq_tran, fixed = TRUE)

owen.veg_tran_0m_gash <-owen.veg_tran_0m_gash %>% group_by(unq_tran) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(owen.veg_tran_0m_gash)

owen.veg_tran_0m_midi <-owen.veg_tran_0m_midi %>% group_by(unq_tran) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(owen.veg_tran_0m_midi)

habitat_veg_soil_by_tran_0m<-merge(habitat_veg_soil_by_tran_0m, owen.veg_tran_0m_midi[,c(1:7)], by="unq_tran", all=TRUE)
habitat_veg_soil_by_tran_0m<-merge(habitat_veg_soil_by_tran_0m, owen.veg_tran_0m_gash[,c(1:7)], by="unq_tran", all=TRUE)
head(habitat_veg_soil_by_tran_0m)




# Chris insects diversity -----------------------------------------------------------
#copied code over from soil regression plotting (by_isl_master) file
#new data July 2019 

chris_insects_master<-read.csv("C:Food web idea//Data by person//Chris.data//invert_id_abundance.csv", header=TRUE, sep=",")
head(chris_insects_master)
chris_insects_master$unq_isl<-strtrim(chris_insects_master$Trapline, 4)
chris_insects_master$unq_tran<-strtrim(chris_insects_master$Trapline, 5)
chris_insects_master$plot<-substr(chris_insects_master$Trapline, 5, 5)
head(chris_insects_master)

# I still want to divde by groups BUT I will do all insects as a whole first then break into groups

# For each island there are 4 transects and one interior plot
#My understanding is that there is only one pitfall or one beat per "transect".. if that's not true I will need to change script. 

chris_insects_master_wide_tran <-chris_insects_master %>% group_by(unq_tran, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
head(chris_insects_master_wide_tran)
#659 Species!!! 

chris_insects_master_wide_tran_richness<-chris_insects_master_wide_tran[,1]
chris_insects_master_wide_tran_richness$insect_richness<-specnumber(chris_insects_master_wide_tran[,-1])
chris_insects_master_wide_tran_richness$insect_abs_abundance<-rowSums(chris_insects_master_wide_tran[,-1],na.rm = TRUE)
chris_insects_master_wide_tran_richness$insect_diversity<-diversity(chris_insects_master_wide_tran[,-1],index="shannon")
chris_insects_master_wide_tran_richness$insect_evenness<-chris_insects_master_wide_tran_richness$insect_diversity/(log(chris_insects_master_wide_tran_richness$insect_richness))
head(chris_insects_master_wide_tran_richness)


#now a more standardized abundance measure per beat or pitfall trap on the island
chris_trapline_data<-read.csv("C:Food web idea//Data by person//Chris.data//trapline_data.csv", header=TRUE, sep=",")
chris_trapline_data$unq_isl<-strtrim(chris_trapline_data$Trapline, 4)
chris_trapline_data$unq_tran<-strtrim(chris_trapline_data$Trapline, 5)
chris_trapline_data$plot<-substr(chris_trapline_data$Trapline, 5, 5)
head(chris_trapline_data)

#sum number of insects found on that trapline/traptype
chris_insects_master_by_trap_tran<-chris_insects_master %>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_tran<-merge(chris_insects_master_by_trap_tran, chris_trapline_data[,-c(2:15)], by="unq_tran")

head(chris_insects_master_by_trap_tran)

chris_insects_master_by_trap_tran$insect_beat_abundance<-chris_insects_master_by_trap_tran$sum_abundance/chris_insects_master_by_trap_tran$BeatTime

chris_insects_master_by_trap_tran_beat<-chris_insects_master_by_trap_tran %>%  filter(Trap=="Beat") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_beat_av_abundance = mean(insect_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_tran$insect_pitfall_abundance<-chris_insects_master_by_trap_tran$sum_abundance/chris_insects_master_by_trap_tran$PitfallsCount
chris_insects_master_by_trap_tran_pitfall<-chris_insects_master_by_trap_tran %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_pitfall_av_abundance = mean(insect_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_beat, all=TRUE)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_pitfall, all=TRUE)

head(chris_insects_master_wide_tran_richness)

#########now the same measures but for a few different categories. 
head(chris_insects_master)

###Are they eaten by birds or not? 
chris_insects_master_wide_tran_birdfood <-chris_insects_master %>% filter(BirdFood=="Yes") %>% group_by(unq_tran, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
head(chris_insects_master_wide_tran_birdfood)
#359 Species that are potential birdfood!!! 

chris_insects_master_wide_tran_birdfood_richness<-chris_insects_master_wide_tran_birdfood[,1]
chris_insects_master_wide_tran_birdfood_richness$insect_birdfood_richness<-specnumber(chris_insects_master_wide_tran_birdfood[,-1])
chris_insects_master_wide_tran_birdfood_richness$insect_birdfood_abs_abundance<-rowSums(chris_insects_master_wide_tran_birdfood[,-1],na.rm = TRUE)
head(chris_insects_master_wide_tran_birdfood_richness)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_wide_tran_birdfood_richness, all=TRUE)


chris_insects_master_by_trap_tran_birdfood<-chris_insects_master %>% filter(BirdFood=="Yes")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))
chris_insects_master_by_trap_tran_birdfood<-merge(chris_insects_master_by_trap_tran_birdfood, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_tran_birdfood$insect_birdfood_beat_abundance<-chris_insects_master_by_trap_tran_birdfood$sum_abundance/chris_insects_master_by_trap_tran_birdfood$BeatTime
chris_insects_master_by_trap_tran_birdfood_beat<-chris_insects_master_by_trap_tran_birdfood %>%  filter(Trap=="Beat") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_birdfood_beat_av_abundance = mean(insect_birdfood_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_tran_birdfood$insect_birdfood_pitfall_abundance<-chris_insects_master_by_trap_tran_birdfood$sum_abundance/chris_insects_master_by_trap_tran_birdfood$PitfallsCount
chris_insects_master_by_trap_tran_birdfood_pitfall<-chris_insects_master_by_trap_tran_birdfood %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_birdfood_pitfall_av_abundance = mean(insect_birdfood_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_birdfood_beat, all=TRUE)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_birdfood_pitfall, all=TRUE)

head(chris_insects_master_wide_tran_richness)


### Trophic Group and trophic diversity


### Start with Herbivores
chris_insects_master_wide_tran_herbivore <-chris_insects_master %>% filter(Trophic=="Herbivore") %>% group_by(unq_tran, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
head(chris_insects_master_wide_tran_herbivore)
#359 Species that are potential herbivore!!! 

chris_insects_master_wide_tran_herbivore_richness<-chris_insects_master_wide_tran_herbivore[,1]
chris_insects_master_wide_tran_herbivore_richness$insect_herbivore_richness<-specnumber(chris_insects_master_wide_tran_herbivore[,-1])
chris_insects_master_wide_tran_herbivore_richness$insect_herbivore_abs_abundance<-rowSums(chris_insects_master_wide_tran_herbivore[,-1],na.rm = TRUE)
head(chris_insects_master_wide_tran_herbivore_richness)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_wide_tran_herbivore_richness, all=TRUE)

#now a standardized abundance
chris_insects_master_by_trap_tran_herbivore<-chris_insects_master %>% filter(Trophic=="Herbivore")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_tran_herbivore<-merge(chris_insects_master_by_trap_tran_herbivore, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_tran_herbivore$insect_herbivore_beat_abundance<-chris_insects_master_by_trap_tran_herbivore$sum_abundance/chris_insects_master_by_trap_tran_herbivore$BeatTime
chris_insects_master_by_trap_tran_herbivore_beat<-chris_insects_master_by_trap_tran_herbivore %>%  filter(Trap=="Beat") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_herbivore_beat_av_abundance = mean(insect_herbivore_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_tran_herbivore$insect_herbivore_pitfall_abundance<-chris_insects_master_by_trap_tran_herbivore$sum_abundance/chris_insects_master_by_trap_tran_herbivore$PitfallsCount
chris_insects_master_by_trap_tran_herbivore_pitfall<-chris_insects_master_by_trap_tran_herbivore %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_herbivore_pitfall_av_abundance = mean(insect_herbivore_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_herbivore_beat, all=TRUE)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_herbivore_pitfall, all=TRUE)

head(chris_insects_master_wide_tran_richness)



### Omnivores
chris_insects_master_wide_tran_omnivore <-chris_insects_master %>% filter(Trophic=="Omnivore") %>% group_by(unq_tran, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
head(chris_insects_master_wide_tran_omnivore)
#359 Species that are potential omnivore!!! 

chris_insects_master_wide_tran_omnivore_richness<-chris_insects_master_wide_tran_omnivore[,1]
chris_insects_master_wide_tran_omnivore_richness$insect_omnivore_richness<-specnumber(chris_insects_master_wide_tran_omnivore[,-1])
chris_insects_master_wide_tran_omnivore_richness$insect_omnivore_abs_abundance<-rowSums(chris_insects_master_wide_tran_omnivore[,-1],na.rm = TRUE)
head(chris_insects_master_wide_tran_omnivore_richness)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_wide_tran_omnivore_richness, all=TRUE)

#now a standardized abundance
chris_insects_master_by_trap_tran_omnivore<-chris_insects_master %>% filter(Trophic=="Omnivore")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_tran_omnivore<-merge(chris_insects_master_by_trap_tran_omnivore, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_tran_omnivore$insect_omnivore_beat_abundance<-chris_insects_master_by_trap_tran_omnivore$sum_abundance/chris_insects_master_by_trap_tran_omnivore$BeatTime
chris_insects_master_by_trap_tran_omnivore_beat<-chris_insects_master_by_trap_tran_omnivore %>%  filter(Trap=="Beat") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_omnivore_beat_av_abundance = mean(insect_omnivore_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_tran_omnivore$insect_omnivore_pitfall_abundance<-chris_insects_master_by_trap_tran_omnivore$sum_abundance/chris_insects_master_by_trap_tran_omnivore$PitfallsCount
chris_insects_master_by_trap_tran_omnivore_pitfall<-chris_insects_master_by_trap_tran_omnivore %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_omnivore_pitfall_av_abundance = mean(insect_omnivore_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_omnivore_beat, all=TRUE)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_omnivore_pitfall, all=TRUE)

head(chris_insects_master_wide_tran_richness)


### carnivores
chris_insects_master_wide_tran_carnivore <-chris_insects_master %>% filter(Trophic=="Carnivore") %>% group_by(unq_tran, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
head(chris_insects_master_wide_tran_carnivore)
#359 Species that are potential carnivore!!! 

chris_insects_master_wide_tran_carnivore_richness<-chris_insects_master_wide_tran_carnivore[,1]
chris_insects_master_wide_tran_carnivore_richness$insect_carnivore_richness<-specnumber(chris_insects_master_wide_tran_carnivore[,-1])
chris_insects_master_wide_tran_carnivore_richness$insect_carnivore_abs_abundance<-rowSums(chris_insects_master_wide_tran_carnivore[,-1],na.rm = TRUE)
head(chris_insects_master_wide_tran_carnivore_richness)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_wide_tran_carnivore_richness, all=TRUE)

#now a standardized abundance
chris_insects_master_by_trap_tran_carnivore<-chris_insects_master %>% filter(Trophic=="Carnivore")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_tran_carnivore<-merge(chris_insects_master_by_trap_tran_carnivore, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_tran_carnivore$insect_carnivore_beat_abundance<-chris_insects_master_by_trap_tran_carnivore$sum_abundance/chris_insects_master_by_trap_tran_carnivore$BeatTime
chris_insects_master_by_trap_tran_carnivore_beat<-chris_insects_master_by_trap_tran_carnivore %>%  filter(Trap=="Beat") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_carnivore_beat_av_abundance = mean(insect_carnivore_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_tran_carnivore$insect_carnivore_pitfall_abundance<-chris_insects_master_by_trap_tran_carnivore$sum_abundance/chris_insects_master_by_trap_tran_carnivore$PitfallsCount
chris_insects_master_by_trap_tran_carnivore_pitfall<-chris_insects_master_by_trap_tran_carnivore %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_carnivore_pitfall_av_abundance = mean(insect_carnivore_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_carnivore_beat, all=TRUE)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_carnivore_pitfall, all=TRUE)

head(chris_insects_master_wide_tran_richness)


### detritivores
chris_insects_master_wide_tran_detritivore <-chris_insects_master %>% filter(Trophic=="Detritivore") %>% group_by(unq_tran, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
head(chris_insects_master_wide_tran_detritivore)
#359 Species that are potential detritivore!!! 

chris_insects_master_wide_tran_detritivore_richness<-chris_insects_master_wide_tran_detritivore[,1]
chris_insects_master_wide_tran_detritivore_richness$insect_detritivore_richness<-specnumber(chris_insects_master_wide_tran_detritivore[,-1])
chris_insects_master_wide_tran_detritivore_richness$insect_detritivore_abs_abundance<-rowSums(chris_insects_master_wide_tran_detritivore[,-1],na.rm = TRUE)
head(chris_insects_master_wide_tran_detritivore_richness)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_wide_tran_detritivore_richness, all=TRUE)

#now a standardized abundance
chris_insects_master_by_trap_tran_detritivore<-chris_insects_master %>% filter(Trophic=="Detritivore")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_tran_detritivore<-merge(chris_insects_master_by_trap_tran_detritivore, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_tran_detritivore$insect_detritivore_beat_abundance<-chris_insects_master_by_trap_tran_detritivore$sum_abundance/chris_insects_master_by_trap_tran_detritivore$BeatTime
chris_insects_master_by_trap_tran_detritivore_beat<-chris_insects_master_by_trap_tran_detritivore %>%  filter(Trap=="Beat") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_detritivore_beat_av_abundance = mean(insect_detritivore_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_tran_detritivore$insect_detritivore_pitfall_abundance<-chris_insects_master_by_trap_tran_detritivore$sum_abundance/chris_insects_master_by_trap_tran_detritivore$PitfallsCount
chris_insects_master_by_trap_tran_detritivore_pitfall<-chris_insects_master_by_trap_tran_detritivore %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_detritivore_pitfall_av_abundance = mean(insect_detritivore_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_detritivore_beat, all=TRUE)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_detritivore_pitfall, all=TRUE)

head(chris_insects_master_wide_tran_richness)


### parasites
chris_insects_master_wide_tran_parasite <-chris_insects_master %>% filter(Trophic %in% c("Parasitic","Parasitoid")) %>% group_by(unq_tran, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
head(chris_insects_master_wide_tran_parasite)
#359 Species that are potential parasite!!! 

chris_insects_master_wide_tran_parasite_richness<-chris_insects_master_wide_tran_parasite[,1]
chris_insects_master_wide_tran_parasite_richness$insect_parasite_richness<-specnumber(chris_insects_master_wide_tran_parasite[,-1])
chris_insects_master_wide_tran_parasite_richness$insect_parasite_abs_abundance<-rowSums(chris_insects_master_wide_tran_parasite[,-1],na.rm = TRUE)
head(chris_insects_master_wide_tran_parasite_richness)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_wide_tran_parasite_richness, all=TRUE)

#now a standardized abundance
chris_insects_master_by_trap_tran_parasite<-chris_insects_master %>% filter(Trophic %in% c("Parasitic","Parasitoid"))%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_tran_parasite<-merge(chris_insects_master_by_trap_tran_parasite, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_tran_parasite$insect_parasite_beat_abundance<-chris_insects_master_by_trap_tran_parasite$sum_abundance/chris_insects_master_by_trap_tran_parasite$BeatTime
chris_insects_master_by_trap_tran_parasite_beat<-chris_insects_master_by_trap_tran_parasite %>%  filter(Trap=="Beat") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_parasite_beat_av_abundance = mean(insect_parasite_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_tran_parasite$insect_parasite_pitfall_abundance<-chris_insects_master_by_trap_tran_parasite$sum_abundance/chris_insects_master_by_trap_tran_parasite$PitfallsCount
chris_insects_master_by_trap_tran_parasite_pitfall<-chris_insects_master_by_trap_tran_parasite %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_tran) %>% 
  summarise(insect_parasite_pitfall_av_abundance = mean(insect_parasite_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_parasite_beat, all=TRUE)
chris_insects_master_wide_tran_richness<-merge(chris_insects_master_wide_tran_richness, chris_insects_master_by_trap_tran_parasite_pitfall, all=TRUE)

head(chris_insects_master_wide_tran_richness)

chris_insects_master_wide_tran_richness_0m<-chris_insects_master_wide_tran_richness %>% filter(!stringr::str_detect(unq_tran, 'I'))
head(chris_insects_master_wide_tran_richness_0m)

head(habitat_veg_soil_by_tran_0m)
by_tran_master_0m<-merge(habitat_veg_soil_by_tran_0m, chris_insects_master_wide_tran_richness_0m, by="unq_tran", all=TRUE)
head(by_tran_master_0m)


# Insect isotopes  ---------------------------------------------------------


chris.isotopes<-read.csv("C:Food web idea//Data by person//Chris.data//chris_isotopes_2018.csv", header=TRUE, sep=",")
head(chris.isotopes)
chris.isotopes$s<-as.numeric(chris.isotopes$s)

chris.isotopes.tran<-chris.isotopes %>% group_by(unq_tran, group) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(chris.isotopes.tran)

chris.isotopes.tran_COL<-chris.isotopes.tran %>% filter(group=="insects_COL")
chris.isotopes.tran_CUR<-chris.isotopes.tran %>% filter(group=="insects_CUR")
chris.isotopes.tran_ISO<-chris.isotopes.tran %>% filter(group=="insects_ISO")


head(chris.isotopes.tran_COL)
length(chris.isotopes.tran_COL$unq_tran)
names(chris.isotopes.tran_COL)[3]<-"d13c_beetles"
names(chris.isotopes.tran_COL)[4]<-"d15n_beetles"
names(chris.isotopes.tran_COL)[5]<-"c_beetles"
names(chris.isotopes.tran_COL)[6]<-"n_beetles"
names(chris.isotopes.tran_COL)[7]<-"cn_beetles"
names(chris.isotopes.tran_COL)[8]<-"s_beetles"

head(chris.isotopes.tran_CUR)
length(chris.isotopes.tran_CUR$unq_tran)
names(chris.isotopes.tran_CUR)[3]<-"d13c_weevils"
names(chris.isotopes.tran_CUR)[4]<-"d15n_weevils"
names(chris.isotopes.tran_CUR)[5]<-"c_weevils"
names(chris.isotopes.tran_CUR)[6]<-"n_weevils"
names(chris.isotopes.tran_CUR)[7]<-"cn_weevils"
names(chris.isotopes.tran_CUR)[8]<-"s_weevils"

head(chris.isotopes.tran_ISO)
length(chris.isotopes.tran_ISO$unq_tran)
names(chris.isotopes.tran_ISO)[3]<-"d13c_isopods"
names(chris.isotopes.tran_ISO)[4]<-"d15n_isopods"
names(chris.isotopes.tran_ISO)[5]<-"c_isopods"
names(chris.isotopes.tran_ISO)[6]<-"n_isopods"
names(chris.isotopes.tran_ISO)[7]<-"cn_isopods"
names(chris.isotopes.tran_ISO)[8]<-"s_isopods"


head(by_tran_master_0m)
by_tran_master_0m<-merge(by_tran_master_0m, chris.isotopes.tran_COL[,-2], by="unq_tran", all.x=TRUE)
by_tran_master_0m<-merge(by_tran_master_0m, chris.isotopes.tran_CUR[,-2], by="unq_tran", all.x=TRUE)
by_tran_master_0m<-merge(by_tran_master_0m, chris.isotopes.tran_ISO[,-2], by="unq_tran", all.x=TRUE)


# Tidying up -------------------------------------------------------------


write.csv(by_tran_master_0m, "C:Food web idea//Data by person//Norah.data/by_tran_master_0m.csv", row.names=FALSE)

length(na.omit(by_tran_master_0m$d15n))    
