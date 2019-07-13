setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Food web idea")
#change to Norah if on work computer

# this is the by transect file, but only the 0 m plots


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
library(viridis)
library(cowplot)






# Loading soil data -------------------------------------------------------


#####OWEN
#owen's isotope data by plot
soil_clean<-read.csv("c:Data by person//Owen's data//soil_clean.csv", header=TRUE, sep=",")
head(soil_clean)
length((soil_clean$unq_plot))

#duplicated plots: 
soil_clean[duplicated(soil_clean$unq_plot),]
#let's keep them in for now

names(soil_clean)[6]<-"d13c"
names(soil_clean)[7]<-"d15n"

head(soil_clean)

#Owen's key data
owen_key<-read.csv("c:Data by person//Owen's data//key_mod_2019.csv", header=TRUE, sep=",")
head(owen_key)
length(unique(owen_key$unq_tran))

#Owen's plot-level soil info - moisture, slope etc
hakai_plot<-read.csv("c:Data by person//Owen's data//hakai_plot.csv", header=TRUE, sep=",")
names(hakai_plot)[3]<-"plant.richness"
head(hakai_plot)

owen_key_expanded<-merge(owen_key, hakai_plot, by="unq_plot", all=TRUE)
head(owen_key_expanded)
length(unique(owen_key_expanded$unq_tran))

#Add in the GPS coordinates
owen_coords<-read.csv("c:Data by person//Becky.data//ofwi_tran_coords_mod_3.csv", header=TRUE, sep=",")
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


#put isotope data together with the key
soil_merge<-merge(soil_clean, owen_key_expanded, by="unq_plot")
head(soil_merge)

soil_merge_0m <- soil_merge %>% filter(shore_dist == 0)
soil_merge_0m<-soil_merge_0m[,-c(8,10, 11, 13, 14, 15)]

head(soil_merge_0m)

#write.csv(soil_merge_0m, "C:Data by person\\Norah.data\\soil_merge_0m.csv")


# Adding plant cover and richnes sshoreline -----------------------------------------

#this loads data from "Habitation data" R script

longform_plant_percentcover<-read.csv("C:Data by person//Kalina.data/Deb_Owen_veg_combined_complete_filled.csv", header=TRUE, sep=",")
longform_plant_percentcover<-longform_plant_percentcover[,-c(1)]
head(longform_plant_percentcover)

#longform_plant_percentcover$unq_tran<-strtrim(longform_plant_percentcover$unq_tran, 5)

longform_plant_percentcover_owen <- longform_plant_percentcover %>% filter(person=="Owen")
longform_plant_percentcover_owen_0m<-longform_plant_percentcover_owen %>% filter(shore_dist=="0")


longform_plant_percentcover2_tran_0m <- longform_plant_percentcover_owen_0m%>% 
  group_by(unq_tran,species) %>% summarise(cover_mean = mean(cover, na.rm=TRUE)) %>% 
  spread(species, cover_mean)%>%  replace(is.na(.), 0)

head(longform_plant_percentcover2_tran_0m)

longform_plant_percentcover_owen_0m_shrub<- longform_plant_percentcover_owen_0m %>% filter(herb_shrub=="shrub")
longform_plant_percentcover_owen_0m_herb<- longform_plant_percentcover_owen_0m %>% filter(herb_shrub=="herb")

longform_plant_percentcover2_tran_0m_shrub <- longform_plant_percentcover_owen_0m_shrub%>% 
  group_by(unq_tran,species) %>% summarise(cover_mean = mean(cover, na.rm=TRUE)) %>% 
  spread(species, cover_mean)%>%  replace(is.na(.), 0)

longform_plant_percentcover2_tran_0m_herb <- longform_plant_percentcover_owen_0m_herb%>% 
  group_by(unq_tran,species) %>% summarise(cover_mean = mean(cover, na.rm=TRUE)) %>% 
  spread(species, cover_mean)%>%  replace(is.na(.), 0)



longform_plant_percentcover_species_tran_0m<-longform_plant_percentcover2_tran_0m
head(longform_plant_percentcover_species_tran_0m)

which( colnames(longform_plant_percentcover2_tran_0m)=="gash" )
which( colnames(longform_plant_percentcover2_tran_0m)=="midi" )


longform_plant_percentcover3_tran_0m<-longform_plant_percentcover2_tran_0m[,c(1,31,43)]
head(longform_plant_percentcover3_tran_0m)
longform_plant_percentcover3_tran_0m$plant_richness<-specnumber(longform_plant_percentcover_species_tran_0m[,-1])
longform_plant_percentcover3_tran_0m$plant_shannon.diversity<-diversity(longform_plant_percentcover_species_tran_0m[,-1], index="shannon")
longform_plant_percentcover3_tran_0m$plant_evenness<-longform_plant_percentcover3_tran_0m$plant_shannon.diversity/(log(longform_plant_percentcover3_tran_0m$plant_richness))
longform_plant_percentcover3_tran_0m$total_cover<-rowSums(longform_plant_percentcover_species_tran_0m[,-1], na.rm=TRUE)

longform_plant_percentcover3_tran_0m$shrub_richness<-specnumber(longform_plant_percentcover2_tran_0m_shrub[,-1])
longform_plant_percentcover3_tran_0m$shrub_cover<-rowSums(longform_plant_percentcover2_tran_0m_shrub[,-1], na.rm=TRUE)
longform_plant_percentcover3_tran_0m$herb_richness<-specnumber(longform_plant_percentcover2_tran_0m_herb[,-1])
longform_plant_percentcover3_tran_0m$herb_cover<-rowSums(longform_plant_percentcover2_tran_0m_herb[,-1], na.rm=TRUE)


longform_plant_percentcover2_tran_0m_shrub$unq_tran[!(longform_plant_percentcover3_tran_0m$unq_tran%in% longform_plant_percentcover2_tran_0m_shrub$unq_tran)]

longform_plant_percentcover3_tran_0m$unq_tran<-strtrim(longform_plant_percentcover3_tran_0m$unq_tran, 5)






habitat_veg_soil_by_tran_0m<-merge(soil_merge_0m, longform_plant_percentcover3_tran_0m, by.x="unq_tran", all=TRUE)
head(habitat_veg_soil_by_tran_0m)


# Vegetation isotopes -----------------------------------------------------


owen.veg_tran<-read.csv("C:Data by person\\Owen's data\\foliar_clean_sorted_merge_meta.csv")
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
# 
# chris_beeetles_2015<-read.csv("c:Data by person//Chris.data//ce_observation2015.csv", header=TRUE, sep=",")
# chris_beeetles_2016<-read.csv("c:Data by person//Chris.data//ce_observation2016.csv", header=TRUE, sep=",")
# chris_beeetles_2017<-read.csv("c:Data by person//Chris.data//ce_observation2017.csv", header=TRUE, sep=",")
# 
# 
# str(chris_beeetles_2015)
# str(chris_beeetles_2016)
# str(chris_beeetles_2017)
# chris_beetles<-rbind(chris_beeetles_2015[,-6], chris_beeetles_2016[,-6], chris_beeetles_2017[,-6])
# head(chris_beetles)
# 
# chris_beetles$unq_tran<-gsub('.{1}$', '',chris_beetles$Trapline )
# 
# chris_beetles_wide <-chris_beetles %>% group_by(unq_tran, Trap, SpeciesID) %>% 
#   summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
#   group_by(unq_tran, SpeciesID) %>% 
#   summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
#   spread( SpeciesID, sum_abundance) %>% 
#   replace(is.na(.), 0) 
# head(chris_beetles_wide)
# 
# chris_beetles_wide_richness<-chris_beetles_wide[,1]
# chris_beetles_wide_richness$beetles_richness<-specnumber(chris_beetles_wide[,-1])
# chris_beetles_wide_richness$beetles_abundance<-rowSums(chris_beetles_wide[,-1],na.rm = TRUE)
# 
# head(chris_beetles_wide_richness)

###collembola
chris_collembola<-read.csv("c:Data by person//Chris.data//collembola_ey.csv", header=TRUE, sep=",")
head(chris_collembola)


chris_collembola$Island.Number<-sprintf("%02d", chris_collembola$Island.Number)
chris_collembola$unq_isl<- paste(chris_collembola$Island,chris_collembola$Island.Number)
chris_collembola$unq_isl<-gsub(" ", "", chris_collembola$unq_isl, fixed = TRUE)
chris_collembola$Direction<-strtrim(chris_collembola$Direction, 1)
chris_collembola$unq_tran<- paste(chris_collembola$unq_isl,chris_collembola$Direction)
chris_collembola$unq_tran<-gsub(" ", "", chris_collembola$unq_tran, fixed = TRUE)


#incorporate Direction if need to have unq_tran... but if want to just get per island average across 5 points
#also trap type .. maybe just combine beat and pitfall = SUM
#okay here we collapsed transects (did mean) and then summed across bet and pitfall ... not sure which is the bvest way to do it
#It's fine for richness but might need to fine tune for abudnance

chris_collembola_wide_tran <-chris_collembola %>% group_by(unq_tran, Trap.Type, Identification) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_tran, Identification) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Identification, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_collembola_wide_tran)
chris_collembola_wide_tran<-chris_collembola_wide_tran[,-c(2,3)]

chris_collembola_wide_tran_richness<-chris_collembola_wide_tran[,1]
chris_collembola_wide_tran_richness$collembola_richness<-specnumber(chris_collembola_wide_tran[,-1])
chris_collembola_wide_tran_richness$collembola_abundance<-rowSums(chris_collembola_wide_tran[,-1],na.rm = TRUE)
head(chris_collembola_wide_tran_richness)


##hymenoptera
chris_hymenoptera<-read.csv("c:Data by person//Chris.data//hymenoptera_ey.csv", header=TRUE, sep=",")
head(chris_hymenoptera)


chris_hymenoptera$Island.Number<-sprintf("%02d", chris_hymenoptera$Island.Number)
chris_hymenoptera$unq_isl<- paste(chris_hymenoptera$Island,chris_hymenoptera$Island.Number)
chris_hymenoptera$unq_isl<-gsub(" ", "", chris_hymenoptera$unq_isl, fixed = TRUE)
chris_hymenoptera$Direction<-strtrim(chris_hymenoptera$Direction, 1)
chris_hymenoptera$unq_tran<- paste(chris_hymenoptera$unq_isl,chris_hymenoptera$Direction)
chris_hymenoptera$unq_tran<-gsub(" ", "", chris_hymenoptera$unq_tran, fixed = TRUE)


chris_hymenoptera_wide_tran <-chris_hymenoptera %>% group_by(unq_tran, Trap.Type, Identification) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_tran, Identification) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Identification, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_hymenoptera_wide_tran)

chris_hymenoptera_wide_tran_richness<-chris_hymenoptera_wide_tran[,1]
chris_hymenoptera_wide_tran_richness$hymenoptera_richness<-specnumber(chris_hymenoptera_wide_tran[,-1])
chris_hymenoptera_wide_tran_richness$hymenoptera_abundance<-rowSums(chris_hymenoptera_wide_tran[,-1],na.rm = TRUE)
head(chris_hymenoptera_wide_tran_richness)

##diptera
chris_diptera<-read.csv("c:Data by person//Chris.data//diptera_ey.csv", header=TRUE, sep=",")
head(chris_diptera)

chris_diptera$Island.Number<-sprintf("%02d", chris_diptera$Island.Number)
chris_diptera$unq_isl<- paste(chris_diptera$Island,chris_diptera$Island.Number)
chris_diptera$unq_isl<-gsub(" ", "", chris_diptera$unq_isl, fixed = TRUE)
chris_diptera$Direction<-strtrim(chris_diptera$Direction, 1)
chris_diptera$unq_tran<- paste(chris_diptera$unq_isl,chris_diptera$Direction)
chris_diptera$unq_tran<-gsub(" ", "", chris_diptera$unq_tran, fixed = TRUE)

chris_diptera_wide_tran <-chris_diptera %>% group_by(unq_tran, Trap.Type, Identification) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_tran, Identification) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Identification, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_diptera_wide_tran)

chris_diptera_wide_tran<-chris_diptera_wide_tran[,-c(2,3)]

chris_diptera_wide_tran_richness<-chris_diptera_wide_tran[,1]
chris_diptera_wide_tran_richness$diptera_richness<-specnumber(chris_diptera_wide_tran[,-1])
chris_diptera_wide_tran_richness$diptera_abundance<-rowSums(chris_diptera_wide_tran[,-1],na.rm = TRUE)
head(chris_diptera_wide_tran_richness)


##gastropoda
chris_gastropoda<-read.csv("c:Data by person//Chris.data//gastropoda_ey.csv", header=TRUE, sep=",")
head(chris_gastropoda)

chris_gastropoda$Island.Number<-sprintf("%02d", chris_gastropoda$IslandNumber)
chris_gastropoda$unq_isl<- paste(chris_gastropoda$Island,chris_gastropoda$IslandNumber)
chris_gastropoda$unq_isl<-gsub(" ", "", chris_gastropoda$unq_isl, fixed = TRUE)
chris_gastropoda$Direction<-strtrim(chris_gastropoda$Direction, 1)
chris_gastropoda$unq_tran<- paste(chris_gastropoda$unq_isl,chris_gastropoda$Direction)
chris_gastropoda$unq_tran<-gsub(" ", "", chris_gastropoda$unq_tran, fixed = TRUE)

chris_gastropoda_wide_tran <-chris_gastropoda %>% group_by(unq_tran, TrapType, Identification) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_tran, Identification) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Identification, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_gastropoda_wide_tran)
chris_gastropoda_wide_tran<-chris_gastropoda_wide_tran[,-c(2)]


chris_gastropoda_wide_tran_richness<-chris_gastropoda_wide_tran[,1]
chris_gastropoda_wide_tran_richness$gastropoda_richness<-specnumber(chris_gastropoda_wide_tran[,-1])
chris_gastropoda_wide_tran_richness$gastropoda_abundance<-rowSums(chris_gastropoda_wide_tran[,-1],na.rm = TRUE)
head(chris_gastropoda_wide_tran_richness)

##spiders
chris_spiders<-read.csv("c:Data by person//Chris.data//spiders_ey.csv", header=TRUE, sep=",")
head(chris_spiders)

chris_spiders$Island.Number<-sprintf("%02d", chris_spiders$Island.Number)
chris_spiders$unq_isl<- paste(chris_spiders$Island,chris_spiders$Island.Number)
chris_spiders$unq_isl<-gsub(" ", "", chris_spiders$unq_isl, fixed = TRUE)
chris_spiders$Direction<-strtrim(chris_spiders$Direction, 1)
chris_spiders$unq_tran<- paste(chris_spiders$unq_isl,chris_spiders$Direction)
chris_spiders$unq_tran<-gsub(" ", "", chris_spiders$unq_tran, fixed = TRUE)

chris_spiders_wide_tran <-chris_spiders %>% group_by(unq_tran, Trap.Type, Identification) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_tran, Identification) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Identification, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_spiders_wide_tran)
chris_spiders_wide_tran<-chris_spiders_wide_tran[,-c(2)]


chris_spiders_wide_tran_richness<-chris_spiders_wide_tran[,1]
chris_spiders_wide_tran_richness$spiders_richness<-specnumber(chris_spiders_wide_tran[,-1])
chris_spiders_wide_tran_richness$spiders_abundance<-rowSums(chris_spiders_wide_tran[,-1],na.rm = TRUE)
head(chris_spiders_wide_tran_richness)

#myriapoda

chris_myriapod<-read.csv("c:Data by person//Chris.data//myriapoda_ey.csv", header=TRUE, sep=",")
head(chris_myriapod)

chris_myriapod$Island.Number<-sprintf("%02d", chris_myriapod$Island.Number)
chris_myriapod$unq_isl<- paste(chris_myriapod$Island,chris_myriapod$Island.Number)
chris_myriapod$unq_isl<-gsub(" ", "", chris_myriapod$unq_isl, fixed = TRUE)
chris_myriapod$Direction<-strtrim(chris_myriapod$Direction, 1)
chris_myriapod$unq_tran<- paste(chris_myriapod$unq_isl,chris_myriapod$Direction)
chris_myriapod$unq_tran<-gsub(" ", "", chris_myriapod$unq_tran, fixed = TRUE)


chris_crustacea <- chris_myriapod %>%  filter(Subphylum=="Crustacea")
head(chris_crustacea)
chris_myriapoda <- chris_myriapod %>%  filter(Subphylum=="Myriapoda")

#
chris_crustacea_wide_tran <-chris_crustacea %>% group_by(unq_tran, Trap.Type, Genus.Species) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_tran,Genus.Species) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Genus.Species, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_crustacea_wide_tran)
chris_crustacea_wide_tran<-chris_crustacea_wide_tran[,-c(2)]

chris_crustacea_wide_tran_richness<-chris_crustacea_wide_tran[,1]
chris_crustacea_wide_tran_richness$crustacea_richness<-specnumber(chris_crustacea_wide_tran[,-1])
chris_crustacea_wide_tran_richness$crustacea_abundance<-rowSums(chris_crustacea_wide_tran[,-1],na.rm = TRUE)
head(chris_crustacea_wide_tran_richness)


#myriapoda
chris_myriapod_wide_tran <-chris_myriapoda %>% group_by(unq_tran, Trap.Type, Genus.Species) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_tran,Genus.Species) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Genus.Species, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_myriapod_wide_tran)
chris_myriapod_wide_tran<-chris_myriapod_wide_tran[,-c(2)]

chris_myriapod_wide_tran_richness<-chris_myriapod_wide_tran[,1]
chris_myriapod_wide_tran_richness$myriapod_richness<-specnumber(chris_myriapod_wide_tran[,-1])
chris_myriapod_wide_tran_richness$myriapod_abundance<-rowSums(chris_myriapod_wide_tran[,-1],na.rm = TRUE)
head(chris_myriapod_wide_tran_richness)


#Otherinsects
chris_miscinsects<-read.csv("c:Data by person//Chris.data//misc_insects_ey.csv", header=TRUE, sep=",")
head(chris_miscinsects)

chris_miscinsects$Island.Number<-sprintf("%02d", chris_miscinsects$Island.Number)
chris_miscinsects$unq_isl<- paste(chris_miscinsects$Island,chris_miscinsects$Island.Number)
chris_miscinsects$unq_isl<-gsub(" ", "", chris_miscinsects$unq_isl, fixed = TRUE)
chris_miscinsects$Direction<-strtrim(chris_miscinsects$Direction, 1)
chris_miscinsects$unq_tran<- paste(chris_miscinsects$unq_isl,chris_miscinsects$Direction)
chris_miscinsects$unq_tran<-gsub(" ", "", chris_miscinsects$unq_tran, fixed = TRUE)


chris_miscinsects_wide_tran <-chris_miscinsects %>% group_by(unq_tran, Trap.Type, Identification) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_tran, Identification) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Identification, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_miscinsects_wide_tran)
chris_miscinsects_wide_tran<-chris_miscinsects_wide_tran[,-2]

chris_miscinsects_wide_tran_richness<-chris_miscinsects_wide_tran[,1]
chris_miscinsects_wide_tran_richness$miscinsects_richness<-specnumber(chris_miscinsects_wide_tran[,-1])
chris_miscinsects_wide_tran_richness$miscinsects_abundance<-rowSums(chris_miscinsects_wide_tran[,-1],na.rm = TRUE)
head(chris_miscinsects_wide_tran_richness)

#merge insects

chris_insects_tran<-merge(chris_miscinsects_wide_tran_richness, chris_collembola_wide_tran_richness, by.y ="unq_tran", all=TRUE)
chris_insects_tran<-merge(chris_insects_tran, chris_diptera_wide_tran_richness, by.x="unq_tran", all=TRUE)
chris_insects_tran<-merge(chris_insects_tran, chris_spiders_wide_tran_richness, by.x="unq_tran", all=TRUE)
chris_insects_tran<-merge(chris_insects_tran, chris_gastropoda_wide_tran_richness, by.x="unq_tran", all=TRUE)
chris_insects_tran<-merge(chris_insects_tran, chris_hymenoptera_wide_tran_richness, by.x="unq_tran", all=TRUE)
chris_insects_tran<-merge(chris_insects_tran, chris_myriapod_wide_tran_richness, by.x="unq_tran", all=TRUE)
chris_insects_tran<-merge(chris_insects_tran, chris_crustacea_wide_tran_richness, by.x="unq_tran", all=TRUE)
#chris_insects_tran<-merge(chris_insects_tran, chris_beetles_wide_richness, by.x="unq_tran", all=TRUE)


##getting evenness
chris_insects_tran_wide<-merge(chris_miscinsects_wide_tran, chris_collembola_wide_tran, by.y ="unq_tran", all=TRUE)
chris_insects_tran_wide<-merge(chris_insects_tran_wide, chris_hymenoptera_wide_tran, by.x="unq_tran", all=TRUE)
chris_insects_tran_wide<-merge(chris_insects_tran_wide, chris_diptera_wide_tran, by.x="unq_tran", all=TRUE)
chris_insects_tran_wide<-merge(chris_insects_tran_wide, chris_spiders_wide_tran, by.x="unq_tran", all=TRUE)
chris_insects_tran_wide<-merge(chris_insects_tran_wide, chris_gastropoda_wide_tran, by.x="unq_tran", all=TRUE)
chris_insects_tran_wide<-merge(chris_insects_tran_wide, chris_myriapod_wide_tran, by.x="unq_tran", all=TRUE)
chris_insects_tran_wide<-merge(chris_insects_tran_wide, chris_crustacea_wide_tran, by.x="unq_tran", all=TRUE)
#chris_insects_tran_wide<-merge(chris_insects_tran_wide, chris_beetles_wide, by.x="unq_tran", all=TRUE)
head(chris_insects_tran_wide)
chris_insects_tran_wide[is.na(chris_insects_tran_wide)] <- 0

chris_insects_tran$insect_richness<-specnumber(chris_insects_tran_wide[,-1])
chris_insects_tran$insect_diversity<-diversity(chris_insects_tran_wide[,-1],index="shannon")
chris_insects_tran$insect_evenness<-chris_insects_tran$insect_diversity/(log(chris_insects_tran$insect_richness))
chris_insects_tran$insect_abundance<-rowSums(chris_insects_tran_wide[,-1],na.rm = TRUE)

head(chris_insects_tran)



chris_insects_tran[is.na(chris_insects_tran)] <- 0
length(chris_insects_tran$unq_tran)

head(habitat_veg_soil_by_tran_0m)
by_tran_master_0m<-merge(habitat_veg_soil_by_tran_0m, chris_insects_tran, by="unq_tran", all.x=TRUE)
head(by_tran_master_0m)

###all.x is true so that I don't get the interior plots... 

# Insect isotopes  ---------------------------------------------------------


chris.isotopes<-read.csv("c:Data by person//Chris.data//chris_isotopes_2018.csv", header=TRUE, sep=",")
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

head(by_tran_master_0m)
write.csv(by_tran_master_0m, "C:Data by person//Norah.data/by_tran_master_0m.csv")
