library(here)
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
#install.packages("forcats")
library(forcats)



#This file should be summarizing by plot when possible and skipping the transect phase. 
#Not possible for Becky's data. 

# Loading soil data -------------------------------------------------------

##### DEB
#Deb's soil data and Deb's shore dist
i.soil.all<-read.csv("C:Food web idea//Data by person//Deb.data//i-soil-all.csv")
#this is isotopes

shoredist.deb<-read.csv("C:Food web idea//Data by person//Deb.data//shoredist.csv")
#this is the point count to distance to shore data

pointcount.gps<-read.csv("C:Food web idea//Data by person//Deb.data//pointcounts.csv")
#head(pointcount.gps)
pointcount.gps$pcid<-gsub(" ", "", pointcount.gps$pcid, fixed = TRUE)
pointcount.gps<-pointcount.gps[,c(3,16,17)]
pointcount.gps<-pointcount.gps[!duplicated(pointcount.gps$pcid),]
#sometimes taken twice...  

shoredist.deb<-merge(shoredist.deb, pointcount.gps, by="pcid", all=TRUE)
#head(shoredist.deb)
length(i.soil.all$sample.id)

soil.deb<-merge(i.soil.all, shoredist.deb, by="pcid", all=TRUE)
head(soil.deb)
names(soil.deb)[16]<-"shore_dist"
names(soil.deb)[11]<-"unq_isl"
names(soil.deb)[4]<-"c"
names(soil.deb)[5]<-"n"
names(soil.deb)[6]<-"s"
names(soil.deb)[7]<-"cn"
names(soil.deb)[1]<-"unq_plot"

length(unique(soil.deb$unq_plot))
#301 plots for deb
head(soil.deb)


#####OWEN
#owen's soil isotope data by plot
soil_clean<-read.csv("C:Food web idea//Data by person//Owen's data//soil_clean.csv", header=TRUE, sep=",")
head(soil_clean)
length(unique(soil_clean$unq_plot))
#682

#duplicated plots: 
soil_clean[duplicated(soil_clean$unq_plot),]
#let's keep them in for now

names(soil_clean)[6]<-"d13c"
names(soil_clean)[7]<-"d15n"

head(soil_clean)

#Owen's key data
owen_key<-read.csv("C:Food web idea//Data by person//Owen's data//key_mod_2019.csv", header=TRUE, sep=",")

#Owen's plot-level soil info - moisture, slope etc
hakai_plot<-read.csv("C:Food web idea//Data by person//Owen's data//hakai_plot.csv", header=TRUE, sep=",")
names(hakai_plot)[3]<-"plant.richness"
head(hakai_plot)

owen_key_expanded<-merge(owen_key, hakai_plot, by="unq_plot", all=TRUE)
head(owen_key_expanded)

#Add in the GPS coordinates
owen_coords<-read.csv("C:Food web idea//Data by person//Owen's data//100Islands_Fitzpatrick_plot.csv", header=TRUE, sep=",")

head(owen_coords)
owen_coords<-owen_coords[,c(1:3)]
head(owen_coords)

owen_key_expanded<-merge(owen_key_expanded, owen_coords, by="unq_plot", all=TRUE)
head(owen_key_expanded)


#put isotope data together with the key
soil_merge<-merge(soil_clean, owen_key_expanded, by="unq_plot")
head(soil_merge)
soil_merge$easting[soil_merge$unq_plot=="MM09WE1"]<-539907
soil_merge$northing[soil_merge$unq_plot=="MM09WE1"]<-5766077
soil_merge$easting[soil_merge$unq_plot=="MM09N1"]<-539916
soil_merge$northing[soil_merge$unq_plot=="MM09N1"]<-5766116
soil_merge$easting[soil_merge$unq_plot=="MM11E1"]<-540664
soil_merge$northing[soil_merge$unq_plot=="MM11E1"]<-5767468
soil_merge$easting[soil_merge$unq_plot=="MM11S1"]<-540619
soil_merge$northing[soil_merge$unq_plot=="MM11S1"]<-5767388




# Combining Owen and Deb's soil data --------------------------------------
col_names_selected<-c("unq_plot" ,
                      "unq_isl" ,
                      "shore_dist" ,
                      "d13c" ,
                      "d15n" ,
                      "c" ,
                      "n" ,
                      "s" ,
                      "cn" , 
                      "node" ,
                      "easting" ,
                      "northing" )

soil_owen_deb<-rbind(soil_merge[,colnames(soil_merge) %in% col_names_selected], soil.deb[,colnames(soil.deb) %in% col_names_selected])
head(soil_owen_deb)

length(unique(soil_owen_deb$unq_plot))
#983 = 301 + 682

### add in d34s here
soil_s<-read.csv("C:Food web idea/Data by person/Norah.data/soil_s.csv")
head(soil_s)

soil_owen_deb<-merge(soil_owen_deb, soil_s[,-3], by="unq_plot", all=TRUE)
head(soil_owen_deb)



# Adding plant cover and richness -----------------------------------------

#this loads data from "Habitation data" R script

longform_plant_percentcover_plot<-read.csv("C:Food web idea//Data by person//Kalina.data/Deb_Owen_veg_combined_complete_filled.csv", header=TRUE, sep=",")
head(longform_plant_percentcover)

longform_plant_percentcover_plot$unq_isl<-fct_explicit_na(longform_plant_percentcover_plot$unq_isl)

longform_plant_percentcover2_plot <- longform_plant_percentcover_plot[,c(1:7)]%>% 
  group_by(unq_plot,species) %>% summarise(cover_mean = mean(cover, na.rm=TRUE)) %>% 
  spread(species, cover_mean)%>%  replace(is.na(.), 0)

longform_plant_percentcover_shrub_plot<- longform_plant_percentcover_plot %>% filter(herb_shrub=="shrub")
longform_plant_percentcover_herb_plot<- longform_plant_percentcover_plot %>% filter(herb_shrub=="herb")

longform_plant_percentcover2_plot_shrub <- longform_plant_percentcover_shrub_plot[,c(1:7)] %>% 
  group_by(unq_plot,species) %>% 
  summarise(cover_mean = mean(cover, na.rm=TRUE)) %>% 
  spread(species, cover_mean)%>%  replace(is.na(.), 0)

longform_plant_percentcover2_plot_herb <- longform_plant_percentcover_herb_plot[,c(1:7)] %>% 
  group_by(unq_plot,species) %>% 
  summarise(cover_mean = mean(cover, na.rm=TRUE)) %>% 
  spread(species, cover_mean)%>%  replace(is.na(.), 0)

longform_plant_percentcover_species_plot<-longform_plant_percentcover2_plot
#head(longform_plant_percentcover_species_isl)

which( colnames(longform_plant_percentcover2_plot)=="gash" )
which( colnames(longform_plant_percentcover2_plot)=="midi" )


longform_plant_percentcover3_plot<-longform_plant_percentcover2_plot[,c(1,37,52)]
#head(longform_plant_percentcover3_plot)
longform_plant_percentcover3_plot$plant_richness<-specnumber(longform_plant_percentcover_species_plot[,-c(1)])
longform_plant_percentcover3_plot$plant_shannon.diversity<-diversity(longform_plant_percentcover_species_plot[,-c(1)], index="shannon")
longform_plant_percentcover3_plot$plant_evenness<-longform_plant_percentcover3_plot$plant_shannon.diversity/(log(longform_plant_percentcover3_plot$plant_richness))
longform_plant_percentcover3_plot$total_cover<-rowSums(longform_plant_percentcover_species_plot[,-c(1)], na.rm=TRUE)

longform_plant_percentcover3_plot$shrub_richness<-specnumber(longform_plant_percentcover2_plot_shrub[,-c(1)])
longform_plant_percentcover3_plot$shrub_cover<-rowSums(longform_plant_percentcover2_plot_shrub[,-c(1)], na.rm=TRUE)
longform_plant_percentcover3_plot$herb_richness<-specnumber(longform_plant_percentcover2_plot_herb[,-c(1)])
longform_plant_percentcover3_plot$herb_cover<-rowSums(longform_plant_percentcover2_plot_herb[,-c(1)], na.rm=TRUE)

head(longform_plant_percentcover3_plot)


#head(soil_merge)
head(soil_owen_deb)

veg_soil_by_plot<-merge(soil_owen_deb, longform_plant_percentcover3_plot, by="unq_plot", all=TRUE)
head(veg_soil_by_plot)

veg_soil_by_plot$unq_isl<-str_sub(veg_soil_by_plot$unq_plot, end=4)


# Chris insects -----------------------------------------------------------

#new data July 2019 
chris_insects_master<-read.csv("C:Food web idea//Data by person//Chris.data//invert_id_abundance_v4.csv", header=TRUE, sep=",")
head(chris_insects_master)
chris_insects_master$unq_isl<-strtrim(chris_insects_master$Trapline, 4)
chris_insects_master$unq_tran<-strtrim(chris_insects_master$Trapline, 5)
chris_insects_master$plot<-substr(chris_insects_master$Trapline, 5, 5)
chris_insects_master$unq_plot<-paste(chris_insects_master$unq_tran,1, sep="")
head(chris_insects_master)

# I still want to divde by groups BUT I will do all insects as a whole first then break into groups

# For each island there are 4 transects and one interior plot
#My understanding is that there is only one pitfall or one beat per "transect".. if that's not true I will need to change script. 

chris_insects_master_wide_plot <-chris_insects_master %>% group_by(unq_plot, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
head(chris_insects_master_wide_plot)
#870 Species!!! 

chris_insects_master_wide_plot_richness<-chris_insects_master_wide_plot[,1]
chris_insects_master_wide_plot_richness$insect_richness<-specnumber(chris_insects_master_wide_plot[,-1])
chris_insects_master_wide_plot_richness$insect_abs_abundance<-rowSums(chris_insects_master_wide_plot[,-1],na.rm = TRUE)
chris_insects_master_wide_plot_richness$insect_diversity<-diversity(chris_insects_master_wide_plot[,-1],index="shannon")
chris_insects_master_wide_plot_richness$insect_evenness<-chris_insects_master_wide_plot_richness$insect_diversity/(log(chris_insects_master_wide_plot_richness$insect_richness))
head(chris_insects_master_wide_plot_richness)


#now a more standardized abundance measure per beat or pitfall trap on the island
chris_trapline_data<-read.csv("C:Food web idea//Data by person//Chris.data//trapline_data.csv", header=TRUE, sep=",")
chris_trapline_data$unq_isl<-strtrim(chris_trapline_data$Trapline, 4)
chris_trapline_data$unq_tran<-strtrim(chris_trapline_data$Trapline, 5)
chris_trapline_data$plot<-substr(chris_trapline_data$Trapline, 5, 5)
chris_trapline_data$unq_plot<-paste(chris_trapline_data$unq_tran,1, sep="")
head(chris_trapline_data)

#sum number of insects found on that trapline/traptype
chris_insects_master_by_trap<-chris_insects_master %>% group_by(unq_plot, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_plot<-merge(chris_insects_master_by_trap, chris_trapline_data[,-c(2:15)], by="unq_plot")

head(chris_insects_master_by_trap_plot)

chris_insects_master_by_trap_plot$insect_beat_abundance<-chris_insects_master_by_trap_plot$sum_abundance/chris_insects_master_by_trap_plot$BeatTime

chris_insects_master_by_trap_plot_beat<-chris_insects_master_by_trap_plot %>%  filter(Trap=="Beat") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_beat_av_abundance = mean(insect_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_plot$insect_pitfall_abundance<-chris_insects_master_by_trap_plot$sum_abundance/chris_insects_master_by_trap_plot$PitfallsCount
chris_insects_master_by_trap_plot_pitfall<-chris_insects_master_by_trap_plot %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_pitfall_av_abundance = mean(insect_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_beat, all=TRUE)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_pitfall, all=TRUE)

head(chris_insects_master_wide_plot_richness)

#########now the same measures but for a few different categories. 
#head(chris_insects_master)

###Are they eaten by birds or not? 
chris_insects_master_wide_plot_birdfood <-chris_insects_master %>% filter(BirdFood=="Yes") %>% group_by(unq_plot, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
#head(chris_insects_master_wide_plot_birdfood)
#359 Species that are potential birdfood!!! 

chris_insects_master_wide_plot_birdfood_richness<-chris_insects_master_wide_plot_birdfood[,1]
chris_insects_master_wide_plot_birdfood_richness$insect_birdfood_richness<-specnumber(chris_insects_master_wide_plot_birdfood[,-1])
chris_insects_master_wide_plot_birdfood_richness$insect_birdfood_abs_abundance<-rowSums(chris_insects_master_wide_plot_birdfood[,-1],na.rm = TRUE)
#head(chris_insects_master_wide_plot_birdfood_richness)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_wide_plot_birdfood_richness, all=TRUE)
#head(chris_insects_master_wide_plot_richness)

chris_insects_master_by_trap_plot_birdfood<-chris_insects_master %>% filter(BirdFood=="Yes")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))
chris_insects_master_by_trap_plot_birdfood<-merge(chris_insects_master_by_trap_plot_birdfood, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_plot_birdfood$insect_birdfood_beat_abundance<-chris_insects_master_by_trap_plot_birdfood$sum_abundance/chris_insects_master_by_trap_plot_birdfood$BeatTime
chris_insects_master_by_trap_plot_birdfood_beat<-chris_insects_master_by_trap_plot_birdfood %>%  filter(Trap=="Beat") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_birdfood_beat_av_abundance = mean(insect_birdfood_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_plot_birdfood$insect_birdfood_pitfall_abundance<-chris_insects_master_by_trap_plot_birdfood$sum_abundance/chris_insects_master_by_trap_plot_birdfood$PitfallsCount
chris_insects_master_by_trap_plot_birdfood_pitfall<-chris_insects_master_by_trap_plot_birdfood %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_birdfood_pitfall_av_abundance = mean(insect_birdfood_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_birdfood_beat, all=TRUE)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_birdfood_pitfall, all=TRUE)

#head(chris_insects_master_wide_plot_richness)
### Trophic Group and trophic diversity
### Start with Herbivores
chris_insects_master_wide_plot_herbivore <-chris_insects_master %>% filter(Trophic=="Herbivore") %>% group_by(unq_plot, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
#head(chris_insects_master_wide_plot_herbivore)
#359 Species that are potential herbivore!!! 

chris_insects_master_wide_plot_herbivore_richness<-chris_insects_master_wide_plot_herbivore[,1]
chris_insects_master_wide_plot_herbivore_richness$insect_herbivore_richness<-specnumber(chris_insects_master_wide_plot_herbivore[,-1])
chris_insects_master_wide_plot_herbivore_richness$insect_herbivore_abs_abundance<-rowSums(chris_insects_master_wide_plot_herbivore[,-1],na.rm = TRUE)
#head(chris_insects_master_wide_plot_herbivore_richness)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_wide_plot_herbivore_richness, all=TRUE)
#head(chris_insects_master_wide_plot_richness)

#now a standardized abundance
chris_insects_master_by_trap_plot_herbivore<-chris_insects_master %>% filter(Trophic=="Herbivore")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_plot_herbivore<-merge(chris_insects_master_by_trap_plot_herbivore, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_plot_herbivore$insect_herbivore_beat_abundance<-chris_insects_master_by_trap_plot_herbivore$sum_abundance/chris_insects_master_by_trap_plot_herbivore$BeatTime
chris_insects_master_by_trap_plot_herbivore_beat<-chris_insects_master_by_trap_plot_herbivore %>%  filter(Trap=="Beat") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_herbivore_beat_av_abundance = mean(insect_herbivore_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_plot_herbivore$insect_herbivore_pitfall_abundance<-chris_insects_master_by_trap_plot_herbivore$sum_abundance/chris_insects_master_by_trap_plot_herbivore$PitfallsCount
chris_insects_master_by_trap_plot_herbivore_pitfall<-chris_insects_master_by_trap_plot_herbivore %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_herbivore_pitfall_av_abundance = mean(insect_herbivore_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_herbivore_beat, all=TRUE)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_herbivore_pitfall, all=TRUE)

#head(chris_insects_master_wide_plot_richness)



### Omnivores
chris_insects_master_wide_plot_omnivore <-chris_insects_master %>% filter(Trophic=="Omnivore") %>% group_by(unq_plot, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
#head(chris_insects_master_wide_plot_omnivore)
#359 Species that are potential omnivore!!! 

chris_insects_master_wide_plot_omnivore_richness<-chris_insects_master_wide_plot_omnivore[,1]
chris_insects_master_wide_plot_omnivore_richness$insect_omnivore_richness<-specnumber(chris_insects_master_wide_plot_omnivore[,-1])
chris_insects_master_wide_plot_omnivore_richness$insect_omnivore_abs_abundance<-rowSums(chris_insects_master_wide_plot_omnivore[,-1],na.rm = TRUE)
#head(chris_insects_master_wide_plot_omnivore_richness)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_wide_plot_omnivore_richness, all=TRUE)
#head(chris_insects_master_wide_plot_richness)

#now a standardized abundance
chris_insects_master_by_trap_plot_omnivore<-chris_insects_master %>% filter(Trophic=="Omnivore")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_plot_omnivore<-merge(chris_insects_master_by_trap_plot_omnivore, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_plot_omnivore$insect_omnivore_beat_abundance<-chris_insects_master_by_trap_plot_omnivore$sum_abundance/chris_insects_master_by_trap_plot_omnivore$BeatTime
chris_insects_master_by_trap_plot_omnivore_beat<-chris_insects_master_by_trap_plot_omnivore %>%  filter(Trap=="Beat") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_omnivore_beat_av_abundance = mean(insect_omnivore_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_plot_omnivore$insect_omnivore_pitfall_abundance<-chris_insects_master_by_trap_plot_omnivore$sum_abundance/chris_insects_master_by_trap_plot_omnivore$PitfallsCount
chris_insects_master_by_trap_plot_omnivore_pitfall<-chris_insects_master_by_trap_plot_omnivore %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_omnivore_pitfall_av_abundance = mean(insect_omnivore_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_omnivore_beat, all=TRUE)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_omnivore_pitfall, all=TRUE)

#head(chris_insects_master_wide_plot_richness)


### carnivores
chris_insects_master_wide_plot_carnivore <-chris_insects_master %>% filter(Trophic=="Carnivore") %>% group_by(unq_plot, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
#head(chris_insects_master_wide_plot_carnivore)
#359 Species that are potential carnivore!!! 

chris_insects_master_wide_plot_carnivore_richness<-chris_insects_master_wide_plot_carnivore[,1]
chris_insects_master_wide_plot_carnivore_richness$insect_carnivore_richness<-specnumber(chris_insects_master_wide_plot_carnivore[,-1])
chris_insects_master_wide_plot_carnivore_richness$insect_carnivore_abs_abundance<-rowSums(chris_insects_master_wide_plot_carnivore[,-1],na.rm = TRUE)
#head(chris_insects_master_wide_plot_carnivore_richness)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_wide_plot_carnivore_richness, all=TRUE)
#head(chris_insects_master_wide_plot_richness)

#now a standardized abundance
chris_insects_master_by_trap_plot_carnivore<-chris_insects_master %>% filter(Trophic=="Carnivore")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_plot_carnivore<-merge(chris_insects_master_by_trap_plot_carnivore, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_plot_carnivore$insect_carnivore_beat_abundance<-chris_insects_master_by_trap_plot_carnivore$sum_abundance/chris_insects_master_by_trap_plot_carnivore$BeatTime
chris_insects_master_by_trap_plot_carnivore_beat<-chris_insects_master_by_trap_plot_carnivore %>%  filter(Trap=="Beat") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_carnivore_beat_av_abundance = mean(insect_carnivore_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_plot_carnivore$insect_carnivore_pitfall_abundance<-chris_insects_master_by_trap_plot_carnivore$sum_abundance/chris_insects_master_by_trap_plot_carnivore$PitfallsCount
chris_insects_master_by_trap_plot_carnivore_pitfall<-chris_insects_master_by_trap_plot_carnivore %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_carnivore_pitfall_av_abundance = mean(insect_carnivore_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_carnivore_beat, all=TRUE)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_carnivore_pitfall, all=TRUE)

#head(chris_insects_master_wide_plot_richness)


### detritivores
chris_insects_master_wide_plot_detritivore <-chris_insects_master %>% filter(Trophic=="Detritivore") %>% group_by(unq_plot, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
#head(chris_insects_master_wide_plot_detritivore)
#359 Species that are potential detritivore!!! 

chris_insects_master_wide_plot_detritivore_richness<-chris_insects_master_wide_plot_detritivore[,1]
chris_insects_master_wide_plot_detritivore_richness$insect_detritivore_richness<-specnumber(chris_insects_master_wide_plot_detritivore[,-1])
chris_insects_master_wide_plot_detritivore_richness$insect_detritivore_abs_abundance<-rowSums(chris_insects_master_wide_plot_detritivore[,-1],na.rm = TRUE)
#head(chris_insects_master_wide_plot_detritivore_richness)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_wide_plot_detritivore_richness, all=TRUE)
#head(chris_insects_master_wide_plot_richness)

#now a standardized abundance
chris_insects_master_by_trap_plot_detritivore<-chris_insects_master %>% filter(Trophic=="Detritivore")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_plot_detritivore<-merge(chris_insects_master_by_trap_plot_detritivore, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_plot_detritivore$insect_detritivore_beat_abundance<-chris_insects_master_by_trap_plot_detritivore$sum_abundance/chris_insects_master_by_trap_plot_detritivore$BeatTime
chris_insects_master_by_trap_plot_detritivore_beat<-chris_insects_master_by_trap_plot_detritivore %>%  filter(Trap=="Beat") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_detritivore_beat_av_abundance = mean(insect_detritivore_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_plot_detritivore$insect_detritivore_pitfall_abundance<-chris_insects_master_by_trap_plot_detritivore$sum_abundance/chris_insects_master_by_trap_plot_detritivore$PitfallsCount
chris_insects_master_by_trap_plot_detritivore_pitfall<-chris_insects_master_by_trap_plot_detritivore %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_detritivore_pitfall_av_abundance = mean(insect_detritivore_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_detritivore_beat, all=TRUE)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_detritivore_pitfall, all=TRUE)

#head(chris_insects_master_wide_plot_richness)


### parasites
chris_insects_master_wide_plot_parasite <-chris_insects_master %>% filter(Trophic %in% c("Parasitic","Parasitoid")) %>% group_by(unq_plot, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
#head(chris_insects_master_wide_plot_parasite)
#359 Species that are potential parasite!!! 

chris_insects_master_wide_plot_parasite_richness<-chris_insects_master_wide_plot_parasite[,1]
chris_insects_master_wide_plot_parasite_richness$insect_parasite_richness<-specnumber(chris_insects_master_wide_plot_parasite[,-1])
chris_insects_master_wide_plot_parasite_richness$insect_parasite_abs_abundance<-rowSums(chris_insects_master_wide_plot_parasite[,-1],na.rm = TRUE)
#head(chris_insects_master_wide_plot_parasite_richness)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_wide_plot_parasite_richness, all=TRUE)
#head(chris_insects_master_wide_plot_richness)

#now a standardized abundance
chris_insects_master_by_trap_plot_parasite<-chris_insects_master %>% filter(Trophic %in% c("Parasitic","Parasitoid"))%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_plot_parasite<-merge(chris_insects_master_by_trap_plot_parasite, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_plot_parasite$insect_parasite_beat_abundance<-chris_insects_master_by_trap_plot_parasite$sum_abundance/chris_insects_master_by_trap_plot_parasite$BeatTime
chris_insects_master_by_trap_plot_parasite_beat<-chris_insects_master_by_trap_plot_parasite %>%  filter(Trap=="Beat") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_parasite_beat_av_abundance = mean(insect_parasite_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_plot_parasite$insect_parasite_pitfall_abundance<-chris_insects_master_by_trap_plot_parasite$sum_abundance/chris_insects_master_by_trap_plot_parasite$PitfallsCount
chris_insects_master_by_trap_plot_parasite_pitfall<-chris_insects_master_by_trap_plot_parasite %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_plot) %>% 
  summarise(insect_parasite_pitfall_av_abundance = mean(insect_parasite_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_parasite_beat, all=TRUE)
chris_insects_master_wide_plot_richness<-merge(chris_insects_master_wide_plot_richness, chris_insects_master_by_trap_plot_parasite_pitfall, all=TRUE)

head(chris_insects_master_wide_plot_richness)

#merging with veg and soil
head(veg_soil_by_plot)

inverts_veg_soil_by_plot<- merge(veg_soil_by_plot, chris_insects_master_wide_plot_richness, by="unq_plot", all=TRUE)
head(inverts_veg_soil_by_plot)

### adding in owen's otter stats per plot: 

marine_by_plot_from_notes_selected<-read.csv("C:Biodiversity idea//Output files//marine_by_plot_from_notes_selected.csv")
head(marine_by_plot_from_notes_selected)

by_plot_master<-merge(inverts_veg_soil_by_plot, marine_by_plot_from_notes_selected[, 1:15], by="unq_plot", all=TRUE)

head(by_plot_master)

by_plot_master$unq_isl<-str_sub(by_plot_master$unq_plot, end=4)




### Adding in island-level biogeographic things: 

habitat_class<-read.csv("C:Food web idea//Data by person//Pat.data//HabitatClass.csv", header=TRUE, sep=",")
length(habitat_class$unq_isl)

island_data_wiebe<-read.csv("C:Food web idea//Data by person//Pat.data//Islands_Master_Vegetation2017.csv", header=TRUE, sep=",")
names(island_data_wiebe)[1]<-"unq_isl"

habitat_class<-merge(habitat_class, island_data_wiebe[,c(1,17,18,19)])
head(habitat_class)

by_plot_master<- merge(by_plot_master, habitat_class, by="unq_isl", all=TRUE)
by_plot_master$log_Area<-log(by_plot_master$Area)


xs_isl_size<- c(0,2500000, 1000000, 5000000)
labels_isl_size <- c("<250m2", "250m2 to 1km2", "over 1km2")

hist(by_plot_master$Area, breaks=200)

by_plot_master<- by_plot_master %>% mutate(isl_size = as.factor(cut(Area, xs_isl_size, labels = labels_isl_size)))
head(by_plot_master)
View(by_plot_master)

# Tidying up -------------------------------------------------------------

write.csv(by_plot_master, "C:Food web idea//Data by person//Norah.data/by_plot_master.csv", row.names = FALSE)
write.csv(by_plot_master, "C://Users//norah//Dropbox//Projects//Owen's MS//Owen_MS//Analysis Data//by_plot_master.csv", row.names=FALSE)




### plotting
head(by_plot_master)

ggplot(aes(y=d15n, x=shore_dist, col=isl_size), data=by_plot_master)+geom_point(jitter=TRUE)+geom_smooth(method="lm")

