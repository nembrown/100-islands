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
soil.deb<-soil.deb[,-11]

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
head(owen_key)
#take uot isl will put in later
owen_key<-owen_key[,-3]
owen_key<-owen_key %>% dplyr::distinct()

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
#NOW we want to cut down to one value per island: 

soil_owen_deb$unq_isl<-strtrim(soil_owen_deb$unq_plot, 4)

soil_owen_deb_by_isl<- soil_owen_deb %>%  group_by(unq_isl)%>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(soil_owen_deb_by_isl)
length(unique(soil_owen_deb_by_isl$unq_isl))
#99 islands 

#we could change this to weight different points differently but here just a mean of all samples on the island...


# Adding habitat class and island characteristics -------------------------

habitat_class<-read.csv("C:Food web idea//Data by person//Pat.data//HabitatClass.csv", header=TRUE, sep=",")

habitat_class<-habitat_class[,-21]
habitat_class$unq_isl<-habitat_class$Island_ID
habitat_class<-habitat_class[,-1]
head(habitat_class)
length(habitat_class$unq_isl)

island_data_wiebe<-read.csv("C:Food web idea//Data by person//Pat.data//Islands_Master_Vegetation2017.csv", header=TRUE, sep=",")
head(island_data_wiebe)

names(island_data_wiebe)[1]<-"unq_isl"
names(island_data_wiebe)[19]<-"slope_isl"

habitat_class<-merge(habitat_class, island_data_wiebe[,c(1,15, 17,18,19)])

habitat_soil_by_isl<-merge(soil_owen_deb_by_isl, habitat_class, by="unq_isl", all=TRUE)
head(habitat_soil_by_isl)
length(habitat_soil_by_isl$unq_isl)



# Becky's tree density and diversity data --------------------------

becky_trees<-read.csv("C:Food web idea//Data by person//Becky.data//data_tree_abund_cover.csv", header=TRUE, sep=",")
becky_trees<-becky_trees[,-1]
#head(becky_trees)
names(becky_trees)[1]<-"unq_isl"
names(becky_trees)[3]<-"species"
becky_trees<-as.data.frame(becky_trees)

becky_trees_wide <-becky_trees %>% group_by(unq_isl, species) %>% 
  summarise(sum_abundance = mean(abund.ab, na.rm=TRUE)) %>% 
  spread(species, sum_abundance) %>% 
  replace(is.na(.), 0) 
#head(becky_trees_wide)


#just getting absolute abundance ... what about relative? 
#what about basal area? cover? what should be included? 
becky_trees_wide_richness<-becky_trees_wide[,1]
becky_trees_wide_richness$tree_richness<-specnumber(becky_trees_wide[,-1])
becky_trees_wide_richness$tree_diversity<-diversity(becky_trees_wide[,-1], index="shannon")
becky_trees_wide_richness$tree_evenness<-becky_trees_wide_richness$tree_diversity/(log(becky_trees_wide_richness$tree_richness))



becky_trees_wide_richness$tree_abundance<-rowSums(becky_trees_wide[,-1],na.rm = TRUE)
head(becky_trees_wide_richness)


#summed total basal area of all species in that island= "cover" type thing 
becky_trees_2<-becky_trees %>% group_by(unq_isl) %>% 
  summarise(sum_basal = sum(ba.tot, na.rm=TRUE)) %>% 
  replace(is.na(.), 0) 


#head(becky_trees_2)
becky_trees_wide_richness<-merge(becky_trees_wide_richness, becky_trees_2)



habitat_soil_by_isl<-merge(habitat_soil_by_isl,becky_trees_wide_richness, by="unq_isl", all=TRUE)
#head(habitat_soil_by_isl)
length(unique(habitat_soil_by_isl$unq_isl))


# Adding plant cover and richness -----------------------------------------

#this loads data from "Habitation data" R script

longform_plant_percentcover<-read.csv("C:Food web idea//Data by person//Kalina.data/Deb_Owen_veg_combined_complete_filled.csv", header=TRUE, sep=",")
head(longform_plant_percentcover)

longform_plant_percentcover$unq_isl<-fct_explicit_na(longform_plant_percentcover$unq_isl)

longform_plant_percentcover2 <- longform_plant_percentcover[,c(1:7)]%>% 
  group_by(unq_isl,species) %>% summarise(cover_mean = mean(cover, na.rm=TRUE)) %>% 
  spread(species, cover_mean)%>%  replace(is.na(.), 0)

longform_plant_percentcover_shrub<- longform_plant_percentcover%>% filter(herb_shrub=="shrub")
longform_plant_percentcover_herb<- longform_plant_percentcover %>% filter(herb_shrub=="herb")

longform_plant_percentcover2_isl_shrub <- longform_plant_percentcover_shrub[,c(1:7)] %>% 
  group_by(unq_isl,species) %>% 
  summarise(cover_mean = mean(cover, na.rm=TRUE)) %>% 
  spread(species, cover_mean)%>%  replace(is.na(.), 0)

longform_plant_percentcover2_isl_herb <- longform_plant_percentcover_herb[,c(1:7)] %>% 
  group_by(unq_isl,species) %>% 
  summarise(cover_mean = mean(cover, na.rm=TRUE)) %>% 
  spread(species, cover_mean)%>%  replace(is.na(.), 0)

longform_plant_percentcover_species_isl<-longform_plant_percentcover2
#head(longform_plant_percentcover_species_isl)

which( colnames(longform_plant_percentcover2)=="gash" )
which( colnames(longform_plant_percentcover2)=="midi" )


longform_plant_percentcover3_isl<-longform_plant_percentcover2[,c(1,37,52)]
#head(longform_plant_percentcover3_isl)
longform_plant_percentcover3_isl$plant_richness<-specnumber(longform_plant_percentcover_species_isl[,-c(1)])
longform_plant_percentcover3_isl$plant_shannon.diversity<-diversity(longform_plant_percentcover_species_isl[,-c(1)], index="shannon")
longform_plant_percentcover3_isl$plant_evenness<-longform_plant_percentcover3_isl$plant_shannon.diversity/(log(longform_plant_percentcover3_isl$plant_richness))
longform_plant_percentcover3_isl$total_cover<-rowSums(longform_plant_percentcover_species_isl[,-c(1)], na.rm=TRUE)

longform_plant_percentcover3_isl$shrub_richness<-specnumber(longform_plant_percentcover2_isl_shrub[,-c(1)])
longform_plant_percentcover3_isl$shrub_cover<-rowSums(longform_plant_percentcover2_isl_shrub[,-c(1)], na.rm=TRUE)
longform_plant_percentcover3_isl$herb_richness<-specnumber(longform_plant_percentcover2_isl_herb[,-c(1)])
longform_plant_percentcover3_isl$herb_cover<-rowSums(longform_plant_percentcover2_isl_herb[,-c(1)], na.rm=TRUE)

#head(longform_plant_percentcover3_isl)


#head(soil_merge)

habitat_veg_soil_by_isl<-merge(habitat_soil_by_isl, longform_plant_percentcover3_isl, by="unq_isl", all=TRUE)
head(habitat_veg_soil_by_isl)
length(habitat_veg_soil_by_isl$unq_isl)



# Adding in bird richness -------------------------------------------------

birdrichness<-read.csv("C:Food web idea//Data by person//Deb.data//bird-summary.csv", header=TRUE, sep=",")
head(birdrichness)
names(birdrichness)[1]<-"unq_isl"
names(birdrichness)[6]<-"bird.richness"
names(birdrichness)[4]<-"bird.density"
birdrichness$bird.evenness<-birdrichness$evenness/(log(birdrichness$bird.richness))
#head(birdrichness)

habitat_veg_bird_soil_by_isl<-merge(habitat_veg_soil_by_isl, birdrichness[,c(1,4,6,11)], by="unq_isl", all=TRUE)
#head(habitat_veg_bird_soil_by_isl)

length(habitat_veg_bird_soil_by_isl$unq_isl)
#101
head(habitat_veg_bird_soil_by_isl)

# Adding in wrack richness and wrack habitat ------------------------------------------------

seawrack_key<-read.csv("C:Food web idea//Data by person//Sara's data//seawrack_spatial_mod.csv", header=TRUE, sep=",")
#head(seawrack_key)
seawrack_key$ISLAND<-sprintf("%02d",seawrack_key$ISLAND)
seawrack_key$unq_isl <- paste(seawrack_key$NODE,seawrack_key$ISLAND)
seawrack_key$unq_isl<-gsub(" ", "", seawrack_key$unq_isl, fixed = TRUE)
#head(seawrack_key)


sara_habitat<-read.csv("C:Food web idea//Data by person//Sara's data//sara_habitat.csv", header=TRUE, sep=",")
#head(sara_habitat)
sara_habitat_merged<-merge(sara_habitat, seawrack_key, by.y="unq_tran", all=TRUE)
head(sara_habitat_merged)

sara_habitat_merged_selected<-c("unq_tran","unq_isl", "MEAN_egarea2k",
       "MEAN_kparea2k",
       "MEAN_rockarea2000",
       "sum_2km" ,
       "SLOPE" ,
       "WIDTH" ,
       "HAB2000" ,
       "SITE_SUM" ,
       "MEAN_egarea100",
       "WAVE_EXPOSURE", "SUBSTRATE")

sara_habitat_merged_simple<-sara_habitat_merged[, colnames(sara_habitat_merged) %in% sara_habitat_merged_selected]
head(sara_habitat_merged_simple)

sara_habitat_merged_simple$WAVE_EXPOSURE[sara_habitat_merged_simple$WAVE_EXPOSURE=="VP"]<-1
sara_habitat_merged_simple$WAVE_EXPOSURE[sara_habitat_merged_simple$WAVE_EXPOSURE=="P"]<-2
sara_habitat_merged_simple$WAVE_EXPOSURE[sara_habitat_merged_simple$WAVE_EXPOSURE=="SP"]<-3
sara_habitat_merged_simple$WAVE_EXPOSURE[sara_habitat_merged_simple$WAVE_EXPOSURE=="SE"]<-4
sara_habitat_merged_simple$WAVE_EXPOSURE[sara_habitat_merged_simple$WAVE_EXPOSURE=="E"]<-5
sara_habitat_merged_simple$WAVE_EXPOSURE[sara_habitat_merged_simple$WAVE_EXPOSURE=="VE"]<-6
sara_habitat_merged_simple$WAVE_EXPOSURE<-as.numeric(sara_habitat_merged_simple$WAVE_EXPOSURE)



### add in water area (calculated by Will)
water_area<-read.csv("C:Food web idea//Data by person//Norah.data//WaterArea.csv", header=TRUE, sep=",")
head(water_area)
water_area<-water_area[,-1]
water_area<-water_area %>% group_by(unq_tran)%>% spread(Radius_m, WaterArea_m2, sep="_")
water_area<-water_area[,c(6,11:17)]
sara_habitat_merged_simple<-merge(sara_habitat_merged_simple, water_area, by="unq_tran")

sara_habitat_merged_simple$beachy_substrate<- ifelse(grepl("ROCK", sara_habitat_merged_simple$SUBSTRATE), "0", "1")
sara_habitat_merged_simple$beachy_substrate<-as.character(sara_habitat_merged_simple$beachy_substrate)
sara_habitat_merged_simple$beachy_substrate<-as.numeric(sara_habitat_merged_simple$beachy_substrate)
sara_habitat_merged_simple$SLOPE_degrees<-(180*(atan(sara_habitat_merged_simple$SLOPE/100)))/pi

sara_habitat_merged_by_isl <- sara_habitat_merged_simple %>% group_by(unq_isl)%>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(sara_habitat_merged_by_isl )
length(sara_habitat_merged_by_isl$unq_isl)
# str(sara_habitat_merged_by_isl )


#### Seaweed composition
sara_composition<-read.csv("C:Food web idea//Data by person//Sara's data//sara_composition.csv", header=TRUE, sep=",")
#head(sara_composition)
#this is by transect

#make it by island
sara_composition_means <- sara_composition %>%  
  group_by(unq_isl)%>% summarise_if(is.numeric, mean, na.rm=TRUE)
#head(sara_composition_means)

which( colnames(sara_composition_means)=="SITE_SUM" )
#take out site sum and island and unq_isl

sara_composition_means_richness<-sara_composition_means[,1]
sara_composition_means_richness$wrack_richness<-specnumber(sara_composition_means[,-c(1,2,48)])
sara_composition_means_richness$site_mean_by_isl<-sara_composition_means$SITE_SUM

sara_composition_sums<-sara_composition %>%  
  group_by(unq_isl)%>% summarise_if(is.numeric, sum, na.rm=TRUE)
#head(sara_composition_sums)

sara_composition_means_richness$site_sum_by_isl<-sara_composition_sums$SITE_SUM

head(sara_habitat_merged_by_isl)
# add in diversity to full wrack story
sara_habitat_merged_by_isl<-merge(sara_habitat_merged_by_isl, sara_composition_means_richness, by="unq_isl", all=TRUE)
head(sara_habitat_merged_by_isl)


#add in wrack to birds, veg, habitat
habitat_veg_bird_wrack_soil_by_isl<-merge(habitat_veg_bird_soil_by_isl, sara_habitat_merged_by_isl, by="unq_isl", all=TRUE)
head(habitat_veg_bird_wrack_soil_by_isl)

length(habitat_veg_bird_wrack_soil_by_isl$unq_isl)


# Adding in eagles and ravens pointcounts ---------------------------------------------
ravens <- read.csv("C:Food web idea//Data by person//Deb.data/ravens.csv")
cora.isls <- unique(ravens$island)
cora.isls <- as.data.frame(cora.isls)
cora.isls$ravens <- 1
names(cora.isls) <- c("unq_isl", "ravens")

by_isl_master<-merge(habitat_veg_bird_wrack_soil_by_isl, cora.isls, by="unq_isl", all=TRUE)

eagles <- read.csv("C:Food web idea//Data by person//Deb.data/eagles.csv")
baea.isls <- unique(eagles$island)
baea.isls <- as.data.frame(baea.isls)
baea.isls$eagles <- 1
names(baea.isls) <- c("unq_isl", "eagles")
#head(baea.isls)

by_isl_master <- merge(by_isl_master, baea.isls, all = TRUE)

by_isl_master$ravens[is.na(by_isl_master$ravens)]<-0
by_isl_master$eagles[is.na(by_isl_master$eagles)]<-0

head(by_isl_master)
length(by_isl_master$unq_isl)


# Chris insects -----------------------------------------------------------

#new data July 2019 
chris_insects_master<-read.csv("C:Food web idea//Data by person//Chris.data//invert_id_abundance_v4.csv", header=TRUE, sep=",")
head(chris_insects_master)
chris_insects_master$unq_isl<-strtrim(chris_insects_master$Trapline, 4)
chris_insects_master$unq_tran<-strtrim(chris_insects_master$Trapline, 5)
chris_insects_master$plot<-substr(chris_insects_master$Trapline, 5, 5)
#head(chris_insects_master)

# I still want to divde by groups BUT I will do all insects as a whole first then break into groups

# For each island there are 4 transects and one interior plot
#My understanding is that there is only one pitfall or one beat per "transect".. if that's not true I will need to change script. 

chris_insects_master_wide <-chris_insects_master %>% group_by(unq_isl, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
head(chris_insects_master_wide)
#871 Species!!! 

chris_insects_master_wide_richness<-chris_insects_master_wide[,1]
chris_insects_master_wide_richness$insect_richness<-specnumber(chris_insects_master_wide[,-1])
chris_insects_master_wide_richness$insect_abs_abundance<-rowSums(chris_insects_master_wide[,-1],na.rm = TRUE)
chris_insects_master_wide_richness$insect_diversity<-diversity(chris_insects_master_wide[,-1],index="shannon")
chris_insects_master_wide_richness$insect_evenness<-chris_insects_master_wide_richness$insect_diversity/(log(chris_insects_master_wide_richness$insect_richness))
#head(chris_insects_master_wide_richness)


#now a more standardized abundance measure per beat or pitfall trap on the island
chris_trapline_data<-read.csv("C:Food web idea//Data by person//Chris.data//trapline_data.csv", header=TRUE, sep=",")
chris_trapline_data$unq_isl<-strtrim(chris_trapline_data$Trapline, 4)
chris_trapline_data$unq_tran<-strtrim(chris_trapline_data$Trapline, 5)
chris_trapline_data$plot<-substr(chris_trapline_data$Trapline, 5, 5)
#head(chris_trapline_data)

#sum number of insects found on that trapline/traptype
chris_insects_master_by_trap<-chris_insects_master %>% group_by(unq_tran, Trap) %>% 
                            summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap<-merge(chris_insects_master_by_trap, chris_trapline_data[,-c(2:15)], by="unq_tran")

#head(chris_insects_master_by_trap)

chris_insects_master_by_trap$insect_beat_abundance<-chris_insects_master_by_trap$sum_abundance/chris_insects_master_by_trap$BeatTime

chris_insects_master_by_trap_beat<-chris_insects_master_by_trap %>%  filter(Trap=="Beat") %>% 
                                    group_by(unq_isl) %>% 
                                    summarise(insect_beat_av_abundance = mean(insect_beat_abundance, na.rm=TRUE)) %>% 
                                    replace(is.na(.), 0)

chris_insects_master_by_trap$insect_pitfall_abundance<-chris_insects_master_by_trap$sum_abundance/chris_insects_master_by_trap$PitfallsCount
chris_insects_master_by_trap_pitfall<-chris_insects_master_by_trap %>%  filter(Trap=="Pitfall") %>% 
                                    group_by(unq_isl) %>% 
                                    summarise(insect_pitfall_av_abundance = mean(insect_pitfall_abundance, na.rm=TRUE)) %>% 
                                    replace(is.na(.), 0)


chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_beat, all=TRUE)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_pitfall, all=TRUE)

#head(chris_insects_master_wide_richness)

#########now the same measures but for a few different categories. 
#head(chris_insects_master)

###Are they eaten by birds or not? 
chris_insects_master_wide_birdfood <-chris_insects_master %>% filter(BirdFood=="Yes") %>% group_by(unq_isl, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
#head(chris_insects_master_wide_birdfood)
#359 Species that are potential birdfood!!! 

chris_insects_master_wide_birdfood_richness<-chris_insects_master_wide_birdfood[,1]
chris_insects_master_wide_birdfood_richness$insect_birdfood_richness<-specnumber(chris_insects_master_wide_birdfood[,-1])
chris_insects_master_wide_birdfood_richness$insect_birdfood_abs_abundance<-rowSums(chris_insects_master_wide_birdfood[,-1],na.rm = TRUE)
#head(chris_insects_master_wide_birdfood_richness)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_wide_birdfood_richness, all=TRUE)
#head(chris_insects_master_wide_richness)

chris_insects_master_by_trap_birdfood<-chris_insects_master %>% filter(BirdFood=="Yes")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))
chris_insects_master_by_trap_birdfood<-merge(chris_insects_master_by_trap_birdfood, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_birdfood$insect_birdfood_beat_abundance<-chris_insects_master_by_trap_birdfood$sum_abundance/chris_insects_master_by_trap_birdfood$BeatTime
chris_insects_master_by_trap_birdfood_beat<-chris_insects_master_by_trap_birdfood %>%  filter(Trap=="Beat") %>% 
  group_by(unq_isl) %>% 
  summarise(insect_birdfood_beat_av_abundance = mean(insect_birdfood_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_birdfood$insect_birdfood_pitfall_abundance<-chris_insects_master_by_trap_birdfood$sum_abundance/chris_insects_master_by_trap_birdfood$PitfallsCount
chris_insects_master_by_trap_birdfood_pitfall<-chris_insects_master_by_trap_birdfood %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_isl) %>% 
  summarise(insect_birdfood_pitfall_av_abundance = mean(insect_birdfood_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_birdfood_beat, all=TRUE)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_birdfood_pitfall, all=TRUE)

#head(chris_insects_master_wide_richness)




### Trophic Group and trophic diversity


### Start with Herbivores
chris_insects_master_wide_herbivore <-chris_insects_master %>% filter(Trophic=="Herbivore") %>% group_by(unq_isl, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
#head(chris_insects_master_wide_herbivore)
#359 Species that are potential herbivore!!! 

chris_insects_master_wide_herbivore_richness<-chris_insects_master_wide_herbivore[,1]
chris_insects_master_wide_herbivore_richness$insect_herbivore_richness<-specnumber(chris_insects_master_wide_herbivore[,-1])
chris_insects_master_wide_herbivore_richness$insect_herbivore_abs_abundance<-rowSums(chris_insects_master_wide_herbivore[,-1],na.rm = TRUE)
#head(chris_insects_master_wide_herbivore_richness)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_wide_herbivore_richness, all=TRUE)
#head(chris_insects_master_wide_richness)

#now a standardized abundance
chris_insects_master_by_trap_herbivore<-chris_insects_master %>% filter(Trophic=="Herbivore")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_herbivore<-merge(chris_insects_master_by_trap_herbivore, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_herbivore$insect_herbivore_beat_abundance<-chris_insects_master_by_trap_herbivore$sum_abundance/chris_insects_master_by_trap_herbivore$BeatTime
chris_insects_master_by_trap_herbivore_beat<-chris_insects_master_by_trap_herbivore %>%  filter(Trap=="Beat") %>% 
  group_by(unq_isl) %>% 
  summarise(insect_herbivore_beat_av_abundance = mean(insect_herbivore_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_herbivore$insect_herbivore_pitfall_abundance<-chris_insects_master_by_trap_herbivore$sum_abundance/chris_insects_master_by_trap_herbivore$PitfallsCount
chris_insects_master_by_trap_herbivore_pitfall<-chris_insects_master_by_trap_herbivore %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_isl) %>% 
  summarise(insect_herbivore_pitfall_av_abundance = mean(insect_herbivore_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_herbivore_beat, all=TRUE)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_herbivore_pitfall, all=TRUE)

#head(chris_insects_master_wide_richness)



### Omnivores
chris_insects_master_wide_omnivore <-chris_insects_master %>% filter(Trophic=="Omnivore") %>% group_by(unq_isl, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
#head(chris_insects_master_wide_omnivore)
#359 Species that are potential omnivore!!! 

chris_insects_master_wide_omnivore_richness<-chris_insects_master_wide_omnivore[,1]
chris_insects_master_wide_omnivore_richness$insect_omnivore_richness<-specnumber(chris_insects_master_wide_omnivore[,-1])
chris_insects_master_wide_omnivore_richness$insect_omnivore_abs_abundance<-rowSums(chris_insects_master_wide_omnivore[,-1],na.rm = TRUE)
#head(chris_insects_master_wide_omnivore_richness)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_wide_omnivore_richness, all=TRUE)
#head(chris_insects_master_wide_richness)

#now a standardized abundance
chris_insects_master_by_trap_omnivore<-chris_insects_master %>% filter(Trophic=="Omnivore")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_omnivore<-merge(chris_insects_master_by_trap_omnivore, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_omnivore$insect_omnivore_beat_abundance<-chris_insects_master_by_trap_omnivore$sum_abundance/chris_insects_master_by_trap_omnivore$BeatTime
chris_insects_master_by_trap_omnivore_beat<-chris_insects_master_by_trap_omnivore %>%  filter(Trap=="Beat") %>% 
  group_by(unq_isl) %>% 
  summarise(insect_omnivore_beat_av_abundance = mean(insect_omnivore_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_omnivore$insect_omnivore_pitfall_abundance<-chris_insects_master_by_trap_omnivore$sum_abundance/chris_insects_master_by_trap_omnivore$PitfallsCount
chris_insects_master_by_trap_omnivore_pitfall<-chris_insects_master_by_trap_omnivore %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_isl) %>% 
  summarise(insect_omnivore_pitfall_av_abundance = mean(insect_omnivore_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_omnivore_beat, all=TRUE)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_omnivore_pitfall, all=TRUE)

#head(chris_insects_master_wide_richness)


### carnivores
chris_insects_master_wide_carnivore <-chris_insects_master %>% filter(Trophic=="Carnivore") %>% group_by(unq_isl, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
#head(chris_insects_master_wide_carnivore)
#359 Species that are potential carnivore!!! 

chris_insects_master_wide_carnivore_richness<-chris_insects_master_wide_carnivore[,1]
chris_insects_master_wide_carnivore_richness$insect_carnivore_richness<-specnumber(chris_insects_master_wide_carnivore[,-1])
chris_insects_master_wide_carnivore_richness$insect_carnivore_abs_abundance<-rowSums(chris_insects_master_wide_carnivore[,-1],na.rm = TRUE)
#head(chris_insects_master_wide_carnivore_richness)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_wide_carnivore_richness, all=TRUE)
#head(chris_insects_master_wide_richness)

#now a standardized abundance
chris_insects_master_by_trap_carnivore<-chris_insects_master %>% filter(Trophic=="Carnivore")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_carnivore<-merge(chris_insects_master_by_trap_carnivore, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_carnivore$insect_carnivore_beat_abundance<-chris_insects_master_by_trap_carnivore$sum_abundance/chris_insects_master_by_trap_carnivore$BeatTime
chris_insects_master_by_trap_carnivore_beat<-chris_insects_master_by_trap_carnivore %>%  filter(Trap=="Beat") %>% 
  group_by(unq_isl) %>% 
  summarise(insect_carnivore_beat_av_abundance = mean(insect_carnivore_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_carnivore$insect_carnivore_pitfall_abundance<-chris_insects_master_by_trap_carnivore$sum_abundance/chris_insects_master_by_trap_carnivore$PitfallsCount
chris_insects_master_by_trap_carnivore_pitfall<-chris_insects_master_by_trap_carnivore %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_isl) %>% 
  summarise(insect_carnivore_pitfall_av_abundance = mean(insect_carnivore_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_carnivore_beat, all=TRUE)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_carnivore_pitfall, all=TRUE)

#head(chris_insects_master_wide_richness)


### detritivores
chris_insects_master_wide_detritivore <-chris_insects_master %>% filter(Trophic=="Detritivore") %>% group_by(unq_isl, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
#head(chris_insects_master_wide_detritivore)
#359 Species that are potential detritivore!!! 

chris_insects_master_wide_detritivore_richness<-chris_insects_master_wide_detritivore[,1]
chris_insects_master_wide_detritivore_richness$insect_detritivore_richness<-specnumber(chris_insects_master_wide_detritivore[,-1])
chris_insects_master_wide_detritivore_richness$insect_detritivore_abs_abundance<-rowSums(chris_insects_master_wide_detritivore[,-1],na.rm = TRUE)
#head(chris_insects_master_wide_detritivore_richness)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_wide_detritivore_richness, all=TRUE)
#head(chris_insects_master_wide_richness)

#now a standardized abundance
chris_insects_master_by_trap_detritivore<-chris_insects_master %>% filter(Trophic=="Detritivore")%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_detritivore<-merge(chris_insects_master_by_trap_detritivore, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_detritivore$insect_detritivore_beat_abundance<-chris_insects_master_by_trap_detritivore$sum_abundance/chris_insects_master_by_trap_detritivore$BeatTime
chris_insects_master_by_trap_detritivore_beat<-chris_insects_master_by_trap_detritivore %>%  filter(Trap=="Beat") %>% 
  group_by(unq_isl) %>% 
  summarise(insect_detritivore_beat_av_abundance = mean(insect_detritivore_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_detritivore$insect_detritivore_pitfall_abundance<-chris_insects_master_by_trap_detritivore$sum_abundance/chris_insects_master_by_trap_detritivore$PitfallsCount
chris_insects_master_by_trap_detritivore_pitfall<-chris_insects_master_by_trap_detritivore %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_isl) %>% 
  summarise(insect_detritivore_pitfall_av_abundance = mean(insect_detritivore_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_detritivore_beat, all=TRUE)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_detritivore_pitfall, all=TRUE)

#head(chris_insects_master_wide_richness)


### parasites
chris_insects_master_wide_parasite <-chris_insects_master %>% filter(Trophic %in% c("Parasitic","Parasitoid")) %>% group_by(unq_isl, SpeciesID) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE)) %>% 
  spread(SpeciesID, sum_abundance) %>% 
  replace(is.na(.), 0)
#head(chris_insects_master_wide_parasite)
#359 Species that are potential parasite!!! 

chris_insects_master_wide_parasite_richness<-chris_insects_master_wide_parasite[,1]
chris_insects_master_wide_parasite_richness$insect_parasite_richness<-specnumber(chris_insects_master_wide_parasite[,-1])
chris_insects_master_wide_parasite_richness$insect_parasite_abs_abundance<-rowSums(chris_insects_master_wide_parasite[,-1],na.rm = TRUE)
#head(chris_insects_master_wide_parasite_richness)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_wide_parasite_richness, all=TRUE)
#head(chris_insects_master_wide_richness)

#now a standardized abundance
chris_insects_master_by_trap_parasite<-chris_insects_master %>% filter(Trophic %in% c("Parasitic","Parasitoid"))%>% group_by(unq_tran, Trap) %>% 
  summarise(sum_abundance = sum(Abundance, na.rm=TRUE))

chris_insects_master_by_trap_parasite<-merge(chris_insects_master_by_trap_parasite, chris_trapline_data[,-c(2:15)], by="unq_tran")

chris_insects_master_by_trap_parasite$insect_parasite_beat_abundance<-chris_insects_master_by_trap_parasite$sum_abundance/chris_insects_master_by_trap_parasite$BeatTime
chris_insects_master_by_trap_parasite_beat<-chris_insects_master_by_trap_parasite %>%  filter(Trap=="Beat") %>% 
  group_by(unq_isl) %>% 
  summarise(insect_parasite_beat_av_abundance = mean(insect_parasite_beat_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)

chris_insects_master_by_trap_parasite$insect_parasite_pitfall_abundance<-chris_insects_master_by_trap_parasite$sum_abundance/chris_insects_master_by_trap_parasite$PitfallsCount
chris_insects_master_by_trap_parasite_pitfall<-chris_insects_master_by_trap_parasite %>%  filter(Trap=="Pitfall") %>% 
  group_by(unq_isl) %>% 
  summarise(insect_parasite_pitfall_av_abundance = mean(insect_parasite_pitfall_abundance, na.rm=TRUE)) %>% 
  replace(is.na(.), 0)


chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_parasite_beat, all=TRUE)
chris_insects_master_wide_richness<-merge(chris_insects_master_wide_richness, chris_insects_master_by_trap_parasite_pitfall, all=TRUE)

#head(chris_insects_master_wide_richness)

#New branch here, deleting the previous work on separating into taxonomic groups - that's more for Chris to work on. 


by_isl_master<-merge(by_isl_master, chris_insects_master_wide_richness, by="unq_isl", all=TRUE)
length(by_isl_master$unq_isl)


# Mammal richness ---------------------------------------------------------

katie_mammals<-read.csv("C:Food web idea//Data by person//Katie.data//100_Islands_mammal_team_diversity_data_2018_03_17.csv", header=TRUE, sep=",")
#head(katie_mammals)
names(katie_mammals)[2]<-"unq_isl"
katie_mammals_simple<-katie_mammals[,-c(1,3, 13)]
#head(katie_mammals_simple)
names(katie_mammals_simple)[10]<-"mammal_richness"
katie_mammals_simple<-katie_mammals[,-c(1,3, 13)]
katie_mammals_wide<-as_data_frame(katie_mammals[,-c(1,3, 12, 13)])


katie_richness<-katie_mammals_wide[,1]
katie_richness$mammal_richness<-specnumber(katie_mammals_wide[,-1])
#katie_richness$mammal_abundance<-rowSums(katie_mammals_wide[,-1],na.rm = TRUE)
#katie_richness$mammal_diversity<-diversity(katie_mammals_wide[,-1],index="shannon")
#katie_richness$mammal_evenness<-katie_richness$mammal_diversity/(log(katie_richness$mammal_richness))

#head(katie_richness)

by_isl_master<-merge(by_isl_master, katie_richness, by="unq_isl", all=TRUE)
length(by_isl_master$unq_isl)


# Tiidying up -------------------------------------------------------------

#Extras
by_isl_master$log_Area<-log(by_isl_master$Area)
by_isl_master$log_SITE_SUM<-log(by_isl_master$SITE_SUM+1)
by_isl_master$log_site_sum_by_isl<-log(by_isl_master$site_sum_by_isl+1)
by_isl_master$log_HAB2000<-log(by_isl_master$HAB2000+1)
by_isl_master_habitat<-by_isl_master %>%  dplyr::select(Closed_Forest, Open_Forest_or_Dense_Shrub, Light_Shrub_or_Grassy, Bog_Vegetation, Bog_Water, Snags_Woody)
by_isl_master$habitat_het<-diversity(by_isl_master_habitat)
by_isl_master$total_richness<-by_isl_master$plant_richness+by_isl_master$tree_richness+by_isl_master$insect_richness+by_isl_master$bird.richness+by_isl_master$mammal_richness
by_isl_master$node<-str_sub(by_isl_master$unq_isl, end=2)

#otters
owen_otter_edge_isl<- read.csv("C:Biodiversity idea//Output files//owen_otter_edge_isl.csv")
by_isl_master<-merge(by_isl_master, owen_otter_edge_isl, by="unq_isl", all=TRUE)

head(by_isl_master)
write.csv(by_isl_master, "C:Food web idea//Data by person//Norah.data/by_isl_master.csv", row.names = FALSE)
write.csv(by_isl_master, "C://Users//norah//Dropbox//Projects//Owen's MS//Owen_MS//Analysis Data//by_isl_master.csv", row.names=FALSE)

length(by_isl_master$unq_isl)


ggplot(aes(x=log_Area, y=otter_pres), data=by_isl_master)+ geom_point()+geom_smooth()






# Plotting correlations ---------------------------------------------------

#extracting numeric/important variables, adjust as necessary 
paste(
which( colnames(by_isl_master)=="tree_abundance" ),
which( colnames(by_isl_master)=="tree_richness" ),
which( colnames(by_isl_master)=="insect_richness" ),
which( colnames(by_isl_master)=="insect_abundance" ),
which( colnames(by_isl_master)=="eagles" ),
which( colnames(by_isl_master)=="ravens" ),
which( colnames(by_isl_master)=="gash" ),
which( colnames(by_isl_master)=="midi" ),
which( colnames(by_isl_master)=="MEAN_egarea2k" ),
which( colnames(by_isl_master)=="bird.richness" ),
which( colnames(by_isl_master)=="plant.richness" ),
which( colnames(by_isl_master)=="log_HAB2000" ),
which( colnames(by_isl_master)=="d15n" ),
which( colnames(by_isl_master)=="PA_norml" ),
which( colnames(by_isl_master)=="Neighb_250" ),
which( colnames(by_isl_master)=="s" ), 
which( colnames(by_isl_master)=="log_Area" ),
which( colnames(by_isl_master)=="wrack_richness" ),
which( colnames(by_isl_master)=="log_site_sum_by_isl" ),
which( colnames(by_isl_master)=="habitat_het" ),sep=","

)

str(by_isl_master)
#head(by_isl_master)

corr_by_isl_selected<-by_isl_master[,c(34,33,58,59,2,3,35,36,39,37,5,63,10,18,22,8,60,47,62,64)]
str(corr_by_isl_selected)
#can extract only numeric with select(if.numeric) 

corr_by_isl_selected_2 <- round(cor(corr_by_isl_selected, use="pairwise.complete.obs"), 1)
#head(corr_by_isl_selected_2[, 1:6])
p.mat_by_isl_selected_2 <- cor_pmat(corr_by_isl_selected)
#head(p.mat_by_isl_selected_2[, 1:4])

ggcorrplot(corr_by_isl_selected_2)
ggcorrplot(corr_by_isl_selected_2, hc.order = TRUE, type = "lower",
           outline.col = "white")
ggcorrplot(corr_by_isl_selected_2, hc.order = TRUE,
           type = "lower", p.mat = p.mat_by_isl_selected_2)
ggcorrplot(corr_by_isl_selected_2, hc.order = TRUE, type = "lower",
           lab = TRUE)


#Network plot
corr_by_isl_selected %>% correlate() %>%  network_plot(min_cor = 0.1)

#using Performance Analytics
chart.Correlation(corr_by_isl_selected , histogram=TRUE, pch=19)


# Plotting regressions using categories ----------------------------------------------------
ggplot(by_isl_master, aes(y=d15n, x=d13c, size=Area))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis()+  scale_colour_viridis()

ggplot(by_isl_master, aes(y=d15n, x=d13c, col=Area, size=Area))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis()+  scale_colour_viridis()


#Size category split soil n15
na0<-ggplot(by_isl_master, aes(y=total_richness, x=d15n, colour=size.cat2, fill=size.cat2))+geom_point()+geom_smooth(aes(fill=size.cat2),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
na1<-ggplot(by_isl_master, aes(y=insect_richness, x=d15n, colour=size.cat2, fill=size.cat2))+geom_point()+geom_smooth(aes(fill=size.cat2),method="glm", method.args = list(family = "poisson"))+ylim(0,350)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+theme(legend.position="none")
na2<-ggplot(by_isl_master, aes(y=tree_richness, x=d15n, colour=size.cat2, fill=size.cat2))+geom_point()+geom_smooth(aes(fill=size.cat2),method="glm", method.args = list(family = "poisson"))+ylim(0,10)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
na3<-ggplot(na.omit(by_isl_master), aes(y=plant_richness, x=d15n, colour=size.cat2, fill=size.cat2))+geom_point()+geom_smooth(aes(fill=size.cat2),method="glm", method.args = list(family = "poisson"))+ylim(0,60)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
na4<-ggplot(by_isl_master, aes(y=bird.richness, x=d15n, colour=size.cat2, fill=size.cat2))+geom_point()+geom_smooth(aes(fill=size.cat2),method="glm", method.args = list(family = "poisson"))+ylim(0,25)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
na5<-ggplot(by_isl_master, aes(y=mammal_richness, x=d15n, colour=size.cat2, fill=size.cat2))+geom_point()+geom_smooth(aes(fill=size.cat2),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
plot_grid(na0,na3,na2,na1,na4,na5, ncol=3)
ggsave("C:Food web idea//Data by person//Plots//Richness_nut//Richness_d15n_area_category.png", width=30, height=20, unit="cm")

nam0<-ggplot(by_isl_master, aes(y=total_richness, x=d15n, colour=size.cat, fill=size.cat))+geom_point()+geom_smooth(aes(fill=size.cat),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
nam1<-ggplot(by_isl_master, aes(y=insect_richness, x=d15n, colour=size.cat, fill=size.cat))+geom_point()+geom_smooth(aes(fill=size.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,350)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+theme(legend.position="none")
nam2<-ggplot(by_isl_master, aes(y=tree_richness, x=d15n, colour=size.cat, fill=size.cat))+geom_point()+geom_smooth(aes(fill=size.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,10)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
nam3<-ggplot(na.omit(by_isl_master), aes(y=plant_richness, x=d15n, colour=size.cat, fill=size.cat))+geom_point()+geom_smooth(aes(fill=size.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,60)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
nam4<-ggplot(by_isl_master, aes(y=bird.richness, x=d15n, colour=size.cat, fill=size.cat))+geom_point()+geom_smooth(aes(fill=size.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,25)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
nam5<-ggplot(by_isl_master, aes(y=mammal_richness, x=d15n, colour=size.cat, fill=size.cat))+geom_point()+geom_smooth(aes(fill=size.cat),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
plot_grid(nam0,nam3,nam2,nam1,nam4,nam5, ncol=3)
ggsave("C:Food web idea//Data by person//Plots//Richness_nut//Richness_d15n_area_category_3.png", width=30, height=20, unit="cm")


nan0<-ggplot(by_isl_master, aes(y=total_richness, x=log_Area, colour=d15n.cat, fill=d15n.cat))+geom_point()+geom_smooth(aes(fill=d15n.cat),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.15))
nan1<-ggplot(by_isl_master, aes(y=insect_richness, x=log_Area, colour=d15n.cat, fill=d15n.cat))+geom_point()+geom_smooth(aes(fill=d15n.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,350)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+theme(legend.position="none")
nan2<-ggplot(by_isl_master, aes(y=tree_richness, x=log_Area, colour=d15n.cat, fill=d15n.cat))+geom_point()+geom_smooth(aes(fill=d15n.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,10)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
nan3<-ggplot(by_isl_master, aes(y=plant_richness, x=log_Area, colour=d15n.cat, fill=d15n.cat))+geom_point()+geom_smooth(aes(fill=d15n.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,60)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
nan4<-ggplot(by_isl_master, aes(y=bird.richness, x=log_Area, colour=d15n.cat, fill=d15n.cat))+geom_point()+geom_smooth(aes(fill=d15n.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,25)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
nan5<-ggplot(by_isl_master, aes(y=mammal_richness, x=log_Area, colour=d15n.cat, fill=d15n.cat))+geom_point()+geom_smooth(aes(fill=d15n.cat),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
plot_grid(nan0,nan3,nan2,nan1,nan4,nan5, ncol=3)
ggsave("C:Food web idea//Data by person//Plots//Richness_nut//Richness_area_d15n_category.png", width=30, height=20, unit="cm")


ggplot(by_isl_master, aes(y=shrub_richness, x=d15n))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
ggplot(by_isl_master, aes(y=shrub_cover, x=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(by_isl_master, aes(y=herb_richness, x=d15n))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
ggplot(by_isl_master, aes(y=herb_cover, x=d15n))+geom_point()+geom_smooth(method="lm")

ggplot(by_isl_master, aes(y=tree_richness, x=d15n))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
ggplot(by_isl_master, aes(y=tree_abundance, x=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(by_isl_master, aes(y=sum_basal, x=d15n))+geom_point()+geom_smooth(method="lm")

ggplot(by_isl_master, aes(y=NDVI_mean, x=d15n))+geom_point()+geom_smooth(method="lm")

ggplot(by_isl_master, aes(y=plant_richness, x=n))+geom_point()+geom_smooth(method="lm")
ggplot(by_isl_master, aes(y=pc1, x=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(by_isl_master, aes(y=plant_evenness, x=n))+geom_point()+geom_smooth(method="lm")
ggplot(by_isl_master, aes(y=d15n, x=n))+geom_point()+geom_smooth(method="lm")



# Plotting insects by species ---------------------------------------------
ggplot(by_isl_master, aes(y=insect_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(by_isl_master, aes(y=insect_birdfood_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(by_isl_master, aes(y=insect_detritivore_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(by_isl_master, aes(y=insect_carnivore_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(by_isl_master, aes(y=insect_herbivore_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(by_isl_master, aes(y=insect_parasite_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(by_isl_master, aes(y=insect_omnivore_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))



ggplot(by_isl_master, aes(y=log(insect_abs_abundance+1), x=d15n))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(by_isl_master, aes(y=(insect_beat_av_abundance), x=d15n))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(by_isl_master, aes(y=(insect_pitfall_av_abundance), x=d15n))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
ggplot(by_isl_master, aes(y=(insect_evenness), x=d15n))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))



##### Plotting insects
insects0<-ggplot(by_isl_master, aes(y=collembola_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
insects1<-ggplot(by_isl_master, aes(y=diptera_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+theme(legend.position="none")
insects2<-ggplot(by_isl_master, aes(y=miscinsects_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
insects3<-ggplot(by_isl_master, aes(y=hymenoptera_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
insects4<-ggplot(by_isl_master, aes(y=spiders_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
insects5<-ggplot(by_isl_master, aes(y=myriapod_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
insects6<-ggplot(by_isl_master, aes(y=gastropoda_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
insects7<-ggplot(by_isl_master, aes(y=crustacea_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")

plot_grid(insects0,insects1,insects6,insects3,insects4,insects5,insects2,insects7,  ncol=4)
ggsave("C:Food web idea//Data by person//Plots//Species_nut//Insects_d15n.png")



abund.insects0<-ggplot(by_isl_master, aes(y=log(collembola_abundance), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
abund.insects1<-ggplot(by_isl_master, aes(y=diptera_abundance, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+theme(legend.position="none")
abund.insects2<-ggplot(by_isl_master, aes(y=log(miscinsects_abundance), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
abund.insects3<-ggplot(by_isl_master, aes(y=log(hymenoptera_abundance), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
abund.insects4<-ggplot(by_isl_master, aes(y=spiders_abundance, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
abund.insects5<-ggplot(by_isl_master, aes(y=log(myriapod_abundance), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
abund.insects6<-ggplot(by_isl_master, aes(y=gastropoda_abundance, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
#abund.insects7<-ggplot(by_isl_master, aes(y=log(beetles_abundance), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")

plot_grid(abund.insects0,abund.insects1,abund.insects6,abund.insects3,abund.insects4,abund.insects5,abund.insects2, ncol=4)
ggsave("C:Food web idea//Data by person//Plots//Species_nut//Insects_Abundance_d15n.png")


##### Plotting insects
insects0<-ggplot(by_isl_master, aes(y=collembola_richness, x=log_Area))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
insects1<-ggplot(by_isl_master, aes(y=diptera_richness, x=log_Area))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+theme(legend.position="none")
insects2<-ggplot(by_isl_master, aes(y=miscinsects_richness, x=log_Area))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
insects3<-ggplot(by_isl_master, aes(y=hymenoptera_richness, x=log_Area))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
insects4<-ggplot(by_isl_master, aes(y=spiders_richness, x=log_Area))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
insects5<-ggplot(by_isl_master, aes(y=myriapod_richness, x=log_Area))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
insects6<-ggplot(by_isl_master, aes(y=gastropoda_richness, x=log_Area))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
#insects7<-ggplot(by_isl_master, aes(y=beetles_richness, x=log_Area))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")

plot_grid(insects0,insects1,insects6,insects3,insects4,insects5,insects2, ncol=4)
ggsave("C:Food web idea//Data by person//Plots//Species_nut//Insects_Area.png")



# Multivariate ordination for marine evidence -----------------------------


#head(by_isl_master)

write.csv(by_isl_master, "C:Food web idea//Data by person//Owen's data/by_isl_master.csv")


feathers.key<-read.csv("C:Food web idea//Data by person//Deb.data//banding-all.csv", header=TRUE, sep=",")
#head(feathers.key)

i.feathers.all<-read.csv("C:Food web idea//Data by person//Deb.data//i-feathers-all (1).csv")
#head(i.feathers.all)
length(unique(i.feathers.all$unq_isl))

feathers.merge<-merge(feathers.key, i.feathers.all, by="band")
#head(feathers.merge)


which( colnames(feathers.merge)=="island" )
which( colnames(feathers.merge)=="d13c" )
names(feather.)
feather.simple<-feathers.merge[,c(37,43)]
#head(feather.simple)

feather.simple.means<- feather.simple %>% group_by(island)%>% summarise_all(funs(mean))
length(unique(feather.simple.means$island))
names(feather.simple.means)[1]<-"unq_isl"

by_isl_multivariate<-merge(feather.simple.means, by_isl_master[, c(1, 2,3,48,49,10)])
#head(by_isl_multivariate)
by_isl_multivariate<-na.omit(by_isl_multivariate)
length(by_isl_multivariate$unq_isl)
#56 islands

#head(owen_key)
by_isl_multivariate_env<-merge(unique(owen_key[,c(2,3)]), by_isl_multivariate)
#head(by_isl_multivariate_env)


#head(by_isl_master)
which( colnames(by_isl_master)=="log_Area" )
by_isl_feathers<-merge(feather.simple.means, by_isl_master[, c(1, 2,3,46,48,49,10)])


ggplot(by_isl_feathers, aes(x=log_Area, y=d13c))+geom_point()+geom_smooth(method="lm")





size.cat.frame<-merge(Veg_means_by_island, by_isl_multivariate_env)
#head(size.cat.frame)
size.cat.frame<-size.cat.frame[,c(1,8)]
size.cat.frame<-unique(size.cat.frame)



library(vegan)
library(ggplot2)
library(betapart)
library(bipartite)
library(car)
library(fitdistrplus)

theme_classic()


row.names(by_isl_multivariate_env)<-by_isl_multivariate_env$unq_isl
row.names(by_isl_multivariate)<-by_isl_multivariate$unq_isl

by_isl_multivariate<-by_isl_multivariate[,-1]

by_isl_multivariate_env$node<-as.factor(by_isl_multivariate_env$node)
by_isl_multivariate_env$size.cat<-size.cat.frame$size.cat





cbbPalette.all.9<-  c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "black")

###CONSTRAINED Ordination

capscale_plot_marine<- function(m, colorby){
  colorby<-factor(colorby) #Convert to factor (just in case it isn't already)
  cols <- cbbPalette.all.9#vector of colors needed
  shapesies<-c(19,19,19,19,19,19,19,19,19)
  ordiplot(m, display = c("sites"), type = "n")
  #ordiellipse(m,groups=by_isl_multivariate_env$node,col=cbbPalette.all.9, draw="polygon",label=T, kind="sd")
  points(m, col = cols[colorby], pch = shapesies[colorby], cex=1.5)
  legend("topright", title ="Size of island", legend=levels(colorby), col=cols, pch = shapesies, cex=1.5)
}



multi.marine<-capscale(by_isl_multivariate~ node,by_isl_multivariate_env , distance="euclidean")
capscale_plot_marine(multi.marine, colorby=by_isl_multivariate_env$size.cat)


?princomp
multi.marine.pca<-princomp(by_isl_multivariate)
biplot(multi.marine.pca)

multi.marine.rda<-rda(by_isl_multivariate)
biplot(multi.marine.rda)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
theme_set(theme_classic())

multi.marine.pca<-prcomp(by_isl_multivariate, scale=TRUE)
ggbiplot(multi.marine.pca)


th
ggbiplot(multi.marine.pca, ellipse = TRUE, groups = by_isl_multivariate_env$size.cat)

ggbiplot(multi.marine.pca, ellipse = TRUE, groups = by_isl_multivariate_env$node)



# Old code ----------------------------------------------------------------


### not sure about this::: 
#Adding in information about seaweed, eagles, otters
wrack.sara<-read.csv("C:Food web idea//Data by person//Sara's data//wrack.isotopes.cn.csv")
wrack.key<-read.csv("C:Food web idea//Data by person//Sara's data//wrack.key.csv")
ang.seaweed<-read.csv("C:Food web idea//Data by person//Ang's data//chokedpass_macrophytes_AMO2015.csv", header=TRUE, sep=",")
becky.eagles<-read.csv("C:Food web idea//Data by person//Becky.data//becky.isotopes.csv", header=TRUE, sep=",")
otter_chris<-read.csv("C:Food web idea//Data by person//Chris.data//otter_sia.csv")


#otter isotopes
#head(otter_chris)
names(otter_chris)[6]<-"d13c"
names(otter_chris)[8]<-"d15n"
names(otter_chris)[13]<-"group"
otter_chris$broad_group<-"otter"
otter_chris_onsite_soil<-otter_chris[otter_chris$group=="AC" & otter_chris$SIASample=="Soil",]
otter_chris_onsite_soil$group<-"otter"
otter_chris_onsite_soil$Node<-"CV"


#seaweed isotopes
names(ang.seaweed)[8]<-"d13c"
names(ang.seaweed)[9]<-"d15n"
ang.seaweed$Dryness<-"L"
ang.seaweed$person<-"Ang"
names(wrack.sara)[2]<-"d13c"
names(wrack.sara)[5]<-"d15n"
wrack.sara$group<-"wrack"
wrack.merge<-merge(wrack.key, wrack.sara, by="Sample.ID")
names(wrack.merge)[3]<-"Node"
wrack.merge$person<-"Sara"


#eagles isotopes
becky.eagles.tree<-becky.eagles[becky.eagles$type=="eagle",]
becky.eagles.tree$group<-"eagles"
names(becky.eagles.tree)[4]<-"d13c"
names(becky.eagles.tree)[5]<-"d15n"


#### plotting soil at 0m, on sites without likely wrack (machine learning)
wrackfree<-read.csv("C:Food web idea//Data by person//Pat.data//wrackfree.csv", header=TRUE, sep=",")
#head(wrackfree)



ggplot(wrackfree, aes(x=n15, y=litter))+geom_point()+geom_smooth( method="glm", method.args = list(family = "binomial"))
ggplot(wrackfree, aes(x=n15, y=moss))+geom_point()+geom_smooth( method="glm", method.args = list(family = "binomial"))
ggplot(wrackfree, aes(x=n15, y=rock_soil_etc))+geom_point()+geom_smooth( method="glm", method.args = list(family = "binomial"))
ggplot(wrackfree, aes(x=n15, y=midi))+geom_point()+geom_smooth( method="glm", method.args = list(family = "binomial"))
ggplot(wrackfree, aes(x=n15, y=gash))+geom_point()+geom_smooth( method="glm", method.args = list(family = "binomial"))





