setwd("C:/Users/Norah/Dropbox/Projects/100-islands/Food web idea")
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
library(viridis)
library(cowplot)
install.packages("forcats")
library(forcats)





# Loading soil data -------------------------------------------------------

##### DEB
#Deb's soil data and Deb's shore dist
i.soil.all<-read.csv("C:Data by person//Deb.data//i-soil-all.csv")
#this is isotopes

shoredist.deb<-read.csv("C:Data by person//Deb.data//shoredist.csv")
#this is the point count to distance to shore data

pointcount.gps<-read.csv("C:Data by person//Deb.data//pointcounts.csv")
head(pointcount.gps)
pointcount.gps$pcid<-gsub(" ", "", pointcount.gps$pcid, fixed = TRUE)
pointcount.gps<-pointcount.gps[,c(3,16,17)]
pointcount.gps<-pointcount.gps[!duplicated(pointcount.gps$pcid),]
#sometimes taken twice...  
  
shoredist.deb<-merge(shoredist.deb, pointcount.gps, by.x="pcid", all=TRUE)
head(shoredist.deb)
length(i.soil.all$sample.id)

soil.deb<-merge(i.soil.all, shoredist.deb, by.x="pcid", all=TRUE)
head(soil.deb)
names(soil.deb)[16]<-"shore_dist"
names(soil.deb)[11]<-"unq_isl"
names(soil.deb)[4]<-"c"
names(soil.deb)[5]<-"n"
names(soil.deb)[6]<-"s"
names(soil.deb)[7]<-"cn"
names(soil.deb)[1]<-"unq_plot"

length(soil.deb$unq_plot)
head(soil.deb)


#####OWEN
#owen's isotope data by plot
soil_clean<-read.csv("C:Data by person//Owen's data//soil_clean.csv", header=TRUE, sep=",")
head(soil_clean)
length((soil_clean$unq_plot))

#duplicated plots: 
soil_clean[duplicated(soil_clean$unq_plot),]
#let's keep them in for now

names(soil_clean)[6]<-"d13c"
names(soil_clean)[7]<-"d15n"

#Owen's key data
owen_key<-read.csv("C:Data by person//Owen's data//key_mod.csv", header=TRUE, sep=",")
head(owen_key)
length(unique(owen_key$unq_isl))

#Owen's plot-level soil info - moisture, slope etc
hakai_plot<-read.csv("C:Data by person//Owen's data//hakai_plot.csv", header=TRUE, sep=",")
names(hakai_plot)[3]<-"plant.richness"
head(hakai_plot)

owen_key_expanded<-merge(owen_key, hakai_plot, by.x="unq_plot", all=TRUE)
head(owen_key_expanded)
length(unique(owen_key_expanded$unq_isl))

#Add in the GPS coordinates
owen_coords<-read.csv("C:Data by person//Becky.data//ofwi_tran_coords.csv", header=TRUE, sep=",")
head(owen_coords)

owen_coords$unq_tran<- paste(owen_coords$unq_isl,owen_coords$TRANSECT)
owen_coords$unq_tran<-gsub(" ", "", owen_coords$unq_tran, fixed = TRUE)

owen_coords<-owen_coords[,c(3,4, 14)]
head(owen_coords)
names(owen_coords)[1]<-"easting"
names(owen_coords)[2]<-"northing"

owen_key_expanded<-merge(owen_key_expanded, owen_coords, by="unq_tran", all=TRUE)
head(owen_key_expanded)


#put isotope data together with the key
soil_merge_isl<-merge(soil_clean, owen_key_expanded, by.x="unq_plot")
head(soil_merge_isl)


soil_merge_isl[duplicated(soil_merge_isl$unq_plot),]
length(unique(soil_merge_isl$unq_isl))
#there are a bunch of extras but will wait first to see how to deal with them ... 


# Combining Owen and Deb's soil data --------------------------------------

paste(
which( colnames(soil_merge_isl)=="unq_plot" ),
which( colnames(soil_merge_isl)=="unq_isl" ),
which( colnames(soil_merge_isl)=="shore_dist" ),
which( colnames(soil_merge_isl)=="d13c" ),
which( colnames(soil_merge_isl)=="d15n" ),
which( colnames(soil_merge_isl)=="c" ),
which( colnames(soil_merge_isl)=="n" ),
which( colnames(soil_merge_isl)=="s" ),
which( colnames(soil_merge_isl)=="cn" ),
# which( colnames(soil_merge_isl)=="pc1" )
# which( colnames(soil_merge_isl)=="plant.richness" )
# which( colnames(soil_merge_isl)=="fs_pc1" )
# which( colnames(soil_merge_isl)=="sm_av" )
# which( colnames(soil_merge_isl)=="slope" )
 which( colnames(soil_merge_isl)=="node" ),
 which( colnames(soil_merge_isl)=="easting" ),
 which( colnames(soil_merge_isl)=="northing" ), sep=","
)

paste(
which( colnames(soil.deb)=="unq_plot" ),
which( colnames(soil.deb)=="unq_isl" ),
which( colnames(soil.deb)=="shore_dist"),
which( colnames(soil.deb)=="d13c" ),
which( colnames(soil.deb)=="d15n" ),
which( colnames(soil.deb)=="c" ),
which( colnames(soil.deb)=="n" ),
which( colnames(soil.deb)=="s" ),
which( colnames(soil.deb)=="cn" ),
# which( colnames(soil.deb)=="pc1" )
# which( colnames(soil.deb)=="plant.richness" )
# which( colnames(soil.deb)=="fs_pc1" )
# which( colnames(soil.deb)=="sm_av" )
# which( colnames(soil.deb)=="slope" )
 which( colnames(soil.deb)=="node" ),
 which( colnames(soil.deb)=="easting" ),
 which( colnames(soil.deb)=="northing" ), sep=","
)

soil_owen_deb<-rbind(soil_merge_isl[,c(1,12,15,6,7,3,2,5,4,11,21,22)], soil.deb[,c(1,11,16,2,3,4,5,6,7,15,17,18)])
head(soil_owen_deb)

length(soil.deb$unq_plot)
length(soil_merge_isl$unq_plot)
length(soil_owen_deb$unq_plot)
length(unique(soil_owen_deb$unq_isl))
#this just adds the two together... 


#now we want one value per island: 

soil_owen_deb_by_isl<- soil_owen_deb %>%  group_by(unq_isl)%>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(soil_owen_deb_by_isl)
length(soil_owen_deb_by_isl$unq_isl)

#we could change this to weight different points differently but here just a mean of all samples on the island...
#including both debv and owen's'




# Adding habitat class and island characteristics -------------------------

habitat_class<-read.csv("C:Data by person//Pat.data//HabitatClass.csv", header=TRUE, sep=",")
head(habitat_class)
length(habitat_class$unq_isl)

island_data_wiebe<-read.csv("C:Data by person//Pat.data//Islands_Master_Vegetation2017.csv", header=TRUE, sep=",")
head(island_data_wiebe)
names(island_data_wiebe)[1]<-"unq_isl"

habitat_class<-merge(habitat_class, island_data_wiebe[,c(1,19)])

habitat_soil_by_isl<-merge(soil_owen_deb_by_isl, habitat_class, by.x="unq_isl", all=TRUE)
head(habitat_soil_by_isl)
str(habitat_soil_by_isl)



# Becky's tree density and diversity data --------------------------

becky_trees<-read.csv("C:Data by person//Becky.data//data_tree_abund_cover.csv", header=TRUE, sep=",")
becky_trees<-becky_trees[,-1]
head(becky_trees)
names(becky_trees)[1]<-"unq_isl"
names(becky_trees)[3]<-"species"
becky_trees<-as.data.frame(becky_trees)

becky_trees_wide <-becky_trees %>% group_by(unq_isl, species) %>% 
  summarise(sum_abundance = mean(abund.ab, na.rm=TRUE)) %>% 
  spread(species, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(becky_trees_wide)


#just getting absolute abundance ... what about relative? 
#what about basal area? cover? what should be included? 
becky_trees_wide_richness<-becky_trees_wide[,1]
becky_trees_wide_richness$tree_richness<-specnumber(becky_trees_wide[,-1])
becky_trees_wide_richness$tree_diversity<-diversity(becky_trees_wide[,-1], index="shannon")
becky_trees_wide_richness$tree_evenness<-becky_trees_wide_richness$tree_diversity/(log(becky_trees_wide_richness$tree_richness))



becky_trees_wide_richness$tree_abundance<-rowSums(becky_trees_wide[,-1],na.rm = TRUE)
head(becky_trees_wide_richness)

habitat_soil_by_isl<-merge(habitat_soil_by_isl,becky_trees_wide_richness, by.x="unq_isl", all=TRUE)
head(habitat_soil_by_isl)
length(habitat_soil_by_isl$unq_isl)


# Adding plant cover and richness -----------------------------------------

#this loads data from "Habitation data" R script

longform_plant_percentcover<-read.csv("C:Data by person//Kalina.data/Deb_Owen_veg_combined_complete_filled.csv", header=TRUE, sep=",")
longform_plant_percentcover<-longform_plant_percentcover[,-c(1)]
head(longform_plant_percentcover)

longform_plant_percentcover$unq_isl<-fct_explicit_na(longform_plant_percentcover$unq_isl)

longform_plant_percentcover2 <- longform_plant_percentcover[,c(1:7)]%>% 
  group_by(unq_isl,species) %>% summarise(cover_mean = mean(cover, na.rm=TRUE)) %>% 
  spread(species, cover_mean)%>%  replace(is.na(.), 0)

#copmmented outt his section, I think the source file changed so there is no longer random "marine" etc remains. Need to check habitation file to see what happened
# head(longform_plant_percentcover2)
# 
# longform_plant_percentcover2$marine<-longform_plant_percentcover2$`marine remains`+longform_plant_percentcover2$`abalone shell`+longform_plant_percentcover2$driftwood+longform_plant_percentcover2$shell
# longform_plant_percentcover2$free_space<-longform_plant_percentcover2$bare+longform_plant_percentcover2$`bare ground`+longform_plant_percentcover2$`sandy soil`+longform_plant_percentcover2$`o soil`+longform_plant_percentcover2$gravel+longform_plant_percentcover2$rock
# longform_plant_percentcover2$grass<-longform_plant_percentcover2$`grass 1`+longform_plant_percentcover2$`grass sp`
# longform_plant_percentcover2$sedge_final<-longform_plant_percentcover2$`sedge 1`+longform_plant_percentcover2$`sedge sp`+longform_plant_percentcover2$sedge+longform_plant_percentcover2$sedge1
# longform_plant_percentcover2$unknown_forb<-longform_plant_percentcover2$'unk forb'+ longform_plant_percentcover2$'unidentified forb'
# longform_plant_percentcover2$unknown_lily<-longform_plant_percentcover2$'unk lily'+ longform_plant_percentcover2$'unk lily sp'
# longform_plant_percentcover2$unknown_monocot<-longform_plant_percentcover2$'unk mono'+ longform_plant_percentcover2$'unk monocot'
# 
# 
# 
# paste(
# which( colnames(longform_plant_percentcover2)=="marine" ),
# which( colnames(longform_plant_percentcover2)=="free_space" ),
# which( colnames(longform_plant_percentcover2)=="bare" ),
# which( colnames(longform_plant_percentcover2)=="bare ground" ),
# which( colnames(longform_plant_percentcover2)=="woody debris" ),
# which( colnames(longform_plant_percentcover2)=="wood" ),
# which( colnames(longform_plant_percentcover2)=="sandy soil" ),
# which( colnames(longform_plant_percentcover2)=="o soil" ),
# which( colnames(longform_plant_percentcover2)=="shell" ),
# which( colnames(longform_plant_percentcover2)=="gravel" ),
# which( colnames(longform_plant_percentcover2)=="rock" ),
# which( colnames(longform_plant_percentcover2)=="marine remains" ),
# which( colnames(longform_plant_percentcover2)=="abalone shell" ),
# which( colnames(longform_plant_percentcover2)=="driftwood" ),
# which( colnames(longform_plant_percentcover2)=="feather" ),
# which( colnames(longform_plant_percentcover2)=="grass 1" ),
# which( colnames(longform_plant_percentcover2)=="grass sp" ),
# which( colnames(longform_plant_percentcover2)=="sedge" ),
# which( colnames(longform_plant_percentcover2)=="sedge1" ),
# which( colnames(longform_plant_percentcover2)=="sedge 1" ),
# which( colnames(longform_plant_percentcover2)=="sedge sp" ),
# which( colnames(longform_plant_percentcover2)=="unk forb" ),
# which( colnames(longform_plant_percentcover2)=="unidentified forb" ),
# which( colnames(longform_plant_percentcover2)=="unk lily" ),
# which( colnames(longform_plant_percentcover2)=="unk lily sp" ),
# which( colnames(longform_plant_percentcover2)=="unk mono" ),
# which( colnames(longform_plant_percentcover2)=="unk monocot" ),sep=","
# )
# 
# longform_plant_percentcover_species<-longform_plant_percentcover2[,-c(1,172,173,13,14,171,170,115,88,122,61,109,81,2,39, 48, 59, 60,118,121,119,120,137,134,151,153,154,157)]
# head(longform_plant_percentcover_species)


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
head(longform_plant_percentcover_species_isl)

which( colnames(longform_plant_percentcover2)=="gash" )
which( colnames(longform_plant_percentcover2)=="midi" )


longform_plant_percentcover3_isl<-longform_plant_percentcover2[,c(1,37,52)]
head(longform_plant_percentcover3_isl)
longform_plant_percentcover3_isl$plant_richness<-specnumber(longform_plant_percentcover_species_isl[,-c(1)])
longform_plant_percentcover3_isl$plant_shannon.diversity<-diversity(longform_plant_percentcover_species_isl[,-c(1)], index="shannon")
longform_plant_percentcover3_isl$plant_evenness<-longform_plant_percentcover3_isl$plant_shannon.diversity/(log(longform_plant_percentcover3_isl$plant_richness))
longform_plant_percentcover3_isl$total_cover<-rowSums(longform_plant_percentcover_species_isl[,-c(1)], na.rm=TRUE)

longform_plant_percentcover3_isl$shrub_richness<-specnumber(longform_plant_percentcover2_isl_shrub[,-c(1)])
longform_plant_percentcover3_isl$shrub_cover<-rowSums(longform_plant_percentcover2_isl_shrub[,-c(1)], na.rm=TRUE)
longform_plant_percentcover3_isl$herb_richness<-specnumber(longform_plant_percentcover2_isl_herb[,-c(1)])
longform_plant_percentcover3_isl$herb_cover<-rowSums(longform_plant_percentcover2_isl_herb[,-c(1)], na.rm=TRUE)

head(longform_plant_percentcover3_isl)


head(soil_merge)

habitat_veg_soil_by_isl<-merge(soil_merge[,-8], longform_plant_percentcover3_isl, by="unq_isl", all=TRUE)
head(habitat_veg_soil_by_isl)



# Adding in bird richness -------------------------------------------------

birdrichness<-read.csv("C:Data by person//Deb.data//bird-summary.csv", header=TRUE, sep=",")
head(birdrichness)
names(birdrichness)[1]<-"unq_isl"
names(birdrichness)[6]<-"bird.richness"
names(birdrichness)[4]<-"bird.density"
birdrichness$bird.evenness<-birdrichness$evenness/(log(birdrichness$bird.richness))
head(birdrichness)

habitat_veg_bird_soil_by_isl<-merge(habitat_veg_soil_by_isl, birdrichness[,c(1,4,6,11)], by.x="unq_isl", all=TRUE)
head(habitat_veg_bird_soil_by_isl)



# Adding in wrack richness and wrack habitat ------------------------------------------------

seawrack_key<-read.csv("C:Data by person//Sara's data//seawrack_spatial_mod.csv", header=TRUE, sep=",")
head(seawrack_key)
seawrack_key$ISLAND<-sprintf("%02d",seawrack_key$ISLAND)
seawrack_key$unq_isl <- paste(seawrack_key$NODE,seawrack_key$ISLAND)
seawrack_key$unq_isl<-gsub(" ", "", seawrack_key$unq_isl, fixed = TRUE)
head(seawrack_key)


sara_habitat<-read.csv("C:Data by person//Sara's data//sara_habitat.csv", header=TRUE, sep=",")
head(sara_habitat)
sara_habitat_merged<-merge(sara_habitat, seawrack_key, by.y="unq_tran", all=TRUE)
head(sara_habitat_merged)

which( colnames(sara_habitat_merged)=="unq_isl" )
which( colnames(sara_habitat_merged)=="unq_tran" )
which( colnames(sara_habitat_merged)=="MEAN_egarea2k" )
which( colnames(sara_habitat_merged)=="MEAN_kparea2k" )
which( colnames(sara_habitat_merged)=="MEAN_rockarea2000" )
which( colnames(sara_habitat_merged)=="sum_2km" )
which( colnames(sara_habitat_merged)=="SLOPE" )
which( colnames(sara_habitat_merged)=="WIDTH" )
which( colnames(sara_habitat_merged)=="HAB2000" )
which( colnames(sara_habitat_merged)=="SITE_SUM" )
which( colnames(sara_habitat_merged)=="sum_25m" )
which( colnames(sara_habitat_merged)=="sum_50m" )
which( colnames(sara_habitat_merged)=="sum_100m" )
which( colnames(sara_habitat_merged)=="sum_250m" )
which( colnames(sara_habitat_merged)=="sum_100m" )
which( colnames(sara_habitat_merged)=="MEAN_egarea100" )




sara_habitat_merged_simple<-sara_habitat_merged[,c(1,62,8,19,30,41,56,57,60,61, 35, 36, 37, 38, 4)]
head(sara_habitat_merged_simple)
sara_habitat_merged_by_isl <- sara_habitat_merged_simple %>% group_by(unq_isl)%>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(sara_habitat_merged_by_isl )
length(sara_habitat_merged_by_isl$unq_isl)
str(sara_habitat_merged_by_isl )


#### Seaweed composition
sara_composition<-read.csv("C:Data by person//Sara's data//sara_composition.csv", header=TRUE, sep=",")
head(sara_composition)
#this is by transect

#make it by island
sara_composition_means <- sara_composition %>%  
  group_by(unq_isl)%>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(sara_composition_means)

which( colnames(sara_composition_means)=="SITE_SUM" )
#take out site sum and island and unq_isl

sara_composition_means_richness<-sara_composition_means[,1]
sara_composition_means_richness$wrack_richness<-specnumber(sara_composition_means[,-c(1,2,48)])
sara_composition_means_richness$site_mean_by_isl<-sara_composition_means$SITE_SUM

sara_composition_sums<-sara_composition %>%  
  group_by(unq_isl)%>% summarise_if(is.numeric, sum, na.rm=TRUE)
head(sara_composition_sums)

sara_composition_means_richness$site_sum_by_isl<-sara_composition_sums$SITE_SUM


# add in diversity to full wrack story
sara_habitat_merged_by_isl<-merge(sara_habitat_merged_by_isl, sara_composition_means_richness, by.x="unq_isl", all=TRUE)
head(sara_habitat_merged_by_isl)


#add in wrack to birds, veg, habitat
habitat_veg_bird_wrack_soil_by_isl<-merge(habitat_veg_bird_soil_by_isl, sara_habitat_merged_by_isl, by.x="unq_isl", all=TRUE)
head(habitat_veg_bird_wrack_soil_by_isl)



# Adding in eagles and ravens pointcounts ---------------------------------------------


ravens <- read.csv("C:Data by person//Deb.data/ravens.csv")

cora.isls <- unique(ravens$island)
cora.isls <- as.data.frame(cora.isls)
cora.isls$ravens <- 1
names(cora.isls) <- c("unq_isl", "ravens")

by_isl_master<-merge(habitat_veg_bird_wrack_soil_by_isl, cora.isls, by.x="unq_isl", all=TRUE)

eagles <- read.csv("C:Data by person//Deb.data/eagles.csv")
baea.isls <- unique(eagles$island)
baea.isls <- as.data.frame(baea.isls)
baea.isls$eagles <- 1
names(baea.isls) <- c("unq_isl", "eagles")
head(baea.isls)


by_isl_master <- merge(by_isl_master, baea.isls, all.x = TRUE)

by_isl_master$ravens[is.na(by_isl_master$ravens)] <- 0
by_isl_master$eagles[is.na(by_isl_master$eagles)] <- 0
head(by_isl_master)



#can do the same thing for SOSP if we want ... 
# pc<- read.csv("C:Data by person//Deb.data/pointcounts.csv")
# head(pc)
# sosp <- pc[pc$spp == "SOSP", ]
# head(sosp)
# 
# sosp.isls <- unique(sosp$island)
# 
# sosp.isls <- as.data.frame(sosp.isls)
# sosp.isls$sosp <- 1
# names(sosp.isls) <- c("unq_isl", "sosp")
# head(sosp.isls)
# by_isl_master_2 <- merge(sosp.isls, by_isl_master, all.y = TRUE)
# by_isl_master_2$sosp[is.na(by_isl_master_2$sosp)] <- 0
# View(by_isl_master_2)





# Chris insects -----------------------------------------------------------
# 
# chris_beeetles_2015<-read.csv("C:Data by person//Chris.data//ce_observation2015.csv", header=TRUE, sep=",")
# chris_beeetles_2016<-read.csv("C:Data by person//Chris.data//ce_observation2016.csv", header=TRUE, sep=",")
# chris_beeetles_2017<-read.csv("C:Data by person//Chris.data//ce_observation2017.csv", header=TRUE, sep=",")
# 
# 
# str(chris_beeetles_2015)
# str(chris_beeetles_2016)
# str(chris_beeetles_2017)
# chris_beetles<-rbind(chris_beeetles_2015[,-6], chris_beeetles_2016[,-6], chris_beeetles_2017[,-6])
# head(chris_beetles)
# 
# chris_beetles$unq_isl<-gsub('.{1}$', '',chris_beetles$Trapline )
# 
# chris_beetles_wide <-chris_beetles %>% group_by(unq_isl, Trap, SpeciesID) %>% 
#   summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
#   group_by(unq_isl, SpeciesID) %>% 
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
chris_collembola<-read.csv("C:Data by person//Chris.data//collembola_ey.csv", header=TRUE, sep=",")
head(chris_collembola)


chris_collembola$Island.Number<-sprintf("%02d", chris_collembola$Island.Number)
chris_collembola$unq_isl <- paste(chris_collembola$Island,chris_collembola$Island.Number)
chris_collembola$unq_isl<-gsub(" ", "", chris_collembola$unq_isl, fixed = TRUE)

#incorporate Direction if need to have unq_tran... but if want to just get per island average across 5 points
#also trap type .. maybe just combine beat and pitfall = SUM
#okay here we collapsed transects (did mean) and then summed across bet and pitfall ... not sure which is the bvest way to do it
#It's fine for richness but might need to fine tune for abudnance

chris_collembola_wide <-chris_collembola %>% group_by(unq_isl, Trap.Type, Identification) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_isl, Identification) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Identification, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_collembola_wide)
chris_collembola_wide<-chris_collembola_wide[,-c(2,3)]

chris_collembola_wide_richness<-chris_collembola_wide[,1]
chris_collembola_wide_richness$collembola_richness<-specnumber(chris_collembola_wide[,-1])
chris_collembola_wide_richness$collembola_abundance<-rowSums(chris_collembola_wide[,-1],na.rm = TRUE)
head(chris_collembola_wide_richness)


##hymenoptera
chris_hymenoptera<-read.csv("C:Data by person//Chris.data//hymenoptera_ey.csv", header=TRUE, sep=",")
head(chris_hymenoptera)

chris_hymenoptera$Island.Number<-sprintf("%02d", chris_hymenoptera$Island.Number)
chris_hymenoptera$unq_isl <- paste(chris_hymenoptera$Island,chris_hymenoptera$Island.Number)
chris_hymenoptera$unq_isl<-gsub(" ", "", chris_hymenoptera$unq_isl, fixed = TRUE)

chris_hymenoptera_wide <-chris_hymenoptera %>% group_by(unq_isl, Trap.Type, Identification) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_isl, Identification) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Identification, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_hymenoptera_wide)

chris_hymenoptera_wide_richness<-chris_hymenoptera_wide[,1]
chris_hymenoptera_wide_richness$hymenoptera_richness<-specnumber(chris_hymenoptera_wide[,-1])
chris_hymenoptera_wide_richness$hymenoptera_abundance<-rowSums(chris_hymenoptera_wide[,-1],na.rm = TRUE)
head(chris_hymenoptera_wide_richness)

##diptera
chris_diptera<-read.csv("C:Data by person//Chris.data//diptera_ey.csv", header=TRUE, sep=",")
head(chris_diptera)

chris_diptera$Island.Number<-sprintf("%02d", chris_diptera$Island.Number)
chris_diptera$unq_isl <- paste(chris_diptera$Island,chris_diptera$Island.Number)
chris_diptera$unq_isl<-gsub(" ", "", chris_diptera$unq_isl, fixed = TRUE)

chris_diptera_wide <-chris_diptera %>% group_by(unq_isl, Trap.Type, Identification) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_isl, Identification) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Identification, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_diptera_wide)

chris_diptera_wide<-chris_diptera_wide[,-c(2,3)]

chris_diptera_wide_richness<-chris_diptera_wide[,1]
chris_diptera_wide_richness$diptera_richness<-specnumber(chris_diptera_wide[,-1])
chris_diptera_wide_richness$diptera_abundance<-rowSums(chris_diptera_wide[,-1],na.rm = TRUE)
head(chris_diptera_wide_richness)

##gastropoda
chris_gastropoda<-read.csv("C:Data by person//Chris.data//gastropoda_ey.csv", header=TRUE, sep=",")
head(chris_gastropoda)

chris_gastropoda$IslandNumber<-sprintf("%02d", chris_gastropoda$IslandNumber)
chris_gastropoda$unq_isl <- paste(chris_gastropoda$Island,chris_gastropoda$IslandNumber)
chris_gastropoda$unq_isl<-gsub(" ", "", chris_gastropoda$unq_isl, fixed = TRUE)

chris_gastropoda_wide <-chris_gastropoda %>% group_by(unq_isl, TrapType, Identification) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_isl, Identification) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Identification, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_gastropoda_wide)
chris_gastropoda_wide<-chris_gastropoda_wide[,-c(2)]


chris_gastropoda_wide_richness<-chris_gastropoda_wide[,1]
chris_gastropoda_wide_richness$gastropoda_richness<-specnumber(chris_gastropoda_wide[,-1])
chris_gastropoda_wide_richness$gastropoda_abundance<-rowSums(chris_gastropoda_wide[,-1],na.rm = TRUE)
head(chris_gastropoda_wide_richness)

##spiders
chris_spiders<-read.csv("C:Data by person//Chris.data//spiders_ey.csv", header=TRUE, sep=",")
head(chris_spiders)

chris_spiders$Island.Number<-sprintf("%02d", chris_spiders$Island.Number)
chris_spiders$unq_isl <- paste(chris_spiders$Island,chris_spiders$Island.Number)
chris_spiders$unq_isl<-gsub(" ", "", chris_spiders$unq_isl, fixed = TRUE)

chris_spiders_wide <-chris_spiders %>% group_by(unq_isl, Trap.Type, Identification) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_isl, Identification) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Identification, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_spiders_wide)
chris_spiders_wide<-chris_spiders_wide[,-c(2)]


chris_spiders_wide_richness<-chris_spiders_wide[,1]
chris_spiders_wide_richness$spiders_richness<-specnumber(chris_spiders_wide[,-1])
chris_spiders_wide_richness$spiders_abundance<-rowSums(chris_spiders_wide[,-1],na.rm = TRUE)
head(chris_spiders_wide_richness)

#myriapoda

chris_myriapod<-read.csv("C:Data by person//Chris.data//myriapoda_ey.csv", header=TRUE, sep=",")
head(chris_myriapod)
chris_myriapod$Island.Number<-sprintf("%02d", chris_myriapod$Island.Number)
chris_myriapod$unq_isl <- paste(chris_myriapod$Island,chris_myriapod$Island.Number)
chris_myriapod$unq_isl<-gsub(" ", "", chris_myriapod$unq_isl, fixed = TRUE)
head(chris_myriapod)

chris_crustacea <- chris_myriapod %>%  filter(Subphylum=="Crustacea")
head(chris_crustacea)
chris_myriapoda <- chris_myriapod %>%  filter(Subphylum=="Myriapoda")

#
chris_crustacea_wide <-chris_crustacea %>% group_by(unq_isl, Trap.Type, Genus.Species) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_isl,Genus.Species) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Genus.Species, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_crustacea_wide)
chris_crustacea_wide<-chris_crustacea_wide[,-c(2)]

chris_crustacea_wide_richness<-chris_crustacea_wide[,1]
chris_crustacea_wide_richness$crustacea_richness<-specnumber(chris_crustacea_wide[,-1])
chris_crustacea_wide_richness$crustacea_abundance<-rowSums(chris_crustacea_wide[,-1],na.rm = TRUE)
head(chris_crustacea_wide_richness)


#myriapoda
chris_myriapod_wide <-chris_myriapoda %>% group_by(unq_isl, Trap.Type, Genus.Species) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_isl,Genus.Species) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Genus.Species, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_myriapod_wide)
chris_myriapod_wide<-chris_myriapod_wide[,-c(2)]

chris_myriapod_wide_richness<-chris_myriapod_wide[,1]
chris_myriapod_wide_richness$myriapod_richness<-specnumber(chris_myriapod_wide[,-1])
chris_myriapod_wide_richness$myriapod_abundance<-rowSums(chris_myriapod_wide[,-1],na.rm = TRUE)
head(chris_myriapod_wide_richness)


#Otherinsects
chris_miscinsects<-read.csv("C:Data by person//Chris.data//misc_insects_ey.csv", header=TRUE, sep=",")
head(chris_miscinsects)
chris_miscinsects$Island.Number<-sprintf("%02d", chris_miscinsects$Island.Number)
chris_miscinsects$unq_isl <- paste(chris_miscinsects$Island,chris_miscinsects$Island.Number)
chris_miscinsects$unq_isl<-gsub(" ", "", chris_miscinsects$unq_isl, fixed = TRUE)

#incorporate Direction if need to have unq_tran... but if want to just get per island average across 5 points
#also trap type .. maybe just combine beat and pitfall = SUM
#okay here we collapsed transects (did mean) and then summed across bet and pitfall ... not sure which is the bvest way to do it
#It's fine for richness but might need to fine tune for abudnance

chris_miscinsects_wide <-chris_miscinsects %>% group_by(unq_isl, Trap.Type, Identification) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm=TRUE)) %>% 
  group_by(unq_isl, Identification) %>% 
  summarise(sum_abundance = sum(mean_abundance, na.rm=TRUE)) %>% 
  spread( Identification, sum_abundance) %>% 
  replace(is.na(.), 0) 
head(chris_miscinsects_wide)
chris_miscinsects_wide<-chris_miscinsects_wide[,-2]

chris_miscinsects_wide_richness<-chris_miscinsects_wide[,1]
chris_miscinsects_wide_richness$miscinsects_richness<-specnumber(chris_miscinsects_wide[,-1])
chris_miscinsects_wide_richness$miscinsects_abundance<-rowSums(chris_miscinsects_wide[,-1],na.rm = TRUE)
head(chris_miscinsects_wide_richness)

#merge insects

chris_insects<-merge(chris_miscinsects_wide_richness, chris_collembola_wide_richness, by.y ="unq_isl", all=TRUE)
chris_insects<-merge(chris_insects, chris_diptera_wide_richness, by.x="unq_isl", all=TRUE)
chris_insects<-merge(chris_insects, chris_spiders_wide_richness, by.x="unq_isl", all=TRUE)
chris_insects<-merge(chris_insects, chris_gastropoda_wide_richness, by.x="unq_isl", all=TRUE)
chris_insects<-merge(chris_insects, chris_hymenoptera_wide_richness, by.x="unq_isl", all=TRUE)
chris_insects<-merge(chris_insects, chris_myriapod_wide_richness, by.x="unq_isl", all=TRUE)
chris_insects<-merge(chris_insects, chris_crustacea_wide_richness, by.x="unq_isl", all=TRUE)
#chris_insects<-merge(chris_insects, chris_beetles_wide_richness, by.x="unq_isl", all=TRUE)


##getting evenness
chris_insects_wide<-merge(chris_miscinsects_wide, chris_collembola_wide, by.y ="unq_isl", all=TRUE)
chris_insects_wide<-merge(chris_insects_wide, chris_hymenoptera_wide, by.x="unq_isl", all=TRUE)
chris_insects_wide<-merge(chris_insects_wide, chris_diptera_wide, by.x="unq_isl", all=TRUE)
chris_insects_wide<-merge(chris_insects_wide, chris_spiders_wide, by.x="unq_isl", all=TRUE)
chris_insects_wide<-merge(chris_insects_wide, chris_gastropoda_wide, by.x="unq_isl", all=TRUE)
chris_insects_wide<-merge(chris_insects_wide, chris_myriapod_wide, by.x="unq_isl", all=TRUE)
chris_insects_wide<-merge(chris_insects_wide, chris_crustacea_wide, by.x="unq_isl", all=TRUE)
#chris_insects_wide<-merge(chris_insects_wide, chris_beetles_wide, by.x="unq_isl", all=TRUE)
head(chris_insects_wide)
chris_insects_wide[is.na(chris_insects_wide)] <- 0

chris_insects$insect_richness<-specnumber(chris_insects_wide[,-1])
chris_insects$insect_diversity<-diversity(chris_insects_wide[,-1],index="shannon")
chris_insects$insect_evenness<-chris_insects$insect_diversity/(log(chris_insects$insect_richness))
chris_insects$insect_abundance<-rowSums(chris_insects_wide[,-1],na.rm = TRUE)

head(chris_insects)
chris_insects[is.na(chris_insects)] <- 0
length(chris_insects$unq_isl)

head(by_isl_master)
by_isl_master<-merge(by_isl_master, chris_insects, by="unq_isl", all=TRUE)


# Mammal richness ---------------------------------------------------------

katie_mammals<-read.csv("C:Data by person//Katie.data//100_Islands_mammal_team_diversity_data_2018_03_17.csv", header=TRUE, sep=",")
head(katie_mammals)
names(katie_mammals)[2]<-"unq_isl"
katie_mammals_simple<-katie_mammals[,-c(1,3, 13)]
head(katie_mammals_simple)
names(katie_mammals_simple)[10]<-"mammal_richness"
katie_mammals_simple<-katie_mammals[,-c(1,3, 13)]
katie_mammals_wide<-as_data_frame(katie_mammals[,-c(1,3, 12, 13)])


katie_richness<-katie_mammals_wide[,1]
katie_richness$mammal_richness<-specnumber(katie_mammals_wide[,-1])
#katie_richness$mammal_abundance<-rowSums(katie_mammals_wide[,-1],na.rm = TRUE)
#katie_richness$mammal_diversity<-diversity(katie_mammals_wide[,-1],index="shannon")
#katie_richness$mammal_evenness<-katie_richness$mammal_diversity/(log(katie_richness$mammal_richness))

head(katie_richness)

by_isl_master<-merge(by_isl_master, katie_richness, by="unq_isl", all=TRUE)


# Tiidying up -------------------------------------------------------------

head(by_isl_master)

by_isl_master$log_Area<-log(by_isl_master$Area)
by_isl_master$log_SITE_SUM<-log(by_isl_master$SITE_SUM+1)
by_isl_master$log_site_sum_by_isl<-log(by_isl_master$site_sum_by_isl+1)
by_isl_master$log_HAB2000<-log(by_isl_master$HAB2000+1)

by_isl_master_habitat<-by_isl_master[,24:28]
by_isl_master$habitat_het<-diversity(by_isl_master_habitat)
head(by_isl_master)
by_isl_master$total_richness<-by_isl_master$plant_richness+by_isl_master$tree_richness+by_isl_master$insect_richness+by_isl_master$bird.richness+by_isl_master$mammal_richness
head(by_isl_master)


#Label small islands as those with Area less than 6000m2
levels <- c(-Inf, 6000, Inf)
xs2=quantile(na.omit(by_isl_master$Area),c(0,1/2,1))
xs2
xs=quantile(na.omit(by_isl_master$Area),c(0,1/3,2/3,1))
xs
xs3=quantile(na.omit(by_isl_master$d15n),c(0,1/3, 2/3,1))

labels <- c("small", "medium", "large")
labels2 <- c("small", "large")
labels3 <- c("low 15N", "med 15N", "high 15N")
by_isl_master<- by_isl_master %>% mutate(size.cat = cut(Area, xs, labels = labels))
by_isl_master<- by_isl_master %>% mutate(size.cat2 = cut(Area, xs2, labels = labels2))
by_isl_master<- by_isl_master %>% mutate(d15n.cat = cut(d15n, xs3, labels = labels3))

head(soil_owen_deb)
node.adding<-soil_owen_deb[,c(2,10)]
head(node.adding)
node.adding<-unique(node.adding)
by_isl_master<-merge(by_isl_master, node.adding, by="unq_isl", all=TRUE)


write.csv(by_isl_master, "C:Data by person//Owen's data/by_isl_master.csv")


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
head(by_isl_master)

corr_by_isl_selected<-by_isl_master[,c(34,33,58,59,2,3,35,36,39,37,5,63,10,18,22,8,60,47,62,64)]
str(corr_by_isl_selected)
#can extract only numeric with select(if.numeric) 

corr_by_isl_selected_2 <- round(cor(corr_by_isl_selected, use="pairwise.complete.obs"), 1)
head(corr_by_isl_selected_2[, 1:6])
p.mat_by_isl_selected_2 <- cor_pmat(corr_by_isl_selected)
head(p.mat_by_isl_selected_2[, 1:4])

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
ggsave("C:Data by person//Plots//Richness_nut//Richness_d15n_area_category.png", width=30, height=20, unit="cm")

nam0<-ggplot(by_isl_master, aes(y=total_richness, x=d15n, colour=size.cat, fill=size.cat))+geom_point()+geom_smooth(aes(fill=size.cat),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.75))
nam1<-ggplot(by_isl_master, aes(y=insect_richness, x=d15n, colour=size.cat, fill=size.cat))+geom_point()+geom_smooth(aes(fill=size.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,350)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+theme(legend.position="none")
nam2<-ggplot(by_isl_master, aes(y=tree_richness, x=d15n, colour=size.cat, fill=size.cat))+geom_point()+geom_smooth(aes(fill=size.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,10)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
nam3<-ggplot(na.omit(by_isl_master), aes(y=plant_richness, x=d15n, colour=size.cat, fill=size.cat))+geom_point()+geom_smooth(aes(fill=size.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,60)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
nam4<-ggplot(by_isl_master, aes(y=bird.richness, x=d15n, colour=size.cat, fill=size.cat))+geom_point()+geom_smooth(aes(fill=size.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,25)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
nam5<-ggplot(by_isl_master, aes(y=mammal_richness, x=d15n, colour=size.cat, fill=size.cat))+geom_point()+geom_smooth(aes(fill=size.cat),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
plot_grid(nam0,nam3,nam2,nam1,nam4,nam5, ncol=3)
ggsave("C:Data by person//Plots//Richness_nut//Richness_d15n_area_category_3.png", width=30, height=20, unit="cm")


nan0<-ggplot(by_isl_master, aes(y=total_richness, x=log_Area, colour=d15n.cat, fill=d15n.cat))+geom_point()+geom_smooth(aes(fill=d15n.cat),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75,0.15))
nan1<-ggplot(by_isl_master, aes(y=insect_richness, x=log_Area, colour=d15n.cat, fill=d15n.cat))+geom_point()+geom_smooth(aes(fill=d15n.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,350)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+theme(legend.position="none")
nan2<-ggplot(by_isl_master, aes(y=tree_richness, x=log_Area, colour=d15n.cat, fill=d15n.cat))+geom_point()+geom_smooth(aes(fill=d15n.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,10)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
nan3<-ggplot(by_isl_master, aes(y=plant_richness, x=log_Area, colour=d15n.cat, fill=d15n.cat))+geom_point()+geom_smooth(aes(fill=d15n.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,60)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
nan4<-ggplot(by_isl_master, aes(y=bird.richness, x=log_Area, colour=d15n.cat, fill=d15n.cat))+geom_point()+geom_smooth(aes(fill=d15n.cat),method="glm", method.args = list(family = "poisson"))+ylim(0,25)+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
nan5<-ggplot(by_isl_master, aes(y=mammal_richness, x=log_Area, colour=d15n.cat, fill=d15n.cat))+geom_point()+geom_smooth(aes(fill=d15n.cat),method="glm", method.args = list(family = "poisson"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
plot_grid(nan0,nan3,nan2,nan1,nan4,nan5, ncol=3)
ggsave("C:Data by person//Plots//Richness_nut//Richness_area_d15n_category.png", width=30, height=20, unit="cm")



# Plotting insects by species ---------------------------------------------



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
ggsave("C:Data by person//Plots//Species_nut//Insects_d15n.png")



abund.insects0<-ggplot(by_isl_master, aes(y=log(collembola_abundance), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position=c(0.75, 0.75))
abund.insects1<-ggplot(by_isl_master, aes(y=diptera_abundance, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+theme(legend.position="none")
abund.insects2<-ggplot(by_isl_master, aes(y=log(miscinsects_abundance), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
abund.insects3<-ggplot(by_isl_master, aes(y=log(hymenoptera_abundance), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
abund.insects4<-ggplot(by_isl_master, aes(y=spiders_abundance, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
abund.insects5<-ggplot(by_isl_master, aes(y=log(myriapod_abundance), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
abund.insects6<-ggplot(by_isl_master, aes(y=gastropoda_abundance, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")
#abund.insects7<-ggplot(by_isl_master, aes(y=log(beetles_abundance), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "gaussian"))+  scale_fill_viridis(discrete=TRUE)+  scale_colour_viridis(discrete=TRUE)+ theme(legend.position="none")

plot_grid(abund.insects0,abund.insects1,abund.insects6,abund.insects3,abund.insects4,abund.insects5,abund.insects2, ncol=4)
ggsave("C:Data by person//Plots//Species_nut//Insects_Abundance_d15n.png")


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
ggsave("C:Data by person//Plots//Species_nut//Insects_Area.png")



# Multivariate ordination for marine evidence -----------------------------


head(by_isl_master)

write.csv(by_isl_master, "C:Data by person//Owen's data/by_isl_master.csv")


feathers.key<-read.csv("C:Data by person//Deb.data//banding-all.csv", header=TRUE, sep=",")
head(feathers.key)

i.feathers.all<-read.csv("C:Data by person//Deb.data//i-feathers-all (1).csv")
head(i.feathers.all)
length(unique(i.feathers.all$unq_isl))

feathers.merge<-merge(feathers.key, i.feathers.all, by="band")
head(feathers.merge)


which( colnames(feathers.merge)=="island" )
which( colnames(feathers.merge)=="d13c" )
names(feather.)
feather.simple<-feathers.merge[,c(37,43)]
head(feather.simple)

feather.simple.means<- feather.simple %>% group_by(island)%>% summarise_all(funs(mean))
length(unique(feather.simple.means$island))
names(feather.simple.means)[1]<-"unq_isl"

by_isl_multivariate<-merge(feather.simple.means, by_isl_master[, c(1, 2,3,48,49,10)])
head(by_isl_multivariate)
by_isl_multivariate<-na.omit(by_isl_multivariate)
length(by_isl_multivariate$unq_isl)
#56 islands

head(owen_key)
by_isl_multivariate_env<-merge(unique(owen_key[,c(2,3)]), by_isl_multivariate)
head(by_isl_multivariate_env)


head(by_isl_master)
which( colnames(by_isl_master)=="log_Area" )
by_isl_feathers<-merge(feather.simple.means, by_isl_master[, c(1, 2,3,46,48,49,10)])


ggplot(by_isl_feathers, aes(x=log_Area, y=d13c))+geom_point()+geom_smooth(method="lm")





size.cat.frame<-merge(Veg_means_by_island, by_isl_multivariate_env)
head(size.cat.frame)
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
wrack.sara<-read.csv("C:Data by person//Sara's data//wrack.isotopes.cn.csv")
wrack.key<-read.csv("C:Data by person//Sara's data//wrack.key.csv")
ang.seaweed<-read.csv("C:Data by person//Ang's data//chokedpass_macrophytes_AMO2015.csv", header=TRUE, sep=",")
becky.eagles<-read.csv("C:Data by person//Becky.data//becky.isotopes.csv", header=TRUE, sep=",")
otter_chris<-read.csv("C:Data by person//Chris.data//otter_sia.csv")


#otter isotopes
head(otter_chris)
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
wrackfree<-read.csv("C:Data by person//Pat.data//wrackfree.csv", header=TRUE, sep=",")
head(wrackfree)



ggplot(wrackfree, aes(x=n15, y=litter))+geom_point()+geom_smooth( method="glm", method.args = list(family = "binomial"))
ggplot(wrackfree, aes(x=n15, y=moss))+geom_point()+geom_smooth( method="glm", method.args = list(family = "binomial"))
ggplot(wrackfree, aes(x=n15, y=rock_soil_etc))+geom_point()+geom_smooth( method="glm", method.args = list(family = "binomial"))
ggplot(wrackfree, aes(x=n15, y=midi))+geom_point()+geom_smooth( method="glm", method.args = list(family = "binomial"))
ggplot(wrackfree, aes(x=n15, y=gash))+geom_point()+geom_smooth( method="glm", method.args = list(family = "binomial"))





