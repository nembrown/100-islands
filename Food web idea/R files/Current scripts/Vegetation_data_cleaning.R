#Cleaned up this file February 2020 - take out references to large and small - that can be computed later... 
# Just combine Deb and Owen's plant information and extract out any marine information
#take out bare ground as species

library(tidyr)
library(plyr)
library(dplyr)
library(doBy)
library(ggplot2)


# Owen's data read and tidy -----------------------------------------------

# Read in Owen's data
islands_plant<-read.csv("Food web idea//Data by person//Owen's data//Complete_long_percentcover_mod.csv", header=TRUE, sep=",")
head(islands_plant)

#summing across layers Owen but no T, because T is summing across layers, so need to exclude
islands_plant_noT <- islands_plant %>%  filter(layer!= "T")
islands_plant_sum<- islands_plant_noT %>%  group_by(unq_plot, species)%>% summarise(cover =sum(cover))
#This is summed per plot across all layers per species
head(islands_plant_sum)
islands_plant_sum[duplicated(islands_plant_sum),]

#need to read in owen's key b/c otherwise the shore_dist is wrong
owen_key<-read.csv("C:Food web idea//Data by person//Owen's data//key_mod_2019.csv", header=TRUE, sep=",")
owen_key<-owen_key %>% dplyr::distinct()
owen_key_slim<-owen_key %>%  dplyr::select(unq_plot, shore_dist)
owen_key_slim<-owen_key_slim %>% dplyr::distinct()
owen_key_slim[duplicated(owen_key_slim),]


islands_plant_filtered<-merge(islands_plant_sum, owen_key_slim, by="unq_plot", all=TRUE)
head(islands_plant_filtered)
islands_plant_filtered[duplicated(islands_plant_filtered),]

islands_plant_filtered$unq_tran<-str_sub(islands_plant_filtered$unq_plot, end=-2)
islands_plant_filtered$plot<-str_sub(islands_plant_filtered$unq_plot, -1, -1)

islands_plant_filtered<- islands_plant_filtered %>% mutate(unq_tran= if_else(plot<4, gsub("SN", "S", unq_tran, fixed = TRUE), gsub("SN", "N", unq_tran, fixed = TRUE))) %>% 
  mutate(unq_tran= if_else(plot<4, gsub("NS", "N", unq_tran, fixed = TRUE), gsub("NS", "S", unq_tran, fixed = TRUE))) %>% 
  mutate(unq_tran= if_else(plot<4, gsub("EW", "E", unq_tran, fixed = TRUE), gsub("EW", "W", unq_tran, fixed = TRUE))) %>% 
  mutate(unq_tran= if_else(plot<4, gsub("WE", "W", unq_tran, fixed = TRUE), gsub("WE", "E", unq_tran, fixed = TRUE))) 


#these ones are double digits - also they are ones are that 25m and 15m (so would actually be less than plot 3)
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="CV04SN25"]<-"CV04S"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="MM04WE25"]<-"MM04W"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="MM08NS25"]<-"MM08N"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="PR05EW25"]<-"PR05E"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="PR06EW25"]<-"PR06E"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="TQ02NS25"]<-"TQ02N"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="TQ05EW25"]<-"TQ05E"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="MM01WE15"]<-"MM01W"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="MM03WE15"]<-"MM03W"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="MM08EW15"]<-"MM08E"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="TQ06EW15"]<-"TQ06E"

islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="AD03WE3"]<-"AD03E"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="CV14SN3"]<-"CV14N"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="CV14EW3"]<-"CV14W"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="MM07NS3"]<-"MM07S"
islands_plant_filtered$unq_tran[islands_plant_filtered$unq_plot=="ST09WE3"]<-"ST09E"

islands_plant_filtered[duplicated(islands_plant_filtered),]
islands_plant_filtered$unq_isl<-str_sub(islands_plant_filtered$unq_tran, 1,4)


head(islands_plant_filtered)

# Deb's data read and tidy ------------------------------------------------

#read in Deb's data
veg1x1_Deb_mod<-read.csv("Food web idea//Data by person//Owen's data//bird_long_percentcover_mod.csv", header=TRUE, sep=",")
head(veg1x1_Deb_mod)
length(unique(veg1x1_Deb_mod$unq_isl))
#99 islands

#adding shore dist to deb's veg
Deb_interior<-read.csv("Food web idea//Data by person//Deb.data//shoredist.csv", header=TRUE, sep=",")
veg1x1_Deb_mod_interior<-merge(veg1x1_Deb_mod, Deb_interior, by="pcid")
head(veg1x1_Deb_mod_interior)
names(veg1x1_Deb_mod_interior)[11]<-"shore_dist"
length(unique(veg1x1_Deb_mod_interior$unq_isl))
#99 islands 

veg1x1_Deb_mod_interior_shore<-veg1x1_Deb_mod_interior %>% dplyr::select(unq_isl, unq_tran, shore_dist)
veg1x1_Deb_mod_interior_shore<-veg1x1_Deb_mod_interior_shore[!duplicated(veg1x1_Deb_mod_interior_shore),]
veg1x1_Deb_mod_interior_size<-veg1x1_Deb_mod_interior


#summing across layers (e.g. canopy, herbs) Deb
veg1x1_Deb_mod_interior_size_noT<-veg1x1_Deb_mod_interior_size %>% filter(layer!="T")
veg1x1_Deb_mod_interior_size_sum<- veg1x1_Deb_mod_interior_size_noT %>%  group_by(unq_tran, species)%>% summarise(cover =sum(cover))
head(veg1x1_Deb_mod_interior_size_sum)

#add this back to the main dataframe without the "species, layer, cover, notes" categories
#make a transect that is the same as plot for Deb's (since each plot is it's own transect) 
veg1x1_Deb_mod_interior_size_sum$unq_plot<-substr(veg1x1_Deb_mod_interior_size_sum$unq_tran, 5, 11)

veg1x1_Deb_mod_interior_size_filtered<-merge(veg1x1_Deb_mod_interior_size_sum, veg1x1_Deb_mod_interior_shore)
head(veg1x1_Deb_mod_interior_size_filtered)


# Combining Owen and Deb --------------------------------------------------


#Adding a column for person
islands_plant_filtered$person<-"Owen"
veg1x1_Deb_mod_interior_size_filtered$person<-"Deb"
islands_plant_filtered<- islands_plant_filtered[c("unq_tran", "species", "cover", "unq_plot", "unq_isl","shore_dist", "person")]
head(islands_plant_filtered)
head(veg1x1_Deb_mod_interior_size_filtered)

#combining deb and owen's data 
Deb_Owen_veg_combined<-rbind(islands_plant_filtered, veg1x1_Deb_mod_interior_size_filtered)



head(Deb_Owen_veg_combined)
length(unique(Deb_Owen_veg_combined$unq_isl))
#99

#Fixing some species

Deb_Owen_veg_combined_wide <-Deb_Owen_veg_combined %>%  group_by(unq_plot) %>% spread(species, cover)

head(Deb_Owen_veg_combined_wide)
longform_plant_percentcover2<-Deb_Owen_veg_combined_wide
longform_plant_percentcover2[is.na(longform_plant_percentcover2)]<-0 

#combining some species
longform_plant_percentcover2$unknown_lily<-longform_plant_percentcover2$'unk lily'+ longform_plant_percentcover2$'unk lily sp'+longform_plant_percentcover2$'lily seedling'
longform_plant_percentcover2$marine<-longform_plant_percentcover2$scat+ longform_plant_percentcover2$`marine remains`+longform_plant_percentcover2$`abalone shell`+longform_plant_percentcover2$shell
longform_plant_percentcover2$free_space<-longform_plant_percentcover2$bare+longform_plant_percentcover2$`bare ground`+longform_plant_percentcover2$`sandy soil`+longform_plant_percentcover2$`o soil`+longform_plant_percentcover2$gravel+longform_plant_percentcover2$rock
longform_plant_percentcover2$grass<-longform_plant_percentcover2$`grass 1`+longform_plant_percentcover2$`grass sp`
longform_plant_percentcover2$sedge_final<-longform_plant_percentcover2$`sedge 1`+longform_plant_percentcover2$`sedge sp`+longform_plant_percentcover2$sedge+longform_plant_percentcover2$sedge1
longform_plant_percentcover2$unknown_forb<-longform_plant_percentcover2$'unk forb'+ longform_plant_percentcover2$'unidentified forb'
longform_plant_percentcover2$unknown_monocot<-longform_plant_percentcover2$'unk mono'+ longform_plant_percentcover2$'unk monocot'
 

#take out the marine indicators, things that are not plants, and the duplicates for the composite variables above.... just for species richness calculations
 not_species_names<-c("marine" ,
                      "free_space" ,
                      "bare" ,
                      "bare ground" ,
                      "woody debris" ,
                      "wood" ,
                      "sandy soil" ,
                      "o soil" ,
                      "shell" ,
                      "gravel" ,
                      "rock" ,
                      "marine remains" ,
                      "abalone shell" ,
                      "driftwood" ,
                      "feather" ,
                      "grass 1" ,
                      "grass sp" ,
                      "scat",
                      "sedge" ,
                      "stump" ,
                      "sedge1" ,
                      "sedge 1" ,
                      "sedge sp" ,
                      "unk forb" ,
                      "unidentified forb",
                      "unk lily" ,
                      "lily seedling" ,
                      "unk lily sp" ,
                      "unk mono" ,
                      "unk monocot")
 
longform_plant_percentcover_species<-longform_plant_percentcover2[, ! colnames(longform_plant_percentcover2) %in% not_species_names]
head(longform_plant_percentcover_species)

longform_plant_percentcover_species_long<-longform_plant_percentcover_species %>% group_by(unq_plot) %>% gather(species, cover, 6:153)
head(longform_plant_percentcover_species_long)
#lots of Nas but we will deal with thme next

Deb_Owen_veg_combined_complete_filled<-longform_plant_percentcover_species_long

Deb_Owen_veg_combined_complete_filled[is.na(Deb_Owen_veg_combined_complete_filled)] <- 0


length(unique(Deb_Owen_veg_combined_complete_filled$unq_isl))
#99

##adding in info about shurb herb or tree
plant_category<-read.csv("Food web idea//Data by person//Owen's data//100Islands_Fitzpatrick_species.csv", header=TRUE, sep=",")
head(plant_category)



Deb_Owen_veg_combined_complete_filled<-merge(Deb_Owen_veg_combined_complete_filled, plant_category[,c(1,4,5)], by="species")


head(Deb_Owen_veg_combined_complete_filled)
Deb_Owen_veg_combined_complete_filled[duplicated(Deb_Owen_veg_combined_complete_filled),]

#add in Deb coords
pointcount.gps<-read.csv("C:Food web idea//Data by person//Deb.data//pointcounts.csv", header=TRUE, sep=",")
pointcount.gps$pcid<-gsub(" ", "", pointcount.gps$pcid, fixed = TRUE)
pointcount.gps<-pointcount.gps[,c(3,16,17)]
pointcount.gps<-pointcount.gps[!duplicated(pointcount.gps$pcid),]
#sometimes taken twice...  
pointcount.gps$unq_tran<- paste("Deb",pointcount.gps$pcid, sep="_")
deb_coords<-pointcount.gps[,-1]
head(deb_coords)

Deb_Owen_veg_combined_complete_filled<-merge(Deb_Owen_veg_combined_complete_filled,deb_coords,by="unq_tran", all.x = TRUE)
head(Deb_Owen_veg_combined_complete_filled)

write.csv(Deb_Owen_veg_combined_complete_filled, "Food web idea//Data by person//Kalina.data/Deb_Owen_veg_combined_complete_filled.csv", row.names=FALSE)
#### This is complete with no "bad" species- all are plnats, no bare ground etc.... 



###Getting a vector of just marine stuff

head(longform_plant_percentcover2)

marine_by_plot_from_plants<-longform_plant_percentcover2 %>% dplyr::select(unq_plot, unq_isl, unq_tran, shore_dist, person, marine, 'marine remains', 'abalone shell', shell, scat, bare, 'o soil',driftwood )
head(marine_by_plot_from_plants)


write.csv(marine_by_plot_from_plants, "Food web idea//Data by person//Norah.data/marine_by_plot_from_plants.csv", row.names=FALSE)

head(marine_by_plot_from_plants)
