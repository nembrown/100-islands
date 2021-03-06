
#This script plots the locations of collected samples from three separate datasets
#Transects from 100 islands project, Beachsines from Hakai Nearshore, and Otter scats from Andrew Sheriff

##Loading necessary libraries
#library(devtools)
#install_github("r-spatial/sf")

library(here)
library(sf)
library(raster)
library(spData)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse vis package
library(shiny)
library(rgdal) # spatial/shp reading
library(viridis) # nice color palette
library(ggmap) # ggplot functionality for maps ---> dplyr, purr is dependency
library(ggsn) # for scale bars/north arrows in ggplots
library(maps)
library(mapdata)


# Loading the data ------------------------------------------------------------


#loading in information on beachseine location
ben_habitat_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_habitat_20142018.csv")
head(ben_habitat_data)

#loading in beachseine data that's combined with transect information 
fish_richness_merged_tran_isl<-read.csv("C:Biodiversity idea//Output files//fish_richness_merged_tran_isl.csv")
fish_richness_merged_tran_isl<-fish_richness_merged_tran_isl[,-1]


#loading information from 100 islands project

#Island level location information
by_isl_master<-read.csv("C:Food web idea//Data by person//Owen's data//by_isl_master.csv")
head(by_isl_master)

#Transect-level information
by_tran_master<-read.csv("C:Food web idea//Data by person//Norah.data//by_tran_master.csv")


#isotopes on the island-level
isotope_by_isl_gathered4<- read.csv("C:Food web idea//Data by person//Norah.data/isotope_by_isl_gathered4.csv")
isotope_by_isl_gathered4<-isotope_by_isl_gathered4[,-1]


# Mapping -----------------------------------------------------------------
#using simple features 

#Island-level combining data from isotopes of all species, and island-level ecological characteristics, lots of NAs but that's okay
head(by_isl_master)
head(isotope_by_isl_gathered4)
which( colnames(by_isl_master)=="node" )
#take out s and cn becasue lot's of NAs
isotope_master_sf<-merge(isotope_by_isl_gathered4[,-c(6,7)], by_isl_master[,-c(2:10)], by="unq_isl", all=TRUE)
head(isotope_master_sf)

#need no NAs
data_subset <- isotope_master_sf[ , c("C_Easting", "C_Northing")]
isotope_master_sf_no_na<- isotope_master_sf[complete.cases(data_subset), ] 

#make an sf object
df.SF <- st_as_sf(isotope_master_sf_no_na, coords = c("C_Easting", "C_Northing"), crs = 26909)
df.SF<-st_transform(x = df.SF, crs = 4326)
df.SF$long<-st_coordinates(df.SF)[,1] # get coordinates
df.SF$lat<-st_coordinates(df.SF)[,2] # get coordinates
head(df.SF)
#this is island-level data frame as an sf object


#Now beachseine
head(ben_habitat_data)
ben_habitat_data_simple<-ben_habitat_data[,c(1:3)]
head(ben_habitat_data_simple)
ben_habitat_data_simple$long<-(ben_habitat_data_simple$long)
ben_habitat_data_simple.SP <- st_as_sf(ben_habitat_data_simple, coords = c("long", "lat"), crs = 4326)
head(ben_habitat_data_simple.SP)
ben_habitat_data_simple.SP$site_type<-"beachseine"

#  ben_habitat_data_simple.SP$long<-st_coordinates(ben_habitat_data_simple.SP)[,1] # get coordinates
#  ben_habitat_data_simple.SP$lat<-st_coordinates(ben_habitat_data_simple.SP)[,2]
#  ben_habitat_data_simple.SP$site<-as.factor(ben_habitat_data_simple.SP$site)
# 
# ggmap(map_marine) + geom_point(data=ben_habitat_data_simple.SP, aes(x = long, y = lat, col=site_type, label=site))+ 
#   scale_colour_viridis_d()+geom_text(data=ben_habitat_data_simple.SP, aes(x = long, y = lat, col=site_type,label=site))
#   



### Islands level data just site-information
head(df.SF)
df.SF_simple<-df.SF[,1]
head(df.SF_simple)
names(df.SF_simple)[1]<-"site"
df.SF_simple$site_type<-"100_islands"

#combining 100 islands and beachseine data 
df.SF_marine<-rbind(df.SF_simple, ben_habitat_data_simple.SP)
head(df.SF_marine)
df.SF_marine$long<-st_coordinates(df.SF_marine)[,1] # get coordinates
df.SF_marine$lat<-st_coordinates(df.SF_marine)[,2]

######
bbox_marine <- make_bbox(df.SF_marine$long, df.SF_marine$lat, f = 0.01)
bbox_marine_big <- make_bbox(df.SF_marine$long, df.SF_marine$lat, f = 5)
map_toner_lite_marine <- get_stamenmap(bbox_marine, source="stamen", maptype= "toner-lite", crop=FALSE)
map_marine <- get_stamenmap(bbox_marine, source="stamen", maptype= "terrain", crop=FALSE)
map_marine_big <- get_stamenmap(bbox_marine_big, source="stamen", maptype= "terrain", crop=FALSE, zoom=8)


ggmap(map_marine) + geom_point(data=df.SF_marine, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()


ggmap(map_marine_big) + stat_density2d(data=df.SF_marine, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()
ggmap(map_marine_big) + geom_blank(data=df.SF_marine, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()
ggsave("C:Biodiversity idea//Plots//Maps//map_big.png", width=40, height=20, unit="cm")


# Transects --------------------------------------------------------------
#load transect 
by_tran_master_0m<-read.csv("C:Food web idea//Data by person//Norah.data//by_tran_master_0m.csv")
data_subset2 <- by_tran_master_0m[ , c("easting", "northing")]
by_tran_master_0m_no_na<- by_tran_master_0m[complete.cases(data_subset2), ]
df.SF_transects <- st_as_sf(by_tran_master_0m_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
df.SF_transects$long<-st_coordinates(df.SF_transects)[,1] # get coordinates
df.SF_transects$lat<-st_coordinates(df.SF_transects)[,2] # get coordinates
head(df.SF_transects)

head(ben_habitat_data)
ben_habitat_data_simple<-ben_habitat_data[,c(3:5)]
head(ben_habitat_data_simple)
ben_habitat_data_simple$long<- -(ben_habitat_data_simple$long)
ben_habitat_data_simple.SP <- st_as_sf(ben_habitat_data_simple, coords = c("long", "lat"), crs = 4326)
head(ben_habitat_data_simple.SP)
ben_habitat_data_simple.SP$site_type<-"beachseine"

head(df.SF_transects)
df.SF_transects_simple<-df.SF_transects[,1]
head(df.SF_transects_simple)
names(df.SF_transects_simple)[1]<-"site"
df.SF_transects_simple$site_type<-"100_islands"

# ###different dataframe.... 
# df.SF_transects_simple_ungeo<-df.SF_transects_simple
# df.SF_transects_simple_ungeo$long<-st_coordinates(df.SF_transects_simple_ungeo)[,1] # get coordinates
# df.SF_transects_simple_ungeo$lat<-st_coordinates(df.SF_transects_simple_ungeo)[,2]
# df.SF_transects_simple_ungeo<-df.SF_transects_simple_ungeo %>% st_set_geometry(NULL)
# head(df.SF_transects_simple_ungeo)
# df.SF_transects_simple_ungeo<-df.SF_transects_simple_ungeo[,-2]


df.SF_transects_marine<-rbind(df.SF_transects_simple, ben_habitat_data_simple.SP)
head(df.SF_transects_marine)
 df.SF_transects_marine$long<-st_coordinates(df.SF_transects_marine)[,1] # get coordinates
 df.SF_transects_marine$lat<-st_coordinates(df.SF_transects_marine)[,2]


# bbox_marine_trans <- make_bbox(df.SF_transects_marine$long, df.SF_transects_marine$lat, f = 0.1)
# map_toner_lite_marine_trans <- get_stamenmap(bbox_marine_trans, source="stamen", maptype= "toner-lite", crop=FALSE)
# map_marine_trans <- get_stamenmap(bbox_marine_trans, source="stamen", maptype= "terrain", crop=TRUE)


ggmap(map_marine_trans) + geom_point(data=df.SF_transects_marine, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()

#outdated file path - update to the project directory. 
ggsave("C:Biodiversity idea//Plots//Maps//map_transects_beachseine.png", width=40, height=20, unit="cm")

library(beyonce)

colorset_map = c("100_islands"="#9FEF5C" , "beachseine" ="#6138EA" )

ggmap(map_marine_trans) + geom_point(data=df.SF_transects_marine, aes(x = long, y = lat, col=site_type))+  scale_colour_manual(values=colorset_map)
ggsave("C:Biodiversity idea//Plots//Maps//map_transects_beachseine.png", width=40, height=20, unit="cm")


#### adding in Arc data
arch_data<-read.csv("C:Biodiversity idea//Output files//arch_sites_selected.csv")

arch_data_simple_new<-arch_data[ , c("site_id", "easting", "northing")]
data_arch_subset2_new <- arch_data_simple_new[ , c("easting", "northing")]
arch_data_simple_new<- arch_data_simple_new[complete.cases(data_arch_subset2_new), ] 
arch_data_simple_new$site_id<-as.factor(arch_data_simple_new$site_id)

arch_data_simple_sf_new <- st_as_sf(arch_data_simple_new, coords = c("easting", "northing"), crs = 26909)
arch_data_simple_st<-st_transform(x = arch_data_simple_sf_new, crs = 4326)
arch_data_simple_st$long<-st_coordinates(arch_data_simple_st)[,1] # get coordinates
arch_data_simple_st$lat<-st_coordinates(arch_data_simple_st)[,2] # get coordinates
arch_data_simple_st$site_type<-"arch_site"
head(arch_data_simple_st)

arch_data_simple_SP<-arch_data_simple_st[,c(1,5)]

names(arch_data_simple_SP)[1]<-"site"
arch_data_simple_SP$site<-as.factor(arch_data_simple_SP$site)

df.SF_transects_marine_arch<-rbind(df.SF_transects_marine[,1:2],arch_data_simple_SP)
head(df.SF_transects_marine_arch)
df.SF_transects_marine_arch$long<-st_coordinates(df.SF_transects_marine_arch)[,1] # get coordinates
df.SF_transects_marine_arch$lat<-st_coordinates(df.SF_transects_marine_arch)[,2]


bbox_marine_trans_arch <- make_bbox(df.SF_transects_marine_arch$long, df.SF_transects_marine_arch$lat, f = 0.1)
map_toner_lite_marine_trans_arch <- get_stamenmap(bbox_marine_trans_arch, source="stamen", maptype= "toner-lite", crop=FALSE)
map_marine_trans_arch <- get_stamenmap(bbox_marine_trans_arch, source="stamen", maptype= "terrain", crop=TRUE)

ggmap(map_marine_trans_arch) + geom_point(data=df.SF_transects_marine_arch, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()
ggsave("C:Biodiversity idea//Plots//map_transects_beachseine_arch.png", width=40, height=20, unit="cm")


colorset_map_3 = c("100_islands"="#9FEF5C" , "beachseine" ="#6138EA", "arch_site" = "yellow")

ggmap(map_marine_trans_arch) + geom_point(data=df.SF_transects_marine_arch, aes(x = long, y = lat, col=site_type))+  scale_colour_manual(values=colorset_map_3)
ggsave("C:Biodiversity idea//Plots//Maps//map_transects_beachseine_arch.png", width=40, height=20, unit="cm")





###### Arch sites midden
#### adding in Arc data
arch_data<-read.csv("C:Biodiversity idea//Output files//arch_sites_selected.csv")
head(arch_data)
arch_data_midden<- arch_data %>% filter(midden_feature == "yes")
head(arch_data_midden)
  
arch_data_simple_new_midden<-arch_data_midden[ , c("site_id", "easting", "northing")]
data_arch_subset2_new_midden <- arch_data_simple_new_midden[ , c("easting", "northing")]
arch_data_simple_new_midden<- arch_data_simple_new_midden[complete.cases(data_arch_subset2_new_midden), ] 
arch_data_simple_new_midden$site_id<-as.factor(arch_data_simple_new_midden$site_id)

arch_data_simple_sf_new_midden <- st_as_sf(arch_data_simple_new_midden, coords = c("easting", "northing"), crs = 26909)
arch_data_simple_st_midden<-st_transform(x = arch_data_simple_sf_new_midden, crs = 4326)
arch_data_simple_st_midden$long<-st_coordinates(arch_data_simple_st_midden)[,1] # get coordinates
arch_data_simple_st_midden$lat<-st_coordinates(arch_data_simple_st_midden)[,2] # get coordinates
arch_data_simple_st_midden$site_type<-"midden_site"
head(arch_data_simple_st_midden)

arch_data_simple_SP_midden<-arch_data_simple_st_midden[,c(1,5)]

names(arch_data_simple_SP_midden)[1]<-"site"
arch_data_simple_SP_midden$site<-as.factor(arch_data_simple_SP_midden$site)

df.SF_transects_marine_arch_midden<-rbind(df.SF_transects_marine[,1:2],arch_data_simple_SP_midden)
head(df.SF_transects_marine_arch_midden)
df.SF_transects_marine_arch_midden$long<-st_coordinates(df.SF_transects_marine_arch_midden)[,1] # get coordinates
df.SF_transects_marine_arch_midden$lat<-st_coordinates(df.SF_transects_marine_arch_midden)[,2]


bbox_marine_trans_arch_midden <- make_bbox(df.SF_transects_marine_arch_midden$long, df.SF_transects_marine_arch_midden$lat, f = 0.1)
map_toner_lite_marine_trans_arch_midden <- get_stamenmap(bbox_marine_trans_arch_midden, source="stamen", maptype= "toner-lite", crop=FALSE)
map_marine_trans_arch_midden <- get_stamenmap(bbox_marine_trans_arch_midden, source="stamen", maptype= "terrain", crop=TRUE)

ggmap(map_marine_trans_arch_midden) + geom_point(data=df.SF_transects_marine_arch_midden, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()
ggsave("C:Biodiversity idea//Plots//map_transects_beachseine_arch_midden.png", width=40, height=20, unit="cm")


###### Arch sites fish
#### adding in Arc data
arch_data<-read.csv("C:Biodiversity idea//Output files//arch_sites_selected.csv")
head(arch_data)
arch_data_fish<- arch_data %>% filter(fish_feature == "yes")
head(arch_data_fish)

arch_data_simple_new_fish<-arch_data_fish[ , c("site_id", "easting", "northing")]
data_arch_subset2_new_fish <- arch_data_simple_new_fish[ , c("easting", "northing")]
arch_data_simple_new_fish<- arch_data_simple_new_fish[complete.cases(data_arch_subset2_new_fish), ] 
arch_data_simple_new_fish$site_id<-as.factor(arch_data_simple_new_fish$site_id)

arch_data_simple_sf_new_fish <- st_as_sf(arch_data_simple_new_fish, coords = c("easting", "northing"), crs = 26909)
arch_data_simple_st_fish<-st_transform(x = arch_data_simple_sf_new_fish, crs = 4326)
arch_data_simple_st_fish$long<-st_coordinates(arch_data_simple_st_fish)[,1] # get coordinates
arch_data_simple_st_fish$lat<-st_coordinates(arch_data_simple_st_fish)[,2] # get coordinates
arch_data_simple_st_fish$site_type<-"fish_site"
head(arch_data_simple_st_fish)

arch_data_simple_SP_fish<-arch_data_simple_st_fish[,c(1,5)]

names(arch_data_simple_SP_fish)[1]<-"site"
arch_data_simple_SP_fish$site<-as.factor(arch_data_simple_SP_fish$site)

df.SF_transects_marine_arch_fish<-rbind(df.SF_transects_marine[,1:2],arch_data_simple_SP_fish)
head(df.SF_transects_marine_arch_fish)
df.SF_transects_marine_arch_fish$long<-st_coordinates(df.SF_transects_marine_arch_fish)[,1] # get coordinates
df.SF_transects_marine_arch_fish$lat<-st_coordinates(df.SF_transects_marine_arch_fish)[,2]


bbox_marine_trans_arch_fish <- make_bbox(df.SF_transects_marine_arch_fish$long, df.SF_transects_marine_arch_fish$lat, f = 0.1)
map_toner_lite_marine_trans_arch_fish <- get_stamenmap(bbox_marine_trans_arch_fish, source="stamen", maptype= "toner-lite", crop=FALSE)
map_marine_trans_arch_fish <- get_stamenmap(bbox_marine_trans_arch_fish, source="stamen", maptype= "terrain", crop=TRUE)

ggmap(map_marine_trans_arch_fish) + geom_point(data=df.SF_transects_marine_arch_fish, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()
ggsave("C:Biodiversity idea//Plots//map_transects_beachseine_arch_fish.png", width=40, height=20, unit="cm")


##### ARch data canoe
#### adding in Arc data
arch_data<-read.csv("C:Biodiversity idea//Output files//arch_sites_selected.csv")
head(arch_data)
arch_data_canoe<- arch_data %>% filter(canoe_skid == "yes")
head(arch_data_canoe)

arch_data_simple_new_canoe<-arch_data_canoe[ , c("site_id", "easting", "northing")]
data_arch_subset2_new_canoe <- arch_data_simple_new_canoe[ , c("easting", "northing")]
arch_data_simple_new_canoe<- arch_data_simple_new_canoe[complete.cases(data_arch_subset2_new_canoe), ] 
arch_data_simple_new_canoe$site_id<-as.factor(arch_data_simple_new_canoe$site_id)

arch_data_simple_sf_new_canoe <- st_as_sf(arch_data_simple_new_canoe, coords = c("easting", "northing"), crs = 26909)
arch_data_simple_st_canoe<-st_transform(x = arch_data_simple_sf_new_canoe, crs = 4326)
arch_data_simple_st_canoe$long<-st_coordinates(arch_data_simple_st_canoe)[,1] # get coordinates
arch_data_simple_st_canoe$lat<-st_coordinates(arch_data_simple_st_canoe)[,2] # get coordinates
arch_data_simple_st_canoe$site_type<-"canoe_site"
head(arch_data_simple_st_canoe)

arch_data_simple_SP_canoe<-arch_data_simple_st_canoe[,c(1,5)]

names(arch_data_simple_SP_canoe)[1]<-"site"
arch_data_simple_SP_canoe$site<-as.factor(arch_data_simple_SP_canoe$site)

df.SF_transects_marine_arch_canoe<-rbind(df.SF_transects_marine[,1:2],arch_data_simple_SP_canoe)
head(df.SF_transects_marine_arch_canoe)
df.SF_transects_marine_arch_canoe$long<-st_coordinates(df.SF_transects_marine_arch_canoe)[,1] # get coordinates
df.SF_transects_marine_arch_canoe$lat<-st_coordinates(df.SF_transects_marine_arch_canoe)[,2]


bbox_marine_trans_arch_canoe <- make_bbox(df.SF_transects_marine_arch_canoe$long, df.SF_transects_marine_arch_canoe$lat, f = 0.1)
map_toner_lite_marine_trans_arch_canoe <- get_stamenmap(bbox_marine_trans_arch_canoe, source="stamen", maptype= "toner-lite", crop=FALSE)
map_marine_trans_arch_canoe <- get_stamenmap(bbox_marine_trans_arch_canoe, source="stamen", maptype= "terrain", crop=TRUE)

ggmap(map_marine_trans_arch_canoe) + geom_point(data=df.SF_transects_marine_arch_canoe, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()
ggsave("C:Biodiversity idea//Plots//map_transects_beachseine_arch_canoe.png", width=40, height=20, unit="cm")


# Map at 1km radius only -------------------------------------------------------

hakai_sites_distance_tran<-read.csv("C:Biodiversity idea//Output files//paired_sites_by_radius.csv")
hakai_sites_distance_tran<-hakai_sites_distance_tran[,-1]
head(hakai_sites_distance_tran)

head(df.SF_transects_marine)


df.SF_transects_marine_1k_beachseine<-subset(df.SF_transects_marine, (site %in% hakai_sites_distance_tran$site))
head(df.SF_transects_marine_1k_beachseine)

df.SF_transects_marine_1k_unq_tran<-subset(df.SF_transects_marine, (site %in% hakai_sites_distance_tran$unq_tran))
head(df.SF_transects_marine_1k_unq_tran)

df.SF_transects_marine_1k <-rbind(df.SF_transects_marine_1k_unq_tran, df.SF_transects_marine_1k_beachseine)
head(df.SF_transects_marine_1k)

ggmap(map_marine_trans) + geom_point(data=df.SF_transects_marine_1k, aes(x = long, y = lat, col=site_type))+  scale_colour_manual(values=colorset_map)
ggsave("C:Biodiversity idea//Plots//Maps//map_transects_beachseine_1k.png", width=40, height=20, unit="cm")





# Adding in some data to map just to beachseine dataset ----------------------------------------------
head(ben_beachseine)

fish_bycatch_richness_merged_tran_year<-read.csv("C:Biodiversity idea//Output files//fish_bycatch_richness_merged_tran_year.csv")
fish_bycatch_richness_merged_tran_year<-fish_bycatch_richness_merged_tran_year[,-1]
head(fish_bycatch_richness_merged_tran_year)

head(ben_habitat_data_simple.SP)
ben_beachseine<-merge(ben_habitat_data_simple.SP, fish_bycatch_richness_merged_tran_year, by="site", all=TRUE)
head(ben_beachseine)

ben_beachseine$long<-st_coordinates(ben_beachseine)[,1] # get coordinates
ben_beachseine$lat<-st_coordinates(ben_beachseine)[,2]


bbox_marine_beachseine <- make_bbox(ben_beachseine$long, ben_beachseine$lat, f = 0.01)
map_marine_beachseine <- get_stamenmap(bbox_marine_beachseine, source="stamen", maptype= "terrain", crop=FALSE)

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=fish_richness_corrected))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_fish_richness.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=fish_biomass_bym3_mean))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_fish_biomass.png", width=40, height=20, unit="cm")


ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=fish_biomass_bym3_mean, size=fish_biomass_bym3_sd))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_fish_biomass_sd.png", width=40, height=20, unit="cm")


ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=fish_biomass_bym3_mean, size=fish_length))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_fish_biomass_v_length.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=fish_biomass_bym3_mean, size=fish_abundance_bym3))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_fish_biomass_v_abundance.png", width=40, height=20, unit="cm")



ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=pelagic_abundance_bym3))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_pelagic_abundance.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=demersal_abundance_bym3))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_demersal_abundance.png", width=40, height=20, unit="cm")


ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=fish_length))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_fish_length.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=log(fish_abundance_bym3+1)))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_fish_abundance_bym3.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=log(fish_biomass_bym3)))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_fish_biomass_bym3.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=bycatch_richness_corrected))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_bycatch_richness.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=log(bycatch_abundance_corrected+1)))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_bycatch_abundance.png", width=40, height=20, unit="cm")




### Pelagic vs. demersal fish 
ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=pelagic_richness_corrected))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_pelagic_richness.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=demersal_richness_corrected))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_demersal_richness.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=prop_pelagic_richness))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_prop_pelagic_richness.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=prop_pelagic_abundance))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_prop_pelagic_abundance.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=log(pelagic_abundance_bym3+1)))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_pelagic_abundance.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=log(demersal_abundance_bym3+1)))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_demersal_abundance.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=pelagic_abundance))+  scale_colour_viridis()
ggsave("C:Biodiversity idea//Plots//Maps//map_beachseine_pelagic_abundance.png", width=40, height=20, unit="cm")


# Adding in otter locations -----------------------------------------------

otter_isotopes<-read.csv("C:Food web idea//Data by person//Norah.data//master_otter_isotope_nb_new.csv")
head(otter_isotopes)
otter_isotopes_simple<-otter_isotopes[,c(7,9,10)]
head(otter_isotopes_simple)
names(otter_isotopes_simple)[1]<-"site"
otter_isotopes_simple$site <- as.factor(otter_isotopes_simple$site)
otter_isotopes_simple<- otter_isotopes_simple %>%  filter(site != "CVMIL12S8")


#NAD83
otter_isotopes_simple.SP <- st_as_sf(otter_isotopes_simple, coords = c("long", "lat"), crs = 4269)
head(otter_isotopes_simple.SP)
otter_isotopes_simple.SP.crs<-st_transform(x = otter_isotopes_simple.SP, crs = 4326)
head(otter_isotopes_simple.SP.crs)
otter_isotopes_simple.SP.crs$site_type<-"otter_scat"

#re-binding this without the lat and long
df.SF_transects_marine2<-rbind(df.SF_transects_simple, ben_habitat_data_simple.SP)
head(df.SF_transects_marine2)

df.SF_transects_marine_otter<-rbind(df.SF_transects_marine2, otter_isotopes_simple.SP.crs)
head(df.SF_transects_marine_otter)
#this has to be before the lat and long above

df.SF_transects_marine_otter$long<-st_coordinates(df.SF_transects_marine_otter)[,1] # get coordinates
df.SF_transects_marine_otter$lat<-st_coordinates(df.SF_transects_marine_otter)[,2]

ggmap(map_marine_trans) + geom_point(data=df.SF_transects_marine_otter, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()
ggsave("C:Food web idea//Plots//map_transects_beachseine_otter.png", width=40, height=20, unit="cm")



# Adding in otter N15 to otter location data ---------------------------------------------


### otter and N15 mapped
head(otter_isotopes)
#NAD83
otter_isotopes.SP <- st_as_sf(otter_isotopes, coords = c("long", "lat"), crs = 4269)
head(otter_isotopes.SP)
otter_isotopes.SP.crs<-st_transform(x = otter_isotopes.SP, crs = 4326)
head(otter_isotopes.SP.crs)

otter_isotopes.SP.crs$long<-st_coordinates(otter_isotopes.SP.crs)[,1] # get coordinates
otter_isotopes.SP.crs$lat<-st_coordinates(otter_isotopes.SP.crs)[,2]


bbox_otter <- make_bbox(otter_isotopes.SP.crs$long, otter_isotopes.SP.crs$lat, f = 0.01)
map_otter <- get_stamenmap(bbox_otter, source="stamen", maptype= "terrain", crop=FALSE)

# otter_isotopes.SP.crs.mm<-otter_isotopes.SP.crs %>% filter(node=="MM")
# bbox_otter_MM <- make_bbox(otter_isotopes.SP.crs.mm$long, otter_isotopes.SP.crs.mm$lat, f = 0.01)
# map_otter_MM <- get_stamenmap(bbox_otter_MM, source="stamen", maptype= "terrain", crop=FALSE)
# 
# otter_isotopes.SP.crs.cv<-otter_isotopes.SP.crs %>% filter(node=="CV")
# bbox_otter_CV <- make_bbox(otter_isotopes.SP.crs.cv$long, otter_isotopes.SP.crs.cv$lat, f = 0.01)
# map_otter_CV <- get_stamenmap(bbox_otter_CV, source="stamen", maptype= "terrain", crop=FALSE)
# 
# otter_isotopes.SP.crs.gs<-otter_isotopes.SP.crs %>% filter(node=="GG")
# bbox_otter_GS <- make_bbox(otter_isotopes.SP.crs.gs$long, otter_isotopes.SP.crs.gs$lat, f = 0.01)
# map_otter_GS <- get_stamenmap(bbox_otter_GS, source="stamen", maptype= "terrain", crop=FALSE)


ggmap(map_otter) + geom_point(data=otter_isotopes.SP.crs, aes(x = long, y = lat, col=d15n))+  scale_colour_viridis()
# 
# ggmap(map_otter_CV) + geom_point(data=otter_isotopes.SP.crs, aes(x = long, y = lat, col=d15n))+  scale_colour_viridis()
# 
# ggmap(map_otter_MM) + geom_point(data=otter_isotopes.SP.crs, aes(x = long, y = lat, col=d15n))+  scale_colour_viridis()
# 
# ggmap(map_otter_GS) + geom_point(data=otter_isotopes.SP.crs, aes(x = long, y = lat, col=d15n))+  scale_colour_viridis()
# ggmap(map_otter_MM) + geom_point(data=df.SF_transects_marine_otter, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()





###only chosen transects:

head(df.SF_transects_marine)
head(fish_richness_merged_tran_isl)
head(df.SF_transects_simple)

df.SF_transects_simple_subset<- subset(df.SF_transects_simple, site %in% fish_richness_merged_tran_isl$unq_tran)
head(df.SF_transects_simple_subset)

ben_habitat_data_simple.SP_subset<- subset(ben_habitat_data_simple.SP, site %in% fish_richness_merged_tran_isl$site)
head(ben_habitat_data_simple.SP_subset)

df.SF_transects_marine_subset<-rbind(df.SF_transects_simple_subset, ben_habitat_data_simple.SP_subset)
head(df.SF_transects_marine_subset)
df.SF_transects_marine_subset$long<-st_coordinates(df.SF_transects_marine_subset)[,1] # get coordinates
df.SF_transects_marine_subset$lat<-st_coordinates(df.SF_transects_marine_subset)[,2]

ggmap(map_marine_trans) + geom_point(data=df.SF_transects_marine_subset, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()


