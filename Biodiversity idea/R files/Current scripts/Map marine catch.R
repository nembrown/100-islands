setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Biodiversity idea")

fish_richness_merged_isl<-read.csv("C:fish_richness_merged_isl.csv")
ben_habitat_data<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_habitat_20142018.csv")

head(fish_richness_merged_isl)
fish_richness_merged_isl<-fish_richness_merged_isl[,-1]

setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Modelling practice")
by_isl_master<-read.csv("C:Owen's data//by_isl_master.csv")
head(by_isl_master)
by_isl_master<-by_isl_master[,-1]

isotope_by_isl_gathered4<- read.csv("C:Norah.data/isotope_by_isl_gathered4.csv")
isotope_by_isl_gathered4<-isotope_by_isl_gathered4[,-1]

#library(devtools)
#install_github("r-spatial/sf")

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

##### Plotting map

###getting lat and long for the islands (from other mapping)
head(by_isl_master)
head(isotope_by_isl_gathered4)
which( colnames(by_isl_master)=="node" )
isotope_master_sf<-merge(isotope_by_isl_gathered4[,-c(6, 7)], by_isl_master[,c(1,11,12,31:89)], by="unq_isl", all=TRUE)
head(isotope_master_sf)

data_subset <- isotope_master_sf[ , c("C_Easting", "C_Northing")]
isotope_master_sf_no_na<- isotope_master_sf[complete.cases(data_subset), ] 


df.SF <- st_as_sf(isotope_master_sf_no_na, coords = c("C_Easting", "C_Northing"), crs = 26909)
df.SF<-st_transform(x = df.SF, crs = 4326)
df.SF$long<-st_coordinates(df.SF)[,1] # get coordinates
df.SF$lat<-st_coordinates(df.SF)[,2] # get coordinates
head(df.SF)

###adding in fish richness
head(fish_richness_merged_isl)
which( colnames(df.SF)=="lat" )
which( colnames(df.SF)=="long" )


head(ben_habitat_data)
ben_habitat_data_simple<-ben_habitat_data[,c(1:3)]
head(ben_habitat_data_simple)
ben_habitat_data_simple$long<--(ben_habitat_data_simple$long)
ben_habitat_data_simple.SP <- st_as_sf(ben_habitat_data_simple, coords = c("long", "lat"), crs = 4326)
head(ben_habitat_data_simple.SP)
ben_habitat_data_simple.SP$site_type<-"beachseine"

head(df.SF)
df.SF_simple<-unique(df.SF[,c(1)])
head(df.SF_simple)
names(df.SF_simple)[1]<-"site"
df.SF_simple$site_type<-"100_islands"

df.SF_marine<-rbind(df.SF_simple, ben_habitat_data_simple.SP)
head(df.SF_marine)
df.SF_marine$long<-st_coordinates(df.SF_marine)[,1] # get coordinates
df.SF_marine$lat<-st_coordinates(df.SF_marine)[,2]

######
bbox_marine <- make_bbox(df.SF_marine$long, df.SF_marine$lat, f = 0.01)
map_toner_lite_marine <- get_stamenmap(bbox_marine, source="stamen", maptype= "toner-lite", crop=FALSE)
map_marine <- get_stamenmap(bbox_marine, source="stamen", maptype= "terrain", crop=FALSE)


ggmap(map_marine) + geom_point(data=df.SF_marine, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()




# Transects --------------------------------------------------------------
setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Modelling practice")
soil_merge_0m<-read.csv("C:Norah.data\\soil_merge_0m.csv")
head(soil_merge_0m)
soil_merge_0m<-soil_merge_0m[,-1]

###### now with transects:
head(soil_merge_0m)
data_subset2 <- soil_merge_0m[ , c("easting", "northing")]
soil_merge_0m_no_na<- soil_merge_0m[complete.cases(data_subset2), ] 


df.SF_transects <- st_as_sf(soil_merge_0m_no_na, coords = c("easting", "northing"), crs = 26909)
df.SF_transects<-st_transform(x = df.SF_transects, crs = 4326)
df.SF_transects$long<-st_coordinates(df.SF_transects)[,1] # get coordinates
df.SF_transects$lat<-st_coordinates(df.SF_transects)[,2] # get coordinates
head(df.SF_transects)

###adding in fish richness
which( colnames(df.SF_transects)=="lat" )
which( colnames(df.SF_transects)=="long" )


head(ben_habitat_data)
ben_habitat_data_simple<-ben_habitat_data[,c(1:3)]
head(ben_habitat_data_simple)
ben_habitat_data_simple$long<--(ben_habitat_data_simple$long)
ben_habitat_data_simple.SP <- st_as_sf(ben_habitat_data_simple, coords = c("long", "lat"), crs = 4326)
head(ben_habitat_data_simple.SP)
ben_habitat_data_simple.SP$site_type<-"beachseine"

head(df.SF_transects)
df.SF_transects_simple<-unique(df.SF_transects[,c(9)])
head(df.SF_transects_simple)
names(df.SF_transects_simple)[1]<-"site"
df.SF_transects_simple$site_type<-"100_islands"

df.SF_transects_marine<-rbind(df.SF_transects_simple, ben_habitat_data_simple.SP)
head(df.SF_transects_marine)
df.SF_transects_marine$long<-st_coordinates(df.SF_transects_marine)[,1] # get coordinates
df.SF_transects_marine$lat<-st_coordinates(df.SF_transects_marine)[,2]


bbox_marine_trans <- make_bbox(df.SF_transects_marine$long, df.SF_transects_marine$lat, f = 0.01)
map_toner_lite_marine_trans <- get_stamenmap(bbox_marine_trans, source="stamen", maptype= "toner-lite", crop=FALSE)
map_marine_trans <- get_stamenmap(bbox_marine_trans, source="stamen", maptype= "terrain", crop=FALSE)


ggmap(map_marine_trans) + geom_point(data=df.SF_transects_marine, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()


setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Modelling practice")
#### adding in otter information
otter_isotopes<-read.csv("C:Norah.data\\master_otter_isotope_nb_new.csv")
head(otter_isotopes)
otter_isotopes<- otter_isotopes %>%  filter(site.id != "CVMIL12S8")
otter_isotopes_simple<-otter_isotopes[,c(7,9,10)]
head(otter_isotopes_simple)
names(otter_isotopes_simple)[1]<-"site"

#NAD83
otter_isotopes_simple.SP <- st_as_sf(otter_isotopes_simple, coords = c("long", "lat"), crs = 4269)
head(otter_isotopes_simple.SP)
otter_isotopes_simple.SP.crs<-st_transform(x = otter_isotopes_simple.SP, crs = 4326)
head(otter_isotopes_simple.SP.crs)
otter_isotopes_simple.SP.crs$site_type<-"otter_scat"

df.SF_transects_marine_otter<-rbind(df.SF_transects_marine, otter_isotopes_simple.SP.crs)
head(df.SF_transects_marine_otter)
#this has to be before the lat and long above

df.SF_transects_marine_otter$long<-st_coordinates(df.SF_transects_marine_otter)[,1] # get coordinates
df.SF_transects_marine_otter$lat<-st_coordinates(df.SF_transects_marine_otter)[,2]



ggmap(map_marine_trans) + geom_point(data=df.SF_transects_marine_otter, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()


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

otter_isotopes.SP.crs.mm<-otter_isotopes.SP.crs %>% filter(node=="MM")
bbox_otter_MM <- make_bbox(otter_isotopes.SP.crs.mm$long, otter_isotopes.SP.crs.mm$lat, f = 0.01)
map_otter_MM <- get_stamenmap(bbox_otter_MM, source="stamen", maptype= "terrain", crop=FALSE)

otter_isotopes.SP.crs.cv<-otter_isotopes.SP.crs %>% filter(node=="CV")
bbox_otter_CV <- make_bbox(otter_isotopes.SP.crs.cv$long, otter_isotopes.SP.crs.cv$lat, f = 0.01)
map_otter_CV <- get_stamenmap(bbox_otter_CV, source="stamen", maptype= "terrain", crop=FALSE)

otter_isotopes.SP.crs.gs<-otter_isotopes.SP.crs %>% filter(node=="GG")
bbox_otter_GS <- make_bbox(otter_isotopes.SP.crs.gs$long, otter_isotopes.SP.crs.gs$lat, f = 0.01)
map_otter_GS <- get_stamenmap(bbox_otter_GS, source="stamen", maptype= "terrain", crop=FALSE)


ggmap(map_otter) + geom_point(data=otter_isotopes.SP.crs, aes(x = long, y = lat, col=d15n))+  scale_colour_viridis()

ggmap(map_otter_CV) + geom_point(data=otter_isotopes.SP.crs, aes(x = long, y = lat, col=d15n))+  scale_colour_viridis()

ggmap(map_otter_MM) + geom_point(data=otter_isotopes.SP.crs, aes(x = long, y = lat, col=d15n))+  scale_colour_viridis()

ggmap(map_otter_GS) + geom_point(data=otter_isotopes.SP.crs, aes(x = long, y = lat, col=d15n))+  scale_colour_viridis()



ggmap(map_otter_MM) + geom_point(data=df.SF_transects_marine_otter, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()





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


