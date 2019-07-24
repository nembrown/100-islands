setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Biodiversity idea")

#This script plots the locations of collected samples from three separate datasets
#Transects from 100 islands project, Beachsines from Hakai Nearshore, and Otter scats from Andrew Sheriff

##Loading necessary libraries
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


# Loading the data ------------------------------------------------------------


#loading in information on beachseine location
ben_habitat_data<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_habitat_20142018.csv")
head(ben_habitat_data)

#loading in beachseine data that's combined with transect information 
fish_richness_merged_tran_isl<-read.csv("C:Output files//fish_richness_merged_tran_isl.csv")
fish_richness_merged_tran_isl<-fish_richness_merged_tran_isl[,-1]


#loading information from 100 islands project
setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Food web idea")

#Island level location information
by_isl_master<-read.csv("C:Data by person//Owen's data//by_isl_master.csv")
head(by_isl_master)
by_isl_master<-by_isl_master[,-1]

#Transect-level information
by_tran_master<-read.csv("C:Data by person//Norah.data//by_tran_master.csv")
by_tran_master<-by_tran_master[,-1]


#isotopes on the island-level
isotope_by_isl_gathered4<- read.csv("C:Data by person//Norah.data/isotope_by_isl_gathered4.csv")
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
ben_habitat_data_simple$long<--(ben_habitat_data_simple$long)
ben_habitat_data_simple.SP <- st_as_sf(ben_habitat_data_simple, coords = c("long", "lat"), crs = 4326)
head(ben_habitat_data_simple.SP)
ben_habitat_data_simple.SP$site_type<-"beachseine"

### Islands level data just site-information
head(df.SF)
df.SF_simple<-unique(df.SF[,c(1)])
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
map_marine_big <- get_stamenmap(bbox_marine_big, source="stamen", maptype= "terrain", crop=FALSE, zoom=7)


ggmap(map_marine) + geom_point(data=df.SF_marine, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()


ggmap(map_marine_big) + stat_density2d(data=df.SF_marine, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()
ggmap(map_marine_big) + geom_blank(data=df.SF_marine, aes(x = long, y = lat, col=site_type))+  scale_colour_viridis_d()
ggsave("C:Plots//map_big.png", width=40, height=20, unit="cm")


# Transects --------------------------------------------------------------
head(by_tran_master)

data_subset2 <- by_tran_master[ , c("easting", "northing")]
by_tran_master_no_na<- by_tran_master[complete.cases(data_subset2), ] 


df.SF_transects <- st_as_sf(by_tran_master_no_na, coords = c("easting", "northing"), crs = 26909)
df.SF_transects<-st_transform(x = df.SF_transects, crs = 4326)
df.SF_transects$long<-st_coordinates(df.SF_transects)[,1] # get coordinates
df.SF_transects$lat<-st_coordinates(df.SF_transects)[,2] # get coordinates
head(df.SF_transects)


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
ggsave("C:Plots//map_transects_beachseine.png", width=40, height=20, unit="cm")



# Adding in some data to map just to beachseine dataset ----------------------------------------------
View(ben_beachseine)

fish_bycatch_richness_merged_tran_year<-read.csv("C:Output files//fish_bycatch_richness_merged_tran_year.csv")
fish_bycatch_richness_merged_tran_year<-fish_bycatch_richness_merged_tran_year[,-1]
head(fish_bycatch_richness_merged_tran_year)

head(ben_habitat_data_simple.SP)
ben_beachseine<-merge(ben_habitat_data_simple.SP, fish_bycatch_richness_merged_tran_year, by="site", all=TRUE)
head(ben_beachseine)

ben_beachseine$long<-st_coordinates(ben_beachseine)[,1] # get coordinates
ben_beachseine$lat<-st_coordinates(ben_beachseine)[,2]


bbox_marine_beachseine <- make_bbox(ben_beachseine$long, ben_beachseine$lat, f = 0.01)
map_marine_beachseine <- get_stamenmap(bbox_marine_beachseine, source="stamen", maptype= "terrain", crop=FALSE)

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=fish_richness))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_fish_richness_uncorrected.png", width=40, height=20, unit="cm")


ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=fish_richness_corrected))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_fish_richness.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=fish_length))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_fish_richness.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=log(fish_abundance_bym3)))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_fish_abundance_bym3.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=log(fish_biomass_bym3)))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_fish_biomass_bym3.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=bycatch_richness_corrected))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_bycatch_richness.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=log(bycatch_abundance_corrected+1)))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_bycatch_abundance.png", width=40, height=20, unit="cm")




### Pelagic vs. demersal fish 
ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=pelagic_richness_corrected))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_pelagic_richness.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=demersal_richness_corrected))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_demersal_richness.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=prop_pelagic_richness))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_prop_pelagic_richness.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=prop_pelagic_abundance))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_prop_pelagic_abundance.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=log(pelagic_abundance_bym3+1)))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_pelagic_abundance.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=log(demersal_abundance_bym3+1)))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_demersal_abundance.png", width=40, height=20, unit="cm")

ggmap(map_marine_beachseine) + geom_point(data=ben_beachseine, aes(x = long, y = lat, col=pelagic_abundance))+  scale_colour_viridis()
ggsave("C:Plots//Maps//map_beachseine_pelagic_abundance.png", width=40, height=20, unit="cm")


# Adding in otter locations -----------------------------------------------

otter_isotopes<-read.csv("C:Data by person//Norah.data//master_otter_isotope_nb_new.csv")
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
ggsave("C:Plots//map_transects_beachseine_otter.png", width=40, height=20, unit="cm")



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


