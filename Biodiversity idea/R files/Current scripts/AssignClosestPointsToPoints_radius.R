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
library(here)

#get Ben's data in right format
ben_habitat_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_habitat_20142018.csv")
head(ben_habitat_data)
ben_habitat_data_simple<-ben_habitat_data[,c(3:5)]
ben_habitat_data_simple$long<- -(ben_habitat_data_simple$long)

ben_habitat_data_simple.SP_new <- st_as_sf(ben_habitat_data_simple, coords = c("long", "lat"), crs = 4326)

ben_habitat_data_simple.SP_new<-ben_habitat_data_simple.SP_new %>% st_transform(3035)
head(ben_habitat_data_simple.SP_new)

#Transect-level information

by_tran_master<-read.csv("C:Food web idea//Data by person//Norah.data//by_tran_master.csv")
by_tran_master<-by_tran_master[,-1]
data_subset2 <- by_tran_master[ , c("easting", "northing")]
by_tran_master_no_na<- by_tran_master[complete.cases(data_subset2), ]
df.SF_transects <- st_as_sf(by_tran_master_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
df.SF_transects_simple<-df.SF_transects[,1]
df.SF_transects_simple_new<- df.SF_transects_simple %>% st_transform(3035) 
head(df.SF_transects_simple_new)

#https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a

# Buffer circles by 2000m -- creates polygons around the 
dat_circles <- st_buffer(df.SF_transects_simple_new, dist = 2000)

#which of the beachseines fall within 2km radius of the transect
transects_beach_joined <- st_join(ben_habitat_data_simple.SP_new, dat_circles, left=FALSE)

head(transects_beach_joined)

transects_beach_joined <-transects_beach_joined  %>% st_set_geometry(NULL)
transects_beach_joined<-as.data.frame(transects_beach_joined)
write.csv(transects_beach_joined, "C:Biodiversity idea//Output files//paired_sites_by_radius.csv", row.names = FALSE)




# Arch data ---------------------------------------------------------------

arch_data<-read.csv("C:Biodiversity idea//Output files//arch_sites_selected.csv")

arch_data_simple_new<-arch_data[ , c("site_id", "easting", "northing")]
data_arch_subset2_new <- arch_data_simple_new[ , c("easting", "northing")]
arch_data_simple_new<- arch_data_simple_new[complete.cases(data_arch_subset2_new), ] 
arch_data_simple_new$site_id<-as.factor(arch_data_simple_new$site_id)

arch_data_simple_sf_new <- st_as_sf(arch_data_simple_new, coords = c("easting", "northing"), crs = 26909)
arch_data_simple_st_new<-st_transform(x = arch_data_simple_sf_new, crs = 3035)
head(arch_data_simple_st_new)

#Transect-level information

by_tran_master<-read.csv("C:Food web idea//Data by person//Norah.data//by_tran_master.csv")
by_tran_master<-by_tran_master[,-1]
data_subset2 <- by_tran_master[ , c("easting", "northing")]
by_tran_master_no_na<- by_tran_master[complete.cases(data_subset2), ]
df.SF_transects <- st_as_sf(by_tran_master_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
df.SF_transects_simple<-df.SF_transects[,1]
df.SF_transects_simple_new<- df.SF_transects_simple %>% st_transform(3035) 
head(df.SF_transects_simple_new)

#https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a

# Buffer circles by 2000m -- creates polygons around the 
dat_circles_arch <- st_buffer(df.SF_transects_simple_new, dist = 300)

#which of the arch sites fall within 2km radius of the transect
transects_arch_joined <- st_join(arch_data_simple_st_new, dat_circles_arch, left=FALSE)

head(transects_arch_joined)
length(unique(transects_arch_joined$unq_tran))
#100m = 2 transects; 200m = 6 transects; 300m = 19 transects; 400m = 25 transects; 500m = 30 tran; 600m = 31 tran; 
#700m = 37 ; 800m = 41; 900m = 43 ; 1km = 47

transects_arch_joined <-transects_arch_joined  %>% st_set_geometry(NULL)
transects_arch_joined<-as.data.frame(transects_arch_joined)
write.csv(transects_arch_joined, "C:Biodiversity idea//Output files//paired_arch_by_radius_300.csv")


dat_circles_arch_big <- st_buffer(df.SF_transects_simple_new, dist = 1000)
transects_arch_joined_big <- st_join(arch_data_simple_st_new, dat_circles_arch_big, left=FALSE)
transects_arch_joined_big <-transects_arch_joined_big  %>% st_set_geometry(NULL)
transects_arch_joined_big<-as.data.frame(transects_arch_joined_big)
write.csv(transects_arch_joined_big, "C:Biodiversity idea//Output files//paired_arch_by_radius_1000.csv", row.names = FALSE)





###### arch and beachsine
head(arch_data_simple_st_new)
#created from above



ben_habitat_data<-read.csv("C:Biodiversity idea//Ben.data//beachseine_calvert_NB//hakaiBS_habitat_20142018.csv")
head(ben_habitat_data)
ben_habitat_data_simple<-ben_habitat_data[,c(3:5)]
ben_habitat_data_simple$long<- -(ben_habitat_data_simple$long)

ben_habitat_data_simple.SP_new <- st_as_sf(ben_habitat_data_simple, coords = c("long", "lat"), crs = 4326)

ben_habitat_data_simple.SP_new<-ben_habitat_data_simple.SP_new %>% st_transform(3035)
head(ben_habitat_data_simple.SP_new)

#https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a

# Buffer circles by 300m -- creates polygons around the beach sites
dat_circles_arch_beach <- st_buffer(ben_habitat_data_simple.SP_new, dist = 1000)

#which of the arch sites fall within 2km radius of the transect
beach_arch_joined <- st_join(arch_data_simple_st_new, dat_circles_arch_beach, left=FALSE)
length(unique(beach_arch_joined$site))
#100m = 8 beach seine; 200m = 20 beach seine; 300m = 25 beach; 400m = 26 beach; 500m = 27 beach; 600m = 31 beach; 
#700m = 33 ; 800m = 35; 900m = 37 ; 1km = 37; 2km = 45 beach 

beach_arch_joined <-beach_arch_joined  %>% st_set_geometry(NULL)
beach_arch_joined<-as.data.frame(beach_arch_joined)
write.csv(beach_arch_joined, "C:Biodiversity idea//Output files//paired_beach_arch_by_radius_1000.csv", row.names = FALSE)



