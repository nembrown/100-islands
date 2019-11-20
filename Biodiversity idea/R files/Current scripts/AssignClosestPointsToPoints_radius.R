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

ben_habitat_data_simple.SP_new <- st_as_sf(ben_habitat_data_simple, coords = c("long", "lat"), crs = 4326)%>% st_transform(3035)
head(ben_habitat_data_simple.SP_new)

#Transect-level information

by_tran_master<-read.csv("C:Food web idea//Data by person//Norah.data//by_tran_master.csv")
by_tran_master<-by_tran_master[,-1]
data_subset2 <- by_tran_master[ , c("easting", "northing")]
by_tran_master_no_na<- by_tran_master[complete.cases(data_subset2), ]
df.SF_transects <- st_as_sf(by_tran_master_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
df.SF_transects_simple<-df.SF_transects[,1]
df.SF_transects_simple_new<- df.SF_transects_simple %>% st_transform(3035) 


#https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a

# Buffer circles by 2000m -- creates polygons around the 
dat_circles <- st_buffer(df.SF_transects_simple_new, dist = 2000)

#which of the beachseines fall within 2km radius of the transect
transects_beach_joined = st_join(ben_habitat_data_simple.SP_new, dat_circles, left=FALSE)

head(transects_beach_joined)

transects_beach_joined <-transects_beach_joined  %>% st_set_geometry(NULL)
transects_beach_joined<-as.data.frame(transects_beach_joined)
write.csv(transects_beach_joined, "C:Biodiversity idea//Output files//paired_sites_by_radius.csv")
