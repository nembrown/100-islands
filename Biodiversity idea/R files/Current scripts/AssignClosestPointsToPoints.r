#######################################################################################
#
# AssignClosestTempPoints(): R function
#  
# Abstract:
# This R script assigns the closest of a set of point temparature measurements 
# to each of a second set of point site measurements.
# The sp package 'spDistsN1' function is used to calculate Euclidean distances
# between points.
# 
# 
# Input Files (2): Comma-separated-value (CSV) files containing, respectively,
#                  point beachseine and site measurements. 
# 
# Output File: Comma-separated-value (CSV) file containing the results table:
#              point site measurements and their location, and the location 
#              index, closest temparature measurement, and distance to the 
#              assigned measurement point.
#
# Notes:  To execute this script: 1) Start R,
#                                 2) Load this script into the workspace,
#                                 3) At R prompt, enter > AssignClosestTempPoints()
#               
# Author: Rick Reeves
# Date created: August 2007    
# Date revised: July 2010
# National Center for Ecological Analysis and Synthesis (NCEAS),
# University of California, Santa Barbara, CA USA
# http://www.nceas.ucsb.edu/scicomp
#
#######################################################################################


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


# Islands -----------------------------------------------------------------


setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Biodiversity idea")

head(ben_habitat_data_simple)
bens_sites<-ben_habitat_data_simple
names(bens_sites)[1]<-"beachseine"
head(bens_sites)

df.SF_simple$long<-st_coordinates(df.SF_simple)[,1] # get coordinates
df.SF_simple$lat<-st_coordinates(df.SF_simple)[,2]
df.SF_simple_df<-df.SF_simple %>% st_set_geometry(NULL)
head(df.SF_simple_df)
df.SF_simple_df<-df.SF_simple_df[,-2]


AssignClosestTempPoints <- function()
{

# Latest version: Assign closest points from a second point set

   require(sp)
   
 
# promote the input lists to SpatialPointsDataFrames

   coordinates(bens_sites) <- c("long", "lat")
   coordinates(df.SF_simple_df) <- c("long", "lat")             


#  Define these vectors, used in the loop.

   closestSiteVec <- vector(mode = "numeric",length = nrow(df.SF_simple_df))
   minDistVec     <- vector(mode = "numeric",length = nrow(df.SF_simple_df))

# Get the vector index of the beachseine station closest to each field station.
# Use the spDistsN1 function to compute the distance vector between each
# field station site and all of the beachseine stations. Then, find and
# retain the actual beachseine, and the index of the closest beachseine
# to each transect station.
#
# spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
#
# where:
#         pointList   : List of candidate points.
#         pointToMatch: Single point for which we seek the closest point in pointList.
#         longlat     : TRUE  computes Great Circle distance in km,
#                       FALSE computes Euclidean distance in units of input geographic coordinates
#
# We use Great Circle distance to increase distance calculation accuracy at high latitudes
# See the discussion of distance units in the header portion of this file
#
# minDistVec stores the distance from to the closest beachseine station to each site measurement point.
# closestSiteVec stores the index of the closest beachseine station to each site measurement point.
#
   for (i in 1 : nrow(df.SF_simple_df))
   {
      distVec <- spDistsN1(bens_sites,df.SF_simple_df[i,],longlat = TRUE)
      minDistVec[i] <- min(distVec)
      closestSiteVec[i] <- which.min(distVec)
   }
#
# Create the output file: merge the beachseine point list with the transect point list
# into a five-column table by merging the beachseine point and transect point lists.
#
   PointAssignTemps <- as(bens_sites[closestSiteVec,]$beachseine,"character")
   FinalTable = data.frame(coordinates(df.SF_simple_df),df.SF_simple_df$site,closestSiteVec,minDistVec,PointAssignTemps)
#
# Update the FinalTable column names 
#
   names(FinalTable) <- c("Long","Lat","site","CloseTempIndex","Distance","beachseine")
#
# And, at the end, write the point assignment file.
#
  message("Write beachseine/site assignment table to disk in .csv format")
  write.csv(FinalTable,file="Distance_btwn_points.csv")
}

AssignClosestTempPoints()


# Transects ---------------------------------------------------------------


### now with transects
setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Biodiversity idea")

head(ben_habitat_data_simple)
bens_sites<-ben_habitat_data_simple
names(bens_sites)[1]<-"beachseine"
head(bens_sites)

by_tran_master<-read.csv("C:Food web idea//Data by person//Norah.data//by_tran_master.csv")
data_subset2 <- by_tran_master[ , c("easting", "northing")]
by_tran_master_no_na<- by_tran_master[complete.cases(data_subset2), ] 
df.SF_transects <- st_as_sf(by_tran_master_no_na, coords = c("easting", "northing"), crs = 26909)
df.SF_transects<-st_transform(x = df.SF_transects, crs = 4326)
df.SF_transects$long<-st_coordinates(df.SF_transects)[,1] # get coordinates
df.SF_transects$lat<-st_coordinates(df.SF_transects)[,2] # get coordinates
head(df.SF_transects)

head(df.SF_transects)
df.SF_transects_simple<-df.SF_transects[,1]
head(df.SF_transects_simple)
names(df.SF_transects_simple)[1]<-"site"
df.SF_transects_simple$site_type<-"100_islands"



df.SF_transects_simple$long<-st_coordinates(df.SF_transects_simple)[,1] # get coordinates
df.SF_transects_simple$lat<-st_coordinates(df.SF_transects_simple)[,2]
df.SF_transects_simple_df<-df.SF_transects_simple %>% st_set_geometry(NULL)
head(df.SF_transects_simple_df)
df.SF_transects_simple_df<-df.SF_transects_simple_df[,-2]

head(df.SF_transects_simple_df)

AssignClosestTempPoints2 <- function()
{
   
   # Latest version: Assign closest points from a second point set
   
   require(sp)
   
   
   # promote the input lists to SpatialPointsDataFrames
   
   coordinates(bens_sites) <- c("long", "lat")
   coordinates(df.SF_transects_simple_df) <- c("long", "lat")             
   
   
   #  Define these vectors, used in the loop.
   
   closestSiteVec <- vector(mode = "numeric",length = nrow(df.SF_transects_simple_df))
   minDistVec     <- vector(mode = "numeric",length = nrow(df.SF_transects_simple_df))
   
   # Get the vector index of the beachseine station closest to each field station.
   # Use the spDistsN1 function to compute the distance vector between each
   # field station site and all of the beachseine stations. Then, find and
   # retain the actual beachseine, and the index of the closest beachseine
   # to each transect station.
   #
   # spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
   #
   # where:
   #         pointList   : List of candidate points.
   #         pointToMatch: Single point for which we seek the closest point in pointList.
   #         longlat     : TRUE  computes Great Circle distance in km,
   #                       FALSE computes Euclidean distance in units of input geographic coordinates
   #
   # We use Great Circle distance to increase distance calculation accuracy at high latitudes
   # See the discussion of distance units in the header portion of this file
   #
   # minDistVec stores the distance from to the closest beachseine station to each site measurement point.
   # closestSiteVec stores the index of the closest beachseine station to each site measurement point.
   #
   for (i in 1 : nrow(df.SF_transects_simple_df))
   {
      distVec <- spDistsN1(bens_sites,df.SF_transects_simple_df[i,],longlat = TRUE)
      minDistVec[i] <- min(distVec)
      closestSiteVec[i] <- which.min(distVec)
   }
   #
   # Create the output file: merge the beachseine point list with the transect point list
   # into a five-column table by merging the beachseine point and transect point lists.
   #
   PointAssignTemps <- as(bens_sites[closestSiteVec,]$beachseine,"character")
   FinalTable2 = data.frame(coordinates(df.SF_transects_simple_df),df.SF_transects_simple_df$site,closestSiteVec,minDistVec,PointAssignTemps)
   #
   # Update the FinalTable column names 
   #
   names(FinalTable2) <- c("Long","Lat","site","CloseTempIndex","Distance","beachseine")
   #
   # And, at the end, write the point assignment file.
   #
   message("Write beachseine/site assignment table to disk in .csv format")
   write.csv(FinalTable2,file="C:Output files//Distance_btwn_points_transects.csv")
}

AssignClosestTempPoints2()



# Otter and beachseine ----------------------------------------------------
setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Modelling practice/Norah.data")
otter_isotopes<-read.csv("C:master_otter_isotope_nb_new.csv")

setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Biodiversity idea")

head(ben_habitat_data_simple)
bens_sites<-ben_habitat_data_simple
names(bens_sites)[1]<-"beachseine"
head(bens_sites)

head(otter_isotopes)
otter_isotopes_simple<-otter_isotopes[,c(7,9,10)]
head(otter_isotopes_simple)
names(otter_isotopes_simple)[1]<-"site"


AssignClosestTempPoints3 <- function()
{
   
   # Latest version: Assign closest points from a second point set
   
   require(sp)
   
   
   # promote the input lists to SpatialPointsDataFrames
   
   coordinates(bens_sites) <- c("long", "lat")
   coordinates(otter_isotopes_simple) <- c("long", "lat")             
   
   
   #  Define these vectors, used in the loop.
   
   closestSiteVec <- vector(mode = "numeric",length = nrow(otter_isotopes_simple))
   minDistVec     <- vector(mode = "numeric",length = nrow(otter_isotopes_simple))
   
   # Get the vector index of the beachseine station closest to each field station.
   # Use the spDistsN1 function to compute the distance vector between each
   # field station site and all of the beachseine stations. Then, find and
   # retain the actual beachseine, and the index of the closest beachseine
   # to each transect station.
   #
   # spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
   #
   # where:
   #         pointList   : List of candidate points.
   #         pointToMatch: Single point for which we seek the closest point in pointList.
   #         longlat     : TRUE  computes Great Circle distance in km,
   #                       FALSE computes Euclidean distance in units of input geographic coordinates
   #
   # We use Great Circle distance to increase distance calculation accuracy at high latitudes
   # See the discussion of distance units in the header portion of this file
   #
   # minDistVec stores the distance from to the closest beachseine station to each site measurement point.
   # closestSiteVec stores the index of the closest beachseine station to each site measurement point.
   #
   for (i in 1 : nrow(otter_isotopes_simple))
   {
      distVec <- spDistsN1(bens_sites,otter_isotopes_simple[i,],longlat = TRUE)
      minDistVec[i] <- min(distVec)
      closestSiteVec[i] <- which.min(distVec)
   }
   #
   # Create the output file: merge the beachseine point list with the transect point list
   # into a five-column table by merging the beachseine point and transect point lists.
   #
   PointAssignTemps <- as(bens_sites[closestSiteVec,]$beachseine,"character")
   FinalTable3 = data.frame(coordinates(otter_isotopes_simple),otter_isotopes_simple$site,closestSiteVec,minDistVec,PointAssignTemps)
   #
   # Update the FinalTable column names 
   #
   names(FinalTable3) <- c("Long","Lat","site","CloseTempIndex","Distance","beachseine")
   #
   # And, at the end, write the point assignment file.
   #
   message("Write beachseine/site assignment table to disk in .csv format")
   write.csv(FinalTable3,file="Distance_btwn_points_otters_beachseine.csv")
}

AssignClosestTempPoints3()


# Otter to transects ------------------------------------------------------

setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Biodiversity idea")

df.SF_transects_simple$long<-st_coordinates(df.SF_transects_simple)[,1] # get coordinates
df.SF_transects_simple$lat<-st_coordinates(df.SF_transects_simple)[,2]
df.SF_transects_simple_df<-df.SF_transects_simple %>% st_set_geometry(NULL)
head(df.SF_transects_simple_df)
df.SF_transects_simple_df<-df.SF_transects_simple_df[,-2]
names(df.SF_transects_simple_df)[1]<-"transect"

head(otter_isotopes)
otter_isotopes_simple<-otter_isotopes[,c(7,9,10)]
head(otter_isotopes_simple)
names(otter_isotopes_simple)[1]<-"site"


AssignClosestTempPoints4 <- function()
{
   
   # Latest version: Assign closest points from a second point set
   
   require(sp)
   
   
   # promote the input lists to SpatialPointsDataFrames
   
   coordinates(df.SF_transects_simple_df) <- c("long", "lat")
   coordinates(otter_isotopes_simple) <- c("long", "lat")             
   
   
   #  Define these vectors, used in the loop.
   
   closestSiteVec <- vector(mode = "numeric",length = nrow(otter_isotopes_simple))
   minDistVec     <- vector(mode = "numeric",length = nrow(otter_isotopes_simple))
   
   # Get the vector index of the transect station closest to each field station.
   # Use the spDistsN1 function to compute the distance vector between each
   # field station site and all of the transect stations. Then, find and
   # retain the actual transect, and the index of the closest transect
   # to each transect station.
   #
   # spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
   #
   # where:
   #         pointList   : List of candidate points.
   #         pointToMatch: Single point for which we seek the closest point in pointList.
   #         longlat     : TRUE  computes Great Circle distance in km,
   #                       FALSE computes Euclidean distance in units of input geographic coordinates
   #
   # We use Great Circle distance to increase distance calculation accuracy at high latitudes
   # See the discussion of distance units in the header portion of this file
   #
   # minDistVec stores the distance from to the closest transect station to each site measurement point.
   # closestSiteVec stores the index of the closest transect station to each site measurement point.
   #
   for (i in 1 : nrow(otter_isotopes_simple))
   {
      distVec <- spDistsN1(df.SF_transects_simple_df,otter_isotopes_simple[i,],longlat = TRUE)
      minDistVec[i] <- min(distVec)
      closestSiteVec[i] <- which.min(distVec)
   }
   #
   # Create the output file: merge the transect point list with the transect point list
   # into a five-column table by merging the transect point and transect point lists.
   #
   PointAssignTemps <- as(df.SF_transects_simple_df[closestSiteVec,]$transect,"character")
   FinalTable4 = data.frame(coordinates(otter_isotopes_simple),otter_isotopes_simple$site,closestSiteVec,minDistVec,PointAssignTemps)
   #
   # Update the FinalTable column names 
   #
   names(FinalTable4) <- c("Long","Lat","site","CloseTempIndex","Distance","transect")
   #
   # And, at the end, write the point assignment file.
   #
   message("Write transect/site assignment table to disk in .csv format")
   write.csv(FinalTable4,file="Distance_btwn_points_otters_transect.csv")
}

AssignClosestTempPoints4()


# Transects to midden sites -------------------------------------------------
arch_data<-read.csv("C:Biodiversity idea//Output files//arch_sites_selected.csv")
head(arch_data)
midden_data<-arch_data[arch_data$midden_feature=="yes",] 
head(midden_data)
midden_data_simple_new<-midden_data[ , c("site_id", "easting", "northing")]
data_midden_subset2_new <- midden_data_simple_new[ , c("easting", "northing")]
midden_data_simple_new<- midden_data_simple_new[complete.cases(data_midden_subset2_new), ] 
midden_data_simple_new$site_id<-as.factor(midden_data_simple_new$site_id)
midden_data_simple_sf_new <- st_as_sf(midden_data_simple_new, coords = c("easting", "northing"), crs = 26909)
midden_data_simple_st_new_long<-st_transform(x = midden_data_simple_sf_new, crs = 4326)
midden_data_simple_st_new_long$long<-st_coordinates(midden_data_simple_st_new_long)[,1]
midden_data_simple_st_new_long$lat<-st_coordinates(midden_data_simple_st_new_long)[,2]
midden_data_simple_st_new_long<-midden_data_simple_st_new_long %>% st_set_geometry(NULL)
names(midden_data_simple_st_new_long)[1]<-"midden"
head(midden_data_simple_st_new_long)


soil_merge_0m<- read.csv("C:Food web idea\\Data by person\\Norah.data\\soil_merge_0m.csv")
head(soil_merge_0m)
soil_merge_0m_simple<-soil_merge_0m %>% dplyr::select("unq_tran", "easting", "northing")
soil_merge_0m_simple<-soil_merge_0m_simple[!duplicated(soil_merge_0m_simple$unq_tran),]
head(soil_merge_0m_simple)

data_subset2_soil <- soil_merge_0m_simple[ , c("easting", "northing")]
soil_merge_0m_simple_no_na<- soil_merge_0m_simple[complete.cases(data_subset2_soil), ]
df_soil_merge_0m_simple <- st_as_sf(soil_merge_0m_simple_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
names(df_soil_merge_0m_simple)[1]<-"site"
head(df_soil_merge_0m_simple)
df_soil_merge_0m_simple$long<-st_coordinates(df_soil_merge_0m_simple)[,1] # get coordinates
df_soil_merge_0m_simple$lat<-st_coordinates(df_soil_merge_0m_simple)[,2]
df_soil_merge_0m_simple_df<-df_soil_merge_0m_simple %>% st_set_geometry(NULL)
head(df_soil_merge_0m_simple_df)

#Need:
head(df_soil_merge_0m_simple_df)
View(midden_data_simple_st_new_long)




AssignClosestTempPoints8 <- function()
{
   
   # Latest version: Assign closest points from a second point set
   
   require(sp)
   
   
   # promote the input lists to SpatialPointsDataFrames
   
   coordinates(midden_data_simple_st_new_long) <- c("long", "lat")
   coordinates(df_soil_merge_0m_simple_df) <- c("long", "lat")             
   
   
   #  Define these vectors, used in the loop.
   
   closestSiteVec <- vector(mode = "numeric",length = nrow(df_soil_merge_0m_simple_df))
   minDistVec     <- vector(mode = "numeric",length = nrow(df_soil_merge_0m_simple_df))
   
   # Get the vector index of the midden station closest to each field station.
   # Use the spDistsN1 function to compute the distance vector between each
   # field station site and all of the midden stations. Then, find and
   # retain the actual midden, and the index of the closest midden
   # to each transect station.
   #
   # spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
   #
   # where:
   #         pointList   : List of candidate points.
   #         pointToMatch: Single point for which we seek the closest point in pointList.
   #         longlat     : TRUE  computes Great Circle distance in km,
   #                       FALSE computes Euclidean distance in units of input geographic coordinates
   #
   # We use Great Circle distance to increase distance calculation accuracy at high latitudes
   # See the discussion of distance units in the header portion of this file
   #
   # minDistVec stores the distance from to the closest midden station to each site measurement point.
   # closestSiteVec stores the index of the closest midden station to each site measurement point.
   #
   for (i in 1 : nrow(df_soil_merge_0m_simple_df))
   {
      distVec <- spDistsN1(midden_data_simple_st_new_long,df_soil_merge_0m_simple_df[i,],longlat = TRUE)
      minDistVec[i] <- min(distVec)
      closestSiteVec[i] <- which.min(distVec)
   }
   #
   # Create the output file: merge the midden point list with the transect point list
   # into a five-column table by merging the midden point and transect point lists.
   #
   PointAssignTemps <- as(midden_data_simple_st_new_long[closestSiteVec,]$midden,"character")
   FinalTable8 = data.frame(coordinates(df_soil_merge_0m_simple_df),df_soil_merge_0m_simple_df$site,closestSiteVec,minDistVec,PointAssignTemps)
   #
   # Update the FinalTable column names 
   #
   names(FinalTable8) <- c("Long","Lat","site","CloseTempIndex","Distance","midden")
   #
   # And, at the end, write the point assignment file.
   #
   message("Write middenne/site assignment table to disk in .csv format")
   write.csv(FinalTable8,file="C:Biodiversity idea//Output files//Distance_btwn_points_midden_transects.csv", row.names = FALSE)
}

AssignClosestTempPoints8()


# Raven distance ----------------------------------------------------------
ravens <- read.csv("C:Food web idea//Data by person//Deb.data/ravens.csv")
head(ravens)
ravens$PCID<-paste(ravens$island, ravens$point, sept="")
ravens$PCID<-str_trim(ravens$PCID, "right")
ravens$PCID<-gsub(" ", "-", ravens$PCID, fixed = TRUE)

raven_data_simple_new<-ravens[ , c("PCID", "easting", "northing")]
data_raven_subset2_new <- raven_data_simple_new[ , c("easting", "northing")]
raven_data_simple_new<- raven_data_simple_new[complete.cases(data_raven_subset2_new), ] 
raven_data_simple_new$PCID<-as.factor(raven_data_simple_new$PCID)
raven_data_simple_sf_new <- st_as_sf(raven_data_simple_new, coords = c("easting", "northing"), crs = 26909)
raven_data_simple_st_new_long<-st_transform(x = raven_data_simple_sf_new, crs = 4326)
raven_data_simple_st_new_long$long<-st_coordinates(raven_data_simple_st_new_long)[,1]
raven_data_simple_st_new_long$lat<-st_coordinates(raven_data_simple_st_new_long)[,2]
raven_data_simple_st_new_long<-raven_data_simple_st_new_long %>% st_set_geometry(NULL)
names(raven_data_simple_st_new_long)[1]<-"raven"
head(raven_data_simple_st_new_long)


soil_merge_0m<- read.csv("C:Food web idea\\Data by person\\Norah.data\\soil_merge_0m.csv")
head(soil_merge_0m)
soil_merge_0m_simple<-soil_merge_0m %>% dplyr::select("unq_tran", "easting", "northing")
soil_merge_0m_simple<-soil_merge_0m_simple[!duplicated(soil_merge_0m_simple$unq_tran),]
head(soil_merge_0m_simple)

data_subset2_soil <- soil_merge_0m_simple[ , c("easting", "northing")]
soil_merge_0m_simple_no_na<- soil_merge_0m_simple[complete.cases(data_subset2_soil), ]
df_soil_merge_0m_simple <- st_as_sf(soil_merge_0m_simple_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
names(df_soil_merge_0m_simple)[1]<-"site"
head(df_soil_merge_0m_simple)
df_soil_merge_0m_simple$long<-st_coordinates(df_soil_merge_0m_simple)[,1] # get coordinates
df_soil_merge_0m_simple$lat<-st_coordinates(df_soil_merge_0m_simple)[,2]
df_soil_merge_0m_simple_df<-df_soil_merge_0m_simple %>% st_set_geometry(NULL)
head(df_soil_merge_0m_simple_df)

#Need:
head(df_soil_merge_0m_simple_df)
head(raven_data_simple_st_new_long)
 

AssignClosestTempPoints_raven <- function()
{
   
   # Latest version: Assign closest points from a second point set
   
   require(sp)
   
   
   # promote the input lists to SpatialPointsDataFrames
   
   coordinates(raven_data_simple_st_new_long) <- c("long", "lat")
   coordinates(df_soil_merge_0m_simple_df) <- c("long", "lat")             
   
   
   #  Define these vectors, used in the loop.
   
   closestSiteVec <- vector(mode = "numeric",length = nrow(df_soil_merge_0m_simple_df))
   minDistVec     <- vector(mode = "numeric",length = nrow(df_soil_merge_0m_simple_df))
   
   # Get the vector index of the raven station closest to each field station.
   # Use the spDistsN1 function to compute the distance vector between each
   # field station site and all of the raven stations. Then, find and
   # retain the actual raven, and the index of the closest raven
   # to each transect station.
   #
   # spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
   #
   # where:
   #         pointList   : List of candidate points.
   #         pointToMatch: Single point for which we seek the closest point in pointList.
   #         longlat     : TRUE  computes Great Circle distance in km,
   #                       FALSE computes Euclidean distance in units of input geographic coordinates
   #
   # We use Great Circle distance to increase distance calculation accuracy at high latitudes
   # See the discussion of distance units in the header portion of this file
   #
   # minDistVec stores the distance from to the closest raven station to each site measurement point.
   # closestSiteVec stores the index of the closest raven station to each site measurement point.
   #
   for (i in 1 : nrow(df_soil_merge_0m_simple_df))
   {
      distVec <- spDistsN1(raven_data_simple_st_new_long,df_soil_merge_0m_simple_df[i,],longlat = TRUE)
      minDistVec[i] <- min(distVec)
      closestSiteVec[i] <- which.min(distVec)
   }
   #
   # Create the output file: merge the raven point list with the transect point list
   # into a five-column table by merging the raven point and transect point lists.
   #
   PointAssignTemps <- as(raven_data_simple_st_new_long[closestSiteVec,]$raven,"character")
   FinalTable_raven = data.frame(coordinates(df_soil_merge_0m_simple_df),df_soil_merge_0m_simple_df$site,closestSiteVec,minDistVec,PointAssignTemps)
   #
   # Update the FinalTable column names 
   #
   names(FinalTable_raven) <- c("Long","Lat","site","CloseTempIndex","Distance","raven")
   #
   # And, at the end, write the point assignment file.
   #
   message("Write ravenne/site assignment table to disk in .csv format")
   write.csv(FinalTable_raven,file="C:Biodiversity idea//Output files//Distance_btwn_points_raven_transects.csv", row.names = FALSE)
}

AssignClosestTempPoints_raven()


# eagle distance ----------------------------------------------------------
eagles <- read.csv("C:Food web idea//Data by person//Deb.data/eagles.csv")
head(eagles)
eagles$PCID<-paste(eagles$island, eagles$point, sept="")
eagles$PCID<-str_trim(eagles$PCID, "right")
eagles$PCID<-gsub(" ", "-", eagles$PCID, fixed = TRUE)

eagle_data_simple_new<-eagles[ , c("PCID", "easting", "northing")]
data_eagle_subset2_new <- eagle_data_simple_new[ , c("easting", "northing")]
eagle_data_simple_new<- eagle_data_simple_new[complete.cases(data_eagle_subset2_new), ] 
eagle_data_simple_new$PCID<-as.factor(eagle_data_simple_new$PCID)
eagle_data_simple_sf_new <- st_as_sf(eagle_data_simple_new, coords = c("easting", "northing"), crs = 26909)
eagle_data_simple_st_new_long<-st_transform(x = eagle_data_simple_sf_new, crs = 4326)
eagle_data_simple_st_new_long$long<-st_coordinates(eagle_data_simple_st_new_long)[,1]
eagle_data_simple_st_new_long$lat<-st_coordinates(eagle_data_simple_st_new_long)[,2]
eagle_data_simple_st_new_long<-eagle_data_simple_st_new_long %>% st_set_geometry(NULL)
names(eagle_data_simple_st_new_long)[1]<-"eagle"
head(eagle_data_simple_st_new_long)


soil_merge_0m<- read.csv("C:Food web idea\\Data by person\\Norah.data\\soil_merge_0m.csv")
head(soil_merge_0m)
soil_merge_0m_simple<-soil_merge_0m %>% dplyr::select("unq_tran", "easting", "northing")
soil_merge_0m_simple<-soil_merge_0m_simple[!duplicated(soil_merge_0m_simple$unq_tran),]
head(soil_merge_0m_simple)

data_subset2_soil <- soil_merge_0m_simple[ , c("easting", "northing")]
soil_merge_0m_simple_no_na<- soil_merge_0m_simple[complete.cases(data_subset2_soil), ]
df_soil_merge_0m_simple <- st_as_sf(soil_merge_0m_simple_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
names(df_soil_merge_0m_simple)[1]<-"site"
head(df_soil_merge_0m_simple)
df_soil_merge_0m_simple$long<-st_coordinates(df_soil_merge_0m_simple)[,1] # get coordinates
df_soil_merge_0m_simple$lat<-st_coordinates(df_soil_merge_0m_simple)[,2]
df_soil_merge_0m_simple_df<-df_soil_merge_0m_simple %>% st_set_geometry(NULL)
head(df_soil_merge_0m_simple_df)

#Need:
head(df_soil_merge_0m_simple_df)
head(eagle_data_simple_st_new_long)


AssignClosestTempPoints_eagle <- function()
{
   
   # Latest version: Assign closest points from a second point set
   
   require(sp)
   
   
   # promote the input lists to SpatialPointsDataFrames
   
   coordinates(eagle_data_simple_st_new_long) <- c("long", "lat")
   coordinates(df_soil_merge_0m_simple_df) <- c("long", "lat")             
   
   
   #  Define these vectors, used in the loop.
   
   closestSiteVec <- vector(mode = "numeric",length = nrow(df_soil_merge_0m_simple_df))
   minDistVec     <- vector(mode = "numeric",length = nrow(df_soil_merge_0m_simple_df))
   
   # Get the vector index of the eagle station closest to each field station.
   # Use the spDistsN1 function to compute the distance vector between each
   # field station site and all of the eagle stations. Then, find and
   # retain the actual eagle, and the index of the closest eagle
   # to each transect station.
   #
   # spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
   #
   # where:
   #         pointList   : List of candidate points.
   #         pointToMatch: Single point for which we seek the closest point in pointList.
   #         longlat     : TRUE  computes Great Circle distance in km,
   #                       FALSE computes Euclidean distance in units of input geographic coordinates
   #
   # We use Great Circle distance to increase distance calculation accuracy at high latitudes
   # See the discussion of distance units in the header portion of this file
   #
   # minDistVec stores the distance from to the closest eagle station to each site measurement point.
   # closestSiteVec stores the index of the closest eagle station to each site measurement point.
   #
   for (i in 1 : nrow(df_soil_merge_0m_simple_df))
   {
      distVec <- spDistsN1(eagle_data_simple_st_new_long,df_soil_merge_0m_simple_df[i,],longlat = TRUE)
      minDistVec[i] <- min(distVec)
      closestSiteVec[i] <- which.min(distVec)
   }
   #
   # Create the output file: merge the eagle point list with the transect point list
   # into a five-column table by merging the eagle point and transect point lists.
   #
   PointAssignTemps <- as(eagle_data_simple_st_new_long[closestSiteVec,]$eagle,"character")
   FinalTable_eagle = data.frame(coordinates(df_soil_merge_0m_simple_df),df_soil_merge_0m_simple_df$site,closestSiteVec,minDistVec,PointAssignTemps)
   #
   # Update the FinalTable column names 
   #
   names(FinalTable_eagle) <- c("Long","Lat","site","CloseTempIndex","Distance","eagle")
   #
   # And, at the end, write the point assignment file.
   #
   message("Write eagle/site assignment table to disk in .csv format")
   write.csv(FinalTable_eagle,file="C:Biodiversity idea//Output files//Distance_btwn_points_eagle_transects.csv", row.names = FALSE)
}

AssignClosestTempPoints_eagle()


# Any arch vs. transects --------------------------------------------------

arch_data<-read.csv("C:Biodiversity idea//Output files//arch_sites_selected.csv")
head(arch_data)
any_arch_data<-arch_data[arch_data$any_arch=="yes",] 
head(any_arch_data)
any_arch_data_simple_new<-any_arch_data[ , c("site_id", "easting", "northing")]
data_any_arch_subset2_new <- any_arch_data_simple_new[ , c("easting", "northing")]
any_arch_data_simple_new<- any_arch_data_simple_new[complete.cases(data_any_arch_subset2_new), ] 
any_arch_data_simple_new$site_id<-as.factor(any_arch_data_simple_new$site_id)
any_arch_data_simple_sf_new <- st_as_sf(any_arch_data_simple_new, coords = c("easting", "northing"), crs = 26909)
any_arch_data_simple_st_new_long<-st_transform(x = any_arch_data_simple_sf_new, crs = 4326)
any_arch_data_simple_st_new_long$long<-st_coordinates(any_arch_data_simple_st_new_long)[,1]
any_arch_data_simple_st_new_long$lat<-st_coordinates(any_arch_data_simple_st_new_long)[,2]
any_arch_data_simple_st_new_long<-any_arch_data_simple_st_new_long %>% st_set_geometry(NULL)
names(any_arch_data_simple_st_new_long)[1]<-"any_arch"
head(any_arch_data_simple_st_new_long)


soil_merge_0m<- read.csv("C:Food web idea\\Data by person\\Norah.data\\soil_merge_0m.csv")
head(soil_merge_0m)
soil_merge_0m_simple<-soil_merge_0m %>% dplyr::select("unq_tran", "easting", "northing")
soil_merge_0m_simple<-soil_merge_0m_simple[!duplicated(soil_merge_0m_simple$unq_tran),]
head(soil_merge_0m_simple)

data_subset2_soil <- soil_merge_0m_simple[ , c("easting", "northing")]
soil_merge_0m_simple_no_na<- soil_merge_0m_simple[complete.cases(data_subset2_soil), ]
df_soil_merge_0m_simple <- st_as_sf(soil_merge_0m_simple_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
names(df_soil_merge_0m_simple)[1]<-"site"
head(df_soil_merge_0m_simple)
df_soil_merge_0m_simple$long<-st_coordinates(df_soil_merge_0m_simple)[,1] # get coordinates
df_soil_merge_0m_simple$lat<-st_coordinates(df_soil_merge_0m_simple)[,2]
df_soil_merge_0m_simple_df<-df_soil_merge_0m_simple %>% st_set_geometry(NULL)
head(df_soil_merge_0m_simple_df)

#Need:
head(df_soil_merge_0m_simple_df)
View(any_arch_data_simple_st_new_long)




AssignClosestTempPoints9 <- function()
{
   
   # Latest version: Assign closest points from a second point set
   
   require(sp)
   
   
   # promote the input lists to SpatialPointsDataFrames
   
   coordinates(any_arch_data_simple_st_new_long) <- c("long", "lat")
   coordinates(df_soil_merge_0m_simple_df) <- c("long", "lat")             
   
   
   #  Define these vectors, used in the loop.
   
   closestSiteVec <- vector(mode = "numeric",length = nrow(df_soil_merge_0m_simple_df))
   minDistVec     <- vector(mode = "numeric",length = nrow(df_soil_merge_0m_simple_df))
   
   # Get the vector index of the any_arch station closest to each field station.
   # Use the spDistsN1 function to compute the distance vector between each
   # field station site and all of the any_arch stations. Then, find and
   # retain the actual any_arch, and the index of the closest any_arch
   # to each transect station.
   #
   # spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
   #
   # where:
   #         pointList   : List of candidate points.
   #         pointToMatch: Single point for which we seek the closest point in pointList.
   #         longlat     : TRUE  computes Great Circle distance in km,
   #                       FALSE computes Euclidean distance in units of input geographic coordinates
   #
   # We use Great Circle distance to increase distance calculation accuracy at high latitudes
   # See the discussion of distance units in the header portion of this file
   #
   # minDistVec stores the distance from to the closest any_arch station to each site measurement point.
   # closestSiteVec stores the index of the closest any_arch station to each site measurement point.
   #
   for (i in 1 : nrow(df_soil_merge_0m_simple_df))
   {
      distVec <- spDistsN1(any_arch_data_simple_st_new_long,df_soil_merge_0m_simple_df[i,],longlat = TRUE)
      minDistVec[i] <- min(distVec)
      closestSiteVec[i] <- which.min(distVec)
   }
   #
   # Create the output file: merge the any_arch point list with the transect point list
   # into a five-column table by merging the any_arch point and transect point lists.
   #
   PointAssignTemps <- as(any_arch_data_simple_st_new_long[closestSiteVec,]$any_arch,"character")
   FinalTable9 = data.frame(coordinates(df_soil_merge_0m_simple_df),df_soil_merge_0m_simple_df$site,closestSiteVec,minDistVec,PointAssignTemps)
   #
   # Update the FinalTable column names 
   #
   names(FinalTable9) <- c("Long","Lat","site","CloseTempIndex","Distance","any_arch")
   #
   # And, at the end, write the point assignment file.
   #
   message("Write any_archne/site assignment table to disk in .csv format")
   write.csv(FinalTable9,file="C:Biodiversity idea//Output files//Distance_btwn_points_any_arch_transects.csv", row.names = FALSE)
}

AssignClosestTempPoints9()


# Fish_traps and transects ------------------------------------------------

arch_data<-read.csv("C:Biodiversity idea//Output files//arch_sites_selected.csv")
head(arch_data)
fish_data<-arch_data[arch_data$fish_feature=="yes",] 
head(fish_data)
fish_data_simple_new<-fish_data[ , c("site_id", "easting", "northing")]
data_fish_subset2_new <- fish_data_simple_new[ , c("easting", "northing")]
fish_data_simple_new<- fish_data_simple_new[complete.cases(data_fish_subset2_new), ] 
fish_data_simple_new$site_id<-as.factor(fish_data_simple_new$site_id)
fish_data_simple_sf_new <- st_as_sf(fish_data_simple_new, coords = c("easting", "northing"), crs = 26909)
fish_data_simple_st_new_long<-st_transform(x = fish_data_simple_sf_new, crs = 4326)
fish_data_simple_st_new_long$long<-st_coordinates(fish_data_simple_st_new_long)[,1]
fish_data_simple_st_new_long$lat<-st_coordinates(fish_data_simple_st_new_long)[,2]
fish_data_simple_st_new_long<-fish_data_simple_st_new_long %>% st_set_geometry(NULL)
names(fish_data_simple_st_new_long)[1]<-"fish"
head(fish_data_simple_st_new_long)


soil_merge_0m<- read.csv("C:Food web idea\\Data by person\\Norah.data\\soil_merge_0m.csv")
head(soil_merge_0m)
soil_merge_0m_simple<-soil_merge_0m %>% dplyr::select("unq_tran", "easting", "northing")
soil_merge_0m_simple<-soil_merge_0m_simple[!duplicated(soil_merge_0m_simple$unq_tran),]
head(soil_merge_0m_simple)

data_subset2_soil <- soil_merge_0m_simple[ , c("easting", "northing")]
soil_merge_0m_simple_no_na<- soil_merge_0m_simple[complete.cases(data_subset2_soil), ]
df_soil_merge_0m_simple <- st_as_sf(soil_merge_0m_simple_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
names(df_soil_merge_0m_simple)[1]<-"site"
head(df_soil_merge_0m_simple)
df_soil_merge_0m_simple$long<-st_coordinates(df_soil_merge_0m_simple)[,1] # get coordinates
df_soil_merge_0m_simple$lat<-st_coordinates(df_soil_merge_0m_simple)[,2]
df_soil_merge_0m_simple_df<-df_soil_merge_0m_simple %>% st_set_geometry(NULL)
head(df_soil_merge_0m_simple_df)

#Need:
head(df_soil_merge_0m_simple_df)
View(fish_data_simple_st_new_long)




AssignClosestTempPoints10 <- function()
{
   
   # Latest version: Assign closest points from a second point set
   
   require(sp)
   
   
   # promote the input lists to SpatialPointsDataFrames
   
   coordinates(fish_data_simple_st_new_long) <- c("long", "lat")
   coordinates(df_soil_merge_0m_simple_df) <- c("long", "lat")             
   
   
   #  Define these vectors, used in the loop.
   
   closestSiteVec <- vector(mode = "numeric",length = nrow(df_soil_merge_0m_simple_df))
   minDistVec     <- vector(mode = "numeric",length = nrow(df_soil_merge_0m_simple_df))
   
   # Get the vector index of the fish station closest to each field station.
   # Use the spDistsN1 function to compute the distance vector between each
   # field station site and all of the fish stations. Then, find and
   # retain the actual fish, and the index of the closest fish
   # to each transect station.
   #
   # spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
   #
   # where:
   #         pointList   : List of candidate points.
   #         pointToMatch: Single point for which we seek the closest point in pointList.
   #         longlat     : TRUE  computes Great Circle distance in km,
   #                       FALSE computes Euclidean distance in units of input geographic coordinates
   #
   # We use Great Circle distance to increase distance calculation accuracy at high latitudes
   # See the discussion of distance units in the header portion of this file
   #
   # minDistVec stores the distance from to the closest fish station to each site measurement point.
   # closestSiteVec stores the index of the closest fish station to each site measurement point.
   #
   for (i in 1 : nrow(df_soil_merge_0m_simple_df))
   {
      distVec <- spDistsN1(fish_data_simple_st_new_long,df_soil_merge_0m_simple_df[i,],longlat = TRUE)
      minDistVec[i] <- min(distVec)
      closestSiteVec[i] <- which.min(distVec)
   }
   #
   # Create the output file: merge the fish point list with the transect point list
   # into a five-column table by merging the fish point and transect point lists.
   #
   PointAssignTemps <- as(fish_data_simple_st_new_long[closestSiteVec,]$fish,"character")
   FinalTable10   = data.frame(coordinates(df_soil_merge_0m_simple_df),df_soil_merge_0m_simple_df$site,closestSiteVec,minDistVec,PointAssignTemps)
   #
   # Update the FinalTable column names 
   #
   names(FinalTable10) <- c("Long","Lat","site","CloseTempIndex","Distance","fish")
   #
   # And, at the end, write the point assignment file.
   #
   message("Write fishne/site assignment table to disk in .csv format")
   write.csv(FinalTable10,file="C:Biodiversity idea//Output files//Distance_btwn_points_fish_transects.csv", row.names = FALSE)
}

AssignClosestTempPoints10()

