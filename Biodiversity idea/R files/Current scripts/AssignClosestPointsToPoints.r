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


df.SF_transects_simple$long<-st_coordinates(df.SF_transects_simple)[,1] # get coordinates
df.SF_transects_simple$lat<-st_coordinates(df.SF_transects_simple)[,2]
df.SF_transects_simple_df<-df.SF_transects_simple %>% st_set_geometry(NULL)
head(df.SF_transects_simple_df)
df.SF_transects_simple_df<-df.SF_transects_simple_df[,-2]


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

