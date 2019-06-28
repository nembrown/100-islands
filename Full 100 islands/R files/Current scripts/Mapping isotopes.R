setwd("C:/Users/norahbrown/Dropbox/Projects/100 islands/Modelling practice")


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
library(ggmap) # ggplot functionality for maps
library(ggsn) # for scale bars/north arrows in ggplots
library(maps)
library(mapdata)

##### Plotting map
isotope_master<-read.csv("C:Owen's data//isotope_master.csv")

head(isotope_master)
by_isl_master_no_na<-na.omit(isotope_master)
sites.SP  <- SpatialPointsDataFrame(by_isl_master_no_na[,c(11,12)],by_isl_master_no_na[,-c(11,12)]) 
head(sites.SP)

utms<-CRS("+proj=utm +zone=9 +datum=WGS84") # manual
utms # simplified but will work
## CRS arguments:
##  +proj=utm +zone=10 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0
utms <- CRS("+init=epsg:26909") ## more detailed def using the EPSG code
utms # more detail
## CRS arguments:
##  +init=epsg:32610 +proj=utm +zone=10 +datum=WGS84 +units=m +no_defs
## +ellps=WGS84 +towgs84=0,0,0
proj4string(sites.SP)<-utms # add to the dataset

# now lets switch over data to long/lat and reproject
lats84<-CRS("+init=epsg:4326") # set the default for lat/longs
sites.SP<-spTransform(sites.SP, lats84)
head(sites.SP)

proj4string(sites.SP) # double check data to make sure it has CRS

head(sites.SP)


mymap<-map("worldHires",
    "Canada",
    xlim=c(-128.5,-127.5),
    ylim=c(51.2,52.2),
    col="gray90",
    fill=TRUE)


map.axes()

# add our point data from earlier
points(sites.SP, cex=1.4, pch=21)


#using sp ... not sf format ... 

isotopes.sf <- st_as_sf(sites.SP) 
head(isotopes.sf)
###Or directly:: 

head(by_isl_master)
head(isotope_by_isl_gathered4)
which( colnames(by_isl_master)=="node" )

by_isl_master<-by_isl_master[,-2]

isotope_master_sf<-merge(isotope_by_isl_gathered4[,-c(6, 7)], by_isl_master[,c(1,11,12,31:89)], by="unq_isl", all=TRUE)
head(isotope_master_sf)
isotope_master_sf_no_na<-na.omit(isotope_master_sf)


df.SP <- st_as_sf(isotope_master_sf_no_na, coords = c("C_Easting", "C_Northing"), crs = 26909)
head(df.SP)

df.SP<-st_transform(x = df.SP, crs = 4326)
df.SP$lon<-st_coordinates(df.SP)[,1] # get coordinates
df.SP$lat<-st_coordinates(df.SP)[,2] # get coordinates
head(df.SP)

######
bbox <- make_bbox(df.SP$lon, df.SP$lat, f = 0.01)
map_toner_lite <- get_stamenmap(bbox, source="stamen", maptype= "toner-lite", crop=FALSE)
map <- get_stamenmap(bbox, source="stamen", maptype= "terrain", crop=FALSE)

map_google <- get_map(bbox, source="google", maptype= "terrain", crop=FALSE)

bbox_north <- make_bbox(c(-128.45, -128.25), c(52.0, 52.1))
map_north <- get_stamenmap(bbox_north, source="stamen", maptype= "toner-lite", crop=TRUE, zoom=13)

#map <- get_openstreetmap(bbox, source="osm", crop=FALSE, color="bw")

df.SP_ST<-df.SP %>% filter(node=="ST")
bbox_ST <- make_bbox(df.SP_ST$lon, df.SP_ST$lat, f = 0.1)
map_ST <- get_stamenmap(bbox_ST, source="stamen", maptype= "toner-lite", crop=TRUE, zoom=13)

df.SP_CV<-df.SP %>% filter(node=="CV")
bbox_CV <- make_bbox(df.SP_CV$lon, df.SP_CV$lat, f = 0.1)
map_CV <- get_stamenmap(bbox_CV, source="stamen", maptype= "toner-lite", crop=TRUE, zoom=13)

df.SP_GS<-df.SP %>% filter(node=="GS")
bbox_GS <- make_bbox(df.SP_GS$lon, df.SP_GS$lat, f = 0.1)
map_GS <- get_stamenmap(bbox_GS, source="stamen", maptype= "toner-lite", crop=TRUE, zoom=13)

df.SP_AD<-df.SP %>% filter(node=="AD")
bbox_AD <- make_bbox(df.SP_AD$lon, df.SP_AD$lat, f = 0.1)
map_AD <- get_stamenmap(bbox_AD, source="stamen", maptype= "toner-lite", crop=TRUE, zoom=13)

df.SP_MM<-df.SP %>% filter(node=="MM")
bbox_MM <- make_bbox(df.SP_MM$lon, df.SP_MM$lat, f = 0.1)
map_MM <- get_stamenmap(bbox_MM, source="stamen", maptype= "toner-lite", crop=TRUE, zoom=13)


df.SP_TQ<-df.SP %>% filter(node=="TQ")
bbox_TQ <- make_bbox(df.SP_TQ$lon, df.SP_TQ$lat, f = 0.1)
map_TQ <- get_stamenmap(bbox_TQ, source="stamen", maptype= "toner-lite", crop=TRUE, zoom=13)



?bbox

ggmap(map) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()


ggmap(map) + geom_point(data=filter(df.SP, group %in% c("bird_feces")), aes(x = lon, y = lat, size=d13c,col=d15n))+  scale_colour_viridis()
ggmap(map) + geom_point(data=filter(df.SP, group %in% c("bird_feces")), aes(x = lon, y = lat, size=log_Area,col=d15n))+  scale_colour_viridis()


ggmap(map) + geom_point(data=filter(df.SP, group %in% c("Mouse hair")), aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()


ggmap(map) + geom_point(data=df.SP, aes(x = lon, y = lat, col=bird.richness))+  scale_colour_viridis()

ggmap(map) + geom_point(data=df.SP, aes(x = lon, y = lat, size=bird.richness, col=d15n))+  scale_colour_viridis()

ggmap(map_ST) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()
ggmap(map_CV) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()
ggmap(map_GS) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()
ggmap(map_AD) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()
ggmap(map_TQ) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()
ggmap(map_MM) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()


ggmap(map_TQ) + geom_point(data=df.SP, aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()


ggmap(map) + geom_point(data=df.SP, aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()
ggmap(map_toner_lite) + geom_point(data=df.SP, aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()
ggmap(map_google) + geom_point(data=df.SP, aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()


ggmap(map_toner_lite) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n, size=plant_richness))+  scale_colour_viridis()
ggmap(map_toner_lite) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n, size=tree_richness))+  scale_colour_viridis()
ggmap(map_toner_lite) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n, size=insect_richness))+  scale_colour_viridis()
ggmap(map_toner_lite) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n, size=bird.richness))+  scale_colour_viridis()
ggmap(map_toner_lite) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n, size=mammal_richness))+  scale_colour_viridis()

ggmap(map_toner_lite) + geom_point(data=df.SP, aes(x = lon, y = lat, col=d15n, size=bird.density))+  scale_colour_viridis()


ggmap(map_toner_lite) + geom_point(data=df.SP, aes(x = lon, y = lat, col=d15n, size=log_site_sum_by_isl))+  scale_colour_viridis()


ggmap(map_toner_lite) + geom_point(data=df.SP, aes(x = lon, y = lat, col=gash))+  scale_colour_viridis()
ggmap(map_toner_lite) + geom_point(data=df.SP, aes(x = lon, y = lat, col=d15n, size=eagles))+  scale_colour_viridis()

ggmap(map_toner_lite) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=n))+  scale_colour_viridis()
ggmap(map_toner_lite) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n))+  scale_colour_viridis()


ggmap(map_north) + geom_point(data=filter(df.SP, group %in% c("soil_whole_island")), aes(x = lon, y = lat, col=d15n, size=plant_richness))+  scale_colour_viridis()

#theme_map <- function(...) {
theme_minimal() +
  theme(
    text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    ...
  )
}
