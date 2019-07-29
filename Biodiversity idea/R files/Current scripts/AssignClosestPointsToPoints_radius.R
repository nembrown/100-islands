setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Biodiversity idea")

#get Ben's data in right format
ben_habitat_data<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_habitat_20142018.csv")
head(ben_habitat_data)
ben_habitat_data_simple<-ben_habitat_data[,c(1:3)]

ben_habitat_data_simple.SP_new <- st_as_sf(ben_habitat_data_simple, coords = c("long", "lat"), crs = 4326)%>% 
  st_transform(3035)
head(ben_habitat_data_simple.SP_new)

#Transect-level information
setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Food web idea")
by_tran_master<-read.csv("C:Data by person//Norah.data//by_tran_master.csv")
by_tran_master<-by_tran_master[,-1]
data_subset2 <- by_tran_master[ , c("easting", "northing")]
by_tran_master_no_na<- by_tran_master[complete.cases(data_subset2), ] 
df.SF_transects <- st_as_sf(by_tran_master_no_na, coords = c("easting", "northing"), crs = 26909)
df.SF_transects<-st_transform(x = df.SF_transects, crs = 4326)
df.SF_transects$long<-st_coordinates(df.SF_transects)[,1] # get coordinates
df.SF_transects$lat<-st_coordinates(df.SF_transects)[,2] # get coordinates
head(df.SF_transects)
df.SF_transects_simple<-unique(df.SF_transects[,c(1)])

df.SF_transects_simple_new<- df.SF_transects_simple %>% st_transform(3035) 




# Buffer circles by 500m -- creates polygons around the 
dat_circles <- st_buffer(df.SF_transects_simple_new, dist = 1000)

#which of the beachseines fall within 500m radius of the transect
transects_beach_joined = st_join(ben_habitat_data_simple.SP_new, dat_circles, left=FALSE)

head(transects_beach_joined)

transects_beach_joined <-transects_beach_joined  %>% st_set_geometry(NULL)
transects_beach_joined<-as.data.frame(transects_beach_joined)
write.csv(transects_beach_joined, "C:Output files//paired_sites_by_radius.csv")
