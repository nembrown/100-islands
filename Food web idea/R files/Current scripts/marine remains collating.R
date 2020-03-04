library(vegan)
library(rlang)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(doBy)
library(plyr)
library(dplyr)
library(doBy)
library(cowplot)
library(viridis)
library(matrixStats)
library(tidyverse)

marine_by_plot_from_plants<- read.csv("C:Food web idea//Data by person//Norah.data/marine_by_plot_from_plants.csv")
head(marine_by_plot_from_plants)
head(marine_by_plot_from_plants)



marine_by_plot_from_notes<- read.csv("C:Food web idea//Data by person//Owen's data//100Islands_Fitzpatrick_plot.csv", header=TRUE, sep=",")
head(marine_by_plot_from_notes)
# head(marine_by_plot_from_notes)
marine_by_plot_from_notes$otter_pres <- ifelse(grepl("otter|ottre|latrine|nearotter", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$otter_pres[marine_by_plot_from_notes$unq_plot=="CV09WE5"]<-0
marine_by_plot_from_notes$otter_pres[marine_by_plot_from_notes$unq_plot=="CV09WE6"]<-0
#Owen wrote - "not an otter spot" ... so these woudl have been counted


marine_by_plot_from_notes$eagle_pres <- ifelse(grepl("eagle", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$raven_pres <- ifelse(grepl("raven", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$unk_bird_pres <- ifelse(grepl("guano|feather|bird poo|bird", marine_by_plot_from_notes$notes), 1, 0)

marine_by_plot_from_notes$marine_invert_pres <- ifelse(grepl("bivalve|shell|crab|abalone|ablone|limpet|snail|clam|scallop|mussel|mussell|urchin|claw|carapace|tube worm", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$marine_invert_pres[marine_by_plot_from_notes$unq_plot=="ST07NS5"]<-0
marine_by_plot_from_notes$marine_invert_pres[marine_by_plot_from_notes$unq_plot=="TQ15W1"]<-0
marine_by_plot_from_notes$marine_invert_pres[marine_by_plot_from_notes$unq_plot=="MM06N1"]<-0

marine_by_plot_from_notes$marine_invert_pres[marine_by_plot_from_notes$unq_plot=="PR04E1"]<-0
marine_by_plot_from_notes$marine_invert_pres[marine_by_plot_from_notes$unq_plot=="PR05NS3"]<-1

#these are where it said shell cove - so not a shell actually, the 1 is from a barnacle in plot but couldn't use that as search term becuase Owen would say "barnacle line" a lot to say where the transect started. 

marine_by_plot_from_notes$driftwood <- ifelse(grepl("driftwood", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$midden <- ifelse(grepl("shelly|midden", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$seaweed <- ifelse(grepl("seaweed|algae", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$mink <- ifelse(grepl("mink", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$fish <- ifelse(grepl("fish|rockfish|marine vertebrae", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$mammal_bones <- ifelse(grepl("mammal bones|mammale bones", marine_by_plot_from_notes$notes), 1, 0)

head(marine_by_plot_from_notes)

marine_by_plot_from_notes_selected<-marine_by_plot_from_notes %>% dplyr::select(unq_plot, easting, northing, otter_pres, eagle_pres, raven_pres, unk_bird_pres, marine_invert_pres, driftwood, midden, seaweed, mink, fish, mammal_bones )
head(marine_by_plot_from_notes_selected)

marine_by_plot_from_notes_selected$vector_evidence<-marine_by_plot_from_notes_selected$otter_pres + marine_by_plot_from_notes_selected$eagle_pres + marine_by_plot_from_notes_selected$unk_bird_pres+ marine_by_plot_from_notes_selected$mink + marine_by_plot_from_notes_selected$raven_pres
marine_by_plot_from_notes_selected$marine_evidence<-marine_by_plot_from_notes_selected$seaweed+ marine_by_plot_from_notes_selected$marine_invert_pres+marine_by_plot_from_notes_selected$fish
marine_by_plot_from_notes_selected$total_marine_evidence<-marine_by_plot_from_notes_selected$otter_pres + marine_by_plot_from_notes_selected$eagle_pres + marine_by_plot_from_notes_selected$unk_bird_pres+ marine_by_plot_from_notes_selected$mink + marine_by_plot_from_notes_selected$raven_pres + marine_by_plot_from_notes_selected$seaweed+ marine_by_plot_from_notes_selected$marine_invert_pres +marine_by_plot_from_notes_selected$fish+marine_by_plot_from_notes_selected$midden

marine_by_plot_from_notes_selected[duplicated(marine_by_plot_from_notes_selected$unq_plot),]


head(marine_by_plot_from_notes_selected)

owen_key<-read.csv("C:Food web idea//Data by person//Owen's data//key_mod_2019.csv", header=TRUE, sep=",")
length(unique(owen_key$unq_tran))
owen_key<-owen_key %>% dplyr::select(-unq_tran)
owen_key$unq_tran<-str_sub(owen_key$unq_plot, end=-2)

owen_key<- owen_key %>% mutate(unq_tran= if_else(plot<4, gsub("SN", "S", unq_tran, fixed = TRUE), gsub("SN", "N", unq_tran, fixed = TRUE))) %>% 
  mutate(unq_tran= if_else(plot<4, gsub("NS", "N", unq_tran, fixed = TRUE), gsub("NS", "S", unq_tran, fixed = TRUE))) %>% 
  mutate(unq_tran= if_else(plot<4, gsub("EW", "E", unq_tran, fixed = TRUE), gsub("EW", "W", unq_tran, fixed = TRUE))) %>% 
  mutate(unq_tran= if_else(plot<4, gsub("WE", "W", unq_tran, fixed = TRUE), gsub("WE", "E", unq_tran, fixed = TRUE))) 

#these ones are double digits - also they are ones are that 25m and 15m (so would actually be less than plot 3)
owen_key$unq_tran[owen_key$unq_plot=="CV04SN25"]<-"CV04S"
owen_key$unq_tran[owen_key$unq_plot=="MM04WE25"]<-"MM04W"
owen_key$unq_tran[owen_key$unq_plot=="MM08NS25"]<-"MM08N"
owen_key$unq_tran[owen_key$unq_plot=="PR05EW25"]<-"PR05E"
owen_key$unq_tran[owen_key$unq_plot=="PR06EW25"]<-"PR06E"
owen_key$unq_tran[owen_key$unq_plot=="TQ02NS25"]<-"TQ02N"
owen_key$unq_tran[owen_key$unq_plot=="TQ05EW25"]<-"TQ05E"
owen_key$unq_tran[owen_key$unq_plot=="MM01WE15"]<-"MM01W"
owen_key$unq_tran[owen_key$unq_plot=="MM03WE15"]<-"MM03W"
owen_key$unq_tran[owen_key$unq_plot=="MM08EW15"]<-"MM08E"
owen_key$unq_tran[owen_key$unq_plot=="TQ06EW15"]<-"TQ06E"

#this transect was only 3 plots long, two exterior and one interior.... so plot #3 is East
owen_key$unq_tran[owen_key$unq_plot=="AD03WE3"]<-"AD03E"
owen_key$unq_tran[owen_key$unq_plot=="CV14SN3"]<-"CV14N"
owen_key$unq_tran[owen_key$unq_plot=="CV14EW3"]<-"CV14W"
owen_key$unq_tran[owen_key$unq_plot=="MM07NS3"]<-"MM07S"
owen_key$unq_tran[owen_key$unq_plot=="ST09WE3"]<-"ST09E"

head(owen_key)
owen_key_subset<-owen_key %>% dplyr::select(unq_plot, unq_tran)


marine_by_plot_from_notes_selected<-merge(marine_by_plot_from_notes_selected, owen_key_subset, by="unq_plot")



###Making sure that plots close to eachother get considered properly... see TB04SW for an eg of this problem... within 4m of an otter site but doesn't pick it up
library(sf)
library(raster)
library(spData)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(maphead) # for interactive maps
library(ggplot2) # tidyverse vis package
library(shiny)
library(rgdal) # spatial/shp reading
library(viridis) # nice color palette
library(ggmap) # ggplot functionality for maps ---> dplyr, purr is dependency
library(ggsn) # for scale bars/north arrows in ggplots
library(maps)
library(mapdata)
library(here)



by_plot_master<-marine_by_plot_from_notes_selected %>% dplyr::select(unq_plot, unq_tran, easting, northing, total_marine_evidence)
head(by_plot_master)

by_plot_master_marine<-by_plot_master %>% filter(total_marine_evidence >0)
by_plot_master_not_marine<-by_plot_master %>% filter(total_marine_evidence == 0)

data_subset3_marine <- by_plot_master_marine[ , c("easting", "northing")]
by_plot_master_marine_no_na<- by_plot_master_marine[complete.cases(data_subset3_marine), ]
df.SF_plot_marine <- st_as_sf(by_plot_master_marine_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
df.SF_plot_simple_marine<-df.SF_plot_marine[,1:2]
df.SF_plot_simple_marine_new<- df.SF_plot_simple_marine %>% st_transform(3035) 
head(df.SF_plot_simple_marine_new)

data_subset3_not_marine <- by_plot_master_not_marine[ , c("easting", "northing")]
by_plot_master_not_marine_no_na<- by_plot_master_not_marine[complete.cases(data_subset3_not_marine), ]
df.SF_plot_not_marine <- st_as_sf(by_plot_master_not_marine_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
df.SF_plot_simple_not_marine<-df.SF_plot_not_marine[,1:2]
df.SF_plot_simple_not_marine_new<- df.SF_plot_simple_not_marine %>% st_transform(3035) 
head(df.SF_plot_simple_not_marine_new)
colnames(df.SF_plot_simple_not_marine_new)[1]<-"unq_plot_not_marine"
colnames(df.SF_plot_simple_not_marine_new)[2]<-"unq_tran_not_marine"

#https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a

# Buffer circles by 10m -- creates polygons around the 
marine_circles <- st_buffer(df.SF_plot_simple_marine_new, dist = 10)

#which of the not-marine plots fall within 10m radius of the marine plots
plots_marine_joined <- st_join(df.SF_plot_simple_not_marine_new, marine_circles, left=FALSE)

head(plots_marine_joined)
### Alright so this is a big list because there are plots every 10 m along the same trasnsect so I only really need to do this for when the transects dont 
#match up
#the transects will get captured when we collect by transect later.... 

#fuzzy matching
# install.packages("stringdist")
library(stringdist)
plots_marine_joined$stringdist<-stringdist(plots_marine_joined$unq_tran_not_marine, plots_marine_joined$unq_tran)


head(plots_marine_joined)

plots_marine_joined_should_marine <- plots_marine_joined %>% filter(stringdist>0) 
head(plots_marine_joined_should_marine)
#There are 106 plots that should get a "marine" indicator since they are within 10m of another plot (but on another TRANSECT) that has a marine indicator

plots_marine_joined_should_marine <-plots_marine_joined_should_marine %>% st_set_geometry(NULL)
plots_marine_joined_should_marine<-as.data.frame(plots_marine_joined_should_marine)
plots_should_marine<-plots_marine_joined_should_marine %>% dplyr::select(unq_plot_not_marine) %>% droplevels()
head(plots_should_marine)

levels(plots_should_marine$unq_plot_not_marine)

head(marine_by_plot_from_notes_selected)
marine_by_plot_from_notes_selected$total_marine_evidence[(levels(marine_by_plot_from_notes_selected$unq_plot) %in% levels(plots_should_marine$unq_plot_not_marine))==TRUE]<-1

head(marine_by_plot_from_notes_selected)

write.csv(marine_by_plot_from_notes_selected, "C:Biodiversity idea//Output files//marine_by_plot_from_notes_selected.csv", row.names=FALSE)





#### by transect


marine_by_transect_from_notes_selected_sum<- marine_by_plot_from_notes_selected %>% group_by(unq_tran) %>%   
                                        summarise_if(is.numeric, sum, na.rm=TRUE)

head(marine_by_transect_from_notes_selected_sum)
marine_by_transect_pres_abs<-ifelse(marine_by_transect_from_notes_selected_sum[,-1] > 0, 1, 0)
head(marine_by_transect_pres_abs)

marine_by_transect_from_notes_selected<-cbind(marine_by_transect_from_notes_selected_sum[,1], marine_by_transect_pres_abs)
head(marine_by_transect_from_notes_selected)



write.csv(marine_by_transect_from_notes_selected, "C:Biodiversity idea//Output files//marine_by_transect_from_notes_selected.csv", row.names=FALSE)





master_transect<-read.csv("C:Biodiversity idea//Output files//master_transect.csv")

ggplot(master_transect, aes(x=as.factor(total_marine_evidence), y=d15n))+geom_boxplot()





#### SUM
head(master_transect2)
master_transect3<-merge(master_transect2, marine_by_transect_from_notes_selected_sum[,-c(2,3)])

ggplot(master_transect3, aes(x=otter_pres, y=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(master_transect3, aes(x=marine_invert_pres, y=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(master_transect3, aes(x=total_marine_evidence, y=d15n))+geom_point()+geom_smooth(method="lm")+geom_text(label=master_transect3$unq_tran)
ggplot(master_transect3, aes(x=vector_evidence, y=d15n))+geom_point()+geom_smooth(method="lm")+geom_text(label=master_transect3$unq_tran)
ggplot(master_transect3, aes(x=marine_evidence, y=d15n))+geom_point()+geom_smooth(method="lm")+geom_text(label=master_transect3$unq_tran)
  
### try by plot?? 
#would need to merge with plot-level d15n
soil_merge<-read.csv("C:Food web idea\\Data by person\\Norah.data\\soil_merge.csv")
marine_by_plot_from_notes_selected<-read.csv("C:Biodiversity idea//Output files//marine_by_plot_from_notes_selected.csv")

master_plot<-merge(soil_merge, marine_by_plot_from_notes_selected)
head(master_plot)

ggplot(master_plot, aes(x=otter_pres, y=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(master_plot, aes(x=marine_invert_pres, y=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(master_plot, aes(x=total_marine_evidence, y=d15n, label=master_plot$unq_plot))+geom_point()+geom_smooth(method="lm")+geom_text(label=master_plot$unq_plot)
ggplot(master_plot, aes(x=vector_evidence, y=d15n))+geom_point()+geom_smooth(method="lm")+geom_text(label=master_plot$unq_plot)
ggplot(master_plot, aes(x=marine_evidence, y=d15n))+geom_point()+geom_smooth(method="lm")+geom_text(label=master_plot$unq_plot)


head(master_plot)
master_tran_trial<-master_plot %>% group_by(unq_tran) %>% summarise_if(is.numeric, mean)


ggplot(master_tran_trial, aes(x=otter_pres, y=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(master_tran_trial, aes(x=marine_invert_pres, y=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(master_tran_trial, aes(x=total_marine_evidence, y=d15n))+geom_point()+geom_smooth(method="lm")+geom_text(label=master_tran_trial$unq_tran)
ggplot(master_tran_trial, aes(x=vector_evidence, y=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(master_tran_trial, aes(x=marine_evidence, y=d15n))+geom_point()+geom_smooth(method="lm")


