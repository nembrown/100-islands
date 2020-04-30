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

# keep this commented out - this is data from the plot level that got into percent cover, 
#but the data from _from_notes is a lot more detailed and covers the same plots so 
#we shoudl use that + Chris's transect level information
#data from Owen's plots, marine remains that got into the percent cover of plots
# marine_by_plot_from_plants<- read.csv("C:Food web idea//Data by person//Norah.data/marine_by_plot_from_plants.csv")
# head(marine_by_plot_from_plants)


# Data from Chris Ernst ---------------------------------------------------
#data from Chris Ernst's plots on trapline
marine_by_plot_from_chris<- read.csv("C:Food web idea//Data by person//Chris.data//chris_habitat.csv", header=TRUE, sep=",")
marine_by_plot_from_chris$unq_tran<- paste(marine_by_plot_from_chris$island,marine_by_plot_from_chris$direction)
marine_by_plot_from_chris$unq_tran<-gsub(" ", "", marine_by_plot_from_chris$unq_tran, fixed = TRUE)
marine_by_plot_from_chris$unq_isl<-marine_by_plot_from_chris$island
marine_by_plot_from_chris$unq_plot<-paste(marine_by_plot_from_chris$island,marine_by_plot_from_chris$direction,"q", marine_by_plot_from_chris$quadrat)
marine_by_plot_from_chris$unq_plot<-gsub(" ", "", marine_by_plot_from_chris$unq_plot, fixed = TRUE)

marine_by_plot_from_chris$otter_pres_chris <- ifelse(grepl("otter|ottre|latrine|nearotter", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$marine_invert_pres_chris <- ifelse(grepl("bivalve|shell|crab|abalone|ablone|limpet|geoduck|sea star|whelk|snail|chiton|clam|scallop|mussel|mussell|urchin|claw|carapace|tube worm|barnacle", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$fish_chris <- ifelse(grepl("fish|rockfish|marine vertebrae", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$unk_bird_pres_chris <- ifelse(grepl("guano|feather|bird poo|bird", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$eagle_pres_chris <- ifelse(grepl("eagle", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$raven_pres_chris <- ifelse(grepl("raven", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$driftwood_chris <- ifelse(grepl("driftwood", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$seaweed_chris <- ifelse(grepl("seaweed|algae|fucus|kelp", marine_by_plot_from_chris$marine), 1, 0)

head(marine_by_plot_from_chris)
length(unique(marine_by_plot_from_chris$island))

marine_by_isl_from_chris_selected<-marine_by_plot_from_chris %>% 
  dplyr::select(unq_isl, otter_pres_chris) %>% 
  group_by(unq_isl) %>%   
  summarise_all(list(otter_chris_plot_sum=sum, n_otter_chris_plots=length))

View(marine_by_isl_from_chris_selected)
hist(marine_by_isl_from_chris_selected$n_otter_chris_plots)
# minimum is 19 but 3 from transects NESW and 7 is lowest from interior. 

marine_by_tran_from_chris_selected<-marine_by_plot_from_chris %>% 
  dplyr::select(unq_tran, otter_pres_chris, unk_bird_pres_chris, marine_invert_pres_chris, driftwood_chris, seaweed_chris,  fish_chris) %>% 
  group_by(unq_tran) %>%   
  summarise_if(is.numeric, sum, na.rm=TRUE)



marine_by_tran_from_chris_selected$vector_evidence_chris<-marine_by_tran_from_chris_selected$otter_pres_chris 
marine_by_tran_from_chris_selected$marine_evidence_chris<-marine_by_tran_from_chris_selected$seaweed_chris+ marine_by_tran_from_chris_selected$marine_invert_pres_chris+marine_by_tran_from_chris_selected$fish_chris
marine_by_tran_from_chris_selected$total_marine_evidence_chris<-marine_by_tran_from_chris_selected$otter_pres_chris  + marine_by_tran_from_chris_selected$seaweed_chris + marine_by_tran_from_chris_selected$marine_invert_pres_chris +marine_by_tran_from_chris_selected$fish_chris

head(marine_by_tran_from_chris_selected)


# Data from Owen's notes --------------------------------------------------
#from Owen's plot-level notes

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

#these are where it said shell cove - so not a shell actually, 
#the 1 is from a barnacle in plot but couldn't use that as search term becuase Owen would say "barnacle line" a lot to say where the transect started. 

marine_by_plot_from_notes$driftwood <- ifelse(grepl("driftwood", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$midden <- ifelse(grepl("shelly|midden", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$seaweed <- ifelse(grepl("seaweed|algae", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$mink <- ifelse(grepl("mink", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$fish <- ifelse(grepl("fish|rockfish|marine vertebrae", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$mammal_bones <- ifelse(grepl("mammal bones|mammale bones", marine_by_plot_from_notes$notes), 1, 0)


marine_by_plot_from_notes_selected<-marine_by_plot_from_notes %>% dplyr::select(unq_plot, easting, northing, otter_pres, eagle_pres, raven_pres, unk_bird_pres, marine_invert_pres, driftwood, midden, seaweed, mink, fish, mammal_bones )
head(marine_by_plot_from_notes_selected)

marine_by_plot_from_notes_selected$vector_evidence<-marine_by_plot_from_notes_selected$otter_pres + marine_by_plot_from_notes_selected$eagle_pres + marine_by_plot_from_notes_selected$mink + marine_by_plot_from_notes_selected$raven_pres
marine_by_plot_from_notes_selected$marine_evidence<-marine_by_plot_from_notes_selected$seaweed+ marine_by_plot_from_notes_selected$marine_invert_pres+marine_by_plot_from_notes_selected$fish
marine_by_plot_from_notes_selected$total_marine_evidence<-marine_by_plot_from_notes_selected$otter_pres + marine_by_plot_from_notes_selected$eagle_pres + marine_by_plot_from_notes_selected$mink + marine_by_plot_from_notes_selected$raven_pres + marine_by_plot_from_notes_selected$seaweed+ marine_by_plot_from_notes_selected$marine_invert_pres +marine_by_plot_from_notes_selected$fish+marine_by_plot_from_notes_selected$midden

marine_by_plot_from_notes_selected[duplicated(marine_by_plot_from_notes_selected$unq_plot),]


head(marine_by_plot_from_notes_selected)
key<-marine_by_plot_from_notes_selected
key$unq_tran<-str_sub(key$unq_plot, end=-2)
key$plot<-str_sub(key$unq_plot, start=-1)
key<-key[, c("unq_plot", "unq_tran", "plot")]
head(key)

key<- key %>% mutate(unq_tran= if_else(plot<4, gsub("SN", "S", unq_tran, fixed = TRUE), gsub("SN", "N", unq_tran, fixed = TRUE))) %>% 
  mutate(unq_tran= if_else(plot<4, gsub("NS", "N", unq_tran, fixed = TRUE), gsub("NS", "S", unq_tran, fixed = TRUE))) %>% 
  mutate(unq_tran= if_else(plot<4, gsub("EW", "E", unq_tran, fixed = TRUE), gsub("EW", "W", unq_tran, fixed = TRUE))) %>% 
  mutate(unq_tran= if_else(plot<4, gsub("WE", "W", unq_tran, fixed = TRUE), gsub("WE", "E", unq_tran, fixed = TRUE))) 

#these ones are double digits - also they are ones are that 25m and 15m (so would actually be less than plot 3)
key$unq_tran[key$unq_plot=="CV04SN25"]<-"CV04S"
key$unq_tran[key$unq_plot=="MM04WE25"]<-"MM04W"
key$unq_tran[key$unq_plot=="MM08NS25"]<-"MM08N"
key$unq_tran[key$unq_plot=="PR05EW25"]<-"PR05E"
key$unq_tran[key$unq_plot=="PR06EW25"]<-"PR06E"
key$unq_tran[key$unq_plot=="TQ02NS25"]<-"TQ02N"
key$unq_tran[key$unq_plot=="TQ05EW25"]<-"TQ05E"
key$unq_tran[key$unq_plot=="MM01WE15"]<-"MM01W"
key$unq_tran[key$unq_plot=="MM03WE15"]<-"MM03W"
key$unq_tran[key$unq_plot=="MM08EW15"]<-"MM08E"
key$unq_tran[key$unq_plot=="TQ06EW15"]<-"TQ06E"

#this transect was only 3 plots long, two exterior and one interior.... so plot #3 is East
key$unq_tran[key$unq_plot=="AD03WE3"]<-"AD03E"
key$unq_tran[key$unq_plot=="CV14SN3"]<-"CV14N"
key$unq_tran[key$unq_plot=="CV14EW3"]<-"CV14W"
key$unq_tran[key$unq_plot=="MM07NS3"]<-"MM07S"
key$unq_tran[key$unq_plot=="ST09WE3"]<-"ST09E"

head(key)
key_subset<-key %>% dplyr::select(unq_plot, unq_tran)


marine_by_plot_from_notes_selected<-merge(marine_by_plot_from_notes_selected, key_subset, by="unq_plot", all.x=TRUE)
head(marine_by_plot_from_notes_selected)
marine_by_plot_from_notes_selected$unq_isl<-str_sub(marine_by_plot_from_notes_selected$unq_tran, end=4)
# 
# marine_by_plot_from_notes_selected_isl<-marine_by_plot_from_notes_selected
# 
# marine_by_plot_from_notes_selected_isl$unq_isl<-str_sub(marine_by_plot_from_notes_selected_isl$unq_tran, end=4)
# head(marine_by_plot_from_notes_selected_isl)
# 
# 
# marine_by_plot_from_notes_selected_isl <- marine_by_plot_from_notes_selected_isl %>% dplyr::select(unq_isl, otter_pres) %>% group_by(unq_isl) %>%  summarise_all(list(sum = sum, n=length))
# View(marine_by_plot_from_notes_selected_isl)

# Matching plots within 10m radius ----------------------------------------

#This is only useful for by-transect analysis
# Commenting this section out for now (April 2020), using for by-isl analysis so this part is unnecessary
# 
# ###Making sure that plots close to eachother get considered properly... see TB04SW for an eg of this problem... within 4m of an otter site but doesn't pick it up
# library(sf)
# library(raster)
# library(spData)
# library(tmap)    # for static and interactive maps
# library(leaflet) # for interactive maps
# library(maphead) # for interactive maps
# library(ggplot2) # tidyverse vis package
# library(shiny)
# library(rgdal) # spatial/shp reading
# library(viridis) # nice color palette
# library(ggmap) # ggplot functionality for maps ---> dplyr, purr is dependency
# library(ggsn) # for scale bars/north arrows in ggplots
# library(maps)
# library(mapdata)
# library(here)
# 
# 
# 
# by_plot_master<-marine_by_plot_from_notes_selected %>% dplyr::select(unq_plot, unq_tran, easting, northing, total_marine_evidence)
# head(by_plot_master)
# 
# by_plot_master_marine<-by_plot_master %>% filter(total_marine_evidence >0)
# by_plot_master_not_marine<-by_plot_master %>% filter(total_marine_evidence == 0)
# 
# data_subset3_marine <- by_plot_master_marine[ , c("easting", "northing")]
# by_plot_master_marine_no_na<- by_plot_master_marine[complete.cases(data_subset3_marine), ]
# df.SF_plot_marine <- st_as_sf(by_plot_master_marine_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
# df.SF_plot_simple_marine<-df.SF_plot_marine[,1:2]
# df.SF_plot_simple_marine_new<- df.SF_plot_simple_marine %>% st_transform(3035) 
# head(df.SF_plot_simple_marine_new)
# 
# data_subset3_not_marine <- by_plot_master_not_marine[ , c("easting", "northing")]
# by_plot_master_not_marine_no_na<- by_plot_master_not_marine[complete.cases(data_subset3_not_marine), ]
# df.SF_plot_not_marine <- st_as_sf(by_plot_master_not_marine_no_na, coords = c("easting", "northing"), crs = 26909) %>% st_transform(crs = 4326)
# df.SF_plot_simple_not_marine<-df.SF_plot_not_marine[,1:2]
# df.SF_plot_simple_not_marine_new<- df.SF_plot_simple_not_marine %>% st_transform(3035) 
# head(df.SF_plot_simple_not_marine_new)
# colnames(df.SF_plot_simple_not_marine_new)[1]<-"unq_plot_not_marine"
# colnames(df.SF_plot_simple_not_marine_new)[2]<-"unq_tran_not_marine"
# 
# #https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a
# 
# # Buffer circles by 10m -- creates polygons around the 
# marine_circles <- st_buffer(df.SF_plot_simple_marine_new, dist = 10)
# 
# #which of the not-marine plots fall within 10m radius of the marine plots
# plots_marine_joined <- st_join(df.SF_plot_simple_not_marine_new, marine_circles, left=FALSE)
# 
# head(plots_marine_joined)
# ### Alright so this is a big list because there are plots every 10 m along the same trasnsect so I only really need to do this for when the transects dont 
# #match up
# #the transects will get captured when we collect by transect later.... 
# 
# #fuzzy matching
# # install.packages("stringdist")
# library(stringdist)
# plots_marine_joined$stringdist<-stringdist(plots_marine_joined$unq_tran_not_marine, plots_marine_joined$unq_tran)
# 
# 
# head(plots_marine_joined)
# 
# plots_marine_joined_should_marine <- plots_marine_joined %>% filter(stringdist>0) 
# head(plots_marine_joined_should_marine)
# #There are 106 plots that should get a "marine" indicator since they are within 10m of another plot (but on another TRANSECT) that has a marine indicator
# 
# plots_marine_joined_should_marine <-plots_marine_joined_should_marine %>% st_set_geometry(NULL)
# plots_marine_joined_should_marine<-as.data.frame(plots_marine_joined_should_marine)
# plots_should_marine<-plots_marine_joined_should_marine %>% dplyr::select(unq_plot_not_marine) %>% droplevels()
# head(plots_should_marine)
# 
# levels(plots_should_marine$unq_plot_not_marine)
# 
# head(marine_by_plot_from_notes_selected)
# marine_by_plot_from_notes_selected$total_marine_evidence[(levels(marine_by_plot_from_notes_selected$unq_plot) %in% levels(plots_should_marine$unq_plot_not_marine))==TRUE]<-1
# 
# head(marine_by_plot_from_notes_selected)
# 
# write.csv(marine_by_plot_from_notes_selected, "C:Biodiversity idea//Output files//marine_by_plot_from_notes_selected.csv", row.names=FALSE)




# Summarizing Owen's plot-level data to transect-level --------------------



marine_by_transect_from_notes_selected_sum<- marine_by_plot_from_notes_selected %>% group_by(unq_tran) %>%   
                                        summarise_if(is.numeric, sum, na.rm=TRUE)

head(marine_by_transect_from_notes_selected_sum)





# Merging Chris Ernst's transect level data with Owen's -------------------

head(marine_by_transect_from_notes_selected_sum)
head(marine_by_tran_from_chris_selected)

marine_by_tran_combined<-merge(marine_by_transect_from_notes_selected_sum[,-c(2:3)], marine_by_tran_from_chris_selected, by="unq_tran", all=TRUE)
head(marine_by_tran_combined)

marine_by_tran_combined[is.na(marine_by_tran_combined)] <- 0

marine_by_tran_combined <- marine_by_tran_combined %>% 
                          mutate(otter_pres_all = otter_pres + otter_pres_chris) %>% 
                          mutate(unk_bird_pres_all = unk_bird_pres + unk_bird_pres_chris) %>% 
                          mutate(marine_invert_pres_all = marine_invert_pres + marine_invert_pres_chris) %>% 
                          mutate(driftwood_all = driftwood + driftwood_chris) %>% 
                          mutate(seaweed_all = seaweed + seaweed_chris) %>% 
                          mutate(fish_all = fish + fish_chris) %>% 
                          mutate(vector_evidence_all = vector_evidence + vector_evidence_chris) %>% 
                          mutate(marine_evidence_all = marine_evidence + marine_evidence_chris) %>% 
                          mutate(total_marine_evidence_all = total_marine_evidence + total_marine_evidence_chris)



write.csv(marine_by_tran_combined, "C:Biodiversity idea//Output files//marine_by_tran_combined.csv", row.names=FALSE)


marine_by_tran_combined_pres_abs<-marine_by_tran_combined[-1]
marine_by_tran_combined_pres_abs[marine_by_tran_combined_pres_abs>0]<-1

marine_by_tran_combined_pres_abs<-cbind(marine_by_tran_combined_pres_abs, marine_by_tran_combined[1])


head(marine_by_tran_combined_pres_abs)

write.csv(marine_by_tran_combined_pres_abs, "C:Biodiversity idea//Output files//marine_by_tran_combined_pres_abs.csv", row.names=FALSE)




########### combing chris' plot level data with owen's plot-level data
#Owen:
head(marine_by_plot_from_notes_selected)
owen_otter<-marine_by_plot_from_notes_selected[ ,c("unq_plot", "unq_tran", "unq_isl", "otter_pres")]
View(owen_otter)

#chris
head(marine_by_plot_from_chris)
marine_by_plot_from_chris$otter_pres<-marine_by_plot_from_chris$otter_pres_chris  
chris_otter<-marine_by_plot_from_chris[, c("unq_plot", "unq_tran", "unq_isl", "otter_pres")]

#did they do the same islands? 
#what's in owen's but not chris'? 
subset(owen_otter, !(unq_isl %in% chris_otter$unq_isl)) %>% select(unq_isl, otter_pres) %>% group_by(unq_isl) %>% summarise_all(list(sum, length))
#CV15, MM07, ST01, ST08, TQ12

#what's in chris's but not owen's? 
subset(chris_otter, !(unq_isl %in% owen_otter$unq_isl)) %>% select(unq_isl, otter_pres) %>% group_by(unq_isl) %>% summarise_all(list(sum, length))
#MM09, MM10, MM11

combined_otter<-rbind(owen_otter, chris_otter)
head(combined_otter)

combined_otter_sum_tran <- combined_otter %>% dplyr::select(unq_tran, otter_pres) %>% group_by(unq_tran) %>%  summarise_all(list(otter_pres_all = sum, otter_n_plots=length))
head(combined_otter_sum_tran)
combined_otter_sum_tran$takeout <- ifelse(grepl("NE|NW|SE|SW|I",combined_otter_sum_tran$unq_tran), 1, 0)
combined_otter_sum_tran<-combined_otter_sum_tran %>% filter(takeout==0)
View(combined_otter_sum_tran)

combined_otter_sum <- combined_otter %>% dplyr::select(unq_isl, otter_pres) %>% group_by(unq_isl) %>%  summarise_all(list(otter_pres_all = sum, otter_n_plots=length))
# combined_otter_sum$prop_otter<-combined_otter_sum$sum/combined_otter_sum$n

View(combined_otter_sum)
write.csv(combined_otter_sum, "C:Biodiversity idea//Output files//combined_otter_sum.csv", row.names=FALSE)



###### Plotting 

ggplot(master_transect, aes(y=d15n, x=otter_pres_all))+geom_point()

#### SUM
head(master_transect2)
master_transect3<-merge(master_transect2, marine_by_tran_combined)

ggplot(master_transect3, aes(x=as.factor(otter_pres_all), y=d15n))+geom_boxplot()


ggplot(master_transect3, aes(x=marine_invert_pres_all, y=d15n))+geom_point()+geom_smooth(method="lm")
ggplot(master_transect3, aes(x=total_marine_evidence_all, y=d15n))+geom_point()+geom_smooth(method="lm")+geom_text(label=master_transect3$unq_tran)
ggplot(master_transect3, aes(x=vector_evidence_all, y=d15n))+geom_point()+geom_smooth(method="lm")+geom_text(label=master_transect3$unq_tran)
ggplot(master_transect3, aes(x=marine_evidence_all, y=d15n))+geom_point()+geom_smooth(method="lm")+geom_text(label=master_transect3$unq_tran)
ggplot(master_transect3, aes(x=fish_all, y=d15n))+geom_point()+geom_smooth(method="lm")+geom_text(label=master_transect3$unq_tran)

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


