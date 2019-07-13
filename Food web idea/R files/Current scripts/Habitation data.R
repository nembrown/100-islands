setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Food web idea")

library(tidyr)
library(plyr)
library(dplyr)
library(doBy)
library(ggplot2)


# Owen's data read and tidy -----------------------------------------------


# Read in Owen's data
longform_plant_percentcover<-read.csv("C:Data by person//Owen's data//Complete_long_percentcover_mod.csv", header=TRUE, sep=",")

#Read in Pat's data for area
islands_data<-read.csv("C:Data by person//Pat.data//HabitatClass.csv", header=TRUE, sep=",")
head(islands_data)

names(islands_data)[1]<-"unq_isl"
length(unique(islands_data$unq_isl))
#100 islands

#simply to just island and area
islands_data<-islands_data[,c(1,4)]

#how many islands does Owen have? 
head(longform_plant_percentcover)
length(unique(longform_plant_percentcover$unq_isl))
#94 owen

#merge owen and area
islands_plant<-merge(longform_plant_percentcover, islands_data, by="unq_isl")
head(islands_plant)

#Label small islands as those with Area less than 6000m2
levels <- c(-Inf, 6000, Inf)
labels <- c("small", "large")
islands_plant<- islands_plant %>% mutate(size.cat = cut(Area, levels, labels = labels))
head(islands_plant)


#Large islands are those that are over 6000m2, except for those that are long and skinny ... so max distance is less than 40
islands_plant<- islands_plant  %>%group_by(unq_isl)%>%  mutate(large_dist= max(dist))
islands_plant <- islands_plant %>% mutate(size.cat= replace(size.cat, large_dist>40, "small")) %>% ungroup()

islands_plant<-islands_plant[!duplicated(islands_plant),]


head(islands_plant)
#summing across layers Owen but no T!!!! 
islands_plant_noT <- islands_plant %>%  filter(layer!= "T")

islands_plant_sum<- islands_plant_noT %>%  group_by(unq_plot, species)%>% summarise(cover =sum(cover))

head(islands_plant_sum)
islands_plant_sum[duplicated(islands_plant_sum),]

islands_plant_info<-islands_plant[,-c(9:11, 14)]
head(islands_plant_info)
islands_plant_info<-islands_plant_info[!duplicated(islands_plant_info),]




islands_plant_filtered<-inner_join(islands_plant_sum, islands_plant_info, by=c("unq_plot")) 
head(islands_plant_filtered)

length(unique(islands_plant_filtered$unq_isl))
#94

islands_plant_filtered[duplicated(islands_plant_filtered),]

# Deb's data read and tidy ------------------------------------------------


#read in Deb's data
veg1x1_Deb_mod<-read.csv("C:Data by person//Owen's data//bird_long_percentcover_mod.csv", header=TRUE, sep=",")
head(veg1x1_Deb_mod)
length(unique(veg1x1_Deb_mod$unq_isl))
#99 islands

#adding shore dist to deb's veg
Deb_interior<-read.csv("C:Data by person//Deb.data//shoredist.csv", header=TRUE, sep=",")
veg1x1_Deb_mod_interior<-merge(veg1x1_Deb_mod, Deb_interior, by="pcid")
head(veg1x1_Deb_mod_interior)
names(veg1x1_Deb_mod_interior)[11]<-"shore_dist"
length(unique(veg1x1_Deb_mod_interior$unq_isl))
#99 islands 


#which column of Owen's data is size.cat? 
which( colnames(islands_plant)=="size.cat" )
#column 16

#adding island size.cat to Deb's data:
veg1x1_Deb_mod_interior_size<-join(veg1x1_Deb_mod_interior,islands_plant[!duplicated(islands_plant$unq_isl), c(1,16 )], by="unq_isl")
head(veg1x1_Deb_mod_interior_size)

#make a transect that is the same as plot for Deb's (since each plot is it's own transect) 
veg1x1_Deb_mod_interior_size$unq_plot<-veg1x1_Deb_mod_interior_size$unq_tran

veg1x1_Deb_mod_interior_size<-veg1x1_Deb_mod_interior_size[!duplicated(veg1x1_Deb_mod_interior_size),]

veg1x1_Deb_mod_interior_size_info<-veg1x1_Deb_mod_interior_size[,-c(7:10)]
veg1x1_Deb_mod_interior_size_info<-veg1x1_Deb_mod_interior_size_info[!duplicated(veg1x1_Deb_mod_interior_size_info),]
head(veg1x1_Deb_mod_interior_size_info)

#summing across layers (e.g. canopy, herbs) Deb
veg1x1_Deb_mod_interior_size_noT<-veg1x1_Deb_mod_interior_size %>% filter(layer!="T")
veg1x1_Deb_mod_interior_size_sum<- veg1x1_Deb_mod_interior_size_noT %>%  group_by(unq_plot, species)%>% summarise(cover =sum(cover))
head(veg1x1_Deb_mod_interior_size_sum)

#add this back to the main dataframe without the "species, layer, cover, notes" categories
veg1x1_Deb_mod_interior_size_filtered<-inner_join(veg1x1_Deb_mod_interior_size_sum,veg1x1_Deb_mod_interior_size_info,  by=c("unq_plot")) 
head(veg1x1_Deb_mod_interior_size_filtered)
length(unique(veg1x1_Deb_mod_interior_size_filtered$unq_isl))
#99 islands 
veg1x1_Deb_mod_interior_size_filtered[duplicated(veg1x1_Deb_mod_interior_size_filtered),]


# Combining Owen and Deb --------------------------------------------------


#Adding a column for person
islands_plant_filtered$person<-"Owen"
veg1x1_Deb_mod_interior_size_filtered$person<-"Deb"


which( colnames(islands_plant_filtered)=="unq_isl" )
which( colnames(islands_plant_filtered)=="size.cat" )
which( colnames(islands_plant_filtered)=="unq_tran" )
which( colnames(islands_plant_filtered)=="unq_plot" )
which( colnames(islands_plant_filtered)=="shore_dist" )
#which( colnames(islands_plant_filtered)=="layer" )
which( colnames(islands_plant_filtered)=="species" )
which( colnames(islands_plant_filtered)=="cover" )
which( colnames(islands_plant_filtered)=="person" )


which( colnames(veg1x1_Deb_mod_interior_size_filtered)=="unq_isl" )
which( colnames(veg1x1_Deb_mod_interior_size_filtered)=="size.cat" )
which( colnames(veg1x1_Deb_mod_interior_size_filtered)=="unq_tran" )
which( colnames(veg1x1_Deb_mod_interior_size_filtered)=="unq_plot" )
which( colnames(veg1x1_Deb_mod_interior_size_filtered)=="shore_dist" )
#which( colnames(veg1x1_Deb_mod_interior_size_filtered)=="layer" )
which( colnames(veg1x1_Deb_mod_interior_size_filtered)=="species" )
which( colnames(veg1x1_Deb_mod_interior_size_filtered)=="cover" )
which( colnames(veg1x1_Deb_mod_interior_size_filtered)=="person" )



#combining deb and owen's data 
Deb_Owen_veg_combined<-rbind(islands_plant_filtered[,c(4,14,12,1,11,2,3, 16)], veg1x1_Deb_mod_interior_size_filtered[,c(8,11,9,1,10,2,3,12)])


head(Deb_Owen_veg_combined)
length(unique(Deb_Owen_veg_combined$unq_isl))
#99

#There are some islands in Deb's database but not Owen's... 
which (is.na(Deb_Owen_veg_combined$size.cat))

#islands in Deb's dataset but not in Owens ... also TQ09 but can't find any info about that - discarded as an island? 
#just did this by hand but could have done earlier in a better way (cutting area etc.. )
Deb_Owen_veg_combined$size.cat[Deb_Owen_veg_combined$unq_isl=="ST05"]<-"large"
Deb_Owen_veg_combined$size.cat[Deb_Owen_veg_combined$unq_isl=="PR12"]<-"small"
Deb_Owen_veg_combined$size.cat[Deb_Owen_veg_combined$unq_isl=="PR07"]<-"small"
Deb_Owen_veg_combined$size.cat[Deb_Owen_veg_combined$unq_isl=="MM09"]<-"small"
Deb_Owen_veg_combined$size.cat[Deb_Owen_veg_combined$unq_isl=="MM10"]<-"large"
Deb_Owen_veg_combined$size.cat[Deb_Owen_veg_combined$unq_isl=="MM11"]<-"large"
Deb_Owen_veg_combined$size.cat[Deb_Owen_veg_combined$unq_isl=="TQ09"]<-"small"
which (is.na(Deb_Owen_veg_combined$size.cat))

head(Deb_Owen_veg_combined)
Deb_Owen_veg_combined<-Deb_Owen_veg_combined[!duplicated(Deb_Owen_veg_combined),]

# Filling 0s for each species ---------------------------------------------


Deb_Owen_veg_combined_complete <- Deb_Owen_veg_combined%>% 
                                    complete(unq_plot, species, fill=list(cover=0)) %>% 
                                    group_by(unq_plot) %>% 
                                    arrange(desc(cover), .by_group=TRUE)

Deb_Owen_veg_combined_complete_filled<-Deb_Owen_veg_combined_complete %>% fill(everything())



length(unique(Deb_Owen_veg_combined_complete_filled$unq_isl))
#100

##adding in info about shurb herb or tree
plant_category<-read.csv("C:Data by person//Owen's data//100Islands_Fitzpatrick_species.csv", header=TRUE, sep=",")
head(plant_category)


Deb_Owen_veg_combined_complete_filled<-merge(Deb_Owen_veg_combined_complete_filled, plant_category[,c(1,4,5)], by.x="species")


View(Deb_Owen_veg_combined_complete_filled)


write.csv(Deb_Owen_veg_combined_complete_filled, "C:Data by person//Kalina.data/Deb_Owen_veg_combined_complete_filled.csv")



# Mean for each island ----------------------------------------------------

#small islands
Deb_Owen_veg_combined_complete_filled_small<-Deb_Owen_veg_combined_complete_filled[Deb_Owen_veg_combined_complete_filled$size.cat=="small",]

length2 <- function (x, na.rm=FALSE) {if (na.rm) sum(!is.na(x))else length(x)}

cdata.Deb_Owen_veg_combined_complete_filled_small <- summaryBy(cover ~ unq_isl + species, data=as.data.frame(Deb_Owen_veg_combined_complete_filled_small), FUN=function(x) { c(mean = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE), length=length2(x)) } )
head(cdata.Deb_Owen_veg_combined_complete_filled_small )
names(cdata.Deb_Owen_veg_combined_complete_filled_small)[5]<-"N"
cdata.Deb_Owen_veg_combined_complete_filled_small$cover.se<-cdata.Deb_Owen_veg_combined_complete_filled_small$cover.sd/sqrt(cdata.Deb_Owen_veg_combined_complete_filled_small$N)
cdata.Deb_Owen_veg_combined_complete_filled_small$size.cat<-"small"
head(cdata.Deb_Owen_veg_combined_complete_filled_small)

#call transect just all_together
cdata.Deb_Owen_veg_combined_complete_filled_small$unq_tran<-"all_together"
cdata.Deb_Owen_veg_combined_complete_filled_small<-cdata.Deb_Owen_veg_combined_complete_filled_small[,c(1,8,2:7)]




#large islands
Deb_Owen_veg_combined_complete_filled_large<-Deb_Owen_veg_combined_complete_filled[Deb_Owen_veg_combined_complete_filled$size.cat=="large",]
head(Deb_Owen_veg_combined_complete_filled_large)

#First separate by Deb and Owen
Deb_Owen_veg_combined_complete_filled_large_Owen<-Deb_Owen_veg_combined_complete_filled_large[Deb_Owen_veg_combined_complete_filled_large$person=="Owen",]
Deb_Owen_veg_combined_complete_filled_large_Deb<-Deb_Owen_veg_combined_complete_filled_large[Deb_Owen_veg_combined_complete_filled_large$person=="Deb",]

Deb_Owen_veg_combined_complete_filled_large_Deb[duplicated(Deb_Owen_veg_combined_complete_filled_large_Deb),]
View(Deb_Owen_veg_combined_complete_filled_large_Deb)
#Deb's plots collapse into one interior transect
Deb_Owen_veg_combined_complete_filled_large_Deb_mean<- Deb_Owen_veg_combined_complete_filled_large_Deb %>%  group_by(unq_isl, species)%>% summarise(cover.mean =mean(cover), cover.sd=sd(cover), N=length(cover))
Deb_Owen_veg_combined_complete_filled_large_Deb_mean$cover.se<-Deb_Owen_veg_combined_complete_filled_large_Deb_mean$cover.sd/sqrt(Deb_Owen_veg_combined_complete_filled_large_Deb_mean$N)

#give each island Deb combo a named transect
Deb_Owen_veg_combined_complete_filled_large_Deb_mean <-Deb_Owen_veg_combined_complete_filled_large_Deb_mean  %>% mutate(unq_tran=paste(unq_isl, "Deb_interior", sep='_'))
View(Deb_Owen_veg_combined_complete_filled_large_Deb_mean)


#Owen's data collapse plot into transect'
head(Deb_Owen_veg_combined_complete_filled_large_Owen)
cdata.Deb_Owen_veg_combined_complete_filled_large_Owen <- summaryBy(cover ~ unq_isl + unq_tran + species, data=as.data.frame(Deb_Owen_veg_combined_complete_filled_large_Owen), FUN=function(x) { c(mean = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE), length=length2(x)) } )
names(cdata.Deb_Owen_veg_combined_complete_filled_large_Owen)[6]<-"N"
cdata.Deb_Owen_veg_combined_complete_filled_large_Owen$cover.se<-cdata.Deb_Owen_veg_combined_complete_filled_large_Owen$cover.sd/sqrt(cdata.Deb_Owen_veg_combined_complete_filled_large_Owen$N)
head(cdata.Deb_Owen_veg_combined_complete_filled_large_Owen)


#combined Owen and Deb's - large islands are now 5 transects per species: 4 Owen's and one interior. 
Deb_Owen_veg_large<-rbind(cdata.Deb_Owen_veg_combined_complete_filled_large_Owen, as.data.frame(Deb_Owen_veg_combined_complete_filled_large_Deb_mean[,c(1,7,2:6)]))

#lot's of Nas replace with 0
Deb_Owen_veg_large$cover.sd<-replace(Deb_Owen_veg_large$cover.sd, is.nan(Deb_Owen_veg_large$cover.sd), 0)
head(Deb_Owen_veg_large)
Deb_Owen_veg_large$size.cat<-"large"




# Combine small and large islands back again: 
head(cdata.Deb_Owen_veg_combined_complete_filled_small)
head(Deb_Owen_veg_large)

Veg_means_by_island<-rbind(cdata.Deb_Owen_veg_combined_complete_filled_small, Deb_Owen_veg_large)
head(Veg_means_by_island)

write.csv(Veg_means_by_island, "C:Data by person//Kalina.data/Veg_means_by_island.csv")


