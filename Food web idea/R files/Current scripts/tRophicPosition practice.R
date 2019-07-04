install.packages("tRophicPosition")
library(tRophicPosition)
#have to run biplots of isotope data script first


BilagayMEC <- read.csv(system.file("extdata", "Bilagay-MEC.csv",
                                   package = "tRophicPosition"))

head(BilagayMEC)
View(BilagayMEC)


consumer <- loadIsotopeData(BilagayMEC, consumer = "Bilagay", consumersColumn = "FG",
                            b1 = "Pelagic_BL", b2 = "Benthic_BL",
                            baselineColumn = "FG",
                            group = "Coquimbo", groupsColumn = "Location")

plot(consumer, b1 = "Pelagic baseline", b2 = "Benthic baseline")
help(loadIsotopeData)


#owen's data
foliar_clean_sorted<-read.csv("c:Owen's data//foliar_clean_sorted.csv", header=TRUE, sep=",")
foliar_clean_sorted<-foliar_clean_sorted[,-9]
foliar_clean_sorted<-na.omit(foliar_clean_sorted)
head(foliar_clean_sorted)
length(foliar_clean_sorted$unq_plot)
#1190 - the NAs were elsewhere


foliar_key<-read.csv("c:Owen's data//key_mod.csv", header=TRUE, sep=",")
head(foliar_key)
length(foliar_key$node)
#1585 when you read in _mod
#20 duplicates
##put NA= 0 for the distance
foliar_key<-foliar_key[,-5]
#there are NAs in the transect so take that out, not impo info
### In foliar mod I also modified the distance to be the closest 10m ... so that we only ahve 5 categories
#### we can just call in key_mod_trans and I have assignee transect numbers ... not sure why they were NA? 

#make sure foliar only has the amount of rows that match with soil clean
foliar_clean_sorted_merge<-merge(foliar_clean_sorted,soil_clean, by="unq_plot")
str(foliar_clean_sorted_merge)
head(foliar_clean_sorted_merge)
##cut down 1190 to 1162
#there are no nas

#now we need foliar_clean sorted with the meta-data foliar key
foliar_clean_sorted_merge_meta<-merge(foliar_clean_sorted_merge[,c(1:8)],foliar_key, by="unq_plot")
head(foliar_clean_sorted_merge_meta)
#there's a problem with the xs, rename
names(foliar_clean_sorted_merge_meta)[7]="d13c"
names(foliar_clean_sorted_merge_meta)[8]="d15n"

str(foliar_clean_sorted_merge_meta)
#1145 
#how did we lose rows?
#because we have soil from weird places not on transect... like 7.5? 
write.csv(foliar_clean_sorted_merge_meta, "C:Owen's data\\foliar_clean_sorted_merge_meta.csv")


soil_clean<-read.csv("c:Owen's data//soil_clean.csv", header=TRUE, sep=",")
head(soil_clean)
soil_clean<-na.omit(soil_clean)
length(soil_clean$unq_plot)
#678

soil_clean_merge_1<-merge(soil_clean, foliar_clean_sorted[,1:2], by=("unq_plot"))
head(soil_clean_merge_1)
length(soil_clean_merge_1$unq_plot)
#1162

soil_clean_merge_2<-merge(soil_clean_merge_1, foliar_key, by=("unq_plot"))
head(soil_clean_merge_2)
length(soil_clean_merge_2$unq_plot)
names(soil_clean_merge_2)[6]="d13c"
names(soil_clean_merge_2)[7]="d15n"


head(soil_clean_merge_2)

soil_clean_merge_2_unique<-unique(soil_clean_merge_2[,-9], by = "unq_plot")
head(soil_clean_merge_2_unique)
soil_clean_merge_2_unique$group<-"soil"
soil_clean_merge_2_unique$species<-"soil"

write.csv(soil_clean_merge_2_unique, "C:Owen's data\\soil_clean_merge_2_unique.csv")


head(foliar_clean_sorted_merge_meta)
foliar_clean_sorted_merge_meta$group<-"veg"

owen.soilveg<-rbind(soil_clean_merge_2_unique[,c(1,6,7,9:16)], foliar_clean_sorted_merge_meta[,c(1,7:15,2)])
head(owen.soilveg)        
str(owen.soilveg)
owen.soilveg$species<-as.factor(owen.soilveg$species)

write.csv(owen.soilveg, "C:Owen's data\\owen.soilveg.csv")

consumer.owen <- loadIsotopeData(owen.soilveg, consumer = "midi", consumersColumn = "species",
                            b1 = "soil",b2="gash",
                            baselineColumn = "species",
                            groupsColumn =NULL,group=NULL,  d13C = "d13c",
                            d15N = "d15n")
consumer.owen
plot(consumer.owen)


###### Now using Ang's seaweeds.... 

ang.seaweed<-read.csv("C:Ang's data//chokedpass_macrophytes_AMO2015.csv", header=TRUE, sep=",")
head(ang.seaweed)
str(ang.seaweed)
ang.seaweed$species


consumer.ang <- loadIsotopeData(ang.seaweed, consumer = "macroalgae", consumersColumn = "group",
                                 b1 = "seagrass",b2="epiphyte",
                                 baselineColumn = "group",
                                 groupsColumn =NULL,group=NULL,  d13C = "d13C",
                                 d15N = "d15N")

plot(consumer.ang)
?loadIsotopeData
help(loadIsotopeData)

ang.seaweed$species <- as.vector(ang.seaweed$species)

consumer.ang.species <- loadIsotopeData(ang.seaweed, consumer ="zostera marina", 
                                consumersColumn = "species",
                                b1 = "nereocystis luetkeana",b2="ulva lactuca",
                                baselineColumn = "species",
                                groupsColumn =NULL,group=NULL,  d13C = "d13C",
                                d15N = "d15N")

plot(consumer.ang.species)

head(ang.seaweed)
head(katie.inverts)

#load in becky's data. 
becky.eagles<-read.csv("c:Becky.data//becky.isotopes.csv", header=TRUE, sep=",")
head(becky.eagles)
names(becky.eagles)[3]="d13c"
names(becky.eagles)[4]="d15n"
becky.eagles$species<-"eagle"
becky.eagles$group<-becky.eagles$location
becky.eagles$broad_group<-"eagle"




#species, group, d13c, d15N, broad_group
all.isotope.together.2<-rbind(becky.eagles[,c(7,8,3,4,9)], ang.seaweed[,c(3,4,8,9,11)], owen.soilveg[,c(11,10,2,3, 12)], i.feathers.all[, c(7,15,2,3, 16)], katie.mouse.hair[,c(3,5,1,2,4)], katie.inverts[,c(3,6,1,2,4)], katie.berries[,c(3,4,1,2,5)])
head(all.isotope.together.2)

consumer.birds.insects <- loadIsotopeData(all.isotope.together.2, consumer ="feathers", 
                                        consumersColumn = "group",
                                        b1 = "inverts",b2=NULL,
                                        baselineColumn = "group",
                                        groupsColumn =NULL,group=NULL,  d13C = "d13c",
                                        d15N = "d15n")

plot(consumer.birds.insects, b2="")



consumer.birds.insects.seaweed <- loadIsotopeData(all.isotope.together.2, consumer ="feathers", 
                                          consumersColumn = "group",
                                          b1 = "inverts",b2="seaweed",
                                          baselineColumn = "group",
                                          groupsColumn =NULL,group=NULL,  d13C = "d13c",
                                          d15N = "d15n")

plot(consumer.birds.insects.seaweed)
head(all.isotope.together.2)

consumer.mice.insects.seaweed <- loadIsotopeData(all.isotope.together.2, consumer ="Mouse hair", 
                                                  consumersColumn = "group",
                                                  b1 = "inverts",b2="seaweed",
                                                  baselineColumn = "group",
                                                    d13C = "d13c",
                                                  d15N = "d15n")

plot(consumer.mice.insects.seaweed)

consumer.birds.insects.berries <- loadIsotopeData(all.isotope.together.2, consumer ="feathers", 
                                                  consumersColumn = "group",
                                                  b1 = "inverts",b2="Vegetation - berry/fruit",
                                                  baselineColumn = "group",
                                                  groupsColumn =NULL,group=NULL,  d13C = "d13c",
                                                  d15N = "d15n")

plot(consumer.birds.insects.berries)


consumer.becky.eagles <- loadIsotopeData(becky.eagles, consumer ="tree", 
                                                  consumersColumn = "location",
                                                  b1 = "center",
                                                  baselineColumn = "location",
                                                  groupsColumn =NULL,group=NULL,  d13C = "d13c",
                                                  d15N = "d15n")

plot(consumer.becky.eagles)

str(all.isotope.together.2)
consumer.becky.eagles.soil <- loadIsotopeData(all.isotope.together.2, consumer ="soil", 
                                         consumersColumn = "group",
                                         b1 = "tree",b2="center",
                                         baselineColumn = "group",
                                         groupsColumn =NULL,group=NULL,  d13C = "d13c",
                                         d15N = "d15n")

plot(consumer.becky.eagles.soil)
