setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Modelling practice")
#change to norahbrown if on work computer

library(tidyr)
library(plyr)
library(dplyr)
library(doBy)
library(ggplot2)

#loading all the data
i.feces.all<-read.csv("C:Deb.data//i-feces-all.csv")
i.feathers.all<-read.csv("C:Deb.data//i-feathers-all (1).csv")
i.veg.all<-read.csv("C:Deb.data//i-veg-all (1).csv")
i.soil.all<-read.csv("C:Deb.data//i-soil-all.csv")
shoredist.deb<-read.csv("C:Deb.data//shoredist.csv")

wrack.sara<-read.csv("C:Sara's data//wrack.isotopes.cn.csv")
wrack.key<-read.csv("C:Sara's data//wrack.key.csv")
ang.seaweed<-read.csv("C:Ang's data//chokedpass_macrophytes_AMO2015.csv", header=TRUE, sep=",")
becky.eagles<-read.csv("c:Becky.data//becky.isotopes.csv", header=TRUE, sep=",")
soil_clean<-read.csv("c:Owen's data//soil_clean.csv", header=TRUE, sep=",")
owen_key<-read.csv("c:Owen's data//key.csv", header=TRUE, sep=",")
otter_chris<-read.csv("C:Chris.data//otter_sia.csv")
katie.mammal<-read.csv("c:Katie.data//katie.mammal.csv", header=TRUE, sep=",")
feathers.key<-read.csv("c:Deb.data//banding-all.csv", header=TRUE, sep=",")
owen.veg<-read.csv("C:Owen's data\\foliar_clean_sorted_merge_meta.csv")
hakai_plot<-read.csv("c:Owen's data//hakai_plot.csv", header=TRUE, sep=",")


#making the data match up
head(owen.veg)
head(owen_key)
head(katie.mammal)
head(otter_chris)
head(soil_clean)
head(ang.seaweed)
head(i.feathers.all)
head(i.feces.all)
head(becky.eagles.tree)
head(wrack.sara)
head(wrack.merge)
head(hakai_plot)
head(owen_key)
head(i.soil.all)
head(shoredist.deb)



#veg
names(owen.veg)[11]<-"Node"
names(owen.veg)[3]<-"group"
head(owen.veg)

#seaweed
names(ang.seaweed)[8]<-"d13c"
names(ang.seaweed)[9]<-"d15n"
ang.seaweed$Dryness<-"L"
ang.seaweed$person<-"Ang"
names(wrack.sara)[2]<-"d13c"
names(wrack.sara)[5]<-"d15n"
wrack.sara$group<-"wrack"
wrack.merge<-merge(wrack.key, wrack.sara, by="Sample.ID")
names(wrack.merge)[3]<-"Node"
wrack.merge$person<-"Sara"

#eagles
becky.eagles.tree<-becky.eagles[becky.eagles$type=="eagle",]
becky.eagles.tree$group<-"eagles"
names(becky.eagles.tree)[4]<-"d13c"
names(becky.eagles.tree)[5]<-"d15n"

#soil
soil.deb<-merge(i.soil.all, shoredist.deb, by="pcid")
head(soil.deb)
names(soil.deb)[15]<-"shore_dist"
names(soil.deb)[11]<-"unq_isl"
names(soil.deb)[4]<-"c"
names(soil.deb)[5]<-"n"
names(soil.deb)[6]<-"s"
names(soil.deb)[7]<-"cn"

head(soil_clean)
names(soil_clean)[6]<-"d13c"
names(soil_clean)[7]<-"d15n"
soil_clean$group<-"soil"
soil_clean$broad_group<-"soil"
soil_merge<-merge(soil_clean, owen_key, by="unq_plot")
soil_merge<-merge(soil_merge, hakai_plot, by="unq_plot")
head(soil_merge)
names(soil_merge)[12]<-"Node"
names(soil_merge)[5]<-"s"

which( colnames(soil_merge)=="unq_isl" )
which( colnames(soil_merge)=="d13c" )
which( colnames(soil_merge)=="d15n" )


soil_owen_deb<-rbind(soil_merge[,c(13,17,6,7,3,2,5,4)], soil.deb[,c(11,15,2,3,4,5,6,7)])
View(soil_owen_deb)

#mice
names(katie.mammal)[5]<-"d13c"
names(katie.mammal)[8]<-"d15n"
names(katie.mammal)[18]<-"species"
names(katie.mammal)[13]<-"group"
names(katie.mammal)[22]<-"broad_group"

#otter
head(otter_chris)
names(otter_chris)[6]<-"d13c"
names(otter_chris)[8]<-"d15n"
names(otter_chris)[13]<-"group"
otter_chris$broad_group<-"otter"
otter_chris_onsite_soil<-otter_chris[otter_chris$group=="AC" & otter_chris$SIASample=="Soil",]
otter_chris_onsite_soil$group<-"otter"
otter_chris_onsite_soil$Node<-"CV"

#birds
i.feathers.all$group<-"feathers"
i.feces.all$group<-"feces"
feathers.merge<-merge(feathers.key, i.feathers.all, by="band")
head(feathers.merge)
# i.feces.all[, c(15,2,3)],

katie.inverts<- dplyr::select(filter(katie.mammal, group == "Invertebrate tissue - whole body"),c(d13c, d15n, species, broad_group, Loc_Caught, Node))
head(katie.inverts)
katie.inverts$group<-"katie_inverts"

katie.berries<- dplyr::select(filter(katie.mammal, group == "Vegetation - berry/fruit"),c(d13c, d15n, species, group, Node))
head(katie.berries)
katie.berries$group<-"katie_berries"

katie.mouse.hair<- dplyr::select(filter(katie.mammal, group == "Mouse hair"),c(d13c, d15n, species, Node))
katie.mouse.hair$group<-"Mouse hair"
head(katie.mouse.hair)

katie.mouse.feces<- dplyr::select(filter(katie.mammal, group == "Small mammal faecal"),c(d13c, d15n, species, Node))
katie.mouse.feces$group<-"Mouse feces"
head(katie.mouse.feces)


which( colnames(wrack.merge)=="d13c" )
which( colnames(wrack.merge)=="d15n" )
which( colnames(wrack.merge)=="group" )
which( colnames(wrack.merge)=="Node" )
which( colnames(wrack.merge)=="person" )

which( colnames(feathers.merge)=="d13c" )
which( colnames(feathers.merge)=="d15n" )
which( colnames(feathers.merge)=="group" )
which( colnames(feathers.merge)=="Node" )

#katie.mouse.feces[,c(5,1,2,4)],

all.isotope.together<-rbind(soil_merge[,c(9,6,7,12)],owen.veg[,c(3,8,9,11)],  katie.berries[,c(4,1,2,5)] ,wrack.merge[,c(31,16,19,3)], katie.mouse.hair[,c(5,1,2,4)], katie.inverts[,c(7,1,2,6)],becky.eagles.tree[, c(8,4,5,3)], otter_chris_onsite_soil[,c(13,6,8, 16)], feathers.merge[, c(55,43,44,36)])
head(all.isotope.together)

all.isotope.together<-all.isotope.together[all.isotope.together$Node!="",]
all.isotope.together<-all.isotope.together[all.isotope.together$Node!="ALL",]


#plotr all together
ggplot(all.isotope.together, aes(x=d13c, y=d15n)) + geom_point(aes(col=group))+stat_ellipse(aes(color=group))




+facet_wrap(~Node, ncol=3)



#p[lot by node]
ggplot(filter(all.isotope.together, Node=="CV"), aes(x=d13c, y=d15n)) + geom_point(aes(col=group))+stat_ellipse(aes(color=group),type = "norm")


#insect species
head(katie.inverts)
ggplot(filter(katie.inverts, species=="Collembola"), aes(x=d13c, y=d15n)) + geom_point(aes(col=Loc_Caught))+stat_ellipse(aes(color=Loc_Caught),type = "norm")


