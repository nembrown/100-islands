library(here)

#read in necessary packages
library(tidyr)
library(plyr)
library(dplyr)
library(doBy)
library(ggplot2)
library(car)
library(fitdistrplus)
library(tidyverse)

library(purrr)
library(ggcorrplot)
library(corrr)
library(PerformanceAnalytics)

library(vegan)
library(betapart)
library(bipartite)
library(viridis)

library(naniar)
library(beyonce)



# Soil -------------------------------------------------------

##### DEB
#Deb's soil data and Deb's shore dist
i.soil.all<-read.csv("C:Food web idea//Data by person//Deb.data//i-soil-all.csv")
#this is isotopes
head(i.soil.all)

shoredist.deb<-read.csv("C:Food web idea//Data by person//Deb.data//shoredist.csv")
#this is the point count to distance to shore data

pointcount.gps<-read.csv("C:Food web idea//Data by person//Deb.data//pointcounts.csv")
head(pointcount.gps)
pointcount.gps$pcid<-gsub(" ", "", pointcount.gps$pcid, fixed = TRUE)
pointcount.gps<-pointcount.gps[,c(3,16,17)]
pointcount.gps<-pointcount.gps[!duplicated(pointcount.gps$pcid),]
#sometimes taken twice...  

shoredist.deb<-merge(shoredist.deb, pointcount.gps, by.x="pcid", all=TRUE)
head(shoredist.deb)
length(i.soil.all$sample.id)

soil.deb<-merge(i.soil.all, shoredist.deb, by.x="pcid", all=TRUE)
head(soil.deb)
names(soil.deb)[16]<-"shore_dist"
names(soil.deb)[11]<-"unq_isl"
names(soil.deb)[4]<-"c"
names(soil.deb)[5]<-"n"
names(soil.deb)[6]<-"s"
names(soil.deb)[7]<-"cn"
names(soil.deb)[1]<-"unq_plot"

length(soil.deb$unq_plot)
head(soil.deb)


#####OWEN
#owen's isotope data by plot
soil_clean<-read.csv("C:Food web idea//Data by person//Owen's data//soil_clean.csv", header=TRUE, sep=",")
head(soil_clean)
length((soil_clean$unq_plot))

#duplicated plots: 
soil_clean[duplicated(soil_clean$unq_plot),]
#let's keep them in for now

names(soil_clean)[6]<-"d13c"
names(soil_clean)[7]<-"d15n"

#Owen's key data
owen_key<-read.csv("C:Food web idea//Data by person//Owen's data//key_mod.csv", header=TRUE, sep=",")
head(owen_key)
length(unique(owen_key$unq_isl))

#Owen's plot-level soil info - moisture, slope etc
hakai_plot<-read.csv("C:Food web idea//Data by person//Owen's data//hakai_plot.csv", header=TRUE, sep=",")
names(hakai_plot)[3]<-"plant.richness"
head(hakai_plot)

owen_key_expanded<-merge(owen_key, hakai_plot, by.x="unq_plot", all=TRUE)
head(owen_key_expanded)
length(unique(owen_key_expanded$unq_isl))

#Add in the GPS coordinates
owen_coords<-read.csv("C:Food web idea//Data by person//Becky.data//ofwi_tran_coords.csv", header=TRUE, sep=",")
head(owen_coords)
owen_coords<-owen_coords[,c(1:9)]

owen_coords$unq_tran<- paste(owen_coords$unq_isl,owen_coords$TRANSECT)
owen_coords$unq_tran<-gsub(" ", "", owen_coords$unq_tran, fixed = TRUE)

owen_coords<-owen_coords[,c(3,4, 10)]
head(owen_coords)
names(owen_coords)[1]<-"easting"
names(owen_coords)[2]<-"northing"

owen_key_expanded<-merge(owen_key_expanded, owen_coords, by="unq_tran", all=TRUE)
head(owen_key_expanded)


#put isotope data together with the key
soil_merge<-merge(soil_clean, owen_key_expanded, by.x="unq_plot")
head(soil_merge)


soil_merge[duplicated(soil_merge$unq_plot),]
length(unique(soil_merge$unq_isl))
#there are a bunch of extras but will wait first to see how to deal with them ... 


col_names_selected<-c("unq_plot" ,
                      "unq_isl" ,
                      "shore_dist" ,
                      "d13c" ,
                      "d15n" ,
                      "c" ,
                      "n" ,
                      "s" ,
                      "cn" , 
                      "node" ,
                      "easting" ,
                      "northing" )

soil_owen_deb<-rbind(soil_merge_isl[,colnames(soil_merge_isl) %in% col_names_selected], soil.deb[,colnames(soil.deb) %in% col_names_selected])
head(soil_owen_deb)


### add in d34s here
soil_s<-read.csv("C:Food web idea/Data by person/Norah.data/soil_s.csv")
head(soil_s)
soil_s$d34s<-as.numeric(soil_s$d34s)

soil_owen_deb<-merge(soil_owen_deb, soil_s, by="unq_plot", all.x=TRUE)

write.csv(soil_owen_deb, "C:Food web idea//Data by person//Norah.data/soil_owen_deb.csv")



length(soil.deb$unq_plot)
length(soil_merge$unq_plot)
length(soil_owen_deb$unq_plot)
length(unique(soil_owen_deb$unq_isl))
#this just adds the two together... 


#now we want one value per island: 

soil_owen_deb_by_isl<- soil_owen_deb %>%  group_by(unq_isl)%>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(soil_owen_deb_by_isl)
soil_owen_deb_by_isl$group<-"soil_whole_island"

soil_owen_deb_by_isl_0m<- soil_owen_deb %>%  filter(shore_dist==0) %>%  group_by(unq_isl)%>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(soil_owen_deb_by_isl_0m)
soil_owen_deb_by_isl_0m$group<-"soil_0m"

head(soil_owen_deb_by_isl_0m)

soil_owen_deb_by_isl<-rbind(soil_owen_deb_by_isl[,c(1:7,11,12)], soil_owen_deb_by_isl_0m[,c(1:7,11,12)])

head(soil_owen_deb_by_isl)




# Vegetation --------------------------------------------------------------


owen.veg<-read.csv("C:Food web idea//Data by person//Owen's data\\foliar_clean_sorted_merge_meta.csv")
head(owen.veg)
owen.veg<-owen.veg[,-1]
names(owen.veg)[3]<-"n"
names(owen.veg)[4]<-"c"
names(owen.veg)[5]<-"cn"
names(owen.veg)[6]<-"s"
names(owen.veg)[7]<-"d13c"
names(owen.veg)[8]<-"d15n"
names(owen.veg)[2]<-"group"


library(car)
deb.veg<-read.csv("C:Food web idea//Data by person//Deb.data//i-veg-all (1).csv")
head(deb.veg)
deb.veg<-deb.veg[,-1]
names(deb.veg)[6]<-"group"
names(deb.veg)[4]<-"n"
names(deb.veg)[3]<-"c"
names(deb.veg)[5]<-"cn"
names(deb.veg)[1]<-"d13c"
names(deb.veg)[2]<-"d15n"
names(deb.veg)[12]<-"unq_isl"
deb.veg$s<-"NA"
deb.veg$shore_dist<-"interior"

deb.veg$unq_plot<-paste(deb.veg$unq_isl, "-", deb.veg$point)
deb.veg$unq_plot<-gsub(" ", "", deb.veg$unq_plot, fixed = TRUE)
deb.veg$group<-gsub(" ", "", deb.veg$group, fixed = TRUE)

deb.veg$group[deb.veg$group=="SAL"] <- "gash"
deb.veg$group[deb.veg$group=="FLV"] <- "midi"

veg.names<-c("unq_plot" ,
                      "unq_isl" ,
                      "shore_dist" ,
                      "d13c" ,
                      "d15n" ,
                      "c" ,
                      "n" ,
                      "s" ,
                      "cn" , 
                      "group" ,
                      "shore_dist" )

veg_owen_deb<-rbind(deb.veg[,colnames(deb.veg) %in% veg.names], owen.veg[,colnames(owen.veg) %in% veg.names])
head(veg_owen_deb)

veg_owen_deb %>%replace_with_na(replace = list(s = "NA"))
str(veg_owen_deb)
veg_owen_deb$s<-as.numeric(veg_owen_deb$s)
head(veg_owen_deb)

veg_owen_deb_by_isl<- veg_owen_deb %>% group_by(unq_isl, group)%>% summarise_if(is.numeric, mean, na.rm=TRUE)
veg_owen_deb_by_isl$d34s<-"NA"
veg_owen_deb_by_isl$d34s<-as.numeric(veg_owen_deb_by_isl$d34s)
head(veg_owen_deb_by_isl)
head(soil_owen_deb_by_isl)

isotope.names<-c("unq_isl" ,
             "d13c" ,
             "d15n" ,
             "c" ,
             "n" ,
             "s" ,
             "cn" , 
             "group" ,
             "d34s" )

veg_soil_owen_deb_by_isl<-dplyr::bind_rows(veg_owen_deb_by_isl[,colnames(veg_owen_deb_by_isl) %in% isotope.names], soil_owen_deb_by_isl[,colnames(soil_owen_deb_by_isl) %in% isotope.names])
head(veg_soil_owen_deb_by_isl)
length(unique(soil_owen_deb_by_isl$unq_isl))

isotope_by_isl_gathered<-veg_soil_owen_deb_by_isl
# Birds -------------------------------------------------------------------


#loading all the data
i.feces.all<-read.csv("C:Food web idea//Data by person//Deb.data//i-feces-all.csv")
i.feathers.all<-read.csv("C:Food web idea//Data by person//Deb.data//i-feathers-all (1).csv")
feathers.key<-read.csv("C:Food web idea//Data by person//Deb.data//banding-all.csv", header=TRUE, sep=",")


#birds
i.feathers.all$group<-"bird_feathers"
i.feces.all$group<-"bird_feces"
feathers.merge<-merge(feathers.key, i.feathers.all, by="band")
which( colnames(feathers.merge)=="island" )
which( colnames(feathers.merge)=="tot.c" )
which( colnames(feathers.merge)=="tot.n" )
which( colnames(feathers.merge)=="cn.ratio" )
names(feathers.merge)[37]<-"unq_isl"
names(feathers.merge)[45]<-"c"
names(feathers.merge)[46]<-"n"
names(feathers.merge)[47]<-"cn"
feathers.merge$s<-"NA"
feathers.merge$s<-as.numeric(feathers.merge$s)
feathers.merge$d34s<-"NA"
feathers.merge$d34s<-as.numeric(feathers.merge$d34s)


feathers.merge<- feathers.merge %>%   group_by(unq_isl, group)%>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(feathers.merge)

bird.feces.merge<-merge(feathers.key, i.feces.all, by="band")
head(bird.feces.merge)
which( colnames(bird.feces.merge)=="island" )
which( colnames(bird.feces.merge)=="tot.c" )
which( colnames(bird.feces.merge)=="tot.n" )
which( colnames(bird.feces.merge)=="cn.ratio" )

names(bird.feces.merge)[37]<-"unq_isl"
names(bird.feces.merge)[45]<-"c"
names(bird.feces.merge)[46]<-"n"
names(bird.feces.merge)[47]<-"cn"
bird.feces.merge$s<-"NA"
bird.feces.merge$s<-as.numeric(bird.feces.merge$s)
bird.feces.merge$d34s<-"NA"
bird.feces.merge$d34s<-as.numeric(bird.feces.merge$d34s)


bird.feces.merge<- bird.feces.merge %>%   group_by(unq_isl, group)%>% summarise_if(is.numeric, mean, na.rm=TRUE)

bird_isotopes<-rbind(bird.feces.merge[,colnames(bird.feces.merge) %in% isotope.names], feathers.merge[,colnames(feathers.merge) %in% isotope.names])

head(bird_isotopes)

head(isotope_by_isl_gathered)

isotope_by_isl_gathered2<-rbind(bird_isotopes, isotope_by_isl_gathered)
head(isotope_by_isl_gathered2)



# Mammals -----------------------------------------------------------------


katie.mammal<-read.csv("C:Food web idea//Data by person//Katie.data//katie.mammal.csv", header=TRUE, sep=",")
head(katie.mammal)
katie.mammal$s<-"NA"
katie.mammal$s<-as.numeric(katie.mammal$s)
katie.mammal$cn<-"NA"
katie.mammal$cn<-as.numeric(katie.mammal$cn)

names(katie.mammal)[2]<-"unq_isl"
names(katie.mammal)[5]<-"d13c"
names(katie.mammal)[6]<-"c"
names(katie.mammal)[8]<-"d15n"
names(katie.mammal)[9]<-"n"

names(katie.mammal)[18]<-"species"
names(katie.mammal)[13]<-"group"
names(katie.mammal)[22]<-"broad_group"
head(katie.mammal)


katie.inverts<- dplyr::select(filter(katie.mammal, group == "Invertebrate tissue - whole body"),c(unq_isl,cn,s,d13c, d15n, c,n,species, broad_group, Loc_Caught))
katie.inverts$group<-paste("katie_inverts", katie.inverts$Loc_Caught)
head(katie.inverts)
katie.inverts$d34s<-"NA"
katie.inverts$d34s<-as.numeric(katie.inverts$d34s)


katie.berries<- dplyr::select(filter(katie.mammal, group == "Vegetation - berry/fruit"),c(unq_isl,cn,s,d13c, d15n,c,n, species, group))
head(katie.berries)
katie.berries$group<-"katie_berries"
katie.berries$d34s<-"NA"
katie.berries$d34s<-as.numeric(katie.berries$d34s)


katie.mouse.hair<- dplyr::select(filter(katie.mammal, group == "Mouse hair"),c(unq_isl,cn,s,d13c, d15n, species,c,n,group))
katie.mouse.hair$group<-"Mouse hair"
head(katie.mouse.hair)
katie.mouse.hair$d34s<-"NA"
katie.mouse.hair$d34s<-as.numeric(katie.mouse.hair$d34s)


katie.mouse.feces<- dplyr::select(filter(katie.mammal, group == "Small mammal faecal"),c(unq_isl,cn,s,d13c, d15n,c,n, species,group))
katie.mouse.feces$group<-"Mouse feces"
head(katie.mouse.feces)
katie.mouse.feces$d34s<-"NA"
katie.mouse.feces$d34s<-as.numeric(katie.mouse.feces$d34s)


katie.isotopes<-rbind(katie.inverts[,colnames(katie.inverts) %in% isotope.names], 
                      katie.berries[,colnames(katie.berries) %in% isotope.names], 
                      katie.mouse.hair[,colnames(katie.mouse.hair) %in% isotope.names], 
                      katie.mouse.feces[,colnames(katie.mouse.feces) %in% isotope.names])



head(katie.isotopes)

katie.isotopes.isl<- katie.isotopes %>% group_by(unq_isl, group) %>% summarise_if(is.numeric, mean, na.rm=TRUE)

head(katie.isotopes.isl)


head(isotope_by_isl_gathered2)
isotope_by_isl_gathered2<-isotope_by_isl_gathered2 %>% group_by(unq_isl, group)

isotope_by_isl_gathered3<-rbind(isotope_by_isl_gathered2, katie.isotopes.isl[,c(1,3,4,5,6,7,8,2)])
head(isotope_by_isl_gathered3)



# Insects -----------------------------------------------------------------

chris.isotopes<-read.csv("C:Food web idea//Data by person//Chris.data//chris_isotopes_2018.csv", header=TRUE, sep=",")
head(chris.isotopes)
chris.isotopes$s<-as.numeric(chris.isotopes$s)

chris.isotopes.isl<-chris.isotopes %>% group_by(unq_isl, group) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(chris.isotopes.isl)
chris.isotopes.isl$d34s<-"NA"
chris.isotopes.isl$d34s<-as.numeric(chris.isotopes.isl$d34s)

length(chris.isotopes.isl$unq_isl[chris.isotopes$group=="insects_ISO"])

isotope_by_isl_gathered4<-rbind(isotope_by_isl_gathered3,chris.isotopes.isl[,colnames(chris.isotopes.isl) %in% isotope.names])

head(isotope_by_isl_gathered4)
write.csv(isotope_by_isl_gathered4, "C:Food web idea//Data by person//Norah.data//isotope_by_isl_gathered4.csv")

####

# chris.isotopes.species<-chris.isotopes %>% group_by(unq_isl, group, species) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
# head(chris.isotopes.species)
# 
# ggplot(chris.isotopes.species, aes(col=group,x=d13c, y=d15n))+geom_point()
# 
# ggplot(filter(chris.isotopes.species, group %in% c("insects_CUR")), aes(col=species,shape=group,x=d13c, y=d15n))+geom_point()+xlim(-30,-15)+ylim(-10,15)
# ggplot(filter(chris.isotopes.species, group %in% c( "insects_ISO")), aes(col=species,shape=group,x=d13c, y=d15n))+geom_point()+xlim(-30,-15)+ylim(-10,15)
# ggplot(filter(chris.isotopes.species, group %in% c("insects_COL")), aes(col=species,shape=group,x=d13c, y=d15n))+geom_point()+xlim(-30,-15)+ylim(-10,15)
# 
# 

# Adding habitat characteristics, terrestrial diversity, nearby marine biodiversity -------------------------

#terrestrial diversity
by_isl_master<-read.csv("C:Food web idea//Data by person//Owen's data//by_isl_master.csv")
head(by_isl_master)
head(isotope_by_isl_gathered4)
which( colnames(by_isl_master)=="d15n" )

isotope_master<-merge(isotope_by_isl_gathered4, by_isl_master[,-c(2:8,11)], by="unq_isl", all=TRUE)
head(isotope_master)

write.csv(isotope_master, "C:Food web idea//Data by person//Owen's data/isotope_master.csv")


#marine biodiversity
which( colnames(fish_richness_merged_tran_isl)=="unq_isl" )
fish_richness_merged_isl_simple<-fish_richness_merged_tran_isl[,c(1:45)]
head(fish_richness_merged_isl_simple)
isotope_master_2<-merge(isotope_master, fish_richness_merged_isl_simple, by.x="unq_isl", all=TRUE)



# Plotting ----------------------------------------------------------------


#marking colours to each type of isotope data
colorset = c("soil_0m"="#482576FF" ,"gash"="#B5C5D5","soil_whole_island"= "#795998",
             "midi"="#35608DFF" ,"insects_COL"="#CA5600","insects_CUR"= "#D41311" ,"insects_ISO"="#BB1717",
             "bird_feces"="#43BF71FF" ,"bird_feathers"= "#7AD151FF", "Mouse feces"= "#BBDF27FF", "Mouse hair" ="#FDE725FF" )

#isotope biplot
ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=d13c, y=d15n))+geom_point()+ stat_ellipse() +scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
ggsave("C:Food web idea//Plots//Biplots//Isotope biplot.png", width=30, height=20, unit="cm")

#d15n vs. n
d15n_n_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=n, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
d15n_n_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=n, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
d15n_n_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,x=n, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
d15n_n_13<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,x=n, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
d15n_n_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=n, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
d15n_n_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=n, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(d15n_n_10+ theme(legend.position="bottom"))
plot_grid(d15n_n_10+ theme(legend.position="none"),d15n_n_13,d15n_n_12,d15n_n_11,d15n_n_14,d15n_n_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biplots//d15n_n.png", width=30, height=20, unit="cm")


ggplot(filter(isotope_master, group %in% c("bird_feathers","bird_feces")), aes(fill=group, col=group,x=d13c, y=d15n, size=Area))+geom_point()+scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
ggplot(filter(isotope_master, group %in% c("bird_feathers")), aes(fill=group, col=group,x=d13c, y=d15n, size=Area))+geom_point()+scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
ggplot(filter(isotope_master, group %in% c("Mouse feces")), aes(fill=group, col=group,x=d13c, y=d15n, size=Area))+geom_point()+scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
ggplot(filter(isotope_master, group %in% c("Mouse hair")), aes(fill=group, col=group,x=d13c, y=d15n, size=Area))+geom_point()+scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
ggplot(filter(isotope_master, group %in% c("insects_COL", "insects_CUR","insects_ISO")), aes(fill=group, col=group,x=d13c, y=d15n, size=Area))+geom_point()+scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
ggplot(filter(isotope_master, group %in% c("insects_ISO")), aes(fill=group, col=group,x=d13c, y=d15n, size=Area))+geom_point()+scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")

ggplot(filter(isotope_master, group %in% c("insects_COL","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=d13c, y=d15n, size=Area))+geom_point()+scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
ggplot(filter(isotope_master, group %in% c("insects_COL","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=d13c, y=d15n, size=Area))+geom_point()+scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+geom_smooth(method="lm")


u# Plotting terrestrial richness and nutrients ----------------------------

#Richness vs. d15n
nutrient_0<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=total_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
nutrient_1<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,y=insect_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))  +ylim(0,350)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
nutrient_2<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=tree_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,10)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nutrient_3<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=plant_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,60)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nutrient_4<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,y=bird.richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,25)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nutrient_5<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=mammal_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend <- get_legend(nutrient_0+ theme(legend.position="bottom"))
plot_grid(nutrient_0+ theme(legend.position="none"),nutrient_3,nutrient_2,nutrient_1,nutrient_4,nutrient_5,legend, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots///Richness_nut//Richness_d15n.png", width=30, height=20, unit="cm")

#Evenness vs. dn15 and log_Area, only plants and insects
nut_evenness_0<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=total_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
nut_evenness_1<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,y=(insect_evenness), x=d15n)) + geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
nut_evenness_2<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=(tree_evenness), x=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_evenness_3<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=(plant_evenness), x=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_evenness_4<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,y=bird.evenness, x=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
#nut_evenness_5<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=mammal_richness, x=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend <- get_legend(nut_evenness_0+ theme(legend.position="bottom"))
plot_grid(nut_evenness_3+ theme(legend.position="none"),nut_evenness_2,nut_evenness_1,nut_evenness_4, legend, ncol=4, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Richness_nut//Evenness_d15n.png", width=30, height=20, unit="cm")


nut_evenness_11<-ggplot(isotope_master, aes(y=insect_evenness, x=log(Area)))+ylim(0,1) + geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
nut_evenness_21<-ggplot(isotope_master, aes(y=tree_evenness, x=log(Area)))+ylim(0,1)+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_evenness_31<-ggplot(isotope_master, aes(y=plant_evenness, x=log(Area)))+ylim(0,1)+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_evenness_41<-ggplot(isotope_master, aes(y=bird.evenness, x=log(Area)))+ylim(0,1)+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")





#Richness vs. d13c
nutrient_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=total_richness, x=d13c))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
nutrient_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,y=insect_richness, x=d13c))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))  +ylim(0,350)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
nutrient_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=tree_richness, x=d13c))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,10)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nutrient_13<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=plant_richness, x=d13c))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,60)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nutrient_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,y=bird.richness, x=d13c))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,25)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nutrient_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=mammal_richness, x=d13c))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(nutrient_10+ theme(legend.position="bottom"))
plot_grid(nutrient_10+ theme(legend.position="none"),nutrient_13,nutrient_12,nutrient_11,nutrient_14,nutrient_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Richness_nut//Richness_d13c.png", width=30, height=20, unit="cm")


#d13c vs. richness (axes flipped)
nutrient_101<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=total_richness, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
nutrient_111<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=insect_richness, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
nutrient_121<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,x=tree_richness, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nutrient_131<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,x=plant_richness, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nutrient_141<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=bird.richness, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nutrient_151<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=mammal_richness, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_101 <- get_legend(nutrient_101+ theme(legend.position="bottom"))
plot_grid(nutrient_101+ theme(legend.position="none"),nutrient_131,nutrient_121,nutrient_111,nutrient_141,nutrient_151,legend_101, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Richness_nut//d13c_richness.png", width=30, height=20, unit="cm")

#Richness dn15 corrected by area
area_nutrient_0<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=((total_richness)/(log(Area))), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
area_nutrient_1<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,y=((insect_richness)/(log(Area))), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson")) +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
area_nutrient_2<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=((tree_richness)/(log(Area))), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_nutrient_3<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=((plant_richness)/(log(Area))), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_nutrient_4<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,y=((bird.richness)/(log(Area))), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_nutrient_5<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=((mammal_richness)/(log(Area))), x=d15n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_0 <- get_legend(area_nutrient_0+ theme(legend.position="bottom"))
plot_grid(area_nutrient_0+ theme(legend.position="none"),area_nutrient_3,area_nutrient_2,area_nutrient_1,area_nutrient_4,area_nutrient_5,legend_0, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Richness_nut//Richness_d15n_corrected.png", width=30, height=20, unit="cm")

#Richness d13c corrected by area
area_nutrient_c_1<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=((insect_richness)/(log(Area))), y=d13c))+geom_point()+geom_smooth(aes(),method="lm") +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
area_nutrient_c_3<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=((insect_richness)/(log(Area))), y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_nutrient_c_2<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=((insect_richness)/(log(Area))), y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_nutrient_c_4<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=((bird.richness)/(log(Area))), y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_nutrient_c_5<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=((mammal_richness)/(log(Area))), y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_0 <- get_legend(area_nutrient_c_0+ theme(legend.position="bottom"))
plot_grid(area_nutrient_c_1+ theme(legend.position="none"),area_nutrient_c_3,area_nutrient_c_2,area_nutrient_c_4,area_nutrient_c_5,legend_0, ncol=3)
ggsave("C:Food web idea//Plots//Richness_nut//Richness_d13c_corrected.png", width=30, height=20, unit="cm")


ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=bird.density, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=bird.density, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")


#Richness vs. total_N
total_N_0<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=total_richness, x=n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
total_N_1<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,y=insect_richness, x=n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))  +ylim(0,350)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
total_N_2<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=tree_richness, x=n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,10)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
total_N_3<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=plant_richness, x=n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,60)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
total_N_4<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,y=bird.richness, x=n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,25)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
total_N_5<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=mammal_richness, x=n))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend <- get_legend(total_N_0+ theme(legend.position="bottom"))
plot_grid(total_N_0+ theme(legend.position="none"),total_N_3,total_N_2,total_N_1,total_N_4,total_N_5,legend, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots///Richness_nut//Richness_total_N.png", width=30, height=20, unit="cm")

#Richness vs. total_C
total_C_0<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=total_richness, x=c))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
total_C_1<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,y=insect_richness, x=c))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))  +ylim(0,350)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
total_C_2<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=tree_richness, x=c))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,10)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
total_C_3<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=plant_richness, x=c))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,60)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
total_C_4<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,y=bird.richness, x=c))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,25)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
total_C_5<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=mammal_richness, x=c))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend <- get_legend(total_C_0+ theme(legend.position="bottom"))
plot_grid(total_C_0+ theme(legend.position="none"),total_C_3,total_C_2,total_C_1,total_C_4,total_C_5,legend, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots///Richness_nut//Richness_total_C.png", width=30, height=20, unit="cm")

#Richness vs. C:Food web idea//n
cn_0<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=total_richness, x=cn))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
cn_1<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,y=insect_richness, x=cn))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))  +ylim(0,350)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
cn_2<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=tree_richness, x=cn))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,10)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
cn_3<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=plant_richness, x=cn))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,60)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
cn_4<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,y=bird.richness, x=cn))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,25)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
cn_5<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=mammal_richness, x=cn))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend <- get_legend(cn_0+ theme(legend.position="bottom"))
plot_grid(cn_0+ theme(legend.position="none"),cn_3,cn_2,cn_1,cn_4,cn_5,legend, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots///Richness_nut//Richness_cn.png", width=30, height=20, unit="cm")

#Richness vs. C:Food web idea//n
s_0<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=total_richness, x=s))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
s_1<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,y=insect_richness, x=s))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))  +ylim(0,350)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
s_2<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=tree_richness, x=s))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,10)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
s_3<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,y=plant_richness, x=s))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,60)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
s_4<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,y=bird.richness, x=s))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+ylim(0,25)+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
s_5<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,y=mammal_richness, x=s))+geom_point()+geom_smooth(aes(),method="glm", method.args = list(family = "poisson"))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend <- get_legend(s_0+ theme(legend.position="bottom"))
plot_grid(s_0+ theme(legend.position="none"),s_3,s_2,s_1,s_4,s_5,legend, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots///Richness_nut//Richness_s.png", width=30, height=20, unit="cm")


# Plotting nutrients and biogeography -------------------------------------


#d13c vs. log_Area
area_101<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log(Area), y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
area_111<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=log(Area), y=d13c))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
area_121<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=log(Area), y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_131<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=log(Area), y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_141<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=log(Area), y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_151<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log(Area), y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_101 <- get_legend(area_101+ theme(legend.position="bottom"))
plot_grid(area_101+ theme(legend.position="none"),area_131,area_121,area_111,area_141,area_151,legend_101, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biogeog_nut//d13c_Area.png", width=30, height=20, unit="cm")


#d15n vs. log_Area
area_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log(Area), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
area_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=log(Area), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
area_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=log(Area), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_13<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=log(Area), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=log(Area), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log(Area), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(area_10+ theme(legend.position="bottom"))
plot_grid(area_10+ theme(legend.position="none"),area_13,area_12,area_11,area_14,area_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biogeog_nut//d15n_log_Area.png", width=30, height=20, unit="cm")


#d15n vs. Area
area_gam_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers")), aes(fill=group, col=group,x=Area, y=d15n))+geom_point()+geom_smooth(aes(),method="gam", formula=y~s(log(x)))+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
area_gam_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=Area, y=d15n))+geom_point()+geom_smooth(aes(),method="gam", formula=y~s(log(x)))  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
area_gam_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=Area, y=d15n))+geom_point()+geom_smooth(aes(),method="gam", formula=y~s(log(x))) +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")+abline(v=115915.6, col="blue")
area_gam_13<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=Area, y=d15n))+geom_point()+geom_smooth(aes(),method="gam", formula=y~s(log(x))) +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
area_gam_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=Area, y=d15n))+geom_point()+geom_smooth(aes(),method="gam", formula=y~s(log(x))) +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(area_gam_10+ theme(legend.position="bottom"))
plot_grid(area_gam_10+ theme(legend.position="none"),area_gam_13,area_gam_12,area_gam_11,area_gam_14,legend_10, ncol=3, rel_heights = c(1,1))
ggsave("C:Food web idea//Plots//Biogeog_nut//d15n_Area.png", width=30, height=20, unit="cm")


#finding the gam inflection point: 
#https://rpubs.com/hrlai/gam_inflection
library(RCurl)
require(bitops)
script <- getURL("https://gist.githubusercontent.com/gavinsimpson/ca18c9c789ef5237dbc6/raw/095c9be4d3654b5a8c05aaa6b9037ad1bdab53b3/derivSimulCI.R", ssl.verifypeer = FALSE)

eval(parse(text = script))
library(mgcv)
m <- gam(data=isotope_master_simple,formula=d15n~s(Area))

isotope_master_simple <- isotope_master %>%  filter( group =="soil_0m")
head(isotope_master_simple)

Y <- predict(m)

plot(isotope_master_simple$Area, isotope_master_simple$d15n)
lines(isotope_master_simple$Area,Y)
abline(v=115915.6, col="blue")

fd <- derivSimulCI(m) 
plot(fd, sizer = TRUE)

CI <- lapply(fd[1], function(x) t(apply(x$simulations, 1, quantile, probs = c(0.025, 0.975))))
first.zero.slope.index <- min(which(sign(CI$Area[, "2.5%"]) != sign(CI$Area[, "97.5%"])))
fd$eval[first.zero.slope.index]


### this shows that below 11 hectares of sizxe the n15 increases rapidly. 

#d15n vs. neighbouring land mass (exposure) 
neighb_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=Neighb_250, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
neighb_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=Neighb_250, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
neighb_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=Neighb_250, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
neighb_13<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=Neighb_250, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
neighb_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=Neighb_250, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
neighb_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=Neighb_250, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(neighb_10+ theme(legend.position="bottom"))
plot_grid(neighb_10+ theme(legend.position="none"),neighb_13,neighb_12,neighb_11,neighb_14,neighb_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biogeog_nut//d15n_exposure.png", width=30, height=20, unit="cm")


#d15n vs. Distance to mainland
DM_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=DistW_ML, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
DM_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=DistW_ML, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
DM_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=DistW_ML, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
DM_13<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=DistW_ML, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
DM_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=DistW_ML, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
DM_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=DistW_ML, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(DM_10+ theme(legend.position="bottom"))
plot_grid(DM_10+ theme(legend.position="none"),DM_13,DM_12,DM_11,DM_14,DM_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biogeog_nut//d15n_Distance_mainland.png", width=30, height=20, unit="cm")


#d15n vs. northing
C_Northing_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=C_Northing, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
C_Northing_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=C_Northing, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
C_Northing_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=C_Northing, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Northing_13<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=C_Northing, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Northing_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=C_Northing, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Northing_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=C_Northing, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(C_Northing_10+ theme(legend.position="bottom"))
plot_grid(C_Northing_10+ theme(legend.position="none"),C_Northing_13,C_Northing_12,C_Northing_11,C_Northing_14,C_Northing_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biogeog_nut//d15n_Northing.png", width=30, height=20, unit="cm")

#d15n vs. Easting
C_Easting_d15n_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=C_Easting, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
C_Easting_d15n_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=C_Easting, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
C_Easting_d15n_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=C_Easting, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Easting_d15n_13<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=C_Easting, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Easting_d15n_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=C_Easting, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Easting_d15n_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=C_Easting, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(C_Easting_d15n_10+ theme(legend.position="bottom"))
plot_grid(C_Easting_d15n_10+ theme(legend.position="none"),C_Easting_d15n_13,C_Easting_d15n_12,C_Easting_d15n_11,C_Easting_d15n_14,C_Easting_d15n_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biogeog_nut//d15n_Easting.png", width=30, height=20, unit="cm")

#cn vs. Easting
C_Easting_cn_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=C_Easting, y=cn))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
C_Easting_cn_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=C_Easting, y=cn))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
C_Easting_cn_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=C_Easting, y=cn))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Easting_cn_13<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=C_Easting, y=cn))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Easting_cn_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=C_Easting, y=cn))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Easting_cn_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=C_Easting, y=cn))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(C_Easting_cn_10+ theme(legend.position="bottom"))
plot_grid(C_Easting_cn_10+ theme(legend.position="none"),C_Easting_cn_13,C_Easting_cn_12,C_Easting_cn_11,C_Easting_cn_14,C_Easting_cn_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biogeog_nut//cn_Easting.png", width=30, height=20, unit="cm")


#n vs. Easting
C_Easting_n_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=C_Easting, y=n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
C_Easting_n_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=C_Easting, y=n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
C_Easting_n_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=C_Easting, y=n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Easting_n_13<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=C_Easting, y=n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Easting_n_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=C_Easting, y=n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Easting_n_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=C_Easting, y=n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(C_Easting_n_10+ theme(legend.position="bottom"))
plot_grid(C_Easting_n_10+ theme(legend.position="none"),C_Easting_n_13,C_Easting_n_12,C_Easting_n_11,C_Easting_n_14,C_Easting_n_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biogeog_nut//n_Easting.png", width=30, height=20, unit="cm")

#s vs. Easting
C_Easting_s_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=C_Easting, y=s))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
C_Easting_s_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=C_Easting, y=s))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
C_Easting_s_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=C_Easting, y=s))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Easting_s_13<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=C_Easting, y=s))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Easting_s_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=C_Easting, y=s))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
C_Easting_s_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=C_Easting, y=s))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(C_Easting_s_10+ theme(legend.position="bottom"))
plot_grid(C_Easting_s_10+ theme(legend.position="none"),C_Easting_s_13,C_Easting_s_12,C_Easting_s_11,C_Easting_s_14,C_Easting_s_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biogeog_nut//s_Easting.png", width=30, height=20, unit="cm")

#d15n vs. log_Dist_Near
Dist_Near_d15n_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log(Dist_Near), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
Dist_Near_d15n_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=log(Dist_Near), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
Dist_Near_d15n_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=log(Dist_Near), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
Dist_Near_d15n_13<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=log(Dist_Near), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
Dist_Near_d15n_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=log(Dist_Near), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
Dist_Near_d15n_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log(Dist_Near), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(Dist_Near_d15n_10+ theme(legend.position="bottom"))
plot_grid(Dist_Near_d15n_10+ theme(legend.position="none"),Dist_Near_d15n_13,Dist_Near_d15n_12,Dist_Near_d15n_11,Dist_Near_d15n_14,Dist_Near_d15n_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biogeog_nut//d15n_log_Dist_Near.png", width=30, height=20, unit="cm")

#d15n vs. slope_mean
slope_mean_d15n_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=slope_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
slope_mean_d15n_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=slope_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
slope_mean_d15n_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=slope_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
slope_mean_d15n_13<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=slope_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
slope_mean_d15n_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=slope_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
slope_mean_d15n_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=slope_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(slope_mean_d15n_10+ theme(legend.position="bottom"))
plot_grid(slope_mean_d15n_10+ theme(legend.position="none"),slope_mean_d15n_13,slope_mean_d15n_12,slope_mean_d15n_11,slope_mean_d15n_14,slope_mean_d15n_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biogeog_nut//d15n_slope_mean.png", width=30, height=20, unit="cm")

#d15n vs. SLOPE
SLOPE_d15n_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=SLOPE, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
SLOPE_d15n_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=SLOPE, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
SLOPE_d15n_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=SLOPE, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
SLOPE_d15n_13<-ggplot(filter(isotope_master, group %in% c("gash","midi")), aes(fill=group, col=group,x=SLOPE, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
SLOPE_d15n_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=SLOPE, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
SLOPE_d15n_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=SLOPE, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(SLOPE_d15n_10+ theme(legend.position="bottom"))
plot_grid(SLOPE_d15n_10+ theme(legend.position="none"),SLOPE_d15n_13,SLOPE_d15n_12,SLOPE_d15n_11,SLOPE_d15n_14,SLOPE_d15n_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Biogeog_nut//d15n_SLOPE.png", width=30, height=20, unit="cm")



# Plotting terrestrial nutrients and marine catch and other marine variables-------------------------

#d15n vs. marine_catch
nut_fish_101<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
nut_fish_111<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log(fish_biomass_corrected), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
nut_fish_121<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=fish_length, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_fish_131<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log(fish_abundance_corrected), y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_fish_141<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=bycatch_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_fish_151<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=bycatch_abundance_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_101 <- get_legend(nut_fish_101+ theme(legend.position="bottom"))
plot_grid(nut_fish_101+ theme(legend.position="none"),nut_fish_131,nut_fish_121,nut_fish_111,nut_fish_141,nut_fish_151,legend_101, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Marine_nut//d15n_marine_catch.png", width=30, height=20, unit="cm")



#d13c vs. marine_catch
nut_fish_c_101<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=fish_richness_corrected, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
nut_fish_c_111<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log(fish_biomass_corrected), y=d13c))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
nut_fish_c_121<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=fish_length, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_fish_c_131<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log(fish_abundance_corrected), y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_fish_c_141<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=bycatch_richness_corrected, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_fish_c_151<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=bycatch_abundance_corrected, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_101 <- get_legend(nut_fish_c_101+ theme(legend.position="bottom"))
plot_grid(nut_fish_c_101+ theme(legend.position="none"),nut_fish_c_131,nut_fish_c_121,nut_fish_c_111,nut_fish_c_141,nut_fish_c_151,legend_101, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Marine_nut//d13c_marine_catch.png", width=30, height=20, unit="cm")


#d15n vs. fish_biomass_bym3_mean
nut_bycatch_101<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=fish_biomass_bym3_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
nut_bycatch_111<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=fish_biomass_bym3_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
nut_bycatch_121<-ggplot(filter(isotope_master_2, group %in% c("gash","midi")), aes(fill=group, col=group,x=fish_biomass_bym3_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_bycatch_131<-ggplot(filter(isotope_master_2, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=fish_biomass_bym3_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_bycatch_141<-ggplot(filter(isotope_master_2, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=fish_biomass_bym3_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_bycatch_151<-ggplot(filter(isotope_master_2, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=fish_biomass_bym3_mean, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_101 <- get_legend(nut_bycatch_101+ theme(legend.position="bottom"))
plot_grid(nut_bycatch_101+ theme(legend.position="none"),nut_bycatch_131,nut_bycatch_121,nut_bycatch_111,nut_bycatch_141,nut_bycatch_151,legend_101, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Marine_nut//d15n_biomass.png", width=40, height=30, unit="cm")

#d15n vs. bycatch_richness_corrected
nut_bycatch_101<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=bycatch_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
nut_bycatch_111<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=bycatch_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
nut_bycatch_121<-ggplot(filter(isotope_master_2, group %in% c("gash","midi")), aes(fill=group, col=group,x=bycatch_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_bycatch_131<-ggplot(filter(isotope_master_2, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=bycatch_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_bycatch_141<-ggplot(filter(isotope_master_2, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=bycatch_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_bycatch_151<-ggplot(filter(isotope_master_2, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=bycatch_richness_corrected, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_101 <- get_legend(nut_bycatch_101+ theme(legend.position="bottom"))
plot_grid(nut_bycatch_101+ theme(legend.position="none"),nut_bycatch_131,nut_bycatch_121,nut_bycatch_111,nut_bycatch_141,nut_bycatch_151,legend_101, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Marine_nut//d15n_bycatch_richness.png", width=30, height=20, unit="cm")


#d13c vs. bycatch_abundance_corrected
nut_bycatch_c_101<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=bycatch_abundance_corrected, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
nut_bycatch_c_111<-ggplot(filter(isotope_master_2, group %in% c("soil_0m","soil_whole_island")), aes(fill=group, col=group,x=bycatch_abundance_corrected, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
nut_bycatch_c_121<-ggplot(filter(isotope_master_2, group %in% c("gash","midi")), aes(fill=group, col=group,x=bycatch_abundance_corrected, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_bycatch_c_131<-ggplot(filter(isotope_master_2, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=bycatch_abundance_corrected, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_bycatch_c_141<-ggplot(filter(isotope_master_2, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=bycatch_abundance_corrected, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
nut_bycatch_c_151<-ggplot(filter(isotope_master_2, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=bycatch_abundance_corrected, y=d13c))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_101 <- get_legend(nut_bycatch_c_101+ theme(legend.position="bottom"))
plot_grid(nut_bycatch_c_101+ theme(legend.position="none"),nut_bycatch_c_131,nut_bycatch_c_121,nut_bycatch_c_111,nut_bycatch_c_141,nut_bycatch_c_151,legend_101, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Marine_nut//d13c_bycatch_abundance.png", width=30, height=20, unit="cm")


#d15n vs. log_HAB2000
HAB2000_d15n_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log_HAB2000, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
HAB2000_d15n_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=log_HAB2000, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
HAB2000_d15n_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,x=log_HAB2000, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
HAB2000_d15n_13<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,x=log_HAB2000, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
HAB2000_d15n_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=log_HAB2000, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
HAB2000_d15n_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log_HAB2000, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(HAB2000_d15n_10+ theme(legend.position="bottom"))
plot_grid(HAB2000_d15n_10+ theme(legend.position="none"),HAB2000_d15n_13,HAB2000_d15n_12,HAB2000_d15n_11,HAB2000_d15n_14,HAB2000_d15n_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Marine_nut//d15n_log_HAB2000.png", width=30, height=20, unit="cm")

#d15n vs. log_site_sum_by_isl
site_sum_by_isl_d15n_10<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi","insects_COL","insects_CUR","insects_ISO","bird_feces","bird_feathers", "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log_site_sum_by_isl, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)
site_sum_by_isl_d15n_11<-ggplot(filter(isotope_master, group %in% c("insects_COL","insects_CUR","insects_ISO")), aes(fill=group, col=group,x=log_site_sum_by_isl, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")  +  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+theme(legend.position="none")
site_sum_by_isl_d15n_12<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,x=log_site_sum_by_isl, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
site_sum_by_isl_d15n_13<-ggplot(filter(isotope_master, group %in% c("soil_0m","gash","soil_whole_island","midi")), aes(fill=group, col=group,x=log_site_sum_by_isl, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
site_sum_by_isl_d15n_14<-ggplot(filter(isotope_master, group %in% c("bird_feces","bird_feathers")), aes(fill=group, col=group,x=log_site_sum_by_isl, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
site_sum_by_isl_d15n_15<-ggplot(filter(isotope_master, group %in% c( "Mouse feces", "Mouse hair")), aes(fill=group, col=group,x=log_site_sum_by_isl, y=d15n))+geom_point()+geom_smooth(aes(),method="lm")+  scale_fill_manual(values=colorset)+  scale_colour_manual(values=colorset)+ theme(legend.position="none")
legend_10 <- get_legend(site_sum_by_isl_d15n_10+ theme(legend.position="bottom"))
plot_grid(site_sum_by_isl_d15n_10+ theme(legend.position="none"),site_sum_by_isl_d15n_13,site_sum_by_isl_d15n_12,site_sum_by_isl_d15n_11,site_sum_by_isl_d15n_14,site_sum_by_isl_d15n_15,legend_10, ncol=3, rel_heights = c(1,1, .2))
ggsave("C:Food web idea//Plots//Marine_nut//d15n_log_site_sum_by_isl.png", width=30, height=20, unit="cm")

ggplot(isotope_master, aes(y=log_site_sum_by_isl, x=Neighb_250))+geom_point()+geom_smooth(aes(),method="lm")
ggsave("C:Food web idea//Plots//Marine_nut//Neighb250_log_site_sum_by_isl.png")

ggplot(isotope_master, aes(y=log_site_sum_by_isl, x=slope_mean))+geom_point()+geom_smooth(aes(),method="lm")
ggsave("C:Food web idea//Plots//Marine_nut//slope_mean_log_site_sum_by_isl.png")

