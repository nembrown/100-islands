
library(tidyr)
library(vegan)
library(ggplot2)
library(ggcorrplot)
library(doBy)
library(plyr)
require(dplyr)
library(doBy)
library(cowplot)
library(viridis)


# Loading and merging terrestrial data by transect ------------------------------------
setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Modelling practice")
by_tran_master<-read.csv("C:Norah.data\\by_tran_master.csv")
head(by_tran_master)
by_tran_master<-by_tran_master[,-1]

by_isl_master<-read.csv("C:Owen's data//by_isl_master.csv")
by_isl_master<-by_isl_master[,-1]


head(by_isl_master)
paste(
  which( colnames(by_isl_master)=="unq_isl" ),
  which( colnames(by_isl_master)=="mammal_richness" ),
  which( colnames(by_isl_master)=="bird.richness" ),
  which( colnames(by_isl_master)=="habitat_het" ),
  which( colnames(by_isl_master)=="log_Area" ),
  which( colnames(by_isl_master)=="Neighb_250" ),
  which( colnames(by_isl_master)=="NDVI_mean" ),
  which( colnames(by_isl_master)=="Perimeter" ),
  which( colnames(by_isl_master)=="PA_norml" ),
  which( colnames(by_isl_master)=="DistW_ML" ),
  which( colnames(by_isl_master)=="Dist_Near" ),
  which( colnames(by_isl_master)=="Area" ),
  sep=","
)

by_isl_master_subset<-by_isl_master[,c(1,84,44,89,85,19,20,14,15,17,18,13)]
head(by_isl_master_subset)



by_tran_master_with_isl<-merge(by_tran_master, by_isl_master_subset, by="unq_isl", all=TRUE)
head(by_tran_master_with_isl)


# Read in otter data ------------------------------------------------------


otter_isotopes<-read.csv("C:Norah.data\\master_otter_isotope_nb_new.csv")
head(otter_isotopes)
otter_isotopes_2<- otter_isotopes 

##%>%  filter(site.id != "CVMIL12S8")
names(otter_isotopes_2)[9]<-"otter_lat"
names(otter_isotopes_2)[10]<-"otter_long"
names(otter_isotopes_2)[12]<-"otter_notes"
names(otter_isotopes_2)[2]<-"d13c_otter"
names(otter_isotopes_2)[3]<-"d15n_otter"
names(otter_isotopes_2)[4]<-"n_otter"
names(otter_isotopes_2)[5]<-"c_otter"
names(otter_isotopes_2)[6]<-"cn_otter"
names(otter_isotopes_2)[8]<-"island_otter"
names(otter_isotopes_2)[11]<-"node_otter"
head(otter_isotopes_2)



# Match otters to transects -----------------------------------------------


setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Biodiversity idea")

tran_distance_otter<-read.csv("C:Distance_btwn_points_otters_transect.csv")
tran_distance_otter<-tran_distance_otter[,-1]
head(tran_distance_otter)
names(tran_distance_otter)[3]<-"site.id"
names(tran_distance_otter)[6]<-"unq_tran"
#tran_distance_otter<- tran_distance_otter%>% filter(Distance < 1)
head(tran_distance_otter)

otter_isotopes_2_tran<-merge(otter_isotopes_2, tran_distance_otter[,c(3,5,6)], by="site.id", all.y=TRUE)
head(otter_isotopes_2_tran)


# Merge transect data and otter data ---------------------------------------
otter_isotopes_2_tran_merged<-merge(otter_isotopes_2_tran, by_tran_master_with_isl, by="unq_tran", all.x=TRUE)
head(otter_isotopes_2_tran_merged)


ggplot(otter_isotopes_2_tran_merged, aes(x=d13c_otter, y=d15n_otter, colour=d15n_beetles))+geom_point()+ scale_colour_viridis()


#otters different spatial scales
otter_isotopes_2_tran_merged_300<-otter_isotopes_2_tran_merged %>% filter(Distance < 0.3)
otter_isotopes_2_tran_merged_350<-otter_isotopes_2_tran_merged %>% filter(Distance < 0.35)
otter_isotopes_2_tran_merged_250<-otter_isotopes_2_tran_merged %>% filter(Distance < 0.25)
otter_isotopes_2_tran_merged_400<-otter_isotopes_2_tran_merged %>% filter(Distance < 0.4)
otter_isotopes_2_tran_merged_500<-otter_isotopes_2_tran_merged %>% filter(Distance < 0.5)
otter_isotopes_2_tran_merged_100<-otter_isotopes_2_tran_merged %>% filter(Distance < 0.1)
otter_isotopes_2_tran_merged_600<-otter_isotopes_2_tran_merged %>% filter(Distance < 0.6)
otter_isotopes_2_tran_merged_750<-otter_isotopes_2_tran_merged %>% filter(Distance < 0.75)
otter_isotopes_2_tran_merged_850<-otter_isotopes_2_tran_merged %>% filter(Distance < 0.85)
otter_isotopes_2_tran_merged_1000<-otter_isotopes_2_tran_merged %>% filter(Distance < 1)

head(otter_isotopes_2_tran_merged)
ggplot(otter_isotopes_2_tran_merged_500, aes(x=d13c_otter, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("100m")+geom_text(aes(label=site.id))

ggplot(otter_isotopes_2_tran_merged, aes(x=d13c_otter, y=MEAN_egarea2k, col=Distance))+geom_point()+geom_smooth(method="lm")


#n15 otters spatial scales
n15_100<-ggplot(otter_isotopes_2_tran_merged_100, aes(x=d15n_otter, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")+ggtitle("100m")
n15_250<-ggplot(otter_isotopes_2_tran_merged_250, aes(x=d15n_otter, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")+ggtitle("250m")
n15_300<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=d15n_otter, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")+ggtitle("300m")
n15_400<-ggplot(otter_isotopes_2_tran_merged_400, aes(x=d15n_otter, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")+ggtitle("400m")
n15_500<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d15n_otter, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")+ggtitle("500m")
n15_600<-ggplot(otter_isotopes_2_tran_merged_600, aes(x=d15n_otter, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")+ggtitle("600m")
n15_750<-ggplot(otter_isotopes_2_tran_merged_750, aes(x=d15n_otter, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")+ggtitle("750m")
n15_850<-ggplot(otter_isotopes_2_tran_merged_850, aes(x=d15n_otter, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")+ggtitle("850m")
n15_1000<-ggplot(otter_isotopes_2_tran_merged_1000, aes(x=d15n_otter, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")+ggtitle("1000m")
plot_grid(n15_100, n15_250, n15_300, n15_400, n15_500,n15_600,n15_750,n15_850, n15_1000, ncol=3)

n15_100<-ggplot(otter_isotopes_2_tran_merged_100, aes(x=d13c_otter, y=MEAN_kparea100))+geom_point()+geom_smooth(method="lm")+ggtitle("100m")
n15_250<-ggplot(otter_isotopes_2_tran_merged_250, aes(x=d13c_otter, y=MEAN_kparea250))+geom_point()+geom_smooth(method="lm")+ggtitle("250m")
n15_300<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=d13c_otter, y=MEAN_kparea250))+geom_point()+geom_smooth(method="lm")+ggtitle("300m")
n15_400<-ggplot(otter_isotopes_2_tran_merged_400, aes(x=d13c_otter, y=MEAN_kparea500))+geom_point()+geom_smooth(method="lm")+ggtitle("400m")
n15_500<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d13c_otter, y=MEAN_kparea500))+geom_point()+geom_smooth(method="lm")+ggtitle("500m")
n15_600<-ggplot(otter_isotopes_2_tran_merged_600, aes(x=d13c_otter, y=MEAN_kparea500))+geom_point()+geom_smooth(method="lm")+ggtitle("600m")
n15_750<-ggplot(otter_isotopes_2_tran_merged_750, aes(x=d13c_otter, y=MEAN_kparea1k))+geom_point()+geom_smooth(method="lm")+ggtitle("750m")
n15_850<-ggplot(otter_isotopes_2_tran_merged_850, aes(x=d13c_otter, y=MEAN_kparea1k))+geom_point()+geom_smooth(method="lm")+ggtitle("850m")
n15_1000<-ggplot(otter_isotopes_2_tran_merged_1000, aes(x=d13c_otter, y=MEAN_kparea1k))+geom_point()+geom_smooth(method="lm")+ggtitle("1000m")
plot_grid(n15_100, n15_250, n15_300, n15_400, n15_500,n15_600,n15_750,n15_850, n15_1000, ncol=3)

ggplot(otter_isotopes_2_tran_merged_300, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+geom_text(aes(label=site))


#soil chemistry
marine_d15n<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d15n_otter, y=d15n))+geom_point()+geom_smooth(method="lm")
marine_d13c<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d13c_otter, y=d13c))+geom_point()+geom_smooth(method="lm")
marine_c<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=c_otter, y=c))+geom_point()+geom_smooth(method="lm")
marine_n<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=n_otter, y=n))+geom_point()+geom_smooth(method="lm")
marine_cn<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=cn_otter, y=cn))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_d15n, marine_d13c, marine_c, marine_n, marine_cn,ncol=3)
ggsave("C:Plots//Otter//soilchem_otterchem.png", width=40, height=20, unit="cm")


#salal chemistry 
gash_d15n<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d15n_otter, y=d15n_gash))+geom_point()+geom_smooth(method="lm")
gash_d13c<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d13c_otter, y=d13c_gash))+geom_point()+geom_smooth(method="lm")
gash_c<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=c_otter, y=c_gash))+geom_point()+geom_smooth(method="lm")
gash_n<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=n_otter, y=n_gash))+geom_point()+geom_smooth(method="lm")
gash_cn<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=cn_otter, y=cn_gash))+geom_point()+geom_smooth(method="lm")
plot_grid(gash_d15n, gash_d13c, gash_c, gash_n, gash_cn, ncol=3)
ggsave("C:Plots//Otter//otter_gash.png", width=40, height=20, unit="cm")

#myanthemum chemistry
midi_d15n<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d15n_otter, y=d15n_midi))+geom_point()+geom_smooth(method="lm")
midi_d13c<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d13c_otter, y=d13c_midi))+geom_point()+geom_smooth(method="lm")
midi_c<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=c_otter, y=c_midi))+geom_point()+geom_smooth(method="lm")
midi_n<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=n_otter, y=n_midi))+geom_point()+geom_smooth(method="lm")
midi_cn<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=cn_otter, y=cn_midi))+geom_point()+geom_smooth(method="lm")
plot_grid(midi_d15n, midi_d13c, midi_c, midi_n, midi_cn, ncol=3)
ggsave("C:Plots//Otter//otter_midi.png", width=40, height=20, unit="cm")


#beetles chemistry
beetles_d15n<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d15n_otter, y=d15n_beetles))+geom_point()+geom_smooth(method="lm")
beetles_d13c<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d13c_otter, y=d13c_beetles))+geom_point()+geom_smooth(method="lm")
beetles_c<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=c_otter, y=c_beetles))+geom_point()+geom_smooth(method="lm")
beetles_n<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=n_otter, y=n_beetles))+geom_point()+geom_smooth(method="lm")
beetles_cn<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=cn_otter, y=cn_beetles))+geom_point()+geom_smooth(method="lm")
plot_grid(beetles_d15n, beetles_d13c, beetles_c, beetles_n, beetles_cn, ncol=3)
ggsave("C:Plots//Otter//otter_beetles.png", width=40, height=20, unit="cm")

#isopods chemistry
isopods_d15n<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d15n_otter, y=d15n_isopods))+geom_point()+geom_smooth(method="lm")
isopods_d13c<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d13c_otter, y=d13c_isopods))+geom_point()+geom_smooth(method="lm")
isopods_c<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=c_otter, y=c_isopods))+geom_point()+geom_smooth(method="lm")
isopods_n<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=n_otter, y=n_isopods))+geom_point()+geom_smooth(method="lm")
isopods_cn<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=cn_otter, y=cn_isopods))+geom_point()+geom_smooth(method="lm")
plot_grid(isopods_d15n, isopods_d13c, isopods_c, isopods_n, isopods_cn, ncol=3)
ggsave("C:Plots//Otter//otter_isopods.png", width=40, height=20, unit="cm")


#weevils chemistry
weevils_d15n<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d15n_otter, y=d15n_weevils))+geom_point()+geom_smooth(method="lm")
weevils_d13c<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=d13c_otter, y=d13c_weevils))+geom_point()+geom_smooth(method="lm")
weevils_c<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=c_otter, y=c_weevils))+geom_point()+geom_smooth(method="lm")
weevils_n<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=n_otter, y=n_weevils))+geom_point()+geom_smooth(method="lm")
weevils_cn<-ggplot(otter_isotopes_2_tran_merged_500, aes(x=cn_otter, y=cn_weevils))+geom_point()+geom_smooth(method="lm")
plot_grid(weevils_d15n, weevils_d13c, weevils_c, weevils_n, weevils_cn, ncol=3)
ggsave("C:Plots//Otter//otter_weevils.png", width=40, height=20, unit="cm")


#plant, tree, insect abundance
marine_plant<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=marine_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_pc1<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=marine_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")
marine_tree<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=marine_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_treeabun<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=marine_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
marine_insect<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=marine_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_insectabund<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=marine_richness_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_plant,marine_pc1, marine_tree,marine_treeabun,marine_insect,marine_insectabund,ncol=3)
ggsave("C:Plots//Otter//Resources_terr_var//marine_richness_terrestrial.png", width=40, height=20, unit="cm")


# mammal, bird, habitat het, NDVI
marine_bird<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=marine_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_habitathet<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=marine_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
marine_NDVI<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=marine_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
marine_mammal<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=marine_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")




# Plotting marine resources vs. marine variables----------------------------------------------------------------

setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Biodiversity idea")


ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")




#Just marine variables on their own
marine1<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=fish_richness_corrected, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine2<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_abundance_corrected), y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine3<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=fish_richness_corrected, y=log(fish_abundance_corrected)))+geom_point()+geom_smooth(method="lm")
marine4<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_richness_corrected, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine16<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=wrack_richness, y=log_site_sum_by_isl))+geom_point()+geom_smooth(method="lm")
marine17<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=fish_richness_corrected, y=fish_length))+geom_point()+geom_smooth(method="lm")
plot_grid(marine1, marine2, marine3, marine4,marine16,marine17, ncol=3)
ggsave("C:Plots//Otter//Resources_marine_var//marine_plot.png")


#nearby habitat abundance on fish and bycatch
marine6<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log_HAB2000, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine7<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log_HAB2000, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine8<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log_HAB2000, y=log(fish_abundance_corrected)))+geom_point()+geom_smooth(method="lm")
marine9<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log_HAB2000, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine10<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log_HAB2000, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine11<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log_HAB2000, y=log_site_sum_by_isl))+geom_point()+geom_smooth(method="lm")
plot_grid(marine6, marine8, marine11, marine7,marine9,marine10, ncol=3)
ggsave("C:Plots//Otter//Resources_marine_var//hab2000_fish_bycatch_corrected.png")



marine_hab_1006<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(sum_100m), y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_1007<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(sum_100m), y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_1008<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(sum_100m), y=fish_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_1009<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(sum_100m), y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_10010<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(sum_100m), y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_10011<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(sum_100m), y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_1006, marine_hab_1008, marine_hab_10011, marine_hab_1007,marine_hab_1009,marine_hab_10010, ncol=3)
ggsave("C:Plots//Otter//Resources_marine_var//hab100_fish_bycatch_corrected.png")

marine_hab_5006<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=sum_500m, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_5007<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=sum_500m, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_5008<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=sum_500m, y=fish_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_5009<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=sum_500m, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50010<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=sum_500m, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50011<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=sum_500m, y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_5006, marine_hab_5008, marine_hab_50011, marine_hab_5007,marine_hab_5009,marine_hab_50010, ncol=3)
ggsave("C:Plots//Otter//Resources_marine_var//hab500_fish_bycatch_corrected.png")


marine_hab_5006<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=MEAN_egarea250, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_5007<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=MEAN_egarea250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_5008<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=MEAN_egarea250, y=fish_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_5009<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=MEAN_egarea250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50010<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=MEAN_egarea250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
plot_grid(marine_hab_5006, marine_hab_5008, marine_hab_5007,marine_hab_5009,marine_hab_50010, ncol=3)
ggsave("C:Plots//Otter//Resources_marine_var//hab500_fish_bycatch_corrected.png")


head(otter_isotopes_2_tran_merged_300)
marine_hab_kelp6<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=MEAN_kparea250, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_kelp7<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=MEAN_kparea250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_kelp8<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=MEAN_kparea250, y=fish_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_kelp9<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=MEAN_kparea250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_kelp10<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=MEAN_kparea250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
plot_grid(marine_hab_kelp6, marine_hab_kelp8, marine_hab_kelp7,marine_hab_kelp9,marine_hab_kelp10, ncol=3)
ggsave("C:Plots//Otter//Resources_marine_var//habkelp_fish_bycatch_corrected.png")






marine_hab_2506<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=sum_250m, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_2507<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=sum_250m, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_2508<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=sum_250m, y=fish_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_2509<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=sum_250m, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_25010<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=sum_250m, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_25011<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=sum_250m, y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_2506, marine_hab_2508, marine_hab_25011, marine_hab_2507,marine_hab_2509,marine_hab_25010, ncol=3)
ggsave("C:Plots//Otter//Resources_marine_var//hab250_fish_bycatch_corrected.png")


veg_marine1<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=bycatch_abundance_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine2<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=bycatch_richness_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine3<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=log(fish_abundance_corrected)))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine4<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=fish_richness_corrected))+geom_boxplot()+ theme(legend.position=c(0.9,0.90))+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine5<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=log(fish_biomass)))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine6<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=fish_length))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
plot_grid(veg_marine4, veg_marine5, veg_marine6, veg_marine3, veg_marine2, veg_marine1, ncol=3)
ggsave("C:Plots//Otter//Resources_marine_var//resource_primary_intertidal_habitat.png")


subtidal_marine1<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=bycatch_abundance_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine2<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=bycatch_richness_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine3<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=log(fish_abundance_corrected)))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine4<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=fish_richness_corrected))+geom_boxplot()+ theme(legend.position=c(0.9,0.90))+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine5<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=log(fish_biomass)))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine6<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=fish_length))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
plot_grid(subtidal_marine4, subtidal_marine5, subtidal_marine6, subtidal_marine3, subtidal_marine2, subtidal_marine1, ncol=3)
ggsave("C:Plots//Otter//Resources_marine_var//resource_primary_subtidal_habitat.png")



# Plotting Marine resources vs. terrestrial variables  ---------------------------------------

# marine resources vs. n15 soil
n_marine1<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
n_marine2<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
n_marine3<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=log(fish_abundance_corrected)))+geom_point()+geom_smooth(method="lm")
n_marine4<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
n_marine5<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=log(fish_biomass_corrected)))+geom_point()+geom_smooth(method="lm")
n_marine6<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=fish_length))+geom_point()+geom_smooth(method="lm")
n_marine7<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")
plot_grid(n_marine4, n_marine5, n_marine7, n_marine3, n_marine2, n_marine1, ncol=3)
ggsave("C:Plots//Otter//Resources_terr_var//d15n_marine_corrected.png")


ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=marine_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))



####GAM marine resources vs. n15n
n_marine_gam1<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam2<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam3<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=log(fish_abundance_corrected)))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam4<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam5<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=log(fish_biomass_corrected)))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam6<-ggplot(otter_isotopes_2_tran_merged_300, aes(y=d15n, x=fish_length))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
plot_grid(n_marine_gam4, n_marine_gam5, n_marine_gam6, n_marine_gam3, n_marine_gam2, n_marine_gam1, ncol=3)
ggsave("C:Plots//Otter//Resources_terr_var//d15n_marine_corrected_GAM.png")


#fish abundance
fish1<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_abundance_corrected), y=d15n))+geom_point()+geom_smooth(method="lm")
fish2<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_abundance_corrected), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish3<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_abundance_corrected), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish4<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_abundance_corrected), y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish13<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_abundance_corrected), y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish19<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_abundance_corrected), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish20<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_abundance_corrected), y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish1,fish2,fish3,fish4,fish13,fish19,fish20, ncol=4)
ggsave("C:Plots//Otter//Resources_terr_var//fish_abund_terrestrial.png")

#fish richness
fish5<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish6<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish7<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=fish_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish8<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
fish14<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish21<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish22<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish8,fish5,fish6,fish7,fish14,fish21,fish22, ncol=4)
ggsave("C:Plots//Otter//Resources_terr_var//fish_richness_terrestrial.png")

#fish biomass
fish9<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_biomass_corrected), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish10<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_biomass_corrected), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish11<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_biomass_corrected), y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish12<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_biomass_corrected), y=d15n))+geom_point()+geom_smooth(method="lm")
fish15<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_biomass_corrected), y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish23<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_biomass_corrected), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish24<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log(fish_biomass_corrected), y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish12, fish9,fish10,fish11,fish15,fish23,fish24, ncol=4)
ggsave("C:Plots//Otter//Resources_terr_var//fish_biomass_corrected_terrestrial.png")

###bycatch richness and abundance
bycatch1<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
bycatch2<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch3<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch4<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
bycatch10<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
bycatch12<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
bycatch13<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch14<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
bycatch19<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch20<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_richness_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(bycatch1,bycatch2,bycatch3,bycatch4,bycatch10,bycatch12, bycatch13,bycatch14,bycatch19,bycatch20, ncol=5)
ggsave("C:Plots//Otter//Resources_terr_var//bycatch_richness_terrestrial.png", width=40, height=20, unit="cm")


bycatch5<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_abundance_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
bycatch6<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_abundance_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch7<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_abundance_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch8<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_abundance_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
bycatch9<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_abundance_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
bycatch11<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_abundance_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
bycatch15<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_abundance_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch16<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_abundance_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
bycatch17<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_abundance_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch18<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=bycatch_abundance_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(bycatch5,bycatch6,bycatch7,bycatch8,bycatch9,bycatch11,bycatch15,bycatch16,bycatch17,bycatch18,ncol=5)
ggsave("C:Plots//Otter//Resources_terr_var//bycatch_abundance_terrestrial.png", width=40, height=20, unit="cm")



# Plotting Marine resources vs. biogeography ----------------------------


### Marine resources vs. Island area
A_marine1<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log_Area, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
A_marine2<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log_Area, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
A_marine3<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log_Area, y=log(fish_abundance_corrected)))+geom_point()+geom_smooth(method="lm")
A_marine4<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log_Area, y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
A_marine5<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log_Area, y=log(fish_biomass_corrected)))+geom_point()+geom_smooth(method="lm")
A_marine6<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=log_Area, y=fish_length))+geom_point()+geom_smooth(method="lm")
plot_grid(A_marine4, A_marine5, A_marine6, A_marine3, A_marine2, A_marine1, ncol=3)
ggsave("C:Plots//Otter//Resources_biogeog//Area_marine.png")



###Coverage of neighbouring land mass, LOW NEighb_250 = high exposure
neib1<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=Neighb_250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
neib2<-ggplot(otter_isotopes_2_tran_merged_300, aes(x=Neighb_250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
neib4<- ggplot(otter_isotopes_2_tran_merged_300, aes(x=Neighb_250, y=log(fish_abundance_corrected)))+geom_point()+geom_smooth(method="lm")
neib5<- ggplot(otter_isotopes_2_tran_merged_300, aes(x=Neighb_250, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
plot_grid(neib1,neib2,neib4,neib5, ncol=2)
ggsave("C:Plots//Otter//Resources_biogeog//Neighb250_marine.png")


###########3
# d15n bycatch ---------------------------------------------------------------
otter_isotopes_2_tran_merged_300_zscores<-otter_isotopes_2_tran_merged_300
otter_isotopes_2_tran_merged_300_zscores$bycatch_richness_corrected<-scale(otter_isotopes_2_tran_merged_300$bycatch_richness_corrected, center=TRUE, scale=TRUE)
otter_isotopes_2_tran_merged_300_zscores$bycatch_richness_corrected.unscaled <-otter_isotopes_2_tran_merged_300_zscores$bycatch_richness_corrected * attr(otter_isotopes_2_tran_merged_300_zscores$bycatch_richness_corrected, 'scaled:scale') + attr(otter_isotopes_2_tran_merged_300_zscores$bycatch_richness_corrected, 'scaled:center')
otter_isotopes_2_tran_merged_300_zscores$bycatch_richness_corrected<-as.numeric(otter_isotopes_2_tran_merged_300_zscores$bycatch_richness_corrected)
ggplot(otter_isotopes_2_tran_merged_300_zscores, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")

str(otter_isotopes_2_tran_merged_300_zscores)

qqp(otter_isotopes_2_tran_merged_300_zscores$d15n)
qqp(otter_isotopes_2_tran_merged_300_zscores$d15n, "lnorm")

lm.d15n.bycatch<-lm(d15n ~ bycatch_richness_corrected, data=otter_isotopes_2_tran_merged_300_zscores)
gam.lm.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = otter_isotopes_2_tran_merged_300_zscores, select=TRUE, method="REML")
gam.loglink.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = otter_isotopes_2_tran_merged_300_zscores, family = gaussian(link="log"), select=TRUE, method="REML")

gam.gamma.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = otter_isotopes_2_tran_merged_300_zscores, family = Gamma, select=TRUE, method="REML")
gam.tweedie.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = otter_isotopes_2_tran_merged_300_zscores, family = tw, select=TRUE, method="REML")


AICtab( lm.d15n.bycatch,  gam.lm.d15n.bycatch, gam.loglink.d15n.bycatch, gam.gamma.d15n.bycatch, gam.tweedie.d15n.bycatch)

#gam.lm.d15n.bycatch


plot(gam.gamma.d15n.bycatch, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
appraise(gam.gamma.d15n.bycatch)
qq_plot(gam.gamma.d15n.bycatch, method = 'simulate')
k.check(gam.gamma.d15n.bycatch)
summary(gam.gamma.d15n.bycatch)



fam.gam.d15n.bycatch <- family(gam.gamma.d15n.bycatch)
fam.gam.d15n.bycatch
str(fam.gam.d15n.bycatch)
ilink.gam.d15n.bycatch<- fam.gam.d15n.bycatch$linkinv
ilink.gam.d15n.bycatch


mod.d15n.bycatch<-gam.gamma.d15n.bycatch
ndata.d15n.bycatch <- with(otter_isotopes_2_tran_merged_300_zscores, data_frame(bycatch_richness_corrected = seq(min(bycatch_richness_corrected), max(bycatch_richness_corrected),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.d15n.bycatch <- add_column(ndata.d15n.bycatch, fit = predict(mod.d15n.bycatch, newdata = ndata.d15n.bycatch, type = 'response'))

predict(mod.d15n.bycatch, newdata = ndata.d15n.bycatch, type = 'response')
ndata.d15n.bycatch <- bind_cols(ndata.d15n.bycatch, setNames(as_tibble(predict(mod.d15n.bycatch, ndata.d15n.bycatch, se.fit = TRUE)[1:2]),
                                                             c('fit_link','se_link')))

## create the interval and backtransform

ndata.d15n.bycatch <- mutate(ndata.d15n.bycatch,
                             fit_resp  = ilink.gam.d15n.bycatch(fit_link),
                             right_upr = ilink.gam.d15n.bycatch(fit_link + (2 * se_link)),
                             right_lwr = ilink.gam.d15n.bycatch(fit_link - (2 * se_link)))

otter_isotopes_2_tran_merged_300_zscores$bycatch_richness_corrected<-scale(otter_isotopes_2_tran_merged_300$bycatch_richness_corrected, center=TRUE, scale=TRUE)

ndata.d15n.bycatch$bycatch_richness_corrected.unscaled<-ndata.d15n.bycatch$bycatch_richness_corrected * attr(otter_isotopes_2_tran_merged_300_zscores$bycatch_richness_corrected, 'scaled:scale') + attr(otter_isotopes_2_tran_merged_300_zscores$bycatch_richness_corrected, 'scaled:center')


# plot 
plt.d15n.bycatch <- ggplot(ndata.d15n.bycatch, aes(x = bycatch_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = otter_isotopes_2_tran_merged_300_zscores)+
  xlab(expression("Bycatch richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.bycatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.bycatch
ggsave("C:Plots//Otter//d15n_bycatch_400m.png")



# d15n GAM marine richness ------------------------------------------------

library(tidyr)
library(bbmle) 
library(glmmTMB)
library(doBy)
library(plyr)
require(dplyr)
library(ggplot2) 
library(doBy)
library(grid)
library(glmmADMB)
library(betareg)
library(lmtest)
library(fitdistrplus)
library(visreg)
library(lme4)
library(coefplot)
library(arm)
library(lmerTest)
library(boot)
library(MASS)
require(scales)
library(car)
library(knitr)
library(tidyverse)
library(kableExtra)
library(multcomp)
library(arm) ## for sim()
library(descr)  ## for LogRegR2
require(reshape2)
library(ggplot2)
library(grid)
library(DHARMa)
library(gap)
library(qrnn)
library(mgcv)
library(colorspace)
library(gratia)
library(cowplot)




#### marine total catch

otter_isotopes_2_tran_merged_300_zscores<-otter_isotopes_2_tran_merged_300
otter_isotopes_2_tran_merged_300_zscores$marine_richness_corrected<-scale(otter_isotopes_2_tran_merged_300$marine_richness_corrected, center=TRUE, scale=TRUE)
otter_isotopes_2_tran_merged_300_zscores$marine_richness_corrected.unscaled <-otter_isotopes_2_tran_merged_300_zscores$marine_richness_corrected * attr(otter_isotopes_2_tran_merged_300_zscores$marine_richness_corrected, 'scaled:scale') + attr(otter_isotopes_2_tran_merged_300_zscores$marine_richness_corrected, 'scaled:center')
otter_isotopes_2_tran_merged_300_zscores$marine_richness_corrected<-as.numeric(otter_isotopes_2_tran_merged_300_zscores$marine_richness_corrected)
ggplot(otter_isotopes_2_tran_merged_300_zscores, aes(y=d15n, x=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

qqp(otter_isotopes_2_tran_merged_300_zscores$d15n)
qqp(otter_isotopes_2_tran_merged_300_zscores$d15n, "lnorm")

lm.d15n.marinecatch<-lm(d15n ~ marine_richness_corrected, data=otter_isotopes_2_tran_merged_300_zscores)
gam.lm.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = otter_isotopes_2_tran_merged_300_zscores, select=TRUE, method="REML")
gam.loglink.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = otter_isotopes_2_tran_merged_300_zscores, family = gaussian(link="log"), select=TRUE, method="REML")

gam.gamma.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = otter_isotopes_2_tran_merged_300_zscores, family = Gamma, select=TRUE, method="REML")
gam.tweedie.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = otter_isotopes_2_tran_merged_300_zscores, family = tw, select=TRUE, method="REML")


AICtab( lm.d15n.marinecatch,  gam.lm.d15n.marinecatch, gam.loglink.d15n.marinecatch, gam.gamma.d15n.marinecatch, gam.tweedie.d15n.marinecatch)

#gam.lm.d15n.marinecatch


plot(gam.lm.d15n.marinecatch , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
appraise(gam.lm.d15n.marinecatch )
qq_plot(gam.lm.d15n.marinecatch , method = 'simulate')
k.check(gam.lm.d15n.marinecatch )
summary(gam.lm.d15n.marinecatch )



fam.gam.d15n.marinecatch <- family(gam.lm.d15n.marinecatch )
fam.gam.d15n.marinecatch
str(fam.gam.d15n.marinecatch)
ilink.gam.d15n.marinecatch<- fam.gam.d15n.marinecatch$linkinv
ilink.gam.d15n.marinecatch


mod.d15n.marinecatch<-gam.lm.d15n.marinecatch 
ndata.d15n.marinecatch <- with(otter_isotopes_2_tran_merged_300_zscores, data_frame(marine_richness_corrected = seq(min(marine_richness_corrected), max(marine_richness_corrected),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.d15n.marinecatch <- add_column(ndata.d15n.marinecatch, fit = predict(mod.d15n.marinecatch, newdata = ndata.d15n.marinecatch, type = 'response'))

predict(mod.d15n.marinecatch, newdata = ndata.d15n.marinecatch, type = 'response')
ndata.d15n.marinecatch <- bind_cols(ndata.d15n.marinecatch, setNames(as_tibble(predict(mod.d15n.marinecatch, ndata.d15n.marinecatch, se.fit = TRUE)[1:2]),
                                                                     c('fit_link','se_link')))

## create the interval and backtransform

ndata.d15n.marinecatch <- mutate(ndata.d15n.marinecatch,
                                 fit_resp  = ilink.gam.d15n.marinecatch(fit_link),
                                 right_upr = ilink.gam.d15n.marinecatch(fit_link + (2 * se_link)),
                                 right_lwr = ilink.gam.d15n.marinecatch(fit_link - (2 * se_link)))

otter_isotopes_2_tran_merged_300_zscores$marine_richness_corrected<-scale(otter_isotopes_2_tran_merged_300$marine_richness_corrected, center=TRUE, scale=TRUE)

ndata.d15n.marinecatch$marine_richness_corrected.unscaled<-ndata.d15n.marinecatch$marine_richness_corrected * attr(otter_isotopes_2_tran_merged_300_zscores$marine_richness_corrected, 'scaled:scale') + attr(otter_isotopes_2_tran_merged_300_zscores$marine_richness_corrected, 'scaled:center')


# plot 
plt.d15n.marinecatch <- ggplot(ndata.d15n.marinecatch, aes(x = marine_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = otter_isotopes_2_tran_merged_300_zscores)+
  xlab(expression("Marine catch richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.marinecatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.marinecatch
ggsave("C:Plots//Otter//d15n_bycatch.png")


# d15n fish richness ------------------------------------------------------


otter_isotopes_2_tran_merged_300_zscores<-otter_isotopes_2_tran_merged_300
otter_isotopes_2_tran_merged_300_zscores$fish_richness_corrected<-scale(otter_isotopes_2_tran_merged_300$fish_richness_corrected, center=TRUE, scale=TRUE)
otter_isotopes_2_tran_merged_300_zscores$fish_richness_corrected.unscaled <-otter_isotopes_2_tran_merged_300_zscores$fish_richness_corrected * attr(otter_isotopes_2_tran_merged_300_zscores$fish_richness_corrected, 'scaled:scale') + attr(otter_isotopes_2_tran_merged_300_zscores$fish_richness_corrected, 'scaled:center')
otter_isotopes_2_tran_merged_300_zscores$fish_richness_corrected<-as.numeric(otter_isotopes_2_tran_merged_300_zscores$fish_richness_corrected)
ggplot(otter_isotopes_2_tran_merged_300_zscores, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")

str(otter_isotopes_2_tran_merged_300_zscores)

qqp(otter_isotopes_2_tran_merged_300_zscores$d15n)
qqp(otter_isotopes_2_tran_merged_300_zscores$d15n, "lnorm")

gamma.12.fish_richness_corrected<-fitdistr(otter_isotopes_2_tran_merged_300$fish_richness_corrected+0.01, "gamma")
qqp(otter_isotopes_2_tran_merged_300$fish_richness_corrected, "gamma", shape = gamma.12.fish_richness_corrected$estimate[[1]], rate = gamma.12.fish_richness_corrected$estimate[[2]])

lm.d15n.fish<-lm(d15n ~ fish_richness_corrected, data=otter_isotopes_2_tran_merged_300_zscores)
gam.lm.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = otter_isotopes_2_tran_merged_300_zscores, select=TRUE, method="REML")
gam.loglink.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = otter_isotopes_2_tran_merged_300_zscores, family = gaussian(link="log"), select=TRUE, method="REML")
gam.gamma.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = otter_isotopes_2_tran_merged_300_zscores, family = Gamma, select=TRUE, method="REML")
gam.tweedie.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = otter_isotopes_2_tran_merged_300_zscores, family = tw, select=TRUE, method="REML")


AICtab( gam.gamma.d15n.fish,  lm.d15n.fish, gam.loglink.d15n.fish, gam.lm.d15n.fish, gam.tweedie.d15n.fish)

#gam.gam.gamma.d15n.fish


plot(gam.gamma.d15n.fish, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
appraise(gam.gamma.d15n.fish)
qq_plot(gam.gamma.d15n.fish, method = 'simulate')
k.check(gam.gamma.d15n.fish)
summary(gam.gamma.d15n.fish)



fam.gam.d15n.fish <- family(gam.gamma.d15n.fish)
fam.gam.d15n.fish
str(fam.gam.d15n.fish)
ilink.gam.d15n.fish<- fam.gam.d15n.fish$linkinv
ilink.gam.d15n.fish


mod.d15n.fish<-gam.gamma.d15n.fish
ndata.d15n.fish <- with(otter_isotopes_2_tran_merged_300_zscores, data_frame(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100)))


## add the fitted values by predicting from the model for the new data
ndata.d15n.fish <- add_column(ndata.d15n.fish, fit = predict(mod.d15n.fish, newdata = ndata.d15n.fish, type = 'response'))

predict(mod.d15n.fish, newdata = ndata.d15n.fish, type = 'response')
ndata.d15n.fish <- bind_cols(ndata.d15n.fish, setNames(as_tibble(predict(mod.d15n.fish, ndata.d15n.fish, se.fit = TRUE)[1:2]),
                                                       c('fit_link','se_link')))

## create the interval and backtransform

ndata.d15n.fish <- mutate(ndata.d15n.fish,
                          fit_resp  = ilink.gam.d15n.fish(fit_link),
                          right_upr = ilink.gam.d15n.fish(fit_link + (2 * se_link)),
                          right_lwr = ilink.gam.d15n.fish(fit_link - (2 * se_link)))

otter_isotopes_2_tran_merged_300_zscores$fish_richness_corrected<-scale(otter_isotopes_2_tran_merged_300$fish_richness_corrected, center=TRUE, scale=TRUE)
ndata.d15n.fish$fish_richness_corrected.unscaled<-ndata.d15n.fish$fish_richness_corrected * attr(otter_isotopes_2_tran_merged_300_zscores$fish_richness_corrected, 'scaled:scale') + attr(otter_isotopes_2_tran_merged_300_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.d15n.fish <- ggplot(ndata.d15n.fish, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = otter_isotopes_2_tran_merged_300_zscores)+
  xlab(expression("fish richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.fish,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.fish
ggsave("C:Plots//Otter//d15n_fish_400.png")





# Correlation -------------------------------------------------------------

which( colnames(otter_isotopes_2_tran_merged_300)=="Exp_SHZN" )
which( colnames(otter_isotopes_2_tran_merged_300)=="lat" )
which( colnames(otter_isotopes_2_tran_merged_300)=="size.cat2" )
which( colnames(otter_isotopes_2_tran_merged_300)=="notes" )

str(otter_isotopes_2_tran_merged_300)
corr_by_isl_selected_fish<-otter_isotopes_2_tran_merged_300 %>% select_if(is.numeric)
str(corr_by_isl_selected_fish)


corr_by_isl_selected_fish_2 <- round(cor(corr_by_isl_selected_fish, use="pairwise.complete.obs"), 1)
head(corr_by_isl_selected_fish_2[, 1:6])
p.mat_by_isl_selected_fish_2 <- cor_pmat(corr_by_isl_selected_fish)
head(p.mat_by_isl_selected_fish_2[, 1:4])

ggcorrplot(corr_by_isl_selected_fish_2)
ggcorrplot(corr_by_isl_selected_fish_2, hc.order = TRUE, type = "lower",
           outline.col = "white")
ggcorrplot(corr_by_isl_selected_fish_2, hc.order = TRUE,
           type = "lower", p.mat = p.mat_by_isl_selected_fish_2)

ggcorrplot(corr_by_isl_selected_fish_2, hc.order = TRUE, type = "lower",
           lab = TRUE)
ggsave("C:Plots//Otter//Resources_terr_var//ggcorrplot_marine_terrestrial.png",  width=40, height=40, unit="cm")

library(purrr)
library(ggcorrplot)
library(corrr)
library(PerformanceAnalytics)


corr_by_isl_selected_fish %>% correlate() %>%  network_plot(min_cor = 0.7)
ggsave("C:Plots//Otter//Resources_terr_var//networkplot_marine_terrestrial_0.7.png")
