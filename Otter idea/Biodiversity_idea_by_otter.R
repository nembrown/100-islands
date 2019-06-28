setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Biodiversity idea")
#change to Norah if on work computer



# Load data and libraries ---------------------------------------------------------------


ben_fish_data<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_speciesabundance_20142018.csv")
hakai_sites_near_islands<-read.csv("C:hakai_sites_near_islands.csv")
by_isl_master<-read.csv("C:by_isl_master.csv")

ben_bycatch_data<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_bycatch_20142018.csv")

ben_size_data<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_specieslength_20142018.csv")

ben_habitat_data<-read.csv("C:Ben.data//beachseine_calvert_NB//hakaiBS_habitat_20142018.csv")

ben_netdimensions<-read.csv("C:Ben.data//beachseine_calvert_NB//netdimensions_nb.csv")



head(ben_netdimensions)
head(ben_fish_data)
head(ben_bycatch_data)
head(hakai_sites_near_islands)



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

theme_set(theme_classic())


#### Net dimensions
ben_netdimensions$area_av<-rowMeans(ben_netdimensions[c('area_set1', 'area_set2')], na.rm=TRUE)
ben_netdimensions_year <-ben_netdimensions %>% group_by(site, year) %>% summarise(mean_area_av = mean(area_av, na.rm=TRUE))
head(ben_netdimensions_year)



# Fish data cleaning ------------------------------------------------------

ben_fish_data_wide_year <-ben_fish_data %>% group_by(site, year, species) %>% 
  summarise(sum_abundance = mean(abundance, na.rm=TRUE)) %>% 
  spread( species, sum_abundance) %>% 
  replace(is.na(.), 0) 

head(ben_fish_data_wide_year)
ben_fish_data_wide_year<-ben_fish_data_wide_year[,-3]

ben_fish_data_wide_year_richness<-ben_fish_data_wide_year[,c(1, 2)]
ben_fish_data_wide_year_richness$fish_richness<-specnumber(ben_fish_data_wide_year[,-c(1,2)])
#ben_fish_data_wide_year_richness$fish_diversity<-diversity(ben_fish_data_wide_year[,-1], index="shannon")
ben_fish_data_wide_year_richness$fish_abundance<-rowSums(ben_fish_data_wide_year[,-c(1,2)],na.rm = TRUE)
head(ben_fish_data_wide_year_richness)



# Bycatch data cleaning ---------------------------------------------------
head(ben_bycatch_data)
names(ben_bycatch_data)[1]<-"site"
ben_bycatch_data[, 7] <- as.numeric(as.character( ben_bycatch_data[, 7] ))

ben_bycatch_data_wide_year <-ben_bycatch_data %>%  replace(is.na(.), 0) %>% group_by(site, year, species) %>% 
  summarise(mean_abundance = mean(estimate, na.rm=TRUE)) %>% 
  spread( species, mean_abundance) %>% 
  replace(is.na(.), 0)

head(ben_bycatch_data_wide_year)
ben_bycatch_data_wide_year<-ben_bycatch_data_wide_year[,-3]

ben_bycatch_data_wide_year_richness<-ben_bycatch_data_wide_year[,c(1,2)]
ben_bycatch_data_wide_year_richness$bycatch_richness<-specnumber(ben_bycatch_data_wide_year[,-c(1,2)])
ben_bycatch_data_wide_year_richness$bycatch_abundance<-rowSums(ben_bycatch_data_wide_year[,-c(1,2)],na.rm = TRUE)
head(ben_bycatch_data_wide_year_richness)



# Fish Biomass data cleaning ----------------------------------------------
str(ben_size_data)

ben_size_data[, 8] <- as.numeric(as.character( ben_size_data[, 8] ))
ben_size_data_wide_year <-ben_size_data %>%  replace(is.na(.), 0) %>% group_by(site, year,species) %>% 
  summarise(fish_length = mean(length, na.rm=TRUE)) %>% 
  spread( species, fish_length) %>% 
  replace(is.na(.), 0)

head(ben_size_data_wide_year)
ben_size_data_wide_year_richness<-ben_size_data_wide_year[,c(1,2)]
ben_size_data_wide_year_richness$size_richness<-specnumber(ben_size_data_wide_year[,-c(1,2)])
ben_size_data_wide_year_richness$fish_length<-rowMeans(ben_size_data_wide_year[,-c(1,2)],na.rm = TRUE)
head(ben_size_data_wide_year_richness)

#ben_fish_data_wide_year<-ben_fish_data_wide_year %>% filter(if (site=="koe3") year !="2018" else year == year)


ben_biomass_data_wide_year<-(ben_size_data_wide_year[,-c(1,2)])*(ben_fish_data_wide_year[,-c(1,2)])
ben_biomass_data_wide_year_nf<-ben_size_data_wide_year
ben_biomass_data_wide_year_nf$site<-ben_size_data_wide_year$site
ben_biomass_data_wide_year_nf$fish_biomass<-rowSums(ben_biomass_data_wide_year)
head(ben_biomass_data_wide_year_nf)

# Merging fish, bycatch, biomass, net dimensions ------------------------------------------


ben_fish_data_wide_year_richness<-ben_fish_data_wide_year_richness%>% unite("site_year", c("site", "year"))
ben_netdimensions_year<- ben_netdimensions_year %>% unite("site_year", c("site", "year"))
ben_bycatch_data_wide_year_richness<- ben_bycatch_data_wide_year_richness %>% unite("site_year", c("site", "year"))
ben_size_data_wide_year_richness<- ben_size_data_wide_year_richness %>% unite("site_year", c("site", "year"))
ben_biomass_data_wide_year_nf<- ben_biomass_data_wide_year_nf %>% unite("site_year", c("site", "year"))

fish_richness_merged_otter_year<-merge(ben_fish_data_wide_year_richness, ben_netdimensions_year, by="site_year", all=TRUE)
fish_bycatch_richness_merged_otter_year<-merge(fish_richness_merged_otter_year, ben_bycatch_data_wide_year_richness, by="site_year", all=TRUE)
fish_bycatch_richness_merged_otter_year<-merge(fish_bycatch_richness_merged_otter_year, ben_size_data_wide_year_richness, by="site_year", all=TRUE)
fish_bycatch_richness_merged_otter_year<-merge(fish_bycatch_richness_merged_otter_year, ben_biomass_data_wide_year_nf[,c(1,97)], by="site_year", all=TRUE)

fish_bycatch_richness_merged_otter_year[is.na(fish_bycatch_richness_merged_otter_year)] <- 0
#taking the bycatch = 0 to NA values

fish_bycatch_richness_merged_otter_year<- fish_bycatch_richness_merged_otter_year %>% separate(site_year, c("site", "year"))
head(fish_bycatch_richness_merged_otter_year)

#correcting richness and abundance for net dimensions x 100 for per 100 m2
fish_bycatch_richness_merged_otter_year$fish_biomass_corrected<-((fish_bycatch_richness_merged_otter_year$fish_biomass)/fish_bycatch_richness_merged_otter_year$mean_area_av)*100
fish_bycatch_richness_merged_otter_year$bycatch_abundance_corrected<-((fish_bycatch_richness_merged_otter_year$bycatch_abundance)/fish_bycatch_richness_merged_otter_year$mean_area_av)*100
fish_bycatch_richness_merged_otter_year$fish_abundance_corrected<-((fish_bycatch_richness_merged_otter_year$fish_abundance)/fish_bycatch_richness_merged_otter_year$mean_area_av)*100
fish_bycatch_richness_merged_otter_year$bycatch_richness_corrected<-((fish_bycatch_richness_merged_otter_year$bycatch_richness)/fish_bycatch_richness_merged_otter_year$mean_area_av)*100
fish_bycatch_richness_merged_otter_year$fish_richness_corrected<-((fish_bycatch_richness_merged_otter_year$fish_richness)/fish_bycatch_richness_merged_otter_year$mean_area_av)*100
fish_bycatch_richness_merged_otter_year$marine_richness_corrected<-(fish_bycatch_richness_merged_otter_year$fish_richness_corrected+fish_bycatch_richness_merged_otter_year$bycatch_richness_corrected)

##### changed 
head(fish_bycatch_richness_merged_otter_year)

fish_bycatch_richness_merged_otter1<- fish_bycatch_richness_merged_otter_year %>% group_by(site) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(fish_bycatch_richness_merged_otter1)
head(fish_bycatch_richness_merged_otter1$site)

#### add it here instead
setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Biodiversity idea")

hakai_sites_distance_otter<-read.csv("C:Distance_btwn_points_otters_beachseine.csv")
hakai_sites_distance_otter<-hakai_sites_distance_otter[,-1]
head(hakai_sites_distance_otter)
names(hakai_sites_distance_otter)[3]<-"unq_otter"
names(hakai_sites_distance_otter)[6]<-"site"
#hakai_sites_distance_otter<- hakai_sites_distance_otter%>% filter(Distance < 1)
head(hakai_sites_distance_otter$site)

fish_bycatch_richness_merged_otter<-merge(fish_bycatch_richness_merged_otter1, hakai_sites_distance_otter[,c(3,5,6)], by="site")
str(fish_bycatch_richness_merged_otter$site)


# Loading and merging otter data ------------------------------------
setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Modelling practice/Norah.data")
otter_isotopes<-read.csv("C:master_otter_isotope_nb_new.csv")
head(otter_isotopes)
#otter_isotopes<- otter_isotopes %>%  filter(site.id != "CVMIL12S8")
names(otter_isotopes)[7]<-"unq_otter"
names(otter_isotopes)[9]<-"otter_lat"
names(otter_isotopes)[10]<-"otter_long"
names(otter_isotopes)[12]<-"otter_notes"


head(fish_bycatch_richness_merged_otter)
head(ben_habitat_data)

fish_richness_merged_otter<-merge(fish_bycatch_richness_merged_otter, otter_isotopes, by="unq_otter")
fish_richness_merged_otter<-merge(fish_richness_merged_otter, ben_habitat_data, by="site")

head(fish_richness_merged_otter)

#this is less than 1km
setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Biodiversity idea")
write.csv(fish_richness_merged_otter, "C:fish_richness_merged_otter.csv")

length(unique(fish_richness_merged_otter$site))
#7 total
length(unique(fish_richness_merged_otter$unq_otter))
#141 data points


fish_richness_merged_otter_300<-fish_richness_merged_otter %>% filter(Distance < 0.3)
fish_richness_merged_otter_350<-fish_richness_merged_otter %>% filter(Distance < 0.35)
fish_richness_merged_otter_250<-fish_richness_merged_otter %>% filter(Distance < 0.25)
fish_richness_merged_otter_400<-fish_richness_merged_otter %>% filter(Distance < 0.4)
fish_richness_merged_otter_500<-fish_richness_merged_otter %>% filter(Distance < 0.5)
fish_richness_merged_otter_100<-fish_richness_merged_otter %>% filter(Distance < 0.1)
fish_richness_merged_otter_600<-fish_richness_merged_otter %>% filter(Distance < 0.6)
fish_richness_merged_otter_750<-fish_richness_merged_otter %>% filter(Distance < 0.75)
fish_richness_merged_otter_850<-fish_richness_merged_otter %>% filter(Distance < 0.85)
fish_richness_merged_otter_1000<-fish_richness_merged_otter %>% filter(Distance < 1)


n15_100<-ggplot(fish_richness_merged_otter_100, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("100 m")
n15_250<-ggplot(fish_richness_merged_otter_250, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("250 m")
n15_300<-ggplot(fish_richness_merged_otter_300, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("300 m")
n15_400<-ggplot(fish_richness_merged_otter_400, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("400 m")
n15_500<-ggplot(fish_richness_merged_otter_500, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("500 m")
n15_600<-ggplot(fish_richness_merged_otter_600, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("600 m")
n15_750<-ggplot(fish_richness_merged_otter_750, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("750 m")
n15_850<-ggplot(fish_richness_merged_otter_850, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("850 m")
n15_1000<-ggplot(fish_richness_merged_otter_1000, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")+ggtitle("1000 m")
plot_grid(n15_100, n15_250, n15_300, n15_400, n15_500,n15_600,n15_750,n15_850, n15_1000, ncol=3)


c13_100<-ggplot(fish_richness_merged_otter_100, aes(x=marine_richness_corrected, y=d13c))+geom_point()+geom_smooth(method="lm")+ggtitle("100 m")
c13_250<-ggplot(fish_richness_merged_otter_250, aes(x=marine_richness_corrected, y=d13c))+geom_point()+geom_smooth(method="lm")+ggtitle("250 m")
c13_300<-ggplot(fish_richness_merged_otter_300, aes(x=marine_richness_corrected, y=d13c))+geom_point()+geom_smooth(method="lm")+ggtitle("300 m")
c13_400<-ggplot(fish_richness_merged_otter_400, aes(x=marine_richness_corrected, y=d13c))+geom_point()+geom_smooth(method="lm")+ggtitle("400 m")
c13_500<-ggplot(fish_richness_merged_otter_500, aes(x=marine_richness_corrected, y=d13c))+geom_point()+geom_smooth(method="lm")+ggtitle("500 m")
c13_600<-ggplot(fish_richness_merged_otter_600, aes(x=marine_richness_corrected, y=d13c))+geom_point()+geom_smooth(method="lm")+ggtitle("600 m")
c13_750<-ggplot(fish_richness_merged_otter_750, aes(x=marine_richness_corrected, y=d13c))+geom_point()+geom_smooth(method="lm")+ggtitle("750 m")
c13_850<-ggplot(fish_richness_merged_otter_850, aes(x=marine_richness_corrected, y=d13c))+geom_point()+geom_smooth(method="lm")+ggtitle("850 m")
c13_1000<-ggplot(fish_richness_merged_otter_1000, aes(x=marine_richness_corrected, y=d13c))+geom_point()+geom_smooth(method="lm")+ggtitle("1000 m")
plot_grid(c13_100, c13_250, c13_300, c13_400, c13_500,c13_600,c13_750,c13_850, c13_1000, ncol=3)


ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_otter, aes(x=intertidal_primary_macroveg, y=d13c, col=intertidal_primary_macroveg))+geom_boxplot()



head(fish_richness_merged_otter)


ggplot(fish_richness_merged_otter, aes(x=d13c, col=fish_richness_corrected, y=d15n))+ geom_point()+  scale_colour_viridis()
ggplot(fish_richness_merged_otter, aes(x=d13c, col=bycatch_richness_corrected, y=d15n))+ geom_point()+  scale_colour_viridis()

ggplot(fish_richness_merged_otter, aes(x=d13c, col=log(fish_biomass_corrected), y=d15n))+ geom_point()+  scale_colour_viridis()
ggplot(fish_richness_merged_otter, aes(x=d13c, col=fish_length, y=d15n))+ geom_point()+  scale_colour_viridis()
ggplot(fish_richness_merged_otter, aes(x=d13c, col=node, y=d15n))+ geom_point()+  scale_colour_viridis_d()


ggplot(fish_richness_merged_otter_1000, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_otter_1000, aes(x=otter_lat, y=d15n))+geom_point()+geom_smooth(method="lm")+geom_text(aes(label=unq_otter))

ggplot(fish_richness_merged_otter, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_otter, aes(x=fish_biomass_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_otter, aes(x=fish_length, y=d15n))+geom_point()+geom_smooth(method="lm")

ggplot(fish_richness_merged_otter, aes(x=size_richness, y=d15n))+geom_point()+geom_smooth(method="lm")

head(fish_richness_merged_otter)

#soil chemistry
marine_d15n<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
marine_d13c<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=d13c))+geom_point()+geom_smooth(method="lm")
marine_c<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=c))+geom_point()+geom_smooth(method="lm")
marine_n<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=n))+geom_point()+geom_smooth(method="lm")
marine_cn<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=cn))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_d15n, marine_d13c, marine_c, marine_n, marine_cn,ncol=3)




ggsave("C:Plots//Transect//Resources_terr_var//marine_richness_soilchem.png", width=40, height=20, unit="cm")


#salal chemistry 
gash_d15n<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=d15n_gash))+geom_point()+geom_smooth(method="lm")
gash_d13c<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=d13c_gash))+geom_point()+geom_smooth(method="lm")
gash_c<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=c_gash))+geom_point()+geom_smooth(method="lm")
gash_n<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=n_gash))+geom_point()+geom_smooth(method="lm")
gash_cn<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=cn_gash))+geom_point()+geom_smooth(method="lm")
gash_s<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=s_gash))+geom_point()+geom_smooth(method="lm")
plot_grid(gash_d15n, gash_d13c, gash_c, gash_n, gash_cn, gash_s,ncol=3)
ggsave("C:Plots//Transect//Resources_terr_var//marine_richness_gash.png", width=40, height=20, unit="cm")

#myanthemum chemistry
midi_d15n<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=d15n_midi))+geom_point()+geom_smooth(method="lm")
midi_d13c<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=d13c_midi))+geom_point()+geom_smooth(method="lm")
midi_c<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=c_midi))+geom_point()+geom_smooth(method="lm")
midi_n<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=n_midi))+geom_point()+geom_smooth(method="lm")
midi_cn<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=cn_midi))+geom_point()+geom_smooth(method="lm")
midi_s<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=s_midi))+geom_point()+geom_smooth(method="lm")
plot_grid(midi_d15n, midi_d13c, midi_c, midi_n, midi_cn, midi_s,ncol=3)
ggsave("C:Plots//Transect//Resources_terr_var//marine_richness_midi.png", width=40, height=20, unit="cm")



#plant, tree, insect abundance
marine_plant<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_pc1<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=pc1))+geom_point()+geom_smooth(method="lm")
marine_tree<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_treeabun<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
marine_insect<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_insectabund<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_plant,marine_pc1, marine_tree,marine_treeabun,marine_insect,marine_insectabund,ncol=3)
ggsave("C:Plots//Transect//Resources_terr_var//marine_richness_terrestrial.png", width=40, height=20, unit="cm")


# mammal, bird, habitat het, NDVI
marine_bird<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_habitathet<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
marine_NDVI<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
marine_mammal<-ggplot(fish_richness_merged_otter, aes(x=marine_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")




# Plotting marine resources vs. marine variables----------------------------------------------------------------

setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Biodiversity idea")


ggplot(fish_richness_merged_otter, aes(y=d15n, x=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")




#Just marine variables on their own
marine1<-ggplot(fish_richness_merged_otter, aes(x=fish_richness_corrected, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine2<-ggplot(fish_richness_merged_otter, aes(x=log(fish_abundance_corrected), y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine3<-ggplot(fish_richness_merged_otter, aes(x=fish_richness_corrected, y=log(fish_abundance_corrected)))+geom_point()+geom_smooth(method="lm")
marine4<-ggplot(fish_richness_merged_otter, aes(x=bycatch_richness_corrected, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine16<-ggplot(fish_richness_merged_otter, aes(x=wrack_richness, y=log_site_sum_by_isl))+geom_point()+geom_smooth(method="lm")
marine17<-ggplot(fish_richness_merged_otter, aes(x=fish_richness_corrected, y=fish_length))+geom_point()+geom_smooth(method="lm")
plot_grid(marine1, marine2, marine3, marine4,marine16,marine17, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//marine_plot.png")


#nearby habitat abundance on fish and bycatch
marine6<-ggplot(fish_richness_merged_otter, aes(x=log_HAB2000, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine7<-ggplot(fish_richness_merged_otter, aes(x=log_HAB2000, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine8<-ggplot(fish_richness_merged_otter, aes(x=log_HAB2000, y=log(fish_abundance_corrected)))+geom_point()+geom_smooth(method="lm")
marine9<-ggplot(fish_richness_merged_otter, aes(x=log_HAB2000, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine10<-ggplot(fish_richness_merged_otter, aes(x=log_HAB2000, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine11<-ggplot(fish_richness_merged_otter, aes(x=log_HAB2000, y=log_site_sum_by_isl))+geom_point()+geom_smooth(method="lm")
plot_grid(marine6, marine8, marine11, marine7,marine9,marine10, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//hab2000_fish_bycatch_corrected.png")



marine_hab_1006<-ggplot(fish_richness_merged_otter, aes(x=log(sum_100m), y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_1007<-ggplot(fish_richness_merged_otter, aes(x=log(sum_100m), y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_1008<-ggplot(fish_richness_merged_otter, aes(x=log(sum_100m), y=fish_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_1009<-ggplot(fish_richness_merged_otter, aes(x=log(sum_100m), y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_10010<-ggplot(fish_richness_merged_otter, aes(x=log(sum_100m), y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_10011<-ggplot(fish_richness_merged_otter, aes(x=log(sum_100m), y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_1006, marine_hab_1008, marine_hab_10011, marine_hab_1007,marine_hab_1009,marine_hab_10010, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//hab100_fish_bycatch_corrected.png")

marine_hab_5006<-ggplot(fish_richness_merged_otter, aes(x=sum_500m, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_5007<-ggplot(fish_richness_merged_otter, aes(x=sum_500m, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_5008<-ggplot(fish_richness_merged_otter, aes(x=sum_500m, y=fish_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_5009<-ggplot(fish_richness_merged_otter, aes(x=sum_500m, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50010<-ggplot(fish_richness_merged_otter, aes(x=sum_500m, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50011<-ggplot(fish_richness_merged_otter, aes(x=sum_500m, y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_5006, marine_hab_5008, marine_hab_50011, marine_hab_5007,marine_hab_5009,marine_hab_50010, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//hab500_fish_bycatch_corrected.png")


marine_hab_5006<-ggplot(fish_richness_merged_otter, aes(x=MEAN_egarea250, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_5007<-ggplot(fish_richness_merged_otter, aes(x=MEAN_egarea250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_5008<-ggplot(fish_richness_merged_otter, aes(x=MEAN_egarea250, y=fish_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_5009<-ggplot(fish_richness_merged_otter, aes(x=MEAN_egarea250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_50010<-ggplot(fish_richness_merged_otter, aes(x=MEAN_egarea250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
plot_grid(marine_hab_5006, marine_hab_5008, marine_hab_5007,marine_hab_5009,marine_hab_50010, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//hab500_fish_bycatch_corrected.png")


head(fish_richness_merged_otter)
marine_hab_kelp6<-ggplot(fish_richness_merged_otter, aes(x=MEAN_kparea250, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_kelp7<-ggplot(fish_richness_merged_otter, aes(x=MEAN_kparea250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_kelp8<-ggplot(fish_richness_merged_otter, aes(x=MEAN_kparea250, y=fish_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_kelp9<-ggplot(fish_richness_merged_otter, aes(x=MEAN_kparea250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_kelp10<-ggplot(fish_richness_merged_otter, aes(x=MEAN_kparea250, y=marine_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
plot_grid(marine_hab_kelp6, marine_hab_kelp8, marine_hab_kelp7,marine_hab_kelp9,marine_hab_kelp10, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//habkelp_fish_bycatch_corrected.png")






marine_hab_2506<-ggplot(fish_richness_merged_otter, aes(x=sum_250m, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_2507<-ggplot(fish_richness_merged_otter, aes(x=sum_250m, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_2508<-ggplot(fish_richness_merged_otter, aes(x=sum_250m, y=fish_abundance_corrected))+geom_point()+geom_smooth(method="lm")
marine_hab_2509<-ggplot(fish_richness_merged_otter, aes(x=sum_250m, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_25010<-ggplot(fish_richness_merged_otter, aes(x=sum_250m, y=wrack_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
marine_hab_25011<-ggplot(fish_richness_merged_otter, aes(x=sum_250m, y=site_mean_by_tran))+geom_point()+geom_smooth(method="lm")
plot_grid(marine_hab_2506, marine_hab_2508, marine_hab_25011, marine_hab_2507,marine_hab_2509,marine_hab_25010, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//hab250_fish_bycatch_corrected.png")


veg_marine1<-ggplot(fish_richness_merged_otter, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=bycatch_abundance_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine2<-ggplot(fish_richness_merged_otter, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=bycatch_richness_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine3<-ggplot(fish_richness_merged_otter, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=log(fish_abundance_corrected)))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine4<-ggplot(fish_richness_merged_otter, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=fish_richness_corrected))+geom_boxplot()+ theme(legend.position=c(0.9,0.90))+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine5<-ggplot(fish_richness_merged_otter, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=log(fish_biomass)))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
veg_marine6<-ggplot(fish_richness_merged_otter, aes(x=intertidal_primary_macroveg, col=intertidal_primary_macroveg, y=fish_length))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
plot_grid(veg_marine4, veg_marine5, veg_marine6, veg_marine3, veg_marine2, veg_marine1, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//resource_primary_intertidal_habitat.png")


subtidal_marine1<-ggplot(fish_richness_merged_otter, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=bycatch_abundance_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine2<-ggplot(fish_richness_merged_otter, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=bycatch_richness_corrected))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine3<-ggplot(fish_richness_merged_otter, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=log(fish_abundance_corrected)))+geom_boxplot()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine4<-ggplot(fish_richness_merged_otter, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=fish_richness_corrected))+geom_boxplot()+ theme(legend.position=c(0.9,0.90))+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine5<-ggplot(fish_richness_merged_otter, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=log(fish_biomass)))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
subtidal_marine6<-ggplot(fish_richness_merged_otter, aes(x=subtidal_primary_macroveg, col=subtidal_primary_macroveg, y=fish_length))+geom_point()+ theme(legend.position="none")+  scale_colour_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE)
plot_grid(subtidal_marine4, subtidal_marine5, subtidal_marine6, subtidal_marine3, subtidal_marine2, subtidal_marine1, ncol=3)
ggsave("C:Plots//Transect//Resources_marine_var//resource_primary_subtidal_habitat.png")



# Plotting Marine resources vs. terrestrial variables  ---------------------------------------

# marine resources vs. n15 soil
n_marine1<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
n_marine2<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
n_marine3<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=log(fish_abundance_corrected)))+geom_point()+geom_smooth(method="lm")
n_marine4<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
n_marine5<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=log(fish_biomass_corrected)))+geom_point()+geom_smooth(method="lm")
n_marine6<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=fish_length))+geom_point()+geom_smooth(method="lm")
n_marine7<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")
plot_grid(n_marine4, n_marine5, n_marine7, n_marine3, n_marine2, n_marine1, ncol=3)
ggsave("C:Plots//Transect//Resources_terr_var//d15n_marine_corrected.png")


ggplot(fish_richness_merged_otter, aes(y=d15n, x=marine_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))



####GAM marine resources vs. n15n
n_marine_gam1<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam2<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam3<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=log(fish_abundance_corrected)))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam4<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam5<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=log(fish_biomass_corrected)))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
n_marine_gam6<-ggplot(fish_richness_merged_otter, aes(y=d15n, x=fish_length))+geom_point()+geom_smooth(method="gam", formula=y~s(x))
plot_grid(n_marine_gam4, n_marine_gam5, n_marine_gam6, n_marine_gam3, n_marine_gam2, n_marine_gam1, ncol=3)
ggsave("C:Plots//Transect//Resources_terr_var//d15n_marine_corrected_GAM.png")


#fish abundance
fish1<-ggplot(fish_richness_merged_otter, aes(x=log(fish_abundance_corrected), y=d15n))+geom_point()+geom_smooth(method="lm")
fish2<-ggplot(fish_richness_merged_otter, aes(x=log(fish_abundance_corrected), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish3<-ggplot(fish_richness_merged_otter, aes(x=log(fish_abundance_corrected), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish4<-ggplot(fish_richness_merged_otter, aes(x=log(fish_abundance_corrected), y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish13<-ggplot(fish_richness_merged_otter, aes(x=log(fish_abundance_corrected), y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish19<-ggplot(fish_richness_merged_otter, aes(x=log(fish_abundance_corrected), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish20<-ggplot(fish_richness_merged_otter, aes(x=log(fish_abundance_corrected), y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish1,fish2,fish3,fish4,fish13,fish19,fish20, ncol=4)
ggsave("C:Plots//Transect//Resources_terr_var//fish_abund_terrestrial.png")

#fish richness
fish5<-ggplot(fish_richness_merged_otter, aes(x=fish_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish6<-ggplot(fish_richness_merged_otter, aes(x=fish_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish7<-ggplot(fish_richness_merged_otter, aes(x=fish_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish8<-ggplot(fish_richness_merged_otter, aes(x=fish_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
fish14<-ggplot(fish_richness_merged_otter, aes(x=fish_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish21<-ggplot(fish_richness_merged_otter, aes(x=fish_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish22<-ggplot(fish_richness_merged_otter, aes(x=fish_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish8,fish5,fish6,fish7,fish14,fish21,fish22, ncol=4)
ggsave("C:Plots//Transect//Resources_terr_var//fish_richness_terrestrial.png")

#fish biomass
fish9<-ggplot(fish_richness_merged_otter, aes(x=log(fish_biomass_corrected), y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish10<-ggplot(fish_richness_merged_otter, aes(x=log(fish_biomass_corrected), y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish11<-ggplot(fish_richness_merged_otter, aes(x=log(fish_biomass_corrected), y=habitat_het))+geom_point()+geom_smooth(method="lm")
fish12<-ggplot(fish_richness_merged_otter, aes(x=log(fish_biomass_corrected), y=d15n))+geom_point()+geom_smooth(method="lm")
fish15<-ggplot(fish_richness_merged_otter, aes(x=log(fish_biomass_corrected), y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
fish23<-ggplot(fish_richness_merged_otter, aes(x=log(fish_biomass_corrected), y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
fish24<-ggplot(fish_richness_merged_otter, aes(x=log(fish_biomass_corrected), y=tree_abundance))+geom_point()+geom_smooth(method="lm")
plot_grid(fish12, fish9,fish10,fish11,fish15,fish23,fish24, ncol=4)
ggsave("C:Plots//Transect//Resources_terr_var//fish_biomass_corrected_terrestrial.png")

###bycatch richness and abundance
bycatch1<-ggplot(fish_richness_merged_otter, aes(x=bycatch_richness_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
bycatch2<-ggplot(fish_richness_merged_otter, aes(x=bycatch_richness_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch3<-ggplot(fish_richness_merged_otter, aes(x=bycatch_richness_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch4<-ggplot(fish_richness_merged_otter, aes(x=bycatch_richness_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
bycatch10<-ggplot(fish_richness_merged_otter, aes(x=bycatch_richness_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
bycatch12<-ggplot(fish_richness_merged_otter, aes(x=bycatch_richness_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
bycatch13<-ggplot(fish_richness_merged_otter, aes(x=bycatch_richness_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch14<-ggplot(fish_richness_merged_otter, aes(x=bycatch_richness_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
bycatch19<-ggplot(fish_richness_merged_otter, aes(x=bycatch_richness_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch20<-ggplot(fish_richness_merged_otter, aes(x=bycatch_richness_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(bycatch1,bycatch2,bycatch3,bycatch4,bycatch10,bycatch12, bycatch13,bycatch14,bycatch19,bycatch20, ncol=5)
ggsave("C:Plots//Transect//Resources_terr_var//bycatch_richness_terrestrial.png", width=40, height=20, unit="cm")


bycatch5<-ggplot(fish_richness_merged_otter, aes(x=bycatch_abundance_corrected, y=d15n))+geom_point()+geom_smooth(method="lm")
bycatch6<-ggplot(fish_richness_merged_otter, aes(x=bycatch_abundance_corrected, y=plant_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch7<-ggplot(fish_richness_merged_otter, aes(x=bycatch_abundance_corrected, y=bird.richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch8<-ggplot(fish_richness_merged_otter, aes(x=bycatch_abundance_corrected, y=habitat_het))+geom_point()+geom_smooth(method="lm")
bycatch9<-ggplot(fish_richness_merged_otter, aes(x=bycatch_abundance_corrected, y=NDVI_mean))+geom_point()+geom_smooth(method="lm")
bycatch11<-ggplot(fish_richness_merged_otter, aes(x=bycatch_abundance_corrected, y=mammal_richness))+geom_point()+geom_smooth(method="lm")
bycatch15<-ggplot(fish_richness_merged_otter, aes(x=bycatch_abundance_corrected, y=tree_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch16<-ggplot(fish_richness_merged_otter, aes(x=bycatch_abundance_corrected, y=tree_abundance))+geom_point()+geom_smooth(method="lm")
bycatch17<-ggplot(fish_richness_merged_otter, aes(x=bycatch_abundance_corrected, y=insect_richness))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
bycatch18<-ggplot(fish_richness_merged_otter, aes(x=bycatch_abundance_corrected, y=log(insect_abundance)))+geom_point()+geom_smooth(method="lm")
plot_grid(bycatch5,bycatch6,bycatch7,bycatch8,bycatch9,bycatch11,bycatch15,bycatch16,bycatch17,bycatch18,ncol=5)
ggsave("C:Plots//Transect//Resources_terr_var//bycatch_abundance_terrestrial.png", width=40, height=20, unit="cm")



# Plotting Marine resources vs. biogeography ----------------------------


### Marine resources vs. Island area
A_marine1<-ggplot(fish_richness_merged_otter, aes(x=log_Area, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
A_marine2<-ggplot(fish_richness_merged_otter, aes(x=log_Area, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")
A_marine3<-ggplot(fish_richness_merged_otter, aes(x=log_Area, y=log(fish_abundance_corrected)))+geom_point()+geom_smooth(method="lm")
A_marine4<-ggplot(fish_richness_merged_otter, aes(x=log_Area, y=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")
A_marine5<-ggplot(fish_richness_merged_otter, aes(x=log_Area, y=log(fish_biomass_corrected)))+geom_point()+geom_smooth(method="lm")
A_marine6<-ggplot(fish_richness_merged_otter, aes(x=log_Area, y=fish_length))+geom_point()+geom_smooth(method="lm")
plot_grid(A_marine4, A_marine5, A_marine6, A_marine3, A_marine2, A_marine1, ncol=3)
ggsave("C:Plots//Transect//Resources_biogeog//Area_marine.png")



###Coverage of neighbouring land mass, LOW NEighb_250 = high exposure
neib1<-ggplot(fish_richness_merged_otter, aes(x=Neighb_250, y=fish_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
neib2<-ggplot(fish_richness_merged_otter, aes(x=Neighb_250, y=bycatch_richness_corrected))+geom_point()+geom_smooth(method="glm", method.args = list(family = "poisson"))
neib4<- ggplot(fish_richness_merged_otter, aes(x=Neighb_250, y=log(fish_abundance_corrected)))+geom_point()+geom_smooth(method="lm")
neib5<- ggplot(fish_richness_merged_otter, aes(x=Neighb_250, y=bycatch_abundance_corrected))+geom_point()+geom_smooth(method="lm")
plot_grid(neib1,neib2,neib4,neib5, ncol=2)
ggsave("C:Plots//Transect//Resources_biogeog//Neighb250_marine.png")


###########3
# d15n bycatch ---------------------------------------------------------------
fish_richness_merged_otter_zscores<-fish_richness_merged_otter
fish_richness_merged_otter_zscores$bycatch_richness_corrected<-scale(fish_richness_merged_otter$bycatch_richness_corrected, center=TRUE, scale=TRUE)
fish_richness_merged_otter_zscores$bycatch_richness_corrected.unscaled <-fish_richness_merged_otter_zscores$bycatch_richness_corrected * attr(fish_richness_merged_otter_zscores$bycatch_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_otter_zscores$bycatch_richness_corrected, 'scaled:center')
fish_richness_merged_otter_zscores$bycatch_richness_corrected<-as.numeric(fish_richness_merged_otter_zscores$bycatch_richness_corrected)
ggplot(fish_richness_merged_otter_zscores, aes(y=d15n, x=bycatch_richness_corrected))+geom_point()+geom_smooth(method="lm")

str(fish_richness_merged_otter_zscores)

qqp(fish_richness_merged_otter_zscores$d15n)
qqp(fish_richness_merged_otter_zscores$d15n, "lnorm")

lm.d15n.bycatch<-lm(d15n ~ bycatch_richness_corrected, data=fish_richness_merged_otter_zscores)
gam.lm.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = fish_richness_merged_otter_zscores, select=TRUE, method="REML")
gam.loglink.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = fish_richness_merged_otter_zscores, family = gaussian(link="log"), select=TRUE, method="REML")

gam.gamma.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = fish_richness_merged_otter_zscores, family = Gamma, select=TRUE, method="REML")
gam.tweedie.d15n.bycatch<- gam(d15n ~ s(bycatch_richness_corrected),data = fish_richness_merged_otter_zscores, family = tw, select=TRUE, method="REML")


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
ndata.d15n.bycatch <- with(fish_richness_merged_otter_zscores, data_frame(bycatch_richness_corrected = seq(min(bycatch_richness_corrected), max(bycatch_richness_corrected),length = 100)))


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

fish_richness_merged_otter_zscores$bycatch_richness_corrected<-scale(fish_richness_merged_otter$bycatch_richness_corrected, center=TRUE, scale=TRUE)

ndata.d15n.bycatch$bycatch_richness_corrected.unscaled<-ndata.d15n.bycatch$bycatch_richness_corrected * attr(fish_richness_merged_otter_zscores$bycatch_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_otter_zscores$bycatch_richness_corrected, 'scaled:center')


# plot 
plt.d15n.bycatch <- ggplot(ndata.d15n.bycatch, aes(x = bycatch_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = fish_richness_merged_otter_zscores)+
  xlab(expression("Bycatch richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.bycatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.bycatch
ggsave("C:Plots//Transect//d15n_bycatch_400m.png")



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

fish_richness_merged_otter_zscores<-fish_richness_merged_otter
fish_richness_merged_otter_zscores$marine_richness_corrected<-scale(fish_richness_merged_otter$marine_richness_corrected, center=TRUE, scale=TRUE)
fish_richness_merged_otter_zscores$marine_richness_corrected.unscaled <-fish_richness_merged_otter_zscores$marine_richness_corrected * attr(fish_richness_merged_otter_zscores$marine_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_otter_zscores$marine_richness_corrected, 'scaled:center')
fish_richness_merged_otter_zscores$marine_richness_corrected<-as.numeric(fish_richness_merged_otter_zscores$marine_richness_corrected)
ggplot(fish_richness_merged_otter_zscores, aes(y=d15n, x=marine_richness_corrected))+geom_point()+geom_smooth(method="lm")

qqp(fish_richness_merged_otter_zscores$d15n)
qqp(fish_richness_merged_otter_zscores$d15n, "lnorm")

lm.d15n.marinecatch<-lm(d15n ~ marine_richness_corrected, data=fish_richness_merged_otter_zscores)
gam.lm.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = fish_richness_merged_otter_zscores, select=TRUE, method="REML")
gam.loglink.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = fish_richness_merged_otter_zscores, family = gaussian(link="log"), select=TRUE, method="REML")

gam.gamma.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = fish_richness_merged_otter_zscores, family = Gamma, select=TRUE, method="REML")
gam.tweedie.d15n.marinecatch<- gam(d15n ~ s(marine_richness_corrected, k=7),data = fish_richness_merged_otter_zscores, family = tw, select=TRUE, method="REML")


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
ndata.d15n.marinecatch <- with(fish_richness_merged_otter_zscores, data_frame(marine_richness_corrected = seq(min(marine_richness_corrected), max(marine_richness_corrected),length = 100)))


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

fish_richness_merged_otter_zscores$marine_richness_corrected<-scale(fish_richness_merged_otter$marine_richness_corrected, center=TRUE, scale=TRUE)

ndata.d15n.marinecatch$marine_richness_corrected.unscaled<-ndata.d15n.marinecatch$marine_richness_corrected * attr(fish_richness_merged_otter_zscores$marine_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_otter_zscores$marine_richness_corrected, 'scaled:center')


# plot 
plt.d15n.marinecatch <- ggplot(ndata.d15n.marinecatch, aes(x = marine_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = fish_richness_merged_otter_zscores)+
  xlab(expression("Marine catch richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.marinecatch,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.marinecatch
ggsave("C:Plots//Transect//d15n_bycatch.png")


# d15n fish richness ------------------------------------------------------


fish_richness_merged_otter_zscores<-fish_richness_merged_otter
fish_richness_merged_otter_zscores$fish_richness_corrected<-scale(fish_richness_merged_otter$fish_richness_corrected, center=TRUE, scale=TRUE)
fish_richness_merged_otter_zscores$fish_richness_corrected.unscaled <-fish_richness_merged_otter_zscores$fish_richness_corrected * attr(fish_richness_merged_otter_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_otter_zscores$fish_richness_corrected, 'scaled:center')
fish_richness_merged_otter_zscores$fish_richness_corrected<-as.numeric(fish_richness_merged_otter_zscores$fish_richness_corrected)
ggplot(fish_richness_merged_otter_zscores, aes(y=d15n, x=fish_richness_corrected))+geom_point()+geom_smooth(method="lm")

str(fish_richness_merged_otter_zscores)

qqp(fish_richness_merged_otter_zscores$d15n)
qqp(fish_richness_merged_otter_zscores$d15n, "lnorm")

gamma.12.fish_richness_corrected<-fitdistr(fish_richness_merged_otter$fish_richness_corrected+0.01, "gamma")
qqp(fish_richness_merged_otter$fish_richness_corrected, "gamma", shape = gamma.12.fish_richness_corrected$estimate[[1]], rate = gamma.12.fish_richness_corrected$estimate[[2]])

lm.d15n.fish<-lm(d15n ~ fish_richness_corrected, data=fish_richness_merged_otter_zscores)
gam.lm.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = fish_richness_merged_otter_zscores, select=TRUE, method="REML")
gam.loglink.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = fish_richness_merged_otter_zscores, family = gaussian(link="log"), select=TRUE, method="REML")
gam.gamma.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = fish_richness_merged_otter_zscores, family = Gamma, select=TRUE, method="REML")
gam.tweedie.d15n.fish<- gam(d15n ~ s(fish_richness_corrected),data = fish_richness_merged_otter_zscores, family = tw, select=TRUE, method="REML")


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
ndata.d15n.fish <- with(fish_richness_merged_otter_zscores, data_frame(fish_richness_corrected = seq(min(fish_richness_corrected), max(fish_richness_corrected),length = 100)))


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

fish_richness_merged_otter_zscores$fish_richness_corrected<-scale(fish_richness_merged_otter$fish_richness_corrected, center=TRUE, scale=TRUE)
ndata.d15n.fish$fish_richness_corrected.unscaled<-ndata.d15n.fish$fish_richness_corrected * attr(fish_richness_merged_otter_zscores$fish_richness_corrected, 'scaled:scale') + attr(fish_richness_merged_otter_zscores$fish_richness_corrected, 'scaled:center')


# plot 
plt.d15n.fish <- ggplot(ndata.d15n.fish, aes(x = fish_richness_corrected.unscaled, y = fit)) + 
  theme_classic()+
  geom_line(size=1.5, aes()) +
  geom_point(aes(y =(d15n)), size=3, data = fish_richness_merged_otter_zscores)+
  xlab(expression("fish richness per m2")) + ylab("d15n")+  
  scale_shape_manual(values=c(19))+
  geom_ribbon(data = ndata.d15n.fish,aes(ymin = right_lwr, ymax = right_upr), alpha = 0.10)+
  theme(legend.position="none")
plt.d15n.fish
ggsave("C:Plots//Transect//d15n_fish_400.png")





# Correlation -------------------------------------------------------------

which( colnames(fish_richness_merged_otter)=="Exp_SHZN" )
which( colnames(fish_richness_merged_otter)=="lat" )
which( colnames(fish_richness_merged_otter)=="size.cat2" )
which( colnames(fish_richness_merged_otter)=="notes" )

str(fish_richness_merged_otter)
corr_by_isl_selected_fish<-fish_richness_merged_otter %>% select_if(is.numeric)
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
ggsave("C:Plots//Transect//Resources_terr_var//ggcorrplot_marine_terrestrial.png",  width=40, height=40, unit="cm")

library(purrr)
library(ggcorrplot)
library(corrr)
library(PerformanceAnalytics)


corr_by_isl_selected_fish %>% correlate() %>%  network_plot(min_cor = 0.7)
ggsave("C:Plots//Transect//Resources_terr_var//networkplot_marine_terrestrial_0.7.png")
