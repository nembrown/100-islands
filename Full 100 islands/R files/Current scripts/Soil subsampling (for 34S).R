setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Modelling practice")
#change to norahbrown if on work computer

#read in necessary packages
library(tidyr)
library(plyr)
library(dplyr)
library(doBy)
library(ggplot2)
library(car)
library(fitdistrplus)


#loading all the data

#Deb's soil data and Deb's shore dist
i.soil.all<-read.csv("C:Deb.data//i-soil-all.csv")
#this is isotopes
shoredist.deb<-read.csv("C:Deb.data//shoredist.csv")
#this is the point count to distance to shore data

head(i.soil.all)
head(shoredist.deb)

soil.deb<-merge(i.soil.all, shoredist.deb, by="pcid")
head(soil.deb)
names(soil.deb)[16]<-"shore_dist"
names(soil.deb)[11]<-"unq_isl"
names(soil.deb)[4]<-"c"
names(soil.deb)[5]<-"n"
names(soil.deb)[6]<-"s"
names(soil.deb)[7]<-"cn"
names(soil.deb)[1]<-"unq_plot"
soil.deb$pc1<-"NA"
soil.deb$species.richness<-"NA"
soil.deb$fs_pc1<-"NA"
soil.deb$sm_av<-"NA"
soil.deb$slope<-"NA"



#owen's data
soil_clean<-read.csv("c:Owen's data//soil_clean.csv", header=TRUE, sep=",")
names(soil_clean)[6]<-"d13c"
names(soil_clean)[7]<-"d15n"

owen_key<-read.csv("c:Owen's data//key.csv", header=TRUE, sep=",")
hakai_plot<-read.csv("c:Owen's data//hakai_plot.csv", header=TRUE, sep=",")
names(hakai_plot)[3]<-"species.richness"

owen_key_expanded<-merge(owen_key, hakai_plot, by="unq_plot")
head(owen_key_expanded)


soil_merge<-merge(soil_clean, owen_key_expanded, by="unq_plot")

head(soil_merge)
names(soil_merge)[5]<-"s"

### don't re-run ... 
set.seed(2)
#subsample --> Owen
#for re-running for 34S analysis
randomSample = function(df,n) { 
  return (df[sample(nrow(df), n),])
}


smallerDF<-randomSample(soil_merge, 100)
head(smallerDF)
hist(smallerDF$d15n)
hist(soil_merge$d15n)

write.csv(smallerDF, "C:Owen's data/subsample_owen_soil.csv")

#subsample -->Deb
randomSample = function(df,n) { 
  return (df[sample(nrow(df), n),])
}


smallerDF.deb<-randomSample(soil.deb, 50)
head(smallerDF.deb)
hist(smallerDF.deb$d15n)
hist(soil.deb$d15n)

write.csv(smallerDF.deb, "C:Deb.data/subsample_deb_soil.csv")

####