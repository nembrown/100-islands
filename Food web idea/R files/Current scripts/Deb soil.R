library(here)

#here() starts at C:/Users/norahbrown/Dropbox/Projects/100-islands


deb_soil_matching<-read.csv("C:Food web idea//Data by person//Norah.data//deb_soil_matching.csv")
subsample_deb_soil<-read.csv("C:Food web idea//Data by person//Deb.data//subsample_deb_soil.csv")

head(deb_soil_matching)

head(subsample_deb_soil)

deb_soil_merged<-merge(deb_soil_matching[,c(1,7,11)], subsample_deb_soil)
head(deb_soil_merged)

