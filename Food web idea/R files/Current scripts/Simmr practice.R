setwd("C:\\Users\\Norah\\Dropbox\\Projects\\100 islands\\MOdelling practice")


library(simmr)
library(tidyr)

library(plyr)
library(dplyr)
library(ggplot2)
library(doBy)
library(grid)
library(cowplot)
######
mix = matrix(c(-10.13, -10.72, -11.39, -11.18, -10.81, -10.7, -10.54, 
               -10.48, -9.93, -9.37, 11.59, 11.01, 10.59, 10.97, 11.52, 11.89, 
               11.73, 10.89, 11.05, 12.3), ncol=2, nrow=10)
colnames(mix) = c('d13C','d15N')
s_names = c("Zostera", "Grass", "U.lactuca", "Enteromorpha")
s_means = matrix(c(-14, -15.1, -11.03, -14.44, 3.06, 7.05, 13.72, 5.96), ncol=2, nrow=4)
s_sds = matrix(c(0.48, 0.38, 0.48, 0.43, 0.46, 0.39, 0.42, 0.48), ncol=2, nrow=4)
c_means = matrix(c(2.63, 1.59, 3.41, 3.04, 3.28, 2.34, 2.14, 2.36), ncol=2, nrow=4)
c_sds = matrix(c(0.41, 0.44, 0.34, 0.46, 0.46, 0.48, 0.46, 0.66), ncol=2, nrow=4)
conc = matrix(c(0.02, 0.1, 0.12, 0.04, 0.02, 0.1, 0.09, 0.05), ncol=2, nrow=4)
####


simmr_in = simmr_load(mixtures=mix,
                      source_names=s_names,
                      source_means=s_means,
                      source_sds=s_sds,
                      correction_means=c_means,
                      correction_sds=c_sds,
                      concentration_means = conc)


plot(simmr_in)
plot(simmr_in,xlab=expression(paste(delta^13, "C (\u2030)",sep="")), 
     ylab=expression(paste(delta^15, "N (\u2030)",sep="")), 
     title='Isospace plot of example data')

help(plot.simmr_input)




# Deb's data --------------------------------------------------------------

i.feces.all<-read.csv(file.choose())
head(i.feces.all)

i.feathers.all<-read.csv(file.choose())
head(i.feathers.all)

i.veg.all<-read.csv(file.choose())
head(i.veg.all)

i.feces.all.mat<-as.matrix(i.feces.all[, 2:3])
head(i.feces.all.mat)

i.feces.all.mat.grp<-as.integer(i.feces.all[,7])
View(i.feces.all.mat.grp)

which(is.na(i.feathers.all$d13c))

i.feathers.all.mat<-as.matrix(i.feathers.all[-c(436,533), 2:3])
head(i.feathers.all.mat)

i.feathers.all.mat.grp<-as.integer(i.feathers.all[-c(436,533),7])
View(i.feathers.all.mat.grp)


cdata.veg <- summaryBy(d13c+d15n ~ spp, data=i.veg.all, FUN=function(x) { c(mean = mean(x), sd = sd(x), length=length(x)) } )

i.veg.means.mat<-as.matrix(cdata.veg[,c(2,5)])
i.veg.sds.mat<-as.matrix(cdata.veg[,c(3,6)])

head(i.veg.means.mat)

cdata.veg.names <- as.vector(cdata.veg[,1])

head(cdata.veg)
simmr_in.deb = simmr_load(mixtures=i.feces.all.mat,
                      source_names=cdata.veg.names,
                      source_means=i.veg.means.mat,
                      source_sds=i.veg.sds.mat,
                    group=i.feces.all.mat.grp)

plot(simmr_in.deb)
plot(simmr_in.deb,group=1:5,xlab=expression(paste(delta^13, "C (\u2030)",sep="")), 
     ylab=expression(paste(delta^15, "N (\u2030)",sep="")), 
     title='Isospace plot of Debs data',mix_name='Birds')

plot(simmr_in.deb,group=1:2,xlab=expression(paste(delta^13, "C (\u2030)",sep="")), 
     ylab=expression(paste(delta^15, "N (\u2030)",sep="")), 
     title='Isospace plot of Debs data',mix_name='Birds')

plot(simmr_in.deb,group=c(3,5),xlab=expression(paste(delta^13, "C (\u2030)",sep="")), 
     ylab=expression(paste(delta^15, "N (\u2030)",sep="")), 
     title='Isospace plot of Debs data',mix_name='Birds')

simmr_in.deb.feathers = simmr_load(mixtures=i.feathers.all.mat,
                          source_names=cdata.veg.names,
                          source_means=i.veg.means.mat,
                          source_sds=i.veg.sds.mat,
                          group=i.feathers.all.mat.grp)

plot(simmr_in.deb.feathers,group=1:5,xlab=expression(paste(delta^13, "C (\u2030)",sep="")), 
     ylab=expression(paste(delta^15, "N (\u2030)",sep="")), 
     title='Isospace plot of Debs data',mix_name='Birds')


plot(simmr_in.deb.feathers,group=1:2,xlab=expression(paste(delta^13, "C (\u2030)",sep="")), 
     ylab=expression(paste(delta^15, "N (\u2030)",sep="")), 
     title='Isospace plot of Debs data',mix_name='Birds')

plot(simmr_in.deb.feathers,group=c(3,5),xlab=expression(paste(delta^13, "C (\u2030)",sep="")), 
     ylab=expression(paste(delta^15, "N (\u2030)",sep="")), 
     title='Isospace plot of Debs data',mix_name='Birds')


