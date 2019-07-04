setwd("C:/Users/Norah/Dropbox/Projects/100 islands/Modelling practice/Norah.data")
otter_isotopes<-read.csv("C:master_otter_isotope_nb_new.csv")

head(otter_isotopes)



ggplot(otter_isotopes,aes(y=d15n, x=d13c, colour=node))+geom_point()+stat_ellipse()

ggplot(otter_isotopes,aes(y=d15n, x=lat, colour=node))+geom_point()
ggplot(otter_isotopes,aes(y=d13c, x=lat, colour=node))+geom_point()


ggplot(otter_isotopes,aes(y=d15n, x=lat))+geom_point(aes(col=node))+geom_smooth(method="lm")
ggplot(otter_isotopes,aes(y=d13c, x=lat))+geom_point(aes(col=node))+geom_smooth(method="lm")

ggplot(otter_isotopes,aes(y=cn, x=lat))+geom_point()+geom_smooth(method="lm")
ggplot(otter_isotopes,aes(y=n, x=lat))+geom_point()+geom_smooth(method="lm")
ggplot(otter_isotopes,aes(y=c, x=lat))+geom_point()+geom_smooth(method="lm")
