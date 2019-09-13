setwd("C:/Users/norahbrown/Dropbox/Projects/100-islands/Food web idea/Data by person/Norah.data")
otter_isotopes<-read.csv("C:master_otter_isotope_nb_new.csv")

head(otter_isotopes)

otter_isotopes_s<-read.csv("C:otter_isotope_s.csv")
head(otter_isotopes_s)
otter_isotopes_all<-merge(otter_isotopes, otter_isotopes_s, by="site.id", all=TRUE)
head(otter_isotopes_all)

ggplot(otter_isotopes_all,aes(y=d15n, x=d34s, colour=node))+geom_point()+stat_ellipse()
ggplot(otter_isotopes_all,aes(y=d15n, x=d13c, colour=node))+geom_point()+stat_ellipse()
ggplot(otter_isotopes_all,aes(y=d34s, x=d13c, colour=node))+geom_point()+stat_ellipse()

ggplot(otter_isotopes_all,aes(y=d15n, x=d13c, colour=node, size=d34s))+geom_point()+stat_ellipse()


ggplot(otter_isotopes,aes(y=d15n, x=lat, colour=node))+geom_point()
ggplot(otter_isotopes,aes(y=d13c, x=lat, colour=node))+geom_point()


ggplot(otter_isotopes,aes(y=d15n, x=lat))+geom_point(aes(col=node))+geom_smooth(method="lm")
ggplot(otter_isotopes,aes(y=d13c, x=lat))+geom_point(aes(col=node))+geom_smooth(method="lm")

ggplot(otter_isotopes,aes(y=cn, x=lat))+geom_point()+geom_smooth(method="lm")
ggplot(otter_isotopes,aes(y=n, x=lat))+geom_point()+geom_smooth(method="lm")
ggplot(otter_isotopes,aes(y=c, x=lat))+geom_point()+geom_smooth(method="lm")
