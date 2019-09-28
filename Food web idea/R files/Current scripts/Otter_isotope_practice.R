
setwd("C:/Users/Norah/Dropbox/Projects/100-islands/Food web idea/Data by person/Norah.data")
otter_isotopes<-read.csv("C:master_otter_isotope_nb_new.csv")

head(otter_isotopes)

ggplot(otter_isotopes,aes(y=d15n, x=d34s, colour=node))+geom_point()+stat_ellipse()
ggplot(otter_isotopes,aes(y=d15n, x=d13c, colour=node))+geom_point()+stat_ellipse()
ggplot(otter_isotopes,aes(y=d15n, x=d13c, colour=node, label=site.id))+geom_point()+geom_text()



ggplot(otter_isotopes,aes(y=d15n, x=d13c, colour=node, size=d34s))+geom_point()+stat_ellipse()

cols <- c("darkblue", "orange", "lightgreen")
library(scatterplot3d)
attach(mtcars)
scatterplot3d(otter_isotopes$d15n, otter_isotopes$d34s, otter_isotopes$d13c,type="h", 
              pch=16,color=cols[as.numeric(otter_isotopes$node)], 
              labels=otter_isotopes$site.id,main="3D Scatterplot")
legend("right", legend = levels(otter_isotopes$node),
       col =  c("darkblue", "orange", "lightgreen"), pch = 16)

?scatterplot3d
ggplot(otter_isotopes,aes(y=d34s, x=lat, colour=node))+geom_point()
ggplot(otter_isotopes,aes(y=d13c, x=lat, colour=node))+geom_point()


ggplot(otter_isotopes,aes(y=d15n, x=lat))+geom_point(aes(col=node))+geom_smooth(method="lm")
ggplot(otter_isotopes,aes(y=d13c, x=lat))+geom_point(aes(col=node))+geom_smooth(method="lm")

ggplot(otter_isotopes,aes(y=cn, x=lat))+geom_point()+geom_smooth(method="lm")
ggplot(otter_isotopes,aes(y=n, x=lat))+geom_point()+geom_smooth(method="lm")
ggplot(otter_isotopes,aes(y=c, x=lat))+geom_point()+geom_smooth(method="lm")
