install.packages("SIBER")

library(SIBER)

# load in the included demonstration dataset
data("demo.siber.data")

head(demo.siber.data)
#
# create the siber object
siber.example <- createSiberObject(demo.siber.data)

# Create lists of plotting arguments to be passed onwards to each 
# of the three plotting functions.
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")



par(mfrow=c(1,1))
plotSiberObject(siber.example,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls =T , group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030')
)


#using owen's data

head(owen.soilveg)
owen.soilveg.siber<-owen.soilveg
names(owen.soilveg.siber)[2]<-"iso1"
names(owen.soilveg.siber)[3]<-"iso2"
names(owen.soilveg.siber)[5]<-"community"
head(owen.soilveg.siber)
owen.soilveg.siber.2<-owen.soilveg.siber[,c(2,3,10,5)]
head(owen.soilveg.siber.2)


owen.siber<-createSiberObject(owen.soilveg.siber.2)

plotSiberObject(owen.siber,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = F, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030')
)

##using deb's data

deb.feces.veg<-rbind(i.feces.all[,c(2,3,13,7)], i.veg.all[,c(2,3,12,7)])
head(deb.feces.veg)
names(deb.feces.veg)[1]<-"iso1"
names(deb.feces.veg)[2]<-"iso2"
names(deb.feces.veg)[3]<-"group"
names(deb.feces.veg)[4]<-"community"

deb.feces.veg.siber<-createSiberObject(deb.feces.veg)

par(mfrow=c(1,1))
?plotSiberObject

plotSiberObject(deb.feces.veg.siber,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = F, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030')
)

#checking to see if owen and debs plant data are the same? 
combined.veg.owendeb<-rbind(owen.soilveg.siber.2,deb.feces.veg)

combined.veg.owendeb.siber<-createSiberObject(combined.veg.owendeb)

plotSiberObject(combined.veg.owendeb.siber,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = F, group.ellipses.args,
                group.hulls = F, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030')
)

