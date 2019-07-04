setwd("C:\\Users\\norahbrown\\Dropbox\\Projects\\100 islands\\Modelling practice")
#work computer is norahbrown, home computer is Norah
library(tidyr)
install.packages("MixSIAR")

# Owen's data -------------------------------------------------------------
library(MixSIAR)


#owen's data
foliar_clean_sorted<-read.csv("c:Owen's data//foliar_clean_sorted.csv", header=TRUE, sep=",")
foliar_clean_sorted<-foliar_clean_sorted[,-9]
foliar_clean_sorted<-na.omit(foliar_clean_sorted)
head(foliar_clean_sorted)
length(foliar_clean_sorted$unq_plot)
#1190 - the NAs were elsewhere


foliar_key<-read.csv("c:Owen's data//key_mod.csv", header=TRUE, sep=",")
head(foliar_key)
length(foliar_key$node)
#1585 when you read in _mod
#20 duplicates
##put NA= 0 for the distance
foliar_key<-foliar_key[,-5]
#there are NAs in the transect so take that out, not impo info
### In foliar mod I also modified the distance to be the closest 10m ... so that we only ahve 5 categories



soil_clean<-read.csv("c:Owen's data//soil_clean.csv", header=TRUE, sep=",")
head(soil_clean)
soil_clean<-na.omit(soil_clean)
length(soil_clean$unq_plot)
#678

#make sure foliar only has the amount of rows that match with soil clean
foliar_clean_sorted_merge<-merge(foliar_clean_sorted,soil_clean, by="unq_plot")
str(foliar_clean_sorted_merge)
head(foliar_clean_sorted_merge)
##cut down 1190 to 1162
#there are no nas

#now we need foliar_clean sorted with the meta-data foliar key
foliar_clean_sorted_merge_meta<-merge(foliar_clean_sorted_merge[,c(1:8)],foliar_key, by="unq_plot")
head(foliar_clean_sorted_merge_meta)
#there's a problem with the xs, rename
names(foliar_clean_sorted_merge_meta)[7]="d13c"
names(foliar_clean_sorted_merge_meta)[8]="d15n"

str(foliar_clean_sorted_merge_meta)
#1145 
#how did we lose rows?
#because we have soil from weird places not on transect... like 7.5? 


soil_clean_merge_1<-merge(soil_clean, foliar_clean_sorted[,1:2], by=("unq_plot"))
head(soil_clean_merge_1)
length(soil_clean_merge_1$unq_plot)
#1162

soil_clean_merge_2<-merge(soil_clean_merge_1, foliar_key, by=("unq_plot"))
head(soil_clean_merge_2)
length(soil_clean_merge_2$unq_plot)
names(soil_clean_merge_2)[6]="d13c"
names(soil_clean_merge_2)[7]="d15n"

#1145
foliar_clean_sorted_merge_meta$shore_dist<-as.factor(foliar_clean_sorted_merge_meta$shore_dist)
head(foliar_clean_sorted_merge_meta)


write.csv(foliar_clean_sorted_merge_meta, "c:Owen's data//foliar_clean_sorted_merge_meta.csv")

str(foliar_clean_sorted_merge_meta)
mix.filename.owen.foliar<-"C:Owen's data\\foliar_clean_sorted_merge_meta.csv"

mix.owen <- load_mix_data(filename=mix.filename.owen.foliar, 
                          iso_names=c("d13c","d15n"), 
                          factors=c("shore_dist", "species"), 
                          fac_random=c(FALSE, FALSE), 
                          fac_nested=c(FALSE, FALSE), 
                          cont_effects=NULL)


soil_clean_merge_2$shore_dist<-as.factor(soil_clean_merge_2$shore_dist)
write.csv(soil_clean_merge_2, "C:Owen's data\\soil_clean_merge_2.csv")
str(soil_clean_merge_2)
head(soil_clean_merge_2)
names(soil_clean_merge_2)[11]="node"

source.filename.owen.soil<-"C:Owen's data\\soil_clean_merge_2.csv"
### here you need to go add a blank column at the front with node as the info otherwise the next part won't run ... can't figure out how to do that in r

source.owen <- load_source_data(filename=source.filename.owen.soil,
                                source_factors=NULL, 
                                conc_dep=FALSE, 
                                data_type="raw", 
                                mix.owen)


discr.filename.owen<-"C:Owen's data\\foliage_discrimination.csv"

discr.owen <- load_discr_data(filename=discr.filename.owen, mix.owen)
str(discr.owen)
head(discr.filename.owen)
str(source.owen)
head(mix.owen)

plot_data(filename="isospace_plot", plot_save_pdf=FALSE, plot_save_png=FALSE, mix.owen,source.owen,discr.owen)


#plot with means


library(doBy)
View(soil_clean_merge_2)
cdata_soil_clean_merge_2 <- summaryBy(d13c+d15n ~ species+node, data=soil_clean_merge_2, FUN=function(x) { c(mean = mean(x), sd = sd(x), n=length(x)) } )


head(cdata_soil_clean_merge_2)
str(cdata_soil_clean_merge_2)
names(cdata_soil_clean_merge_2)[4]="Meand13c"
names(cdata_soil_clean_merge_2)[5]="SDd13c"
names(cdata_soil_clean_merge_2)[6]="n"
names(cdata_soil_clean_merge_2)[7]="Meand15n"
names(cdata_soil_clean_merge_2)[8]="SDd15n"
View(cdata_soil_clean_merge_2)
write.csv(cdata_soil_clean_merge_2,"C:Owen's data\\cdata_soil_clean_merge_2.csv")
### again here you need to go add a blank column at the front with node as the info otherwise the next part won't run ... can't figure out how to do that in r

source.filename.owen.soil.means<-"C:Owen's data\\cdata_soil_clean_merge_2.csv"


source.owen.means <- load_source_data(filename=source.filename.owen.soil.means,
                                      source_factors=NULL, 
                                      conc_dep=FALSE, 
                                      data_type="means", 
                                      mix.owen)

plot_data(filename="isospace_plot", plot_save_pdf=FALSE, plot_save_png=FALSE, mix.owen,source.owen.means,discr.owen)
##### for some reason doesn't work ... 
#says must be same length as data?? 



###let's try with distance as mean?

cdata_soil_clean_merge_2_distmean <- summaryBy(d13c+d15n ~ shore_dist, data=soil_clean_merge_2, FUN=function(x) { c(mean = mean(x), sd = sd(x), n=length(x)) } )
head(cdata_soil_clean_merge_2_distmean)
str(cdata_soil_clean_merge_2_distmean)
cdata_soil_clean_merge_2_distmean$shore_dist<-as.factor(cdata_soil_clean_merge_2_distmean$shore_dist)

names(cdata_soil_clean_merge_2_distmean)[2]="Meand13c"
names(cdata_soil_clean_merge_2_distmean)[3]="SDd13c"
names(cdata_soil_clean_merge_2_distmean)[4]="n"
names(cdata_soil_clean_merge_2_distmean)[5]="Meand15n"
names(cdata_soil_clean_merge_2_distmean)[6]="SDd15n"

write.csv(cdata_soil_clean_merge_2_distmean,"C:Owen's data\\cdata_soil_clean_merge_2_distmean.csv")
### again here you need to go add a blank column at the front with shore_dist as the info otherwise the next part won't run ... can't figure out how to do that in r

source.filename.owen.soil.distmeans<-"C:Owen's data\\cdata_soil_clean_merge_2_distmean.csv"


source.owen.distmeans <- load_source_data(filename=source.filename.owen.soil.distmeans,
                                      source_factors=NULL, 
                                      conc_dep=FALSE, 
                                      data_type="means", 
                                      mix.owen)

discr.filename.owen.dist<-"C:Owen's data\\distance_discrimination.csv"

discr.owen.dist <- load_discr_data(filename=discr.filename.owen.dist, mix.owen)


plot_data(filename="isospace_plot", plot_save_pdf=FALSE, plot_save_png=FALSE, mix.owen,source.owen.distmeans,discr.owen.dist)



##### let's try with JUST node
cdata_soil_clean_merge_2_nodemean <- summaryBy(d13c+d15n ~node, data=soil_clean_merge_2, FUN=function(x) { c(mean = mean(x), sd = sd(x), n=length(x)) } )
head(cdata_soil_clean_merge_2_nodemean)
names(cdata_soil_clean_merge_2_nodemean)[2]="Meand13c"
names(cdata_soil_clean_merge_2_nodemean)[3]="SDd13c"
names(cdata_soil_clean_merge_2_nodemean)[4]="n"
names(cdata_soil_clean_merge_2_nodemean)[5]="Meand15n"
names(cdata_soil_clean_merge_2_nodemean)[6]="SDd15n"

write.csv(cdata_soil_clean_merge_2_nodemean,"C:Owen's data\\cdata_soil_clean_merge_2_nodemean.csv")
### again here you need to go add a blank column at the front with shore_node as the info otherwise the next part won't run ... can't figure out how to do that in r

source.filename.owen.soil.nodemeans<-"C:Owen's data\\cdata_soil_clean_merge_2_nodemean.csv"


source.owen.nodemeans <- load_source_data(filename=source.filename.owen.soil.nodemeans,
                                          source_factors=NULL, 
                                          conc_dep=FALSE, 
                                          data_type="means", 
                                          mix.owen)

#mix.owen <- load_mix_data(filename=mix.filename.owen.foliar, 
#                       iso_names=c("d13c","d15n"), 
 #                         factors="node", 
  #                        fac_random=FALSE, 
   #                       fac_nested=FALSE, 
    #                      cont_effects="shore_dist")#


plot_data(filename="isospace_plot", plot_save_pdf=FALSE, plot_save_png=FALSE, mix.owen,source.owen.nodemeans,discr.owen)
#exactly the same as if you plot with raw data - I think it takes the means for you.... 



###plotting with MIXSIAR is okay but doesn't really get at the grouping of things... can't edit the plots as ggplot... 
#might be better to first do in ggplot

## could also subset the data to see the patterens more clearly... like only one node at a time? 
