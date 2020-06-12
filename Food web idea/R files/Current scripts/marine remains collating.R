#Description
#this script is for gathering evidence of marine remains in terrestrial plots. 
#we have two main sources of information - Chris Ernst's plots - 8 plots per direction (NESW + Interior) = ~40 plots per island
#and Owen's plot: one plot per 10m of a transect, so ruoghly 5 x 4 = 20 per island.... together that's 60 plots
#however, we have a different # of plots per island - from 10-72 .. so will standardize by transect first then by island

#if we are doing a plot-level analysis, we will only use Owen's plots and use data from: marine_by_plot_from_notes_selected

#if we are doing a transect-level analysis, depending on analysis, we could use only Owen's transects: marine_by_transect_from_notes_selected_sum
#Or we could use a shoreline estimate, based on 8 of chris' plots + the 0m and 10m plots of owens to combine to 10 plots: marine_by_tran_combined
#for SEM will do proportion of plots found in at least 8 plots - I think below that I will change to NA, so doesn't estimate

#if we're doing by island, use: combined_otter_sum; which is a combo of the shoreline plots only (8 Chris Ernst, 2 Owen's x 4 = 40), i.e. not interior 

#load packages
library(vegan)
library(rlang)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(doBy)
library(plyr)
library(dplyr)
library(doBy)
library(cowplot)
library(viridis)
library(matrixStats)
library(tidyverse)


# Load data from Chris Ernst ---------------------------------------------------
#data from Chris Ernst's plots on trapline
marine_by_plot_from_chris<- read.csv("C:Food web idea//Data by person//Chris.data//chris_habitat.csv", header=TRUE, sep=",")

#fix transect and plot names
marine_by_plot_from_chris$unq_tran<- paste(marine_by_plot_from_chris$island,marine_by_plot_from_chris$direction)
marine_by_plot_from_chris$unq_tran<-gsub(" ", "", marine_by_plot_from_chris$unq_tran, fixed = TRUE)
marine_by_plot_from_chris$unq_isl<-marine_by_plot_from_chris$island
marine_by_plot_from_chris$unq_plot<-paste(marine_by_plot_from_chris$island,marine_by_plot_from_chris$direction,"q", marine_by_plot_from_chris$quadrat)
marine_by_plot_from_chris$unq_plot<-gsub(" ", "", marine_by_plot_from_chris$unq_plot, fixed = TRUE)

#extract marine information
marine_by_plot_from_chris$otter_pres_chris <- ifelse(grepl("otter|ottre|latrine|nearotter", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$marine_invert_pres_chris <- ifelse(grepl("bivalve|shell|crab|abalone|ablone|limpet|geoduck|sea star|whelk|snail|chiton|clam|scallop|mussel|mussell|urchin|claw|carapace|tube worm|barnacle", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$fish_chris <- ifelse(grepl("fish|rockfish|marine vertebrae", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$unk_bird_pres_chris <- ifelse(grepl("guano|feather|bird poo|bird", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$eagle_pres_chris <- ifelse(grepl("eagle", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$raven_pres_chris <- ifelse(grepl("raven", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$driftwood_chris <- ifelse(grepl("driftwood", marine_by_plot_from_chris$marine), 1, 0)
marine_by_plot_from_chris$seaweed_chris <- ifelse(grepl("seaweed|algae|fucus|kelp", marine_by_plot_from_chris$marine), 1, 0)

marine_by_plot_from_chris$vector_evidence_chris<-marine_by_plot_from_chris$otter_pres_chris 
marine_by_plot_from_chris$marine_evidence_chris<-marine_by_plot_from_chris$seaweed_chris+ marine_by_plot_from_chris$marine_invert_pres_chris+marine_by_plot_from_chris$fish_chris
marine_by_plot_from_chris$total_marine_evidence_chris<-marine_by_plot_from_chris$otter_pres_chris  + marine_by_plot_from_chris$seaweed_chris + marine_by_plot_from_chris$marine_invert_pres_chris +marine_by_plot_from_chris$fish_chris

marine_by_plot_from_chris$n_chris<-1

head(marine_by_plot_from_chris)
length(unique(marine_by_plot_from_chris$island))

#summarise chris plots by island just to see where we have the plots - how many island have the full number
marine_by_isl_from_chris_selected<-marine_by_plot_from_chris %>% 
  dplyr::select(unq_isl, otter_pres_chris) %>% 
  group_by(unq_isl) %>%   
  summarise_all(list(otter_chris_plot_sum=sum, n_otter_chris_plots=length))

#View(marine_by_isl_from_chris_selected)
hist(marine_by_isl_from_chris_selected$n_otter_chris_plots)
# minimum is 19 but 3 from transects NESW and 7 is lowest from interior. 


#summarise chris plots by transect (I is interior, sum across the transect)
marine_by_tran_from_chris_selected<-marine_by_plot_from_chris %>% 
  dplyr::select(unq_tran, otter_pres_chris, unk_bird_pres_chris, marine_invert_pres_chris, driftwood_chris, seaweed_chris,  fish_chris) %>% 
  group_by(unq_tran) %>%   
  summarise_if(is.numeric, sum, na.rm=TRUE)
head(marine_by_tran_from_chris_selected)

#no interior, just the outside plots, summarise
marine_by_tran_from_chris_selected_noI<-marine_by_plot_from_chris %>% filter(direction != "I") %>% 
  dplyr::select(unq_tran, otter_pres_chris, unk_bird_pres_chris, marine_invert_pres_chris, driftwood_chris, seaweed_chris,  fish_chris, vector_evidence_chris, total_marine_evidence_chris, marine_evidence_chris, n_chris) %>% 
  group_by(unq_tran) %>% summarise_if(is.numeric, sum, na.rm=TRUE) 
head(marine_by_tran_from_chris_selected_noI)

#done up these two different ways of summarising, based on how we'll use the data later.... 
#Without interior plots will lign up better for combinging with owen's observations


# Load data from Owen's notes --------------------------------------------------
#from Owen's plot-level notes

marine_by_plot_from_notes<- read.csv("C:Food web idea//Data by person//Owen's data//100Islands_Fitzpatrick_plot.csv", header=TRUE, sep=",")
head(marine_by_plot_from_notes)

#extract marine information
marine_by_plot_from_notes$otter_pres <- ifelse(grepl("otter|ottre|latrine|nearotter", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$eagle_pres <- ifelse(grepl("eagle", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$raven_pres <- ifelse(grepl("raven", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$unk_bird_pres <- ifelse(grepl("guano|feather|bird poo|bird", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$marine_invert_pres <- ifelse(grepl("bivalve|shell|crab|abalone|ablone|limpet|snail|clam|scallop|mussel|mussell|urchin|claw|carapace|tube worm", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$driftwood <- ifelse(grepl("driftwood", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$midden <- ifelse(grepl("shelly|midden", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$seaweed <- ifelse(grepl("seaweed|algae", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$mink <- ifelse(grepl("mink", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$fish <- ifelse(grepl("fish|rockfish|marine vertebrae", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$mammal_bones <- ifelse(grepl("mammal bones|mammale bones", marine_by_plot_from_notes$notes), 1, 0)

#fixing mistakes
#Here Owen wrote - "not an otter spot", changing from 1 to 0
marine_by_plot_from_notes$otter_pres[marine_by_plot_from_notes$unq_plot=="CV09WE5"]<-0
marine_by_plot_from_notes$otter_pres[marine_by_plot_from_notes$unq_plot=="CV09WE6"]<-0
#these are where it said shell cove - so not a shell actually
marine_by_plot_from_notes$marine_invert_pres[marine_by_plot_from_notes$unq_plot=="ST07NS5"]<-0
marine_by_plot_from_notes$marine_invert_pres[marine_by_plot_from_notes$unq_plot=="TQ15W1"]<-0
marine_by_plot_from_notes$marine_invert_pres[marine_by_plot_from_notes$unq_plot=="MM06N1"]<-0
marine_by_plot_from_notes$marine_invert_pres[marine_by_plot_from_notes$unq_plot=="PR04E1"]<-0
#this one is from a barnacle in plot but couldn't use that as search term becuase Owen would say "barnacle line" a lot to say where the transect started. 
marine_by_plot_from_notes$marine_invert_pres[marine_by_plot_from_notes$unq_plot=="PR05NS3"]<-1

#cutting df down to just the unq_plot and marine variables
marine_by_plot_from_notes_selected<-marine_by_plot_from_notes %>% dplyr::select(unq_plot, otter_pres, eagle_pres, raven_pres, unk_bird_pres, marine_invert_pres, driftwood, midden, seaweed, mink, fish, mammal_bones )
head(marine_by_plot_from_notes_selected)
marine_by_plot_from_notes_selected$vector_evidence<-marine_by_plot_from_notes_selected$otter_pres + marine_by_plot_from_notes_selected$eagle_pres + marine_by_plot_from_notes_selected$mink + marine_by_plot_from_notes_selected$raven_pres
marine_by_plot_from_notes_selected$marine_evidence<-marine_by_plot_from_notes_selected$seaweed+ marine_by_plot_from_notes_selected$marine_invert_pres+marine_by_plot_from_notes_selected$fish
marine_by_plot_from_notes_selected$total_marine_evidence<-marine_by_plot_from_notes_selected$otter_pres + marine_by_plot_from_notes_selected$eagle_pres + marine_by_plot_from_notes_selected$mink + marine_by_plot_from_notes_selected$raven_pres + marine_by_plot_from_notes_selected$seaweed+ marine_by_plot_from_notes_selected$marine_invert_pres +marine_by_plot_from_notes_selected$fish+marine_by_plot_from_notes_selected$midden

#checking for duplicate plots
marine_by_plot_from_notes_selected[duplicated(marine_by_plot_from_notes_selected$unq_plot),]

#adding in transect names - making unq_tran from unq_plot is necessary
head(marine_by_plot_from_notes_selected)
key<-marine_by_plot_from_notes_selected
key$unq_tran<-str_sub(key$unq_plot, end=-2)
key$plot<-str_sub(key$unq_plot, start=-1)
key<-key[, c("unq_plot", "unq_tran", "plot")]

#fixing the one transect for small islands problem:
key<- key %>% mutate(unq_tran= if_else(plot<4, gsub("SN", "S", unq_tran, fixed = TRUE), gsub("SN", "N", unq_tran, fixed = TRUE))) %>% 
  mutate(unq_tran= if_else(plot<4, gsub("NS", "N", unq_tran, fixed = TRUE), gsub("NS", "S", unq_tran, fixed = TRUE))) %>% 
  mutate(unq_tran= if_else(plot<4, gsub("EW", "E", unq_tran, fixed = TRUE), gsub("EW", "W", unq_tran, fixed = TRUE))) %>% 
  mutate(unq_tran= if_else(plot<4, gsub("WE", "W", unq_tran, fixed = TRUE), gsub("WE", "E", unq_tran, fixed = TRUE))) 

#fixing mistakes: these ones are double digits - also they are ones are that 25m and 15m (so would actually be less than plot 3)
key$unq_tran[key$unq_plot=="CV04SN25"]<-"CV04S"
key$unq_tran[key$unq_plot=="MM04WE25"]<-"MM04W"
key$unq_tran[key$unq_plot=="MM08NS25"]<-"MM08N"
key$unq_tran[key$unq_plot=="PR05EW25"]<-"PR05E"
key$unq_tran[key$unq_plot=="PR06EW25"]<-"PR06E"
key$unq_tran[key$unq_plot=="TQ02NS25"]<-"TQ02N"
key$unq_tran[key$unq_plot=="TQ05EW25"]<-"TQ05E"
key$unq_tran[key$unq_plot=="MM01WE15"]<-"MM01W"
key$unq_tran[key$unq_plot=="MM03WE15"]<-"MM03W"
key$unq_tran[key$unq_plot=="MM08EW15"]<-"MM08E"
key$unq_tran[key$unq_plot=="TQ06EW15"]<-"TQ06E"

#fixing mistakes: this transect was only 3 plots long, two exterior and one interior.... so plot #3 is East
key$unq_tran[key$unq_plot=="AD03WE3"]<-"AD03E"
key$unq_tran[key$unq_plot=="CV14SN3"]<-"CV14N"
key$unq_tran[key$unq_plot=="CV14EW3"]<-"CV14W"
key$unq_tran[key$unq_plot=="MM07NS3"]<-"MM07S"
key$unq_tran[key$unq_plot=="ST09WE3"]<-"ST09E"

head(key)

marine_by_plot_from_notes_selected<-merge(marine_by_plot_from_notes_selected, key, by="unq_plot", all.x=TRUE)
marine_by_plot_from_notes_selected$unq_isl<-str_sub(marine_by_plot_from_notes_selected$unq_tran, end=4)
marine_by_plot_from_notes_selected$n<-1

head(marine_by_plot_from_notes_selected)
write.csv(marine_by_plot_from_notes_selected, "C:Biodiversity idea//Output files//marine_by_plot_from_notes_selected.csv", row.names=FALSE)



### plot level exploration of prevalnce of otter presence in data. 
otter_pres<-marine_by_plot_from_notes_selected$otter_pres
n_otter<-sum(otter_pres)
n_ttl<-length(otter_pres)

n_pgrid = 1000
p_grid = seq(0, 1, length.out = n_pgrid)
prior = rep(1, n_pgrid)
likelihood = dbinom(n_otter, size=n_ttl, prob = p_grid)
posterior = likelihood * prior
posterior = posterior / sum(posterior)
plot(p_grid, posterior, type='l')
p_grid[which.max(posterior)] #0.0980981




# Summarizing Owen's plot-level data to transect-level, using the whole transect 
marine_by_transect_from_notes_selected_sum<- marine_by_plot_from_notes_selected %>% group_by(unq_tran) %>%   
                                        summarise_if(is.numeric, sum, na.rm=TRUE)
head(marine_by_transect_from_notes_selected_sum)


#Summarising Owen's plot level, just plot 1 and 2 ... (up to 10 m inland .. should correspond to Chris' 8 plots ... so combine those - that's 10 plots)
marine_by_transect_from_notes_selected_sum_shoreline<- marine_by_plot_from_notes_selected %>% filter(plot < 3) %>%  group_by(unq_tran) %>%   
  summarise_if(is.numeric, sum, na.rm=TRUE)
#View(marine_by_transect_from_notes_selected_sum_shoreline)


# Merging Chris Ernst's transect level data with Owen's transect level (shoreline) -------------------

#going with just the shoreline plots, aiming to get 10 per transect, to have even sampling. 
head(marine_by_transect_from_notes_selected_sum_shoreline)
head(marine_by_tran_from_chris_selected_noI)

marine_by_tran_combined<-merge(marine_by_transect_from_notes_selected_sum_shoreline, marine_by_tran_from_chris_selected_noI, by="unq_tran", all=TRUE)

#this line is only for adding purposes
marine_by_tran_combined[is.na(marine_by_tran_combined)] <- 0

marine_by_tran_combined <- marine_by_tran_combined %>% 
                          mutate(otter_pres_all = otter_pres + otter_pres_chris) %>% 
                          mutate(unk_bird_pres_all = unk_bird_pres + unk_bird_pres_chris) %>% 
                          mutate(marine_invert_pres_all = marine_invert_pres + marine_invert_pres_chris) %>% 
                          mutate(driftwood_all = driftwood + driftwood_chris) %>% 
                          mutate(seaweed_all = seaweed + seaweed_chris) %>% 
                          mutate(fish_all = fish + fish_chris) %>% 
                          mutate(vector_evidence_all = vector_evidence + vector_evidence_chris) %>% 
                          mutate(marine_evidence_all = marine_evidence + marine_evidence_chris) %>% 
                          mutate(total_marine_evidence_all = total_marine_evidence + total_marine_evidence_chris) %>% 
                          mutate(n_all = n + n_chris)

head(marine_by_tran_combined)
write.csv(marine_by_tran_combined, "C:Biodiversity idea//Output files//marine_by_tran_combined.csv", row.names=FALSE)


# Merging Owen and chris' plot level data to get island level (sho --------
#Owen:
head(marine_by_plot_from_notes_selected)
marine_by_plot_from_notes_selected_shoreline<- marine_by_plot_from_notes_selected %>% filter(plot < 3)
owen_otter<-marine_by_plot_from_notes_selected_shoreline[ ,c("unq_plot", "unq_tran", "unq_isl", "otter_pres")]

#chris
head(marine_by_plot_from_chris)
marine_by_plot_from_chris_noI<-marine_by_plot_from_chris %>% filter(direction != "I") 
marine_by_plot_from_chris_noI$otter_pres<-marine_by_plot_from_chris_noI$otter_pres_chris  
chris_otter<-marine_by_plot_from_chris_noI[, c("unq_plot", "unq_tran", "unq_isl", "otter_pres")]

#did they do the same islands? 
#what's in owen's but not chris'? 
subset(owen_otter, !(unq_isl %in% chris_otter$unq_isl)) %>% select(unq_isl, otter_pres) %>% group_by(unq_isl) %>% summarise_all(list(sum, length))
#CV15, MM07, ST01, ST08, TQ12

#what's in chris's but not owen's? 
subset(chris_otter, !(unq_isl %in% owen_otter$unq_isl)) %>% select(unq_isl, otter_pres) %>% group_by(unq_isl) %>% summarise_all(list(sum, length))
#MM09, MM10, MM11

combined_otter<-rbind(owen_otter, chris_otter)
head(combined_otter)

#code to summarize transects from this data and take out the NNW for eg
# combined_otter_sum_tran <- combined_otter %>% dplyr::select(unq_tran, otter_pres) %>% group_by(unq_tran) %>%  summarise_all(list(otter_pres_all = sum, otter_n_plots=length))
# head(combined_otter_sum_tran)
# combined_otter_sum_tran$takeout <- ifelse(grepl("NE|NW|SE|SW|I",combined_otter_sum_tran$unq_tran), 1, 0)
# combined_otter_sum_tran<-combined_otter_sum_tran %>% filter(takeout==0)
# View(combined_otter_sum_tran)

#with shoreline plots and noI, there should be 10 x 4 = 40 per island. Use at least 20 to make proportion
combined_otter_sum <- combined_otter %>% dplyr::select(unq_isl, otter_pres) %>% group_by(unq_isl) %>%  summarise_all(list(otter_pres_all = sum, otter_n_plots=length))
combined_otter_sum <- combined_otter_sum %>% filter(otter_n_plots > 20)
combined_otter_sum$prop_otter<-combined_otter_sum$otter_pres_all/combined_otter_sum$otter_n_plots
head(combined_otter_sum)
write.csv(combined_otter_sum, "C:Biodiversity idea//Output files//combined_otter_sum.csv", row.names=FALSE)
write.csv(combined_otter_sum, "C://Users//norah//Dropbox//Projects//Owen's MS//Owen_MS//Analysis Data//combined_otter_sum.csv", row.names=FALSE)



#alternative way to look at ist is the mean of the transect
combined_otter_mean_tran <- combined_otter %>% dplyr::select(unq_tran, otter_pres) %>% group_by(unq_tran) %>%  summarise_all(list(otter_pres_all = sum, otter_n_plots=length))
head(combined_otter_mean_tran)

combined_otter_mean_tran<- combined_otter_mean_tran %>% filter(otter_n_plots > 8)
combined_otter_mean_tran$prop_otter<-combined_otter_mean_tran$otter_pres_all/combined_otter_mean_tran$otter_n_plots
combined_otter_mean_tran$unq_isl<-str_sub(combined_otter_mean_tran$unq_tran, end=4)

combined_otter_mean_tran_isl<-combined_otter_mean_tran %>% group_by(unq_isl) %>%  summarise_if(is.numeric, mean, na.rm=TRUE)
head(combined_otter_mean_tran_isl)
write.csv(combined_otter_mean_tran_isl, "C://Users//norah//Dropbox//Projects//Owen's MS//Owen_MS//Analysis Data//combined_otter_mean_tran_isl.csv", row.names=FALSE)





### Just looking at edge plots:
head(marine_by_plot_from_notes_selected)
marine_by_plot_from_notes_selected_shoreline_edge<- marine_by_plot_from_notes_selected %>% filter(plot == 1)
owen_otter_edge<-marine_by_plot_from_notes_selected_shoreline_edge[ ,c("unq_plot", "unq_tran", "unq_isl", "otter_pres")]

owen_otter_edge_isl <- owen_otter_edge %>% group_by(unq_isl) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
head(owen_otter_edge_isl)
