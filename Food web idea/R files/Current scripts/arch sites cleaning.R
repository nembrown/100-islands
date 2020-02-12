arch_sites<-read.csv("C:Food web idea//Data by person//Kalina.data//arch_sites.csv", header=TRUE, sep=",")
View(arch_sites)


arch_sites$midden_feature <- ifelse(grepl("Shell Midden", arch_sites$TY_TYPOLOGY), "yes", "no")
arch_sites$CMT <- ifelse(grepl("Culturally Modified Tree", arch_sites$TY_TYPOLOGY), "yes", "no")
arch_sites$clam_garden <- ifelse(grepl("Clam Garden", arch_sites$TY_TYPOLOGY), "yes", "no")
arch_sites$fish_feature <- ifelse(grepl("Fish Trap", arch_sites$TY_TYPOLOGY), "yes", "no")
arch_sites$canoe_skid <- ifelse(grepl("Canoe Skid", arch_sites$TY_TYPOLOGY), "yes", "no")

arch_sites_selected<- arch_sites %>% dplyr::select(BORDENNUMBER, MR_GISUTMEASTING, MR_GISUTMNORTHING, 
                                            midden_feature, fish_feature, CMT, clam_garden, canoe_skid) %>% 
                                            rename(site_id=BORDENNUMBER , easting=MR_GISUTMEASTING , northing=MR_GISUTMNORTHING)
head(arch_sites_selected)

write.csv(arch_sites_selected, "C:Biodiversity idea//Output files//arch_sites_selected.csv", row.names=FALSE)
