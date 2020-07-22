arch_sites<-read.csv("C:Food web idea//Data by person//Kalina.data//arch_sites.csv", header=TRUE, sep=",")
head(arch_sites)


arch_sites$midden_feature <- ifelse(grepl("Shell Midden", arch_sites$TY_TYPOLOGY), "yes", "no")
arch_sites$CMT <- ifelse(grepl("Culturally Modified Tree", arch_sites$TY_TYPOLOGY), "yes", "no")
arch_sites$clam_garden <- ifelse(grepl("Clam Garden", arch_sites$TY_TYPOLOGY), "yes", "no")
arch_sites$fish_feature <- ifelse(grepl("Fish Trap", arch_sites$TY_TYPOLOGY), "yes", "no")
arch_sites$canoe_skid <- ifelse(grepl("Canoe Skid", arch_sites$TY_TYPOLOGY), "yes", "no")
arch_sites$any_arch <- ifelse(grepl("PRECONTACT|TRADITIONAL USE|Shell Midden|HISTORIC", arch_sites$TY_TYPOLOGY), "yes", "no")



arch_sites_selected<- arch_sites %>% dplyr::select(BORDENNUMBER, MR_GISUTMEASTING, MR_GISUTMNORTHING, 
                                            midden_feature, fish_feature, CMT, clam_garden, canoe_skid, any_arch) %>% 
                                            rename(site_id=BORDENNUMBER , easting=MR_GISUTMEASTING , northing=MR_GISUTMNORTHING)
head(arch_sites_selected)

write.csv(arch_sites_selected, "C:Biodiversity idea//Output files//arch_sites_selected.csv", row.names=FALSE)








#### sode code for combing arch sites:
#arch sites paired
arch_sites_distance_tran<-read.csv("Biodiversity idea//Output files//paired_arch_by_radius_300.csv")
head(arch_sites_distance_tran)
length(unique(arch_sites_distance_tran$unq_tran))
#84 unique transects if using 300m radius 

##adding in arch data from output file fed from arch sites cleaning.R
arch_data<-read.csv("C:Biodiversity idea//Output files//arch_sites_selected.csv")
head(arch_data)
arch_data_simple<-arch_data[ , c("site_id", "CMT", "clam_garden", "midden_feature", "fish_feature", "canoe_skid")]
head(arch_data_simple)

# arch_merged<-merge(arch_sites_distance_tran, arch_data_simple, by="site_id", all=TRUE)
# head(arch_merged)
# 
# fish_richness_merged_tran_arch<-merge(fish_richness_merged_tran, arch_sites_distance_tran, by="unq_tran", all.x=TRUE)
# 
# #head(fish_richness_merged_tran_arch)
# length(unique(fish_richness_merged_tran_arch$unq_tran))
# 
# 
# fish_richness_merged_tran_arch<-merge(fish_richness_merged_tran_arch, arch_data_simple, by="site_id", all.x=TRUE)
# View(fish_richness_merged_tran_arch)

# #for sem:
# fish_richness_merged_tran_arch$midden_feature_sem<-as.character(fish_richness_merged_tran_arch$midden_feature)
# fish_richness_merged_tran_arch$midden_feature_sem<- dplyr::recode(fish_richness_merged_tran_arch$midden_feature_sem, yes = "1", no="0")
# fish_richness_merged_tran_arch$midden_feature_sem[is.na(fish_richness_merged_tran_arch$midden_feature_sem)] <- 0
# fish_richness_merged_tran_arch$midden_feature_sem<-as.numeric(fish_richness_merged_tran_arch$midden_feature_sem)
# 
# fish_richness_merged_tran_arch$fish_feature_sem<-as.character(fish_richness_merged_tran_arch$fish_feature)
# fish_richness_merged_tran_arch$fish_feature_sem<-dplyr::recode(fish_richness_merged_tran_arch$fish_feature_sem, yes = "1", no="0")
# fish_richness_merged_tran_arch$fish_feature_sem[is.na(fish_richness_merged_tran_arch$fish_feature_sem)] <- 0
# fish_richness_merged_tran_arch$fish_feature_sem<-as.numeric(fish_richness_merged_tran_arch$fish_feature_sem)
# 
# fish_richness_merged_tran_arch$canoe_skid_sem<-as.character(fish_richness_merged_tran_arch$canoe_skid)
# fish_richness_merged_tran_arch$canoe_skid_sem<-dplyr::recode(fish_richness_merged_tran_arch$canoe_skid_sem, yes = "1", no="0")
# fish_richness_merged_tran_arch$canoe_skid_sem[is.na(fish_richness_merged_tran_arch$canoe_skid_sem)] <- 0
# fish_richness_merged_tran_arch$canoe_skid_sem<-as.numeric(fish_richness_merged_tran_arch$canoe_skid_sem)
# 
# 
# fish_richness_merged_tran_arch$CMT<-as.factor(fish_richness_merged_tran_arch$CMT)
# fish_richness_merged_tran_arch$clam_garden<-as.factor(fish_richness_merged_tran_arch$clam_garden)
# fish_richness_merged_tran_arch$midden_feature<-factor(fish_richness_merged_tran_arch$midden_feature)
# fish_richness_merged_tran_arch$fish_feature<-as.factor(fish_richness_merged_tran_arch$fish_feature)
# fish_richness_merged_tran_arch$canoe_skid<-as.factor(fish_richness_merged_tran_arch$canoe_skid)
# 
