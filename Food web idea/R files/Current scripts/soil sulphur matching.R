cross_ref_list_samples<- read.csv("C:Food web idea//Data by person//Owen's data//2017_samples_CrossRefList.csv", header=TRUE, sep=",")
head(cross_ref_list_samples)
cross_ref_list_samples$moe_code<- paste("S1634-",cross_ref_list_samples$Sample)
cross_ref_list_samples$moe_code<- gsub(" ", "", cross_ref_list_samples$moe_code, fixed = TRUE)



sulphur_subset_moe<- read.csv("C:Food web idea//Data by person//Owen's data//sulphur_subset_moe.csv", header=TRUE, sep=",")
head(sulphur_subset_moe)



sulphur_subset_merged<-merge(cross_ref_list_samples, sulphur_subset_moe, by="moe_code", all.y=TRUE)
View(sulphur_subset_merged)




Karson_soil<- read.csv("C:Food web idea//Data by person//Owen's data//Norah_soil_code_only.csv", header=TRUE, sep=",")
head(Karson_soil)
