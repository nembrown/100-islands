install.packages("lavaan", dependencies = TRUE)
install.packages("ggm", dependencies = TRUE)

library(lavaan)

head(fish_richness_merged_isl)

#pair down the 
fish_richness_merged_isl_colnames<-c("fish_biomass_bym3_mean", "fish_bycatch_biomass")

N15_model<-'#latent variables as responses
            fish
            
            
            #latent variables measurement model
            human_pres =~ midden_feature







                        '
