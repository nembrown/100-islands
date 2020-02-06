install.packages("lavaan", dependencies = TRUE)
install.packages("ggm", dependencies = TRUE)

library(lavaan)

head(fish_richness_merged_isl)

N15_model<-'#latent variables
            vectors=~fish_biomass_bym3_mean + 



                        '
