library(piecewiseSEM)


dat <- data.frame(x1 = runif(50), y1 = runif(50), y2 = runif(50), y3 = runif(50))
model <- psem(lm(y1 ~ x1, dat), lm(y1 ~ y2, dat), lm(y2 ~ x1, dat), lm(y3 ~ y1, dat))

model <- psem(lm(y1 ~ x1 + y2, dat), lm(y2 ~ x1, dat), lm(y3 ~ y1, dat))

#To evaluate the model, we call summary on the psem object.

summary(model, .progressBar = F)




#### my data need to be all together
fish_richness_merged_isl<-read.csv("C:Biodiversity idea//Output files//fish_richness_merged_isl.csv")
head(fish_richness_merged_isl)
