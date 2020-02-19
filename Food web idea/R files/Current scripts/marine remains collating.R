
marine_by_plot_from_plants<- read.csv("C:Food web idea//Data by person//Norah.data/marine_by_plot_from_plants.csv")
head(marine_by_plot_from_plants)




marine_by_plot_from_notes<- read.csv("C:Food web idea//Data by person//Owen's data//100Islands_Fitzpatrick_plot.csv", header=TRUE, sep=",")
head(marine_by_plot_from_notes)
# View(marine_by_plot_from_notes)
marine_by_plot_from_notes$otter_pres <- ifelse(grepl("otter|ottre|scat|latrine|den|otter-y|ottery|nearotter|otters", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$eagle_pres <- ifelse(grepl("eagle|eagle's nest", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$raven_pres <- ifelse(grepl("raven", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$unk_bird_pres <- ifelse(grepl("guano|feather|bird poo|bird", marine_by_plot_from_notes$notes), 1, 0)

marine_by_plot_from_notes$marine_invert_pres <- ifelse(grepl("bivalve|shell|crab|abalone|ablone|limpet|snail|clam|scallop|mussel|mussell|shells|urchin|claw|carapace|tube worm", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$driftwood <- ifelse(grepl("driftwood", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$midden <- ifelse(grepl("shelly|midden", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$seaweed <- ifelse(grepl("seaweed|algae", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$mink <- ifelse(grepl("mink", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$fish <- ifelse(grepl("fish|rockfish|marine vertebrae", marine_by_plot_from_notes$notes), 1, 0)
marine_by_plot_from_notes$mammal_bones <- ifelse(grepl("mammal bones|mammale bones", marine_by_plot_from_notes$notes), 1, 0)
