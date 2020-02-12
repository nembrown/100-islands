## CULTURALLY IMPORTANT SPECIES LIST
# Create column - cult important (yes/no), then merge
cult.imp.sp <- c("rhgr",
                 "vevi",
                 "frca",
                 "lyam",
                 "midi",
                 "pogl",
                 "pomu",
                 "ptaq",
                 "cosp",
                 "laja",
                 "goob",
                 "drro",
                 "poan",
                 "epan",
                 "amal",
                 "gash",
                 "rusp",
                 "vaov",
                 "vapa",
                 "ribr",
                 "rupa",
                 "rila",
                 "rhpu",
                 "loin",
                 "rupe",
                 "opho",
                 "ronu",
                 "thpl",
                 "tshe",
                 "chno",
                 "alru",
                 "mafu",
                 "pisi",
                 "tabr",
                 "pico")
# Creating new dataframe, filter by cult sp
plant_data_cult <- plant_data %>% filter(species %in% cult.imp.sp)
view(plant_data_cult)
# Plot bar chart for cult sp (how many instances of a CIP per node) *not the best one
ggplot(data = plant_data_cult %>% filter(pres_abs == "1"), aes(x = node, fill = node)) +
  geom_bar()
# Create wide dataframe (node by CIP) and fill with instances of presence
plant_data_cult_wide <- plant_data_cult %>% group_by(node, species) %>%
  summarise(sum_abundance = sum(pres_abs, na.rm = TRUE)) %>%
  spread(species, sum_abundance)
# Create new DF for richness by node
plant_data_cult_richness <- plant_data_cult_wide %>% select(node)
# Add new col richness, dont wanna count node for sp (-1), will give 1 sp number per node
plant_data_cult_richness$richness <- specnumber(plant_data_cult_wide[,-1])
# Order nodes by richness
plant_data_cult_richness$node <- factor(plant_data_cult_richness$node,
                                        levels = plant_data_cult_richness$node[order(plant_data_cult_richness$richness)])
# Get rid of NAs
plant_data_cult_richness <- na.omit(plant_data_cult_richness)