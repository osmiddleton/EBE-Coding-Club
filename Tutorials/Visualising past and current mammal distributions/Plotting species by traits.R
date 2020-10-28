

library(sf)
library(tidyverse)
library(scico)
library(cowplot)

# Exploring current and potential distribution of mammals

# Load data
c <- read.csv("../Current_Species_Matrix.csv")
pn <- read.csv("../PN_Species_Matrix.csv")
traits <- read.csv("../Trait_data.csv")
continent <- st_read("../continentbehrmann/continentprojected.shp")


# Find the biggest species in each cell
large <- levels(factor(traits[traits$Marine == 0 &
                                traits$Freshwater == 0 &
                                traits$Diet.Vertebrate >= 70,]$Binomial.1.2))

large.cols <- which(colnames(c) %in% large)

# Current
c.update <- c[c(1:8, large.cols)]
colnames(c.update)
c.tidy <- gather(c.update, "Binomial.1.2", "Presence", Acinonyx_jubatus:Vulpes_vulpes)
c.tidy <- merge(c.tidy, traits[c(1,12)])
c.tidy.update <- c.tidy %>% filter(Presence == 1)

c.tidy.update <- c.tidy.update %>% filter(Binomial.1.2 %in% large)

c.summary <- c.tidy.update %>% group_by(Community,x,y) %>% summarise(maximum = max(Mass.g))

c.summary$Scenario <- "Current"

(c.plot <- ggplot() +
  geom_tile(data = c.summary, 
            aes(x = x, y = y, fill = maximum/1000)) +
  geom_sf(data = continent, fill = NA) +
  scale_fill_scico(palette = "lajolla", name = "Mass (Kg)") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.grid = element_blank()))


# Present-natural
pn.update <- pn[c(1:8, large.cols)]
colnames(pn.update)
pn.tidy <- gather(pn.update, "Binomial.1.2", "Presence", Acinonyx_jubatus:Zenkerella_insignis)
pn.tidy <- merge(pn.tidy, traits[c(1,12)], by = "Binomial.1.2")
pn.tidy.update <- pn.tidy %>% filter(Presence == 1)

pn.tidy.update <- pn.tidy.update %>% filter(Binomial.1.2 %in% large)
pn.summary <- pn.tidy.update %>% group_by(Community,x,y) %>% summarise(maximum = max(Mass.g))

pn.summary$Scenario <- "Present-natural"


(pn.plot <- ggplot() +
  geom_tile(data = pn.summary, 
            aes(x = x, y = y, fill = maximum/1000)) +
  geom_sf(data = continent, fill = NA) +
  scale_fill_scico(palette = "lajolla", name = "Mass (Kg)") +
  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid = element_blank()))

master.plot <- cowplot::plot_grid(c.plot, pn.plot, ncol = 1)


master.summary <- rbind(pn.summary, c.summary)

(master.plot <- ggplot() +
    geom_tile(data = master.summary, 
              aes(x = x, y = y, fill = maximum/1000)) +
    geom_sf(data = continent, fill = NA) +
    scale_fill_scico(palette = "lajolla", name = "Mass (Kg)") +
    theme_bw() +
    facet_wrap(.~Scenario, ncol = 1) +
    labs(x = "", y = "") +
    theme(panel.grid = element_blank()))


ggsave("../Largest carnivore in grids.tiff", master.plot,
       width = 9, height = 6, units = "in")





  