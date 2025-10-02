# TradMod WP2 - NBR survey cleaning script
#Description of the data
#Year - 2023
#Who - Morgane Kerdoncuff
#Project - TradMod
#Funding - NFR
#Place - University of Bergen, Norway

#### DATA FILE - SITE REPARTITION ####

#
## Package loading

library(tidyverse)
library(purrr)

#
## Data loading

siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
landuse_full <- read.csv("data/cleandata/NBR_FullLandUse.csv", sep=",")

#
## Data preparation - landscape grouping per type of field

sites <- purrr::reduce(list(
  subset(siteinfo_full, select = c(SiteID, EcoZone, EPSG.25832_X, EPSG.25832_Y, Livestock)),
  subset(landuse_full, select = c(SiteID, FieldType))
), dplyr::left_join)

#
## Export csv
write.csv(sites, file = "data/NBR_SiteRepartition.csv")

#### INTRODUCTION ####
# 
# # Landscape pie charts
# 
# ## Package loading
# library(tidyverse)
# library(ggplot2)
# 
# ## Data loading
# siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
# landscape_full <- read.csv("data/cleandata/NBR_FullLandscape.csv")
# 
# ## Data preparation - landscape grouping per type of field
# 
# ### Merge habitat type & landscape dataset
# landscapegp <- left_join(subset(siteinfo_full, select = c(SiteID, Habitat)), landscape_full)
# landscapegp <- subset(landscapegp, select = -c(SiteID))
# 
# ### Simplify categories
# landscapegp <- landscapegp |> 
#   mutate(Forest = NonProductiveForest_percent + ProductiveForest_percent) |> 
#   mutate(Cultivated = FullyCultivatedLand_percent + SuperficiallyCultivatedLand_percent)
# 
# ### Rename categories
# names(landscapegp) <- gsub("Freshwater_percent", "Freshwater", names(landscapegp))
# names(landscapegp) <- gsub("Infield_percent", "Infield", names(landscapegp))
# names(landscapegp) <- gsub("Outfield_percent", "Outfield", names(landscapegp))
# names(landscapegp) <- gsub("Sea_percent", "Sea", names(landscapegp))
# names(landscapegp) <- gsub("Wetland_percent", "Wetland", names(landscapegp))
# names(landscapegp) <- gsub("Infrastructure_percent", "Infrastructure", names(landscapegp))
# 
# ### Select final categories
# landscapegp <- subset(landscapegp, select = -c(FullyCultivatedLand_percent, SuperficiallyCultivatedLand_percent, NonProductiveForest_percent, ProductiveForest_percent))
# 
# ### Make average
# landscapegp <- landscapegp |> 
#   group_by(Habitat) |> 
#   summarise(Infield = mean(Infield), 
#             Outfield = mean(Outfield), 
#             Forest = mean(Forest),
#             Wetland = mean(Wetland),
#             Freshwater = mean(Freshwater),
#             Sea = mean(Sea),
#             Cultivated = mean(Cultivated),
#             Infrastructure = mean(Infrastructure))
# 
# ### Rearrange factors
# longlandscapegp <- landscapegp |>
#   pivot_longer(cols = c(Infield, Outfield, Cultivated, Forest, Wetland, Freshwater, Infrastructure, Sea), names_to = "landcover", values_to = "percentcover") |> 
#   arrange(percentcover) |> 
#   mutate(landcover = factor(landcover, levels=c("Cultivated", "Infield", "Outfield", "Forest", "Wetland", "Freshwater", "Sea", "Infrastructure")))
# 
# ## Graphs
# 
# ### Coastal outfield
# piecoastal <- ggplot(filter(longlandscapegp, Habitat == "coastal heathland"), aes(x = "", y = percentcover, fill = landcover)) +
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0) +
#   scale_fill_manual(values = c("#DDCC77", "#CC6677", "#AA4499", "#882255", "#117733", "#44AA99", "#88CCEE", "#332288")) +
#   theme_void() +
#   theme(axis.text.x=element_blank(),
#         legend.position = "none")
# piecoastal
# ggsave("illustrations/piecoastal.png", plot = piecoastal, width = 3.5, height = 3.5)
# 
# ### Infield
# pieinfield <- ggplot(filter(longlandscapegp, Habitat == "permanent grassland"), aes(x = "", y = percentcover, fill = landcover)) +
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0) +
#   scale_fill_manual(values = c("#DDCC77", "#CC6677", "#AA4499", "#882255", "#117733", "#44AA99", "#88CCEE", "#332288")) +
#   theme_void() +
#   theme(axis.text.x=element_blank(),
#         legend.position = "none")
#   pieinfield
# ggsave("illustrations/pieinfield.png", plot = pieinfield, width = 3.5, height = 3.5)
# 
# ### Moutain outfields
# piemountain <- ggplot(filter(longlandscapegp, Habitat == "subalpine heathland"), aes(x = "", y = percentcover, fill = landcover)) +
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0) +
#   scale_fill_manual(values = c("#DDCC77", "#CC6677", "#AA4499", "#882255", "#117733", "#44AA99", "#88CCEE", "#332288")) +
#   theme_void() +
#   theme(axis.text.x = element_blank(),
#         legend.title = element_blank(),
#         legend.position = "none")
# piemountain
# ggsave("illustrations/piemountain.png", plot = piemountain, width = 3.5, height = 3.5)
# 
# ### Legend
# pielegend <- ggplot(filter(longlandscapegp, Habitat == "subalpine heathland"), aes(x = "", y = percentcover, fill = landcover)) +
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0) +
#   scale_fill_manual(values = c("#DDCC77", "#CC6677", "#AA4499", "#882255", "#117733", "#44AA99", "#88CCEE", "#332288")) +
#   theme_void() +
#   theme(axis.text.x = element_blank(),
#         legend.title = element_blank(),
#         legend.position = "bottom")
# pielegend
# ggsave("illustrations/pielegend.png", plot = pielegend, width = 7, height = 7)
# 
# 
#### STUDY DESIGN ####

# Climate gradients

## Package loading
library(tidyverse)
library(purrr)
library(ggplot2)
library(ggpubr) # Function ggarrange for several plots on same file

## Data loading
siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
samplingarea_full <- read.csv("data/cleandata/NBR_FullArea20x20.csv")
landuse_full <- read.csv("data/cleandata/NBR_FullLandUse.csv", sep=",")
climate_full <- read.csv("data/cleandata/NBR_FullClimate.csv", sep=",")
soilchem_full <- read.csv("data/cleandata/NBR_FullSoilChem.csv", sep=",")

## Site level soil chemistry
soilchem <- soilchem_full |> 
  group_by(SiteID) |> 
  summarise(
    LOI = mean(LOI),
    N = mean(TotalN_percentDM),
    P = mean(P.Al_mg.100g),
    pH = mean(pH),
    Na = mean(Na.Al_mg.100g)
  )

## Merge data
barprep <- purrr::reduce(list(siteinfo_full, subset(landuse_full, select = c(SiteID, FieldType)), samplingarea_full, climate_full, soilchem), dplyr::left_join)

## Remove UC1
barprep <- filter(barprep, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")

## Climate x longitude

### Whole range July temperature x longitude
wratemplong_lm <- ggplot(
  barprep, aes(x = EPSG.25832_X, y = avgtempJuly)
) +
  geom_point(aes(colour = EcoZone)) +
  xlab("") +
  ylab("July temperature (°C)") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4", "mountain" = "purple3")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
wratemplong_lm

### Short range July temperature x longitude
sratemplong_lm <- ggplot(
  filter(barprep, EcoZone != "mountain"),
  aes(x = EPSG.25832_X, y = avgtempJuly)
) +
  geom_point(aes(colour = EcoZone)) +
  xlab("") +
  ylab("July temperature (°C)") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size = 9),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
sratemplong_lm

### Whole range Janv-July temperature extent x longitude
wratempdifflong_lm <- ggplot(
  barprep, aes(x = EPSG.25832_X, y = maxtempJuly-mintempJan)
) +
  geom_point(aes(colour = EcoZone)
  ) +
  xlab("") +
  ylab("Temperature range (°C)") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4", "mountain" = "purple3")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size = 9),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
wratempdifflong_lm

### Short range Janv-July temperature extent x longitude
sratempdifflong_lm <- ggplot(
  filter(barprep, EcoZone != "mountain"),
  aes(x = EPSG.25832_X, y = maxtempJuly-mintempJan)
) +
  geom_point(aes(colour = EcoZone)) +
  xlab("") +
  ylab("Temperature range (°C)") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
sratempdifflong_lm

### Whole range precipitation x longitude
wraprecilong_lm <- ggplot(
  barprep, aes(x = EPSG.25832_X, y = annualprecipitation)
) +
  geom_point(aes(colour = EcoZone)) +
  xlab("") +
  ylab("Annual precipitation (mm)") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4", "mountain" = "purple3")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
wraprecilong_lm

### Short range precipitation x longitude
sraprecilong_lm <- ggplot(
  filter(barprep, EcoZone != "mountain"),
  aes(x = EPSG.25832_X, y = annualprecipitation)
) +
  geom_point(aes(colour = EcoZone)) +
  xlab("") +
  ylab("Annual precipitation (mm)") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size = 9),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
sraprecilong_lm

### Whole range elevation x longitude
wraelevlong_lm <- ggplot(
  barprep, aes(x = EPSG.25832_X, y = Elevation, , colour = EcoZone)
) +
  geom_point(aes(colour = EcoZone)) +
  xlab("Longitude") +
  ylab("Elevation (m)") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4", "mountain" = "purple3")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size = 9),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
wraelevlong_lm

### Short range elevation x longitude
sraelevlong_lm <- ggplot(
  filter(barprep, EcoZone != "mountain"),
  aes(x = EPSG.25832_X, y = Elevation)
) +
  geom_point(aes(colour = EcoZone)) +
  xlab("Longitude") +
  ylab("Elevation (m)") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
sraelevlong_lm

## Climate x elevation

### Whole range July temperature x elevation
wratempelev_lm <- ggplot(
  barprep, aes(x = Elevation, y = avgtempJuly)
) +
  geom_point(aes(colour = EcoZone)) +
  xlab("") +
  ylab("") +
  # ylab("July temperature (°C)") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4", "mountain" = "purple3")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
wratempelev_lm

### Short range July temperature x elevation
sratempelev_lm <- ggplot(
  filter(barprep, EcoZone != "mountain"),
  aes(x = Elevation, y = avgtempJuly)
) +
  geom_point(aes(colour = EcoZone)) +
  xlab("") +
  ylab("") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
sratempelev_lm

### Whole range Janv-July temperature extent x elevation
wratempdiffelev_lm <- ggplot(
  barprep, aes(x = Elevation, y = maxtempJuly-mintempJan)
) +
  geom_point(aes(colour = EcoZone)
) +
  xlab("") +
  ylab("") +
  # ylab("Temperature range (°C)") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4", "mountain" = "purple3")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size = 9),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
wratempdiffelev_lm

### Short range Janv-July temperature extent x elevation
sratempdiffelev_lm <- ggplot(
  filter(barprep, EcoZone != "mountain"),
  aes(x = Elevation, y = maxtempJuly-mintempJan)
) +
  geom_point(aes(colour = EcoZone)
  ) +
  xlab("") +
  ylab("") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size = 9),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
sratempdiffelev_lm

### Whole range precipitation x elevation
wraprecielev_lm <- ggplot(
  barprep, aes(x = Elevation, y = annualprecipitation)
) +
  geom_point(aes(colour = EcoZone)) +
  xlab("Elevation (m)") +
  ylab("") +
  # ylab("Annual precipitation (mm)") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4", "mountain" = "purple3")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
wraprecielev_lm

### Short range precipitation x elevation
sraprecielev_lm <- ggplot(
  filter(barprep, EcoZone != "mountain"),
  aes(x = Elevation, y = annualprecipitation)
) +
  geom_point(aes(colour = EcoZone)) +
  xlab("Elevation (m)") +
  ylab("") +
  scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
sraprecielev_lm


## Plot arrangement

# According to x axis

# gradientdistrilong <- ggarrange(wratemplong_lm, sratemplong_lm, wratempdifflong_lm, sratempdifflong_lm, wraprecilong_lm, sraprecilong_lm, wraelevlong_lm, sraelevlong_lm, ncol = 2, nrow = 4, labels = c("A", "B", "C", "D", "E", "F", "G", "H"), common.legend = TRUE)
# ggsave(filename = "illustrations/NBR_ClimateLandformDistri_longitude.png", plot = gradientdistrilong, width = 13, height = 18, units = "cm")
# 
# gradientdistrielev <- ggarrange(wratempelev_lm, sratempelev_lm, wratempdiffelev_lm, sratempdiffelev_lm, wraprecielev_lm, sraprecielev_lm, ncol = 2, nrow = 3, labels = c("A", "B", "C", "D", "E", "F"), common.legend = TRUE)
# ggsave(filename = "illustrations/NBR_ClimateLandformDistri_elevation.png", plot = gradientdistrielev, width = 13, height = 14.5, units = "cm")

# According to range

gradientdistriwra <- ggarrange(
  wratemplong_lm, wratempelev_lm, wratempdifflong_lm, wratempdiffelev_lm, wraprecilong_lm, wraprecielev_lm, wraelevlong_lm, 
  ncol = 2, 
  nrow = 4, 
  # labels = c("A", "B", "C", "D", "E", "F", "G"), 
  common.legend = TRUE 
  # font.label = list(size = 12),
  # hjust = 0.5
  )
ggsave(filename = "illustrations/NBR_DesignClimateDistri.png", plot = gradientdistriwra, width = 13, height = 18, units = "cm")

# gradientdistrisra <- ggarrange(sratemplong_lm, sratempelev_lm, sratempdifflong_lm, sratempdiffelev_lm, sraprecilong_lm, sraprecielev_lm, sraelevlong_lm, ncol = 2, nrow = 4, labels = c("A", "B", "C", "D", "E", "F", "G"), common.legend = TRUE)
# ggsave(filename = "illustrations/NBR_ClimateLandformDistri_sra.png", plot = gradientdistrisra, width = 13, height = 18, units = "cm")

# Description plant communities

# Package loading
library(tidyverse)
library(purrr)

# Data loading
siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
plant_full <- read.csv("data/cleandata/NBR_FullPlantComm.csv")
landuse_full <- read.csv("data/cleandata/NBR_FullLandUse.csv", sep=",")

# Preparation

## Site ID
explanatory_site <- full_join(
  subset(siteinfo_full, select = c(SiteID, EcoZone, EPSG.25832_X, EPSG.25832_Y, Livestock)),
  subset(landuse_full, select = c(SiteID, FieldType))
)
explanatory_site <- filter(explanatory_site, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")
write.csv(explanatory_site, file = "data/NBR_SiteRepartition.csv")

## Remove sites
plant <- filter(plant_full, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")

## Average species abundance per site
plant <- plant |> 
  group_by(SiteID, Species) |> 
  summarise(Abundance = mean(Abundance)) |> 
  filter(Abundance > 0)

## Functional group attribution
plant <- plant |>
  # assign functional group to species
  mutate(FunctionalGroup = case_when(
    grepl("Achillea|Alchemilla|Anagalis|Anemone|Angelica|Anthriscus|Armeria|Bartsia|Campanula|Cardamine|Cerastium|Cirsium|Conopodium|Dactylorhiza|Digitalis|Epilobium|Euphrasia|Fraxinus|Galeopsis|Galium|Geranium|Gnaphalium|Hieracium|Hypericum|Hypochaeris|Lathyrus|Leontodon|Lotus|Melampyrum|Moneses|Myosotis|Narthecium|Oxalis|Pedicularis|Pinguicula|Plantago|Potentilla|Prunella|Ranunculus|Rumex|Sagina|Sedum|Senecio|Silene|Solidago|Stellaria|Succisa|Taraxacum|Trientalis|Trifolium|Urtica|Valeriana|Veronica|Vicia|Viola", Species) ~ "DIV_forbs",
    grepl("Agrostis|Aira|Alopecurus|Anthoxanthum|Bromopsis|Calamagrostis|Carex|Dactylis|Danthonia|Deschampsia|Eriophorum|Festuca|Holcus|Juncus|Lolium|Luzula|Molinia|Nardus|Phleum|Poa|Trichophorum", Species) ~ "DIV_monocotyledons",
    grepl("Andromeda|Arctostaphylos|Betula|Calluna|Chamaepericlymenum|Empetrum|Erica|Juniperus|Loiseleuria|Picea|Polygala|Polygonum|Populus|Prunus|Rubus|Salix|Sorbus|Ulmus|Vaccinium", Species) ~ "DIV_woody",
    grepl("Athyrium|Blechnum|Dryopteris|Gymnocarpium|Phegopteris|Polypodium|Pteridium", Species) ~ "DIV_ferns",
    .default = "DIV_cryptogams"
  ))

## Reference table
habitat <- left_join(explanatory_site, plant)

## Coastal outfields
coastal <- filter(habitat, FieldType == "outfield" & EcoZone == "coastal")
coastal <- coastal |> 
  group_by(Species) |> 
  summarise(Abundance = mean(Abundance))

## Mountain outfields
mountain <- filter(habitat, FieldType == "outfield" & EcoZone == "mountain")
mountain <- mountain |> 
  group_by(Species) |> 
  summarise(Abundance = mean(Abundance))

## Fjord outfield
fjord <- filter(habitat, FieldType == "outfield" & EcoZone == "fjord")
fjord <- fjord |> 
  group_by(Species) |> 
  summarise(Abundance = mean(Abundance))

## Infields
infield <- filter(habitat, FieldType == "infield")
infield <- infield |> 
  group_by(Species, SiteID) |> 
  summarise(Abundance = mean(Abundance)) |> 
  filter(Abundance > 5) |> 
  pivot_wider(names_from = SiteID, values_from = Abundance)

# # Description field management
# 
# ## Packages
# library(tidyverse)
# 
# ## Data loading
# siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
# landuse_full <- read.csv("data/cleandata/NBR_FullLandUse.csv", sep=",")
# 
# ## Base dataset
# management <- left_join(
#   subset(siteinfo_full, select = c(SiteID, EcoZone)),
#   landuse_full
# )
# management <- filter(management, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")
# 
# ### Outfield
# outfield <- filter(management, FieldType == "outfield")
# 
# ### Coastal outfield
# coastal <- filter(management, EcoZone == "coastal" & FieldType == "outfield")
# 
# ### Fjord outfield
# fjord <- filter(management, EcoZone == "fjord" & FieldType == "outfield")
# 
# ### Mountain outfield
# mountain <- filter(management, EcoZone == "mountain" & FieldType == "outfield")
# 
# ### Infields
# infield <- filter(management, FieldType == "infield")
# 
# ### Cattle
# cattle <- filter(management, FieldType == "infield" & Livestock1 == "cattle")
# 
# ### Sheep
# sheep <- filter(management, FieldType == "infield" & Livestock1 == "sheep")
# 
# ### Goat
# goat <- filter(management, FieldType == "infield" & Livestock1 == "goat")

# Management graphs

## Packages
library(tidyverse)
library(ggplot2)
library(ggpubr)

## Data loading
siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
landuse_full <- read.csv("data/cleandata/NBR_FullLandUse.csv", sep=",")

## Base dataset
livestock <- left_join(subset(siteinfo_full, select = c(SiteID, EcoZone, EPSG.25832_X, EPSG.25832_Y, Livestock)), landuse_full)
livestock <- filter(livestock, FieldType == "infield")

## Flock size
flocksize_box <- livestock |> 
  ggplot(aes(x = as.factor(Livestock), y = FlockSize1_adults, fill = Livestock)) +
  geom_boxplot() +
  xlab("") +
  ylab("Number of heads") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
flocksize_box

## Yearly field occupation
month_box <- livestock |> 
  ggplot(aes(x = as.factor(Livestock), y = YearlyGrazing1_month, fill = Livestock)) +
  geom_boxplot() +
  xlab("") +
  ylab("Period on field (months/year)") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
month_box

## Average stocking density
stock_box <- livestock |> 
  ggplot(aes(x = as.factor(Livestock), y = AvgStockDensity_perha, fill = Livestock)) +
  geom_boxplot() +
  xlab("") +
  ylab("Average stocking density (LU/ha)") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
stock_box

## Area of the selected grazing field
fieldsize_box <- livestock |> 
  ggplot(aes(x = as.factor(Livestock), y = SelectedFieldArea_ha, fill = Livestock)) +
  geom_boxplot() +
  xlab("") +
  ylab("Area of visited fields (ha)") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
fieldsize_box

managementgraph <- ggarrange(
  flocksize_box, stock_box, month_box, fieldsize_box,
  ncol = 2, 
  nrow = 2 
  # labels = c("A", "B", "C", "D"),
  # font.label = list(size = 12),
  # hjust = 0.5
  )
ggsave(filename = "illustrations/NBR_DesignManagement.png", plot = managementgraph, width = 12, height = 12, units = "cm")


#### MAIN RESULTS FJORD ####

# R1 - Regional-scale

## Package loading
library(tidyverse)
library(purrr)
library(ggplot2)
library(ggpubr) # Function ggarrange for several plots on same file

## Data loading
siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
plant_full <- read.csv("data/cleandata/NBR_FullPlantComm.csv")
landuse_full <- read.csv("data/cleandata/NBR_FullLandUse.csv", sep=",")
soilchem_full <- read.csv("data/cleandata/NBR_FullSoilChem.csv", sep=",")
meso_full <- read.csv("data/cleandata/NBR_FullMesobio.csv", sep=",")

## Preparation

### Site ID
explanatory_site <- full_join(
  subset(siteinfo_full, select = c(SiteID, EcoZone, EPSG.25832_X, EPSG.25832_Y, Livestock)),
  subset(landuse_full, select = c(SiteID, FieldType))
)
explanatory_site <- filter(explanatory_site, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")
# write.csv(explanatory_site, file = "data/NBR_SiteRepartition.csv")

### Remove sites
plant <- filter(plant_full, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")
meso <- filter(meso_full, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")

### Average species abundance per site
plant <- plant |> 
  group_by(SiteID, Species) |> 
  summarise(Abundance = mean(Abundance)) |> 
  filter(Abundance > 0)

### Functional group attribution
plant <- plant |>
  # assign functional group to species
  mutate(FunctionalGroup = case_when(
    grepl("Achillea|Alchemilla|Anagalis|Anemone|Angelica|Anthriscus|Armeria|Bartsia|Campanula|Cardamine|Cerastium|Cirsium|Conopodium|Dactylorhiza|Digitalis|Epilobium|Euphrasia|Fraxinus|Galeopsis|Galium|Geranium|Gnaphalium|Hieracium|Hypericum|Hypochaeris|Lathyrus|Leontodon|Lotus|Melampyrum|Moneses|Myosotis|Narthecium|Oxalis|Pedicularis|Pinguicula|Plantago|Potentilla|Prunella|Ranunculus|Rumex|Sagina|Sedum|Senecio|Silene|Solidago|Stellaria|Succisa|Taraxacum|Trientalis|Trifolium|Urtica|Valeriana|Veronica|Vicia|Viola", Species) ~ "DIV_forbs",
    grepl("Agrostis|Aira|Alopecurus|Anthoxanthum|Bromopsis|Calamagrostis|Carex|Dactylis|Danthonia|Deschampsia|Eriophorum|Festuca|Holcus|Juncus|Lolium|Luzula|Molinia|Nardus|Phleum|Poa|Trichophorum", Species) ~ "DIV_monocotyledons",
    grepl("Andromeda|Arctostaphylos|Betula|Calluna|Chamaepericlymenum|Empetrum|Erica|Juniperus|Loiseleuria|Picea|Polygala|Polygonum|Populus|Prunus|Rubus|Salix|Sorbus|Ulmus|Vaccinium", Species) ~ "DIV_woody",
    grepl("Athyrium|Blechnum|Dryopteris|Gymnocarpium|Phegopteris|Polypodium|Pteridium", Species) ~ "DIV_ferns",
    .default = "DIV_cryptogams"
  ))

## Variables x longitude

### Average species cover per functional group
plant_enviR1 <- plant |> 
  group_by(SiteID, FunctionalGroup) |>
  summarise(Abundance = sum(Abundance)) |> 
  pivot_wider(names_from = FunctionalGroup, values_from = Abundance) |> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

### Average meso abundance per site
meso_enviR1 <- meso |> 
  group_by(SiteID) |> 
  summarise(Acari.m2 = mean(Acari.m2), Collembola.m2 = mean(Collembola.m2)) |> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

### Base dataset
plant_enviR1 <- left_join(plant_enviR1, explanatory_site)
plant_enviR1 <- filter(plant_enviR1, Livestock == "sheep")
meso_enviR1 <- left_join(meso_enviR1, explanatory_site)
meso_enviR1 <- filter(meso_enviR1, Livestock == "sheep")

### Shrub cover lollipop
shrubgradient <- plant_enviR1 |> 
  ggplot(
    aes(x = as.factor(EPSG.25832_X), y = DIV_woody)
  ) +
  geom_point(aes(colour = FieldType)) +
  geom_segment(aes(x = as.factor(EPSG.25832_X), xend = as.factor(EPSG.25832_X), y = 0, yend = DIV_woody, colour = FieldType)) +
  xlab("Longitude") +
  ylab("Shrub percent cover (%)") +
  scale_colour_manual(values = c("outfield" = "#AA4499", "infield" = "#CC6677")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.text.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
shrubgradient

### Forb cover lollipop
forbgradient <- plant_enviR1 |> 
  ggplot(
    aes(x = as.factor(EPSG.25832_X), y = DIV_forbs)
  ) +
  geom_point(aes(colour = FieldType)) +
  geom_segment(aes(x = as.factor(EPSG.25832_X), xend = as.factor(EPSG.25832_X), y = 0, yend = DIV_forbs, colour = FieldType)) +
  xlab("Longitude") +
  ylab("Forb percent cover (%)") +
  scale_colour_manual(values = c("outfield" = "#AA4499", "infield" = "#CC6677")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
forbgradient

### Collembola density lollipop
collgradient <- meso_enviR1 |> 
  ggplot(
    aes(x = as.factor(EPSG.25832_X), y = Collembola.m2)
  ) +
  geom_point(aes(colour = FieldType)) +
  geom_segment(aes(x = as.factor(EPSG.25832_X), xend = as.factor(EPSG.25832_X), y = 0, yend = Collembola.m2, colour = FieldType)) +
  xlab("Longitude") +
  ylab("Collembola density (m.2)") +
  scale_colour_manual(values = c("outfield" = "#AA4499", "infield" = "#CC6677")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
collgradient

### Acari density lollipop
acarigradient <- meso_enviR1 |> 
  ggplot(
    aes(x = as.factor(EPSG.25832_X), y = Acari.m2)
  ) +
  geom_point(aes(colour = FieldType)) +
  geom_segment(aes(x = as.factor(EPSG.25832_X), xend = as.factor(EPSG.25832_X), y = 0, yend = Acari.m2, colour = FieldType)) +
  xlab("Longitude") +
  ylab("Acari density (m.2)") +
  scale_colour_manual(values = c("outfield" = "#AA4499", "infield" = "#CC6677")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
acarigradient

fjordR1 <- ggarrange(shrubgradient, forbgradient, collgradient, acarigradient, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"), font.label = list(size = 12))
ggsave(filename = "illustrations/NBR_ResultFjordR1.png", plot = fjordR1, width = 12, height = 12, units = "cm")

# R2 - Infield range

## Package loading
library(tidyverse)
library(purrr)
library(ggplot2)
library(ggpubr) # Function ggarrange for several plots on same file
library(GGally) # Extension ggplot

## Data loading
siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
landuse_full <- read.csv("data/cleandata/NBR_FullLandUse.csv", sep=",")
locenvi_full <- read.csv("data/cleandata/NBR_FullArea20x20.csv")
plant_full <- read.csv("data/cleandata/NBR_FullPlantComm.csv")
beetle_full <- read.csv("data/cleandata/NBR_FullBeetleComm.csv")

## Preparation

### Site ID
explanatory_site <- purrr::reduce(list(
  subset(siteinfo_full, select = c(SiteID, EcoZone, EPSG.25832_X, EPSG.25832_Y, Livestock)),
  subset(landuse_full, select = c(SiteID, FieldType)),
  subset(locenvi_full, select = c(SiteID, Slope_degree))
), dplyr::left_join)
explanatory_site <- filter(explanatory_site, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")
# write.csv(explanatory_site, file = "data/NBR_SiteRepartition.csv")

### Remove sites
plant <- filter(plant_full, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")
beetle <- filter(beetle_full, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")

### Average species abundance per site
plant <- plant |> 
  group_by(SiteID, Species) |> 
  summarise(Abundance = mean(Abundance)) |> 
  filter(Abundance > 0)
beetle <- beetle |> 
  group_by(SiteID, BeetleFamilies) |> 
  summarise(Abundance = mean(BeetleFam_abundance)) |> 
  filter(Abundance > 0) 

### Functional group attribution
plant <- plant |>
  # assign functional group to species
  mutate(FunctionalGroup = case_when(
    grepl("Achillea|Alchemilla|Anagalis|Anemone|Angelica|Anthriscus|Armeria|Bartsia|Campanula|Cardamine|Cerastium|Cirsium|Conopodium|Dactylorhiza|Digitalis|Epilobium|Euphrasia|Fraxinus|Galeopsis|Galium|Geranium|Gnaphalium|Hieracium|Hypericum|Hypochaeris|Lathyrus|Leontodon|Lotus|Melampyrum|Moneses|Myosotis|Narthecium|Oxalis|Pedicularis|Pinguicula|Plantago|Potentilla|Prunella|Ranunculus|Rumex|Sagina|Sedum|Senecio|Silene|Solidago|Stellaria|Succisa|Taraxacum|Trientalis|Trifolium|Urtica|Valeriana|Veronica|Vicia|Viola", Species) ~ "forbs",
    grepl("Agrostis|Aira|Alopecurus|Anthoxanthum|Bromopsis|Calamagrostis|Carex|Dactylis|Danthonia|Deschampsia|Eriophorum|Festuca|Holcus|Juncus|Lolium|Luzula|Molinia|Nardus|Phleum|Poa|Trichophorum", Species) ~ "monocotyledons",
    grepl("Andromeda|Arctostaphylos|Betula|Calluna|Chamaepericlymenum|Empetrum|Erica|Juniperus|Loiseleuria|Picea|Polygala|Polygonum|Populus|Prunus|Rubus|Salix|Sorbus|Ulmus|Vaccinium", Species) ~ "woody",
    grepl("Athyrium|Blechnum|Dryopteris|Gymnocarpium|Phegopteris|Polypodium|Pteridium", Species) ~ "ferns",
    .default = "cryptogams"
  ))

## Stacked barplots

### Base datasets
plantbar_enviR2 <- plant |> 
  group_by(SiteID, FunctionalGroup) |>
  summarise(Abundance = sum(Abundance)) |> 
  filter(FunctionalGroup != "ferns" & FunctionalGroup != "woody")

beetlebar_enviR2 <- beetle |> 
  filter(BeetleFamilies == "Carabidae" | BeetleFamilies == "Staphylinidae" | BeetleFamilies == "Hydrophilidae" | BeetleFamilies == "Ptiliidae" | BeetleFamilies == "Scarabaeidae")

### Filter data
plantbar_enviR2 <- left_join(plantbar_enviR2, explanatory_site)
plantbar_enviR2 <- filter(plantbar_enviR2, FieldType == "infield")
beetlebar_enviR2 <- left_join(beetlebar_enviR2, explanatory_site)
beetlebar_enviR2 <- filter(beetlebar_enviR2, FieldType == "infield")

### Plant functional group cover
plantcov_envibar <- plantbar_enviR2 |> 
  ggplot(
    aes(x = as.factor(EcoZone), y = Abundance, fill = FunctionalGroup)
  ) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("") +
  ylab("") +
  scale_fill_viridis_d() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.7, "line"),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "bottom",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
plantcov_envibar

### Beetle family abundance graph
beetle_envibar <- beetlebar_enviR2 |> 
  ggplot(
    aes(x = as.factor(EcoZone), y = Abundance, fill = BeetleFamilies)
  ) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("") +
  ylab("") +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.7, "line"),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "bottom",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
beetle_envibar

## Boxplots

### Base datasets
plantbox_enviR2 <- plant |> 
  group_by(SiteID, FunctionalGroup) |>
  summarise(Abundance = sum(Abundance)) |> 
  filter(FunctionalGroup != "ferns" & FunctionalGroup != "woody") |> 
  pivot_wider(names_from = FunctionalGroup, values_from = Abundance) |> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

beetlebox_enviR2 <- beetle |> 
  filter(BeetleFamilies == "Carabidae" | BeetleFamilies == "Staphylinidae" | BeetleFamilies == "Hydrophilidae" | BeetleFamilies == "Ptiliidae" | BeetleFamilies == "Scarabaeidae") |> 
  pivot_wider(names_from = BeetleFamilies, values_from = Abundance)|> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

### Filter data
plantbox_enviR2 <- left_join(plantbox_enviR2, explanatory_site)
plantbox_enviR2 <- filter(plantbox_enviR2, FieldType == "infield")
beetlebox_enviR2 <- left_join(beetlebox_enviR2, explanatory_site)
beetlebox_enviR2 <- filter(beetlebox_enviR2, FieldType == "infield")

### Grass cover
grasscov_envibox <- plantbox_enviR2 |> 
  ggplot(
    aes(x = as.factor(EcoZone), y = monocotyledons, fill = EcoZone)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Monocotyledon cover (%)") +
  scale_fill_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
grasscov_envibox

### Forb cover
forbcov_envibox <- plantbox_enviR2 |> 
  ggplot(
    aes(x = as.factor(EcoZone), y = forbs, fill = EcoZone)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Forb cover (%)") +
  scale_fill_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
forbcov_envibox

### Staphylinidae abundance
staph_envibox <- beetlebox_enviR2 |> 
  ggplot(
    aes(x = as.factor(EcoZone), y = Staphylinidae, fill = EcoZone)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Staphylinidae abundance") +
  scale_fill_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
staph_envibox

### Hydrophilidae abundance
hydro_envibox <- beetlebox_enviR2 |> 
  ggplot(
    aes(x = as.factor(EcoZone), y = Hydrophilidae, fill = EcoZone)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Hydrophilidae abundance") +
  scale_fill_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
hydro_envibox

### Ptiliidae abundance
ptili_envibox <- beetlebox_enviR2 |> 
  ggplot(
    aes(x = as.factor(EcoZone), y = Ptiliidae, fill = EcoZone)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Ptiliidae abundance") +
  scale_fill_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
ptili_envibox

### Carabidae abundance
carab_envibox <- beetlebox_enviR2 |> 
  ggplot(
    aes(x = as.factor(EcoZone), y = Carabidae, fill = EcoZone)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Carabidae abundance") +
  scale_fill_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
carab_envibox

### Scarabaeidae abundance
scara_envibox <- beetlebox_enviR2 |> 
  ggplot(
    aes(x = as.factor(EcoZone), y = Scarabaeidae, fill = EcoZone)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Scarabaeidae abundance") +
  scale_fill_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
scara_envibox

## Plot arrange
fjordR2 <- ggarrange(
  plantcov_envibar, grasscov_envibox, beetle_envibar, staph_envibox, 
  ncol = 2, 
  nrow = 2, 
  labels = c("A", "B", "C", "D"),
  font.label = list(size = 12)
  )
ggsave(filename = "illustrations/NBR_ResultFjordR2.png", plot = fjordR2, width = 12, height = 12, units = "cm")

#### MAIN RESULTS GRAZING R1 - Diversity small-scale grazing systems ####

## Package loading
library(tidyverse)
library(purrr)
library(ggplot2)
library(ggpubr) # Function ggarrange for several plots on same file
library(readxl)

## Data loading
siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
plant_full <- read.csv("data/cleandata/NBR_FullPlantComm.csv")
value_full <- read_excel("data/NBR_ForageNectarValues.xlsx")
landuse_full <- read.csv("data/cleandata/NBR_FullLandUse.csv", sep=",")
soilchem_full <- read.csv("data/cleandata/NBR_FullSoilChem.csv", sep=",")

## Datasets

### Site ID
explanatory_site <- purrr::reduce(list(
  subset(siteinfo_full, select = c(SiteID, EcoZone, EPSG.25832_X, EPSG.25832_Y, Livestock)),
  subset(landuse_full, select = c(SiteID, FieldType))
), dplyr::left_join)
explanatory_site <- filter(explanatory_site, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")
# write.csv(explanatory_site, file = "data/NBR_SiteRepartition.csv")

### Remove sites
plant <- filter(plant_full, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")

### Site-level data
plant <- plant |> 
  group_by(SiteID, Species) |> 
  summarise(Abundance = mean(Abundance)) |> 
  filter(Abundance > 0)

### Functional group attribution
plant <- plant |>
  # assign functional group to species
  mutate(FunctionalGroup = case_when(
    grepl("Achillea|Alchemilla|Anagalis|Anemone|Angelica|Anthriscus|Armeria|Bartsia|Campanula|Cardamine|Cerastium|Cirsium|Conopodium|Dactylorhiza|Digitalis|Epilobium|Euphrasia|Fraxinus|Galeopsis|Galium|Geranium|Gnaphalium|Hieracium|Hypericum|Hypochaeris|Lathyrus|Leontodon|Lotus|Melampyrum|Moneses|Myosotis|Narthecium|Oxalis|Pedicularis|Pinguicula|Plantago|Potentilla|Prunella|Ranunculus|Rumex|Sagina|Sedum|Senecio|Silene|Solidago|Stellaria|Succisa|Taraxacum|Trientalis|Trifolium|Urtica|Valeriana|Veronica|Vicia|Viola", Species) ~ "DIV_forbs",
    grepl("Agrostis|Aira|Alopecurus|Anthoxanthum|Bromopsis|Calamagrostis|Carex|Dactylis|Danthonia|Deschampsia|Eriophorum|Festuca|Holcus|Juncus|Lolium|Luzula|Molinia|Nardus|Phleum|Poa|Trichophorum", Species) ~ "DIV_monocotyledons",
    grepl("Andromeda|Arctostaphylos|Betula|Calluna|Chamaepericlymenum|Empetrum|Erica|Juniperus|Loiseleuria|Picea|Polygala|Polygonum|Populus|Prunus|Rubus|Salix|Sorbus|Ulmus|Vaccinium", Species) ~ "DIV_woody",
    grepl("Athyrium|Blechnum|Dryopteris|Gymnocarpium|Phegopteris|Polypodium|Pteridium", Species) ~ "DIV_ferns",
    .default = "DIV_cryptogams"
  ))

## Average species richness histogram

### Average species richness per site
plantdiv_grazingR1 <- plant |> 
  group_by(SiteID) |> 
  summarise(SRsite = n())
summary(plantdiv_grazingR1$SRsite)

### Average per site type
plantdiv_grazingR1 <- left_join(plantdiv_grazingR1, explanatory_site)

### Graphical representation
SR_histo <- ggplot(
  plantdiv_grazingR1, aes(x = SRsite)
) +
  geom_histogram(binwidth = 5, fill="#7b7b7b", color="#e9ecef") +
  xlab("Plant species richness") +
  ylab("Number grazing fields") +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
SR_histo

## Infield ES synergy

### Site scaling
value <- value_full |> 
  group_by(SiteID) |> 
  summarise(forage = mean(Forage_value), nectar = mean(Nectar_value_final), SR = mean(Species_Richness)) |> 
  mutate(across(where(is.numeric), scale))

### synergy nectar x forage
synergynecfor <- value |> 
  ggplot(
    aes(x = forage, y = nectar)
) +
  geom_point() +
  geom_smooth(method = "lm", color="#7b7b7b", fill = "#e9ecef", se = TRUE) +
  xlab("Forage resource value") +
  ylab("Nectar resource value") +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
synergynecfor

### synergy nectar x plant species richness
synergynecrich <- value |> 
  ggplot(
    aes(x = SR, y = nectar)
  ) +
  geom_point() +
  geom_smooth(method = "lm", color="navy", fill = "navy", se = TRUE) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
synergynecrich

### synergy forage x plant species richness
synergyforrich <- value |> 
  ggplot(
    aes(x = SR, y = forage)
  ) +
  geom_point() +
  geom_smooth(method = "lm", color="navy", fill = "navy", se = TRUE) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
synergyforrich

## Save file

grazingR1 <- ggarrange(SR_histo, synergynecfor, labels = c("A", "B"), font.label = list(size = 12))
ggsave(filename = "illustrations/NBR_ResultGrazingR1.png", plot = grazingR1, width = 12, height = 6, units = "cm")


## Outfield ES synergy

### Base dataset 
plantdivfunc_grazingR1 <- plant |> 
  group_by(SiteID, FunctionalGroup) |>
  summarise(SRfun = n()) |> 
  pivot_wider(names_from = FunctionalGroup, values_from = SRfun) |> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |> 
  mutate(SRtot = rowSums(across(where(is.numeric)), na.rm=TRUE))
soilchem <- soilchem_full |> 
  group_by(SiteID) |> 
  summarise(LOI = mean(LOI))

plantdivfunc_grazingR1 <- left_join(plantdivfunc_grazingR1, soilchem)
# plantdivfunc_grazingR1 <- plantdivfunc_grazingR1 |> 
#   mutate(across(where(is.numeric), scale))

### synergy Stot x crypto
synergytotcry <- plantdivfunc_grazingR1 |> 
  ggplot(aes(x = SRtot, y = DIV_cryptogams)
      ) +
  geom_point() +
  geom_smooth(method = "lm", color="navy", fill = "navy", se = TRUE) +
  xlab("Overall plant species richness") +
  ylab("Cryptogam species richness") +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
synergytotcry

### synergy Stot x LOI
synergytotloi <- plantdivfunc_grazingR1 |> 
  ggplot(aes(x = SRtot, y = LOI)
  ) +
  geom_point() +
  geom_smooth(method = "lm", color="navy", fill = "navy", se = TRUE) +
  xlab("Overall plant species richness") +
  ylab("Loss of ignition") +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
synergytotloi


#### MAIN RESULTS GRAZING R2 - Postburn diversity ####

## Package loading
library(tidyverse)
library(purrr)
library(ggplot2)
library(ggpubr) # Function ggarrange for several plots on same file

## Data loading
siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
plant_full <- read.csv("data/cleandata/NBR_FullPlantComm.csv")
landuse_full <- read.csv("data/cleandata/NBR_FullLandUse.csv", sep=",")

## Preparation

### Site ID
explanatory_site <- full_join(
  subset(siteinfo_full, select = c(SiteID, EcoZone, EPSG.25832_X, EPSG.25832_Y, Livestock)),
  subset(landuse_full, select = c(SiteID, FieldType))
)
explanatory_site <- filter(explanatory_site, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")
write.csv(explanatory_site, file = "data/NBR_SiteRepartition.csv")

### Remove sites
plant <- filter(plant_full, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")
beetle <- filter(beetle_full, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")

### Average species abundance per site
plant <- plant |> 
  group_by(SiteID, Species) |> 
  summarise(Abundance = mean(Abundance)) |> 
  filter(Abundance > 0)
beetle <- beetle |> 
  group_by(SiteID, BeetleFamilies) |> 
  summarise(Abundance = mean(BeetleFam_abundance)) |> 
  filter(Abundance > 0) 

### Functional group attribution
plant <- plant |>
  # assign functional group to species
  mutate(FunctionalGroup = case_when(
    grepl("Achillea|Alchemilla|Anagalis|Anemone|Angelica|Anthriscus|Armeria|Bartsia|Campanula|Cardamine|Cerastium|Cirsium|Conopodium|Dactylorhiza|Digitalis|Epilobium|Euphrasia|Fraxinus|Galeopsis|Galium|Geranium|Gnaphalium|Hieracium|Hypericum|Hypochaeris|Lathyrus|Leontodon|Lotus|Melampyrum|Moneses|Myosotis|Narthecium|Oxalis|Pedicularis|Pinguicula|Plantago|Potentilla|Prunella|Ranunculus|Rumex|Sagina|Sedum|Senecio|Silene|Solidago|Stellaria|Succisa|Taraxacum|Trientalis|Trifolium|Urtica|Valeriana|Veronica|Vicia|Viola", Species) ~ "DIV_forbs",
    grepl("Agrostis|Aira|Alopecurus|Anthoxanthum|Bromopsis|Calamagrostis|Carex|Dactylis|Danthonia|Deschampsia|Eriophorum|Festuca|Holcus|Juncus|Lolium|Luzula|Molinia|Nardus|Phleum|Poa|Trichophorum", Species) ~ "DIV_monocotyledons",
    grepl("Andromeda|Arctostaphylos|Betula|Calluna|Chamaepericlymenum|Empetrum|Erica|Juniperus|Loiseleuria|Picea|Polygala|Polygonum|Populus|Prunus|Rubus|Salix|Sorbus|Ulmus|Vaccinium", Species) ~ "DIV_woody",
    grepl("Athyrium|Blechnum|Dryopteris|Gymnocarpium|Phegopteris|Polypodium|Pteridium", Species) ~ "DIV_ferns",
    .default = "DIV_cryptogams"
  ))

## Dispersion outfields

### Average species richness per functional group
plantdiv_func <- plant |> 
  group_by(SiteID, FunctionalGroup) |>
  summarise(Sfunc = n()) |> 
  pivot_wider(names_from = FunctionalGroup, values_from = Sfunc) |> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

### Base dataset
plantdiv_func <- left_join(plantdiv_func, explanatory_site)
COdiv_func <- filter(plantdiv_func, FieldType == "outfield" & EcoZone == "coastal")

### Average species cover per functional group
plantcov_func <- plant |> 
  group_by(SiteID, FunctionalGroup) |>
  summarise(Abundance = sum(Abundance)) |> 
  pivot_wider(names_from = FunctionalGroup, values_from = Abundance) |> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

### Base dataset
plantcov_func <- left_join(plantcov_func, explanatory_site)
COcov_func <- filter(plantcov_func, FieldType == "outfield" & EcoZone == "coastal")

#### MAIN RESULTS GRAZING R3 - Livestock effect x plant functional group ####

## Package loading
library(tidyverse)
library(purrr)
library(ggplot2)
library(ggpubr) # Function ggarrange for several plots on same file

## Data loading
siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
plant_full <- read.csv("data/cleandata/NBR_FullPlantComm.csv")
landuse_full <- read.csv("data/cleandata/NBR_FullLandUse.csv", sep=",")

## Preparation

### Site ID
explanatory_site <- full_join(
  subset(siteinfo_full, select = c(SiteID, EcoZone, EPSG.25832_X, EPSG.25832_Y, Livestock)),
  subset(landuse_full, select = c(SiteID, FieldType))
)
explanatory_site <- filter(explanatory_site, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")
write.csv(explanatory_site, file = "data/NBR_SiteRepartition.csv")

### Remove sites
plant <- filter(plant_full, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")

### Average species abundance per site
plant <- plant |> 
  group_by(SiteID, Species) |> 
  summarise(Abundance = mean(Abundance)) |> 
  filter(Abundance > 0)

### Functional group attribution
plant <- plant |>
  # assign functional group to species
  mutate(FunctionalGroup = case_when(
    grepl("Achillea|Alchemilla|Anagalis|Anemone|Angelica|Anthriscus|Armeria|Bartsia|Campanula|Cardamine|Cerastium|Cirsium|Conopodium|Dactylorhiza|Digitalis|Epilobium|Euphrasia|Fraxinus|Galeopsis|Galium|Geranium|Gnaphalium|Hieracium|Hypericum|Hypochaeris|Lathyrus|Leontodon|Lotus|Melampyrum|Moneses|Myosotis|Narthecium|Oxalis|Pedicularis|Pinguicula|Plantago|Potentilla|Prunella|Ranunculus|Rumex|Sagina|Sedum|Senecio|Silene|Solidago|Stellaria|Succisa|Taraxacum|Trientalis|Trifolium|Urtica|Valeriana|Veronica|Vicia|Viola", Species) ~ "forbs",
    grepl("Agrostis|Aira|Alopecurus|Anthoxanthum|Bromopsis|Calamagrostis|Carex|Dactylis|Danthonia|Deschampsia|Eriophorum|Festuca|Holcus|Juncus|Lolium|Luzula|Molinia|Nardus|Phleum|Poa|Trichophorum", Species) ~ "monocotyledons",
    grepl("Andromeda|Arctostaphylos|Betula|Calluna|Chamaepericlymenum|Empetrum|Erica|Juniperus|Loiseleuria|Picea|Polygala|Polygonum|Populus|Prunus|Rubus|Salix|Sorbus|Ulmus|Vaccinium", Species) ~ "woody",
    grepl("Athyrium|Blechnum|Dryopteris|Gymnocarpium|Phegopteris|Polypodium|Pteridium", Species) ~ "ferns",
    .default = "cryptogams"
  ))

## Stacked barplots

### Base datasets
plantbar_grazingR3 <- left_join(plant, explanatory_site)
plantbar_grazingR3 <- filter(plantbar_grazingR3, FieldType == "infield") |> 
  group_by(SiteID, FunctionalGroup, Livestock) |>
  summarise(Abundance = sum(Abundance)) |> 
  filter(FunctionalGroup != "ferns" & FunctionalGroup != "woody")

### Plant functional group cover
plantcov_grazingbar <- plantbar_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = Abundance, fill = FunctionalGroup)
  ) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("") +
  ylab("") +
  scale_fill_viridis_d() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.7, "line"),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "bottom",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
plantcov_grazingbar

## Boxplots

### Base datasets
plantbox_grazingR3 <- plantbar_grazingR3 |> 
  pivot_wider(names_from = FunctionalGroup, values_from = Abundance)|> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

### Cryptogams
cryptocov_grazingbox <- plantbox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = cryptogams, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Cryptogam cover (%)") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
cryptocov_grazingbox

### Grass
grasscov_grazingbox <- plantbox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = monocotyledons, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Monocotyledon cover (%)") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
grasscov_grazingbox

### Forbs
forbcov_grazingbox <- plantbox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = forbs, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Forb cover (%)") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
forbcov_grazingbox

## Plant plot arrange
plantR3 <- ggarrange(plantcov_grazingbar, cryptocov_grazingbox, grasscov_grazingbox, forbcov_grazingbox, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"), font.label = list(size = 12), common.legend = TRUE)
ggsave(filename = "illustrations/NBR_ResultGrazingPlantR3.png", plot = plantR3, width = 12, height = 12, units = "cm")

#### MAIN RESULTS GRAZING R3 - Livestock effect x Plant species ####

library(tidyverse)
library(purrr)
library(ggplot2)
library(ggpubr) # Function ggarrange for several plots on same file

## Data loading
siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
plant_full <- read.csv("data/cleandata/NBR_FullPlantComm.csv")
landuse_full <- read.csv("data/cleandata/NBR_FullLandUse.csv", sep=",")

## Preparation

### Site ID
explanatory_site <- full_join(
  subset(siteinfo_full, select = c(SiteID, EcoZone, EPSG.25832_X, EPSG.25832_Y, Livestock)),
  subset(landuse_full, select = c(SiteID, FieldType))
)
explanatory_site <- filter(explanatory_site, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")
write.csv(explanatory_site, file = "data/NBR_SiteRepartition.csv")

### Remove sites
plant <- filter(plant_full, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")

### Average species abundance per site
plant <- plant |> 
  group_by(SiteID, Species) |> 
  summarise(Abundance = mean(Abundance)) |> 
  filter(Abundance > 0)

### Functional group attribution
plant <- plant |>
  # assign functional group to species
  mutate(FunctionalGroup = case_when(
    grepl("Achillea|Alchemilla|Anagalis|Anemone|Angelica|Anthriscus|Armeria|Bartsia|Campanula|Cardamine|Cerastium|Cirsium|Conopodium|Dactylorhiza|Digitalis|Epilobium|Euphrasia|Fraxinus|Galeopsis|Galium|Geranium|Gnaphalium|Hieracium|Hypericum|Hypochaeris|Lathyrus|Leontodon|Lotus|Melampyrum|Moneses|Myosotis|Narthecium|Oxalis|Pedicularis|Pinguicula|Plantago|Potentilla|Prunella|Ranunculus|Rumex|Sagina|Sedum|Senecio|Silene|Solidago|Stellaria|Succisa|Taraxacum|Trientalis|Trifolium|Urtica|Valeriana|Veronica|Vicia|Viola", Species) ~ "forbs",
    grepl("Agrostis|Aira|Alopecurus|Anthoxanthum|Bromopsis|Calamagrostis|Carex|Dactylis|Danthonia|Deschampsia|Eriophorum|Festuca|Holcus|Juncus|Lolium|Luzula|Molinia|Nardus|Phleum|Poa|Trichophorum", Species) ~ "monocotyledons",
    grepl("Andromeda|Arctostaphylos|Betula|Calluna|Chamaepericlymenum|Empetrum|Erica|Juniperus|Loiseleuria|Picea|Polygala|Polygonum|Populus|Prunus|Rubus|Salix|Sorbus|Ulmus|Vaccinium", Species) ~ "woody",
    grepl("Athyrium|Blechnum|Dryopteris|Gymnocarpium|Phegopteris|Polypodium|Pteridium", Species) ~ "ferns",
    .default = "cryptogams"
  ))

### Base datasets
speciesbar_grazingR3 <- left_join(plant, explanatory_site)
speciesbar_grazingR3 <- filter(speciesbar_grazingR3, FieldType == "infield") |> 
  group_by(Species, FunctionalGroup, Livestock) |>
  summarise(Abundance = mean(Abundance)) |> 
  filter(
    Species == "Agrostis capillaris" |
      Species == "Anthoxantum odoratum" |
      Species == "Deschampsia cespitosa" |
      Species == "Deschampsia flexuosa" |
      Species == "Festuca rubra" |
      Species == "Holcus lanatus" |
      Species == "Poa pratensis" |
      Species == "Poa trivialis" |
      Species == "Achillea millefolium" |
      Species == "Galium saxatile" |
      Species == "Potentilla erecta" |
      Species == "Ranunculus acris" |
      Species == "Ranunculus repens" |
      Species == "Rumex acetosa" |
      Species == "Trifolium repens"
  ) |> 
  mutate(Species = dplyr::recode(Species, "Agrostis capillaris" = "A.capillaris")) |>
  mutate(Species = dplyr::recode(Species, "Anthoxantum odoratum" = "A.odoratum")) |>
  mutate(Species = dplyr::recode(Species, "Deschampsia cespitosa" = "D.cespitosa")) |>
  mutate(Species = dplyr::recode(Species, "Deschampsia flexuosa" = "D.flexuosa")) |>
  mutate(Species = dplyr::recode(Species, "Festuca rubra" = "F.rubra")) |>
  mutate(Species = dplyr::recode(Species, "Holcus lanatus" = "H.lanatus")) |>
  mutate(Species = dplyr::recode(Species, "Poa pratensis" = "P.pratensis")) |>
  mutate(Species = dplyr::recode(Species, "Poa trivialis" = "P.trivialis")) |>
  mutate(Species = dplyr::recode(Species, "Achillea millefolium" = "A.millefolium")) |>
  mutate(Species = dplyr::recode(Species, "Galium saxatile" = "G.saxatile")) |>
  mutate(Species = dplyr::recode(Species, "Potentilla erecta" = "P.erecta")) |>
  mutate(Species = dplyr::recode(Species, "Ranunculus acris" = "R.acris")) |>
  mutate(Species = dplyr::recode(Species, "Ranunculus repens" = "R.repens")) |>
  mutate(Species = dplyr::recode(Species, "Rumex acetosa" = "R.acetosa")) |>
  mutate(Species = dplyr::recode(Species, "Trifolium repens" = "T.repens"))

### Grass species
grass_grazingbar <- filter(speciesbar_grazingR3, FunctionalGroup == "monocotyledons") |> 
  ggplot(
    aes(x = as.factor(Livestock), y = Abundance, fill = Species)
  ) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("") +
  ylab("") +
  scale_fill_viridis_d() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.7, "line"),
        legend.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.position = "bottom",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
grass_grazingbar

### Forb species
forb_grazingbar <- filter(speciesbar_grazingR3, FunctionalGroup == "forbs") |> 
  ggplot(
    aes(x = as.factor(Livestock), y = Abundance, fill = Species)
  ) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("") +
  ylab("") +
  scale_fill_viridis_d() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.7, "line"),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "bottom",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
forb_grazingbar

## Species boxplots

### Base dataset
speciesbox_grazingR3 <- speciesbar_grazingR3 |> 
  pivot_wider(names_from = Species, values_from = Abundance) |> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

### Agrostis capillaris
agcap_grazingbox <- speciesbox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = A.capillaris, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("A. capillaris cover (%)") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
agcap_grazingbox

### Deschampsia cespitosa
desces_grazingbox <- speciesbox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = D.cespitosa, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("D. cespitosa cover (%)") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
desces_grazingbox

### Poa trivialis
poatri_grazingbox <- speciesbox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = P.trivialis, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("P. trivialis cover (%)") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
poatri_grazingbox

### Trifolium repens
trirep_grazingbox <- speciesbox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = T.repens, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("T. repens cover (%)") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
trirep_grazingbox

### Galium saxatile
galsax_grazingbox <- speciesbox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = G.saxatile, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("G. saxatile cover (%)") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
galsax_grazingbox

### Potentilla erecta
potere_grazingbox <- speciesbox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = P.erecta, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("P. erecta cover (%)") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
potere_grazingbox

## Grass plot arrange
grassR3 <- ggarrange(grass_grazingbar, agcap_grazingbox, labels = c("A", "B"), font.label = list(size = 12), common.legend = TRUE)
ggsave(filename = "illustrations/NBR_ResultGrazingGrassR3small.png", plot = grassR3, width = 12, height = 6, units = "cm")
# grassR3 <- ggarrange(grass_grazingbar, agcap_grazingbox, desces_grazingbox, poatri_grazingbox, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"), font.label = list(size = 12), common.legend = TRUE)
# ggsave(filename = "illustrations/NBR_ResultGrazingGrassR3.png", plot = grassR3, width = 12, height = 12, units = "cm")

## Forb plot arrange
forbR3 <- ggarrange(forb_grazingbar, trirep_grazingbox, labels = c("C", "D"), font.label = list(size = 12), common.legend = TRUE)
ggsave(filename = "illustrations/NBR_ResultGrazingForbR3small.png", plot = forbR3, width = 12, height = 6, units = "cm")
# forbR3 <- ggarrange(forb_grazingbar, trirep_grazingbox, galsax_grazingbox, potere_grazingbox, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"), font.label = list(size = 12), common.legend = TRUE)
# ggsave(filename = "illustrations/NBR_ResultGrazingForbR3.png", plot = forbR3, width = 12, height = 12, units = "cm")

#### MAIN RESULTS GRAZING R3 - Livestock effect x beetle families ####

library(tidyverse)
library(purrr)
library(ggplot2)
library(ggpubr) # Function ggarrange for several plots on same file

## Data loading
siteinfo_full <- read.csv("data/cleandata/NBR_FullSiteInfo.csv")
landuse_full <- read.csv("data/cleandata/NBR_FullLandUse.csv", sep=",")
beetle_full <- read.csv("data/cleandata/NBR_FullBeetleComm.csv")

## Preparation

### Site ID
explanatory_site <- full_join(
  subset(siteinfo_full, select = c(SiteID, EcoZone, EPSG.25832_X, EPSG.25832_Y, Livestock)),
  subset(landuse_full, select = c(SiteID, FieldType))
)
explanatory_site <- filter(explanatory_site, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")

### Remove sites
beetle <- filter(beetle_full, SiteID != "UC1" & SiteID != "UG1" & SiteID != "UG2")

### Average species abundance per site
beetle <- beetle |> 
  group_by(SiteID, BeetleFamilies) |> 
  summarise(Abundance = mean(BeetleFam_abundance)) |> 
  filter(Abundance > 0)

## Stacked barplots

### Base datasets
beetlebar_grazingR3 <- left_join(beetle, explanatory_site)
beetlebar_grazingR3 <- filter(beetlebar_grazingR3, FieldType == "infield")
beetlebar_grazingR3 <- beetlebar_grazingR3 |> 
  filter(BeetleFamilies == "Carabidae" | BeetleFamilies == "Staphylinidae" | BeetleFamilies == "Hydrophilidae" | BeetleFamilies == "Ptiliidae" | BeetleFamilies == "Scarabaeidae")

### Beetle family abundance graph
beetle_grazingbar <- beetlebar_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = Abundance, fill = BeetleFamilies)
  ) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("") +
  ylab("") +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.7, "line"),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "bottom",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
beetle_grazingbar

## Boxplots

### Base datasets
beetlebox_grazingR3 <- beetlebar_grazingR3 |> 
  pivot_wider(names_from = BeetleFamilies, values_from = Abundance)|> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

### Staphylinidae abundance
staph_grazingbox <- beetlebox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = Staphylinidae, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Staphylinidae abundance") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
staph_grazingbox

### Ptiliidae abundance
ptili_grazingbox <- beetlebox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = Ptiliidae, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Ptiliidae abundance") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
ptili_grazingbox

### Hydrophilidae abundance
hydro_grazingbox <- beetlebox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = Hydrophilidae, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Hydrophilidae abundance") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
hydro_grazingbox

### Carabidae abundance
carab_grazingbox <- beetlebox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = Carabidae, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Carabidae abundance") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
carab_grazingbox

### Scarabaeidae abundance
scara_grazingbox <- beetlebox_grazingR3 |> 
  ggplot(
    aes(x = as.factor(Livestock), y = Scarabaeidae, fill = Livestock)
  ) +
  geom_boxplot() +
  xlab("") +
  ylab("Scarabaeidae abundance") +
  scale_fill_manual(values = c("cattle" = "black", "goat" = "white", "sheep" = "grey")) +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
scara_grazingbox

## Plot arrange
beetleR3 <- ggarrange(beetle_grazingbar, scara_grazingbox, labels = c("E", "F"), font.label = list(size = 12), common.legend = TRUE)
ggsave(filename = "illustrations/NBR_ResultGrazingBeetleR3small.png", plot = beetleR3, width = 12, height = 6, units = "cm")
# beetleR3 <- ggarrange(beetle_grazingbar, staph_grazingbox, ptili_grazingbox, scara_grazingbox, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"), font.label = list(size = 12), common.legend = TRUE)
# ggsave(filename = "illustrations/NBR_ResultGrazingBeetleR3.png", plot = beetleR3, width = 12, height = 12, units = "cm")


#### ON HOLD ####


# 
# ## Salt
# 
# # Na x longitude
# saltlong_plot <- ggplot(
#   # filter(barprep, FieldType == "infield"),
#   filter(barprep, EcoZone == "coastal" | EcoZone == "fjord"),
#   aes(x = EPSG.25832_X, y = Na)
# ) +
#   geom_point(aes(x = EPSG.25832_X, y = Na, colour = EcoZone), size = 2) +
#   # geom_smooth(method = "lm") +
#   xlab("Longitude") +
#   ylab("Na.Al concentration (mg/100g)") +
#   scale_colour_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
#   theme(panel.background = element_blank(),
#         legend.background = element_blank(),
#         legend.position = "none",
#         axis.title = element_text(size = 8),
#         panel.grid.major = element_blank(),  #remove major-grid labels
#         panel.grid.minor = element_blank(),  #remove minor-grid labels
#         plot.background = element_blank())
# saltlong_plot
# 
# # salt boxplot
# salt_box <- filter(barprep, EcoZone == "coastal" | EcoZone == "fjord") |> 
#   ggplot(aes(x = as.factor(EcoZone), y = Na, fill = EcoZone)) +
#   geom_boxplot() +
#   xlab("") +
#   ylab("") +
#   scale_fill_manual(values = c("coastal" = "yellow3", "fjord" = "green4")) +
#   theme(panel.background = element_blank(),
#         legend.background = element_blank(),
#         axis.title = element_text(size = 10),
#         legend.position = "none",
#         panel.grid.major = element_blank(),  #remove major-grid labels
#         panel.grid.minor = element_blank(),  #remove minor-grid labels
#         plot.background = element_blank())
# salt_box
# wilcox.test(Na ~ EcoZone, data = filter(barprep, EcoZone == "coastal" | EcoZone == "fjord"))
# 
# # Arrange plot
# saltdistri <- ggarrange(saltlong_plot, salt_box, labels = c("A", "B"))
# ggsave(filename = "illustrations/NBR_SaltDistri.png", plot = saltdistri, width = 13, height = 6, units = "cm")
# 
# ## Topography - slope + % exposed rock
# 
# # Slope histogram
# slope_histo <- ggplot(
#   barprep, aes(x = Slope_degree)
# ) +
#   geom_histogram(binwidth = 5, fill="#7b7b7b", color="#e9ecef") +
#   xlab("Slope (°)") +
#   ylab("") +
#   theme(panel.background = element_blank(),
#         legend.background = element_blank(),
#         axis.title = element_text(size = 10),
#         panel.grid.major = element_blank(),  #remove major-grid labels
#         panel.grid.minor = element_blank(),  #remove minor-grid labels
#         plot.background = element_blank())
# slope_histo
# 
# # % rock boxplot
# rock_box <- barprep |> 
#   ggplot(aes(x = as.factor(FieldType), y = PercentRock, fill = FieldType)) +
#   geom_boxplot() +
#   xlab("Exposed bedrock (%)") +
#   ylab("") +
#   scale_fill_manual(values = c("infield" = "white", "outfield" = "grey")) +
#   theme(panel.background = element_blank(),
#         legend.background = element_blank(),
#         axis.title = element_text(size = 10),
#         legend.position = "none",
#         panel.grid.major = element_blank(),  #remove major-grid labels
#         panel.grid.minor = element_blank(),  #remove minor-grid labels
#         plot.background = element_blank())
# rock_box
# 
# # Combining both plots
# topo_plot <- ggarrange(slope_histo, rock_box, labels = c("A", "B"))
# ggsave(filename = "illustrations/NBR_TopoGradient.png", plot = topo_plot, width = 13, height = 6, units = "cm")
# 
# 
# # Nitrogen
# N_plot <- ggplot(
#   barprep 
# ) +
#   geom_point(aes(x = EPSG.25832_X, y = N, colour = FieldType), size = 2) +
#   scale_colour_manual(values = c("infield" = "gray", "outfield" = "black")) +
#   theme(panel.background = element_blank(),
#         legend.background = element_blank(),
#         panel.grid.major = element_blank(),  #remove major-grid labels
#         panel.grid.minor = element_blank(),  #remove minor-grid labels
#         plot.background = element_blank())
# N_plot
# 
# # Phosphorus
# P_plot <- ggplot(
#   barprep 
# ) +
#   geom_point(aes(x = EPSG.25832_X, y = P, colour = FieldType), size = 2) +
#   scale_colour_manual(values = c("infield" = "gray", "outfield" = "black")) +
#   geom_line(aes(x = EPSG.25832_X, y = maxtempJuly)) +
#   theme(panel.background = element_blank(),
#         legend.background = element_blank(),
#         panel.grid.major = element_blank(),  #remove major-grid labels
#         panel.grid.minor = element_blank(),  #remove minor-grid labels
#         plot.background = element_blank())
# P_plot
# 
# # pH
# pH_plot <- ggplot(
#   barprep 
# ) +
#   geom_point(aes(x = EPSG.25832_X, y = pH, colour = FieldType), size = 2) +
#   scale_colour_manual(values = c("infield" = "gray", "outfield" = "black")) +
#   theme(panel.background = element_blank(),
#         legend.background = element_blank(),
#         panel.grid.major = element_blank(),  #remove major-grid labels
#         panel.grid.minor = element_blank(),  #remove minor-grid labels
#         plot.background = element_blank())
# pH_plot
# 
# # LOI
# LOI_plot <- ggplot(
#   barprep 
# ) +
#   geom_point(aes(x = EPSG.25832_X, y = LOI, colour = FieldType), size = 2) +
#   scale_colour_manual(values = c("infield" = "gray", "outfield" = "black")) +
#   theme(panel.background = element_blank(),
#         legend.background = element_blank(),
#         panel.grid.major = element_blank(),  #remove major-grid labels
#         panel.grid.minor = element_blank(),  #remove minor-grid labels
#         plot.background = element_blank())
# LOI_plot
# 
# # % rock sampling area
# rock_plot <- ggplot(
#   barprep 
# ) +
#   geom_point(aes(x = EPSG.25832_X, y = PercentRock, colour = FieldType), size = 2) +
#   scale_colour_manual(values = c("infield" = "gray", "outfield" = "black")) +
#   theme(panel.background = element_blank(),
#         legend.background = element_blank(),
#         panel.grid.major = element_blank(),  #remove major-grid labels
#         panel.grid.minor = element_blank(),  #remove minor-grid labels
#         plot.background = element_blank())
# rock_plot
# 
# # Slope
# slope_plot <- ggplot(
#   barprep 
# ) +
#   geom_point(aes(x = EPSG.25832_X, y = Slope_degree, colour = FieldType), size = 2) +
#   scale_colour_manual(values = c("infield" = "gray", "outfield" = "black")) +
#   theme(panel.background = element_blank(),
#         legend.background = element_blank(),
#         panel.grid.major = element_blank(),  #remove major-grid labels
#         panel.grid.minor = element_blank(),  #remove minor-grid labels
#         plot.background = element_blank())
# slope_plot


