
# 02_Figures.R ####

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally); library(patchwork); library(dplyr); library(beepr); library(sf); library(fs)

library(rgeos); library(rgdal); library(RColorBrewer)

dir_create("Figures")

IMListM <- readRDS("Output/MaleModels.rds")

IMListF <- readRDS("Output/FemaleModels.rds")

# Figure 1: Schematic, not coded here.

# Figure 2: Effect estimates ####

# Figure 3: Maps ####

wyt <- readOGR("woodoutlinefiles","perimeter poly with clearings_region")
poly.sp <- SpatialPolygons(list(wyt@polygons[[1]]))
m.bound <- poly.sp@polygons[[1]]@Polygons[[1]]@coords
boxout <- gEnvelope(wyt)
wytdiff <- gDifference(boxout, wyt)

ggField(IMListF[["April.lay.date"]][["Model1"]][["Spatial"]][["Model"]], 
        IMListF[["April.lay.date"]][["Model1"]][["Spatial"]][["Mesh"]],
        # Boundary = wytdiff,
        Fill = "Continuous") + 
  theme_void() +
  labs(fill="April lay date") +  
  geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
  # scale_fill_discrete_sequential(palette = "Blues", rev=FALSE) + 
  scale_fill_continuous(low = "white", high = 
                          brewer.pal(5, "Set1")[1]) +
  geom_contour(aes(z = Fill), colour = "white", lty = 2, alpha = 0.4) +
  # geom_contour_label(aes(z = Fill), colour = "white", lty = 2, alpha = 0.4) +
  # geom_polygon(data = wytdiff, 
  #              fill = "white", 
  #              aes(x = long, 
  #                  y = lat, 
  #                  group = group))
  NULL +
  
  # ggField(IMListF[["Binary.succ"]][["Model1"]][["Spatial"]][["Model"]], 
  #         IMListF[["Binary.succ"]][["Model1"]][["Spatial"]][["Mesh"]],
  #         Fill = "Continuous") + 
  #   theme_void() +
  #   labs(fill="Binary success") +  
  #   geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
  #   # scale_fill_discrete_sequential(palette = "Blues") + 
  #   scale_fill_continuous(low = "white", high = 
  #                           brewer.pal(5, "Set1")[4]) +
  #   geom_contour(aes(z = Fill), colour = "white", lty = 2, alpha = 0.4) +
#   # geom_polygon(data = wytdiff, fill="white", aes(x=long, y=lat, group group))
NULL +
  
  ggField(IMListF[["Clutch.size"]][["Model1"]][["Spatial"]][["Model"]], 
          IMListF[["Clutch.size"]][["Model1"]][["Spatial"]][["Mesh"]],
          Fill = "Continuous") + 
  theme_void() +
  labs(fill="Clutch size") +  
  geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
  # scale_fill_discrete_sequential(palette = "Blues") + 
  scale_fill_continuous(low = "white", high = 
                          brewer.pal(5, "Set1")[3]) +
  geom_contour(aes(z = Fill), colour = "white", lty = 2, alpha = 0.4) +
  # geom_polygon(data=wytdiff, fill="white", aes(x=long, y=lat, group=group)) +
  NULL +
  
  ggField(IMListF[["Mean.chick.weight"]][["Model1"]][["Spatial"]][["Model"]], 
          IMListF[["Mean.chick.weight"]][["Model1"]][["Spatial"]][["Mesh"]],
          Fill = "Continuous") + 
  theme_void() +
  labs(fill="Mean chick weight") +  
  geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
  scale_fill_discrete_sequential(palette = "Blues") + 
  scale_fill_continuous(low = "white", high = 
                          brewer.pal(5, "Set1")[4]) +
  geom_contour(aes(z = Fill), colour = "white", lty = 2, alpha = 0.4) +
  # geom_polygon(data=wytdiff, fill="white", aes(x=long, y=lat, group=group)) +
  NULL +
  
  ggField(IMListF[["Num.fledglings"]][["Model1"]][["Spatial"]][["Model"]], 
          IMListF[["Num.fledglings"]][["Model1"]][["Spatial"]][["Mesh"]],
          Fill = "Continuous") + 
  theme_void() +
  labs(fill = "Number of fledglings") +  
  geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
  scale_fill_discrete_sequential(palette = "Blues") + 
  scale_fill_continuous(low = "white", high = 
                          brewer.pal(5, "Set1")[5]) +
  geom_contour(aes(z = Fill), colour = "white", lty = 2, alpha = 0.4) +
  # geom_polygon(data = wytdiff, 
  #              fill="white", 
  #              aes(x = long, 
  #                  y = lat, 
  #                  group = group)) +
  NULL +
  plot_layout(nrow = 2) +
  plot_annotation(tag_levels = "A")

ggsave("Figures/MapFigure.jpeg", units = "mm", height = 200, width = 300)
