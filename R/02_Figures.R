
# 02_Figures.R ####

{
  
  library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
  library(GGally); library(patchwork); library(dplyr); library(beepr); library(sf); library(fs)
  library(rgeos); library(rgdal); library(RColorBrewer)
  
  dir_create("Figures")
  
  IMListM <- readRDS("Output/MaleModels.rds")
  
  IMListF <- readRDS("Output/FemaleModels.rds")
  
  WoodOutline <- st_read("woodoutlinefiles")
  
  WoodOutline %<>% slice(1)
  
}

# Figure 1: Schematic, not coded here.

# Figure 2: Effect estimates ####

IMListF %>% map(c("Model1", "Spatial","Model")) %>% 
  Efxplot(Intercept = F, PointSize = 3, PointOutline = T, 
          ModelNames = Resps %>% rev %>% 
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                  "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[-c(1:6)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[-c(1:5)])
  ) +
  scale_color_brewer(palette = "Spectral", direction = -1) + 
  guides(color = guide_legend(reverse = T)) +
  ylim(c(-.5,.5)) + 
  
  ggtitle("Female") +
  
  IMListM %>% map(c("Model1", "Spatial", "Model")) %>% 
  Efxplot(Intercept = F, PointSize = 3, PointOutline = T, 
          ModelNames = Resps %>% rev %>% 
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                  "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[-c(1:6)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[-c(1:5)])
  ) + theme(axis.text.y = element_blank()) +
  scale_color_brewer(palette = "Spectral", direction = -1) + 
  guides(color = guide_legend(reverse = T)) +
  ylim(c(-.5,.5)) + 
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("Figures/Figure2.jpeg", units = "mm", height = 180, width = 220)

## Overall spatial and non-spatial ####

FemaleEffects <- 
  IMListF %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F, PointSize = 3) +
  # scale_colour_brewer(palette = "Spectral") +
  scale_colour_brewer(palette = "Spectral") +
  guides(color = guide_legend(reverse = T)) +
  IMListF %>% map(c("Model1", "Spatial", "Model")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F, PointSize = 3) +
  scale_colour_brewer(palette = "Spectral") +
  guides(color = guide_legend(reverse = T)) +
  plot_layout(guides = "collect")

MaleEffects <- 
  IMListM %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F, PointSize = 3) +
  # scale_colour_brewer(palette = "Spectral") +
  scale_colour_brewer(palette = "Spectral") +
  guides(color = guide_legend(reverse = T)) +
  IMListF %>% map(c("Model1", "Spatial", "Model")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F, PointSize = 3) +
  scale_colour_brewer(palette = "Spectral") +
  guides(color = guide_legend(reverse = T)) +
  plot_layout(guides = "collect")

FemaleEffects + MaleEffects


# Figure 3: Maps ####

wyt <- readOGR("woodoutlinefiles", "perimeter poly with clearings_region")
poly.sp <- SpatialPolygons(list(wyt@polygons[[1]]))
m.bound <- poly.sp@polygons[[1]]@Polygons[[1]]@coords
boxout <- gEnvelope(wyt)
wytdiff <- gDifference(boxout, wyt)

ggField(IMListF[["April.lay.date"]][["Model1"]][["Spatial"]][["Model"]], 
        IMListF[["April.lay.date"]][["Model1"]][["Spatial"]][["Mesh"]],
        FillAlpha = 1,
        # Boundary = wytdiff,
        Fill = "Continuous") + 
  theme_void() +
  labs(fill="April lay date") +  
  geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
  # scale_fill_discrete_sequential(palette = "Blues", rev=FALSE) + 
  scale_fill_continuous(low = "white", high = 
                          brewer.pal(5, "Spectral")[1]) +
  geom_contour(aes(z = Fill), colour = "white", lty = 2, alpha = 0.4) +
  # geom_contour_label(aes(z = Fill), colour = "white", lty = 2, alpha = 0.4) +
  # geom_polygon(data = wytdiff, 
  #              fill = "white", 
  #              aes(x = long, 
  #                  y = lat, 
  #                  group = group))
  labs(fill = "April \n lay date") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
  NULL +
  
  # ggField(IMListF[["Binary.succ"]][["Model1"]][["Spatial"]][["Model"]], 
  #         IMListF[["Binary.succ"]][["Model1"]][["Spatial"]][["Mesh"]],
  #         Fill = "Continuous") + 
  #   theme_void() +
  #   labs(fill="Binary success") +  
  #   geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
  #   # scale_fill_discrete_sequential(palette = "Blues") + 
  #   scale_fill_continuous(low = "white", high = 
  #                           brewer.pal(5, "Spectral")[4]) +
  #   geom_contour(aes(z = Fill), colour = "white", lty = 2, alpha = 0.4) +
#   # geom_polygon(data = wytdiff, fill="white", aes(x=long, y=lat, group group))
NULL +
  
  ggField(IMListF[["Clutch.size"]][["Model1"]][["Spatial"]][["Model"]], 
          IMListF[["Clutch.size"]][["Model1"]][["Spatial"]][["Mesh"]],
          FillAlpha = 1,
          Fill = "Continuous") + 
  theme_void() +
  labs(fill="Clutch size") +  
  geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
  # scale_fill_discrete_sequential(palette = "Blues") + 
  scale_fill_continuous(low = "white", high = 
                          brewer.pal(5, "Spectral")[3]) +
  geom_contour(aes(z = Fill), colour = "dark grey", lty = 2, alpha = 0.8) +
  # geom_polygon(data=wytdiff, fill="white", aes(x=long, y=lat, group=group)) +
  labs(fill = "Clutch\nsize") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
  NULL +
  
  ggField(IMListF[["Mean.chick.weight"]][["Model1"]][["Spatial"]][["Model"]], 
          IMListF[["Mean.chick.weight"]][["Model1"]][["Spatial"]][["Mesh"]],
          FillAlpha = 1,
          Fill = "Continuous") + 
  theme_void() +
  labs(fill="Mean chick weight") +  
  geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
  # scale_fill_discrete_sequential(palette = "Blues") + 
  scale_fill_continuous(low = "white", high = 
                          brewer.pal(5, "Spectral")[4]) +
  geom_contour(aes(z = Fill), colour = "white", lty = 2, alpha = 0.4) +
  # geom_polygon(data=wytdiff, fill="white", aes(x=long, y=lat, group=group)) +
  labs(fill = "Mean\nchick\nweight") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
  NULL +
  
  ggField(IMListF[["Num.fledglings"]][["Model1"]][["Spatial"]][["Model"]], 
          IMListF[["Num.fledglings"]][["Model1"]][["Spatial"]][["Mesh"]],
          FillAlpha = 1,
          Fill = "Continuous") + 
  theme_void() +
  labs(fill = "Number of fledglings") +  
  geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
  # scale_fill_discrete_sequential(palette = "Blues") + 
  scale_fill_continuous(low = "white", high = 
                          brewer.pal(5, "Spectral")[5]) +
  geom_contour(aes(z = Fill), colour = "white", lty = 2, alpha = 0.4) +
  # geom_polygon(data = wytdiff, 
  #              fill="white", 
  #              aes(x = long, 
  #                  y = lat, 
  #                  group = group)) +
  NULL +
  plot_layout(nrow = 2) +
  plot_annotation(tag_levels = "A") +
  labs(fill = "Number of\n fledglings") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
  NULL

ggsave("Figures/MapFigure.jpeg", units = "mm", height = 200, width = 300)

# Figure 4: Other Spatial Effects ####

IMListF %>% map(c("Model1", "Spatial","Model")) %>% 
  Efxplot(Intercept = F, PointSize = 3, PointOutline = T, 
          ModelNames = Resps %>% rev %>% 
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                  "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[c(3:6)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[c(3:6)])
  ) +
  scale_color_brewer(palette = "Spectral", direction = -1) + 
  guides(color = guide_legend(reverse = T)) +
  # ylim(c(-.5,.5)) + 
  
  ggtitle("Female") +
  
  IMListM %>% map(c("Model1", "Spatial", "Model")) %>% 
  Efxplot(Intercept = F, PointSize = 3, PointOutline = T, 
          ModelNames = Resps %>% rev %>% 
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                  "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[c(3:6)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[c(3:6)])
  ) + theme(axis.text.y = element_blank()) +
  scale_color_brewer(palette= "Spectral", direction = -1) + 
  guides(color = guide_legend(reverse = T)) +
  # ylim(c(-.5,.5)) + 
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("Figures/Figure4.jpeg", units = "mm", height = 180, width = 220)

# Supplementary Figure 1? Correlation plot ####

# cor plot social 

social <- DF_all %>% 
  dplyr::select(Strength_mean, Degree, Bondstrength, 
                N.avg.bs, N.avg.male.bs, N.avg.female.bs, 
                Spatial.assoc, LifetimeDensity, AnnualDensity)

socialm <-  cor(social, method="pearson", use="complete.obs")

library(corrplot)

corrplot(socialm, 
         type = "upper", 
         order = "hclust", 
         tl.col = "black", tl.srt = 45)  

ggsave("Figures/SupplementaryFigure1.jpeg", 
       units = "mm", 
       height = 180, width = 180)

# Supplementary Figure 2: Social effects without spatial ####

IMListF %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(Intercept = F, PointSize = 3, PointOutline = T, 
          ModelNames = Resps %>% rev %>% 
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                  "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[-c(1:6)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[-c(1:5)])
  ) +
  scale_color_brewer(palette = "Spectral", direction = -1) + 
  guides(color = guide_legend(reverse = T)) +
  ylim(c(-.5,.5)) + 
  
  ggtitle("Female") +
  
  IMListM %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(Intercept = F, PointSize = 3, PointOutline = T, 
          ModelNames = Resps %>% rev %>% 
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                  "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[-c(1:6)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[-c(1:5)])
  ) + theme(axis.text.y = element_blank()) +
  scale_color_brewer(palette = "Spectral", direction = -1) + 
  guides(color = guide_legend(reverse = T)) +
  ylim(c(-.5,.5)) + 
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("Figures/SuppFigure2.jpeg", units = "mm", height = 180, width = 220)

# Supplementary Figure 3: Non-Social effects without spatial ####

IMListF %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(Intercept = F, PointSize = 3, PointOutline = T, 
          ModelNames = Resps %>% rev %>% 
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                  "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[c(3:6)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[c(3:6)])
  ) +
  scale_color_brewer(palette = "Spectral", direction = -1) + 
  guides(color = guide_legend(reverse = T)) +
  ylim(c(-.5,.5)) + 
  
  ggtitle("Female") +
  
  IMListM %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(Intercept = F, PointSize = 3, PointOutline = T, 
          ModelNames = Resps %>% rev %>% 
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                  "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[c(3:6)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[c(3:6)])
  ) + theme(axis.text.y = element_blank()) +
  scale_color_brewer(palette = "Spectral", direction = -1) + 
  guides(color = guide_legend(reverse = T)) +
  ylim(c(-.5,.5)) + 
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("Figures/SuppFigure3.jpeg", units = "mm", height = 180, width = 220)




