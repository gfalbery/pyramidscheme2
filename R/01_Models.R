
# 0_Greg Script ####

rm(list = ls())

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally); library(patchwork); library(dplyr); library(beepr); library(sf); library(fs)

dir_create("Output")

theme_set(theme_cowplot())

DF_all <- readRDS("Data/CleanData.rds")

DF_all$Age_cat <- as.factor(DF_all$Age_cat)

mcw <- readRDS("Data/mcw2012.Rds")

DF_temp <- merge(DF_all, 
                 mcw, 
                 by = c("Nestbox","Year.s"), 
                 all.x = TRUE)

DF_temp %>% 
  mutate(Mean.chick.weight = coalesce(Mean.chick.weight.x, Mean.chick.weight.y)) -> 
  DF_temp

DF_all <- DF_temp

WoodOutline <- st_read("woodoutlinefiles")

WoodOutline %<>% slice(1)

Resps <- c("April.lay.date",
           "Binary.succ",
           "Clutch.size",
           "Mean.chick.weight",
           "Num.fledglings")

SocialCovar <- c("Strength_mean", "Degree", #social 
                 "Bondstrength", #pair bond
                 "N.avg.bs", "N.avg.female.bs", "N.avg.male.bs", #neighbors
                 "Spatial.assoc") #spatial 

DensityCovar <- c("LifetimeDensity", "AnnualDensity")

# DF %>% mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>% 
# dplyr::select(all_of(SocialCovar)) %>% ggpairs()

# DF %>% mutate_at(Resps, ~as.numeric(as.character(.x))) %>%
#   dplyr::select(all_of(Resps)) %>%
#   ggpairs()

ClashList <- list(
  c("Strength_mean", "Degree"),
  c("AnnualDensity", "Spatial.assoc", "LifetimeDensity"),
  c(SocialCovar[c(4:6)])
)

Covar <- c("Age_cat", "Year.w", "Largeoaks")

r <- 1

IMList <- list()

# FamilyList <- rep("gaussian", 5)
# names(FamilyList) <- Resps
# FamilyList$Binary.succ <- "binomial"

Resps %<>% sort

# FEMALE ####

# DF <- DF_all[which(DF_all$Focal.sex == "F"),]

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  if(0){
    
    TestDF <- DF_all %>% 
      filter(Focal.sex == "F") %>% 
      dplyr::select(all_of(Covar), Focal.ring, Resps[r], X, Y) %>% na.omit
    
    if(Resps[r] == "April.lay.date"){
      
      TestDF %<>% 
        filter(April.lay.date < 55)
      
    }
    
    print("Female!") 
    
    IM1 <- INLAModelAdd(Data = TestDF,
                        Response = Resps[r], 
                        Explanatory = Covar, 
                        Add = "f(Focal.ring, model = 'iid')",
                        # Family = FamilyList[[Resps[r]]],
                        # Random = "Focal.ring", RandomModel = "iid", 
                        AddSpatial = T)
    
    IMList[[Resps[r]]]$Base <- IM1
    
    IM1$FinalModel %>% list(IM1$Spatial$Model) %>% INLADICFig()
    
    IM1$Spatial$Model %>% ggField(
      Mesh = IM1$Spatial$Mesh
      
    ) + scale_fill_discrete_sequential(palette = "Mint")
    
  }
  
  TestDF <- DF_all %>% 
    filter(Focal.sex == "F") %>% 
    dplyr::select(all_of(Covar), all_of(SocialCovar), all_of(DensityCovar), 
                  Focal.ring, Resps[r], X, Y) %>% 
    mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>%
    mutate_at(SocialCovar[3], ~log(.x+1)) %>% mutate_at(SocialCovar[4], ~kader:::cuberoot(.x)) %>% 
    na.omit %>% droplevels %>% 
    mutate_at("Year.w", as.factor)
  
  TestDF %>% nrow %>% print
  
  # TestDF %<>% mutate_at(c(Resps[r], SocialCovar), ~c(scale(.x)))
  
  IM2 <- INLAModelAdd(Data = TestDF, 
                      Response = Resps[r], 
                      Explanatory = Covar, 
                      Add = SocialCovar %>% c(DensityCovar),
                      AllModels = T,
                      Base = T,
                      # Rounds = 1,
                      Clashes = ClashList,
                      Random = "Focal.ring", RandomModel = "iid",
                      AddSpatial = T)
  
  # IM2$FinalModel %>% Efxplot()
  
  IMList[[Resps[r]]] <- list()
  
  IMList[[Resps[r]]]$Model1 <- IM2
  
}

IMListF <- IMList

IMListF %>% saveRDS("Output/FemaleModels.rds")

# MALE ####

# DF <- DF_all[which(DF_all$Focal.sex == "M"),]

IMList <-

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  TestDF <- DF_all %>% 
    filter(Focal.sex == "M") %>% 
    dplyr::select(all_of(Covar), all_of(SocialCovar), all_of(DensityCovar), 
                  Focal.ring, Resps[r], X, Y) %>%
    mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>%
    mutate_at(SocialCovar[3], ~log(.x+1)) %>% mutate_at(SocialCovar[4], ~kader:::cuberoot(.x)) %>% 
    na.omit %>% droplevels %>% 
    mutate_at("Year.w", as.factor)
  
  TestDF %<>% mutate_at(c(Resps[r], SocialCovar), ~c(scale(.x)))
  
  IM2 <- INLAModelAdd(Data = TestDF, 
                      Response = Resps[r], 
                      Explanatory = Covar, 
                      Add = SocialCovar %>% c(DensityCovar),
                      AllModels = T,
                      Base = T,
                      # Rounds = 1,
                      Family = FamilyList[[Resps[r]]],
                      Clashes = ClashList,
                      Random = "Focal.ring", RandomModel = "iid",
                      AddSpatial = T)
  
  IMList[[Resps[r]]] <- list()
  IMList[[Resps[r]]]$Model1 <- IM2
  
}

IMListM <- IMList

IMListM %>% saveRDS("Output/MaleModels.rds")
