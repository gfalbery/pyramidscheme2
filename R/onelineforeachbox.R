DF <- readRDS("Data/CleanData.rds")
DF$Age_cat <- as.factor(DF$Age_cat)

##first making a df where the columns are the same for both 
as.data.frame(colnames(DF))
df_base <- DF[,c(1,3,4,5,6,7,8,10,12,19,22,23,24,25,27,35, 95)]
df_base <- unique(df_base)

#then one with individual characteristics we need 
df_ind <- DF[,c(4, 2,28:33,89,92, 93, 94, 97,98 )]

df_female <- df_ind[which(df_ind$Focal.sex=="F"),]
df_female$Focal.sex <- NULL
colnames(df_female) <- paste("Female", colnames(df_female), sep = "_")
names(df_female)[1] <- "Box.yr"

df_male <- df_ind[which(df_ind$Focal.sex=="M"),]
df_male$Focal.sex <- NULL
colnames(df_male) <- paste("Male", colnames(df_male), sep = "_")
names(df_male)[1] <- "Box.yr"

#put it together 
df_onerow <- merge(df_female, df_male)

#add with shared information 
DF_onerow <- merge(df_base, df_onerow, by="Box.yr")

## add an ID for each pair ###
DF_onerow$Pair <- paste(DF_onerow$Mother, DF_onerow$Father)


###now run the model like this 
WoodOutline <- st_read("woodoutlinefiles")

WoodOutline %<>% slice(1)


Resps <- c("April.lay.date",
           "Binary.succ",
           "Clutch.size",
           "Mean.chick.weight",
           "Num.fledglings")

SocialCovar <- c("Female_Strength_mean", "Male_Strength_mean", "Female_Degree", "Male_Degree", #social 
                 "Bondstrength", #pair bond
                 "Female_N.avg.bs", "Female_N.avg.female.bs", "Female_N.avg.male.bs", #neighbors female
                 "Male_N.avg.bs", "Male_N.avg.female.bs", "Male_N.avg.male.bs", #neighbors male
                 "Male_Spatial.assoc", "Female_Spatial.assoc") #spatial 

DensityCovar <- c("Male_LifetimeDensity", "Male_AnnualDensity", "Female_LifetimeDensity", "Female_AnnualDensity")


ClashList <- list(
  c("Female_Strength_mean", "Female_Degree"),
  c("Male_Strength_mean", "Male_Degree"),
  c("Female_AnnualDensity", "Female_Spatial.assoc", "Female_LifetimeDensity"),
  c("Male_AnnualDensity", "Male_Spatial.assoc", "Male_LifetimeDensity"),
  c("Female_N.avg.bs", "Female_N.avg.female.bs", "Female_N.avg.male.bs"),
  c("Male_N.avg.bs", "Male_N.avg.female.bs", "Male_N.avg.male.bs")
)

Covar <- c("Female_Age_num", "Female_Age_cat", "Male_Age_num", "Male_Age_cat","Year.w", "Largeoaks")

r <- 1

IMList <- list()

Resps %<>% sort

####one row####

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  if(0){
    
    TestDF <- DF_onerow %>% 
      dplyr::select(all_of(Covar), Resps[r], X, Y) %>% na.omit
    
    if(Resps[r] == "April.lay.date"){
      
      TestDF %<>% 
        filter(April.lay.date < 55)
      
    }
    
    IM1 <- INLAModelAdd(Data = TestDF,
                        Response = Resps[r], 
                        Explanatory = Covar, 
                        Add = "f(Pair, model = 'iid')",
                        # Random = "Pair", RandomModel = "iid", 
                        AddSpatial = T)
    
    IMList[[Resps[r]]]$Base <- IM1
    
    IM1$FinalModel %>% list(IM1$Spatial$Model) %>% INLADICFig()
    
    IM1$Spatial$Model %>% ggField(
      Mesh = IM1$Spatial$Mesh
      
    ) + scale_fill_discrete_sequential(palette = "Mint")
    
  }
  
  TestDF <- DF_onerow %>% 
    dplyr::select(all_of(Covar), all_of(SocialCovar), all_of(DensityCovar), 
                  Pair, Resps[r], X, Y) %>% 
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
                      Random = "Pair", RandomModel = "iid",
                      AddSpatial = T)
  
  # IM2$FinalModel %>% Efxplot()
  
  IMList[[Resps[r]]] <- list()
  
  IMList[[Resps[r]]]$Model1 <- IM2
  
  SocialKept <- SocialCovar %>% c(DensityCovar) %>% intersect(IM2$Kept)
  
  if(length(SocialKept) > 0){
    
    NewDF <- expand.grid(Var1 = SocialKept,
                         Var2 = Covar) %>% 
      mutate(Var = paste0(Var1, ":", Var2))
    
    IM3 <- INLAModelAdd(Data = TestDF, 
                        Response = Resps[r], 
                        Explanatory = IM2$Kept, 
                        Add = NewDF$Var,
                        AllModels = T,
                        Base = T,
                        # Rounds = 1,
                        Clashes = ClashList,
                        Random = "Pair", RandomModel = "iid",
                        AddSpatial = T)
    
    IMList[[Resps[r]]]$Model2 <- IM3
    
  } 
}

IMListONEROW <- IMList

onerow <- IMListONEROW %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F, Size = 3) +
  scale_colour_brewer(palette = "Set1") +
  guides(color = guide_legend(reverse = T)) +
  IMListONEROW %>% map(c("Model1", "Spatial", "Model")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F, Size = 3) +
  scale_colour_brewer(palette = "Set1") +
  guides(color = guide_legend(reverse = T)) +
  plot_layout(guides = "collect")


IMListONEROW %>% map(~list(.x$FinalModel, .x$Spatial$Model) %>% INLADICFig) %>% ArrangeCowplot()


IMListONEROW %>% names %>% 
  map(~ggField(IMListONEROW[[.x]]$Model1$Spatial$Model, IMListONEROW[[.x]]$Model1$Spatial$Mesh) + 
        labs(fill = .x) +
        geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
        scale_fill_discrete_sequential(palette = "SunsetDark")) %>% 
  ArrangeCowplot()


