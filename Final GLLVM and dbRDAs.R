############ Final Soil GLLVM and dbRDAs


# Library-----
library(gllvm)
library(dplyr)
library(ggplot2)
library(cowplot)

# Prep----
av.mh.otu
av.mh.meta

br.mh.otu
br.mh.meta

# Note: the actually GLLVM models were run on a cluster, not on this computer. The outputs from that run were uploaded here to continue the analysis

# GLLVM-----
## Soil----
###### read in data as the following object names
av.mh.soil.otu <- read.csv("data/av.mh.soil.otu.csv")
av.mh.soil.meta <- read.csv("data/av.mh.soil.meta.csv")

br.mh.soil.otu <- read.csv("data/br.mh.soil.otu.csv")
br.mh.soil.meta <- read.csv("data/br.mh.soil.meta.csv")

####### The first column should be made into rownames and then removed
rownames(av.mh.soil.otu) <- av.mh.soil.otu$X #if column 1 is not call 'X', change
av.mh.soil.otu <- av.mh.soil.otu[,-1] # remove first column

rownames(av.mh.soil.meta) <- av.mh.soil.meta$X 
av.mh.soil.meta <- av.mh.soil.meta[,-1]

rownames(br.mh.soil.otu) <- br.mh.soil.otu$X 
br.mh.soil.otu <- br.mh.soil.otu[,-1]

rownames(br.mh.soil.meta) <- br.mh.soil.meta$X 
br.mh.soil.meta <- br.mh.soil.meta[,-1]

## Roots----
av.mh.roots.otu <- read.csv("data/av.mh.roots.otu.csv")
av.mh.roots.meta <- read.csv("data/av.mh.roots.meta.csv")

br.mh.roots.otu <- read.csv("data/br.mh.roots.otu.csv")
br.mh.roots.meta <- read.csv("data/br.mh.roots.meta.csv")

rownames(av.mh.roots.otu) <- av.mh.roots.otu$X #if column 1 is not call 'X', change
av.mh.roots.otu <- av.mh.roots.otu[,-1] # remove first column

rownames(av.mh.roots.meta) <- av.mh.roots.meta$X 
av.mh.roots.meta <- av.mh.roots.meta[,-1]

rownames(br.mh.roots.otu) <- br.mh.roots.otu$X 
br.mh.roots.otu <- br.mh.roots.otu[,-1]

rownames(br.mh.roots.meta) <- br.mh.roots.meta$X 
br.mh.roots.meta <- br.mh.roots.meta[,-1]

# Soil Models-----
#### prep the random effect variable
study_design_av_soil <- data.frame(Site = factor(av.mh.soil.meta$Site)) 

study_design_br_soil <- data.frame(Site = factor(br.mh.soil.meta$Site)) 

######## Models
## Avena----
#Species Only
set.seed(2026)
gllvm.av.soil.species <- gllvm(y=av.mh.soil.otu,
                               X=av.mh.soil.meta,
                               formula= ~Species,
                               family="negative.binomial",
                               studyDesign= study_design_av_soil,
                               row.eff= ~(1|Site), 
                               num.lv=2,
                               method="VA")
saveRDS(gllvm.av.soil.species,"gllvm.av.soil.species")

#Precip Only
set.seed(2026)
gllvm.av.soil.precip <- gllvm(y=av.mh.soil.otu,
                              X=av.mh.soil.meta,
                              formula= ~Mean_Annual_Precip,
                              family="negative.binomial",
                              studyDesign= study_design_av_soil,
                              row.eff= ~(1|Site), 
                              num.lv=2,
                              method="VA")
saveRDS(gllvm.av.soil.precip,"gllvm.av.soil.precip")

# Species and precip
set.seed(2026) 
gllvm.av.soil.sp.prec <- gllvm(y=av.mh.soil.otu,
                               X=av.mh.soil.meta,
                               formula= ~Species + Mean_Annual_Precip,
                               family="negative.binomial",
                               studyDesign= study_design_av_soil,
                               row.eff= ~(1|Site), 
                               num.lv=2,
                               method="VA")
saveRDS(gllvm.av.soil.sp.prec,"gllvm.av.soil.sp.prec")

#Interaction Term
set.seed(2026)
gllvm.av.soil.spxprecip <- gllvm(y=av.mh.soil.otu,
                                 X=av.mh.soil.meta,
                                 formula= ~Species*Mean_Annual_Precip,
                                 family="negative.binomial",
                                 studyDesign= study_design_av_soil,
                                 row.eff= ~(1|Site), 
                                 num.lv=2,
                                 method="VA")
saveRDS(gllvm.av.soil.spxprecip,"gllvm.av.soil.spxprecip")

## Bromus----
#Species Only
set.seed(2026)

gllvm.br.soil.species <- gllvm(y=br.mh.soil.otu,
                               X=br.mh.soil.meta,
                               formula= ~Species,
                               family="negative.binomial",
                               studyDesign= study_design_br_soil,
                               row.eff= ~(1|Site), 
                               num.lv=2,
                               method="VA")
saveRDS(gllvm.br.soil.species,"gllvm.br.soil.species")

#Precip Only
set.seed(2026)
gllvm.br.soil.precip <- gllvm(y=br.mh.soil.otu,
                              X=br.mh.soil.meta,
                              formula= ~Mean_Annual_Precip,
                              family="negative.binomial",
                              studyDesign= study_design_br_siol,
                              row.eff= ~(1|Site), 
                              num.lv=2,
                              method="VA")
saveRDS(gllvm.br.soil.precip,"gllvm.br.soil.precip")

# Species and precip
set.seed(2026) 
gllvm.br.soil.sp.prec <- gllvm(y=br.mh.soil.otu,
                               X=br.mh.soil.meta,
                               formula= ~Species + Mean_Annual_Precip,
                               family="negative.binomial",
                               studyDesign= study_design_br_soil,
                               row.eff= ~(1|Site), 
                               num.lv=2,
                               method="VA")
saveRDS(gllvm.br.soil.sp.prec,"gllvm.br.soil.sp.prec")

#Interaction Term
set.seed(2026)
gllvm.br.soil.spxprecip <- gllvm(y=br.mh.soil.otu,
                                 X=br.mh.soil.meta,
                                 formula= ~Species*Mean_Annual_Precip,
                                 family="negative.binomial",
                                 studyDesign= study_design_br_soil,
                                 row.eff= ~(1|Site), 
                                 num.lv=2,
                                 method="VA")
saveRDS(gllvm.br.soil.spxprecip,"gllvm.br.soil.spxprecip") 

# Roots Models-----
study_design_av_roots <- data.frame(Site = factor(av.mh.roots.meta$Site)) 

study_design_br_roots <- data.frame(Site = factor(br.mh.roots.meta$Site)) 

######## Models
## Avena----
#Species Only
set.seed(2026) 
gllvm.av.roots.species <- gllvm(y=av.mh.roots.otu,
                                X=av.mh.roots.meta,
                                formula= ~Species,
                                family="negative.binomial",
                                studyDesign= study_design_av_roots,
                                row.eff= ~(1|Site), 
                                num.lv=2,
                                method="VA")
saveRDS(gllvm.av.roots.species,"gllvm.av.roots.species") 

#Precip Only
set.seed(2026)
gllvm.av.roots.precip <- gllvm(y=av.mh.roots.otu,
                               X=av.mh.roots.meta,
                               formula= ~Mean_Annual_Precip,
                               family="negative.binomial",
                               studyDesign= study_design_av_roots,
                               row.eff= ~(1|Site), 
                               num.lv=2,
                               method="VA")
saveRDS(gllvm.av.roots.precip,"gllvm.av.roots.precip") 

# Species and precip
set.seed(2026)
gllvm.av.roots.sp.prec <- gllvm(y=av.mh.roots.otu,
                                X=av.mh.roots.meta,
                                formula= ~Species + Mean_Annual_Precip,
                                family="negative.binomial",
                                studyDesign= study_design_av_roots,
                                row.eff= ~(1|Site), 
                                num.lv=2,
                                method="VA")
saveRDS(gllvm.av.roots.sp.prec,"gllvm.av.roots.sp.prec") 

#Interaction Term
set.seed(2026)
gllvm.av.roots.spxprecip <- gllvm(y=av.mh.roots.otu,
                                  X=av.mh.roots.meta,
                                  formula= ~Species*Mean_Annual_Precip,
                                  family="negative.binomial",
                                  studyDesign= study_design_av_roots,
                                  row.eff= ~(1|Site), 
                                  num.lv=2,
                                  method="VA")
saveRDS(gllvm.av.roots.spxprecip,"gllvm.av.roots.spxprecip") 

## Bromus----
#Species Only
set.seed(2026) 
gllvm.br.roots.species <- gllvm(y=br.mh.roots.otu,
                                X=br.mh.roots.meta,
                                formula= ~Species,
                                family="negative.binomial",
                                studyDesign= study_design_br_roots,
                                row.eff= ~(1|Site), 
                                num.lv=2,
                                method="VA")
saveRDS(gllvm.br.roots.species,"gllvm.br.roots.species") 


#Precip Only
set.seed(2026)
gllvm.br.roots.precip <- gllvm(y=br.mh.roots.otu,
                               X=br.mh.roots.meta,
                               formula= ~Mean_Annual_Precip,
                               family="negative.binomial",
                               studyDesign= study_design_br_siol,
                               row.eff= ~(1|Site), 
                               num.lv=2,
                               method="VA")
saveRDS(gllvm.br.roots.precip,"gllvm.br.roots.precip") 

# Species and precip
set.seed(2026)
gllvm.br.roots.sp.prec <- gllvm(y=br.mh.roots.otu,
                                X=br.mh.roots.meta,
                                formula= ~Species + Mean_Annual_Precip,
                                family="negative.binomial",
                                studyDesign= study_design_br_roots,
                                row.eff= ~(1|Site), 
                                num.lv=2,
                                method="VA")
saveRDS(gllvm.br.roots.sp.prec,"gllvm.br.roots.sp.prec") 

#Interaction Term
set.seed(2026)
gllvm.br.roots.spxprecip <- gllvm(y=br.mh.roots.otu,
                                  X=br.mh.roots.meta,
                                  formula= ~Species*Mean_Annual_Precip,
                                  family="negative.binomial",
                                  studyDesign= study_design_br_roots,
                                  row.eff= ~(1|Site), 
                                  num.lv=2,
                                  method="VA")
saveRDS(gllvm.br.roots.spxprecip,"gllvm.br.roots.spxprecip") 

# Analysis-------
## Soil AV-MH-----

##### Precip
gllvm.av.soil.precip <- readRDS("data/gllvm.av.soil.precip")
gllvm.av.soil.precip
# AIC: 58009
summary(gllvm.av.soil.precip)
plot(gllvm.av.soil.precip,which=1:2) # qq

##### Species
gllvm.av.soil.species <- readRDS("data/gllvm.av.soil.species")
gllvm.av.soil.species
#AIC: 58605

##### Species + Precip
gllvm.av.soil.sp.precip <- readRDS("data/gllvm.av.soil.sp.prec")
gllvm.av.soil.sp.precip
#AIC: 57848

##### Species x Precip
gllvm.av.soil.spxprecip <- readRDS("data/gllvm.av.soil.spxprecip")
gllvm.av.soil.spxprecip
#AIC: 57837

############################# Compare
#anova(gllvm.av.soil.precip,gllvm.av.soil.species)
# it is not accruate to use this test to compare un-nested models
anova(gllvm.av.soil.precip,gllvm.av.soil.sp.precip)
# large dif in df, but additive is better

anova(gllvm.av.soil.species,gllvm.av.soil.sp.precip)
# large dif in df, but additive is better

anova(gllvm.av.soil.sp.precip,gllvm.av.soil.spxprecip)
# large dif in df, but interaction is better

#### Conclusions:
#  based on AIC and model fit (log likelihood), the interaction term is best


## Soil BR-MH-----
##### Precip
gllvm.br.soil.precip <- readRDS("data/gllvm.br.soil.precip")
gllvm.br.soil.precip
# AIC: 43336

##### Species
gllvm.br.soil.species <- readRDS("data/gllvm.br.soil.species")
gllvm.br.soil.species
#AIC: 43392

##### Species + Precip
gllvm.br.soil.sp.precip <- readRDS("data/gllvm.br.soil.sp.prec")
gllvm.br.soil.sp.precip
#AIC: 43009

##### Species x Precip
gllvm.br.soil.spxprecip <- readRDS("data/gllvm.br.soil.spxprecip")
gllvm.br.soil.spxprecip
#AIC: 43021

#### Compare
anova(gllvm.br.soil.precip,gllvm.br.soil.sp.precip)
# additive is better

anova(gllvm.br.soil.species,gllvm.br.soil.sp.precip)
# additive is better

anova(gllvm.br.soil.sp.precip,gllvm.br.soil.spxprecip)
# interaction is better, but this conflicts with AIC

#### Conclusions:
#clearly, including both terms improves the model compared to just species or precip. The additive model has the best AIC, though the interactive model continues to improve model fit. 

## Roots AV-MH-----

##### Precip
gllvm.av.roots.precip <- readRDS("data/gllvm.av.roots.precip")
gllvm.av.roots.precip
# AIC: 89519

##### Species
gllvm.av.roots.species <- readRDS("data/gllvm.av.roots.species")
gllvm.av.roots.species
#AIC: 88219, better!

##### Species + Precip
gllvm.av.roots.sp.precip <- readRDS("data/gllvm.av.roots.sp.prec")
gllvm.av.roots.sp.precip
#AIC: 89702, worse even than just precip. basically precip alone doesn't matter at all

##### Species x Precip
gllvm.av.roots.spxprecip <- readRDS("data/gllvm.av.roots.spxprecip")
gllvm.av.roots.spxprecip
#AIC: 87925, best!

################## Compare
anova(gllvm.av.roots.precip,gllvm.av.roots.sp.precip)
# additive is better

anova(gllvm.av.roots.species,gllvm.av.roots.sp.precip)
#just species is better, interesting

anova(gllvm.av.roots.species,gllvm.av.roots.spxprecip)
# large dif in df, but interaction is better!

#### Conclusions:
#  


## Roots BR-MH-----
##### Precip
gllvm.br.roots.precip <- readRDS("data/gllvm.br.roots.precip")
gllvm.br.roots.precip
# AIC: 32549

##### Species
gllvm.br.roots.species <- readRDS("data/gllvm.br.roots.species")
gllvm.br.roots.species
#AIC: 31587, worse

##### Species + Precip
gllvm.br.roots.sp.precip <- readRDS("data/gllvm.br.roots.sp.prec")
gllvm.br.roots.sp.precip
#AIC: 32227, better

##### Species x Precip
gllvm.br.roots.spxprecip <- readRDS("data/gllvm.br.roots.spxprecip")
gllvm.br.roots.spxprecip
#AIC: 32149, best

##################### Compare
anova(gllvm.br.roots.precip,gllvm.br.roots.sp.precip)
# additive is better

anova(gllvm.br.roots.species,gllvm.br.roots.sp.precip)
# additive is better

anova(gllvm.br.roots.sp.precip,gllvm.br.roots.spxprecip)
# interaction is better

#### Conclusions:
#clearly, interactive model is best


# Figures-----
## Soil Avena-----
# extract scores to plot from model
av.soil.lv <- getLV(gllvm.av.soil.spxprecip,type = "residual")
av.soil.lv<- as.data.frame(av.soil.lv[, 1:2, drop = FALSE])

av.soil.lv$Sample <- rownames(av.soil.lv)

av.soil.lv.meta <- av.mh.meta %>%
  tibble::rownames_to_column("SampleID")
av.soil.lv.meta$Sample <- as.character((av.soil.lv.meta$Sample))

av.soil.lv.meta <- av.soil.lv %>%
  left_join(av.soil.lv.meta, by = "Sample")

#### plot
av.soil.lv.plot <- ggplot(av.soil.lv.meta,aes(x = LV1,y = LV2,
                                              color = Mean_Annual_Precip,
                                              shape = Species)) +
  geom_point(size = 3) +
  ggtitle("A. barbata-Medusahead Soil Community") +
  scale_color_gradientn(
    colors = c("#83B3D6", "#558FBD", "#3B70A1", "#09335C"),
    name = "Mean annual precipitation"
  ) +
  labs(
    x = "Latent variable 1",
    y = "Latent variable 2"
  ) +
  ggnewscale::new_scale_color() +
  stat_ellipse(data = av.soil.lv.meta,
               aes(x = LV1, y = LV2,
                   group = Species,
                   color = Species),
               inherit.aes = FALSE) +
  scale_color_manual(name = "Species",
                     values = c("Avena" = "#F8766D","MH" = "#619CFF"),
                     guide = guide_legend(order = 1)) +
  scale_shape_manual(name = "Species",
                     breaks = c("Avena", "MH"),
                     values = c(
                       "Avena" = 16,
                       "MH" = 17),
                     guide = guide_legend(order = 1)) +
  theme(
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.key.size = grid::unit(0.6, "cm"),
    plot.title = element_text(size = 12)
  )
av.soil.lv.plot

## Soil Bromus-----
# extract scores to plot from model
br.soil.lv <- getLV(gllvm.br.soil.sp.precip,type = "residual")
br.soil.lv<- as.data.frame(br.soil.lv[, 1:2, drop = FALSE])

br.soil.lv$Sample <- rownames(br.soil.lv)

br.soil.lv.meta <- br.mh.meta %>%
  tibble::rownames_to_column("SampleID")
br.soil.lv.meta$Sample <- as.character((br.soil.lv.meta$Sample))

br.soil.lv.meta <- br.soil.lv %>%
  left_join(br.soil.lv.meta, by = "Sample")

#### plot
br.soil.lv.plot <- ggplot(br.soil.lv.meta,aes(x = LV1,y = LV2,
                                              color = Mean_Annual_Precip,
                                              shape = Species)) +
  geom_point(size = 3) +
  ggtitle("B. tectorum-Medusahead Soil Community") +
  scale_color_gradientn(
    colors = c("#83B3D6", "#558FBD", "#3B70A1", "#09335C"),
    name = "Mean annual precipitation"
  ) +
  labs(
    x = "Latent Variable 1",
    y = "Latent Variable 2"
  ) +
  ggnewscale::new_scale_color() +
  stat_ellipse(data = br.soil.lv.meta,
               aes(x = LV1, y = LV2,
                   group = Species,
                   color = Species),
               inherit.aes = FALSE) +
  scale_color_manual(name = "Species",
                     values=c("Bromus"="#7CAE00",
                              "MH"="#619CFF"))+
  # scale_shape_manual(name = "Species",
  #                    breaks = c("Bromus", "MH"),
  #                    values = c(
  #                      "Bromus" = 16,
  #                      "MH" = 17),
  #                    guide = guide_legend(order = 1)) +
  theme(
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.key.size = grid::unit(0.6, "cm"),
    plot.title = element_text(size = 12)
  )
br.soil.lv.plot

## Root Avena-----
# extract scores to plot from model
av.roots.lv <- getLV(gllvm.av.roots.spxprecip,type = "residual")
av.roots.lv<- as.data.frame(av.roots.lv[, 1:2, drop = FALSE])

av.roots.lv$Sample <- rownames(av.roots.lv)

av.roots.lv.meta <- av.mh.meta %>%
  tibble::rownames_to_column("SampleID")
av.roots.lv.meta$Sample <- as.character((av.roots.lv.meta$Sample))

av.roots.lv.meta <- av.roots.lv %>%
  left_join(av.roots.lv.meta, by = "Sample")

#### plot
av.roots.lv.plot <- ggplot(av.roots.lv.meta,aes(x = LV1,y = LV2,
                                                color = Mean_Annual_Precip,
                                                shape = Species)) +
  geom_point(size = 3) +
  ggtitle("A. barbata-Medusahead Root Community") +
  scale_color_gradientn(
    colors = c("#83B3D6", "#558FBD", "#3B70A1", "#09335C"),
    name = "Mean annual precipitation"
  ) +
  labs(
    x = "Latent variable 1",
    y = "Latent variable 2"
  ) +
  ggnewscale::new_scale_color() +
  stat_ellipse(data = av.roots.lv.meta,
               aes(x = LV1, y = LV2,
                   group = Species,
                   color = Species),
               inherit.aes = FALSE) +
  scale_color_manual(name = "Species",
                     values = c("Avena" = "#F8766D","MH" = "#619CFF"),
                     guide = guide_legend(order = 1)) +
  scale_shape_manual(name = "Species",
                     breaks = c("Avena", "MH"),
                     values = c(
                       "Avena" = 16,
                       "MH" = 17),
                     guide = guide_legend(order = 1)) +
  theme(    legend.text = element_text(size = 7),
            legend.title = element_text(size = 8),
            legend.key.size = grid::unit(0.6, "cm"),
            plot.title = element_text(size = 12)
  )
av.roots.lv.plot

## Root Bromus-----
# extract scores to plot from model
br.roots.lv <- getLV(gllvm.br.roots.spxprecip,type = "residual")
br.roots.lv<- as.data.frame(br.roots.lv[, 1:2, drop = FALSE])

br.roots.lv$Sample <- rownames(br.roots.lv)

br.roots.lv.meta <- br.mh.meta %>%
  tibble::rownames_to_column("SampleID")
br.roots.lv.meta$Sample <- as.character((br.roots.lv.meta$Sample))

br.roots.lv.meta <- br.roots.lv %>%
  left_join(br.roots.lv.meta, by = "Sample")

#### plot
br.roots.lv.plot <- ggplot(br.roots.lv.meta,aes(x = LV1,y = LV2,
                                                color = Mean_Annual_Precip,
                                                shape = Species)) +
  geom_point(size = 3) +
  ggtitle("B. tectorum-Medusahead Root Community") +
  scale_color_gradientn(
    colors = c("#83B3D6", "#558FBD", "#3B70A1", "#09335C"),
    name = "Mean annual precipitation"
  ) +
  labs(
    x = "Latent Variable 1",
    y = "Latent Variable 2"
  ) +
  ggnewscale::new_scale_color() +
  stat_ellipse(data = br.roots.lv.meta,
               aes(x = LV1, y = LV2,
                   group = Species,
                   color = Species),
               inherit.aes = FALSE) +
  scale_color_manual(name = "Species",
                     values=c("Bromus"="#7CAE00",
                              "MH"="#619CFF"))+
  # scale_shape_manual(name = "Species",
  #                    breaks = c("Bromus", "MH"),
  #                    values = c(
  #                      "Bromus" = 16,
  #                      "MH" = 17),
  #                    guide = guide_legend(order = 1)) +
  theme(
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.key.size = grid::unit(0.6, "cm"),
    plot.title = element_text(size = 12)
  )
br.roots.lv.plot

# Cowplot-------
##### Soil
plot_grid(av.soil.lv.plot, br.soil.lv.plot, 
          labels = c('a', 'b'), 
          label_size = 12)

##### Roots
plot_grid(av.roots.lv.plot, br.roots.lv.plot, 
          labels = c('c', 'd'), 
          label_size = 12)