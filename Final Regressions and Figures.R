########## Final Richness and Guild Models and Figures

#Libraries ====
library(nlme)
library(multcomp)
library(ggplot2)
library(dplyr)
library(cowplot)

# Prep------

########### Soil
soil.rich.new <- read.csv("soil.rich.csv")
rownames(soil.rich.new) <- soil.rich.new$Sample
soil.rich.new <- soil.rich.new[,-1]
soil.rich.merge <- soil.rich[,c(1,8:15,28:31)]
soil.rich.new <- left_join(soil.rich.new,soil.rich.merge,by="Sample")
write.csv(soil.rich.new,"data/soil.rich.new.csv")

# make plumas the same under Site
soil.rich <- soil.rich %>%
  mutate(Site = recode(Site,
                       "Plumas-Avena" = "Plumas",
                       "Plumas-Bromus" = "Plumas"))
soilrafun <- soilrafun %>%
  mutate(Site = recode(Site,
                       "Plumas-Avena" = "Plumas",
                       "Plumas-Bromus" = "Plumas"))

# add annual precip averages by site
soil.rich <- soil.rich %>%
  mutate(Mean_Annual_Precip=case_when(
    Site_Code == "LS" ~ 427.87,
    Site_Code == "SF" ~ 565.90,
    Site_Code == "PLAV" ~ 881.55,
    Site_Code == "PLBR" ~ 881.55,
    Site_Code == "DC" ~ 575.78,
    Site_Code == "TC" ~ 1091.88,
    Site_Code == "PP" ~ 1094.94,
    TRUE ~ NA_real_
  ))

soilrafun <- read.csv("data/soilrafun.csv")
soilrafun <- soilrafun %>%
  mutate(Mean_Annual_Precip=case_when(
    Site_Code == "LS" ~ 427.87,
    Site_Code == "SF" ~ 565.90,
    Site_Code == "PLAV" ~ 881.55,
    Site_Code == "PLBR" ~ 881.55,
    Site_Code == "DC" ~ 575.78,
    Site_Code == "TC" ~ 1091.88,
    Site_Code == "PP" ~ 1094.94,
    TRUE ~ NA_real_
  ))

#Square Root transformed soil symbionts
#Log transform didn't help
soilrafun$Sym.sqr <- sqrt(soilrafun$Symbiotroph)

########## Roots
root.rich # i recently edited this dataframe, need to adjust the data
root.rich.new <- read.csv("root.rich.csv")
rownames(root.rich.new) <- root.rich.new$Sample
root.rich.new <- root.rich.new[,-1]
root.rich.merge <- root.rich[,c(1,8:17)]
root.rich.merge$Sample <- as.numeric(root.rich.merge$Sample)
root.rich.new <- left_join(root.rich.new,root.rich.merge,by="Sample")
write.csv(root.rich.new,"data/root.rich.new.csv")

root.rich <- subset.data.frame(root.rich,root.rich$Species=="Forage"|
                                 root.rich$Species=="MH")

root.rich$SpeciesxPrecip <- factor(root.rich$SpeciesxPrecip,levels=c("Forage_Low","MH_Low","Forage_High","MH_High"))
root.rich$Species <- factor(root.rich$Species,levels=c("Forage","MH"))

rootrafun <- subset.data.frame(rootrafun,rootrafun$Species=="Forage"|
                                 rootrafun$Species=="MH")
rootrafun$SpeciesxPrecip <- factor(rootrafun$SpeciesxPrecip,levels=c("Forage_Low","MH_Low","Forage_High","MH_High"))
rootrafun$Species <- factor(rootrafun$Species,levels=c("Forage","MH"))

root.rich <- unite(data= root.rich,
                   col=SpeciesxPrecip,
                   c("Species","Precip_Level"),
                   sep= "_")
root.rich$SpeciesxPrecip <- factor(root.rich$SpeciesxPrecip, levels = c("Avena_Low","Avena_High","Bromus_Low","Bromus_High","MH_Low","MH_High"))

root.rich <- separate(root.rich,SpeciesxPrecip,into=c("Species","Precip_Level"),sep = "_",remove=F)
root.rich$Species <- factor(root.rich$Species,levels = c("Avena","Bromus","MH"))
root.rich$Precip_Level <- factor(root.rich$Precip_Level,levels = c("Low","High"))

root.rich <- root.rich %>%
  left_join(meta %>% select(Sample, gravimentric_content),by="Sample")

# make plumas the same under Site
root.rich <- root.rich %>%
  mutate(Site = recode(Site,
                       "Plumas-Avena" = "Plumas",
                       "Plumas-Bromus" = "Plumas"))
rootrafun <- rootrafun %>%
  mutate(Site = recode(Site,
                       "Plumas-Avena" = "Plumas",
                       "Plumas-Bromus" = "Plumas"))

root.rich <- root.rich %>%
  mutate(Mean_Annual_Precip=case_when(
    Site== "Lassen" ~ 427.87,
    Site == "SFREC" ~ 565.90,
    Site == "Plumas-Avena" ~ 881.55,
    Site == "Plumas-Bromus" ~ 881.55,
    Site== "Dye Creek" ~ 575.78,
    Site == "Traverse Creek" ~ 1091.88,
    Site == "Pollock Pines" ~ 1094.94,
    TRUE ~ NA_real_
  ))

#Square Root transformed soil symbionts
#Log transform didn't help
rootrafun$Sym.sqr <- sqrt(rootrafun$Symbiotroph)

rootrafun.label <- unite(data= rootrafun.label,
                         col=SpeciesxPrecip,
                         c("Species","Precip_Level"),
                         sep= "_")
rootrafun.label <- separate(rootrafun.label,SpeciesxPrecip,into=c("Species","Precip_Level"),sep = "_",remove=F)
rootrafun.label$Species <- factor(rootrafun.label$Species,levels = c("Avena","Bromus","MH"))
rootrafun.label$Precip_Level <- factor(rootrafun.label$Precip_Level,levels = c("Low","High"))

rootrafun.label$SpeciesxPrecip <- factor(rootrafun.label$SpeciesxPrecip, levels = c("Avena_Low","Avena_High","Bromus_Low","Bromus_High","MH_Low","MH_High"))

rootrafun <- rootrafun %>%
  left_join(meta %>% select(Sample, gravimentric_content),by="Sample")

rootrafun <- rootrafun %>%
  mutate(Mean_Annual_Precip=case_when(
    Site== "Lassen" ~ 427.87,
    Site == "SFREC" ~ 565.90,
    Site == "Plumas-Avena" ~ 881.55,
    Site == "Plumas-Bromus" ~ 881.55,
    Site== "Dye Creek" ~ 575.78,
    Site == "Traverse Creek" ~ 1091.88,
    Site == "Pollock Pines" ~ 1094.94,
    TRUE ~ NA_real_
  ))

# Soil Final Models------

##Richness----
rich.gls <- lme(RichnessITS~Species*Mean_Annual_Precip,
                random=~1|Site,
                method="REML",
                data=soil.rich)

soilrichplot <- ggplot(data=soil.rich.new,aes(Mean_Annual_Precip,RichnessITS,color=Species))+
  geom_point(alpha=0.7,
             position=position_jitter(width=20))+
  geom_smooth(method=lm,aes(color=Species),se=FALSE)+
  ggtitle("Soil Richness")+
  #ylim(0,625)+
  ylab("Richness")+
  theme(legend.position = "none",
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.key.size = unit(0.4,"cm"),
        plot.title = element_text(size=10),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())

########### Roots
root.rich.label <- rootrich
levels(root.rich.label$Species)[3] <- 'Local' #need to add a new factor level before I can swap forage to loca, or it won't take it
root.rich.label$Species <- replace(root.rich.label$Species, root.rich.label$Species=="Forage", "Local")
root.rich.label$Species <- factor(root.rich.label$Species,levels = c("Local","MH"))

rootfun.label <- rootfun
rootfun.label$Species <- replace(rootfun.label$Species, rootfun.label$Species=="Forage", "Local")
rootfun.label$Species <- factor(rootfun.label$Species,levels = c("Local","MH"))


##Symbiont----
symb.gls <- lme(Symbiotroph~Species*Mean_Annual_Precip,
                random=~1|Site,
                method="REML",
                weights=varIdent(form = ~1|Site),
                data=soilrafun)

#plot
soilsymplot <- ggplot(data=soilfun,
                      aes(Mean_Annual_Precip,Symbiotroph,color=Species))+
  geom_point(alpha=0.7,
             position=position_jitter(width=20))+
  geom_smooth(method=lm,aes(color=Species),se=FALSE)+
  ggtitle("Soil Symbionts")+
  ylab("Relative Abundance")+
  #ylim(0,0.3)+
  theme(legend.position = "none",
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.key.size = unit(0.4,"cm"),
        plot.title = element_text(size=10),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())
##Pathogen----
path.gls <- lme(Pathotroph~Species*Mean_Annual_Precip,
                method="REML",
                random=~1|Site,
                weights=varIdent(form = ~1|Site),
                data=soilrafun)
#plot
soilpathplot <- ggplot(data=soilfun,aes(Mean_Annual_Precip,Pathotroph,color=Species))+
  geom_point(alpha=0.7,
             position=position_jitter(width=20))+
  geom_smooth(method=lm,aes(color=Species),se=FALSE)+
  ggtitle("Soil Pathogens")+
  xlab("Precipitation (mm)")+
  ylab("Relative Abundance")+
  #ylim(0,0.35)+
  theme(legend.position = "none",
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.key.size = unit(0.4,"cm"),
        plot.title = element_text(size=10))

##Sap====
sap.gls <- lme(Saprotroph~Species*Mean_Annual_Precip,
               method="REML",
               random=~1|Site,
               weights=varIdent(form = ~1|Site),
               data=soilrafun)
#plot
soilsapplot <- ggplot(data=soilfun,aes(Mean_Annual_Precip,Saprotroph,color=Species))+
  geom_point(alpha=0.7,
             position=position_jitter(width=20))+
  geom_smooth(method=lm,aes(color=Species),se=FALSE)+
  ggtitle("Soil Saprotrophs")+
  ylab("Relative Abundance")+
  xlab("Precipitation (mm)")+
  ylim(0,0.76)+
  theme(legend.position = "none",
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.key.size = unit(0.4,"cm"),
        plot.title = element_text(size=10))

# Root Final Models-----
##Richness----
rich.gls <- lme(RichnessITS~Species*Mean_Annual_Precip,
                random=~1|Site,
                method="REML",
                data=root.rich)

#plot
rootrichplot <- ggplot(data=root.rich.new,aes(Mean_Annual_Precip,RichnessITS,color=Species))+
  geom_point(alpha=0.7,
             position=position_jitter(width=20))+
  geom_smooth(method=lm,aes(color=Species),se=FALSE)+
  ggtitle("Root Richness")+
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.key.size = unit(0.6,"cm"),
        plot.title = element_text(size=10),
        axis.text.x=element_blank(),
        axis.title=element_blank(),
        axis.ticks.x=element_blank())

##Symbiont----
symb.gls <- lme(Symbiotroph~Species*Mean_Annual_Precip,
                random=~1|Site,
                method="REML",
                weights=varIdent(form = ~1|Site),
                data=rootrafun)

#plot
rootsymplot <- ggplot(data=rootfun,aes(Mean_Annual_Precip,Symbiotroph,color=Species))+
  geom_point(alpha=0.7,
             position=position_jitter(width=20))+
  geom_smooth(method=lm,aes(color=Species),se=FALSE)+
  ylim(0,0.04)+
  ggtitle("Root Symbionts")+
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.key.size = unit(0.6,"cm"),
        plot.title = element_text(size=10),
        axis.text.x=element_blank(),
        axis.title=element_blank(),
        axis.ticks.x=element_blank())


##Pathogen----
path.gls <- lme(Pathotroph~Species*Mean_Annual_Precip,
                random=~1|Site,
                method="REML",
                data=rootrafun)


#plot
rootpathplot <- ggplot(data=rootfun,aes(Mean_Annual_Precip,Pathotroph,color=Species))+
  geom_point(alpha=0.7,
             position=position_jitter(width=20))+
  geom_smooth(method=lm,aes(color=Species),se=FALSE)+
  #ylim(0,0.6)+
  xlab("Precipitation (mm)")+
  ggtitle("Root Pathogens")+
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.key.size = unit(0.6,"cm"),
        plot.title = element_text(size=10),
        axis.title.y=element_blank())


##Sap====
sap.gls <- lme(Saprotroph~Species*Mean_Annual_Precip,
               random=~1|Site,
               method="REML",
               data=rootrafun)

#plot
rootsapplot <- ggplot(data=rootfun,aes(Mean_Annual_Precip,Saprotroph,color=Species))+
  geom_point(alpha=0.7,
             position=position_jitter(width=20))+
  geom_smooth(method=lm,aes(color=Species),se=FALSE)+
  ylim(0,0.76)+
  xlab("Precipitation (mm)")+
  ggtitle("Root Saprotrophs")+
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.key.size = unit(0.6,"cm"),
        plot.title = element_text(size=10),
        axis.title.y=element_blank())

# Cow plot-----
# only rich and symb
plot_grid(soilrichplot,rootrichplot,soilsymplot,rootsymplot,soilpathplot,rootpathplot,
          labels = c("a",'b','c','d','e','f'),
          label_size=12,
          ncol=2,
          rel_widths=c(1,1.2),
          rel_heights = c(1,1,1.2))

# only sap
plot_grid(soilsapplot,rootsapplot,
          labels = c("a",'b','c','d'),
          label_size=12,
          ncol=2,
          rel_widths=c(1,1.25))