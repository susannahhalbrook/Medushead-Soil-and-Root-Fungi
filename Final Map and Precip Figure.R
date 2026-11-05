########### Map and Precip Figure

# Map------

## Libraries----
library(tidyverse)
library(ggmap)
library(cowplot)
register_google(key="AIzaSyAaWOnP04tLvc8Gm3XOk3DFExGY_WF0qDQ",write=TRUE)

## Base map-----
ca.map <- get_map(location=c(lon=-121.5,lat=39.5),
                  source = "google",
                  maptype = "satellite",
                  zoom=7)
ca <- ggmap(ca.map,extent= "panel")
plot(ca)

## Add Points----
Sites <- c("SFREC","Dye Creek","El Dorado (TC)","El Dorado (PP)","Plumas","Lassen")
Precip <- c("Low","Low","High","High","High","Low")
Lat <- c(39.26,40.11,38.87,38.79,39.98,40.38)
Long <- c(-121.33,-122.05,-120.82,-120.56,-120.90,-120.81)
Locations <- data.frame(Sites,Lat,Long,Precip)
nudge_x <- c(0,0,-0.1,1.6,0,0)
nudge_y <- c(0,0,-0.1,-0.5,0,0)
Locations$nudge_x <- nudge_x
Locations$nudge_y <- nudge_y
Locations$Precip <- factor(Locations$Precip,levels=c("Low","High"))
Locations
Locations$Mean.Precip <- c("566","575","1092","1295","882","428")
Locations$Mean.Precip <- as.numeric(Locations$Mean.Precip)

# SFREC: 39.26, -121.33
# DC: 40.11, -122.05
# TC: 38.87, -120.82
# PP: 38.79, -120.56
#Plumas: 39.98, -120.90
# Lassen: 40.38, -120.81

ca.map <- get_map(location=c(lon=-121.5,lat=39.5),
                  source = "google",
                  maptype = "satellite",
                  zoom=7)
map.plot <- ggmap(ca.map,extent= "panel")+
  geom_point(data=Locations,
             aes(x=Long,y=Lat,color=Mean.Precip),
             size=4)+
  scale_color_gradientn(
    colors = c(
      "#B9D7ED",
      "#90B8D4",
      "#69A3CF",
      "#3972A3",
      "#2D6191",
      "#1B4E85"
    )
  )+
  geom_label(data=Locations,
             aes(x=Long,y=Lat,label=Sites),
             hjust=1,vjust=-0.5,size=6,
             nudge_x = Locations$nudge_x,
             nudge_y = Locations$nudge_y)+
  ggtitle("Field Sites") +
  xlab("Longitute")+
  ylab("Latitude")+
  labs(color = "Mean Precip")+
  theme(legend.position = "left",
        text = element_text(size=16),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12))
ggsave("Figures/map.png",width = 8,height=8)

# Precip-----

## Libraries-----
library(dplyr)
library(tidyverse)
library(ggplot2)
library(nlme)
library(cowplot)

install.packages(c("ggpubr","broom"))
library(ggpubr)
library(broom)

## Prep-----
####Precip on Prism

dye_creek_precip <- read.csv("General data/dye_creek_prism.csv")
eld_pp_precip <- read.csv("General data/eld_pp_prism.csv")
eld_tc_precip <- read.csv("General data/eld_tc_prism.csv")
plumas_precip <- read.csv("General data/plumas_prism.csv")
lassen_precip <- read.csv("General data/lassen_prism.csv")
sfrec_precip <- read.csv("General data/sfrec_prism.csv")

dye_creek_precip$site <- "Dye Creek"
eld_pp_precip$site <- "El Dorado (PP)"
eld_tc_precip$site <- "El Dorado (TC)"
plumas_precip$site <- "Plumas"
lassen_precip$site <- "Lassen"
sfrec_precip$site <- "SFREC"

all_precip <- bind_rows(dye_creek_precip,eld_pp_precip,eld_tc_precip,plumas_precip,lassen_precip,sfrec_precip)
all_precip <- all_precip%>%
  rename(precip_inches=precip..inches.)
all_precip <- all_precip%>%
  rename(mean_temp=tmean..degrees.F.)
all_precip <- all_precip[,c(5,1,2,3)]
all_precip$month.name <- factor(all_precip$month,
                                levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
all_precip$Precip.Level[all_precip$site=="Dye Creek"] <- "Low"
all_precip$Precip.Level[all_precip$site=="El Dorado (PP)"] <- "High"
all_precip$Precip.Level[all_precip$site=="SFREC"] <- "Low"
all_precip$Precip.Level[all_precip$site=="Lassen"] <- "Low"
all_precip$Precip.Level[all_precip$site=="El Dorado (TC)"] <- "High"
all_precip$Precip.Level[all_precip$site=="Plumas"] <- "High"

# Convert in to cm
all_precip$precip_cm <- all_precip$precip_inches*2.54
#convert ot mm
all_precip$precip_mm <- all_precip$precip_cm*10

## Annual Summary--------
precip.annual <- all_precip %>%
  group_by(site,year) %>%
  summarise(Total_Annual=sum(precip_mm))

## Plot-----
annual.precip.plot <- ggplot(precip.annual,
                             aes(x=reorder(site,Total_Annual),
                                 y=Total_Annual,
                                 color = reorder(site, Total_Annual))) +
  geom_boxplot() +
  scale_color_manual(
    values = c(
      "#B9D7ED",
      "#90B8D4",
      "#69A3CF",
      "#3972A3",
      "#2D6191",
      "#1B4E85"
    )
  ) +
  ggtitle("Annual Precipitation by Site") +
  xlab("Site") +
  ylab("Precip (mm)") +
  theme(plot.margin = margin(10, 5, 5, 10))+
  theme(text = element_text(size=16),
        legend.position="none",
        axis.text.x=element_text(angle=45,vjust=1,hjust=1))
ggsave("Figures/annual_precip_mm.png",width = 8, height=8)

# Cowplot-----
plot_grid(map.plot,annual.precip.plot,
          labels=c("a","b"),label_size=24,
          rel_widths = c(1,0.9))
ggsave("Figures/map-precip.pdf",width = 15.35,height=7)