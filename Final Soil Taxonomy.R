############# Taxonomy

#Library-----
library(phyloseq)
library(dplyr)
library(ggplot2)

#Prep------
soilra # use relative abundance data

# cut Mixed
soilra.edit <- prune_taxa(taxa_sums(subset_samples(soilra, 
                                                   Transect != "Mixed")) > 0, 
                          subset_samples(soilra, Transect != "Mixed"))

# add precip
sample_data(soilra.edit)$Mean_Annual_Precip <- case_when(
  sample_data(soilra.edit)$Site_Code == "LS" ~ 427.87,
  sample_data(soilra.edit)$Site_Code == "SF" ~ 565.90,
  sample_data(soilra.edit)$Site_Code == "PLAV" ~ 881.55,
  sample_data(soilra.edit)$Site_Code == "PLBR" ~ 881.55,
  sample_data(soilra.edit)$Site_Code == "DC" ~ 575.78,
  sample_data(soilra.edit)$Site_Code == "TC" ~ 1091.88,
  sample_data(soilra.edit)$Site_Code == "PP" ~ 1094.94,
  TRUE ~ NA_real_
)

estimate_richness(soilra.edit)

# Taxonomy----

## Basic-----
View(otu_table(soilra.edit))
#7507 otus

soilra.tax <- as.data.frame(tax_table(soilra.edit))

# Get ASVs that have a phylum assignment
phylum_assigned <- !is.na(soilra.tax$Phylum) & soilra.tax$Phylum != ""

# Keep only those ASVs
ps_phylum <- prune_taxa(phylum_assigned, soilra.edit)

# Number of samples containing at least one phylum-assigned ASV
View(otu_table(ps_phylum))
#7332= assigned to the phylum level

class_assigned <- !is.na(soilra.tax$Class) & soilra.tax$Class != ""
ps_class <- prune_taxa(class_assigned, soilra.edit)

# Number of samples containing at least one class-assigned ASV
View(otu_table(ps_class))
#6530= 90%

# mostly asco (91%)

## Stats----
soilra.tax <- as.data.frame(tax_table(soilra.edit))
#total ASVs 7507

sum(soilra.tax$Phylum == "p__Ascomycota", na.rm = TRUE) / nrow(soilra.tax) * 100
# 91% asco across all samples
# 6833 are Asco

### per species-------
soilra.av <- prune_taxa(taxa_sums(subset_samples(soilra.edit, Species == "Avena")) > 0, subset_samples(soilra.edit, Species == "Avena"))
#3430 asvs
soilra.av.tax <- as.data.frame(tax_table(soilra.av))

sum(soilra.av.tax$Phylum == "p__Ascomycota", na.rm = TRUE) / nrow(soilra.av.tax) * 100
# 92.2% asco 
#3162 are Asco

soilra.mh <- prune_taxa(taxa_sums(subset_samples(soilra.edit, Species == "MH")) > 0, subset_samples(soilra.edit, Species == "MH"))
soilra.mh.tax <- as.data.frame(tax_table(soilra.mh))
#3339

sum(soilra.mh.tax$Phylum == "p__Ascomycota", na.rm = TRUE) / nrow(soilra.mh.tax) * 100
# 92% asco 
#3072 are Asco

soilra.br <- prune_taxa(taxa_sums(subset_samples(soilra.edit, Species == "Bromus")) > 0, subset_samples(soilra.edit, Species == "Bromus"))
soilra.br.tax <- as.data.frame(tax_table(soilra.br))
#3473

sum(soilra.br.tax$Phylum == "p__Ascomycota", na.rm = TRUE) / nrow(soilra.br.tax) * 100
# 91% asco 
#3173 are Asco

### Class-----
soilra.tax.asco <- subset.data.frame(soilra.tax,soilra.tax$Phylum=="p__Ascomycota")

unique(soilra.tax.asco$Class) #18

class.percent <- soilra.tax.asco %>%
  count(Class) %>%
  mutate(Percent = n / sum(n) * 100)
class.percent.10 <- subset.data.frame(class.percent,class.percent$Percent >=5)

## check per species
#AV
soilra.av.class <- subset.data.frame(soilra.av.tax,soilra.av.tax$Phylum=="p__Ascomycota")
unique(soilra.av.class$Class) #17

class.percent.av <- soilra.av.class %>%
  count(Class) %>%
  mutate(Percent = n / sum(n) * 100)
class.percent.av.5 <- subset.data.frame(class.percent.av,class.percent.av$Percent >=5)

#MH
soilra.mh.class <- subset.data.frame(soilra.mh.tax,soilra.mh.tax$Phylum=="p__Ascomycota")
unique(soilra.mh.class$Class) #16

class.percent.mh <- soilra.mh.class %>%
  count(Class) %>%
  mutate(Percent = n / sum(n) * 100)
class.percent.mh.5 <- subset.data.frame(class.percent.mh,class.percent.mh$Percent >=5)

#Br
soilra.br.class <- subset.data.frame(soilra.br.tax,soilra.br.tax$Phylum=="p__Ascomycota")
unique(soilra.br.class$Class) #16

class.percent.br <- soilra.br.class %>%
  count(Class) %>%
  mutate(Percent = n / sum(n) * 100)
class.percent.br.5 <- subset.data.frame(class.percent.br,class.percent.br$Percent >=5)


## Figure----
#make it long form
soilra.tax <- psmelt(soilra.edit)
#get rid of random stuff
soilra.tax <- soilra.tax[,-c(11:25)]

#make version just asco
soilra.asco <- subset.data.frame(soilra.tax,soilra.tax$Phylum=="p__Ascomycota")

# aggregate
species.asco.mean <- soilra.asco %>%
  group_by(sample_Species, Class) %>%
  summarise(Abundance = mean(Abundance, na.rm = TRUE),
    .groups = "drop")

# plot
### tweaks
species.asco.mean$sample_Species <- factor(species.asco.mean$sample_Species,levels=c("Avena","MH","Bromus"))
species.asco.mean$Class <- sub("^c__","",species.asco.mean$Class)

asco.class.species <- ggplot(species.asco.mean,aes(x=sample_Species,y=Abundance,fill=Class))+
  geom_col(position="fill",color="black")+
  xlab("Species")+
  ylab("Mean Relative Abundance")+
  ggtitle("Soil Ascomycota Classes by Plant Species")
ggsave("figures/soil.asco.class.species.png",width = 6,height = 7)
