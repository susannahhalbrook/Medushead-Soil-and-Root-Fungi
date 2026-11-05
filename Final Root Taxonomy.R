############# Taxonomy

#Library-----
library(phyloseq)
library(dplyr)
library(ggplot2)

#Prep------
rootra # use relative abundance data

# cut Mixed
rootra.edit <- prune_taxa(taxa_sums(subset_samples(rootra, Transect != "Mixed")) > 0,  subset_samples(rootra, Transect != "Mixed"))

# add precip
sample_data(rootra.edit)$Mean_Annual_Precip <- case_when(
  sample_data(rootra.edit)$Site_Code == "LS" ~ 427.87,
  sample_data(rootra.edit)$Site_Code == "SF" ~ 565.90,
  sample_data(rootra.edit)$Site_Code == "PLAV" ~ 881.55,
  sample_data(rootra.edit)$Site_Code == "PLBR" ~ 881.55,
  sample_data(rootra.edit)$Site_Code == "DC" ~ 575.78,
  sample_data(rootra.edit)$Site_Code == "TC" ~ 1091.88,
  sample_data(rootra.edit)$Site_Code == "PP" ~ 1094.94,
  TRUE ~ NA_real_
)

# Taxonomy----
# mostly asco (96%)

## basic-----
View(otu_table(rootra.edit))
#7507 otus

rootra.tax <- as.data.frame(tax_table(rootra.edit))

# Get ASVs that have a phylum assignment
phylum_assigned <- !is.na(rootra.tax$Phylum) & rootra.tax$Phylum != ""

# Keep only those ASVs
ps_phylum <- prune_taxa(phylum_assigned, rootra.edit)

# Number of samples containing at least one phylum-assigned ASV
View(otu_table(ps_phylum))
#10829 assigned to the phylum level

class_assigned <- !is.na(rootra.tax$Class) & rootra.tax$Class != ""
ps_class <- prune_taxa(class_assigned, rootra.edit)

# Number of samples containing at least one class-assigned ASV
View(otu_table(ps_class))
#10018=

## Stats----
rootra.tax <- as.data.frame(tax_table(rootra.edit))
#total ASVs 10933

sum(rootra.tax$Phylum == "p__Ascomycota", na.rm = TRUE) / nrow(rootra.tax) * 100
# 96% asco across all samples
# 10500 are Asco

### per species-------
rootra.av <- prune_taxa(taxa_sums(subset_samples(rootra.edit, Species == "Avena")) > 0, subset_samples(rootra.edit, Species == "Avena"))
rootra.av.tax <- as.data.frame(tax_table(rootra.av))
#5449 asvs

sum(rootra.av.tax$Phylum == "p__Ascomycota", na.rm = TRUE) / nrow(rootra.av.tax) * 100
# 96% asco 
#5239 are Asco

rootra.mh <- prune_taxa(taxa_sums(subset_samples(rootra.edit, Species == "MH")) > 0, subset_samples(rootra.edit, Species == "MH"))
rootra.mh.tax <- as.data.frame(tax_table(rootra.mh))
#6370

sum(rootra.mh.tax$Phylum == "p__Ascomycota", na.rm = TRUE) / nrow(rootra.mh.tax) * 100
# 96.3% asco 
#6135 are Asco

rootra.br <- prune_taxa(taxa_sums(subset_samples(rootra.edit, Species == "Bromus")) > 0, subset_samples(rootra.edit, Species == "Bromus"))
rootra.br.tax <- as.data.frame(tax_table(rootra.br))
#2065

sum(rootra.br.tax$Phylum == "p__Ascomycota", na.rm = TRUE) / nrow(rootra.br.tax) * 100
# 96% asco 
#1988 are Asco

### Class-----
rootra.tax.asco <- subset.data.frame(rootra.tax,rootra.tax$Phylum=="p__Ascomycota")

unique(rootra.tax.asco$Class) #17

class.percent <- rootra.tax.asco %>%
  count(Class) %>%
  mutate(Percent = n / sum(n) * 100)
class.percent.10 <- subset.data.frame(class.percent,class.percent$Percent >=5) # same 4

## check per species
#AV
rootra.av.class <- subset.data.frame(rootra.av.tax,rootra.av.tax$Phylum=="p__Ascomycota")
unique(rootra.av.class$Class) #17

class.percent.av <- rootra.av.class %>%
  count(Class) %>%
  mutate(Percent = n / sum(n) * 100)
class.percent.av.5 <- subset.data.frame(class.percent.av,class.percent.av$Percent >=5)

#MH
rootra.mh.class <- subset.data.frame(rootra.mh.tax,rootra.mh.tax$Phylum=="p__Ascomycota")
unique(rootra.mh.class$Class) #15

class.percent.mh <- rootra.mh.class %>%
  count(Class) %>%
  mutate(Percent = n / sum(n) * 100)
class.percent.mh.5 <- subset.data.frame(class.percent.mh,class.percent.mh$Percent >=5)

#Br
rootra.br.class <- subset.data.frame(rootra.br.tax,rootra.br.tax$Phylum=="p__Ascomycota")
unique(rootra.br.class$Class) #11

class.percent.br <- rootra.br.class %>%
  count(Class) %>%
  mutate(Percent = n / sum(n) * 100)
class.percent.br.5 <- subset.data.frame(class.percent.br,class.percent.br$Percent >=5)


## Figure----
#make it long form
rootra.tax <- psmelt(rootra.edit)
#get rid of random stuff
rootra.tax <- rootra.tax[,-c(11:25)]

#make version just asco
rootra.asco <- subset.data.frame(rootra.tax,rootra.tax$Phylum=="p__Ascomycota")

# aggregate
species.asco.mean <- rootra.asco %>%
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
  ggtitle("Root Ascomycota Classes by Plant Species")
ggsave("figures/root.asco.class.species.png",width = 6,height = 7)
