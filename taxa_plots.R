#######################################################################
# Heather Deel
# July 13th, 2021
# Code for relative abundance taxa plots with optimal color coding
# Written by Alex Emmons, original code here:
# /Users/heatherdeel/Dropbox/PMI_3_analyses/bone/01_16S/04_results_other/taxa_plots/CrazyColors_AE.R
#######################################################################

library(qiime2R)
library(phyloseq)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(randomcoloR)
library(tidyr)
library(ggpubr)
library(randomcoloR)
library(tibble)
library(ggpubr)
library(ggrepel)
library(viridis)
library(RColorBrewer)
library(forcats)

# for installing qiime2R if needed
# devtools::install_github("jbisanz/qiime2R")

### Relative abundance of taxa plot using fly only data at the class level

#make a phyloseq object
physeq_fly<-qza_to_phyloseq(
  features="/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/05_feature_tables/merged_table_nochlomito_filtered_flyonly_5937.qza", 
  metadata = "/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/02_metadata/merged_mapping_fly_flybodies_final_R.txt")

#taxa table as formatted wouldn't work with my qiime2 library, so reformatted and merged
taxa<-read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/03_taxonomy/taxonomy_R.qza")$data
tax<-taxa %>% separate("Taxon", c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ";", remove = TRUE,
                            convert = FALSE, extra = "warn", fill = "warn")

#getting rid of blanks and NAs and calling them "Unclassified"
tax[tax==" "]<-NA
tax[is.na(tax)]<-" Unclassified"

#formatting taxonomy table for merging
rownames(tax)<-tax$Feature.ID
tax<-tax[,-1]
tax<-as.matrix(tax)
tax<-tax_table(tax)

#remake physeq to include taxonomy
physeq_fly<-merge_phyloseq(physeq_fly,tax)

#combine at class level and transform abundance to relative abundance
physeq_fly_class <-physeq_fly %>%
  tax_glom(taxrank = "Class") %>%                     # agglomerate at level of interest
  phyloseq::transform_sample_counts(function(x) {x/sum(x)} )  # Transform to rel. abundance

#can use this for filtering rare taxa;  genera with a mean greater than 0.05 are kept. Works across entire data set
ftax<-physeq_fly_class %>% filter_taxa(function(x) mean(x) < 0.05, TRUE) %>% filter_taxa(function(x) sum(x > 0) > (0.02*length(x)), TRUE)
#get names of taxa with mean less than 0.05
ftax.names<-taxa_names(ftax)

#melt the physeq table
physeq_fly_class<-physeq_fly_class %>% psmelt()

#Average relative abundance by category; obviously you don't have to get a mean relative abundance 
class_fly_Avg <- physeq_fly_class %>%
  group_by(placement_season,sample_site_fly, Phylum, Class, OTU) %>%
  dplyr::summarise(avg.rel.abund = mean(Abundance)) %>%
  dplyr::arrange(placement_season,sample_site_fly) %>%
  unite(Taxa, c(Phylum, Class), sep = ",", remove = FALSE)

#assign rare taxa by sample
class_fly_Avg$Phylum[class_fly_Avg$avg.rel.abund <0.05] <- as.character(" Rare Taxa") #rename a genus to rare taxa if relative abundance is less than 0.05
class_fly_Avg$Taxa[class_fly_Avg$avg.rel.abund <0.05] <- as.character(" Rare Taxa") #rename a genus to rare taxa if relative abundance is less than 0.05

#assign to rare taxa alternative
#class_fly_Avg$Phylum[class_fly_Avg$OTU %in% ftax.names] <- as.character(" RareTaxa")
#class_fly_Avg$Taxa[class_fly_Avg$OTU %in% ftax.names] <- as.character(" RareTaxa")

#check for names of palettes to choose; see what for loops below require
#hcl.pals()
#display.brewer.all() 
#colors()

#make palette; need colors to equal phylum levels 
levels(as.factor(class_fly_Avg$Phylum)) #5 levels 

palette<-c("green","yellow","blue","red","monochrome")

#use these same colors for the same taxa; build a palette
myColors1 <- palette
names(myColors1)<-levels(as.factor(class_fly_Avg$Phylum))

#create column Phy_Colors; just adding the palette to my data frame
class_fly_Avg$Phy_Colors<-class_fly_Avg$Phylum
class_fly_Avg$Phy_Colors<-as.factor(class_fly_Avg$Phy_Colors)
levels(class_fly_Avg$Phy_Colors)<-c(palette)

###get a small summarised dataframe for color for loop
p<-class_fly_Avg %>% select_all() %>% group_by(Phylum,Phy_Colors)  %>%
  summarize(n_colors=n_distinct(Taxa)) %>%
  mutate_if(is.factor, as.character)

#gets unique colors for each phylum and save to object called phylum
phylum<-c()

################
#for loops; pick one but make sure you are using an appropriate palette
#################
#just give this color names from colors(); transitions to brown, which is annoying
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-scales::seq_gradient_pal(p$Phy_Colors[i])(seq(0,1,length.out=(p$n_colors[i])))
  phylum<-c(phylum,a)
}

#to be used with colors from hcl.pals()
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-hcl.colors(n=(p$n_colors[i]+1),palette=p$Phy_Colors[i], alpha=1,rev=FALSE)[-(p$n_colors[i]+1)] 
  phylum<-c(phylum,a)
}

#to be used with colors from display.brewer.all() 
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-colorRampPalette(brewer.pal(8, p$Phy_Colors[i])[3:8],bias=0.75,interpolate="linear")(p$n_colors[i]) 
  phylum<-c(phylum,a)
}

#if more random colors rather than sequential
library(randomcoloR) 
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-randomColor(count = p$n_colors[i], hue = p$Phy_Colors[i], luminosity = "bright")
  phylum<-c(phylum,a)
}

#get unique taxa to pair with colors
gen_colors<-class_fly_Avg %>% select_all() %>% 
  group_by(Phylum,Phy_Colors) %>% arrange(avg.rel.abund) %>%
  summarize(u_genera=unique(Taxa))

#add unique taxa to our newly made palette named phylum
names(phylum)<-gen_colors$u_genera

#need to reorder factor levels for the Taxa column so we get a nice gradient
class_fly_Avg$Taxa<- factor(class_fly_Avg$Taxa, levels = gen_colors$u_genera)

# reorder the months so they're in chronological order
class_fly_Avg$placement_season_chron = factor(class_fly_Avg$placement_season,
                                        levels = c("February","April","July"))

#plot
ggplot(data=class_fly_Avg, aes(x = sample_site_fly, y = avg.rel.abund)) + 
  geom_bar(aes(fill=Taxa),stat = "identity",color="black") +
  facet_grid(~placement_season_chron, scales = 'free', space = "free") +
  scale_fill_manual(values = phylum) +
  theme_classic()+
  theme(axis.title.x = element_text(size=10)) + 
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  xlab("Fly Organ by Placement Season")+
  ylab("Average Relative Abundance") +
  theme(legend.position = "bottom") +
  theme(axis.text.y=element_text(size=8),
        axis.title.y= element_text(size=10), legend.title=element_blank(), legend.text =element_text(size=10))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1, ncol = 3))

ggsave("flyonly_taxaplot_class.tif", units="in", width = 8.5, height = 5.5,  dpi=500, device="tiff")  

##### same taxa plot as above, but at the order level
# make sure to clear environment and start from scratch here

#make a phyloseq object
physeq_fly<-qza_to_phyloseq(
  features="/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/05_feature_tables/merged_table_nochlomito_filtered_flyonly_5937.qza", 
  metadata = "/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/02_metadata/merged_mapping_fly_flybodies_final_R.txt")

#taxa table as formatted wouldn't work with my qiime2 library, so reformatted and merged
taxa<-read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/03_taxonomy/taxonomy_R.qza")$data
tax<-taxa %>% separate("Taxon", c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ";", remove = TRUE,
                       convert = FALSE, extra = "warn", fill = "warn")

#getting rid of blanks and NAs and calling them "Unclassified"
tax[tax==" "]<-NA
tax[is.na(tax)]<-" Unclassified"

#formatting taxonomy table for merging
rownames(tax)<-tax$Feature.ID
tax<-tax[,-1]
tax<-as.matrix(tax)
tax<-tax_table(tax)

#remake physeq to include taxonomy
physeq_fly<-merge_phyloseq(physeq_fly,tax)

#combine at order level and transform abundance to relative abundance
physeq_fly_order <-physeq_fly %>%
  tax_glom(taxrank = "Order") %>%                     # agglomerate at level of interest
  phyloseq::transform_sample_counts(function(x) {x/sum(x)} )  # Transform to rel. abundance

#can use this for filtering rare taxa;  genera with a mean greater than 0.05 are kept. Works across entire data set
ftax<-physeq_fly_order %>% filter_taxa(function(x) mean(x) < 0.05, TRUE) %>% filter_taxa(function(x) sum(x > 0) > (0.02*length(x)), TRUE)
#get names of taxa with mean less than 0.05
ftax.names<-taxa_names(ftax)

#melt the physeq table
physeq_fly_order<-physeq_fly_order %>% psmelt()

#Average relative abundance by category; obviously you don't have to get a mean relative abundance 
order_fly_Avg <- physeq_fly_order %>%
  group_by(placement_season,sample_site_fly, Phylum, Class, Order, OTU) %>%
  dplyr::summarise(avg.rel.abund = mean(Abundance)) %>%
  dplyr::arrange(placement_season,sample_site_fly) %>%
  unite(Taxa, c(Phylum, Class, Order), sep = ",", remove = FALSE)

#assign rare taxa by sample
order_fly_Avg$Phylum[order_fly_Avg$avg.rel.abund <0.05] <- as.character(" Rare Taxa") #rename a genus to rare taxa if relative abundance is less than 0.05
order_fly_Avg$Taxa[order_fly_Avg$avg.rel.abund <0.05] <- as.character(" Rare Taxa") #rename a genus to rare taxa if relative abundance is less than 0.05

#assign to rare taxa alternative
#class_fly_Avg$Phylum[class_fly_Avg$OTU %in% ftax.names] <- as.character(" RareTaxa")
#class_fly_Avg$Taxa[class_fly_Avg$OTU %in% ftax.names] <- as.character(" RareTaxa")

#check for names of palettes to choose; see what for loops below require
#hcl.pals()
#display.brewer.all() 
#colors()

#make palette; need colors to equal phylum levels 
levels(as.factor(order_fly_Avg$Phylum)) #5 levels 

palette<-c("green","yellow","blue","red","monochrome")

#use these same colors for the same taxa; build a palette
myColors1 <- palette
names(myColors1)<-levels(as.factor(order_fly_Avg$Phylum))

#create column Phy_Colors; just adding the palette to my data frame
order_fly_Avg$Phy_Colors<-order_fly_Avg$Phylum
order_fly_Avg$Phy_Colors<-as.factor(order_fly_Avg$Phy_Colors)
levels(order_fly_Avg$Phy_Colors)<-c(palette)

###get a small summarised dataframe for color for loop
p<-order_fly_Avg %>% select_all() %>% group_by(Phylum,Phy_Colors)  %>%
  summarize(n_colors=n_distinct(Taxa)) %>%
  mutate_if(is.factor, as.character)

#gets unique colors for each phylum and save to object called phylum
phylum<-c()

################
#for loops; pick one but make sure you are using an appropriate palette
#################
#just give this color names from colors(); transitions to brown, which is annoying
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-scales::seq_gradient_pal(p$Phy_Colors[i])(seq(0,1,length.out=(p$n_colors[i])))
  phylum<-c(phylum,a)
}

#to be used with colors from hcl.pals()
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-hcl.colors(n=(p$n_colors[i]+1),palette=p$Phy_Colors[i], alpha=1,rev=FALSE)[-(p$n_colors[i]+1)] 
  phylum<-c(phylum,a)
}

#to be used with colors from display.brewer.all() 
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-colorRampPalette(brewer.pal(8, p$Phy_Colors[i])[3:8],bias=0.75,interpolate="linear")(p$n_colors[i]) 
  phylum<-c(phylum,a)
}

#if more random colors rather than sequential
library(randomcoloR) 
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-randomColor(count = p$n_colors[i], hue = p$Phy_Colors[i], luminosity = "bright")
  phylum<-c(phylum,a)
}

#get unique taxa to pair with colors
gen_colors<-order_fly_Avg %>% select_all() %>% 
  group_by(Phylum,Phy_Colors) %>% arrange(avg.rel.abund) %>%
  summarize(u_genera=unique(Taxa))

#add unique taxa to our newly made palette named phylum
names(phylum)<-gen_colors$u_genera

#need to reorder factor levels for the Taxa column so we get a nice gradient
order_fly_Avg$Taxa<- factor(order_fly_Avg$Taxa, levels = gen_colors$u_genera)

# reorder the months so they're in chronological order
order_fly_Avg$placement_season_chron = factor(order_fly_Avg$placement_season,
                                              levels = c("February","April","July"))

#plot
ggplot(data=order_fly_Avg, aes(x = sample_site_fly, y = avg.rel.abund)) + 
  geom_bar(aes(fill=Taxa),stat = "identity",color="black") +
  facet_grid(~placement_season_chron, scales = 'free', space = "free") +
  scale_fill_manual(values = phylum) +
  theme_classic()+
  theme(axis.title.x = element_text(size=10)) + 
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  xlab("Fly Organ by Placement Season")+
  ylab("Average Relative Abundance") +
  theme(legend.position = "bottom") +
  theme(axis.text.y=element_text(size=8),
        axis.title.y= element_text(size=10), legend.title=element_blank(), legend.text =element_text(size=6.5))+
  guides(fill = guide_legend(keywidth = 0.6, keyheight = 0.6, ncol = 3))

ggsave("flyonly_taxaplot_order.tif", units="in", width = 8.5, height = 5.5,  dpi=500, device="tiff")  

############ same taxa plot, but at the family level
# clear environment here and start from scratch

#make a phyloseq object
physeq_fly<-qza_to_phyloseq(
  features="/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/05_feature_tables/merged_table_nochlomito_filtered_flyonly_5937.qza", 
  metadata = "/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/02_metadata/merged_mapping_fly_flybodies_final_R.txt")

#taxa table as formatted wouldn't work with my qiime2 library, so reformatted and merged
taxa<-read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/03_taxonomy/taxonomy_R.qza")$data
tax<-taxa %>% separate("Taxon", c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ";", remove = TRUE,
                       convert = FALSE, extra = "warn", fill = "warn")

#getting rid of blanks and NAs and calling them "Unclassified"
tax[tax==" "]<-NA
tax[is.na(tax)]<-" Unclassified"

#formatting taxonomy table for merging
rownames(tax)<-tax$Feature.ID
tax<-tax[,-1]
tax<-as.matrix(tax)
tax<-tax_table(tax)

#remake physeq to include taxonomy
physeq_fly<-merge_phyloseq(physeq_fly,tax)

#combine at order level and transform abundance to relative abundance
physeq_fly_family <-physeq_fly %>%
  tax_glom(taxrank = "Family") %>%                     # agglomerate at level of interest
  phyloseq::transform_sample_counts(function(x) {x/sum(x)} )  # Transform to rel. abundance

#can use this for filtering rare taxa;  genera with a mean greater than 0.05 are kept. Works across entire data set
ftax<-physeq_fly_family %>% filter_taxa(function(x) mean(x) < 0.02, TRUE) %>% filter_taxa(function(x) sum(x > 0) > (0.02*length(x)), TRUE)
#get names of taxa with mean less than 0.05
ftax.names<-taxa_names(ftax)

#melt the physeq table
physeq_fly_family<-physeq_fly_family %>% psmelt()

#Average relative abundance by category; obviously you don't have to get a mean relative abundance 
family_fly_Avg <- physeq_fly_family %>%
  group_by(placement_season,sample_site_fly, Phylum, Class, Order, Family, OTU) %>%
  dplyr::summarise(avg.rel.abund = mean(Abundance)) %>%
  dplyr::arrange(placement_season,sample_site_fly) %>%
  unite(Taxa, c(Phylum, Class, Order, Family), sep = ",", remove = FALSE)

#assign rare taxa by sample
family_fly_Avg$Phylum[family_fly_Avg$avg.rel.abund <0.02] <- as.character(" Rare Taxa") #rename a genus to rare taxa if relative abundance is less than 0.05
family_fly_Avg$Taxa[family_fly_Avg$avg.rel.abund <0.02] <- as.character(" Rare Taxa") #rename a genus to rare taxa if relative abundance is less than 0.05

#assign to rare taxa alternative
#class_fly_Avg$Phylum[class_fly_Avg$OTU %in% ftax.names] <- as.character(" RareTaxa")
#class_fly_Avg$Taxa[class_fly_Avg$OTU %in% ftax.names] <- as.character(" RareTaxa")

#check for names of palettes to choose; see what for loops below require
#hcl.pals()
#display.brewer.all() 
#colors()

#make palette; need colors to equal phylum levels 
levels(as.factor(family_fly_Avg$Phylum)) #5 levels 

palette<-c("green","yellow","blue","red","monochrome")

#use these same colors for the same taxa; build a palette
myColors1 <- palette
names(myColors1)<-levels(as.factor(family_fly_Avg$Phylum))

#create column Phy_Colors; just adding the palette to my data frame
family_fly_Avg$Phy_Colors<-family_fly_Avg$Phylum
family_fly_Avg$Phy_Colors<-as.factor(family_fly_Avg$Phy_Colors)
levels(family_fly_Avg$Phy_Colors)<-c(palette)

###get a small summarised dataframe for color for loop
p<-family_fly_Avg %>% select_all() %>% group_by(Phylum,Phy_Colors)  %>%
  summarize(n_colors=n_distinct(Taxa)) %>%
  mutate_if(is.factor, as.character)

#gets unique colors for each phylum and save to object called phylum
phylum<-c()

################
#for loops; pick one but make sure you are using an appropriate palette
#################
#just give this color names from colors(); transitions to brown, which is annoying
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-scales::seq_gradient_pal(p$Phy_Colors[i])(seq(0,1,length.out=(p$n_colors[i])))
  phylum<-c(phylum,a)
}

#to be used with colors from hcl.pals()
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-hcl.colors(n=(p$n_colors[i]+1),palette=p$Phy_Colors[i], alpha=1,rev=FALSE)[-(p$n_colors[i]+1)] 
  phylum<-c(phylum,a)
}

#to be used with colors from display.brewer.all() 
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-colorRampPalette(brewer.pal(8, p$Phy_Colors[i])[3:8],bias=0.75,interpolate="linear")(p$n_colors[i]) 
  phylum<-c(phylum,a)
}

#if more random colors rather than sequential
library(randomcoloR) 
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-randomColor(count = p$n_colors[i], hue = p$Phy_Colors[i], luminosity = "bright")
  phylum<-c(phylum,a)
}

#get unique taxa to pair with colors
gen_colors<-family_fly_Avg %>% select_all() %>% 
  group_by(Phylum,Phy_Colors) %>% arrange(avg.rel.abund) %>%
  summarize(u_genera=unique(Taxa))

#add unique taxa to our newly made palette named phylum
names(phylum)<-gen_colors$u_genera

#need to reorder factor levels for the Taxa column so we get a nice gradient
family_fly_Avg$Taxa<- factor(family_fly_Avg$Taxa, levels = gen_colors$u_genera)

# reorder the months so they're in chronological order
family_fly_Avg$placement_season_chron = factor(family_fly_Avg$placement_season,
                                              levels = c("February","April","July"))

#plot
ggplot(data=family_fly_Avg, aes(x = sample_site_fly, y = avg.rel.abund)) + 
  geom_bar(aes(fill=Taxa),stat = "identity",color="black") +
  facet_grid(~placement_season_chron, scales = 'free', space = "free") +
  scale_fill_manual(values = phylum) +
  theme_classic()+
  theme(axis.title.x = element_text(size=10)) + 
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  xlab("Fly Organ by Placement Season")+
  ylab("Average Relative Abundance") +
  theme(legend.position = "bottom") +
  theme(axis.text.y=element_text(size=8),
        axis.title.y= element_text(size=10), legend.title=element_blank(), legend.text =element_text(size=6.5))+
  guides(fill = guide_legend(keywidth = 0.6, keyheight = 0.6, ncol = 2))

ggsave("flyonly_taxaplot_family.tif", units="in", width = 8.5, height = 5.5,  dpi=500, device="tiff")  

######## same plot as above, but now at the genus level
# clear environment here and start from scratch

#make a phyloseq object
physeq_fly<-qza_to_phyloseq(
  features="/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/05_feature_tables/merged_table_nochlomito_filtered_flyonly_5937.qza", 
  metadata = "/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/02_metadata/merged_mapping_fly_flybodies_final_R.txt")

#taxa table as formatted wouldn't work with my qiime2 library, so reformatted and merged
taxa<-read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/03_taxonomy/taxonomy_R.qza")$data
tax<-taxa %>% separate("Taxon", c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ";", remove = TRUE,
                       convert = FALSE, extra = "warn", fill = "warn")

#getting rid of blanks and NAs and calling them "Unclassified"
tax[tax==" "]<-NA
tax[is.na(tax)]<-" Unclassified"

#formatting taxonomy table for merging
rownames(tax)<-tax$Feature.ID
tax<-tax[,-1]
tax<-as.matrix(tax)
tax<-tax_table(tax)

#remake physeq to include taxonomy
physeq_fly<-merge_phyloseq(physeq_fly,tax)

#combine at genus level and transform abundance to relative abundance
physeq_fly_genus <-physeq_fly %>%
  tax_glom(taxrank = "Genus") %>%                     # agglomerate at level of interest
  phyloseq::transform_sample_counts(function(x) {x/sum(x)} )  # Transform to rel. abundance

#can use this for filtering rare taxa;  genera with a mean greater than 0.05 are kept. Works across entire data set
ftax<-physeq_fly_genus %>% filter_taxa(function(x) mean(x) < 0.01, TRUE) %>% filter_taxa(function(x) sum(x > 0) > (0.02*length(x)), TRUE)
#get names of taxa with mean less than 0.05
ftax.names<-taxa_names(ftax)

#melt the physeq table
physeq_fly_genus<-physeq_fly_genus %>% psmelt()

#Average relative abundance by category; obviously you don't have to get a mean relative abundance 
genus_fly_Avg <- physeq_fly_genus %>%
  group_by(placement_season,sample_site_fly, Phylum, Class, Order, Family, Genus, OTU) %>%
  dplyr::summarise(avg.rel.abund = mean(Abundance)) %>%
  dplyr::arrange(placement_season,sample_site_fly) %>%
  unite(Taxa, c(Phylum, Class, Order, Family, Genus), sep = ",", remove = FALSE)

#assign rare taxa by sample
genus_fly_Avg$Phylum[genus_fly_Avg$avg.rel.abund <0.01] <- as.character(" Rare Taxa") #rename a genus to rare taxa if relative abundance is less than 0.05
genus_fly_Avg$Taxa[genus_fly_Avg$avg.rel.abund <0.01] <- as.character(" Rare Taxa") #rename a genus to rare taxa if relative abundance is less than 0.05

#assign to rare taxa alternative
#class_fly_Avg$Phylum[class_fly_Avg$OTU %in% ftax.names] <- as.character(" RareTaxa")
#class_fly_Avg$Taxa[class_fly_Avg$OTU %in% ftax.names] <- as.character(" RareTaxa")

#check for names of palettes to choose; see what for loops below require
#hcl.pals()
#display.brewer.all() 
#colors()

#make palette; need colors to equal phylum levels 
levels(as.factor(genus_fly_Avg$Phylum)) #5 levels 

palette<-c("green","yellow","blue","red","monochrome")

#use these same colors for the same taxa; build a palette
myColors1 <- palette
names(myColors1)<-levels(as.factor(genus_fly_Avg$Phylum))

#create column Phy_Colors; just adding the palette to my data frame
genus_fly_Avg$Phy_Colors<-genus_fly_Avg$Phylum
genus_fly_Avg$Phy_Colors<-as.factor(genus_fly_Avg$Phy_Colors)
levels(genus_fly_Avg$Phy_Colors)<-c(palette)

###get a small summarised dataframe for color for loop
p<-genus_fly_Avg %>% select_all() %>% group_by(Phylum,Phy_Colors)  %>%
  summarize(n_colors=n_distinct(Taxa)) %>%
  mutate_if(is.factor, as.character)

#gets unique colors for each phylum and save to object called phylum
phylum<-c()

################
#for loops; pick one but make sure you are using an appropriate palette
#################
#just give this color names from colors(); transitions to brown, which is annoying
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-scales::seq_gradient_pal(p$Phy_Colors[i])(seq(0,1,length.out=(p$n_colors[i])))
  phylum<-c(phylum,a)
}

#to be used with colors from hcl.pals()
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-hcl.colors(n=(p$n_colors[i]+1),palette=p$Phy_Colors[i], alpha=1,rev=FALSE)[-(p$n_colors[i]+1)] 
  phylum<-c(phylum,a)
}

#to be used with colors from display.brewer.all() 
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-colorRampPalette(brewer.pal(8, p$Phy_Colors[i])[3:8],bias=0.75,interpolate="linear")(p$n_colors[i]) 
  phylum<-c(phylum,a)
}

#if more random colors rather than sequential
library(randomcoloR) 
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-randomColor(count = p$n_colors[i], hue = p$Phy_Colors[i], luminosity = "bright")
  phylum<-c(phylum,a)
}

#get unique taxa to pair with colors
gen_colors<-genus_fly_Avg %>% select_all() %>% 
  group_by(Phylum,Phy_Colors) %>% arrange(avg.rel.abund) %>%
  summarize(u_genera=unique(Taxa))

#add unique taxa to our newly made palette named phylum
names(phylum)<-gen_colors$u_genera

#need to reorder factor levels for the Taxa column so we get a nice gradient
genus_fly_Avg$Taxa<- factor(genus_fly_Avg$Taxa, levels = gen_colors$u_genera)

# reorder the months so they're in chronological order
genus_fly_Avg$placement_season_chron = factor(genus_fly_Avg$placement_season,
                                               levels = c("February","April","July"))

#plot
ggplot(data=genus_fly_Avg, aes(x = sample_site_fly, y = avg.rel.abund)) + 
  geom_bar(aes(fill=Taxa),stat = "identity",color="black") +
  facet_grid(~placement_season_chron, scales = 'free', space = "free") +
  scale_fill_manual(values = phylum) +
  theme_classic()+
  theme(axis.title.x = element_text(size=10)) + 
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  xlab("Fly Organ by Placement Season")+
  ylab("Average Relative Abundance") +
  theme(legend.position = "bottom") +
  theme(axis.text.y=element_text(size=8),
        axis.title.y= element_text(size=10), legend.title=element_blank(), legend.text =element_text(size=6))+
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5, ncol = 2))

ggsave("flyonly_taxaplot_genus.tif", units="in", width = 9, height = 7,  dpi=500, device="tiff")  
ggsave("flyonly_taxaplot_genus.png", units="in", width = 9, height = 7,  dpi=500, device="png")  

######## human data only, at the genus level
# clear environment here and start from scratch

#make a phyloseq object
physeq_human<-qza_to_phyloseq(
  features="/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/05_feature_tables/merged_table_nochlomito_filtered_humanonly_5937.qza", 
  metadata = "/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/02_metadata/merged_mapping_fly_flybodies_final_R.txt")

#taxa table as formatted wouldn't work with my qiime2 library, so reformatted and merged
taxa<-read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/03_taxonomy/taxonomy_R.qza")$data
tax<-taxa %>% separate("Taxon", c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ";", remove = TRUE,
                       convert = FALSE, extra = "warn", fill = "warn")

#getting rid of blanks and NAs and calling them "Unclassified"
tax[tax==" "]<-NA
tax[is.na(tax)]<-" Unclassified"

#formatting taxonomy table for merging
rownames(tax)<-tax$Feature.ID
tax<-tax[,-1]
tax<-as.matrix(tax)
tax<-tax_table(tax)

#remake physeq to include taxonomy
physeq_human<-merge_phyloseq(physeq_human,tax)

#combine at genus level and transform abundance to relative abundance
physeq_human_genus <-physeq_human %>%
  tax_glom(taxrank = "Genus") %>%                     # agglomerate at level of interest
  phyloseq::transform_sample_counts(function(x) {x/sum(x)} )  # Transform to rel. abundance

#can use this for filtering rare taxa;  genera with a mean greater than 0.05 are kept. Works across entire data set
ftax<-physeq_human_genus %>% filter_taxa(function(x) mean(x) < 0.01, TRUE) %>% filter_taxa(function(x) sum(x > 0) > (0.02*length(x)), TRUE)
#get names of taxa with mean less than 0.05
ftax.names<-taxa_names(ftax)

#melt the physeq table
physeq_human_genus<-physeq_human_genus %>% psmelt()

#Average relative abundance by category; obviously you don't have to get a mean relative abundance 
genus_human_Avg <- physeq_human_genus %>%
  group_by(placement_season,sample_type_combo, Phylum, Class, Order, Family, Genus, OTU) %>%
  dplyr::summarise(avg.rel.abund = mean(Abundance)) %>%
  dplyr::arrange(placement_season,sample_type_combo) %>%
  unite(Taxa, c(Phylum, Class, Order, Family, Genus), sep = ",", remove = FALSE)

#assign rare taxa by sample
genus_human_Avg$Phylum[genus_human_Avg$avg.rel.abund <0.01] <- as.character(" Rare Taxa") #rename a genus to rare taxa if relative abundance is less than 0.05
genus_human_Avg$Taxa[genus_human_Avg$avg.rel.abund <0.01] <- as.character(" Rare Taxa") #rename a genus to rare taxa if relative abundance is less than 0.05

#assign to rare taxa alternative
#class_fly_Avg$Phylum[class_fly_Avg$OTU %in% ftax.names] <- as.character(" RareTaxa")
#class_fly_Avg$Taxa[class_fly_Avg$OTU %in% ftax.names] <- as.character(" RareTaxa")

#check for names of palettes to choose; see what for loops below require
#hcl.pals()
#display.brewer.all() 
#colors()

#make palette; need colors to equal phylum levels 
levels(as.factor(genus_human_Avg$Phylum)) #6 levels 

palette<-c("green","yellow","blue","orange","red","monochrome")

#use these same colors for the same taxa; build a palette
myColors1 <- palette
names(myColors1)<-levels(as.factor(genus_human_Avg$Phylum))

#create column Phy_Colors; just adding the palette to my data frame
genus_human_Avg$Phy_Colors<-genus_human_Avg$Phylum
genus_human_Avg$Phy_Colors<-as.factor(genus_human_Avg$Phy_Colors)
levels(genus_human_Avg$Phy_Colors)<-c(palette)

###get a small summarised dataframe for color for loop
p<-genus_human_Avg %>% select_all() %>% group_by(Phylum,Phy_Colors)  %>%
  summarize(n_colors=n_distinct(Taxa)) %>%
  mutate_if(is.factor, as.character)

#gets unique colors for each phylum and save to object called phylum
phylum<-c()

################
#for loops; pick one but make sure you are using an appropriate palette
#################
#just give this color names from colors(); transitions to brown, which is annoying
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-scales::seq_gradient_pal(p$Phy_Colors[i])(seq(0,1,length.out=(p$n_colors[i])))
  phylum<-c(phylum,a)
}

#to be used with colors from hcl.pals()
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-hcl.colors(n=(p$n_colors[i]+1),palette=p$Phy_Colors[i], alpha=1,rev=FALSE)[-(p$n_colors[i]+1)] 
  phylum<-c(phylum,a)
}

#to be used with colors from display.brewer.all() 
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-colorRampPalette(brewer.pal(8, p$Phy_Colors[i])[3:8],bias=0.75,interpolate="linear")(p$n_colors[i]) 
  phylum<-c(phylum,a)
}

#if more random colors rather than sequential
library(randomcoloR) 
for(i in 1:nrow(p)) {
  print(p$Phylum[i])
  a<-randomColor(count = p$n_colors[i], hue = p$Phy_Colors[i], luminosity = "bright")
  phylum<-c(phylum,a)
}

#get unique taxa to pair with colors
gen_colors<-genus_human_Avg %>% select_all() %>% 
  group_by(Phylum,Phy_Colors) %>% arrange(avg.rel.abund) %>%
  summarize(u_genera=unique(Taxa))

#add unique taxa to our newly made palette named phylum
names(phylum)<-gen_colors$u_genera

#need to reorder factor levels for the Taxa column so we get a nice gradient
genus_human_Avg$Taxa<- factor(genus_human_Avg$Taxa, levels = gen_colors$u_genera)

# reorder the months so they're in chronological order
genus_human_Avg$placement_season_chron = factor(genus_human_Avg$placement_season,
                                              levels = c("February","April","July"))

#plot
ggplot(data=genus_human_Avg, aes(x = sample_type_combo, y = avg.rel.abund)) + 
  geom_bar(aes(fill=Taxa),stat = "identity",color="black") +
  facet_grid(~placement_season_chron, scales = 'free', space = "free") +
  scale_fill_manual(values = phylum) +
  theme_classic()+
  theme(axis.title.x = element_text(size=10)) + 
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  xlab("Human Sample Types by Placement Season")+
  scale_x_discrete(labels = c("bicep","face","fecal","inner cheek","torso")) +
  ylab("Average Relative Abundance") +
  theme(legend.position = "bottom") +
  theme(axis.text.y=element_text(size=8),
        axis.title.y= element_text(size=10), legend.title=element_blank(), legend.text = element_text(size=5.2))+
  guides(fill = guide_legend(keywidth = 0.45, keyheight = 0.45, ncol = 2))

setwd("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results")
ggsave("humanonly_taxaplot_genus.tif", units="in", width = 9, height = 7,  dpi=500, device="tiff")  
ggsave("humanonly_taxaplot_genus.png", units="in", width = 9, height = 7,  dpi=500, device="png")  





