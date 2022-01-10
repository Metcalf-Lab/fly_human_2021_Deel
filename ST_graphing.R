########################################################################
# Heather Deel
# July 9th, 2021
# Source proportion graphing of fly/human data collected at SHSU/STAFS
########################################################################

#################################################
# Libraries
# Probably don't need all of these but it's fine
#################################################
library(ggplot2)
library(reshape2)
library(plyr)
library(ggpubr)
library(RColorBrewer)
library(randomcoloR)
library(tidyverse)
library(qiime2R)
library(rstatix)
library(rlang)
library(qiime2R)
library(phyloseq)
library(tibble)
library(ggrepel)
library(viridis)
library("ggpattern") 
library(sf)

# for ggpattern
#install.packages("remotes")
#remotes::install_github("coolbutuseless/ggpattern")

##############################################
# Source proportion graphing
##############################################

### April

st_april <- read.csv("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/09_sourcetracking/STout_april/mixing_proportions_april_R.csv")
st_april <- ddply(melt(st_april, id.vars = 'sample_type'), .(sample_type))

library(dplyr)   ### LOAD THIS HERE, NOT ABOVE
st_long_prop_april <- st_april %>% group_by(sample_type,variable) %>% summarise(Source_proportion = mean(value))

black.bold.text = element_text(face = "bold", color = "black")
black.text = element_text(color = "black")

plot_april <- ggplot(st_long_prop_april, aes(x = sample_type, y = Source_proportion, fill = variable)) + 
  geom_col_pattern(fill = "white",
                   color = "black",
                   pattern_color = "black",
                   pattern_fill = "black",
                   pattern_density = 0.35,
                   aes(pattern = variable,
                       pattern_angle = variable,
                       pattern_spacing = variable)) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.text.x = element_text(color = "black", angle = 90, hjust = 0.5, vjust = 0.5), 
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", size = 18),
        title = black.bold.text, axis.title = black.bold.text,
        plot.background = element_rect(fill='white'),
        legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_x_discrete(labels=c("bicep", "face", "fecal", "inner cheek", "torso")) +
  labs(title = "April", x = "Sample Type", y = "Source Proportion")
plot_april

### February

st_feb <- read.csv("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/09_sourcetracking/STout_feb_no247/mixing_proportions_no247_feb_R.csv")
st_feb <- ddply(melt(st_feb, id.vars = 'sample_type'), .(sample_type))

st_long_prop_feb <- st_feb %>% group_by(sample_type,variable) %>% summarise(Source_proportion = mean(value))

black.bold.text = element_text(face = "bold", color = "black")
black.text = element_text(color = "black")

plot_feb <- ggplot(st_long_prop_feb, aes(x = sample_type, y = Source_proportion, fill = variable)) + 
  geom_col_pattern(fill = "white",
                   color = "black",
                   pattern_color = "black",
                   pattern_fill = "black",
                   pattern_density = 0.35,
                   aes(pattern = variable,
                       pattern_angle = variable,
                       pattern_spacing = variable)) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.text.x = element_text(color = "black", angle = 90, hjust = 0.5, vjust = 0.5), 
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", size = 18),
        title = black.bold.text, axis.title = black.bold.text,
        plot.background = element_rect(fill='white'),
        legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_x_discrete(labels=c("bicep", "face", "fecal", "inner cheek", "torso")) +
  labs(title = "February", x = "Sample Type", y = "Source Proportion")
plot_feb

### July

st_july <- read.csv("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/09_sourcetracking/STout_july/mixing_proportions_july_R.csv")
st_july <- ddply(melt(st_july, id.vars = 'sample_type'), .(sample_type))

st_long_prop_july <- st_july %>% group_by(sample_type,variable) %>% summarise(Source_proportion = mean(value))

black.bold.text = element_text(face = "bold", color = "black")
black.text = element_text(color = "black")

plot_july <- ggplot(st_long_prop_july, aes(x = sample_type, y = Source_proportion, fill = variable)) + 
  geom_col_pattern(fill = "white",
                   color = "black",
                   pattern_color = "black",
                   pattern_fill = "black",
                   pattern_density = 0.35,
                   aes(pattern = variable,
                       pattern_angle = variable,
                       pattern_spacing = variable)) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.text.x = element_text(color = "black", angle = 90, hjust = 0.5, vjust = 0.5), 
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", size = 18),
        title = black.bold.text, axis.title = black.bold.text,
        plot.background = element_rect(fill='white')) +
  theme(axis.text.x = element_text(angle = 0)) +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(legend.title = element_blank()) +
  scale_x_discrete(labels=c("bicep", "face", "fecal", "inner cheek", "torso")) +
  labs(title = "July", x = "Sample Type", y = "Source Proportion")
plot_july

### merge plots together
# go back and get rid of legend in all but july graph - only need one

plot_merged <- ggarrange(plot_feb, plot_april, plot_july,
                                      labels = c("A", "B", "C"),
                                      ncol = 3,
                                      widths = c(1.5, 1.5, 2.3))
plot_merged
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/09_sourcetracking/ST_results.tif", units="in", width = 12, height = 4,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/09_sourcetracking/ST_results.jpeg", units="in", width = 12, height = 4,  dpi=300, device="jpeg")  




