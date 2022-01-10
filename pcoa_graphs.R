###################################################
# Heather Deel
# July 9th, 2021
# PCoA graphing of fly/human data using qiime2R
###################################################

#######################################
# Libraries
#######################################

# devtools::install_github("jbisanz/qiime2R")
library(qiime2R)
library(tidyverse)
library(ggpubr)

# import metadata and weighted unifrac qza files
metadata_fly <- read_tsv("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/02_metadata/merged_mapping_fly_flybodies_final.txt")
metadata_fly

### fly data weighted

pcoa_fly_weighted <- read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/06_core_metrics/core_metrics_flyonly_5937/weighted_unifrac_pcoa_results.qza")

# just looking at stuff 
pcoa_fly_weighted$uuid
head(pcoa_fly_weighted$data$ProportionExplained)
pcoa_fly_weighted$data$Vectors[1:5, 1:3]

# plot weighted unifrac by organ
pcoa_fly_organ_weighted <- pcoa_fly_weighted$data$Vectors %>%
  rename("#SampleID"=SampleID) %>%
  left_join(metadata_fly) %>%
  ggplot(aes(x=PC1, y=PC2, shape=sample_site_fly)) +
  geom_point() +
  xlab(paste("PC1: ", round(100*pcoa_fly_weighted$data$ProportionExplained[1]), "%")) +
  ylab(paste("PC2: ", round(100*pcoa_fly_weighted$data$ProportionExplained[2]), "%")) +
  theme_classic() +
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=20),
        legend.title =element_text(size=20),
        plot.title=element_text(size=20)) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  scale_shape_manual(values=c(8, 2, 10),
                     name = "Fly Organ") +
  ggtitle("Fly Organs, Weighted UniFrac") +
  guides(color = guide_legend(override.aes = list(size = 5)))
pcoa_fly_organ_weighted

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_weighteduni_fly_organs.tif", units="in", width = 6, height = 5,  dpi=300, device="tiff")  

# plot weighted unifrac by placement season
pcoa_fly_season_weighted <- pcoa_fly_weighted$data$Vectors %>%
  rename("#SampleID"=SampleID) %>%
  left_join(metadata_fly) %>%
  ggplot(aes(x=PC1, y=PC2, shape=placement_season)) +
  geom_point() +
  xlab(paste("PC1: ", round(100*pcoa_fly_weighted$data$ProportionExplained[1]), "%")) +
  ylab(paste("PC2: ", round(100*pcoa_fly_weighted$data$ProportionExplained[2]), "%")) +
  theme_classic() +
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=20),
        legend.title =element_text(size=20),
        plot.title=element_text(size=20)) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  scale_shape_manual(values=c(8, 2, 10),
                     name = "Season") +
  ggtitle("Fly by Season, Weighted UniFrac") +
  guides(color = guide_legend(override.aes = list(size = 5)))
pcoa_fly_season_weighted

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_weighteduni_fly_seasons.tif", units="in", width = 6, height = 5,  dpi=300, device="tiff")  

# make merged graph

fly_season_organ_weighted <- ggarrange(pcoa_fly_organ_weighted, pcoa_fly_season_weighted,
                              labels = c("A", "B"),
                              ncol = 2)
fly_season_organ_weighted
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_weighteduni_fly_seasons_organs.tif", units="in", width = 12, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_weighteduni_fly_seasons_organs.png", units="in", width = 12, height = 5,  dpi=300, device="png")  


### fly data unweighted

pcoa_fly_unweighted <- read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/06_core_metrics/core_metrics_flyonly_5937/unweighted_unifrac_pcoa_results.qza")

# just looking at stuff 
pcoa_fly_unweighted$uuid
head(pcoa_fly_unweighted$data$ProportionExplained)
pcoa_fly_unweighted$data$Vectors[1:5, 1:3]

# plot unweighted unifrac by organ
pcoa_fly_organ_unweighted <- pcoa_fly_unweighted$data$Vectors %>%
  rename("#SampleID"=SampleID) %>%
  left_join(metadata_fly) %>%
  ggplot(aes(x=PC1, y=PC2, color=sample_site_fly)) +
  geom_point() +
  xlab(paste("PC1: ", round(100*pcoa_fly_unweighted$data$ProportionExplained[1]), "%")) +
  ylab(paste("PC2: ", round(100*pcoa_fly_unweighted$data$ProportionExplained[2]), "%")) +
  theme_classic() +
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=20),
        legend.title =element_text(size=20),
        plot.title=element_text(size=20)) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  scale_colour_manual(values=c("darkorchid", "gold", "forestgreen"),
                     name = "Fly Organ") +
  ggtitle("Fly Organs, Unweighted UniFrac") +
  guides(color = guide_legend(override.aes = list(size = 5)))
pcoa_fly_organ_unweighted

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_unweighteduni_fly_organs_color.tif", units="in", width = 6, height = 5,  dpi=300, device="tiff")  

# plot unweighted unifrac by placement season
pcoa_fly_season_unweighted <- pcoa_fly_unweighted$data$Vectors %>%
  rename("#SampleID"=SampleID) %>%
  left_join(metadata_fly) %>%
  ggplot(aes(x=PC1, y=PC2, color=placement_season)) +
  geom_point() +
  xlab(paste("PC1: ", round(100*pcoa_fly_unweighted$data$ProportionExplained[1]), "%")) +
  ylab(paste("PC2: ", round(100*pcoa_fly_unweighted$data$ProportionExplained[2]), "%")) +
  theme_classic() +
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=20),
        legend.title =element_text(size=20),
        plot.title=element_text(size=20)) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  scale_colour_manual(values=c("deepskyblue", "deeppink", "seagreen"),
                     name = "Season") +
  ggtitle("Fly by Season, Unweighted UniFrac") +
  guides(color = guide_legend(override.aes = list(size = 5)))
pcoa_fly_season_unweighted

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_unweighteduni_fly_seasons_color.tif", units="in", width = 6, height = 5,  dpi=300, device="tiff")  

fly_season_organ_unweighted <- ggarrange(pcoa_fly_organ_unweighted, pcoa_fly_season_unweighted,
                              labels = c("A", "B"),
                              ncol = 2)
fly_season_organ_unweighted
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_unweighteduni_fly_seasons_organs_color.tif", units="in", width = 12, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_unweighteduni_fly_seasons_organs_color.jpeg", units="in", width = 12, height = 5,  dpi=300, device="jpeg")  

### fly and human data weighted by sample type

# import metadata and weighted unifrac qza files
metadata <- read_tsv("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/02_metadata/merged_mapping_fly_flybodies_final.txt")
metadata

pcoa_fly_human_weighted <- read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/06_core_metrics/core_metrics_no247_5937/weighted_unifrac_pcoa_results.qza")

# just looking at stuff 
pcoa_fly_human_weighted$uuid
head(pcoa_fly_human_weighted$data$ProportionExplained)
pcoa_fly_human_weighted$data$Vectors[1:5, 1:3]

# plot weighted unifrac by organ
pcoa_fly_human_weighted$data$Vectors %>%
  rename("#SampleID"=SampleID) %>%
  left_join(metadata) %>%
  ggplot(aes(x=PC1, y=PC2, shape=sample_type_combo)) +
  geom_point() +
  xlab(paste("PC1: ", round(100*pcoa_fly_human_weighted$data$ProportionExplained[1]), "%")) +
  ylab(paste("PC2: ", round(100*pcoa_fly_human_weighted$data$ProportionExplained[2]), "%")) +
  theme_classic() +
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=20),
        legend.title =element_text(size=20),
        plot.title=element_text(size=20)) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  scale_shape_manual(values=c(1,4,6,20,11,12),
                     name = "Sample Type",
                     labels = c("bicep","face","fecal","fly","inner cheek","torso")) +
  ggtitle("Flies and Human Sample Types, Weighted UniFrac") +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_weighteduni_fly_human.tif", units="in", width = 8, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_weighteduni_fly_human.png", units="in", width = 8, height = 5,  dpi=300, device="png")

### fly and human data unweighted by sample type

pcoa_fly_human_unweighted <- read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/06_core_metrics/core_metrics_no247_5937/unweighted_unifrac_pcoa_results.qza")

# just looking at stuff 
pcoa_fly_human_unweighted$uuid
head(pcoa_fly_human_unweighted$data$ProportionExplained)
pcoa_fly_human_unweighted$data$Vectors[1:5, 1:3]

# plot weighted unifrac by organ
pcoa_fly_human_unweighted$data$Vectors %>%
  rename("#SampleID"=SampleID) %>%
  left_join(metadata) %>%
  ggplot(aes(x=PC1, y=PC2, shape=sample_type_combo)) +
  geom_point() +
  xlab(paste("PC1: ", round(100*pcoa_fly_human_unweighted$data$ProportionExplained[1]), "%")) +
  ylab(paste("PC2: ", round(100*pcoa_fly_human_unweighted$data$ProportionExplained[2]), "%")) +
  theme_classic() +
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=20),
        legend.title =element_text(size=20),
        plot.title=element_text(size=20)) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  scale_shape_manual(values=c(2,4,6,20,11,12),
                     name = "Sample Type",
                     labels = c("bicep","face","fecal","fly","inner cheek","torso")) +
  ggtitle("Flies and Human Sample Types, Unweighted UniFrac") +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_unweighteduni_fly_human.tif", units="in", width = 8, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_unweighteduni_fly_human.png", units="in", width = 8, height = 5,  dpi=300, device="png")


### unweighted unifrac of just fly vs human

# reimport metadata with a new fly_or_human column
metadata <- read_tsv("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/02_metadata/merged_mapping_fly_flybodies_final.txt")
metadata

pcoa_fly_human_2_unweighted <- read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/06_core_metrics/core_metrics_no247_5937/unweighted_unifrac_pcoa_results.qza")

# just looking at stuff 
pcoa_fly_human_2_unweighted$uuid
head(pcoa_fly_human_2_unweighted$data$ProportionExplained)
pcoa_fly_human_2_unweighted$data$Vectors[1:5, 1:3]

# plot
flyhuman2 <- pcoa_fly_human_2_unweighted$data$Vectors %>%
  rename("#SampleID"=SampleID) %>%
  left_join(metadata) %>%
  ggplot(aes(x=PC1, y=PC2, color=fly_or_human)) +
  geom_point() +
  xlab(paste("PC1: ", round(100*pcoa_fly_human_2_unweighted$data$ProportionExplained[1]), "%")) +
  ylab(paste("PC2: ", round(100*pcoa_fly_human_2_unweighted$data$ProportionExplained[2]), "%")) +
  theme_classic() +
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=20),
        legend.title =element_text(size=20),
        plot.title=element_text(size=20)) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  scale_color_manual(values=c("grey25","darkgoldenrod1"),
                     name = "Sample Type",
                     labels = c("fly","human")) +
  ggtitle("Flies vs. Human Samples, Unweighted") +
  guides(color = guide_legend(override.aes = list(size = 5)))
flyhuman2

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_unweighteduni_fly_human2_color.tif", units="in", width = 8, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_unweighteduni_fly_human2_color.png", units="in", width = 8, height = 5,  dpi=300, device="png")

### weighted unifrac of just fly vs human

pcoa_fly_human_2_weighted <- read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/06_core_metrics/core_metrics_no247_5937/weighted_unifrac_pcoa_results.qza")

# just looking at stuff 
pcoa_fly_human_2_weighted$uuid
head(pcoa_fly_human_2_weighted$data$ProportionExplained)
pcoa_fly_human_2_weighted$data$Vectors[1:5, 1:3]

# plot
flyhuman2_we <- pcoa_fly_human_2_weighted$data$Vectors %>%
  rename("#SampleID"=SampleID) %>%
  left_join(metadata) %>%
  ggplot(aes(x=PC1, y=PC2, shape=fly_or_human)) +
  geom_point() +
  xlab(paste("PC1: ", round(100*pcoa_fly_human_2_weighted$data$ProportionExplained[1]), "%")) +
  ylab(paste("PC2: ", round(100*pcoa_fly_human_2_weighted$data$ProportionExplained[2]), "%")) +
  theme_classic() +
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=20),
        legend.title =element_text(size=20),
        plot.title=element_text(size=20)) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  scale_shape_manual(values=c(1,19),
                     name = "Sample Type",
                     labels = c("fly","human")) +
  ggtitle("Flies vs. Human Samples, Weighted") +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_weighteduni_fly_human2.tif", units="in", width = 8, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_weighteduni_fly_human2.png", units="in", width = 8, height = 5,  dpi=300, device="png")

### unweighted unifrac of just human by sample type

pcoa_human_unweighted <- read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/06_core_metrics/core_metrics_humanonly_no247_5937/unweighted_unifrac_pcoa_results.qza")

# just looking at stuff 
pcoa_human_unweighted$uuid
head(pcoa_human_unweighted$data$ProportionExplained)
pcoa_human_unweighted$data$Vectors[1:5, 1:3]

# plot
humanonly <- pcoa_human_unweighted$data$Vectors %>%
  rename("#SampleID"=SampleID) %>%
  left_join(metadata) %>%
  ggplot(aes(x=PC1, y=PC2, color=sample_type_combo)) +
  geom_point() +
  xlab(paste("PC1: ", round(100*pcoa_human_unweighted$data$ProportionExplained[1]), "%")) +
  ylab(paste("PC2: ", round(100*pcoa_human_unweighted$data$ProportionExplained[2]), "%")) +
  theme_classic() +
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=20),
        legend.title =element_text(size=20),
        plot.title=element_text(size=20)) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  scale_color_manual(values=c("red1","yellow","limegreen","blue3","darkorchid1"),
                     name = "Sample Type",
                     labels = c("bicep","face","fecal","inner cheek","torso")) +
  ggtitle("Human Sample Types, Unweighted") +
  guides(color = guide_legend(override.aes = list(size = 5)))
humanonly

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_unweighteduni_humanonly_color.tif", units="in", width = 8, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_unweighteduni_humanonly_color.png", units="in", width = 8, height = 5,  dpi=300, device="png")

### weighted unifrac of just human by sample type

pcoa_human_weighted <- read_qza("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/06_core_metrics/core_metrics_humanonly_no247_5937/weighted_unifrac_pcoa_results.qza")

# just looking at stuff 
pcoa_human_weighted$uuid
head(pcoa_human_weighted$data$ProportionExplained)
pcoa_human_weighted$data$Vectors[1:5, 1:3]

# plot
humanonly_we <- pcoa_human_weighted$data$Vectors %>%
  rename("#SampleID"=SampleID) %>%
  left_join(metadata) %>%
  ggplot(aes(x=PC1, y=PC2, shape=sample_type_combo)) +
  geom_point() +
  xlab(paste("PC1: ", round(100*pcoa_human_weighted$data$ProportionExplained[1]), "%")) +
  ylab(paste("PC2: ", round(100*pcoa_human_weighted$data$ProportionExplained[2]), "%")) +
  theme_classic() +
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=20),
        legend.title =element_text(size=20),
        plot.title=element_text(size=20)) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  scale_shape_manual(values=c(1,4,6,19,11),
                     name = "Sample Type",
                     labels = c("bicep","face","fecal","inner cheek","torso")) +
  ggtitle("Human Sample Types, Weighted") +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_weighteduni_humanonly.tif", units="in", width = 8, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_weighteduni_humanonly.png", units="in", width = 8, height = 5,  dpi=300, device="png")

### make merged figure of unweighted fly vs human and unweighted human only by sample type

fly_human_unweighted <- ggarrange(flyhuman2, humanonly,
                                         labels = c("A", "B"),
                                         ncol = 2)
fly_human_unweighted

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_unweighteduni_flyhuman2_humanonly_merged_color.tif", units="in", width = 12, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_unweighteduni_flyhuman2_humanonly_merged_color.png", units="in", width = 12, height = 5,  dpi=300, device="png")

### make merged figure of weighted fly vs human and weighted human only by sample type

fly_human_weighted <- ggarrange(flyhuman2_we, humanonly_we,
                                  labels = c("A", "B"),
                                  ncol = 2)
fly_human_weighted

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_weighteduni_flyhuman2_humanonly_merged.tif", units="in", width = 12, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/pcoa_weighteduni_flyhuman2_humanonly_merged.png", units="in", width = 12, height = 5,  dpi=300, device="png")



