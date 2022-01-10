############################################
# Heather Deel
# August 16th, 2021
# Bubble charts for relative abundances 
# Used to substitute taxa plots
# Using this tutorial: https://jkzorz.github.io/2019/06/05/Bubble-plots.html
############################################

### libraries
library(ggplot2)
library(reshape2)

### bubble chart of fly organs
### read in feature table
# has relative abundance info already of top 50 features
# went from exported biom --> tsv --> csv --> relative abundance + metadata columns

fly_organ_features = read.csv("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/05_feature_tables/merged_table_nochlomito_filtered_flyonly_min5937_top50features_relabund.csv", header = TRUE)

#convert data frame from a "wide" format to a "long" format
fly_organ_features_melt = melt(fly_organ_features, id = c("Sample", "fly_organ","placement_season"))

# keep sample order the same as in the excel sheet
# make sure excel sheet is sorted by organ for this
fly_organ_features_melt$Sample <- factor(fly_organ_features_melt$Sample,levels=unique(fly_organ_features_melt$Sample))

# reorder the months so they're in chronological order
fly_organ_features_melt$placement_season_chron = factor(fly_organ_features_melt$placement_season,
                                                levels = c("February","April","July"))


# plot
fly_organs_plot = ggplot(fly_organ_features_melt, aes(x = Sample, y = variable)) + 
  geom_point(aes(size = value, fill = fly_organ), alpha = 0.75, shape = 21) + 
  facet_wrap(~placement_season_chron) +
  scale_size_continuous(limits = c(0.000001, 100), range = c(1,10), breaks = c(1,10,50,75)) + 
  labs( x= "", y = "", size = "Relative Abundance (%)", fill = "Fly Organ")  + 
  theme(legend.key=element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(colour = "black", face = "bold", size = 9), 
        legend.text = element_text(size = 10, face ="bold", colour ="black"), 
        legend.title = element_text(size = 11, face = "bold"), panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
        legend.position = "right", panel.grid.major.y = element_line(colour = "grey95")) +  
  scale_fill_manual(values = c("darkorange", "deepskyblue","forestgreen"), guide = guide_legend(override.aes = list(size=5))) +
  scale_y_discrete(limits = rev(levels(fly_organ_features_melt$variable))) 
fly_organs_plot

setwd("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results")
ggsave("fly_organs_bubble.tif", units="in", width = 9, height = 7,  dpi=500, device="tiff")  
ggsave("fly_organs_bubble.png", units="in", width = 9, height = 7,  dpi=500, device="png")  

# use this list in case I decide I want to edit the y-axis names
list = c("Vagococcus","Vagococcus","Acinetobacter","Tumebacillus","Pseudomonas","Providencia",
"Pseudomonas","Vagococcus","Chishuiella wautersiella","Acinetobacter","Psychrobacter pulmonis",
"Ignatzschineria","Ignatzschineria","Providencia stuartii","Dysgonomonas","Acinetobacter",
"Oblitimonas_alkaliphila","Corynebacterium","Ignatzschineria","Zymobacter palmae","Wolbachia",
"Psychrobacter","Enterococcus","Dysgonomonas","Spiroplasma","Wohlfahrtiimonas chitiniclastica",
"Bacillus","Psychrobacter immobilis","Dysgonomonas","Dysgonomonas","Planococcaceae",
"Erwiniaceae","Enterobacterales","Kurthia","Corynebacterium urealyticum","Savagea",
"Ignatzschineria","Acinetobacter","Dysgonomonas","Psychrobacter","Heliconius melpomene",
"Serratia","Staphylococcus","Weeksellaceae","Enterobacterales","Acinetobacter",
"Suttonella","Savagea","Koukoulia aurantiaca","Leuconostoc mesenteroides")

### bubble chart of human only no 247 data

human_features = read.csv("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/01_qiime2_analysis/05_feature_tables/merged_table_nochlomito_filtered_humanonly_no247_min5937_top50_relabund.csv", header = TRUE)

#convert data frame from a "wide" format to a "long" format
human_features_melt = melt(human_features, id = c("Sample", "sample_type","placement_season"))

# keep sample order the same as in the excel sheet
# make sure excel sheet is sorted by sample type for this

human_features_melt$Sample <- factor(human_features_melt$Sample,levels=unique(human_features_melt$Sample))

# reorder the months so they're in chronological order
human_features_melt$placement_season_chron = factor(human_features_melt$placement_season,
                                                        levels = c("February","April","July"))


# plot
human_plot = ggplot(human_features_melt, aes(x = Sample, y = variable)) + 
  geom_point(aes(size = value, fill = sample_type), alpha = 0.75, shape = 21) + 
  facet_wrap(~placement_season_chron) +
  scale_size_continuous(limits = c(0.000001, 100), range = c(1,10), breaks = c(1,10,50,75)) + 
  labs( x= "", y = "", size = "Relative Abundance (%)", fill = "Sample Type")  + 
  theme(legend.key=element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(colour = "black", face = "bold", size = 9), 
        legend.text = element_text(size = 10, face ="bold", colour ="black"), 
        legend.title = element_text(size = 11, face = "bold"), panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
        legend.position = "right", panel.grid.major.y = element_line(colour = "grey95")) +  
  scale_fill_manual(values = c("red", "blue","gold","darkgreen","cadetblue1"), guide = guide_legend(override.aes = list(size=5))) +
  scale_y_discrete(limits = rev(levels(human_features_melt$variable))) 
human_plot

setwd("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results")
ggsave("human_bubble.tif", units="in", width = 9, height = 7,  dpi=500, device="tiff")  
ggsave("human_bubble.png", units="in", width = 9, height = 7,  dpi=500, device="png")



