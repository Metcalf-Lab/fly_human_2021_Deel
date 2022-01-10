#################################################################
# Heather Deel
# August 11th, 2021
# Presence/Absence chart of fly species occurrences for fly data
#################################################################

### libraries
library(reshape)
library(ggplot2)
library(ggpubr)

### plot by month

dat_fly_month <- data.frame(flies = c("_Calliphora livida_","Calliphora vicina",
                                "Cochliomya macellaria","Cynomya cadaverina",
                                "Lucilia coeruleiviridis","Lucilia cuprina",
                                "Lucilia eximia","Lucilia mexicana","Lucilia sericata",
                                "Phormia regina","Unidentified","Hydrotaea",
                                "Musca domestica","Polietes griseocaerulea",
                                "Stomoxys calcitrans","Sarcophaga haemorrhoidalis"),
                      February = c(1,1,0,1,1,0,1,0,0,1,1,0,0,1,1,1),
                      April = c(0,0,1,0,1,1,1,1,0,1,0,1,1,0,0,1),
                      July = c(0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0))

melt_data_fly_month <- melt(dat_fly_month, id.vars="flies",variable_name="month")

fly_plot_month <- qplot(data = melt_data_fly_month,
      x=month, y=flies, fill = factor(value), geom='tile') +
  scale_fill_manual(values=c("0"="white","1"="black")) +
  ylab("Fly Species") +
  xlab("Month") +
  theme(legend.position = "none") +
        axis.text.y = element_text(face = "italic")
fly_plot_month

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/fly_occurrences_month.tif", units="in", width = 3.5, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/fly_occurrences_month.png", units="in", width = 3.5, height = 5,  dpi=300, device="png")


### plot by year

dat_fly_year <- data.frame(flies = c("Calliphora livida","Calliphora vicina",
                                      "Cochliomya macellaria","Cynomya cadaverina",
                                      "Lucilia coeruleiviridis","Lucilia cuprina",
                                      "Lucilia eximia","Lucilia mexicana","Lucilia sericata",
                                      "Phormia regina","Unidentified","Hydrotaea",
                                      "Musca domestica","Polietes griseocaerulea",
                                      "Stomoxys calcitrans","Sarcophaga haemorrhoidalis"),
                            Y2014 = c(1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1),
                            Y2015 = c(0,1,1,0,1,0,0,1,0,1,0,0,1,1,0,1))

melt_data_fly_year <- melt(dat_fly_year, id.vars="flies",variable_name="year")

fly_plot_year <- qplot(data = melt_data_fly_year,
      x=year, y=flies, fill = factor(value), geom='tile') +
  scale_fill_manual(values=c("0"="white","1"="black")) +
  scale_x_discrete(labels = c("2014","2015")) +
  ylab("Fly Species") +
  xlab("Year") +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())
fly_plot_year

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/fly_occurrences_year.tif", units="in", width = 3.5, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/fly_occurrences_year.png", units="in", width = 3.5, height = 5,  dpi=300, device="png")

### merge the two plots

fly_occurence_combo <- ggarrange(fly_plot_month, fly_plot_year,
                                         labels = c("A", "B"),
                                         ncol = 2,
                                         widths = c(2.3, 1.5))
fly_occurence_combo

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/fly_occurrences_merged.tif", units="in", width = 7.5, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/fly_occurrences_merged.png", units="in", width = 7.5, height = 5,  dpi=300, device="png")



