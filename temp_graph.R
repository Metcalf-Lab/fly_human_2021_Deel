#############################################
# Heather Deel
# September 20th, 2021
# Graphing of temp data for fly 2021 paper
#############################################

### libraries
library(ggplot2)
library(ggpubr)
library(forcats)

### read in data
weather_data <- read.csv("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/02_metadata/fly_temp_data.csv", header = TRUE)

### temp plot
temp_plot <- weather_data %>% 
  mutate(month_year = fct_relevel(month_year, "february_2014",
                                  "april_2014","july_2014",
                                  "february_2015","april_2015")) %>% 
  ggplot(aes(x = month_year, y = avg_temp_c)) +
  geom_bar(stat = "identity", fill = "red3") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, size = 10, vjust = 0.65),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("Average Monthly Temperature (C)") +
  scale_x_discrete(labels = c("Feb 2014","April 2014","July 2014",
                   "Feb 2015","April 2015"))
temp_plot

### dewpoint plot
dew_plot <- weather_data %>% 
  mutate(month_year = fct_relevel(month_year, "february_2014",
                                  "april_2014","july_2014",
                                  "february_2015","april_2015")) %>% 
  ggplot(aes(x = month_year, y = avg_dew_point)) +
  geom_bar(stat = "identity", fill = "springgreen4") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, size = 10, vjust = 0.65),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("Average Monthly Dew Point") +
  scale_x_discrete(labels = c("Feb 2014","April 2014","July 2014",
                              "Feb 2015","April 2015"))
dew_plot

### precipitation plot
precip_plot <- weather_data %>% 
  mutate(month_year = fct_relevel(month_year, "february_2014",
                                  "april_2014","july_2014",
                                  "february_2015","april_2015")) %>% 
  ggplot(aes(x = month_year, y = avg_precipitation_inches)) +
  geom_bar(stat = "identity", fill = "dodgerblue2") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, size = 10, vjust = 0.65),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("Average Monthly Precipitation (inches)") +
  scale_x_discrete(labels = c("Feb 2014","April 2014","July 2014",
                              "Feb 2015","April 2015"))
precip_plot

### merge plots together
weather_plot <- ggarrange(temp_plot, precip_plot, dew_plot,
                                 labels = c("A", "B", "C"),
                                 ncol = 3, nrow = 1,
                                 widths = c(1.5, 1.5, 1.5))
weather_plot

ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/weather_data.tif", units="in", width = 7.5, height = 5,  dpi=300, device="tiff")  
ggsave("/Users/heatherdeel/Dropbox/SHSU_body_farm_projects/Projects/fly/04_figures_results/weather_data.png", units="in", width = 7.5, height = 5,  dpi=300, device="png")

