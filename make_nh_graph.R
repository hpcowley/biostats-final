###############################################################################
# Biostatistics 1 Final Project
# Create Nursing Home Map
# October 2021
# Steps:
# 1. Housekeeping
# 2. Load data and clean
# 3. Make graph 
###############################################################################

# 1. Housekeeping
# Remove objects previously stored in R
rm(list = ls())

# Set Paths
setwd('/Users/rcorgel/OneDrive - Johns Hopkins/Bios_140.651_Final/Data')
data_path <- 'raw/'
tmp_path <- 'tmp/'
output_path <- 'output/'

# Load Libraries
library('tidyverse')
library('assertr')
library('lubridate')

# 2. Load data and clean
nh_data <- read.csv(paste(tmp_path, 'nursing_home_data.csv', sep=""))

# Drop if pctblack is NA
nh_data <- nh_data %>% drop_na(pctblack) %>% filter(pctblack != 'LNE')

# Convert pctblack to numeric 
nh_data$pctblack <- as.integer(nh_data$pctblack )

# 3. Make graph 
ggplot(nh_data, aes(x=total.res.cases.p, y=pctblack)) +
  geom_point(col = 'blue', alpha = 0.5) + xlab('Total COVID-19 Resident Cases / Avg. Number of Occupied Beds') + ylab('Percent of Rrsidents that are Black') + 
  labs(title = 'Pct of Residents with Confirmed Cases by Pct of Nursing Home that is Black\nMaryland, May 2020 - Sept 2021') +
  theme(legend.position="none", plot.title = element_text(face = 'bold', hjust = 0.5), 
  panel.background = element_rect(fill = 'white'), axis.line = element_line(size = 0.5, colour = "black"), 
  panel.grid.major.y = element_line(colour = "gray90")) +
  scale_x_continuous(limits = c(0, 1.5)) +
  scale_y_continuous(limits = c(0, 100))
ggsave('output/nh_covid_pct_black.pdf', plot = last_plot())
