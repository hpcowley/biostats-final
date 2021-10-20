###############################################################################
# Biostatistics 651 Final Project
# Create Nursing Home COVID-19 Graphs
# October 2021
# Steps:
# 1. Housekeeping
# 2. Load data and clean
# 3. Make summary graphs
###############################################################################

# 1. Housekeeping
# Remove objects previously stored in R
rm(list = ls())

# Set Paths
setwd('/Users/rcorgel/OneDrive - Johns Hopkins/Bios_140.651_Final/Data')

# Load Libraries
library('tidyverse')
library('assertr')
library('lubridate')

# 2. Load data and clean
nh_covid_md_week <- read.csv('tmp/nh_covid_md_week.csv')
nh_data <- read.csv('tmp/nursing_home_data.csv')

# Drop if pctblack is NA
nh_data_scatter <- nh_data %>% drop_na(pctblack) %>% filter(pctblack != 'LNE')

# Convert pctblack to numeric 
nh_data_scatter$pctblack <- as.integer(nh_data_scatter$pctblack )

# Convert Week.Ending to date
nh_covid_md_week$Week.Ending <- as.Date(nh_covid_md_week$Week.Ending)

# Convert Federal.Provider.Number to character
nh_covid_md_week$Federal.Provider.Number <- as.character(nh_covid_md_week$Federal.Provider.Number)

# Convert total.res.cases.p to be more in line with vaccine rate
nh_data$total.res.cases.p <- nh_data$total.res.cases.p * 100
nh_data_scatter$total.res.cases.p <- nh_data_scatter$total.res.cases.p * 100

# 3. Make summary graphs
# Resident Vaccine Rate Histogram
c1 <- rgb(0, 255, 0, max = 255, alpha = 170, names = "a.green")
h <- hist(nh_data$avg.res.vaccine.rate, breaks = 40)
h$density = h$counts/sum(h$counts)
plot(h, freq=FALSE, main = '', 
     xlab = 'Average Resident Vaccination Rate (%)', col = c1)

# Quick mean and SD calculation
mean(nh_data$avg.res.vaccine.rate, na.rm = TRUE)
sd(nh_data$avg.res.vaccine.rate, na.rm = TRUE)

# Resident COVID-19 Cases / Avg Number of Occupied Beds Histogram
c2 <- rgb(0, 0, 255, max = 255, alpha = 160, names = "a.blue")
h2 <- hist(nh_data$total.res.cases.p, breaks = 40, xlim=c(0, 250))
h2$density = h2$counts/sum(h2$counts)
plot(h2, freq=FALSE, main = '', 
     xlab = 'Total COVID-19 Resident Cases / Avg. Number of Occupied Beds (%)', col = c2)

# Quick mean and SD calculation
mean(nh_data$total.res.cases.p, na.rm = TRUE)
sd(nh_data$total.res.cases.p, na.rm = TRUE)

# Weekly Cases Line Chart
ggplot(nh_covid_md_week,aes(x=Week.Ending, y=Residents.Weekly.Confirmed.COVID.19, group=Federal.Provider.Number)) + 
  geom_line(col = 'red', alpha = 0.2) + theme(legend.position="none", 
  axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(face = 'bold', hjust = 0.5), 
  axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_blank(), panel.background = element_blank()) + 
  scale_x_date(date_breaks = "1 months", date_labels = "%m-%Y") +
  xlab('') + ylab('Residents Weekly Confirmed COVID-19 Cases') + 
  scale_y_continuous(limits = c(0, 100))
ggsave('output/nh_covid_cases_timeseries.png', plot = last_plot())

# Weekly Cases Lasagna Chart
ggplot(nh_covid_md_week, aes(x=Week.Ending, y=Federal.Provider.Number, fill=Residents.Weekly.Confirmed.COVID.19)) + 
  geom_tile(col = 'white', size = 0) + scale_fill_viridis_c(option = "A" , direction = -1) +
  scale_x_date(date_breaks = "4 weeks", date_labels = "%m-%d-%y", limits = as.Date(c('2020-05-24','2021-09-16'))) +
  xlab('') + ylab('Nursing Homes') + 
  labs(title = '') +
  theme(legend.position='bottom', axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
  plot.title = element_text(face = 'bold', hjust = 0.5))  +
  guides(fill = guide_colourbar(title = 'Resident Cases by Week', barwidth = 10, barheight = 0.5, direction = "horizontal", nbin = 10)) 
ggsave('output/nh_covid_cases_lasagna.png', plot = last_plot())

# Resident COVID-19 Cases / Avg Number of Occupied Beds by % Black Scatter Plot
ggplot(nh_data_scatter, aes(x=total.res.cases.p, y=pctblack)) +
  geom_point(col = 'blue', alpha = 0.5) + xlab('Total COVID-19 Resident Cases / Avg. Number of Occupied Beds (%)') + ylab('Percent of Rrsidents that are Black') + 
  labs(title = '') +
  theme(legend.position="none", plot.title = element_text(face = 'bold', hjust = 0.5), 
  panel.background = element_rect(fill = 'white'), axis.line = element_line(size = 0.5, colour = "black"), 
  panel.grid.major.y = element_line(colour = "gray90")) +
  scale_x_continuous(limits = c(0, 150)) +
  scale_y_continuous(limits = c(0, 100))
ggsave('output/nh_covid_pct_black.png', plot = last_plot())
