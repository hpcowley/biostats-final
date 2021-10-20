###############################################################################
# Biostatistics 1 Final Project
# Create Nursing Home Map
# October 2021
# Steps:
# 1. Housekeeping
# 2. Load provider data and basic cleaning
# 3. Geocoding
# 4. Load covid data and basic cleaning
# 5. Load the shape file and basic cleaning
# 6. Create Maps
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
library('ggmap')
library('sf')
library('raster')
library('spData')
library('tmap')   
library('rgdal')
library('broom')
library('readxl')

# 2. Load data and basic cleaning
# Load Provider File
provider <- read.csv(paste(data_path, 'NH_ProviderInfo_Sep2021.csv', sep=""))
provider$Federal.Provider.Number <- as.character(provider$Federal.Provider.Number)
# Make full address variable
provider$full.address <-paste0(as.character(provider$Provider.Address),", ", 
                               as.character(provider$Provider.City), ", ", 
                               as.character(provider$Provider.State))
# Keep relevant variables
provider <- provider[, c('Federal.Provider.Number', 'Provider.Name', 
                         'Provider.Address', 'Provider.City', 'Provider.State',
                         'Provider.Zip.Code', 'Provider.County.Name', 'full.address')]
# Subset to only Maryland nursing homes
provider <- provider[which(provider$Provider.State=='MD'), ]

# 3. Geocoding
# Set API Key
register_google(key = 'AIzaSyDtXAJKxd8dbPXuqNkyz-ypVHRElovqGCA')

# Run the geocode function from ggmap package
provider <- mutate_geocode(data = provider, location = full.address, output = "more", source = "google")

# 4. Load covid data and basic cleaning
nh_covid <- read.csv(paste(tmp_path, 'nh_covid_md_base.csv', sep=""))
# Keep relevant variables
nh_covid <- nh_covid[, c('Federal.Provider.Number', 'total.res.cases', 
                         'avg.res.vaccine.rate', 'total.res.cases.p')]
# Make Federal.Provider.Number a character
nh_covid$Federal.Provider.Number <- as.character(nh_covid$Federal.Provider.Number)

# Join provider and covid data
provider <- left_join(provider, nh_covid, by= 'Federal.Provider.Number')

# 5. Load the shape file and basic cleaning
spdf <- readOGR( 
  dsn= 'raw/cb_2018_us_county_500k/' , 
  layer= 'cb_2018_us_county_500k',
  verbose=FALSE
)

# Tidy up shape file
choropleth <- tidy(spdf, region = c('GEOID'))

# Extract state and county fips
choropleth$state.fips <- substr(choropleth$id, 1, 2)
choropleth$county.fips <- substr(choropleth$id, 3, 5)

# Subset to Maryland
choropleth <- choropleth[which(choropleth$state.fips=='24'), ]

# 6. Create Maps
# Map of nursing homes by % of cases to bed
nh_covid_map <- ggplot() +
  geom_polygon(data = choropleth, aes(x = long, y = lat, group = group), fill= "grey80", color="white", size=0.05, alpha=4) +
  geom_point(data = provider, aes(x = lon, y = lat, alpha= 0.3, color = total.res.cases.p), size = 2) +
  theme_void() +
  labs(
    title = "Nursing Homes by Percent of Total Resident Cases to Avg. Beds Occupied",
    subtitle = "May 2020 - September 2021"
  ) +
  scale_colour_viridis_c() +
  theme(
    legend.position="bottom",
    text = element_text(color = "#000000"),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.margin = margin(r = 1),
    plot.title = element_text(size= 14, color = "#000000", margin = margin(b = 0, t = 1, r = 5, unit = "mm"), face = 'bold', hjust = 0.5),
    plot.subtitle = element_text(size= 12,  color = "#000000", margin = margin(b = 0, t = 1, r = 5, unit = "mm"), face = 'bold', hjust = 0.5),
  ) +
  guides(alpha = 'none', color = guide_colourbar(title = '% of Cases to Beds', barwidth = 10, barheight = 0.5, direction = "horizontal", nbin = 10)) +
  coord_map() 
nh_covid_map
ggsave('output/nh_covid_cases_map.pdf', plot = last_plot())

# Map of nursing homes by vaccine rate
nh_covid_map2 <- ggplot() +
  geom_polygon(data = choropleth, aes(x = long, y = lat, group = group), fill= "grey80", color="white", size=0.05, alpha=4) +
  geom_point(data = provider, aes(x = lon, y = lat, alpha= 0.3, color = avg.res.vaccine.rate), size = 2) +
  theme_void() +
  labs(
    title = "Nursing Homes by Average Resident Vaccine Rate",
    subtitle = "May 2021 - September 2021"
  ) +
  scale_colour_viridis_c(option = "plasma") +
  theme(
    legend.position="bottom",
    text = element_text(color = "#000000"),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.margin = margin(r = 1),
    plot.title = element_text(size= 14, color = "#000000", margin = margin(b = 0, t = 1, r = 5, unit = "mm"), face = 'bold', hjust = 0.5),
    plot.subtitle = element_text(size= 12,  color = "#000000", margin = margin(b = 0, t = 1, r = 5, unit = "mm"), face = 'bold', hjust = 0.5),
  ) +
  guides(alpha = 'none', color = guide_colourbar(title = 'Resident Vaccine Rate (%)', barwidth = 10, barheight = 0.5, direction = "horizontal", nbin = 10)) +
  coord_map() 
nh_covid_map2
ggsave('output/nh_covid_vaccine_map.pdf', plot = last_plot())
