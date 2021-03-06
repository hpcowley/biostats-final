###############################################################################
# Biostatistics 651 Final Project
# Create Nursing Home COVID-19 Maps
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
setwd('...replace with path to folder.../Bios_140.651_Final/Data')

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
provider <- read.csv('raw/NH_ProviderInfo_Sep2021.csv')
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
# Censored private API key, so this geocode will not run (but saved geocoded data)
# The saved geocoded data is reloaded before creation of the maps
register_google(key = '')

# Run the geocode function from ggmap package
provider <- mutate_geocode(data = provider, location = full.address, output = "more", source = "google")

# 4. Load covid data and basic cleaning
nh_covid <- read.csv('tmp/nh_covid_md_base.csv')
# Keep relevant variables
nh_covid <- nh_covid[, c('Federal.Provider.Number', 'total.res.cases', 
                         'avg.res.vaccine.rate', 'total.res.cases.p')]
# Make Federal.Provider.Number a character
nh_covid$Federal.Provider.Number <- as.character(nh_covid$Federal.Provider.Number)

# Join provider and covid data
provider <- left_join(provider, nh_covid, by= 'Federal.Provider.Number')

# Export geocoded data
# This row is commented out so geocoded data is not over-writen
# write.csv(provider, 'tmp/nh_covid_md_geocode.csv', row.names = FALSE)

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
# Reload geocoded provider data so the maps are reproducible
provider <- read.csv('tmp/nh_covid_md_geocode.csv')
# Convert total.res.cases.p to be more in line with vaccine rate
provider$total.res.cases.p <- provider$total.res.cases.p * 100
nh_covid_map <- ggplot() +
  geom_polygon(data = choropleth, aes(x = long, y = lat, group = group), fill= "grey80", color="white", size=0.05, alpha=4) +
  geom_point(data = provider, aes(x = lon, y = lat, alpha= 0.3, color = total.res.cases.p), size = 2) +
  theme_void() +
  labs(
    title = "",
    subtitle = ""
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
  guides(alpha = 'none', color = guide_colourbar(title = 'Total COVID-19 Resident Cases / Avg. Number of Occupied Beds (%)', barwidth = 10, barheight = 0.5, direction = "horizontal", nbin = 10)) +
  coord_map() 
nh_covid_map
ggsave('output/nh_covid_cases_map.png', plot = last_plot())

# Map of nursing homes by vaccine rate
nh_covid_map2 <- ggplot() +
  geom_polygon(data = choropleth, aes(x = long, y = lat, group = group), fill= "grey80", color="white", size=0.05, alpha=4) +
  geom_point(data = provider, aes(x = lon, y = lat, alpha= 0.3, color = avg.res.vaccine.rate), size = 2) +
  theme_void() +
  labs(
    title = "",
    subtitle = ""
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
  guides(alpha = 'none', color = guide_colourbar(title = 'Average Resident Vaccination Rate (%)', barwidth = 10, barheight = 0.5, direction = "horizontal", nbin = 10)) +
  coord_map() 
nh_covid_map2
ggsave('output/nh_covid_vaccine_map.png', plot = last_plot())
