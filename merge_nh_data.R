###############################################################################
# Biostatistics 651 Final Project
# Merge together nursing home data and MD COVID-19 data
# October 2021
# Steps:
# 1. Housekeeping
# 2. Load all data to be merged
# 3. Merge data
# 4. Export data
###############################################################################

# 1. Housekeeping
# Remove objects previously stored in R
rm(list = ls())

# Set Paths
setwd('/Users/rcorgel/OneDrive - Johns Hopkins/Bios_140.651_Final/Data')

# Load Libraries
library(tidyverse)
library(readxl)

# 2. Load all data to be merged
#read all dfs in
provider_MD_formerge <- read.csv('tmp/provider_MD_formerge.csv')
newcases_rates_formerge <- read.csv('tmp/newcases_rates_formerge.csv')
nh_covid_md_base <- read.csv('tmp/nh_covid_md_base.csv')
demographics_poverty_county_join_hpc <- read.csv('tmp/hannah/demographics_poverty_county_join_hpc.csv')

#change federal.provider.number to provider.id for nh_covid_base
nh_covid_md_base <- nh_covid_md_base %>% 
  rename(provider.id = Federal.Provider.Number)

# 3. Merge data
#merge the provider, demographic data
nursing_home_data <-left_join(provider_MD_formerge, demographics_poverty_county_join_hpc, by = c('provider.id'))

#merge nursing home with covid data
nursing_home_data <-left_join(nursing_home_data, nh_covid_md_base, by = c('provider.id'))

#add leading zeros
nursing_home_data$Provider.SSA.County.Code <- sprintf("%03d", nursing_home_data$Provider.SSA.County.Code)
#add state fips code
nursing_home_data$Provider.SSA.County.Code <- paste("21", nursing_home_data$Provider.SSA.County.Code, sep="")

#rename column to SSACD
nursing_home_data <- nursing_home_data %>% 
  rename(SSACD = Provider.SSA.County.Code)
#change from character to numeric
nursing_home_data$SSACD <- as.numeric(nursing_home_data$SSACD)

#merge provider and county data
nursing_home_data <-left_join(nursing_home_data, newcases_rates_formerge, by = c('SSACD'))

# 4. Export data
write.csv(nursing_home_data, "tmp/nursing_home_data.csv", row.names = FALSE)
