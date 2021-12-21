###############################################################################
# Biostatistics 652 Final Project
# Merge together nursing home data and MD COVID-19 data
# December 2021
# Steps:
# 1. Housekeeping
# 2. Load all data to be merged
# 3. Merge data
# 4. Clean variable names
# 5. Export data
###############################################################################

# 1. Housekeeping
# Remove objects previously stored in R
rm(list = ls())

# Set Paths
setwd('/Users/rcorgel/OneDrive - Johns Hopkins/Bios_140.651_Final/Data')

# Load Libraries
library(tidyverse)
library(readxl)
library(janitor)

# 2. Load all data to be merged and get rid of some variables
#read all dfs in
# Provider File and Citations
load('tmp/provider_MD_formerge.RData')
provider_MD_formerge <- provider_MD %>% 
  dplyr::select(-c(Provider.Phone.Number,
            Automatic.Sprinkler.Systems.in.All.Required.Areas,
            Processing.Date))
# Maryland COVID-19 Data
load('tmp/newcases_rates_formerge.RData')
newcases_rates_formerge <- newcases_rates %>%
  dplyr::select(-c(county))
# Nursing Home COVID-19 Data
load('tmp/nh_covid_md_base.RData')
nh_covid_md_base <- nh_covid_md_base %>%
  dplyr::select(-c(Provider.Name,	
            Provider.Address,	
            Provider.City,	
            Provider.State,	
            Provider.Zip.Code,	
            County))
# Nursing Home Demographics Data
load('tmp/demographics_poverty_county_join_hpc.RData')
demographics_poverty_county_join_hpc <- final_data %>%
  dplyr::select(-c(FIPS_nostate,
            county,
            zipcode))

#change federal.provider.number to provider.id for nh_covid_base
nh_covid_md_base <- nh_covid_md_base %>% 
  dplyr::rename(provider.id = Federal.Provider.Number)

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
  dplyr::rename(SSACD = Provider.SSA.County.Code)
#change from character to numeric
nursing_home_data$SSACD <- as.numeric(nursing_home_data$SSACD)

#merge provider and county data
nursing_home_data <-left_join(nursing_home_data, newcases_rates_formerge, by = c('SSACD'))

# Merge ACS Data
# Re Merge this data
nursing_home_data$county_poverty_pct <- NULL
load('tmp/acs_subset.RData')
acs_subset$SSACD <- as.numeric(acs_subset$SSACD)
nursing_home_data <-left_join(nursing_home_data, acs_subset, by = c('SSACD'))
# St. Mary's county was excluded for some reason, adding it back in
nursing_home_data$pct_poverty <- ifelse(is.na(nursing_home_data$pct_poverty), 8.400000, nursing_home_data$pct_poverty)
# Rename
nursing_home_data$county_poverty_pct <- nursing_home_data$pct_poverty
nursing_home_data$pct_poverty <- NULL

# 4. Clean variable names
nursing_home_data <- janitor::clean_names(nursing_home_data, case = 'snake')

# 5. Export data
write.csv(nursing_home_data, "tmp/nursing_home_data_clean.csv", row.names = FALSE)
save(nursing_home_data, file = "tmp/nursing_home_data_clean.RData")
