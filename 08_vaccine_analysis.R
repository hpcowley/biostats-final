###############################################################################
# Biostatistics 652 Final Project
# Examine Booster Data
# Analyze Association of Vaccination & COVID-19 in Nursing Homes
# December 2021
# Steps:
# 1. Housekeeping
# 2. Load Data
# 3. Bar graphs over time
# 4. Vaccination Hist for 2nd and 3rd doses
# 5. Make Maps 
# 6. Chi Squared Testing
# 7. Difference in High/Low Booster Rate NHs
###############################################################################

# 1. Housekeeping
# Remove objects previously stored in R
rm(list = ls())

# Set Paths
setwd('...replace with path to folder.../Bios_140.651_Final/Data')

# Load Libraries
library('tidyverse')
library('ggplot2')
library('reshape2')
library('gridExtra')
library('gtools')
library('ggmap')
library('sf')
library('raster')
library('spData')
library('tmap')   
library('rgdal')
library('broom')
library('readxl')

# 2. Load Data
load('tmp/nursing_home_data_clean.RData')
load('tmp/nh_covid_md_month_all.RData')

# Get rid of May because it has March to May data incorperated
nh_covid_md_month_all <- nh_covid_md_month_all[-c(1), ]

# 3. Bar graphs over time
cases <- ggplot(nh_covid_md_month_all, aes(x=year.month, y= total.res.cases.month)) + 
  geom_bar(stat = "identity", color = '#440154FF', fill = '#440154FF', alpha = 0.5) + labs(title = 'Cases', x = 'Months', y = 'Number of Cases') +
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
cases

deaths <- ggplot(nh_covid_md_month_all, aes(x=year.month, y= total.res.deaths.month)) + 
  geom_bar(stat = "identity", color = '#31688EFF', fill = '#31688EFF', alpha = 0.5) + labs(title = 'Deaths', x = 'Months', y = 'Number of Deaths') +
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
deaths

nh_covid_md_month_all$deaths_per_cases <- nh_covid_md_month_all$total.res.deaths.month / nh_covid_md_month_all$total.res.cases.month
ratio <- ggplot(nh_covid_md_month_all, aes(x=year.month, y= deaths_per_cases)) + 
  geom_bar(stat = "identity", color = '#35B779FF', fill = '#35B779FF', alpha = 0.5) + labs(title = 'Deaths / Cases', x = 'Months', y = 'Ratio of Deaths / Cases') +
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
ratio

grid.arrange(cases, deaths, ratio, ncol = 3)
g <- grid.arrange(cases, deaths, ratio, ncol = 3)
#ggsave('visuals/bar_charts_story.png', g)

# 4. Vaccination Hist for 2nd and 3rd doses
two <- ggplot(nursing_home_data, aes(x=avg_res_vaccine_rate)) + 
  geom_histogram(color="#31688EFF", fill="#31688EFF", bins = 9, alpha = 0.5) + labs(title = '2nd Dose Resident Vaccination Rate', y = 'Count') + 
  xlab("Vaccination Rate") + coord_cartesian(xlim = c(0, 100), ylim = c(0, 60)) + theme_light() + theme(plot.title = element_text(hjust = 0.5))
three <- ggplot(nursing_home_data, aes(x=res_boosted)) + 
  geom_histogram(color="#FDE725FF", fill="#FDE725FF", bins = 19, alpha = 0.5) + labs(title = '3rd Dose Resident Vaccination Rate', y = 'Count') + 
  xlab("Vaccination Rate") + coord_cartesian(xlim = c(0, 100), ylim = c(0, 60)) + theme_light() + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(two, three, ncol = 2)
g <- grid.arrange(two, three, ncol = 2)
#ggsave('visuals/histogram_vax.png', g)

# 5. Make Maps (commented out because geocoding does not beed to be run each time)
# Make full address variable
#nursing_home_data$full.address <-paste0(as.character(nursing_home_data$provider_address),", ", 
                                        #as.character(nursing_home_data$provider_city), ", ", 
                                        #as.character(nursing_home_data$provider_state))
# Keep relevant variables
#location <- nursing_home_data[, c('provider_id', 'provider_name', 
                                  #'provider_address', 'provider_city', 'provider_state',
                                  #'provider_zip_code', 'provider_county_name', 'full.address')]

# Geocoding
# Set API Key
# Censored private API key, so this geocode will not run (but saved geocoded data)
# The saved geocoded data is reloaded before creation of the maps
#register_google(key = '')

# Run the geocode function from ggmap package
#geocode <- mutate_geocode(data = location, location = full.address, output = "more", source = "google")

# Save
# save(geocode, file = "tmp/geocode.RData")

# Load the shape file and basic cleaning
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

# Create Maps
# Map of nursing homes by % of cases to bed
# Reload geocoded provider data so the maps are reproducible
load('tmp/geocode.RData')
# Remove outdated variables
geocode <- geocode %>% dplyr::select(c(provider_id, lon, lat))

# Merge to NHG data
nursing_home_data <- left_join(nursing_home_data, geocode, by = c("provider_id"))

# Create Maps
two_map <- ggplot() +
  geom_polygon(data = choropleth, aes(x = long, y = lat, group = group), fill= "grey80", color="white", size=0.05, alpha=4) +
  geom_point(data = nursing_home_data, aes(x = lon, y = lat, alpha= 0.3, color = avg_res_vaccine_rate), size = 1.5) +
  theme_void() +
  labs(
    subtitle = "2nd Dose Resident Vaccination Rate"
  ) +
  scale_colour_viridis_c(limits = c(0,100)) +
  theme(
    legend.position="bottom",
    text = element_text(color = "#000000"),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.margin = margin(r = 1),
    plot.subtitle = element_text(hjust = 0.5),
  ) +
  guides(alpha = 'none', color = guide_colourbar(title = 'Vaccination Rate', barwidth = 10, barheight = 0.5, direction = "horizontal", nbin = 10)) +
  coord_map() 
two_map

three_map <- ggplot() +
  geom_polygon(data = choropleth, aes(x = long, y = lat, group = group), fill= "grey80", color="white", size=0.05, alpha=4) +
  geom_point(data = nursing_home_data, aes(x = lon, y = lat, alpha= 0.3, color = res_boosted), size = 1.5) +
  theme_void() +
  labs(
    subtitle = "3rd Dose Resident Vaccination Rate"
  ) +
  scale_colour_viridis_c() +
  theme(
    legend.position="bottom",
    text = element_text(color = "#000000"),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.margin = margin(r = 1),
    plot.subtitle = element_text(hjust = 0.5),
  ) +
  guides(alpha = 'none', color = guide_colourbar(title = 'Vaccination Rate', barwidth = 10, barheight = 0.5, direction = "horizontal", nbin = 10)) +
  coord_map() 
three_map
grid.arrange(two_map, three_map, ncol = 2)
g <- grid.arrange(two_map, three_map, ncol = 2)
#ggsave('visuals/map_vax.png', g)

# Number Missing
sum(is.na(nursing_home_data$avg_res_vaccine_rate))
sum(is.na(nursing_home_data$res_boosted))

# 6. Chi Squared Testing
# Various combinations of vaccination and case/death totals
nursing_home_data$res_cases_very_recent_binary <- ifelse(nursing_home_data$total_res_cases_very_recent > 0, 1, 0)
nursing_home_data$res_deaths_very_recent_binary <- ifelse(nursing_home_data$total_res_deaths_very_recent > 0, 1, 0)
nursing_home_data$boosted_med <- ifelse(nursing_home_data$res_boosted > median(nursing_home_data$res_boosted, na.rm = TRUE), 1, 0)
nursing_home_data$boosted_group <- quantcut(nursing_home_data$res_boosted, 4)

nursing_home_data$res_cases_recent_binary <- ifelse(nursing_home_data$total_res_cases_recent > 0, 1, 0)
nursing_home_data$res_deaths_recent_binary <- ifelse(nursing_home_data$total_res_deaths_recent > 0, 1, 0)
nursing_home_data$vax_med <- ifelse(nursing_home_data$avg_res_vaccine_rate > median(nursing_home_data$avg_res_vaccine_rate, na.rm = TRUE), 1, 0)
nursing_home_data$vax_group <- quantcut(nursing_home_data$avg_res_vaccine_rate, 4)

nursing_home_data$count_test <- 1

chi_table <- nursing_home_data %>% group_by(boosted_med, res_cases_very_recent_binary) %>% summarize(var1 = sum(count_test))
chi_table <- chi_table[complete.cases(chi_table), ]
chi_table <- spread(chi_table, boosted_med, var1)
chisq.test(chi_table[,-(1)], correct = FALSE)

chi_table_2 <- nursing_home_data %>% group_by(boosted_med, res_deaths_very_recent_binary) %>% summarize(var1 = sum(count_test))
chi_table_2 <- chi_table_2[complete.cases(chi_table_2), ]
chi_table_2 <- spread(chi_table_2, boosted_med, var1)
chisq.test(chi_table_2[,-(1)], correct = FALSE)

chi_table_3 <-  nursing_home_data %>% group_by(boosted_group, res_cases_very_recent_binary) %>% summarize(var1 = sum(count_test))
chi_table_3 <- chi_table_3[complete.cases(chi_table_3), ]
chi_table_3 <- spread(chi_table_3, boosted_group, var1)
chisq.test(chi_table_3[,-(1)], correct = FALSE)

chi_table_4 <-  nursing_home_data %>% group_by(boosted_group, res_deaths_very_recent_binary) %>% summarize(var1 = sum(count_test))
chi_table_4 <- chi_table_4[complete.cases(chi_table_4), ]
chi_table_4 <- spread(chi_table_4, boosted_group, var1)
chisq.test(chi_table_4[,-(1)], correct = FALSE)

chi_table_5 <- nursing_home_data %>% group_by(vax_med, res_cases_recent_binary) %>% summarize(var1 = sum(count_test))
chi_table_5 <- chi_table_5[complete.cases(chi_table_5), ]
chi_table_5 <- spread(chi_table_5, vax_med, var1)
chisq.test(chi_table_5[,-(1)], correct = FALSE)

chi_table_6 <- nursing_home_data %>% group_by(vax_med, res_deaths_recent_binary) %>% summarize(var1 = sum(count_test))
chi_table_6 <- chi_table_6[complete.cases(chi_table_6), ]
chi_table_6 <- spread(chi_table_6, vax_med, var1)
chisq.test(chi_table_6[,-(1)], correct = FALSE)

chi_table_7 <-  nursing_home_data %>% group_by(vax_group, res_cases_recent_binary) %>% summarize(var1 = sum(count_test))
chi_table_7 <- chi_table_7[complete.cases(chi_table_7), ]
chi_table_7 <- spread(chi_table_7, vax_group, var1)
chisq.test(chi_table_7[,-(1)], correct = FALSE)

chi_table_8 <-  nursing_home_data %>% group_by(vax_group, res_deaths_recent_binary) %>% summarize(var1 = sum(count_test))
chi_table_8 <- chi_table_8[complete.cases(chi_table_8), ]
chi_table_8 <- spread(chi_table_8, vax_group, var1)
chisq.test(chi_table_8[,-(1)], correct = FALSE)

# 7. Difference in High/Low Booster Rate NHs
# What factors are associated with a high booster rate? 
nursing_home_data$for_profit_binary <- ifelse(grepl("For profit", nursing_home_data$ownership_type, fixed = TRUE), 1, 0)
nursing_home_data$pctblack_binary <- ifelse(nursing_home_data$pctblack > median(nursing_home_data$pctblack, na.rm = TRUE), 1, 0)
nursing_home_data$pctblack_binary <- ifelse(is.na(nursing_home_data$pctblack), NA, nursing_home_data$pctblack_binary)
nursing_home_data$pctblack_count <- ifelse(is.na(nursing_home_data$pctblack), 0, 1)
nursing_home_data$poverty_binary <- ifelse(nursing_home_data$county_poverty_pct > median(nursing_home_data$county_poverty_pct), 1, 0)
nursing_home_data$staffing_binary <- ifelse(nursing_home_data$adjusted_nurse_aide_staffing_hours_per_resident_per_day > 
                                              median(nursing_home_data$adjusted_nurse_aide_staffing_hours_per_resident_per_day, na.rm = TRUE), 1, 0)
nursing_home_data$staffing_binary <- ifelse(is.na(nursing_home_data$adjusted_nurse_aide_staffing_hours_per_resident_per_day), NA, nursing_home_data$staffing_binary)
nursing_home_data$staff_count <- ifelse(is.na(nursing_home_data$adjusted_nurse_aide_staffing_hours_per_resident_per_day), 0, 1)
nursing_home_data$rating_binary <- ifelse(nursing_home_data$overall_rating >= 4, 1, 0)
nursing_home_data$rating_binary <- ifelse(is.na(nursing_home_data$overall_rating), NA, nursing_home_data$rating_binary)
nursing_home_data$rate_count <- ifelse(is.na(nursing_home_data$overall_rating), 0, 1)


boost_factors <- nursing_home_data %>% group_by(boosted_group) %>% summarize(num_profit = sum(for_profit_binary),
                                                                    den_profit = sum(count_test),
                                                                    p_profit = mean(for_profit_binary),
                                                                    num_black = sum(pctblack_binary, na.rm = TRUE),
                                                                    den_black = sum(pctblack_count),
                                                                    p_black = mean(pctblack_binary, na.rm = TRUE),
                                                                    num_pov = sum(poverty_binary),
                                                                    den_pov = sum(count_test),
                                                                    p_pov = mean(poverty_binary),
                                                                    num_staff = sum(staffing_binary, na.rm = TRUE),
                                                                    den_staff = sum(staff_count),
                                                                    p_staff = mean(staffing_binary, na.rm = TRUE),
                                                                    num_rate = sum(rating_binary, na.rm = TRUE),
                                                                    den_rate = sum(rate_count),
                                                                    p_rate = mean(rating_binary, na.rm = TRUE))

boost_factors <- boost_factors[-c(2, 3, 5),]

prop.test(boost_factors$num_profit, boost_factors$den_profit, alternative = 'two.sided', conf.level = 0.95)
prop.test(boost_factors$num_black, boost_factors$den_black, alternative = 'two.sided', conf.level = 0.95)
prop.test(boost_factors$num_pov, boost_factors$den_pov, alternative = 'two.sided', conf.level = 0.95)
prop.test(boost_factors$num_staff, boost_factors$den_staff, alternative = 'two.sided', conf.level = 0.95)
prop.test(boost_factors$num_rate, boost_factors$den_rate, alternative = 'two.sided', conf.level = 0.95)

# Quick sums for callouts in paper
test <- nursing_home_data %>% mutate(sum_nh_cases = sum(res_cases_very_recent_binary),
                                sum_cases = sum(total_res_cases_very_recent),
                                sum_nh_death = sum(res_deaths_very_recent_binary),
                                sum_death = sum(total_res_deaths_very_recent))
