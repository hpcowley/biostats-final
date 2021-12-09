###############################################################################
# Biostatistics 651 Final Project
# Explore and create graphs with MD COVID data and nursing home COVID data
# October 2021
# Steps:
# 1. Housekeeping
# 2. Exploratory Data Analysis: Maryland COVID-19 Rates Data Preparation
# 3. Exploratory Data Analysis: Maryland COVID-19 Rates
###############################################################################

# 1. Housekeeping
# Remove objects previously stored in R
rm(list = ls())

# Set Paths
setwd('...replace with path to folder.../Bios_140.651_Final/Data')

###############################################################################
# 2. Exploratory Data Analysis: Maryland COVID-19 Rates Data Preparation
###############################################################################

library(dplyr)
library(readr)
library (stringr)
library(ggplot2)

dta <- read_csv("tmp/nursing_home_data.csv")
mth <- read_csv("tmp/nh_covid_md_month.csv")
md_covid <- read_csv("tmp/newcases_rates_formerge.csv")

#Linking the monthly nursing home data to the combined CMS file for SSACD code and county name.
mth <- mth %>%
  rename(provider.id=Federal.Provider.Number)
combined <- mth %>%
  full_join(dta, by= "provider.id")
combined <- data_frame(combined)

#Selecting relevant variables for time series analysis.
mth_cty <- combined %>%
  select(c(year:total.res.deaths.month.p, SSACD, Provider.County.Name, year.month))
md_covid <- md_covid %>%
  select(-c(apr2020:sept2021)) %>%
  select(-c(avg_total, population))

#Reshaping to match the nursing home COVID-19 data.
md_covid_long <- md_covid %>%
  select(-c(county))
md_covid_long <- gather(md_covid_long,
                        key="time",
                        value="case_rate",
                        c(-SSACD))

#Renaming month and year variables to match the nursing home COVID-19 data.
md_covid1 <- md_covid_long %>%
  mutate(year = ifelse(grepl("2020", time),2020,2021)) %>%
  mutate(month = ifelse(grepl("jan",time),1,0)) %>%
  mutate(month = ifelse(grepl("feb",time),2,month)) %>%
  mutate(month = ifelse(grepl("mar",time),3,month)) %>%
  mutate(month = ifelse(grepl("apr",time),4,month)) %>%
  mutate(month = ifelse(grepl("may",time),5,month)) %>%
  mutate(month = ifelse(grepl("jun",time),6,month)) %>%
  mutate(month = ifelse(grepl("july",time),7,month)) %>%
  mutate(month = ifelse(grepl("aug",time),8,month)) %>%
  mutate(month = ifelse(grepl("sept",time),9,month)) %>%
  mutate(month = ifelse(grepl("oct",time),10,month)) %>%
  mutate(month = ifelse(grepl("nov",time),11,month)) %>%
  mutate(month = ifelse(grepl("dec",time),12,month))

#Merging the Maryland COVID data and the CMS Nursing Home COVID data.
fn <- full_join(mth_cty, md_covid1, by=c("SSACD","month","year"))
fn <- fn %>%
  select(-c(time))
fn <- data_frame(fn)

#Averaging the case rates for each nursing home.
fn1 <- fn %>%
  rowwise() %>%
  group_by(year.month, Provider.County.Name) %>%
  mutate(res_cases = mean(total.res.cases.month.p, na.rm=TRUE)*100) %>% #New resident cases per 100 NH residents.
  mutate(state_cases = mean(case_rate)) %>% #New case rate per 1,000 Maryland residents. This variable is already expressed as per 1,000.
  mutate(staff_cases = mean(total.staff.cases.month.p, na.rm=TRUE)*100) %>% #New cases in staff per 100 NH residents.
  filter(SSACD > 0)

#Averaging each county's nursing homes in Maryland by month
fn2 <- fn1 %>%
  group_by(year.month) %>%
  rowwise() %>%
  mutate(res_cases = mean(res_cases, na.rm=TRUE)) %>%
  mutate(state_cases = mean(state_cases, na.rm=TRUE)) %>%
  mutate(staff_cases = mean(staff_cases, na.rm=TRUE))

fn2 <- fn2 %>%
  group_by(Provider.County.Name, year.month) %>%
  filter(row_number()==1)

#Averaging the state nursing home rates
fn3 <- fn2 %>%
  group_by(year.month) %>%
  mutate(res_cases = mean(res_cases, na.rm=TRUE)) %>%
  mutate(state_cases = mean(state_cases, na.rm=TRUE)) %>%
  mutate(staff_cases = mean(staff_cases, na.rm=TRUE))

fn3 <- fn3 %>%
  group_by(year.month) %>%
  filter(row_number()==1)

#Final plotting data frame
for_plots <- fn3 %>%
  filter(year >0)

###############################################################################
# 3. Exploratory Data Analysis: Maryland COVID-19 Rates
###############################################################################

#Setting vertical line for when COVID-19 vaccines became available for nursing homes.
dates_vline <- as.Date("2020-12-01")
dates_vline <- which(for_plots$year.month %in% dates_vline)

#Plotting Maryland residents
ggplot(for_plots, aes(x=year.month, y=state_cases)) +
    geom_line(color="red", size = 0.5) +
  xlab("Month and Year (May 2020 - September 2021)") +
  ylab("Monthly New Cases per 1,000 Residents") +
  ggtitle("Average Monthly New COVID-19 Cases in Maryland per 1,000 Residents") +
  geom_vline(xintercept = as.numeric(for_plots$year.month[dates_vline]),
             col="black", size=.75) +
  theme_classic()

#Plotting nursing home residents
ggplot(for_plots, aes(x=year.month, y=res_cases)) +
  geom_line(color="blue", size = 0.5) +
  xlab("Month and Year (May 2020 - September 2021)") +
  ylab("Monthly New Cases per 100 NH Residents") +
  ggtitle("Average Monthly New COVID-19 Cases Among Residents in Nursing Homes per 100 NH Residents") +
  geom_vline(xintercept = as.numeric(for_plots$year.month[dates_vline]),
             col="black", size=.75) +
  theme_classic()

#Plotting nursing home staff
ggplot(for_plots, aes(x=year.month, y=staff_cases)) +
  geom_line(color="green", size = 0.5) +
  xlab("Month and Year (May 2020 - September 2021)") +
  ylab("Monthly New Cases per 100 NH Residents") +
  ggtitle("Average Monthly New COVID-19 Cases Among Staff in Nursing Homes per 100 NH Residents") +
  geom_vline(xintercept = as.numeric(for_plots$year.month[dates_vline]),
             col="black", size=.75) +
  theme_classic()
