
################################################################################################################
#Maryland COVID-19 Rates Data Preparation
################################################################################################################

library(dplyr)
library(stringr)
library(tidyr)

rm(list = ls())

dta <- read.csv("/Users/gracegahlon/OneDrive - Johns Hopkins/Bios_140.651_Final/Data/tmp/nursing_home_data.csv")
mth <- read.csv("/Users/gracegahlon/OneDrive - Johns Hopkins/Bios_140.651_Final/Data/tmp/nh_covid_md_month.csv")
md_covid <- read.csv("/Users/gracegahlon/OneDrive - Johns Hopkins/Bios_140.651_Final/Data/tmp/newcases_rates_formerge.csv")

#Linking the monthly nursing home data to the combined CMS file for SSACD code and county name.
mth <- mth %>%
  rename(provider.id=Federal.Provider.Number)
combined <- mth %>%
  full_join(dta, by= "provider.id")
combined <- data_frame(combined)

#Selecting relevant variables for time series analysis.
mth_cty <- combined %>%
  select(c(year:total.res.deaths.month.p, SSACD, Provider.County.Name, year.month,
           avg.res.weekly.vaccine.rate.month, avg.staff.weekly.vaccine.rate.month, population))
md_covid <- md_covid %>%
  select(-c(apr2020:nov2021)) %>%
  select(-c(avg_total))

#Reshaping to match the nursing home COVID-19 data.
md_covid_long <- md_covid %>%
  select(-c(county))
md_covid_long <- gather(md_covid_long,
                        key="time",
                        value="case_rate",
                        c(-SSACD))
md_covid_long <- tibble(md_covid_long)

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
  mutate(res_cases = mean(total.res.cases.month.p, na.rm=TRUE)) %>%
  mutate(county_cases = mean(case_rate, na.rm=TRUE)) %>% #New case rate per 1,000 Maryland residents. This variable is already expressed as per 1,000.
  mutate(staff_vac = mean(avg.staff.weekly.vaccine.rate.month, na.rm=TRUE)) %>%
  mutate(res_vac = mean(avg.res.weekly.vaccine.rate.month, na.rm=TRUE)) %>%
  mutate(staff_cases = mean(total.staff.cases.month.p, na.rm=TRUE)) %>%
  mutate(res_death = mean(total.res.deaths.month.p, na.rm=TRUE)) %>%
  filter(SSACD >0)

#Averaging each county's nursing homes in Maryland by month
fn2 <- fn1 %>%
  group_by(year.month) %>%
  rowwise() %>%
  mutate(res_cases = mean(res_cases, na.rm=TRUE)) %>%
  mutate(county_cases = mean(county_cases, na.rm=TRUE)) %>%
  mutate(staff_cases = mean(staff_cases, na.rm=TRUE))

fn2 <- fn2 %>%
  group_by(Provider.County.Name, year.month) %>%
  filter(row_number()==1)

fn2$Provider.County.Name[fn2$Provider.County.Name=="Prince Georges"] <- "Prince George's"
fn2$Provider.County.Name[fn2$Provider.County.Name=="Queen Annes"] <- "Queen Anne's"
fn2$Provider.County.Name[fn2$Provider.County.Name=="St. Marys"] <- "St. Mary's"

fn2 <- fn2 %>% rename(county = Provider.County.Name)

################################################################################################################
#Maryland Vaccination Rates: Grabbing those...
################################################################################################################

library(lubridate)
library(stringr)

#Reading in cleaned and combined ACS and RWJF data
v <- read.csv("/Users/gracegahlon/OneDrive - Johns Hopkins/Bios_140.651_Final/Data/raw/county_vaccination.csv")

#Formatting date and the monthly total number fully vaccinated
vac <- v %>%
  mutate(date = as.Date(VACCINATION_DATE)) %>%
  group_by(month=floor_date(date, "month"), County) %>%
  mutate(num_fullyvac=max(SecondDoseCumulative)) %>%
  select(County, date, month, num_fullyvac)

#Formatting dates
vac <- vac %>%
  group_by(County, month) %>%
  filter(row_number()==1) %>%
  filter(date < as.Date("2021-12-01")) %>%
  filter(County != "") %>%
  rename(county = County) %>%
  rename(year.month = month) %>%
  mutate(month = month(year.month), year = year(year.month)) %>%
  select(-c(date))

#Trimming whitespace off of the county names to faciliate join.
vac$county <- trimws(vac$county, which ="both")

#Joining the CMS data with the new vaccination data
with_vac <- full_join(fn2, vac, by=c("county", "year", "month"))

with_vac <- with_vac %>%
  mutate(pct_vac_county = num_fullyvac/population) %>%
  filter(county != "Unknown")

dem <- read.csv("/Users/gracegahlon/Desktop/dem_data.csv")
with_dem <- full_join(with_vac, dem, by="county")

################################################################################################################
#Maryland COVID-19 Rates: Regression Analyses
################################################################################################################

#Selecting relevant variables.
regr <- with_dem %>%
  select(year, month, SSACD, county, res_cases, staff_cases, county_cases,
         res_death, pct_vac_county, res_vac, staff_vac,acspct_black:income_ratio) %>%
  mutate(res_vac = res_vac/100, staff_vac = staff_vac/100, res_death*100)


#Want to split up pre-vaccine vs. post-vaccine
regr <- regr %>%
  mutate(prevaccine = ifelse(year.month < "2020-12-01", 1, 0))
prevac <- regr %>% filter(prevaccine == 1)
postvac <- regr %>% filter(prevaccine == 0)
postvac_o <- postvac %>% filter(staff_cases < 1) #Removing the outlier

#Preliminary analysis for visualization
#//Pre Vaccine//#
#Pre-vaccine: Residents compared to Staff
res_staff_prevac <- lm(res_cases~staff_cases, data = prevac)
summary(res_staff_prevac) #Adjusted R-squared:  0.7449 

#Pre-vaccine: Residents compared to County
res_state_prevac <- lm(res_cases~county_cases, data = prevac)
summary(res_state_prevac) #Adjusted R-squared:  0.1256 

#//Post Vaccine//#
#Post-vaccine: Residents compared to Staff - there is a clear outlier
res_staff_postvac <- lm(res_cases~staff_cases, data = postvac)
summary(res_staff_postvac)



### PRE-VACCINE MULTIPLE LINEAR REGRESSION ANALYSIS
library(olsrr)
c.all <- lm(res_cases~acspct_black + acspct_white + pct_obese + health_outcome_rank + income_ratio + county_cases + staff_cases, data = prevac)
summary(c.all)
all.pred <- ols_step_all_possible(model=c.all)
all.pred 
plot(all.pred) #Plots to evaluate R-squared and AIC

#Model 8
# county_cases staff_cases
model8 <- lm(res_cases~staff_cases + county_cases, data=prevac)
summary(model8)
plot(res_cases~staff_cases, data = prevac)
abline(model8)

#Model 29
#pct_obese county_cases staff_cases
model29 <- lm(res_cases~staff_cases + county_cases+ pct_obese, data=prevac)
summary(model29)
plot(res_cases~staff_cases + county_cases, data = prevac)
abline(model29)

#MODEL 64
#acspct_white pct_obese county_cases staff_cases
summary(lm(res_cases~staff_cases + county_cases + pct_obese + acspct_white, data = prevac))



### POST-VACCINE MULTIPLE LINEAR REGRESSION ANALYSIS

c.all <- lm(res_cases~acspct_black + acspct_white + pct_obese + health_outcome_rank + income_ratio + 
              county_cases + staff_cases + pct_vac_county, data = postvac_o)
summary(c.all)
all.pred <- ols_step_all_possible(model=c.all)
all.pred
plot(all.pred) #Plots to evaluate R-squared and AIC

#model 37
#county_cases staff_cases pct_vac_county
summary(lm(res_cases~staff_cases + county_cases + pct_vac_county, data=postvac_o))


#model 93
#pct_obese county_cases staff_cases pct_vac_county
summary(lm(res_cases~staff_cases + county_cases + pct_vac_county + pct_obese, data=postvac_o))


#model 163
#acspct_white pct_obese county_cases staff_cases pct_vac_county
summary(lm(res_cases~staff_cases + county_cases + pct_vac_county + pct_obese + acspct_white, data=postvac_o))




##### PLOTTING FOR PRESENTATION
prime <- summary(lm(res_cases~staff_cases+county_cases, data=postvac_o))

plot(res_cases~staff_cases, data = prevac, col="red",
     xlab = "Staff cases per 10 staff members",
     ylab = "Resident cases per 10 residents",
     pch=18) + points(postvac_o$staff_cases, postvac_o$res_cases, col="blue", pch=18)
abline(model8, col="red")
abline(prime, col="blue")

