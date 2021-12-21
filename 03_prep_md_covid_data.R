###############################################################################
# Biostatistics 652 Final Project
# Prepare Maryland COVID-19 County data
# December 2021
# Steps:
# 1. Set-up and creating totals variables
# 2. Reshaping dataset for merging
# 3. Creating time variables and linking with county-level census data
# 4. New cases only file
# 5. Cleaning and merging with Census data for the county-level rates (denominator)
# 6. Adding SSA codes for the merge with other files in the project.
# 7. Make county-level rates per month. (Prevalence rate per 1,000 residents)
# 8. Export
###############################################################################

###############################################################################
# 1. Set-up and creating totals variables
###############################################################################
# Remove objects previously stored in R
rm(list = ls())

# Set Paths
setwd('/Users/rcorgel/OneDrive - Johns Hopkins/Bios_140.651_Final/Data')

#Read in the Maryland COVID-19 data downloaded from the Maryland Department of Health.
md_covid <- read.csv("raw/MDCOVID19_CasesByCounty_Dec14.csv")

#Load relevant packages for data preparation.
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

#Take a look at the data.
head(md_covid)

#Create a State Total variable (Maryland) that sums all the counties.
md_covid <- md_covid %>%
  rowwise() %>%
  mutate(Maryland = sum(c(Allegany, Anne_Arundel, Baltimore, Baltimore_City, Calvert, Caroline, Carroll, Cecil, Charles, Dorchester, Frederick, Garrett, Harford, Howard, Kent, Montgomery, Prince_Georges, Queen_Annes, Somerset, St_Marys, Talbot, Washington, Wicomico, Worcester, Unknown), na.rm=TRUE))
md_covid <- tibble(md_covid)

#R does not recognize the raw data as containing dates, so we need to convert DATE to a date-time variable using lubridate.
md_covid <- md_covid %>%
  mutate(date = as.Date(DATE, rm.na=TRUE))

#In the loop below, we create variables for new cases by day in each county and the state.
for (i in 3:28){ #Selects the columns containing the county totals.
  md_covid <- md_covid %>%
    print(i)
    c <- paste("county", i, sep="")
    md_covid[[c]] = md_covid[[i]] - lag(md_covid[[i]])
}

#So that we know which counties are which, we rename the loop-generated variables.
md_covid <- md_covid %>% 
  dplyr::rename(Allegany_new = county3,
         Anne_Arundel_new = county4,
         Baltimore_new = county5,
         Baltimore_City_new = county6,
         Calvert_new = county7,
         Caroline_new = county8,
         Caroll_new = county9,
         Cecil_new =county10,
         Charles_new = county11,
         Dorchester_new = county12,
         Frederick_new = county13,
         Garrett_new = county14,
         Harford_new = county15,
         Howard_new = county16,
         Kent_new = county17,
         Montgomery_new = county18,
         Prince_Georges_new = county19,
         Queen_Annes_new = county20,
         Somerset_new = county21,
         St_Marys_new = county22,
         Talbot_new = county23,
         Washington_new = county24,
         Wicomico_new = county25,
         Worcester_new = county26,
         Unknown_new = county27,
         Maryland_new = county28)

#There are some cases where the new cases are negative. We made the decision to make these values 0 since we sum total new cases by month.
#We contacted the Maryland Department of Health, but did not receive follow-up as to why total cases decreases occasionally, when it should be strictly increasing.
#We hypothesize this was due to data collection or input error.
md_covid[md_covid < 0] <- 0

#Check to make sure there are no negative new cases.
summary(md_covid)

###############################################################################
# 2. Reshaping dataset for merging
###############################################################################

#Below, we get rid of two unneeded variables. Keep in mind that we created a 'date' variable in the above section.
md_covid <- md_covid %>%
  select(-c(OBJECTID, DATE,))
md_covid <- md_covid %>% select(date, everything())

#Reshape the data long so each row is one day for one county/jurisdiction. There are 30,726 observations in this dataframe.
long_md_covid <- md_covid %>%
  gather(key = "County",
         value = "Cases",
         c(-date))

#Reshape the data wide so each county has a row for the total new cases per day and the cumulative total cases.
wide_md_covid_bydate <- long_md_covid %>%
  spread(key = "date",
         value = "Cases")

#Here, we creating a variable that indicates whether the observation is observing the new cases or the total cases.
wide_md_covid_bydate <- wide_md_covid_bydate%>%
  mutate(new_total = ifelse(grepl("_new", County), 1, 0))
wide_md_covid_bydate <- data_frame(wide_md_covid_bydate)

###############################################################################
# 3. Creating time variables and linking with county-level census data
###############################################################################

#We create a variable for total new cases by month.
wide_monthly <- wide_md_covid_bydate %>%
  mutate(apr2020 = rowSums(wide_md_covid_bydate[19:48])) %>%
  mutate(may2020 = rowSums(wide_md_covid_bydate[49:79])) %>%
  mutate(june2020 = rowSums(wide_md_covid_bydate[80:109])) %>%
  mutate(july2020 = rowSums(wide_md_covid_bydate[110:140])) %>%
  mutate(aug2020 = rowSums(wide_md_covid_bydate[141:171])) %>%
  mutate(sept2020 = rowSums(wide_md_covid_bydate[172:201])) %>%
  mutate(oct2020 = rowSums(wide_md_covid_bydate[202:232])) %>%
  mutate(nov2020 = rowSums(wide_md_covid_bydate[233:262])) %>%
  mutate(dec2020 = rowSums(wide_md_covid_bydate[263:293])) %>%
  mutate(jan2021 = rowSums(wide_md_covid_bydate[294:324])) %>%
  mutate(feb2021 = rowSums(wide_md_covid_bydate[325:352])) %>%
  mutate(mar2021 = rowSums(wide_md_covid_bydate[353:383])) %>%
  mutate(apr2021 = rowSums(wide_md_covid_bydate[384:413])) %>%
  mutate(may2021 = rowSums(wide_md_covid_bydate[414:444])) %>%
  mutate(june2021 = rowSums(wide_md_covid_bydate[445:474])) %>%
  mutate(july2021 = rowSums(wide_md_covid_bydate[475:505])) %>%
  mutate(aug2021 = rowSums(wide_md_covid_bydate[506:536])) %>%
  mutate(sept2021 = rowSums(wide_md_covid_bydate[537:566])) %>%
  mutate(oct2021 = rowSums(wide_md_covid_bydate[567:597])) %>%
  mutate(nov2021 = rowSums(wide_md_covid_bydate[598:627]))

wide_monthly <- wide_monthly[-(2:631)]

###############################################################################
# 4. New cases only file
###############################################################################

#We filter out the observations that are not new cases.
library(stringr) #This package is useful for working with strings.
new_cases_md <- wide_monthly %>%
  filter(new_total==1)%>%
  mutate(County = str_remove(County,"_new"))

#Cleaning up county names.
new_cases_md <- new_cases_md %>%
  rowwise() %>%
  mutate(County = str_replace(County, "_", " "))
new_cases_md[18, 1] <- "Prince George's"
new_cases_md[19, 1] <- "Queen Anne's"
new_cases_md[7, 1] <- "Carroll"
new_cases_md[21, 1] <- "St. Mary's"

#Creating fixed average new cases per month variable. This can also be adjusted in larger merged file after/during exploratory data analysis.
new_cases_md <- data_frame(new_cases_md)
new_cases_md <- new_cases_md %>%
  rowwise() %>%
  mutate(avg_total = mean(c(apr2020, may2020, june2020, july2020, aug2020, sept2020, oct2020, nov2020,
                            dec2020, jan2021, feb2021, mar2021, apr2021, may2021, june2021, july2021,
                            aug2021, sept2021, oct2021, nov2021), na.rm=TRUE))

###############################################################################
# 5. Cleaning and merging with Census data for the county-level rates (denominator)
###############################################################################

#Merging Census county-level populations for a rate denominator. Data was obtained from the 2019 American Community Survey 5-year estimates.
maryland_census <- read.csv("raw/ACSDP5Y2019/ACSDP5Y2019.DP05_data_with_overlays_2021-10-11T195203.csv")

#Selecting relevant variables.
maryland_census <- maryland_census[-c(4:358)]
maryland_census<- maryland_census[-1,]
maryland_census<- maryland_census[-c(1)]

#Cleaning up the jurisdiction names for a clean merge with the new cases file.
maryland_census <- maryland_census %>%
  mutate(NAME = str_remove(NAME," County, Maryland")) %>%
  mutate(NAME = str_remove(NAME,", Maryland"))
maryland_census <- maryland_census %>% 
  dplyr::rename(County=NAME,
         population=DP05_0001E) 
maryland_census[24, 1] <- "Baltimore City"

#Converting the population variable from string to a numeric so it can serve as a 
maryland_census <- maryland_census %>%
  mutate(population = as.numeric(population))

#Check to make sure the variable types are correct.
str(maryland_census)

#Sum each county for the state total.
sum(maryland_census$population) #this value is 6018848. Add to census file
md.pop <- data.frame("Maryland",6018848)
names(md.pop) <- c("County", "population")
maryland_census <- rbind(maryland_census, md.pop)

newcases_rates <- left_join(new_cases_md, maryland_census, by="County")

###############################################################################
# 6. Adding SSA codes for the merge with other files in the project.
###############################################################################

#Reading in the SSA-FIP Crosswalk.
ssa <- read.csv("raw/ssa_fips_xwalk_mdonly.csv")

ssa <- ssa %>%
  dplyr::rename(County = 'County.Name')

ssa$County <- str_to_title(ssa$County)

ssa[17, 7] <- "Prince George's"
ssa[18, 7] <- "Queen Anne's"
ssa[19, 7] <- "St. Mary's"
ssa[25, 7] <- "Maryland"

ssa <- ssa %>%
  dplyr::select(c(County, SSACD))

newcases_rates <- left_join(newcases_rates, ssa, by="County")

newcases_rates <- newcases_rates[-2]

###############################################################################
# 7. Make county-level rates per month. (Prevalence rate per 1,000 residents)
###############################################################################

#Create variables in a loop for rates by month in each county and for the state of Maryland.
for (i in 2:22){
  newcases_rates <- newcases_rates %>%
    print(i)
  c <- paste("rate", i-1, sep="")
  newcases_rates[[c]] = (newcases_rates[[i]]/newcases_rates$population)*1000 #prevalence rate per 1,000 residents
}

#Rename the variables to indicate the month, year, and that the variable is a rate.
newcases_rates <- newcases_rates %>% 
  dplyr::rename(apr2020_rate = rate1,
         may2020_rate = rate2,
         june2020_rate = rate3,
         july2020_rate = rate4,
         aug2020_rate = rate5,
         sept2020_rate = rate6,
         oct2020_rate = rate7,
         nov2020_rate = rate8,
         dec2020_rate = rate9,
         jan2021_rate = rate10,
         feb2021_rate = rate11,
         mar2021_rate = rate12,
         apr2021_rate = rate13,
         may2021_rate = rate14,
         june2021_rate = rate15,
         july2021_rate = rate16,
         aug2021_rate = rate17,
         sept2021_rate = rate18,
         oct2021_rate = rate19,
         nov2021_rate = rate20,
         avg_total_rate = rate21)

newcases_rates <- newcases_rates[-23,]
newcases_rates <- newcases_rates %>%
  dplyr::rename(county = County)

###############################################################################
# 8. Export
newcases_rates
write.csv(newcases_rates, "tmp/newcases_rates_formerge.csv", row.names = FALSE)
save(newcases_rates, file = 'tmp/newcases_rates_formerge.RData')
###############################################################################
