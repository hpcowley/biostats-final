###############################################################################
# Biostatistics 1 Final Project
# Prepare Nursing Home COVID-19 Data
# October 2021
###############################################################################

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
library('zoo')

# Load Nursing Home COVID-19 Data
nh_covid_21 <- read.csv(paste(data_path, 'faclevel_2021.csv', sep=""))
nh_covid_20 <- read.csv(paste(data_path, 'fac_level_2020.csv', sep=""))


# Only examine Maryland nursing homes
nh_covid_md_21 <- nh_covid_21[which(nh_covid_21$Provider.State=='MD'),]
nh_covid_md_20 <- nh_covid_20[which(nh_covid_20$Provider.State=='MD'),]

# Append 2020 and 2021 data
nh_covid_md <- rbind(nh_covid_md_20, nh_covid_md_21)

# Only examine weeks that nursing homes passed quality check
prop.table(table(nh_covid_md$Passed.Quality.Assurance.Check))
nh_covid_md <- nh_covid_md[which(nh_covid_md$Passed.Quality.Assurance.Check=='Y'),]

# Convert Week.Ending to date variable
nh_covid_md$Week.Ending <- mdy(nh_covid_md$Week.Ending)

# Only keep relevant variables
nh_covid_md <- nh_covid_md[, c('Week.Ending', 'Federal.Provider.Number', 'Provider.Name', 
                         'Provider.Address', 'Provider.City', 'Provider.State',
                         'Provider.Zip.Code', 'County', 'Submitted.Data', 
                         'Passed.Quality.Assurance.Check', 'Residents.Weekly.Confirmed.COVID.19', 
                         'Residents.Total.Confirmed.COVID.19', 'Residents.Weekly.COVID.19.Deaths',
                         'Residents.Total.COVID.19.Deaths', 'Number.of.All.Beds', 
                         'Total.Number.of.Occupied.Beds', 'Staff.Weekly.Confirmed.COVID.19',
                         'Staff.Total.Confirmed.COVID.19', 'Staff.Weekly.COVID.19.Deaths', 
                         'Staff.Total.COVID.19.Deaths', 'Shortage.of.Nursing.Staff', 
                         'Percentage.of.Current.Residents.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time',
                         'Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time')]

# Create aggregate variables
nh_covid_md <- nh_covid_md %>% group_by(Federal.Provider.Number) %>% 
  mutate(total.res.cases = sum(Residents.Weekly.Confirmed.COVID.19),
  # Resident total cases and deaths from most recent week
  total.res.deaths = sum(Residents.Weekly.COVID.19.Deaths),
  # Staff total cases and deaths from most recent week
  total.staff.cases = sum(Staff.Weekly.Confirmed.COVID.19),
  total.staff.deaths = sum(Staff.Weekly.COVID.19.Deaths),
  # Most recent resident vaccine rate, sometimes had to go back one week
  current.res.vaccine.rate = 
    ifelse(is.na(Percentage.of.Current.Residents.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time[n()]),
    Percentage.of.Current.Residents.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time[n()-1],
    Percentage.of.Current.Residents.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time[n()]),
  # Most recent healthcare staff vaccine rate, sometimes had to go back one or two weeks
  current.staff.vaccine.rate = 
    ifelse(!is.na(Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time[n()]),
    Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time[n()],
    ifelse(!is.na(Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time[n()-1]),
    Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time[n()-1],
    Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time[n()-2])),
  # Avg vaccination rate
  avg.res.vaccine.rate = mean(Percentage.of.Current.Residents.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time, na.rm = TRUE),
  avg.staff.vaccine.rate = mean(Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time, na.rm = TRUE),
  # Current percent of occupied beds
  avg.bed.occupancy.rate = mean(Total.Number.of.Occupied.Beds, na.rm = TRUE) / mean(Number.of.All.Beds, na.rm = TRUE),
  avg.number.of.occupied.beds = mean(Total.Number.of.Occupied.Beds, na.rm = TRUE))

# Check that new variables are not missing
verify(nh_covid_md, !is.na(total.staff.cases))
verify(nh_covid_md, !is.na(total.res.cases))
verify(nh_covid_md, !is.na(total.staff.deaths))
verify(nh_covid_md, !is.na(total.res.deaths))
verify(nh_covid_md, !is.na(avg.bed.occupancy.rate))
verify(nh_covid_md, !is.na(avg.number.of.occupied.beds))
verify(nh_covid_md, !is.na(avg.res.vaccine.rate))
verify(nh_covid_md, !is.na(avg.staff.vaccine.rate))
verify(nh_covid_md, !is.na(current.res.vaccine.rate))
verify(nh_covid_md, !is.na(current.staff.vaccine.rate))

# Create temporal average variables
nh_covid_md_month <- nh_covid_md %>%
  mutate(year = year(Week.Ending), month = month(Week.Ending)) %>%
  group_by(year, month, Federal.Provider.Number) %>%
  summarise(total.res.cases.month = sum(Residents.Weekly.Confirmed.COVID.19),
            total.res.deaths.month = sum(Residents.Weekly.COVID.19.Deaths),
            total.staff.cases.month = sum(Staff.Weekly.Confirmed.COVID.19),
            total.staff.deaths.month = sum(Staff.Weekly.COVID.19.Deaths),
            avg.res.weekly.cases.month = mean(Residents.Weekly.Confirmed.COVID.19),
            avg.res.weekly.deaths.month = mean(Residents.Weekly.COVID.19.Deaths),
            avg.staff.weekly.cases.month = mean(Staff.Weekly.Confirmed.COVID.19),
            avg.staff.weekly.deaths.month = mean(Staff.Weekly.COVID.19.Deaths),
            avg.res.weekly.vaccine.rate.month = mean(Percentage.of.Current.Residents.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time, na.rm = TRUE),
            avg.staff.weekly.vaccine.rate.month = mean(Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time, na.rm = TRUE),
            avg.weekly.beds.occupied.month = mean(Total.Number.of.Occupied.Beds, na.rm = TRUE),
            avg.weekly.bed.occupancy.rate.month = mean(Total.Number.of.Occupied.Beds, na.rm = TRUE) / mean(Number.of.All.Beds, na.rm = TRUE))
              
# Bring nh_covid_md to overall level
nh_covid_md_base <- nh_covid_md %>% group_by(Federal.Provider.Number) %>% filter(row_number() == n())
nh_covid_md_base <- nh_covid_md_base[, c('Federal.Provider.Number', 'Provider.Name', 
                                         'Provider.Address', 'Provider.City', 'Provider.State',
                                         'Provider.Zip.Code', 'County',
                                         'total.res.cases', 'total.res.deaths', 'total.staff.cases', 
                                         'total.staff.deaths', 'avg.number.of.occupied.beds',
                                         'avg.bed.occupancy.rate', 'avg.res.vaccine.rate', 
                                         'avg.staff.vaccine.rate')]

# Loop through to create a percent of cases/death out of the average number of occupied beds
# To compare nursing homes to each other
# Base data
loop_list_base <- c('total.staff.cases', 'total.res.cases', 'total.staff.deaths', 'total.res.deaths')
for (i in loop_list_base) {
  new_var = paste((i), ".p", sep = '')
  nh_covid_md_base[, new_var] = nh_covid_md_base[, paste(i)] / nh_covid_md_base[, 'avg.number.of.occupied.beds']
}

# Month data
loop_list_month <- c('total.staff.cases.month', 'total.res.cases.month', 'total.staff.deaths.month', 'total.res.deaths.month')
for (i in loop_list_month) {
  new_var = paste((i), ".p", sep = '')
  nh_covid_md_month[, new_var] = nh_covid_md_month[, paste(i)] / nh_covid_md_month[, 'avg.weekly.beds.occupied.month']
}

# Summary Statistics of Base Data
table(nh_covid_md_base$County)
hist(nh_covid_md_base$total.res.cases, breaks = 40, main = 'total MD nursing home resident cases 2021')
hist(nh_covid_md_base$total.staff.cases, breaks = 40, main = 'total MD nursing home staff cases 2021')
hist(nh_covid_md_base$total.res.deaths, breaks = 40, main = 'total MD nursing home resident deaths 2021')
hist(nh_covid_md_base$total.staff.deaths, breaks = 40, main = 'total MD nursing home staff deaths 2021')
hist(nh_covid_md_base$total.res.cases.p, breaks = 40, main = '% of resident cases out of avg occupied beds')
hist(nh_covid_md_base$total.staff.cases.p, breaks = 40, main = '% of staff cases out of avg occupied beds')
hist(nh_covid_md_base$current.res.vaccine.rate, breaks = 40, main = 'current resident vaccination rate')
hist(nh_covid_md_base$current.staff.vaccine.rate, breaks = 40, main = 'current staff vaccination rate')
plot(nh_covid_md_base$total.res.cases.p, nh_covid_md_base$avg.res.vaccine.rate)
cor(nh_covid_md_base$total.res.cases.p, nh_covid_md_base$avg.res.vaccine.rate)
plot(nh_covid_md_base$total.res.deaths.p, nh_covid_md_base$avg.res.vaccine.rate)
cor(nh_covid_md_base$total.res.deaths.p, nh_covid_md_base$avg.res.vaccine.rate)

# Summary Statistics of Month Data
# Make year/month variable
nh_covid_md_month$year_month <- as.yearmon(paste(nh_covid_md_month$year, nh_covid_md_month$month), "%Y %m")
nh_covid_md_month$year_month <- as_date(nh_covid_md_month$year_month)
nh_covid_md_month_all <- nh_covid_md_month %>%
  group_by(year, month, year_month) %>%
  summarise(total.res.cases.month = sum(total.res.cases.month),
            total.res.deaths.month = sum(total.res.deaths.month),
            total.staff.cases.month = sum(total.staff.cases.month),
            total.staff.deaths.month = sum(total.staff.deaths.month),
            avg.res.weekly.vaccine.rate = mean(avg.res.weekly.vaccine.rate.month, na.rm = TRUE),
            avg.staff.weekly.vaccine.rate = mean(avg.staff.weekly.vaccine.rate.month, na.rm = TRUE))
ggplot(nh_covid_md_month_all, aes(x = year_month, y = total.res.cases.month)) +
  geom_bar(stat = "identity") + scale_x_date(date_breaks = "2 months", date_labels = "%Y %m")
ggplot(nh_covid_md_month_all, aes(x = year_month, y = total.res.deaths.month)) +
  geom_bar(stat = "identity") + scale_x_date(date_breaks = "2 months", date_labels = "%Y %m")
ggplot(nh_covid_md_month_all, aes(x = year_month, y = avg.res.weekly.vaccine.rate)) +
  geom_bar(stat = "identity") + scale_x_date(date_breaks = "2 months", date_labels = "%Y %m")
ggplot(nh_covid_md_month_all, aes(x = year_month, y = total.staff.cases.month)) +
  geom_bar(stat = "identity") + scale_x_date(date_breaks = "2 months", date_labels = "%Y %m")
ggplot(nh_covid_md_month_all, aes(x = year_month, y = total.staff.deaths.month)) +
  geom_bar(stat = "identity") + scale_x_date(date_breaks = "2 months", date_labels = "%Y %m")
ggplot(nh_covid_md_month_all, aes(x = year_month, y = avg.staff.weekly.vaccine.rate)) +
  geom_bar(stat = "identity") + scale_x_date(date_breaks = "2 months", date_labels = "%Y %m")

# Export data to tmp folder
write.csv(nh_covid_md_base, paste(tmp_path, 'nh_covid_md_base.csv', sep=""), row.names = FALSE)
write.csv(nh_covid_md_month, paste(tmp_path, 'nh_covid_md_month.csv', sep=""), row.names = FALSE)
