###############################################################################
# Biostatistics 1 Final Project
# Prepare Nursing Home COVID-19 Data
# October 2021
# Steps:
# 1. Housekeeping
# 2. Load data and basic cleaning
# 3. Create recent cases data
# 4. Create all cases data
# 5. Create monthly cases data
# 6. Create summary data set combining all nursing homes
# 7. Make summary graphs
# 8. Export data
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
library('zoo')

# 2. Load data and basic cleaning
# Load Nursing Home COVID-19 Data
nh_covid_21 <- read.csv(paste(data_path, 'faclevel_2021.csv', sep=""))
nh_covid_20 <- read.csv(paste(data_path, 'fac_level_2020.csv', sep=""))

# Only examine Maryland nursing homes
nh_covid_md_21 <- nh_covid_21[which(nh_covid_21$Provider.State=='MD'), ]
nh_covid_md_20 <- nh_covid_20[which(nh_covid_20$Provider.State=='MD'), ]

# Append 2020 and 2021 data
nh_covid_md <- rbind(nh_covid_md_20, nh_covid_md_21)

# Only examine weeks that nursing homes passed quality check
prop.table(table(nh_covid_md$Passed.Quality.Assurance.Check))
nh_covid_md <- nh_covid_md[which(nh_covid_md$Passed.Quality.Assurance.Check=='Y'), ]

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

# 3. Create recent cases data
# Restrict to since vaccines were reported (last week of May 2021 onward)
nh_covid_md_recent <- subset(nh_covid_md, Week.Ending > as.Date('2021-05-29'))

# Create aggregate variables for recent data
nh_covid_md_recent <- nh_covid_md_recent  %>% group_by(Federal.Provider.Number) %>% 
  # Resident total cases and deaths
  mutate(total.res.cases.recent = sum(Residents.Weekly.Confirmed.COVID.19),
         total.res.deaths.recent = sum(Residents.Weekly.COVID.19.Deaths),
         # Staff total cases and deaths
         total.staff.cases.recent = sum(Staff.Weekly.Confirmed.COVID.19),
         total.staff.deaths.recent = sum(Staff.Weekly.COVID.19.Deaths),
         # Current percent of occupied beds
         avg.number.of.occupied.beds.recent = mean(Total.Number.of.Occupied.Beds, na.rm = TRUE))

# Check that new variables are not missing
verify(nh_covid_md_recent, !is.na(total.res.cases.recent))
verify(nh_covid_md_recent, !is.na(total.res.deaths.recent))
verify(nh_covid_md_recent, !is.na(total.staff.cases.recent))
verify(nh_covid_md_recent, !is.na(total.staff.deaths.recent))
verify(nh_covid_md_recent, !is.na(avg.number.of.occupied.beds.recent))

# Bring nh_covid_md_recent to overall level
nh_covid_md_recent <- nh_covid_md_recent %>% group_by(Federal.Provider.Number) %>% filter(row_number() == n())
nh_covid_md_recent <- nh_covid_md_recent[, c('Federal.Provider.Number',
                                             'total.res.cases.recent', 'total.res.deaths.recent', 'total.staff.cases.recent', 
                                             'total.staff.deaths.recent', 'avg.number.of.occupied.beds.recent')]

# Loop through to create a percent of cases/death out of the average number of occupied beds
# To compare nursing homes to each other
# Recent data
loop_list_recent <- c('total.staff.cases.recent', 'total.res.cases.recent', 'total.staff.deaths.recent', 'total.res.deaths.recent')
for (i in loop_list_recent) {
  new_var = paste((i), ".p", sep = '')
  nh_covid_md_recent[, new_var] = nh_covid_md_recent[, paste(i)] / nh_covid_md_recent[, 'avg.number.of.occupied.beds.recent']
}

# 4. Create all cases data
# Create aggregate variables
nh_covid_md <- nh_covid_md %>% group_by(Federal.Provider.Number) %>% 
  # Resident total cases and deaths
  mutate(total.res.cases = sum(Residents.Weekly.Confirmed.COVID.19),
  total.res.deaths = sum(Residents.Weekly.COVID.19.Deaths),
  # Staff total cases and deaths
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

# Join with nh_covid_md_recent
nh_covid_md_base <- left_join(nh_covid_md_base, nh_covid_md_recent, by = 'Federal.Provider.Number')

# 5. Create monthly cases data
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

# Check that new variables are not missing
verify(nh_covid_md_month, !is.na(total.res.cases.month))
verify(nh_covid_md_month, !is.na(total.res.deaths.month))
verify(nh_covid_md_month, !is.na(total.staff.cases.month))
verify(nh_covid_md_month, !is.na(total.staff.deaths.month))
verify(nh_covid_md_month, !is.na(avg.res.weekly.cases.month))
verify(nh_covid_md_month, !is.na(avg.res.weekly.deaths.month))
verify(nh_covid_md_month, !is.na(avg.staff.weekly.cases.month))
verify(nh_covid_md_month, !is.na(avg.staff.weekly.deaths.month))

# Loop through to create a percent of cases/death out of the average number of occupied beds
# To compare nursing homes to each other
# Month data
loop_list_month <- c('total.staff.cases.month', 'total.res.cases.month', 'total.staff.deaths.month', 'total.res.deaths.month')
for (i in loop_list_month) {
  new_var = paste((i), ".p", sep = '')
  nh_covid_md_month[, new_var] = nh_covid_md_month[, paste(i)] / nh_covid_md_month[, 'avg.weekly.beds.occupied.month']
}

# 6. Create summary data set combining all nursing homes
# Make year/month variable
nh_covid_md_month$year.month <- as.yearmon(paste(nh_covid_md_month$year, nh_covid_md_month$month), "%Y %m")
nh_covid_md_month$year.month <- as_date(nh_covid_md_month$year.month)
# Bring to year month level
nh_covid_md_month_all <- nh_covid_md_month %>%
  group_by(year, month, year.month) %>%
  summarise(total.res.cases.month = sum(total.res.cases.month),
            total.res.deaths.month = sum(total.res.deaths.month),
            total.staff.cases.month = sum(total.staff.cases.month),
            total.staff.deaths.month = sum(total.staff.deaths.month),
            avg.res.weekly.vaccine.rate = mean(avg.res.weekly.vaccine.rate.month, na.rm = TRUE),
            avg.staff.weekly.vaccine.rate = mean(avg.staff.weekly.vaccine.rate.month, na.rm = TRUE))

# 7. Make summary graphs
# Resident Vaccine Rate
c1 <- rgb(24, 140, 80, max = 255, alpha = 90, names = "a.green")
h <- hist(nh_covid_md_base$avg.res.vaccine.rate, breaks = 40)
h$density = h$counts/sum(h$counts)
plot(h, freq=FALSE, main = 'Average Resident Full Vaccination Rate \nMaryland, May 2021 - Sept 2021', 
     xlab = 'Resident Vaccine Rate', col = c1)

# Resident COVID-19 Cases / Avg Number of Occupied Beds
c2 <- rgb(24, 113, 140, max = 255, alpha = 90, names = "a.blue")
h2 <- hist(nh_covid_md_base$total.res.cases.p, breaks = 40)
h2$density = h2$counts/sum(h2$counts)
plot(h2, freq=FALSE, main = 'Percent of Residents with Confirmed COVID-19 Cases \nMaryland, May 2020 - Sept 2021', 
     xlab = 'Total COVID-19 Resident Cases / Avg. Number of Occupied Beds', col = c2)

# Weekly Cases
ggplot(nh_covid_md,aes(x=Week.Ending, y=Residents.Weekly.Confirmed.COVID.19, group=Federal.Provider.Number)) + 
  geom_line(col = 'slateblue', alpha = 0.2) + theme(legend.position="none", 
  axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(face = 'bold', hjust = 0.5), 
  axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_blank(), panel.background = element_blank()) + 
  scale_x_date(date_breaks = "1 months", date_labels = "%Y-%m") +
  xlab('') + ylab('Residents Weekly Confirmed COVID-19 Cases') + 
  labs(title = 'Nursing Home Residents Weekly Confirmed COVID-19 Cases \nMaryland, May 2020 - Sept 2021') 
ggsave('output/nh_covid_cases_timeseries.pdf', plot = last_plot())

# Lasagna Chart
ggplot(nh_covid_md,aes(x=Week.Ending, y=Federal.Provider.Number, fill=Residents.Weekly.Confirmed.COVID.19)) + 
  geom_tile(col = 'white', size = 0) + scale_fill_viridis_c(option = "A" , direction = -1) +
  scale_x_date(date_breaks = "4 weeks", date_labels = "%m-%d-%y", limits = as.Date(c('2020-05-24','2021-09-16'))) +
  xlab('') + ylab('Nursing Homes') + 
  labs(title = 'Nursing Home Residents Weekly Confirmed COVID-19 Cases \nMaryland, May 2020 - Sept 2021') +
  theme(legend.position='bottom', axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
  plot.title = element_text(face = 'bold', hjust = 0.5))  +
  guides(fill = guide_colourbar(title = 'Resident Cases by Week', barwidth = 10, barheight = 0.5, direction = "horizontal", nbin = 10)) 
ggsave('output/nh_covid_cases_lasagna.pdf', plot = last_plot())

# 8. Export data
# Export data to tmp folder
write.csv(nh_covid_md_base, paste(tmp_path, 'nh_covid_md_base.csv', sep=""), row.names = FALSE)
write.csv(nh_covid_md_month, paste(tmp_path, 'nh_covid_md_month.csv', sep=""), row.names = FALSE)
