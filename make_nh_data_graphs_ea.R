###############################################################################
# Biostatistics 651 Final Project
# Create graphs exploring nursing home variables and how they are related
# October 2021
# Steps:
# 1. Housekeeping
# 2. Load data
# 3. Create graphs
###############################################################################

# 1. Housekeeping
# Remove objects previously stored in R
rm(list = ls())

# Set Paths
setwd('...replace with path to folder.../Bios_140.651_Final/Data')

# Load Libraries
library(tidyverse)
library(readxl)

# 2. Load data
nursing_home_data <- read.csv('tmp/nursing_home_data.csv')

# 3. Create graphs
#plot of overall rating vs infection citations
plot(nursing_home_data$Overall.Rating, nursing_home_data$infection.cit,
     main="overall rating vs infection citations",
     xlab = "overall rating",
     ylab = "Infection Citations")

#plot of infection citations vs staff vaccination
plot(nursing_home_data$infection.cit, nursing_home_data$avg.staff.vaccine.rate,
     main="infection citations vs avg staff vaccination",
     xlab = "infection citations",
     ylab = "avg staff vaccination")

# plot of avg staff vaccination vs total res cases 
staff_vax_vs_pct_cases <- nursing_home_data %>%
  ggplot(aes(avg.staff.vaccine.rate, total.res.cases.p, color=infection.cit))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Staff Vaccination Rate", y="% Total Resident Cases", subtitle="scatter plot of avg staff vaccination rate vs total resident cases rate")
staff_vax_vs_pct_cases

# plot of avg staff vaccination vs total res cases and health inspection rating
nursing_home_data %>%
  ggplot(aes(avg.staff.vaccine.rate, total.res.cases.p, color=Health.Inspection.Rating))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Staff Vaccination Rate", y="% Total Resident Cases", subtitle="scatter plot of avg staff vaccination rate vs total resident cases rate")

# plot of avg staff vaccination vs total res cases and fine amount
nursing_home_data %>%
  ggplot(aes(avg.staff.vaccine.rate, total.res.cases.p, color=Total.Amount.of.Fines.in.Dollars))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Staff Vaccination Rate", y="% Total Resident Cases", subtitle="scatter plot of avg staff vaccination rate vs total resident cases rate")

# plot of avg staff vaccination vs total res cases and total citations
nursing_home_data %>%
  ggplot(aes(avg.staff.vaccine.rate, total.res.cases.p, color=citation.count))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Staff Vaccination Rate", y="% Total Resident Cases", subtitle="scatter plot of avg staff vaccination rate vs total resident cases rate")

# plot of avg staff vaccination vs total res cases and and number of citations from infection control inspections
nursing_home_data %>%
  ggplot(aes(avg.staff.vaccine.rate, total.res.cases.p, color=Number.of.Citations.from.Infection.Control.Inspections))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Staff Vaccination Rate", y="% Total Resident Cases", subtitle="scatter plot of avg staff vaccination rate vs total resident cases rate")

# plot of % staff cases vs % of resident cases and and health inspection rating
graph1a <- nursing_home_data %>%
  ggplot(aes(total.staff.cases.p, total.res.cases.p, color=Health.Inspection.Rating))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Percentage of Staff Cases", y="Percentage of Resident Cases", subtitle="Percentage of Staff Cases vs Resident Cases by Health Inspection Rating")

graph1b <- graph1a + labs(color = "Health Inspection Rating")
graph1b

# plot of % staff vaccination vs % of resident cases and and health inspection rating
graph2a <- nursing_home_data %>%
  ggplot(aes(avg.staff.vaccine.rate, total.res.cases.p, color=Health.Inspection.Rating))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Staff Vaccination Rate", y="Percentage of Resident Cases", subtitle="Staff Vaccination Rate vs Resident Cases by Health Inspection Rating")

graph2b <- graph1a + labs(color = "Health Inspection Rating")

print(graph2b)

# plot of % staff vaccination vs % of resident cases and and infection citations
graph2a <- nursing_home_data %>%
  ggplot(aes(avg.staff.vaccine.rate, total.res.cases.p, color=infection.cit))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Staff Vaccination Rate", y="Percentage of Resident Cases", subtitle="Staff Vaccination Rate vs Percentage Resident Cases by Number of Infection Citations")

graph2b <- graph2a + labs(color = "Number of Infection Citations") + theme_bw()

print(graph2b)

# plot of % staff vaccination vs % of resident cases and total weighted health survey score
graph3a <- nursing_home_data %>%
  ggplot(aes(avg.staff.vaccine.rate, total.res.cases.recent.p, color=Total.Weighted.Health.Survey.Score))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Staff Vaccination Rate", y="Percentage of Resident Cases", subtitle="Staff Vaccination Rate vs Percentage Resident Cases by Weighted CMS Health Survey Score")

graph3b <- graph3a + labs(color = "Weighted CMS Health Survey Score") + theme_bw()

print(graph3b)

# plot of % staff vaccination vs % of resident cases and overall CMS score
graph4a <- nursing_home_data %>%
  ggplot(aes(avg.staff.vaccine.rate, total.res.cases.recent.p, color=Overall.Rating))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Staff Vaccination Rate", y="Percentage of Resident Cases", subtitle="Staff Vaccination Rate vs Percentage Resident Cases by CMS Score")

graph4b <- graph4a + labs(color = "Overall CMS Score")

print(graph4b)

# plot of % staff vaccination vs % of resident cases and ownership type
graph5a <- nursing_home_data %>%
  ggplot(aes(avg.staff.vaccine.rate, total.res.cases.recent.p, color=Ownership.Type))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Staff Vaccination Rate", y="Percentage of Resident Cases", subtitle="Staff Vaccination Rate vs Percentage Resident Cases by Ownership Type")

graph5b <- graph5a + labs(color = "Ownership Type") + theme_bw()

print(graph5b)















# plot of % staff vaccination vs % of resident deaths and and infection citations
graph6a <- nursing_home_data %>%
  ggplot(aes(avg.staff.vaccine.rate, total.res.deaths.recent.p, color=infection.cit))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Staff Vaccination Rate", y="Percentage of Resident Deaths", subtitle="Staff Vaccination Rate vs Percentage Resident Deaths by Number of Infection Citations")

graph6b <- graph6a + labs(color = "Number of Infection Citations") + theme_bw()

print(graph6b)

# plot of % staff vaccination vs % of resident deaths and total weighted health survey score
graph7a <- nursing_home_data %>%
  ggplot(aes(avg.staff.vaccine.rate, total.res.deaths.recent.p, color=Total.Weighted.Health.Survey.Score))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Staff Vaccination Rate", y="Percentage of Resident Deaths", subtitle="Staff Vaccination Rate vs Percentage Resident Deaths by Weighted CMS Health Survey Score")

graph7b <- graph7a + labs(color = "Weighted CMS Health Survey Score") + theme_bw()

print(graph7b)



# plot of % res vaccination vs % of resident deaths and total weighted health survey score
graph8a <- nursing_home_data %>%
  ggplot(aes(avg.res.vaccine.rate, total.res.deaths.recent.p, color=Total.Weighted.Health.Survey.Score))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Resident Vaccination Rate", y="Percentage of Resident Deaths", subtitle="Resident Vaccination Rate vs Percentage Resident Deaths by Weighted CMS Health Survey Score")

graph8b <- graph8a + labs(color = "Weighted CMS Health Survey Score") + theme_bw()

print(graph8b)


# plot of % res vaccination vs % of resident deaths and infection citations
graph9a <- nursing_home_data %>%
  ggplot(aes(avg.res.vaccine.rate, total.res.deaths.recent.p, color=infection.cit))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Resident Vaccination Rate", y="Percentage of Resident Deaths", subtitle="Resident Vaccination Rate vs Percentage Resident Deaths by Infection Citations")

graph9b <- graph9a + labs(color = "Number of Infection Citations") + theme_bw()

print(graph9b)


# plot of % res vaccination vs % of resident deaths and cms weighted health survey score
graph10a <- nursing_home_data %>%
  ggplot(aes(avg.res.vaccine.rate, total.res.deaths.recent.p, color=Total.Weighted.Health.Survey.Score))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Average Resident Vaccination Rate", y="Percentage of Resident Deaths", subtitle="Resident Vaccination Rate vs Percentage Resident Deaths by Weighted CMS Health Survey Score")

graph10b <- graph10a + labs(color = "Weighted CMS Health Survey Score") + theme_bw()

print(graph10b)


# plot of % nurse time spent w res vs % of resident deaths and cms weighted health survey score
graph11a <- nursing_home_data %>%
  ggplot(aes(Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day, total.res.deaths.recent.p, color=Total.Weighted.Health.Survey.Score))+
  geom_point(alpha=0.5, size=2)+
  labs(x="Nursing Hours per Resident per Day", y="Percentage of Resident Deaths", subtitle="Nursing Hours per Resident per Day vs Percentage Resident Deaths by Weighted CMS Health Survey Score")

graph11b <- graph11a + labs(color = "Weighted CMS Health Survey Score") + theme_bw()

print(graph11b)
