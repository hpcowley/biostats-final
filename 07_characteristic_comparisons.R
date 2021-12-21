###############################################################################
# Biostatistics 652 Final Project
# Examine nursing homes characteristics associated with high/low cases/deaths
# December 2021
###############################################################################
library(tidyverse)
library(naniar)
library(ltm)
library(reshape)
library(dplyr)
library(dplyr)
library(gtsummary)
library(labelled)
library(lme4)
library(rms)
library(AICcmodavg)
library(sjPlot)
setwd('...replace with path to folder.../Bios_140.651_Final/Data')
load("tmp/nursing_home_data_clean.rdata")

##
# Prepare the data for this analysis
##
dta <- nursing_home_data %>% replace_with_na_all(condition = ~.x == "LNE")

dta$pctblack_missing <- as.numeric(is.na(dta$pctblack))
dta$pctwhite_missing <- as.numeric(is.na(dta$pctwhite))
dta$pcthighcfs_missing <- as.numeric(is.na(dta$pcthighcfs))
dta$pctunder65_missing <- as.numeric(is.na(dta$pctunder65))
dta$pctfem_missing <- as.numeric(is.na(dta$pctfem))

##
# Get some basic counts for ourselves
##
n_providers <- length(unique(df$provider_id))
n_counties <- length(unique(df$provider_county_name))
n_zip_codes <- length(unique(df$provider_zip_code))
n_total_res_cases <- sum(df$total_res_cases)
n_total_res_deaths <- sum(df$total_res_deaths)
n_total_staff_cases <- sum(df$total_staff_cases)
n_total_staff_deaths <- sum(df$total_staff_deaths)

counts_df <- tibble(
  var=c(
    "providers",
    "counties",
    "zip_codes",
    "total_res_cases",
    "total_res_deaths",
    "total_staff_cases",
    "total_staff_deaths"
  ), 
  count=c(
    n_providers,
    n_counties,
    n_zip_codes,
    n_total_res_cases,
    n_total_res_deaths,
    n_total_staff_cases,
    n_total_staff_deaths
  )
)


##
# Making the comparison tables
##

#########################################################################################################################
#EXPOSURES
#########################################################################################################################

### Making categorical (ordinal and labeled) variables

#Ownership.Type - collapsing into 3 categories (ownr.type)
dta <- dta %>%
  mutate(ownr.type = ifelse(grepl("For profit", ownership_type), 1, 0))
dta$ownr.type[grepl("Non profit", dta$ownership_type)] <- 2
dta$ownr.type[grepl("Government", dta$ownership_type)] <- 3
dta$ownr.type <- factor(dta$ownr.type, levels=c(1,2,3), labels = c("For profit", "Non-profit", "Government"))


#Provider.Type
dta <- dta %>%
  mutate(pvdr.type = ifelse(provider_type=="Medicare", 1, 0))
dta$pvdr.type[dta$provider_type=="Medicaid"] <- 2
dta$pvdr.type[dta$provider_type=="Medicare and Medicaid"] <- 3
dta$pvdr.type <- factor(dta$pvdr.type, levels=c(1,2,3), labels = c("Medicare", "Medicaid", "Medicare and Medicaid"))


#########################################################################################################################=
#OUTCOMES
#########################################################################################################################

### Making these categorical to fit nicely in gtsummary

#total.staff.cases.p
dta <- dta %>%
  mutate(total.staff.cases.p.q = ntile(total_staff_cases_p, 2))
dta$total.staff.cases.p.q[is.na(dta$total.staff.cases.p)] <- 0
dta$total.staff.cases.p.q <- factor(dta$total.staff.cases.p.q, levels = c(0,1,2),
                                    labels = c("Missing", "Below median (% staff cases/resident)", "Above median (% staff cases/resident)"))

#Overall.Rating

#Health.Inspection.Rating

#pctfem - Making binary by the median (pctfem.b)
dta$pctfem <- as.numeric(dta$pctfem)
x <- median(dta$pctfem, na.rm=TRUE)
dta <- dta %>%
  mutate(pctfem.b = ifelse(pctfem>=x, 2,1))
dta$pctfem.b[is.na(dta$pctfem)] <- 0
dta$pctfem.b <- factor(dta$pctfem.b, levels = c(0,1,2),
                       labels = c("Missing", "Below median (% female)", "Above median (% female)"))

#pctblack into quartiles (pctblack.q)
dta$pctblack <- as.numeric(dta$pctblack)
dta <- dta %>%
  mutate(pctblack.q = ntile(pctblack, 4))
dta$pctblack.q[is.na(dta$pctblack.q)] <- 0
dta$pctblack.q <- factor(dta$pctblack.q, levels = c(0,1,2,3,4),
                         labels = c("Missing", "1st quartile (% black)", "2nd quartile (% black)",
                                    "3rd quartile (% black)", "4th quartile (% black)"))

#pctwhite into quartiles (pctwhite.q)
dta$pctwhite <- as.numeric(dta$pctwhite)
dta <- dta %>%
  mutate(pctwhite.q = ntile(pctwhite, 4))
dta$pctwhite.q[is.na(dta$pctwhite.q)] <- 0
dta$pctwhite.q <- factor(dta$pctwhite.q, levels = c(0,1,2,3,4),
                         labels = c("Missing", "1st quartile (% white)", "2nd quartile (% white)",
                                    "3rd quartile (% white)", "4th quartile (% white)"))

#pctunder65 into quartiles (pctunder65.q)
dta$pctunder65 <- as.numeric(dta$pctunder65)
dta <- dta %>%
  mutate(pctunder65.q = ntile(pctunder65, 4))
dta$pctunder65.q[is.na(dta$pctunder65.q)] <- 0
dta$pctunder65.q <- factor(dta$pctunder65.q, levels = c(0,1,2,3,4),
                           labels = c("Missing", "1st quartile (% under 65)", "2nd quartile (% under 65)",
                                      "3rd quartile (% under 65)", "4th quartile (% under 65)"))

#pcthighcfs
dta$pcthighcfs <- as.numeric(dta$pcthighcfs)
dta <- dta %>%
  mutate(pcthighcfs.q = ntile(pcthighcfs, 4))
dta$pcthighcfs.q[is.na(dta$pcthighcfs.q)] <- 0
dta$pcthighcfs.q <- factor(dta$pcthighcfs.q, levels = c(0,1,2,3,4),
                           labels = c("Missing", "1st quartile (% high CFS)", "2nd quartile (% high CFS)",
                                      "3rd quartile (% high CFS)", "4th quartile (% high CFS)"))

#total.res.cases.p
dta <- dta %>%
  mutate(total.res.cases.p.q = ntile(total_res_cases_p, 2))
dta$total.res.cases.p.q[is.na(dta$total.res.cases.p)] <- 0
dta$total.res.cases.p.q <- factor(dta$total.res.cases.p.q, levels = c(0,1,2),
                                  labels = c("Missing", "Below median (% cases/resident)", "Above median (% cases/resident)"))

#total.res.deaths.p
dta <- dta %>%
  mutate(total.res.deaths.p.q = ntile(total_res_deaths_p, 2))
dta$total.res.deaths.p.q[is.na(dta$total.res.deaths.p)] <- 0
dta$total.res.deaths.p.q <- factor(dta$total.res.deaths.p.q, levels = c(0,1,2),
                                   labels = c("Missing", "Below median (% deaths/resident)", "Above median (% deaths/resident)"))

#########################################################################################################################
#GTSUMMARY TABLE: CHARACTERISTICS WITH SIGNIFCANCE
#########################################################################################################################

dta_cat <- dta %>% select(ownr.type, pvdr.type, overall_rating, health_inspection_rating, 
                          pctfem.b, pctblack.q, pctwhite.q, pctunder65.q, pcthighcfs.q, 
                          total.staff.cases.p.q, 
                          total.res.deaths.p.q,
                          total.res.cases.p.q, pctblack_missing, pctwhite_missing, pctfem_missing, pcthighcfs_missing, pctunder65_missing)

tbl_summary(dta_cat, by=total.res.deaths.p.q) %>% add_p()
tbl_summary(dta_cat, by=total.res.cases.p.q) %>% add_p()

##
# Linear regression approach
#

# first, make status of above/below median an integer for logistic regression to calculate
dta$outcome <- as.numeric(as.integer(dta$total.res.cases.p.q) == 3)

##
# Feed-forward modeling
##

m1 <- glm(
  total_res_cases_p ~ avg_staff_vaccine_rate,
  data = dta
)

m2 <- glm(
  total_res_cases_p ~ avg_staff_vaccine_rate + health_inspection_rating,
  data = dta
)

m3 <- glm(
  total_res_cases_p ~ avg_staff_vaccine_rate + health_inspection_rating + C(pvdr.type, treatment),
  data = dta
)

m4 <- glm(
  total_res_cases_p ~ avg_staff_vaccine_rate + health_inspection_rating + C(pvdr.type, treatment) + C(ownr.type, treatment),
  data = dta
)


m5 <- glm(
  total_res_cases_p ~ avg_staff_vaccine_rate + health_inspection_rating + C(pvdr.type, treatment) + C(ownr.type, treatment) +
    pctwhite,
  data = dta
)


m6 <- glm(
  total_res_cases_p ~ avg_staff_vaccine_rate + health_inspection_rating + C(pvdr.type, treatment) + C(ownr.type, treatment) +
    pctwhite + pctfem,
  data = dta
)

m7 <- glm(
  total_res_cases_p ~ avg_staff_vaccine_rate + health_inspection_rating + C(pvdr.type, treatment) + C(ownr.type, treatment) +
    pctwhite + pctfem + pcthighcfs,
  data = dta
)

m8 <- glm(
  total_res_cases_p ~ avg_staff_vaccine_rate + health_inspection_rating + C(pvdr.type, treatment) + C(ownr.type, treatment) +
    pctwhite + pctfem + pcthighcfs + total_staff_cases_p,
  data = dta
)

m9 <- glm(
  total_res_cases_p ~ avg_staff_vaccine_rate + health_inspection_rating + C(pvdr.type, treatment) + C(ownr.type, treatment) +
    pctwhite + pctfem + pcthighcfs + total_staff_cases_p + factor(provider_county_name),
  data = dta
)

m10 <- glm(
  total_res_cases_p ~ avg_staff_vaccine_rate + health_inspection_rating + C(pvdr.type, treatment) + C(ownr.type, treatment) +
    pctwhite + pctfem + pcthighcfs + total_staff_cases_p + factor(provider_county_name) + avg_bed_occupancy_rate,
  data = dta
)

m11 <- glm(
  total_res_cases_p ~ avg_staff_vaccine_rate + health_inspection_rating + C(pvdr.type, treatment) + C(ownr.type, treatment) +
    pctwhite + pctfem + pcthighcfs + total_staff_cases_p + factor(provider_county_name) + avg_number_of_occupied_beds,
  data = dta
)

aictab(cand.set = list(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11))

r2 <- with(summary(m8), 1 - deviance/null.deviance)
print(r2)
print(summary(m8))

##
# Forest plot
##

pm <- plot_model(m8) + 
  ggtitle("Predicting Resident Cases\nLinear Regression Coefficients with 95% CI")
pm$data$term <- factor(pm$data$term)
pm
levels(pm$data$term) <-rev(c(
  "Avg Staff Vaccination", "Health Inspection Rating", "Medicaid-Only Provider\n(Ref=Medicare Only)", "Medicare and Medicaid Provider\n(Ref=Medicare Only)",
  "Non-Profit Ownership\n(Ref=For-Profit)", "Gov't Ownership\n(Ref=For-Profit)", "% White Residents", "% Female Residents", "% High Clinical Frailty", "Staff Case Rate" 
))
pm