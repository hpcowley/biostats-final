###############################################################################
# Biostatistics 652 Final Project
# Examine relationships between staffing, PPE, and COVID-19 outcomes
# December 2021
###############################################################################
library(dplyr)
library(writexl)
library(gtsummary)
library(abd)
library(dplyr)
library(ggplot2)

#read in data
base_data <- read.csv('...replace with path to folder.../Bios_140.651_Final/Data\\tmp\\nh_covid_md_base.csv')
base_data_yearlong <- read.csv('...replace with path to folder.../Bios_140.651_Final/Data\\raw\\faclevel_2021.csv')

#Base data filtering and cleaning

#filter to MD only
base_data_yearlong_MD <- base_data_yearlong %>% filter(Provider.State=='MD')

#keeping only columns of interest
base_data_yearlong_MD_interest <- base_data_yearlong_MD %>%
  select(Week.Ending,
         Federal.Provider.Number,
         Number.of.Residents.with.a.New.Positive.COVID.19.Test.Result,
         Number.of.Staff.and.or.Personnel.with.a.New.Positive.COVID.19.Test.Result,
         Residents.Weekly.Confirmed.COVID.19,
         Residents.Total.Confirmed.COVID.19,
         Residents.Weekly.All.Deaths,
         Residents.Total.All.Deaths,
         Weekly.Resident.Confirmed.COVID.19.Cases.Per.1.000.Residents,
         Weekly.Resident.COVID.19.Deaths.Per.1.000.Residents,
         Total.Resident.Confirmed.COVID.19.Cases.Per.1.000.Residents,
         Total.Resident.COVID.19.Deaths.Per.1.000.Residents,
         Total.Residents.COVID.19.Deaths.as.a.Percentage.of.Confirmed.COVID.19.Cases,
         Able.to.Test.or.Obtain.Resources.to.Test.All.Current.Residents.Within.Next.7.Days,
         Able.to.Test.or.Obtain.Resources.to.Test.All.Staff.and.or.Personnel.Within.Next.7.Days,
         COVID.19.Non.Point.of.Care.Tests.Performed.on.Residents.Since.Last.Report,
         COVID.19.Non.Point.of.Care.Tests.Performed.on.Residents.Since.Last.Report,
         COVID.19.Point.of.Care.Tests.Performed.on.Staff.and.or.Personnel.Since.Last.Report,
         COVID.19.Point.of.Care.Tests.Performed.on.Staff.and.or.Personnel.Since.Last.Report,
         Staff.Weekly.Confirmed.COVID.19,
         Staff.Total.Confirmed.COVID.19,
         Staff.Weekly.COVID.19.Deaths,
         Staff.Total.COVID.19.Deaths,
         Shortage.of.Nursing.Staff,
         Shortage.of.Clinical.Staff,
         Shortage.of.Aides,
         Shortage.of.Other.Staff,
         One.Week.Supply.of.N95.Masks,
         One.Week.Supply.of.Surgical.Masks,
         N95.Respirator.No.Longer.Available.in.7.Days,
         N95.Respirator.Strategy.for.Optimization,
         Face.Masks.No.Longer.Available.in.7.Days,
         Facility.Would.Like.Assistance.Outreach.for.Infection.Control.Outbreak.Management,
         Facility.Would.Like.Assistance.Outreach.for.Testing.Supply.Shortages,
         Facility.Would.Like.Assistance.Outreach.for.Staffing.Shortages,
         Facility.Would.Like.Assistance.Outreach.for.Personal.Protective.Equipment..PPE.,
         Percentage.of.Current.Residents.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time,
         Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time
         )


#summary stats
summary(base_data_yearlong_MD_interest$Number.of.Residents.with.a.New.Positive.COVID.19.Test.Result)



#base data stuff

base_omit <- na.omit(base_data_yearlong_MD_interest, na.rm = TRUE)


#recode Y to 1 and N to 0 for all cols
base_omit[base_omit=="Y"] <- 1
base_omit[base_omit=="N"] <- 0
base_omit$onepluscases <- as.numeric(base_omit$Residents.Weekly.Confirmed.COVID.19 > 0)
base_omit$onepluscases.staff <- as.numeric(base_omit$Number.of.Staff.and.or.Personnel.with.a.New.Positive.COVID.19.Test.Result > 1)
base_omit$twopluscases <- as.numeric(base_omit$Residents.Weekly.Confirmed.COVID.19 > 0)
base_omit$twopluscases.staff <- as.numeric(base_omit$Number.of.Staff.and.or.Personnel.with.a.New.Positive.COVID.19.Test.Result > 1)
base_omit$facemasks <- as.numeric(base_omit$Face.Masks.No.Longer.Available.in.7.Days > 0)
base_omit$ppe <- as.numeric(base_omit$Facility.Would.Like.Assistance.Outreach.for.Personal.Protective.Equipment..PPE. > 0)

#columns for strata - staff vax above 75%
base_omit$twopluscases_above <- as.numeric(base_omit$Residents.Weekly.Confirmed.COVID.19 > 0 &
                                             base_omit$Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time > 75)

base_omit$twopluscases.staff_above <- as.numeric(base_omit$Number.of.Staff.and.or.Personnel.with.a.New.Positive.COVID.19.Test.Result > 0 & 
                                                   base_omit$Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time > 75)

base_omit$onepluscases_above <- as.numeric(base_omit$Residents.Weekly.Confirmed.COVID.19 > 0 &
                                     base_omit$Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time > 75)

base_omit$onepluscases.staff_above <- as.numeric(base_omit$Number.of.Staff.and.or.Personnel.with.a.New.Positive.COVID.19.Test.Result > 0 & 
                                                 base_omit$Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time > 75)

#columns for strata - staff vax below 75%
base_omit$twopluscases.staff_below <- as.numeric(base_omit$Number.of.Staff.and.or.Personnel.with.a.New.Positive.COVID.19.Test.Result > 0 &
                                                   base_omit$Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time <= 75)

base_omit$twopluscases_below <- as.numeric(base_omit$Residents.Weekly.Confirmed.COVID.19 > 0 & 
                                             base_omit$Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time <= 75)

base_omit$onepluscases.staff_below <- as.numeric(base_omit$Number.of.Staff.and.or.Personnel.with.a.New.Positive.COVID.19.Test.Result > 0 &
                                                 base_omit$Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time <= 75)

base_omit$onepluscases_below <- as.numeric(base_omit$Residents.Weekly.Confirmed.COVID.19 > 0 & 
                                     base_omit$Percentage.of.Current.Healthcare.Personnel.who.Received.a.Completed.COVID.19.Vaccination.at.Any.Time <= 75)

#######################ODDS RATIOS#############################################################################################
#####OUTCOME=2+ RES CASES CRUDE

#shortage of nursing staff
short_nursing_table <- table(base_omit$Shortage.of.Nursing.Staff, base_omit$twopluscases)
short_nursing <- odds.ratio(short_nursing_table)
short_nursing_OR <- short_nursing$OR^-1
short_nursing_OR
short_nursing_upper <- short_nursing$upper^-1
short_nursing_lower <- short_nursing$lower^-1

#shortage of clinical staff
short_clinical_table <- table(base_omit$Shortage.of.Clinical.Staff, base_omit$twopluscases)
short_clinical_table

short_clinical <- odds.ratio(short_clinical_table)
short_clinical_OR <- short_clinical$OR^-1
short_clinical_OR
short_clinical_upper <- short_clinical$upper^-1
short_clinical_lower <- short_clinical$lower^-1

#shortage of aides
short_aides_table <- table(base_omit$Shortage.of.Aides, base_omit$twopluscases)
short_aides_table

short_aides <- odds.ratio(short_aides_table)
short_aides_OR <- short_aides$OR^-1
short_aides_OR
short_aides_upper <- short_aides$upper^-1
short_aides_lower <- short_aides$lower^-1

#shortage of other staff
short_other_table <- table(base_omit$Shortage.of.Other.Staff, base_omit$twopluscases)
short_other_table

short_other <- odds.ratio(short_other_table)
short_other_OR <- short_other$OR^-1
short_other_OR
short_other_upper <- short_other$upper^-1
short_other_lower <- short_other$lower^-1

#mask shortage
short_masks_table <- table(base_omit$facemasks, base_omit$twopluscases)
short_masks_table

short_masks <- odds.ratio(short_masks_table)
short_masks_OR <- short_masks$OR^-1
short_masks_OR
short_masks_upper <- short_masks$upper^-1
short_masks_lower <- short_masks$lower^-1

#PPE shortage
short_ppe_table <- table(base_omit$ppe, base_omit$twopluscases)
short_ppe_table

short_ppe <- odds.ratio(short_ppe_table)
short_ppe_OR <- short_ppe$OR^-1
short_ppe_OR
short_ppe_upper <- short_ppe$upper^-1
short_ppe_lower <- short_ppe$lower^-1

#summarized table
exposure <- c('Nursing Shortage',
              'Clinical Shortage',
              'Aides Shortage',
              'Other Staff Shortage',
              'Masks Shortage',
              'PPE Shortage')

OR <- c(short_nursing_OR,
        short_clinical_OR,
        short_aides_OR,
        short_other_OR,
        short_masks_OR,
        short_ppe_OR)

upper_CI <- c(short_nursing_lower,
              short_clinical_lower,
              short_aides_lower,
              short_other_lower,
              short_masks_lower,
              short_ppe_lower)

lower_CI <- c(short_nursing_upper,
              short_clinical_upper,
              short_aides_upper,
              short_other_upper,
              short_masks_upper,
              short_ppe_upper)


crude_OR_table_res_greaterthan2 <- data.frame(exposure, OR, lower_CI, upper_CI)


#
#####OUTCOME=RES 2+ CASES STRATIFIED BY STAFF VAX ABOVE 75%

#resetting variable names
remove(exposure, OR, upper_CI, lower_CI)


#shortage of nursing staff
short_nursing_table <- table(base_omit$Shortage.of.Nursing.Staff, base_omit$twopluscases_above)
short_nursing <- odds.ratio(short_nursing_table)
short_nursing_OR <- short_nursing$OR^-1
short_nursing_OR
short_nursing_upper <- short_nursing$upper^-1
short_nursing_lower <- short_nursing$lower^-1

#shortage of clinical staff
short_clinical_table <- table(base_omit$Shortage.of.Clinical.Staff, base_omit$twopluscases_above)
short_clinical_table

short_clinical <- odds.ratio(short_clinical_table)
short_clinical_OR <- short_clinical$OR^-1
short_clinical_OR
short_clinical_upper <- short_clinical$upper^-1
short_clinical_lower <- short_clinical$lower^-1

#shortage of aides
short_aides_table <- table(base_omit$Shortage.of.Aides, base_omit$twopluscases_above)
short_aides_table

short_aides <- odds.ratio(short_aides_table)
short_aides_OR <- short_aides$OR^-1
short_aides_OR
short_aides_upper <- short_aides$upper^-1
short_aides_lower <- short_aides$lower^-1

#shortage of other staff
short_other_table <- table(base_omit$Shortage.of.Other.Staff, base_omit$twopluscases_above)
short_other_table

short_other <- odds.ratio(short_other_table)
short_other_OR <- short_other$OR^-1
short_other_OR
short_other_upper <- short_other$upper^-1
short_other_lower <- short_other$lower^-1

#mask shortage
short_masks_table <- table(base_omit$facemasks, base_omit$twopluscases_above)
short_masks_table

short_masks <- odds.ratio(short_masks_table)
short_masks_OR <- short_masks$OR^-1
short_masks_OR
short_masks_upper <- short_masks$upper^-1
short_masks_lower <- short_masks$lower^-1

#PPE shortage
short_ppe_table <- table(base_omit$ppe, base_omit$twopluscases_above)
short_ppe_table

short_ppe <- odds.ratio(short_ppe_table)
short_ppe_OR <- short_ppe$OR^-1
short_ppe_OR
short_ppe_upper <- short_ppe$upper^-1
short_ppe_lower <- short_ppe$lower^-1

#summarized table
exposure <- c('   Nursing Shortage (Above)',
              '   Clinical Shortage (Above)',
              '   Aides Shortage (Above)',
              '   Other Staff Shortage (Above)',
              '   Masks Shortage (Above)',
              '   PPE Shortage (Above)')

OR <- c(short_nursing_OR,
        short_clinical_OR,
        short_aides_OR,
        short_other_OR,
        short_masks_OR,
        short_ppe_OR)

upper_CI <- c(short_nursing_lower,
              short_clinical_lower,
              short_aides_lower,
              short_other_lower,
              short_masks_lower,
              short_ppe_lower)

lower_CI <- c(short_nursing_upper,
              short_clinical_upper,
              short_aides_upper,
              short_other_upper,
              short_masks_upper,
              short_ppe_upper)


above_OR_table_res_greaterthan2 <- data.frame(exposure, OR, lower_CI, upper_CI)


#####OUTCOME= 2+RES CASES STRATIFIED BY STAFF VAX below 75%

#shortage of nursing staff
short_nursing_table <- table(base_omit$Shortage.of.Nursing.Staff, base_omit$twopluscases_below)
short_nursing <- odds.ratio(short_nursing_table)
short_nursing_OR <- short_nursing$OR^-1
short_nursing_OR
short_nursing_upper <- short_nursing$upper^-1
short_nursing_lower <- short_nursing$lower^-1

#shortage of clinical staff
short_clinical_table <- table(base_omit$Shortage.of.Clinical.Staff, base_omit$twopluscases_below)
short_clinical_table

short_clinical <- odds.ratio(short_clinical_table)
short_clinical_OR <- short_clinical$OR^-1
short_clinical_OR
short_clinical_upper <- short_clinical$upper^-1
short_clinical_lower <- short_clinical$lower^-1

#shortage of aides
short_aides_table <- table(base_omit$Shortage.of.Aides, base_omit$twopluscases_below)
short_aides_table

short_aides <- odds.ratio(short_aides_table)
short_aides_OR <- short_aides$OR^-1
short_aides_OR
short_aides_upper <- short_aides$upper^-1
short_aides_lower <- short_aides$lower^-1

#shortage of other staff
short_other_table <- table(base_omit$Shortage.of.Other.Staff, base_omit$twopluscases_below)
short_other_table

short_other <- odds.ratio(short_other_table)
short_other_OR <- short_other$OR^-1
short_other_OR
short_other_upper <- short_other$upper^-1
short_other_lower <- short_other$lower^-1

#mask shortage
short_masks_table <- table(base_omit$facemasks, base_omit$twopluscases_below)
short_masks_table

short_masks <- odds.ratio(short_masks_table)
short_masks_OR <- short_masks$OR^-1
short_masks_OR
short_masks_upper <- short_masks$upper^-1
short_masks_lower <- short_masks$lower^-1

#PPE shortage
short_ppe_table <- table(base_omit$ppe, base_omit$twopluscases_below)
short_ppe_table

short_ppe <- odds.ratio(short_ppe_table)
short_ppe_OR <- short_ppe$OR^-1
short_ppe_OR
short_ppe_upper <- short_ppe$upper^-1
short_ppe_lower <- short_ppe$lower^-1

#summarized table
exposure <- c('   Nursing Shortage (Below)',
              '   Clinical Shortage (Below)',
              '   Aides Shortage (Below)',
              '   Other Staff Shortage (Below)',
              '   Masks Shortage (Below)',
              '   PPE Shortage (Below)')

OR <- c(short_nursing_OR,
        short_clinical_OR,
        short_aides_OR,
        short_other_OR,
        short_masks_OR,
        short_ppe_OR)

upper_CI <- c(short_nursing_lower,
              short_clinical_lower,
              short_aides_lower,
              short_other_lower,
              short_masks_lower,
              short_ppe_lower)

lower_CI <- c(short_nursing_upper,
              short_clinical_upper,
              short_aides_upper,
              short_other_upper,
              short_masks_upper,
              short_ppe_upper)


below_OR_table_res_greaterthan2 <- data.frame(exposure, OR, lower_CI, upper_CI)


#######################################################OUTCOME = 1 OR MORE RES CASES CRUDE

#shortage of nursing staff
short_nursing_table_1 <- table(base_omit$Shortage.of.Nursing.Staff, base_omit$onepluscases)
short_nursing_table_1

short_nursing <- odds.ratio(short_nursing_table_1)
short_nursing_OR <- short_nursing$OR^-1
short_nursing_OR
short_nursing_upper <- short_nursing$upper^-1
short_nursing_lower <- short_nursing$lower^-1

#shortage of clinical staff
short_clinical_table_1 <- table(base_omit$Shortage.of.Clinical.Staff, base_omit$onepluscases)
short_clinical_table_1

short_clinical <- odds.ratio(short_clinical_table_1)
short_clinical_OR <- short_clinical$OR^-1
short_clinical_OR
short_clinical_upper <- short_clinical$upper^-1
short_clinical_lower <- short_clinical$lower^-1

#shortage of aides
short_aides_table_1 <- table(base_omit$Shortage.of.Aides, base_omit$onepluscases)
short_aides_table_1

short_aides <- odds.ratio(short_aides_table_1)
short_aides_OR <- short_aides$OR^-1
short_aides_OR
short_aides_upper <- short_aides$upper^-1
short_aides_lower <- short_aides$lower^-1

#shortage of other staff
short_other_table_1 <- table(base_omit$Shortage.of.Other.Staff, base_omit$onepluscases)
short_other_table_1

short_other <- odds.ratio(short_other_table_1)
short_other_OR <- short_other$OR^-1
short_other_OR
short_other_upper <- short_other$upper^-1
short_other_lower <- short_other$lower^-1

#mask shortage
short_masks_table_1 <- table(base_omit$facemasks, base_omit$onepluscases)
short_masks_table_1


short_masks <- odds.ratio(short_masks_table_1)
short_masks_OR <- short_masks$OR^-1
short_masks_OR
short_masks_upper <- short_masks$upper^-1
short_masks_lower <- short_masks$lower^-1

#PPE shortage
short_ppe_table_1 <- table(base_omit$ppe, base_omit$onepluscases)
short_ppe_table_1

short_ppe <- odds.ratio(short_ppe_table_1)
short_ppe_OR <- short_ppe$OR^-1
short_ppe_OR
short_ppe_upper <- short_ppe$upper^-1
short_ppe_lower <- short_ppe$lower^-1

#summarized table
exposure <- c('Nursing Shortage',
              'Clinical Shortage',
              'Aides Shortage',
              'Other Staff Shortage',
              'Masks Shortage',
              'PPE Shortage')

OR <- c(short_nursing_OR,
        short_clinical_OR,
        short_aides_OR,
        short_other_OR,
        short_masks_OR,
        short_ppe_OR)

upper_CI <- c(short_nursing_lower,
              short_clinical_lower,
              short_aides_lower,
              short_other_lower,
              short_masks_lower,
              short_ppe_lower)

lower_CI <- c(short_nursing_upper,
              short_clinical_upper,
              short_aides_upper,
              short_other_upper,
              short_masks_upper,
              short_ppe_upper)


crude_OR_table_res_1ormore <- data.frame(exposure, OR, lower_CI, upper_CI)

#######################################################OUTCOME = STRATIFIED 1 OR MORE RES CASES ABOVE

#shortage of nursing staff
short_nursing_table_1 <- table(base_omit$Shortage.of.Nursing.Staff, base_omit$onepluscases_above)
short_nursing_table_1

short_nursing <- odds.ratio(short_nursing_table_1)
short_nursing_OR <- short_nursing$OR^-1
short_nursing_OR
short_nursing_upper <- short_nursing$upper^-1
short_nursing_lower <- short_nursing$lower^-1

#shortage of clinical staff
short_clinical_table_1 <- table(base_omit$Shortage.of.Clinical.Staff, base_omit$onepluscases_above)
short_clinical_table_1

short_clinical <- odds.ratio(short_clinical_table_1)
short_clinical_OR <- short_clinical$OR^-1
short_clinical_OR
short_clinical_upper <- short_clinical$upper^-1
short_clinical_lower <- short_clinical$lower^-1

#shortage of aides
short_aides_table_1 <- table(base_omit$Shortage.of.Aides, base_omit$onepluscases_above)
short_aides_table_1

short_aides <- odds.ratio(short_aides_table_1)
short_aides_OR <- short_aides$OR^-1
short_aides_OR
short_aides_upper <- short_aides$upper^-1
short_aides_lower <- short_aides$lower^-1

#shortage of other staff
short_other_table_1 <- table(base_omit$Shortage.of.Other.Staff, base_omit$onepluscases_above)
short_other_table_1

short_other <- odds.ratio(short_other_table_1)
short_other_OR <- short_other$OR^-1
short_other_OR
short_other_upper <- short_other$upper^-1
short_other_lower <- short_other$lower^-1

#mask shortage
short_masks_table_1 <- table(base_omit$facemasks, base_omit$onepluscases_above)
short_masks_table_1


short_masks <- odds.ratio(short_masks_table_1)
short_masks_OR <- short_masks$OR^-1
short_masks_OR
short_masks_upper <- short_masks$upper^-1
short_masks_lower <- short_masks$lower^-1

#PPE shortage
short_ppe_table_1 <- table(base_omit$ppe, base_omit$onepluscases_above)
short_ppe_table_1

short_ppe <- odds.ratio(short_ppe_table_1)
short_ppe_OR <- short_ppe$OR^-1
short_ppe_OR
short_ppe_upper <- short_ppe$upper^-1
short_ppe_lower <- short_ppe$lower^-1

#summarized table
exposure <- c('Nursing Shortage (Above)',
              'Clinical Shortage (Above)',
              'Aides Shortage (Above)',
              'Other Staff Shortage (Above)',
              'Masks Shortage (Above)',
              'PPE Shortage (Above)')

OR <- c(short_nursing_OR,
        short_clinical_OR,
        short_aides_OR,
        short_other_OR,
        short_masks_OR,
        short_ppe_OR)

upper_CI <- c(short_nursing_lower,
              short_clinical_lower,
              short_aides_lower,
              short_other_lower,
              short_masks_lower,
              short_ppe_lower)

lower_CI <- c(short_nursing_upper,
              short_clinical_upper,
              short_aides_upper,
              short_other_upper,
              short_masks_upper,
              short_ppe_upper)


above_OR_table_res_1ormore <- data.frame(exposure, OR, lower_CI, upper_CI)


#######################################################OUTCOME = STRATIFIED 1 OR MORE RES CASES below

#shortage of nursing staff
short_nursing_table_1 <- table(base_omit$Shortage.of.Nursing.Staff, base_omit$onepluscases_below)
short_nursing_table_1

short_nursing <- odds.ratio(short_nursing_table_1)
short_nursing_OR <- short_nursing$OR^-1
short_nursing_OR
short_nursing_upper <- short_nursing$upper^-1
short_nursing_lower <- short_nursing$lower^-1

#shortage of clinical staff
short_clinical_table_1 <- table(base_omit$Shortage.of.Clinical.Staff, base_omit$onepluscases_below)
short_clinical_table_1

short_clinical <- odds.ratio(short_clinical_table_1)
short_clinical_OR <- short_clinical$OR^-1
short_clinical_OR
short_clinical_upper <- short_clinical$upper^-1
short_clinical_lower <- short_clinical$lower^-1

#shortage of aides
short_aides_table_1 <- table(base_omit$Shortage.of.Aides, base_omit$onepluscases_below)
short_aides_table_1

short_aides <- odds.ratio(short_aides_table_1)
short_aides_OR <- short_aides$OR^-1
short_aides_OR
short_aides_upper <- short_aides$upper^-1
short_aides_lower <- short_aides$lower^-1

#shortage of other staff
short_other_table_1 <- table(base_omit$Shortage.of.Other.Staff, base_omit$onepluscases_below)
short_other_table_1

short_other <- odds.ratio(short_other_table_1)
short_other_OR <- short_other$OR^-1
short_other_OR
short_other_upper <- short_other$upper^-1
short_other_lower <- short_other$lower^-1

#mask shortage
short_masks_table_1 <- table(base_omit$facemasks, base_omit$onepluscases_below)
short_masks_table_1


short_masks <- odds.ratio(short_masks_table_1)
short_masks_OR <- short_masks$OR^-1
short_masks_OR
short_masks_upper <- short_masks$upper^-1
short_masks_lower <- short_masks$lower^-1

#PPE shortage
short_ppe_table_1 <- table(base_omit$ppe, base_omit$onepluscases_below)
short_ppe_table_1

short_ppe <- odds.ratio(short_ppe_table_1)
short_ppe_OR <- short_ppe$OR^-1
short_ppe_OR
short_ppe_upper <- short_ppe$upper^-1
short_ppe_lower <- short_ppe$lower^-1

#summarized table
exposure <- c('Nursing Shortage (Below)',
              'Clinical Shortage (Below)',
              'Aides Shortage (Below)',
              'Other Staff Shortage (Below)',
              'Masks Shortage (Below)',
              'PPE Shortage (Below)')

OR <- c(short_nursing_OR,
        short_clinical_OR,
        short_aides_OR,
        short_other_OR,
        short_masks_OR,
        short_ppe_OR)

upper_CI <- c(short_nursing_lower,
              short_clinical_lower,
              short_aides_lower,
              short_other_lower,
              short_masks_lower,
              short_ppe_lower)

lower_CI <- c(short_nursing_upper,
              short_clinical_upper,
              short_aides_upper,
              short_other_upper,
              short_masks_upper,
              short_ppe_upper)


below_OR_table_res_1ormore <- data.frame(exposure, OR, lower_CI, upper_CI)


#####OUTCOME=2+ STAFF CASES CRUDE #############################################################

#shortage of nursing staff
short_nursing_table <- table(base_omit$Shortage.of.Nursing.Staff, base_omit$twopluscases.staff)
short_nursing_table

short_nursing <- odds.ratio(short_nursing_table)
short_nursing_OR <- short_nursing$OR^-1
short_nursing_OR
short_nursing_upper <- short_nursing$upper^-1
short_nursing_lower <- short_nursing$lower^-1

#shortage of clinical staff
short_clinical_table <- table(base_omit$Shortage.of.Clinical.Staff, base_omit$twopluscases.staff)
short_clinical_table

short_clinical <- odds.ratio(short_clinical_table)
short_clinical_OR <- short_clinical$OR^-1
short_clinical_OR
short_clinical_upper <- short_clinical$upper^-1
short_clinical_lower <- short_clinical$lower^-1

#shortage of aides
short_aides_table <- table(base_omit$Shortage.of.Aides, base_omit$twopluscases.staff)
short_aides_table

short_aides <- odds.ratio(short_aides_table)
short_aides_OR <- short_aides$OR^-1
short_aides_OR
short_aides_upper <- short_aides$upper^-1
short_aides_lower <- short_aides$lower^-1

#shortage of other staff
short_other_table <- table(base_omit$Shortage.of.Other.Staff, base_omit$twopluscases.staff)
short_other_table

short_other <- odds.ratio(short_other_table)
short_other_OR <- short_other$OR^-1
short_other_OR
short_other_upper <- short_other$upper^-1
short_other_lower <- short_other$lower^-1

#mask shortage
short_masks_table <- table(base_omit$facemasks, base_omit$twopluscases.staff)
short_masks_table

short_masks <- odds.ratio(short_masks_table)
short_masks_OR <- short_masks$OR^-1
short_masks_OR
short_masks_upper <- short_masks$upper^-1
short_masks_lower <- short_masks$lower^-1

#PPE shortage
short_ppe_table <- table(base_omit$ppe, base_omit$twopluscases.staff)
short_ppe_table

short_ppe <- odds.ratio(short_ppe_table)
short_ppe_OR <- short_ppe$OR^-1
short_ppe_OR
short_ppe_upper <- short_ppe$upper^-1
short_ppe_lower <- short_ppe$lower^-1


#summarized table
exposure <- c('Nursing Shortage',
              'Clinical Shortage',
              'Aides Shortage',
              'Other Staff Shortage',
              'Masks Shortage',
              'PPE Shortage')

OR <- c(short_nursing_OR,
        short_clinical_OR,
        short_aides_OR,
        short_other_OR,
        short_masks_OR,
        short_ppe_OR)

upper_CI <- c(short_nursing_lower,
              short_clinical_lower,
              short_aides_lower,
              short_other_lower,
              short_masks_lower,
              short_ppe_lower)

lower_CI <- c(short_nursing_upper,
              short_clinical_upper,
              short_aides_upper,
              short_other_upper,
              short_masks_upper,
              short_ppe_upper)


crude_OR_table_staff_greaterthan2 <- data.frame(exposure, OR, lower_CI, upper_CI)


#####OUTCOME=2+ STAFF CASES ABOVE #############################################################

#shortage of nursing staff
short_nursing_table <- table(base_omit$Shortage.of.Nursing.Staff, base_omit$twopluscases.staff_above)
short_nursing_table

short_nursing <- odds.ratio(short_nursing_table)
short_nursing_OR <- short_nursing$OR^-1
short_nursing_OR
short_nursing_upper <- short_nursing$upper^-1
short_nursing_lower <- short_nursing$lower^-1

#shortage of clinical staff
short_clinical_table <- table(base_omit$Shortage.of.Clinical.Staff, base_omit$twopluscases.staff_above)
short_clinical_table

short_clinical <- odds.ratio(short_clinical_table)
short_clinical_OR <- short_clinical$OR^-1
short_clinical_OR
short_clinical_upper <- short_clinical$upper^-1
short_clinical_lower <- short_clinical$lower^-1

#shortage of aides
short_aides_table <- table(base_omit$Shortage.of.Aides, base_omit$twopluscases.staff_above)
short_aides_table

short_aides <- odds.ratio(short_aides_table)
short_aides_OR <- short_aides$OR^-1
short_aides_OR
short_aides_upper <- short_aides$upper^-1
short_aides_lower <- short_aides$lower^-1

#shortage of other staff
short_other_table <- table(base_omit$Shortage.of.Other.Staff, base_omit$twopluscases.staff_above)
short_other_table

short_other <- odds.ratio(short_other_table)
short_other_OR <- short_other$OR^-1
short_other_OR
short_other_upper <- short_other$upper^-1
short_other_lower <- short_other$lower^-1

#mask shortage
short_masks_table <- table(base_omit$facemasks, base_omit$twopluscases.staff_above)
short_masks_table

short_masks <- odds.ratio(short_masks_table)
short_masks_OR <- short_masks$OR^-1
short_masks_OR
short_masks_upper <- short_masks$upper^-1
short_masks_lower <- short_masks$lower^-1

#PPE shortage
short_ppe_table <- table(base_omit$ppe, base_omit$twopluscases.staff_above)
short_ppe_table

short_ppe <- odds.ratio(short_ppe_table)
short_ppe_OR <- short_ppe$OR^-1
short_ppe_OR
short_ppe_upper <- short_ppe$upper^-1
short_ppe_lower <- short_ppe$lower^-1


#summarized table
exposure <- c('   Nursing Shortage (Above)',
              '   Clinical Shortage (Above)',
              '   Aides Shortage (Above)',
              '   Other Staff Shortage (Above)',
              '   Masks Shortage (Above)',
              '   PPE Shortage (Above)')

OR <- c(short_nursing_OR,
        short_clinical_OR,
        short_aides_OR,
        short_other_OR,
        short_masks_OR,
        short_ppe_OR)

upper_CI <- c(short_nursing_lower,
              short_clinical_lower,
              short_aides_lower,
              short_other_lower,
              short_masks_lower,
              short_ppe_lower)

lower_CI <- c(short_nursing_upper,
              short_clinical_upper,
              short_aides_upper,
              short_other_upper,
              short_masks_upper,
              short_ppe_upper)


above_OR_table_staff_greaterthan2 <- data.frame(exposure, OR, lower_CI, upper_CI)



#####OUTCOME=2+ STAFF CASES below #############################################################

#shortage of nursing staff
short_nursing_table <- table(base_omit$Shortage.of.Nursing.Staff, base_omit$twopluscases.staff_below)
short_nursing_table

short_nursing <- odds.ratio(short_nursing_table)
short_nursing_OR <- short_nursing$OR^-1
short_nursing_OR
short_nursing_upper <- short_nursing$upper^-1
short_nursing_lower <- short_nursing$lower^-1

#shortage of clinical staff
short_clinical_table <- table(base_omit$Shortage.of.Clinical.Staff, base_omit$twopluscases.staff_below)
short_clinical_table

short_clinical <- odds.ratio(short_clinical_table)
short_clinical_OR <- short_clinical$OR^-1
short_clinical_OR
short_clinical_upper <- short_clinical$upper^-1
short_clinical_lower <- short_clinical$lower^-1

#shortage of aides
short_aides_table <- table(base_omit$Shortage.of.Aides, base_omit$twopluscases.staff_below)
short_aides_table

short_aides <- odds.ratio(short_aides_table)
short_aides_OR <- short_aides$OR^-1
short_aides_OR
short_aides_upper <- short_aides$upper^-1
short_aides_lower <- short_aides$lower^-1

#shortage of other staff
short_other_table <- table(base_omit$Shortage.of.Other.Staff, base_omit$twopluscases.staff_below)
short_other_table

short_other <- odds.ratio(short_other_table)
short_other_OR <- short_other$OR^-1
short_other_OR
short_other_upper <- short_other$upper^-1
short_other_lower <- short_other$lower^-1

#mask shortage
short_masks_table <- table(base_omit$facemasks, base_omit$twopluscases.staff_below)
short_masks_table

short_masks <- odds.ratio(short_masks_table)
short_masks_OR <- short_masks$OR^-1
short_masks_OR
short_masks_upper <- short_masks$upper^-1
short_masks_lower <- short_masks$lower^-1

#PPE shortage
short_ppe_table <- table(base_omit$ppe, base_omit$twopluscases.staff_below)
short_ppe_table

short_ppe <- odds.ratio(short_ppe_table)
short_ppe_OR <- short_ppe$OR^-1
short_ppe_OR
short_ppe_upper <- short_ppe$upper^-1
short_ppe_lower <- short_ppe$lower^-1


#summarized table
exposure <- c('   Nursing Shortage (Below)',
              '   Clinical Shortage (Below)',
              '   Aides Shortage (Below)',
              '   Other Staff Shortage (Below)',
              '   Masks Shortage (Below)',
              '   PPE Shortage (Below)')

OR <- c(short_nursing_OR,
        short_clinical_OR,
        short_aides_OR,
        short_other_OR,
        short_masks_OR,
        short_ppe_OR)

upper_CI <- c(short_nursing_lower,
              short_clinical_lower,
              short_aides_lower,
              short_other_lower,
              short_masks_lower,
              short_ppe_lower)

lower_CI <- c(short_nursing_upper,
              short_clinical_upper,
              short_aides_upper,
              short_other_upper,
              short_masks_upper,
              short_ppe_upper)


below_OR_table_staff_greaterthan2 <- data.frame(exposure, OR, lower_CI, upper_CI)



#######################################################OUTCOME = ONE OR MORE STAFF CASES CRUDE
#shortage of nursing staff
short_nursing_table_1 <- table(base_omit$Shortage.of.Nursing.Staff, base_omit$onepluscases.staff)
short_nursing_table_1

short_nursing <- odds.ratio(short_nursing_table_1)
short_nursing_OR <- short_nursing$OR^-1
short_nursing_OR
short_nursing_upper <- short_nursing$upper^-1
short_nursing_lower <- short_nursing$lower^-1

#shortage of clinical staff
short_clinical_table_1 <- table(base_omit$Shortage.of.Clinical.Staff, base_omit$onepluscases.staff)
short_clinical_table_1

short_clinical <- odds.ratio(short_clinical_table_1)
short_clinical_OR <- short_clinical$OR^-1
short_clinical_OR
short_clinical_upper <- short_clinical$upper^-1
short_clinical_lower <- short_clinical$lower^-1

#shortage of aides
short_aides_table_1 <- table(base_omit$Shortage.of.Aides, base_omit$onepluscases.staff)
short_aides_table_1

short_aides <- odds.ratio(short_aides_table_1)
short_aides_OR <- short_aides$OR^-1
short_aides_OR
short_aides_upper <- short_aides$upper^-1
short_aides_lower <- short_aides$lower^-1

#shortage of other staff
short_other_table_1 <- table(base_omit$Shortage.of.Other.Staff, base_omit$onepluscases.staff)
short_other_table_1

short_other <- odds.ratio(short_other_table_1)
short_other_OR <- short_other$OR^-1
short_other_OR
short_other_upper <- short_other$upper^-1
short_other_lower <- short_other$lower^-1

#mask shortage
short_masks_table_1 <- table(base_omit$facemasks, base_omit$onepluscases.staff)
short_masks_table_1

short_masks <- odds.ratio(short_masks_table_1)
short_masks_OR <- short_masks$OR^-1
short_masks_OR
short_masks_upper <- short_masks$upper^-1
short_masks_lower <- short_masks$lower^-1

#PPE shortage
short_ppe_table_1 <- table(base_omit$ppe, base_omit$onepluscases.staff)
short_ppe_table_1

short_ppe <- odds.ratio(short_ppe_table_1)
short_ppe_OR <- short_ppe$OR^-1
short_ppe_OR
short_ppe_upper <- short_ppe$upper^-1
short_ppe_lower <- short_ppe$lower^-1

#summarized table
exposure <- c('Nursing Shortage',
              'Clinical Shortage',
              'Aides Shortage',
              'Other Staff Shortage',
              'Masks Shortage',
              'PPE Shortage')

OR <- c(short_nursing_OR,
        short_clinical_OR,
        short_aides_OR,
        short_other_OR,
        short_masks_OR,
        short_ppe_OR)

upper_CI <- c(short_nursing_lower,
              short_clinical_lower,
              short_aides_lower,
              short_other_lower,
              short_masks_lower,
              short_ppe_lower)

lower_CI <- c(short_nursing_upper,
              short_clinical_upper,
              short_aides_upper,
              short_other_upper,
              short_masks_upper,
              short_ppe_upper)


crude_OR_table_staff_1ormore <- data.frame(exposure, OR, lower_CI, upper_CI)






#######################################################OUTCOME = ONE OR MORE STAFF CASES above
#shortage of nursing staff
short_nursing_table_1 <- table(base_omit$Shortage.of.Nursing.Staff, base_omit$onepluscases.staff_above)
short_nursing_table_1

short_nursing <- odds.ratio(short_nursing_table_1)
short_nursing_OR <- short_nursing$OR^-1
short_nursing_OR
short_nursing_upper <- short_nursing$upper^-1
short_nursing_lower <- short_nursing$lower^-1

#shortage of clinical staff
short_clinical_table_1 <- table(base_omit$Shortage.of.Clinical.Staff, base_omit$onepluscases.staff_above)
short_clinical_table_1

short_clinical <- odds.ratio(short_clinical_table_1)
short_clinical_OR <- short_clinical$OR^-1
short_clinical_OR
short_clinical_upper <- short_clinical$upper^-1
short_clinical_lower <- short_clinical$lower^-1

#shortage of aides
short_aides_table_1 <- table(base_omit$Shortage.of.Aides, base_omit$onepluscases.staff_above)
short_aides_table_1

short_aides <- odds.ratio(short_aides_table_1)
short_aides_OR <- short_aides$OR^-1
short_aides_OR
short_aides_upper <- short_aides$upper^-1
short_aides_lower <- short_aides$lower^-1

#shortage of other staff
short_other_table_1 <- table(base_omit$Shortage.of.Other.Staff, base_omit$onepluscases.staff_above)
short_other_table_1

short_other <- odds.ratio(short_other_table_1)
short_other_OR <- short_other$OR^-1
short_other_OR
short_other_upper <- short_other$upper^-1
short_other_lower <- short_other$lower^-1

#mask shortage
short_masks_table_1 <- table(base_omit$facemasks, base_omit$onepluscases.staff_above)
short_masks_table_1

short_masks <- odds.ratio(short_masks_table_1)
short_masks_OR <- short_masks$OR^-1
short_masks_OR
short_masks_upper <- short_masks$upper^-1
short_masks_lower <- short_masks$lower^-1

#PPE shortage
short_ppe_table_1 <- table(base_omit$ppe, base_omit$onepluscases.staff_above)
short_ppe_table_1

short_ppe <- odds.ratio(short_ppe_table_1)
short_ppe_OR <- short_ppe$OR^-1
short_ppe_OR
short_ppe_upper <- short_ppe$upper^-1
short_ppe_lower <- short_ppe$lower^-1

#summarized table
exposure <- c('Nursing Shortage (Above)',
              'Clinical Shortage (Above)',
              'Aides Shortage (Above)',
              'Other Staff Shortage (Above)',
              'Masks Shortage (Above)',
              'PPE Shortage (Above)')

OR <- c(short_nursing_OR,
        short_clinical_OR,
        short_aides_OR,
        short_other_OR,
        short_masks_OR,
        short_ppe_OR)

upper_CI <- c(short_nursing_lower,
              short_clinical_lower,
              short_aides_lower,
              short_other_lower,
              short_masks_lower,
              short_ppe_lower)

lower_CI <- c(short_nursing_upper,
              short_clinical_upper,
              short_aides_upper,
              short_other_upper,
              short_masks_upper,
              short_ppe_upper)


above_OR_table_staff_1ormore <- data.frame(exposure, OR, lower_CI, upper_CI)


#######################################################OUTCOME = ONE OR MORE STAFF CASES below
#shortage of nursing staff
short_nursing_table_1 <- table(base_omit$Shortage.of.Nursing.Staff, base_omit$onepluscases.staff_below)
short_nursing_table_1

short_nursing <- odds.ratio(short_nursing_table_1)
short_nursing_OR <- short_nursing$OR^-1
short_nursing_OR
short_nursing_upper <- short_nursing$upper^-1
short_nursing_lower <- short_nursing$lower^-1

#shortage of clinical staff
short_clinical_table_1 <- table(base_omit$Shortage.of.Clinical.Staff, base_omit$onepluscases.staff_below)
short_clinical_table_1

short_clinical <- odds.ratio(short_clinical_table_1)
short_clinical_OR <- short_clinical$OR^-1
short_clinical_OR
short_clinical_upper <- short_clinical$upper^-1
short_clinical_lower <- short_clinical$lower^-1

#shortage of aides
short_aides_table_1 <- table(base_omit$Shortage.of.Aides, base_omit$onepluscases.staff_below)
short_aides_table_1

short_aides <- odds.ratio(short_aides_table_1)
short_aides_OR <- short_aides$OR^-1
short_aides_OR
short_aides_upper <- short_aides$upper^-1
short_aides_lower <- short_aides$lower^-1

#shortage of other staff
short_other_table_1 <- table(base_omit$Shortage.of.Other.Staff, base_omit$onepluscases.staff_below)
short_other_table_1

short_other <- odds.ratio(short_other_table_1)
short_other_OR <- short_other$OR^-1
short_other_OR
short_other_upper <- short_other$upper^-1
short_other_lower <- short_other$lower^-1

#mask shortage
short_masks_table_1 <- table(base_omit$facemasks, base_omit$onepluscases.staff_below)
short_masks_table_1

short_masks <- odds.ratio(short_masks_table_1)
short_masks_OR <- short_masks$OR^-1
short_masks_OR
short_masks_upper <- short_masks$upper^-1
short_masks_lower <- short_masks$lower^-1

#PPE shortage
short_ppe_table_1 <- table(base_omit$ppe, base_omit$onepluscases.staff_below)
short_ppe_table_1

short_ppe <- odds.ratio(short_ppe_table_1)
short_ppe_OR <- short_ppe$OR^-1
short_ppe_OR
short_ppe_upper <- short_ppe$upper^-1
short_ppe_lower <- short_ppe$lower^-1

#summarized table
exposure <- c('Nursing Shortage (Below)',
              'Clinical Shortage (Below)',
              'Aides Shortage (Below)',
              'Other Staff Shortage (Below)',
              'Masks Shortage (Below)',
              'PPE Shortage (Below)')

OR <- c(short_nursing_OR,
        short_clinical_OR,
        short_aides_OR,
        short_other_OR,
        short_masks_OR,
        short_ppe_OR)

upper_CI <- c(short_nursing_lower,
              short_clinical_lower,
              short_aides_lower,
              short_other_lower,
              short_masks_lower,
              short_ppe_lower)

lower_CI <- c(short_nursing_upper,
              short_clinical_upper,
              short_aides_upper,
              short_other_upper,
              short_masks_upper,
              short_ppe_upper)


below_OR_table_staff_1ormore <- data.frame(exposure, OR, lower_CI, upper_CI)




##########################################APPENDING TABLES
rescases2 <- rbind(crude_OR_table_res_greaterthan2, above_OR_table_res_greaterthan2, below_OR_table_res_greaterthan2)

rescases1 <- rbind(crude_OR_table_res_1ormore, above_OR_table_res_1ormore, below_OR_table_res_1ormore)

staffcases2 <- rbind(crude_OR_table_staff_greaterthan2, above_OR_table_staff_greaterthan2, below_OR_table_staff_greaterthan2)

staffcases1 <- rbind(crude_OR_table_staff_1ormore, above_OR_table_staff_1ormore, below_OR_table_staff_1ormore)




library("ggplot2")
ggplot(rescases2, aes(exposure, OR)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI))






############staff 2 cases vs resident 2+ CASES CRUDE
crude.staff.2.res.2 <- table(base_omit$twopluscases.staff, base_omit$twopluscases)

crude.staff.2.res.2 <- odds.ratio(crude.staff.2.res.2)
crude.staff.2.res.2.OR <- crude.staff.2.res.2$OR^-1
crude.staff.2.res.2.upper <- crude.staff.2.res.2$upper^-1
crude.staff.2.res.2.lower <- crude.staff.2.res.2$lower^-1

############staff 2 cases vs resident 2+ CASES above
above.staff.2.res.2 <- table(base_omit$twopluscases.staff_above, base_omit$twopluscases_above)

above.staff.2.res.2 <- odds.ratio(above.staff.2.res.2)
above.staff.2.res.2.OR <- above.staff.2.res.2$OR^-1
above.staff.2.res.2.upper <- above.staff.2.res.2$upper^-1
above.staff.2.res.2.lower <- above.staff.2.res.2$lower^-1

############staff 2 cases vs resident 2+ CASES below
below.staff.2.res.2 <- table(base_omit$twopluscases.staff_below, base_omit$twopluscases_below)

below.staff.2.res.2 <- odds.ratio(below.staff.2.res.2)
below.staff.2.res.2.OR <- below.staff.2.res.2$OR^-1
below.staff.2.res.2.upper <- below.staff.2.res.2$upper^-1
below.staff.2.res.2.lower <- below.staff.2.res.2$lower^-1

exposure <- c("Crude",
              "Above 75% Staff Vaccinated",
              "Below 75% Staff Vaccinated")

OR <- c(crude.staff.2.res.2.OR,
        above.staff.2.res.2.OR,
        below.staff.2.res.2.OR)

upper_CI <- c(crude.staff.2.res.2.lower,
              above.staff.2.res.2.lower,
              below.staff.2.res.2.lower)

lower_CI <- c(crude.staff.2.res.2.upper,
              above.staff.2.res.2.upper,
              below.staff.2.res.2.upper)

staff2res2_OR_table <- data.frame(exposure, OR, lower_CI, upper_CI)


########################################################################################################################################
############staff 1 cases vs resident 1+ CASES CRUDE
crude.staff.1.res.1 <- table(base_omit$onepluscases.staff, base_omit$onepluscases)

crude.staff.1.res.1 <- odds.ratio(crude.staff.1.res.1)
crude.staff.1.res.1.OR <- crude.staff.1.res.1$OR^-1
crude.staff.1.res.1.upper <- crude.staff.1.res.1$upper^-1
crude.staff.1.res.1.lower <- crude.staff.1.res.1$lower^-1

############staff 1 cases vs resident 1+ CASES above
above.staff.1.res.1 <- table(base_omit$onepluscases.staff_above, base_omit$onepluscases_above)

above.staff.1.res.1 <- odds.ratio(above.staff.1.res.1)
above.staff.1.res.1.OR <- above.staff.1.res.1$OR^-1
above.staff.1.res.1.upper <- above.staff.1.res.1$upper^-1
above.staff.1.res.1.lower <- above.staff.1.res.1$lower^-1

############staff 1 cases vs resident 1+ CASES crude
below.staff.1.res.1 <- table(base_omit$onepluscases.staff_below, base_omit$onepluscases_below)

below.staff.1.res.1 <- odds.ratio(below.staff.1.res.1)
below.staff.1.res.1.OR <- below.staff.1.res.1$OR^-1
below.staff.1.res.1.upper <- below.staff.1.res.1$upper^-1
below.staff.1.res.1.lower <- above.staff.1.res.1$lower^-1



