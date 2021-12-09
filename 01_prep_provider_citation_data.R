###############################################################################
# Biostatistics 651 Final Project
# Prepare Nursing Home Provider and Deficiencies data
# October 2021
# Steps:
# 1. Housekeeping
# 2. Load data and basic cleaning
# 3. Flag infection control violations and create count variables
# 4. Export data
###############################################################################

# 1. Housekeeping
# Remove objects previously stored in R
rm(list = ls())

# Set Paths
setwd('/Users/rcorgel/OneDrive - Johns Hopkins/Bios_140.651_Final/Data')

# Load Libraries
library(tidyverse)
library(writexl)

# 2. Load data and basic cleaning
#move Deficiencies to single nursing home, infection control violations, sum of this, total violations, mess with citation data first and then merge with provider
#read in data sets
citations <- read.csv('raw/HealthDefiencies_Download_March2020.csv')
provider <- read.csv('raw/NH_ProviderInfo_Oct2021.csv')

#change provider number variable to character
citations$PROVNUM <- as.character(citations$PROVNUM)
provider$Federal.Provider.Number <- as.character(provider$Federal.Provider.Number)

#filter by state, MD
citations_MD <- citations %>% filter(STATE=='MD')
provider_MD <- provider %>% filter(Provider.State=='MD')

#provider data work
#renaming the Federal Provider Number to provider.id
provider_MD <- provider_MD %>% dplyr::rename(provider.id = Federal.Provider.Number)

#dropping columns with NA values
provider_MD <- provider_MD %>% select(-c(Average.Number.of.Residents.per.Day.Footnote,
                                         Special.Focus.Status,
                                         Overall.Rating.Footnote,
                                         Health.Inspection.Rating.Footnote,
                                         QM.Rating.Footnote,
                                         Short.Stay.QM.Rating.Footnote,
                                         Long.Stay.QM.Rating.Footnote,
                                         Reported.Staffing.Footnote,
                                         Staffing.Rating.Footnote,
                                         RN.Staffing.Rating.Footnote,
                                         Physical.Therapist.Staffing.Footnote))

# 3. Flag infection control violations and create count variables
#citation data work
#counting if there was an infection control violation
citations_MD <- citations_MD %>% mutate(infection.violation = ifelse(TAG =="880", 1, 0))

#total number of citations per nursing home by federal provider number
cit_counts <- citations_MD %>% count(PROVNUM, name = 'citation.count')

#sum of infection control violations per nursing home and put into new df
cit_counts1 <- citations_MD %>% group_by(PROVNUM) %>%
  summarise(infection.cit = sum(infection.violation))

#joining the dfs
cit_counts <- left_join(cit_counts, cit_counts1, by =
                          c('PROVNUM'))

#rename the provider number for another join
cit_counts <- cit_counts  %>% dplyr::rename(provider.id = PROVNUM)

#replace compromised provider numbers
cit_counts$provider.id <- ifelse(cit_counts$provider.id == '21000000000', '21E009', cit_counts$provider.id)
cit_counts$provider.id <- ifelse(cit_counts$provider.id == '2.1E+105', '21E104', cit_counts$provider.id)

#join the cit_counts df to the provider_md df
provider_MD <- left_join(provider_MD, cit_counts, by =
                           c('provider.id'))

# 4. Export data
write.csv(provider_MD, 'tmp/provider_MD_formerge.csv')
save(provider_MD, file = 'tmp/provider_MD_formerge.RData')
