###############################################################################
# Biostatistics 652 Final Project
# Prepare Maryland COVID-19 County data
# December 2021
# Steps:
# 1. Filter data to only MD facilities 
# 2. Subset to columns of interest for further investigation
# 3. Merge LTC and crosswalk file to get the facility's county
# 4. Process ACS data to get poverty information
# 5. Join LTC and ACS information
# 6. Create some exploratory plots
# 7. Final variable down-select in preparation for merging
# 8. Final plotting
# 9. Write final data to file for merging by team member
###############################################################################

# Remove objects previously stored in R
rm(list = ls())

# Set Paths
setwd('...replace with path to folder.../Bios_140.651_Final/Data')

#Load relevant packages for data preparation.
library('readxl')
library('ggplot2')
library('GGally')
library('dplyr')

###############################################################################
# READ DATA
###############################################################################
raw_data <- read_excel("raw/facility_2019_MDS_new.xlsx") 
acs <- read_csv("raw/acs_over_60_by_county.csv", skip=1)
crosswalk <- read.csv("raw/ssa_fips_xwalk_mdonly.csv")

###############################################################################
# FILTER TO MD FACILITIES
###############################################################################
md_data <- filter(raw_data, state=='MD')
# columns of interest: race/ethnicity, age, gender distribution, percent that are ambulatory, percent high/mid/low CFS (clinical frailty scale)

###############################################################################
# SUBSET COLUMNS
###############################################################################
data_of_interest <- dplyr::select(md_data, c(
    accpt_id, PROV1680, PROV0475, PROV2720, PROV3225, PROV2905, county, nresid,
    pctfem, pctblack_mds3, pcthisp_mds3, pctwhite_mds3, pctunder65, pctlowcfs, pctmidcfs, pcthighcfs, pctwalking
  )
)
# want to rename to some easier column names
data <- dplyr::rename(
  data_of_interest,
  id=accpt_id,
  name=PROV0475,
  street_address=PROV2720,
  city=PROV3225,
  zipcode=PROV2905,
  pctblack=pctblack_mds3,
  pcthisp=pcthisp_mds3,
  pctwhite=pctwhite_mds3,
  FIPS_nostate=county
)

###############################################################################
# MERGE WITH CROSSWALK
###############################################################################
crosswalk$FIPS_nostate <- substring(
  crosswalk$FIPS.County.Code, nchar(crosswalk$FIPS.County.Code)-2, nchar(crosswalk$FIPS.County.Code)
)
data <- data %>% 
  left_join(dplyr::select(crosswalk, c(FIPS_nostate, County.Name)), by='FIPS_nostate') %>%
  dplyr::rename(county=County.Name) %>%
  na.omit()

###############################################################################
# PROCESS ACS DATA TO GET POVERTY INVORMATION
###############################################################################
# population of 60+ with poverty status in last 12 months: Estimate!!60 years and over!!POVERTY STATUS IN THE PAST 12 MONTHS!!Population for whom poverty status is determined
# population 60+ years old: Estimate!!60 years and over!!Total population

acs_select <- acs %>%
  dplyr::rename(
    pct_poverty=`Estimate!!60 years and over!!POVERTY STATUS IN THE PAST 12 MONTHS!!Population for whom poverty status is determined!!Below 100 percent of the poverty level`,
    'county_id'='id'
  ) %>%
  dplyr::select(c(county_id, pct_poverty)) %>%
  mutate(pct_poverty=as.numeric(pct_poverty))

acs_select$FIPS.County.Code <- as.numeric(substring(acs_select$county_id, 10))
acs_select <- acs_select %>% 
  left_join(dplyr::select(crosswalk, c(FIPS.County.Code, County.Name)), by='FIPS.County.Code') %>%
  dplyr::rename(county=County.Name)

avg_poverty_rate <- mean(acs_select$pct_poverty, na.rm=TRUE)
print(avg_poverty_rate)

# 2 counties don't report these data. Although imperfect, we'll replace with the MD average.
acs_select$pct_poverty[is.na(acs_select$pct_poverty)] <- avg_poverty_rate 
acs_select <- na.omit(acs_select)

###############################################################################
# JOIN ACS AND LTC
###############################################################################
acs_select['FIPS_nostate'] <- substring(
  acs_select$FIPS.County.Code, 3, length(acs_select$FIPS.County.Code)
)

acs_subset <- dplyr::select(acs_select, c(FIPS_nostate, pct_poverty, "Geographic Area Name")) 

#Reading in the SSA-FIP Crosswalk.
ssa <- read.csv("raw/ssa_fips_xwalk_mdonly.csv")
ssa$FIPS_nostate <- substring(ssa$FIPS.County.Code, 3, length(ssa$FIPS.County.Code))
ssa <- ssa %>%
  dplyr::select(c(County.Name, SSACD, FIPS_nostate))
acs_subset <- left_join(acs_subset, ssa, by = c("FIPS_nostate"))

save(acs_subset, file = 'tmp/acs_subset.RData')

final_join <- data %>%
  left_join(acs_subset, by='FIPS_nostate') %>%
  dplyr::rename(county_poverty_pct=pct_poverty)

###############################################################################
# EXPLORATORY PLOTTING
###############################################################################

# Percentage of white residents by county
n_missing <- nrow(data[data$pctwhite=="LNE", ])
sprintf("Number of missing data points: %s", n_missing)
plt_white <- ggplot(
  data[data$pctwhite!="LNE", ],
  aes(x=county, y=as.numeric(pctwhite))
) + geom_boxplot(aes())
plt_white <- plt_white + ggtitle("Percent White Race Per County") + xlab("County") + ylab("Percent White residents")
plt_white + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("output/white_pcts_by_county.png")

# Percentage of black residents by county
n_missing <- nrow(data[data$pctblack=="LNE", ])
sprintf("Number of missing data points: %s", n_missing)
plt_black <- ggplot(
  data[data$pctblack!="LNE", ],
  aes(x=county, y=as.numeric(pctblack))
) + geom_boxplot(aes())
plt_black <- plt_black + ggtitle("Percent Black Race Per County") + xlab("County") + ylab("Percent Black residents")
plt_black + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("output/black_pcts_by_county.png")


# Percentage hispanic residents by county
n_missing <- nrow(data[data$pcthisp=="LNE", ])
sprintf("Number of missing data points: %s", n_missing)
plt_hisp <- ggplot(
  data[data$pcthisp!="LNE", ],
  aes(x=county, y=as.numeric(pcthisp))
) + geom_boxplot(aes())
plt_hisp <- plt_hisp + ggtitle("Percent Hispanic Per County") + xlab("County") + ylab("Percent Hispanic residents")
plt_hisp + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("output/hispanic_pcts_by_county.png")

# Percentage female residents by county
n_missing <- nrow(data[data$pctfem=="LNE", ])
sprintf("Number of missing data points: %s", n_missing)
plt_fem <- ggplot(
  data[data$pctfem!="LNE", ],
  aes(x=county, y=as.numeric(pctfem))
) + geom_boxplot(aes())
plt_fem <- plt_fem + ggtitle("Percent Female Per County") + xlab("County") + ylab("Percent Female residents")
plt_fem + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("output/female_pcts_by_county.png")

# Percentage of residents under 65 by county
n_missing <- nrow(data[data$pctunder65=="LNE", ])
sprintf("Number of missing data points: %s", n_missing)
plt_age <- ggplot(
  data[data$pctunder65!="LNE", ],
  aes(x=county, y=as.numeric(pctunder65))
) + geom_boxplot(aes())
plt_age <- plt_age + ggtitle("Percent of Residents Under 65 Per County") + xlab("County") + ylab("Percent <65y/o residents")
plt_age + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("output/ageU65_pcts_by_county.png")

# And percentage of residents under 65 as a histogram
plt_age_hist <- ggplot(
  data[data$pctunder65!="LNE", ],
  aes(as.numeric(pctunder65))
) + geom_histogram()
plt_age_hist <- plt_age_hist + ggtitle("Histogram of Percent of Residents Under 65")
ggsave("output/ageU65_hist.png")

# Clinical frailty scores as a histogram
plt_hcfs_hist <- ggplot(
  data[data$pcthighcfs!="LNE", ],
  aes(as.numeric(pcthighcfs))
) + geom_histogram()
plt_hcfs_hist <- plt_hcfs_hist + ggtitle("Histogram of Percent of Residents With High Clinical Frailty")
ggsave("output/high_cfs_hist.png")

# Percent of residents walking as a histogram
plt_walking_hist <- ggplot(
  data[data$pctwalking!="LNE", ],
  aes(as.numeric(pctwalking))
) + geom_histogram()
plt_walking_hist <- plt_walking_hist + ggtitle("Histogram of Percent of Residents Who Are Walking")
ggsave("output/pct_walking_hist.png")

###############################################################################
# FINAL VARIABLE DOWN-SELECT
###############################################################################
final_data <- final_join %>%
  dplyr::select(
    c(
      id,
      FIPS_nostate,
      county,
      zipcode,
      county_poverty_pct,
      nresid,
      pctfem,
      pctblack,
      pctwhite,
      pctunder65,
      pcthighcfs
    )
  ) %>%
  dplyr::rename(
    provider.id=id
  )

###############################################################################
# PAIR-PLOT OF RELATIONSHIPS BETWEEN VARIABLES
###############################################################################
for_plotting <- final_data %>% dplyr::select(c(county_poverty_pct,
                                        nresid,
                                        pctfem,
                                        pctblack,
                                        pctwhite,
                                        pctunder65,
                                        pcthighcfs))
# original length of data: 

for_plotting <- na.omit(for_plotting)
# after dropping nans
df2 <- mutate_all(for_plotting, function(x) as.numeric(as.character(x)))
ggpairs(df2) #+ geom_point(size=4) 
ggsave(file="output/interactions.png", width=10, height=10, dpi=300)

###############################################################################
# WRITE FINAL DATA
###############################################################################
write.csv(final_data, "tmp/demographics_poverty_county_join_hpc.csv")
save(final_data, file = 'tmp/demographics_poverty_county_join_hpc.RData')
