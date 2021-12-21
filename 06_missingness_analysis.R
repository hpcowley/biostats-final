###############################################################################
# Biostatistics 652 Final Project
# Conduct a missingness analysis on the clean data
# December 2021
###############################################################################
library(tidyverse)
library(naniar)
library(ltm)
library(reshape)
library(dplyr)

setwd('...replace with path to folder.../Bios_140.651_Final/Data')
load("tmp/nursing_home_data_clean.rdata")

# replace values of 'LNE' with nan
nursing_home_data <- nursing_home_data %>% replace_with_na_all(condition = ~.x == "LNE")

# subset down to the columns where we know there is missingness
# and also potential dependent variables
df <- nursing_home_data %>% dplyr::select(c(
  provider_id,
  provider_county_name,
  ownership_type,
  overall_rating,
  citation_count,
  nresid,
  pctfem,
  pctblack,
  pctwhite,
  pctunder65,
  pcthighcfs,
  county_poverty_pct,
  avg_res_vaccine_rate,
  avg_staff_vaccine_rate,
  total_res_cases_p,
  total_staff_cases_p,
  total_res_deaths_p,
  total_staff_deaths_p
))
a
###
# Plot the percentage of missing data by characteristic
###
toplot <- df %>% dplyr::select(c(pctblack, pctwhite, pctfem, pcthighcfs, pctunder65))
pct_missing <- data.frame(colSums(is.na(toplot)) / nrow(toplot))
colnames(pct_missing) <- c('pct')
ggplot(pct_missing, aes(x=factor(rownames(pct_missing)), y=pct)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels = c("% Black", "% Female", "% High Clinical Frailty", "% Under 65", "% White")) +
  ylab("Percent Missing") +
  xlab("Facility-Level Variable") + 
  ggtitle("Percent of facilities missing data by variable")


###
# Is our data  missingness correlated with any key dependent variables?
###
correlations <- data.frame()

for (x in c('nresid', 'pctblack', 'pctwhite', 'pctfem', 'pcthighcfs', 'pctunder65')) {
  for (y in c('total_res_cases_p', 'total_res_deaths_p', 'total_staff_cases_p', 'total_staff_deaths_p', 'county_poverty_pct')) {
    correlations <- rbind(correlations, c(x, y, biserial.cor(df[[y]], as.numeric(is.na(df[x])))))
  }
}
colnames(correlations) <- c('variable_with_missingness', 'outcome_variable', 'pbc')

ggplot(
  data=correlations, aes(x=variable_with_missingness, y=outcome_variable, fill=as.numeric(pbc))
) + geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Point Biserial Correlation"
  ) + 
  labs(
    title = "Correlation Coefficients"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1)) + 
  coord_fixed()

###
# Chi squared tests for independence between missingness level and county
###
# missingness in percent black and county
cty_x_black <- table(df$provider_county_name, as.numeric(is.na(df$pctblack)))
print(chisq.test(cty_x_black, simulate.p.value = TRUE))

# missingness in percent white and county
cty_x_white <- table(df$provider_county_name, as.numeric(is.na(df$pctwhite)))
print(chisq.test(cty_x_white, simulate.p.value = TRUE))

# missingness in percent high clincial frailty and county
cty_x_cfs <- table(df$provider_county_name, as.numeric(is.na(df$pcthighcfs)))
print(chisq.test(cty_x_cfs, simulate.p.value = TRUE))

# missingness in percent under 65 and county
cty_x_age <- table(df$provider_county_name, as.numeric(is.na(df$pctunder65)))
print(chisq.test(cty_x_age, simulate.p.value = TRUE))

###
# Does the percent of individuals over 65 differ significantly in different counties?
###
# derive the income by county table
cty_x_pov <- df %>% dplyr::select(c(provider_county_name, county_poverty_pct)) %>% group_by(provider_county_name)
cty_x_pov <- cty_x_pov[!duplicated(cty_x_pov), ]

# now test for equality of proportions
print(chisq.test(cty_x_pov$county_poverty_pct))

