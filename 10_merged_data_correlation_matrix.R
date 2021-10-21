library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(reshape)
library(tibble)
library(GGally)
library(ltm)
library(reshape2)

data <- read_csv("./nursing_home_data.csv")

# Data checks: make sure that cities align
double_check <- data %>% dplyr::select(c(
  "provider.id",                                         
  "Provider.Name.x",                                  
  "Provider.Address.x",                      
  "Provider.City.x",                          
  "Provider.State.x",                                               
  "Provider.Zip.Code.x",
  "Provider.Phone.Number",
  "SSACD",
  "Provider.County.Name",
  "Provider.Name.y",                                                
  "Provider.Address.y",                                           
  "Provider.City.y",                                                
  "Provider.State.y",                                               
  "Provider.Zip.Code.y",                                            
  "County",
  "county.x",
  "county.y"
))

# check that counties (upper case) match?
double_check$County = toupper(double_check$County)
double_check$county.x = toupper(double_check$county.x)
double_check$county.y = toupper(double_check$county.y)

print("Number of times county of sources don't match %d", nrow(subset(double_check, double_check$County != double_check$county.x)))
print("Number of times county of sources don't match %d", nrow(subset(double_check, double_check$County != double_check$county.y)))
print("Number of times county of sources don't match %d", nrow(subset(double_check, double_check$county.x != double_check$county.y)))
# good, the length of this is 0!

# check that cities(upper case) match?
double_check$Provider.City.x = toupper(double_check$Provider.City.x)
double_check$Provider.City.y = toupper(double_check$Provider.City.y)

print("Number of times city of sources don't match %d", nrow(subset(double_check, double_check$Provider.City.x != double_check$Provider.City.y)))

### DEFINING OUR DATA OF INTEREST ###
# "total.res.deaths.p": comparable from nursing home to nursing home. Deaths per average number of occupied beds.

selected <- data %>% dplyr::select(
  c(
    "provider.id",  # nursing home ID
    "pctfem", # percentage women
    "pctblack", # percentage black
    "pctwhite", # percentage white
    "pctunder65", # percentage under 65
    "pcthighcfs", # percentage with a high clinical frailty score
    "county_poverty_pct", # percentage of individuals 60+ who are in poverty by county
    "Ownership.Type", # Ownership status of the home (non-profit, etc)
    "Most.Recent.Health.Inspection.More.Than.2.Years.Ago", # self-explanatory
    "Overall.Rating", # rating of the ltc facility
    "Health.Inspection.Rating", # rating of the ltc facility
    "Reported.Total.Nurse.Staffing.Hours.per.Resident.per.Day", # number of hours residents have with nurses per day
    "Total.Weighted.Health.Survey.Score", # number of hours residents have with nurses per day
    "citation.count", # number of citations for the facility
    "infection.cit", # number of infection citations
    "avg.num.residents", # average number of residents
    "avg.bed.occupancy.rate", # average bed occupancy rate
    "avg.res.vaccine.rate", # average resident vaccine rate
    "avg.staff.vaccine.rate", # average staff vaccine rate
    "total.staff.cases.p", # total staff COVID cases as a percentage of staff
    "total.res.cases.p", # total resident COVID cases as a percentage of total occupied beds
    "avg_total_rate" # average COVID rate in the county
  )
)

# converting our data all to numeric, using dummy variables as needed
selected$provider_for_profit <- as.factor(ifelse(selected$Ownership.Type %in% c("For profit - Corporation", "For profit - Partnership", "For profit - Limited Liability company", "For profit - Individual"), 1, 0))
selected$provider_govt <- as.factor(ifelse(selected$Ownership.Type %in% c("Government - State", "Government - County", "Government - City/county", "Government - Federal"), 1, 0))

selected$health_inspection_gt_2_yrs_ago <- factor(
  selected$Most.Recent.Health.Inspection.More.Than.2.Years.Ago,
  levels=c('Y', 'N'),
  labels=c(1, 0)
)

selected$total_res_cases_as_pctage <- selected$total.res.cases.p

df <- selected %>% dplyr::select(
  -c(
    Ownership.Type,
    Most.Recent.Health.Inspection.More.Than.2.Years.Ago,
    Provider.Changed.Ownership.in.Last.12.Months,
    total.res.cases.p
  )
)

df <- df %>% mutate_all(
  as.numeric
) %>% na.omit(df) 

# Making our matrix for correlation data
sprintf("Number of homes for analysis: %d", nrow(df))  # we removed nan rows
sprintf("Original number of homes for analysis: %d", nrow(data))

for_corr <- dplyr::select(df, -c(provider.id))

correlation <- cor(
  data.frame(for_corr),
  method="spearman"
)
correlation <- data.frame(correlation) %>% dplyr::select(
  -c(
    provider_type_medicaid_only,
    provider_govt
  )
) %>% na.omit() %>% as.matrix()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(correlation)
to_plot <- melt(upper_tri)

corr_plot <- ggplot(
  data=to_plot, aes(x=Var1, y=Var2, fill=value)
) + geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", name="Spearman\nCorrelation") + 
  labs(title = "Correlation Coefficients") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1)) + coord_fixed()

ggsave("correlation_matrix.png")
corr_plot
