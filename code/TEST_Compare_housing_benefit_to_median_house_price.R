library(tidyverse)
library(readxl)
library(stringr)
#####
# This is a test script to combine two data sets and plot a graph to investigate 
# correlation between them. 
#
# The example used is housing benefit correlated to average house price. Both
# datasets are broken down by local authority.
#
# For now, just look at most recent data available, but in future can be extended
# to look at several time windows
#
#####

#Read in data

# House price data comes from ONS
# HPSSA Dataset 9. Median price paid for administrative geographies, 
# Table 2a (all house types), local authority

house_price <- read_excel('./data/HousePriceStatsSmallAreas.xls', 
                          sheet = '2a', skip=6, .name_repair = "universal") %>%
  #column names with spaces in have spaces replaced by a full stop. 
  #Other fixes are also applied in required
  subset(select = c('Local.authority.name','Year.ending.Jun.2020') ) %>%
  #For this example, just consider one time period, the most recent
  mutate(median_house_price = Year.ending.Jun.2020, .keep = 'unused')
  

benefits <- read_excel('./data/House_benefit_LA.xlsx', skip=7, 
                       .name_repair = "universal") %>%
  mutate(Local_authority = ...2, .keep = 'unused',
         num_HB_claimants = as.numeric(Housing.Benefit.Claimants),
         mean_HB_award = as.numeric(Mean.of.Weekly.Award.Amount)) %>% #column heading for LA is in row below: set manually
  subset(select=-c(1)) %>% #Remove empty columns
  slice(-c(1)) %>% # remove first row which contained more headers
  filter(!is.na(mean_HB_award) & !is.na(num_HB_claimants)) %>% 
  #Remove notes at bottom of table, also '..' values for negligible/insufficient data
  filter(Local_authority != 'Total' & Local_authority != 'Unknown')
#This table still has 'abroad' in
benefits$Local_authority <- str_trim(gsub('\\/.*',"",benefits$Local_authority),"both") #Remove everything after /


population <- read_excel('./data/Population_by_age_LA.xls', sheet = 'MYE2 - Persons',
                         skip = 4, .name_repair = "universal") %>%
  filter(Geography1 %in% c('Metropolitan District',
                                    'Unitary Authority', 'Non-metropolitan District',
                                    'London Borough','Council Area',
                                    'Local Government District')) %>%
  #Get rid of any geographies that aren't local authorities (see Admin Geography Heirarchy sheet)
  subset(select = c(Name, All.ages, Geography1)) %>%
  mutate(Local_authority = Name, Population = All.ages, .keep = "unused") 

#grep('\\(',population$Local_authority)

#test <- population$Local_authority
#test_alt <- str_trim(gsub('\\(.*',"",test),"both")
#test[8]
#test_alt[8]

#gsub('\\(','',population$Local_authority)

#Do a full join at first
combined_full <- full_join(house_price, benefits, by = c("Local.authority.name" = "Local_authority")) %>%
  full_join(population, by = c("Local.authority.name" = 'Local_authority')) 

#These are the local authorities that don't match up. For example if dataset covers different countires
#Or the name is entered differently
benefit_missing <- combined_full$Local.authority.name[is.na(combined_full$num_HB_claimants)]
house_price_missing <- combined_full$Local.authority.name[is.na(combined_full$median_house_price)]
pop_missing <- combined_full$Local.authority.name[is.na(combined_full$Population)]
#could also do an antijoi to achieve same effect

combined_partial <- combined_full %>%
  filter(!is.na(as.numeric(mean_HB_award)) & 
           !is.na(as.numeric(median_house_price)))

min_award <- min(combined_partial$mean_HB_award, na.rm = TRUE)
max_award <- max(combined_partial$mean_HB_award, na.rm = TRUE)
min_award
max_award

plot <- ggplot(data = combined_partial) +
  geom_point(aes(x = median_house_price, y = mean_HB_award))
plot
