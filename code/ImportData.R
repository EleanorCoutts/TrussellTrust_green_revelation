library(tidyverse)
library(readxl)
library(stringr)
library(nomisr)

############################################################################

# IMPORT DATA

############################################################################

getwd()

###TRUSSELL TRUST DATA

# Read in annual 2019/2020 Trussell Trust foodbank data from David Massey
foodbanks<- read_csv('./data/Total Fed By Fiscal Year (2).csv' )
colnames(foodbanks)<- c("foodbank", "year", "vouchers")
foodbanks <- foodbanks %>%
  slice(-c(1,2)) %>%  #drop 2 rows that are a filter not data
  # change year from FY 20xx character sting to numeric 20xx 
  mutate(year = as.integer(gsub("FY ","",year)))%>%
  group_by(foodbank) %>%
  mutate(prevYrVouchers=lag(vouchers))  #add a column of difference in vouchers over previous year

# Read in Trussell Trust foodbank location data from David Massey
FBpostcodes <- read_excel('./data/FB Postcodes.xlsx',.name_repair = "universal") 


###MAP DATA

#Read in Uk postcode data downloaded from doogle.co.uk
#https://www.doogal.co.uk/UKPostcodes.php 
Df_UK <- read.csv('./data/postcodes.csv')


###NOMIS DATA

#Read in ONS model-based estimate values of unemployment from Nomis by LAD
unemployment <- nomis_get_data(id = "NM_127_1", time = "2019-01-2020-12", 
                    geography = "TYPE434", measures="20100", tidy=TRUE)

#Read in work-limited disabled from Nomis by LAD
workLimitingDisabled <- nomis_get_data(id = "NM_17_1", time = "2019-01-2020-12", 
                               geography = "TYPE434", cell = "405278465",
                               measures="20100", tidy=TRUE)

