library(readxl)
library(nomisr)
library(PostcodesioR)
library(purrr)
library(tidyverse)
source('code/Stat-Xplore_API_funcs.R')

############################################################################

# IMPORT DATA

############################################################################

getwd()

###TRUSSELL TRUST DATA

# Read in Trussell Trust foodbank data by quarter/FY from David Massey
foodbanks <- read.csv('./data/Total Feb each FB each quarter, calendar year.csv')
colnames(foodbanks)<- c("foodbank", "year", "quarter", "vouchers")

# Read in Trussell Trust foodbank location data from David Massey
FBpostcodes <- read_excel('./data/FB Postcodes.xlsx',.name_repair = "universal") 


###MAP DATA

#Get data for the first 100 postcodes in the FBpostcodes data frame via
#the Postcodes.io API using the PostcodesioR package
pc_list <- as.list(FBpostcodes[1:100,"Post.Code"])
names(pc_list) <- c("postcodes")
bulk_lookup_result <- bulk_postcode_lookup(pc_list)
bulk_list <- lapply(bulk_lookup_result, "[[", 2)
bulk_df <- map_dfr(bulk_list,`[`,c("postcode", "admin_district", "country",
                                   "longitude", "latitude"))

#Get data for the remaining postcodes in the FBpostcodes data frame and combine
#all postcode data into a single dataframe
fullbulk_df <- bulk_df
numPostcodes <- as.numeric(nrow(FBpostcodes))

i<-1
for (i in 1:floor(numPostcodes/100)){
  if ((i+1)*100 < numPostcodes){
    pc_list <- as.list(FBpostcodes[((i*100)+1):((i+1)*100),"Post.Code"])
  } else {
    pc_list <- as.list(FBpostcodes[((i*100)+1):numPostcodes,"Post.Code"])
  }
  names(pc_list) <- c("postcodes")
  bulk_lookup_result <- bulk_postcode_lookup(pc_list)
  bulk_list <- lapply(bulk_lookup_result, "[[", 2)
  bulk_df <- map_dfr(bulk_list,`[`,c("postcode", "admin_district", "country",
                                     "longitude", "latitude"))
  fullbulk_df <- rbind(fullbulk_df,bulk_df)
}

#Get rid of intermediary variables
rm(bulk_lookup_result)
rm(bulk_list)
rm(bulk_df)



###NOMIS DATA

#Note that currently the time is hardcoded, so won't give most up-to-date data
#Read in ONS model-based estimate values of unemployment from Nomis by LAD
unemployment <- nomis_get_data(id = "NM_127_1", time = "2019-01-2020-12", 
                    geography = "TYPE434", measures="20100", tidy=TRUE) %>%
  rename(LAD = geography_name) %>%
  rename(EstUnemploymentRate = obs_value) %>%
  filter(item_name == "Unemployment rate (model based)") %>%
  mutate(date = paste0(date,'-01')) %>% #Add a day to the date
  mutate(date = as.Date(date, format='%Y-%m-%d')) %>%
  as.data.frame() #Naturally they are tibbles

#Read in work-limited disabled from Nomis by LAD

workLimitingDisabled <- nomis_get_data(id = "NM_17_1", time = "2019-01-2020-12", 
                               geography = "TYPE434", cell = "405278465",
                               measures="20100", tidy=TRUE) %>%
  rename(LAD = geography_name) %>%
  rename(workLimitingDisabled = obs_value) %>%
  mutate(date = paste0(date,'-01')) %>% #Add a day to the date
  mutate(date = as.Date(date, format='%Y-%m-%d')) %>%
  as.data.frame() 
  


### Stat-Xplore Data
#This requires a file containing key for StatXplore API, and JSON queries to be saved.
#See documentation for 'get_statxplore_api_results' function in 
#Stat-Xplore_API_funcs.R file for details and instructions

api_key_path <- 'code/Stat-Xplore_API_key.txt'

list_queries <- list('housing_beneift' = 'code/Stat-Xplore_queries/Housing_benefit.json', 
                  'carers_entitlement' = 'code/Stat-Xplore_queries/Carers_allowance_entitlement.json',
                  'carers_payment' = 'code/Stat-Xplore_queries/Carers_allowance_payment.json',
                  'Households_UC' = 'code/Stat-Xplore_queries/Households_UC.json',
                  'State_pension' = 'code/Stat-Xplore_queries/State_pension.json')
#This is a list of filenames of the json queries
StatXplore_data <- list()
#Get data from API

for(sx_name in names(list_queries)){
  print(sx_name)
  res <- get_statxplore_api_results(list_queries[[sx_name]], api_key_path)
  #res is a temporary variable, overwritten each time
  print(names(res$dfs))
  for(df_name in names(res$dfs)){
    StatXplore_data[[df_name]] <- res$dfs[[df_name]] %>%
      as.data.frame() %>%
      StatXplore_date_conversion() %>%
      rename(LAD = `National - Regional - LA - OAs`)
  }
}

