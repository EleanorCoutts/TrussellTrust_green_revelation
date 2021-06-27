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

start_date <- "2015-01" #The earliest date to get data for in the format YYYY-mm
end_date <- format(Sys.Date(), '%Y-%m')
date_range <- paste0(start_date, '-' ,end_date)

#Read in ONS model-based estimate values of unemployment from Nomis by LAD

#The following list is a names list of variables to be read from Nomis
#the name of each list must be an approriate column name for the data: no spaces, no special characters
#In each list there must be the following items:
# - ID to pass to the nomis_get_data function
# - item : This is the item to retrieve, an integer. Use nomis_codelist(id, 'item') 
#          to get a list of options (look at id column). Can be NULL.
# - cell : Alternative to item - which to use depends on the dataset. Use
#          nomis_codelist(id, 'cell') to get the options (look at id column)
# - Description : This is a fuller description that is human readable - it will 
#                 appear on axes labels for example
# - Unit : The unit. Appears on axes labels. Can be '' if dimensionless.
nomis_data_list <- list(
'unemployment' = list('nomisr_id' = "NM_127_1", 
                      'item' = 2, 
                      'description' = "Unemployment rate",
                      'unit' = '', 
                      'cell' = NULL),
'workLimitingDisabled' = list('nomisr_id' = "NM_17_1", 
                              'item' = NULL, 
                              'description' = "Work-limiting disabled - All",
                              'unit' = '',
                              'cell' = "405278465")
)


Nomis_data <- list()
for(nomis_name in names(nomis_data_list)){
  print(nomis_name)
  info <- nomis_data_list[[nomis_name]]
  df <- nomis_get_data(id = info$nomisr_id, time = date_range, cell = info$cell, 
                        item = info$item, geography = "TYPE434", measures="20100", 
                        tidy=TRUE) %>%
     rename(LAD = geography_name) %>%
     rename(!!nomis_name := obs_value) %>%
     mutate(date = paste0(date,'-01')) %>% #Add a day to the date
     mutate(date = as.Date(date, format='%Y-%m-%d')) %>%
     select(c('date', 'date_name', 'date_code', 'LAD', 'geography', nomis_name)) %>%
     as.data.frame() #Otherwise they are tibbles
     
    
  Nomis_data[[nomis_name]] <- df
}



### Stat-Xplore Data
#This requires a file containing key for StatXplore API, and JSON queries to be saved.
#See documentation for 'get_statxplore_api_results' function in 
#Stat-Xplore_API_funcs.R file for details and instructions

api_key_path <- 'code/Stat-Xplore_API_key.txt'

StatXplore_data_list <- list('housing_benefit_claimants' = 
                       list('query_file' = 'code/Stat-Xplore_queries/Housing_benefit.json',
                            'column' = 'Housing Benefit Claimants',
                            'description' = 'Number of Housing Benefit Claimants',
                            'unit' = ''),
                     
                     'Mean_Housing_benefit_award_weekly' = 
                       list('query_file' = 'code/Stat-Xplore_queries/Housing_benefit.json',
                            'column' = 'Mean of Weekly Award Amount',
                            'description' = 'Mean weekly housing benefit award',
                            'unit' = '£'),
                     
                     'carers_entitlement' = list(
                       'query_file' = 'code/Stat-Xplore_queries/Carers_allowance_entitlement.json', 
                       'column' = "CA (Entitled) - 2011 Geographies",
                       'description' = "Number of people entitled to carer's allowance",
                       'unit' = ""
                     ),
                     
                     'carers_payment' = list(
                       'query_file' = 'code/Stat-Xplore_queries/Carers_allowance_payment.json',
                       'column' = "CA (In Payment) - 2011 Geographies",
                       'description' = "Number of people in receipt of carer's allowance",
                       'unit' = ''
                     ),
                     
                     'Households_UC' = list(
                       'query_file' = 'code/Stat-Xplore_queries/Households_UC.json',
                       'column' = "Households on Universal Credit",
                       'description' = 'Households on Universal Credit',
                       'unit' = ''),
                     
                     'State_pension' = list(
                       'query_file' = 'code/Stat-Xplore_queries/State_pension.json',
                       'column' = "State Pension caseload - 2011 Geographies",
                       'description' = 'State pension caseload',
                       'unit' = ''
                     )
                     )

#This is a list of filenames of the json queries
StatXplore_data <- list()
#Get data from API

for(sx_name in names(StatXplore_data_list)){
  info <- StatXplore_data_list[[sx_name]]
  #The StatXplore API can sometimes return errors - the reason for this is unknown.
  #Make several attempts for each dataset via a try loop.
  
  #attempts <- 1
  for(attempts in 1:6){
    #try()
    print(paste('Attempt',attempts,'for',sx_name))
    tryCatch({
      res <- get_statxplore_api_results(info$query_file, api_key_path)
    }, 
    error = function(cond){
      print(paste('Attempt ',attempts,'failed with error:',cond))
      return(NULL)
      }
      )
    
    #attempts <- attempts + 1
    if(!(is.null(try))){
      #Success
      break
    }
    }
  
  
  #res is a temporary variable, overwritten each time
  print(names(res$dfs))

  StatXplore_data[[sx_name]] <- res$dfs[[info$column]] %>%
    as.data.frame() %>%
    StatXplore_date_conversion() %>%
    rename(LAD = `National - Regional - LA - OAs`) %>%
    rename(!!sx_name := .data[[info$column]]) %>%
    filter(LAD != 'Total')

}

