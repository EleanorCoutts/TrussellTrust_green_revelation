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

# Read in Trussell Trust foodbank data by quarter/FY
foodbanks <- read.csv('./data/Total Feb each FB each quarter, calendar year.csv')
colnames(foodbanks)<- c("foodbank", "year", "quarter", "vouchers")

# Read in Trussell Trust foodbank location data
FBpostcodes <- read_excel('./data/FB Postcodes.xlsx',.name_repair = "universal") 

### Other locally saved data

#English indeices of multipe dprivation, summarised by local authority
IMD_LA_Eng <- "./data/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx"


###MAP DATA

#Get data for the first 100 postcodes in the FBpostcodes data frame via
#the Postcodes.io API using the PostcodesioR package
pc_list <- as.list(FBpostcodes[1:100,"Post.Code"])
names(pc_list) <- c("postcodes")
bulk_lookup_result <- bulk_postcode_lookup(pc_list)
bulk_list <- lapply(bulk_lookup_result, "[[", 2)
bulk_list_codes <- lapply(bulk_list, function(i) list(
  admin_district = i$admin_district, admin_district_code = i$codes$admin_district, 
  postcode = i$postcode, country = i$country, longitude = i$longitude, latitude = i$latitude
))

bulk_df <- map_dfr(bulk_list_codes,`[`,c("postcode", "admin_district", "country",
                                   "longitude", "latitude", "admin_district_code"))


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
  bulk_list_codes <- lapply(bulk_list, function(i) list(
    admin_district = i$admin_district, admin_district_code = i$codes$admin_district, 
    postcode = i$postcode, country = i$country, longitude = i$longitude, latitude = i$latitude
  ))
  bulk_df <- map_dfr(bulk_list_codes,`[`,c("postcode", "admin_district", "country",
                                           "longitude", "latitude", "admin_district_code"))
  
  fullbulk_df <- rbind(fullbulk_df,bulk_df)
}

#Get rid of intermediary variables
rm(bulk_lookup_result)
rm(bulk_list)
rm(bulk_df)
rm(bulk_list_codes)



###NOMIS DATA

start_date <- "2015-01" #The earliest date to get data for in the format YYYY-mm
end_date <- format(Sys.Date(), '%Y-%m')
date_range <- paste0(start_date, '-' ,end_date)


nomis_data_list <- list( 
  unemployment = list(description = "Unemployment rate", source = "Nomis", unit = '%',
                      params = list(id = "NM_127_1", item = 2)),
  workLimitingDisabled = list(description = "Work-limiting disabled - All",
                              source = "Nomis", unit = '',
                              params = list(id = "NM_17_1", cell = "405278465")),
  longTermSick = list(description = "Long term sick", source = "Nomis", unit = "",
                      params = list(id = "NM_17_1", cell = "404819713")),
  jobsDensity = list(description = 'Jobs density', source = "Nomis", unit = "",
                     params = list(id = "NM_57_1", item = "3")),
  loneParentHouseholds = list(description = 'Lone parent households', 
                              source = "Nomis", unit = "",
                              params = list(id = "NM_137_1", households_children = "1",
                                            eastatus = "0", depchild = "0",
                                            housetype = "1"))
  
)

Nomis_data <- list()
for(nomis_name in names(nomis_data_list)){
  print(nomis_name)
  info <- nomis_data_list[[nomis_name]]
  
  df <- do.call("nomis_get_data", c(list(time = date_range, geography = "TYPE434", 
                                       measures="20100", tidy=TRUE), 
                                    nomis_data_list[[nomis_name]]$params)) %>%
    rename(LAD = geography_name, LAD_code = geography_code) %>%
    rename(!!nomis_name := obs_value) #%>%
  
  if(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", df$date[1])){
    #Date is in YYYY-MM-DD format - no action needed
  } else if(grepl("^[0-9]{4}-[0-9]{2}$", df$date[1])){
    #Date is in YYYY-MM format - add in day (DD)
    df <- df %>% mutate(date = paste0(date,'-01')) 
  } else if(grepl("^[0-9]{4}$", df$date[1])){
    #Date is in YYYY format, as in month and day (MM-DD)
    df <- df %>% mutate(date = paste0(date,'-01-01'))
  }

  df <- df %>%
    mutate(date = as.Date(date, format='%Y-%m-%d')) %>%
    select(c('date', 'date_name', 'date_code', 'LAD', 'geography', nomis_name, 'LAD_code')) %>%
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
                            'unit' = '',
                            'source' = 'StatXplore'),
                     
                     'Mean_Housing_benefit_award_weekly' = 
                       list('query_file' = 'code/Stat-Xplore_queries/Housing_benefit.json',
                            'column' = 'Mean of Weekly Award Amount',
                            'description' = 'Mean weekly housing benefit award',
                            'unit' = "\u00A3",
                            'source' = 'StatXplore'), #£ is U+00A3 in unicode
                     
                     'carers_entitlement' = list(
                       'query_file' = 'code/Stat-Xplore_queries/Carers_allowance_entitlement.json', 
                       'column' = "CA (Entitled) - 2011 Geographies",
                       'description' = "Number of people entitled to carer's allowance",
                       'unit' = "",
                       'source' = 'StatXplore'
                     ),
                     
                     'carers_payment' = list(
                       'query_file' = 'code/Stat-Xplore_queries/Carers_allowance_payment.json',
                       'column' = "CA (In Payment) - 2011 Geographies",
                       'description' = "Number of people in receipt of carer's allowance",
                       'unit' = '',
                       'source' = 'StatXplore'
                     ),
                     
                     'Households_UC' = list(
                       'query_file' = 'code/Stat-Xplore_queries/Households_UC.json',
                       'column' = "Households on Universal Credit",
                       'description' = 'Households on Universal Credit',
                       'unit' = '',
                       'source' = 'StatXplore'),
                     
                     'State_pension' = list(
                       'query_file' = 'code/Stat-Xplore_queries/State_pension.json',
                       'column' = "State Pension caseload - 2011 Geographies",
                       'description' = 'State pension caseload',
                       'unit' = '',
                       'source' = 'StatXplore'
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
    filter(LAD != 'Total', LAD != 'Abroad')

}

