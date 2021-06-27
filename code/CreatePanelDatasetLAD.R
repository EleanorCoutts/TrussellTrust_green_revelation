library(stringr) 
#library(sqldf)

###################################################################################

#Create panel dataset by LAD

###################################################################################

###Link Trussell Trust foodbanks to LADs

#Merge the postcode lat and long data with the FB postcodes data
names(FBpostcodes)[names(FBpostcodes)=="Post.Code"] <- "postcode"
FBpostcodesLL <- merge(FBpostcodes,fullbulk_df,"postcode",all=TRUE)

#Merge FBPostcodesLL with TrussellTrust foodbank voucher data
FBpostcodesLL$foodbank <- str_remove(FBpostcodesLL$Name, " Foodbank")
foodBankUsage <- merge(foodbanks, FBpostcodesLL)

quarter_to_month = c('Q1' = 'January', 'Q2' = 'April', 'Q3' = 'July', 
                     'Q4' = 'October')

LAD_panel <- foodBankUsage %>%
  select(admin_district, year, quarter, vouchers,foodbank, country) %>%
  arrange(foodbank,year,quarter) %>%
  group_by(admin_district,year,quarter,country) %>%
  summarise(sum_vouchers = sum(vouchers)) %>%
  rename(LAD = admin_district) %>%
  mutate(Month = recode(quarter, !!!quarter_to_month)) %>%
  mutate(date = paste('01',Month, year)) %>%
  mutate(date = as.Date(date, format='%d %B %Y')) 

#Group all explanatory dataframes into a list
#The names are the names of the column the data (i.e. variable of concern) is stored in
explanatory_vars <- c(StatXplore_data, Nomis_data)
explanatory_metadata <- c(StatXplore_data_list, nomis_data_list)

create_panel_dataset <- function(FB_data, explanatory_data, 
                                 explanatory_metadata, target_date){
  
  df_list <- list() #This will hold the data frames that have been filtered by date
  
  for(e in names(explanatory_data)){
    date_list <- unique(explanatory_data[[e]]$date)
    date_diffs_abs <- abs(date_list - target_date) 
    chosen_date <- date_list[which(date_diffs_abs == min(date_diffs_abs))]
    
    explanatory_metadata[[e]]$chosen_date <- chosen_date
    #Add the date chosen (i.e closest to the target) to the modified metadata list
    
    df <- explanatory_vars[[e]] %>% 
      filter(date == chosen_date) %>%
      mutate(LAD2 = LAD) %>%
      mutate(LAD = gsub('/(.*)',"",LAD2)) %>%
      mutate(LAD = str_trim(LAD)) %>%
      select(c('LAD',e))
    
    
    df_list <- c(df_list, list(df))
    
  }
  
  #Choose date for foodbank data
  date_list <- unique(FB_data$date)
  date_diffs_abs <- abs(date_list - target_date) 
  chosen_date <- date_list[which(date_diffs_abs == min(date_diffs_abs))]
  
  explanatory_metadata[['Foodbank_data']] <- list()
  explanatory_metadata[['Foodbank_data']]$chosen_date = chosen_date
  
  FB_new <- FB_data %>%
    filter(date == chosen_date) %>%
    select(c('LAD', 'sum_vouchers', 'country'))
  df_list <- c(df_list, list(FB_new))
  
  
  final_df <- data.frame(LAD = character())
  for(df in df_list){
    final_df <- full_join(final_df, df)
  }
  
  #view(final_df)
  list('panel_dataset' = final_df, 'metadata' = explanatory_metadata)
  
}

#Get a list of all LADs that appear in the explanatory data
LAD_list <- c()
for(e in names(explanatory_vars)){
  LAD_list <- c(LAD_list, explanatory_vars[[e]]$LAD)
}
LAD_list <- sort(unique(LAD_list))

#test_date = as.Date('2020-01-01')
#test <- create_panel_dataset(LAD_panel, explanatory_vars, 
#                           explanatory_metadata, test_date)
# 
# 
# #For now, just use most recent data for each of the variables. This may mean they are not all the same
# #time period.
# explanatory_df_list <- list()
# date_list <- list()
# for(e in names(explanatory_vars)){
#   
#   date_to_filter = max(explanatory_vars[[e]]$date)
#   date_list[e] <- as.character(date_to_filter)
#   print(e)
#   #print(names(explanatory_vars[[e]]))
#   new <- explanatory_vars[[e]] %>%
#     filter(date == date_to_filter) %>%
#     mutate(LAD2 = LAD) %>%
#     mutate(LAD = gsub('/(.*)',"",LAD2)) %>%
#     mutate(LAD = str_trim(LAD)) %>%
#     select(c('LAD',e)) 
#   
#   explanatory_df_list <- c(explanatory_df_list, list(new))
# }
# #Loop through merging in 1 at a time
# explanatory_panel <- data.frame(LAD = character())
# for(df in explanatory_df_list){
#   explanatory_panel <- full_join(explanatory_panel, df)
# }
# 
# 
# LAD_panelALL <- merge(LAD_panel, explanatory_panel, by = "LAD", all = TRUE)
# 
# make.names(names(LAD_panelALL))