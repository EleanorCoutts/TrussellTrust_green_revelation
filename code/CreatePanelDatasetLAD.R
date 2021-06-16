library(stringr) 
library(sqldf)

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

LAD_panel <- foodBankUsage %>%
  select(admin_district, year, quarter, vouchers,foodbank) %>%
  arrange(foodbank,year,quarter) %>%
  group_by(admin_district,year,quarter) %>%
  summarise(sum_vouchers = sum(vouchers)) %>%
  rename(LAD = admin_district)

#Group all explanatory dataframes into a list
#The names are the names of the column the data (i.e. variable of concern) is stored in
explanatory_vars <- c(StatXplore_data, 
                      list('EstUnemploymentRate' = unemployment, 
                           'workLimitingDisabled' = workLimitingDisabled))

#For now, just use most recent data for each of the variables. This may mean they are not all the same
#time period.
explanatory_df_list <- list()
date_list <- list()
for(e in names(explanatory_vars)){
  print(e)
  date_to_filter = max(explanatory_vars[[e]]$date)
  date_list[e] <- as.character(date_to_filter)
  new <- explanatory_vars[[e]] %>%
    filter(date == date_to_filter) %>%
    mutate(LAD2 = LAD) %>%
    mutate(LAD = gsub('/(.*)',"",LAD2)) %>%
    mutate(LAD = str_trim(LAD)) %>%
    select(c('LAD',e)) 
  explanatory_df_list <- c(explanatory_df_list, list(new))
}
#Loop through merging in 1 at a time
explanatory_panel <- data.frame(LAD = character())
for(df in explanatory_df_list){
  explanatory_panel <- full_join(explanatory_panel, df)
}


LAD_panelALL <- merge(LAD_panel, explanatory_panel, by = "LAD", all = TRUE)
