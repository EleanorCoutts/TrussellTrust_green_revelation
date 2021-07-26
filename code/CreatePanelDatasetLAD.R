library(stringr) 
library(xlsx)
library(lubridate)
#library(sqldf)

# If getting error message 
#
# Error: package or namespace load failed for ‘xlsx’:
#   .onLoad failed in loadNamespace() for 'rJava', details:
#   call: fun(libname, pkgname)
# error: JAVA_HOME cannot be determined from the Registry
#
# when loading package xlsx, then download and install the 64-bit Java 
# (assuming your R version is 64 bit). Download from https://java.com/en/download/manual.jsp
# Install it, and check path is "C:\Program Files\Java\jre1.8.0_291"
# 
# Then in windows cmd run
# setx PATH "C:\Program Files\Java\jre1.8.0_211\bin\server;%PATH%"
#
# Then in RStudio run
# Sys.setenv(JAVA_HOME="")

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
  select(admin_district, admin_district_code, year, quarter, vouchers,foodbank, country) %>%
  arrange(foodbank,year,quarter) %>%
  group_by(admin_district,admin_district_code,year,quarter,country) %>%
  summarise(sum_vouchers = sum(vouchers)) %>%
  rename(LAD = admin_district, LAD_code = admin_district_code) %>%
  mutate(Month = recode(quarter, !!!quarter_to_month)) %>%
  mutate(date = paste('01',Month, year)) %>%
  mutate(date = as.Date(date, format='%d %B %Y')) 

#Group all explanatory dataframes into a list
#The names are the names of the column the data (i.e. variable of concern) is stored in
explanatory_vars <- c(StatXplore_data, Nomis_data, IMD_data)
explanatory_metadata <- c(StatXplore_data_list, nomis_data_list, IMD_metadata)

#Named list of the descriptions
explanatory_metadata_description <- sapply(explanatory_metadata, function(i)i[['description']])
#swap names and values
explanatory_metadata_description <- setNames(names(explanatory_metadata_description), explanatory_metadata_description)

create_snapshot_dataset <- function(FB_data, explanatory_data, 
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
      select(c('LAD','LAD_code',e))
    
    
    df_list <- c(df_list, list(df))
    
  }
  
  #Choose date for foodbank data
  date_list <- unique(FB_data$date)
  date_diffs_abs <- abs(date_list - target_date) 
  chosen_date <- date_list[which(date_diffs_abs == min(date_diffs_abs))]
  
  explanatory_metadata[['Foodbank_data']] <- list()
  explanatory_metadata[['Foodbank_data']]$description = 'Number of food parcels given out by Trussel Trust'
  explanatory_metadata[['Foodbank_data']]$chosen_date = chosen_date
  explanatory_metadata[['Foodbank_data']]$source = 'Locally saved foodbank data'
  
  
  
  FB_new <- FB_data %>%
    filter(date == chosen_date) %>%
    select(c('LAD', 'LAD_code', 'sum_vouchers', 'country')) #%>%
  
#
  df_list <- c(df_list, list(FB_new))
  
  
  final_df <- data.frame(LAD_code = character())
  for(df in df_list){
    final_df <- full_join(final_df, df, by = "LAD_code") 
      #Both original dataframes had LAD - they are renamed LAD.x and LAD.y. 
      #Keep x as LAD, remove y. 
    if('LAD.x' %in% names(final_df)){final_df = rename(final_df,LAD = LAD.x)}
    if('LAD.y' %in% names(final_df)){final_df = select(final_df,-LAD.y)}
  }
  
  #view(final_df)
  list('dataset' = final_df, 'metadata' = explanatory_metadata)
  
}

create_panel_dataset_time <- function(FB_data, explanatory_data, 
                          explanatory_metadata, snapshots){
  data_ls <- list()
  metadata_ls <- list()
  
  for(date in snapshots){
    date <- ymd(date)
    func_return <- create_snapshot_dataset(FB_data, explanatory_data, 
                                           explanatory_metadata, date)
    
    
    d <- func_return$dataset
    d$date <- date
    data_ls[[as.character(date)]] <- d
    
    #prepare dataframe for metadata to go into
    selected_metadata_df <- data.frame(description = character(), source = character(),
                                       chosen_date = character(), variable = character(),
                                       snapshot_date = character())
    
    for(var in names(func_return$metadata)){
      #amalgamate metadata into a dataframe (for given date)
      l <- func_return$metadata[[var]][c('description','source','chosen_date')]
      l$variable <- var
      l$snapshot_date <- date
      selected_metadata_df <- rbind(selected_metadata_df, as.data.frame(l))
    }
    metadata_ls[[as.character(date)]] <- selected_metadata_df
  }
  
  data <- bind_rows(data_ls) %>% arrange(LAD, date)
  metadata <- bind_rows(metadata_ls) %>% 
    arrange(variable, snapshot_date) %>%
    relocate(variable, snapshot_date) %>% #reorder columns
    rename(display_date = snapshot_date, actual_date = chosen_date)
  
  
  list('data' = data, 'metadata' = metadata)
}

#Get a list of all LADs that appear in the explanatory data
LAD_list <- c()
for(e in names(explanatory_vars)){
  LAD_list <- c(LAD_list, explanatory_vars[[e]]$LAD)
}
LAD_list <- sort(unique(LAD_list))


xlsx_description <- paste(scan(file = 'code/Excel_information_page.txt', 
                               what = character(), sep = "\n", blank.lines.skip = FALSE),
                          sep = '/n')
  
write_panel_data_to_file <- function(fname, data, metadata){
  write.xlsx(x = xlsx_description, file = fname, sheetName = 'Information', 
             row.names = FALSE, col.names = FALSE)
  write.xlsx(x = data, file = fname, sheetName = 'Data', append=TRUE)
  write.xlsx(x = metadata, file = fname, sheetName = 'metadata', 
             row.names = FALSE, append = TRUE)
  

  
}


 
 
test_date = as.Date('2020-01-01')
test <- create_snapshot_dataset(LAD_panel, explanatory_vars,
                         explanatory_metadata, test_date)
test_dates <- list(as.Date('2020-01-01'), as.Date('2020-02-01'), as.Date('2020-03-01'))
test_dates <- list("2015-01-01", "2015-04-01", "2015-07-01", "2015-10-01", "2016-01-01", "2016-04-01", "2016-07-01",
                  "2016-10-01", "2017-01-01", "2017-04-01", "2017-07-01", "2017-10-01", "2018-01-01", "2018-04-01",
                  "2018-07-01", "2018-10-01", "2019-01-01", "2019-04-01", "2019-07-01", "2019-10-01", "2020-01-01",
                  "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01", "2021-04-01", "2021-07-01")

# 
# test_panel <- create_panel_dataset_time(LAD_panel, explanatory_vars,
#                                         explanatory_metadata, test_dates)


 
# write_panel_data_to_file('test.xlsx',test_panel$data, test_panel$metadata)


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