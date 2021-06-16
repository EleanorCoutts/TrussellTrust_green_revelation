### Script containing functions to get data from Stat-Xplore using the API

# This is heavily based on the package statxplorer https://github.com/houseofcommonslibrary/statxplorer (olihawkins)
# It is recreated here to simplify it and put in a single R file.
library(stringr)
library(httr)
library(jsonlite)
library(purrr)

URL_INFO <- "https://stat-xplore.dwp.gov.uk/webapi/rest/v1/info"
URL_TABLE <- "https://stat-xplore.dwp.gov.uk/webapi/rest/v1/table"


#' Get the results from an API query
#' 
#' @param query_json_file is a .json file containing the query. Can be an absolute 
#' or realtive file path
#' @param api_key_filename is a .txt file containing your Stat-Xplore API key.
#' Can be absolute or relative
#' @return named list. One elements is dfs: this is a named list of dataframes
#' (one for each measure)
#' 
#' The easiest way to get the json file is to use the Stat-Xplore website. Get 
#' the table you want, and in the top right hand corner there's a dropdown box 
#' next to download. Choose the 'Open Data API Query (.json)' option and click go.
#' 
#' The downloaded JSON query will have the date shown on the table you viewed specified.
#' Remove this to get all dates (i.e. open up json file and remove the line that
#' specifies dates. e.g.
#' "map" : [ [ "...:202102..." ] ],
#' turns into 
#' "map" : [],
#' )
#' 
#' To get an API key you need a log-in on the Stat-Xplore website. On home page
#' click on 3 dots in right hand corner, choose Account, under Open Data API Access
#' click copy to copy the key to the clipboard. Paste into a .txt document, and 
#' the filepath of this document is the api_key_filename.
#' 
get_statxplore_api_results <- function(query_json_file, api_key_filename){
  
  if(!file.exists(query_json_file)){
    stop('json file ',query_json_file,'does not exist')
  }
  if(!file.exists(api_key_filename)){
    stop('api key file ',api_key_filename,'does not exist')
  }
  
  api_key = stringr::str_trim(readr::read_file(api_key_filename))
  
  query <- readr::read_file(query_json_file)
  
  response_json <- request_table(query, api_key)
  
  extract_results(response_json)
}

#' Send an http request with a query and return the response
#'
#' request_table sends a query to the table endpoint, checks the
#' response, parses the response text as json, and returns the parsed data. If
#' the query syntax is not valid or the request fails for any other reason an
#' error is raised with the response text.
#'
#' @param query A valid Stat-Xplore query as a string.
#' @return A list of the query results as parsed json.
request_table <- function(query, api_key) {
  
  # Set headers
  headers <- httr::add_headers(
    "APIKey" = api_key,
    "Content-Type" = "application/json")
  
  # POST and return
  tryCatch({
    response <- httr::POST(
      URL_TABLE,
      headers,
      body = query,
      encode = "form",
      timeout = 5000)},
    error = function(c) {
      stop(paste("Error message:",c,"Could not connect to Stat-Xplore: the server may be down"))
    })
  
  response_headers <- headers(response)
  num_queries_remaining <- response_headers$`x-ratelimit-remaining-table`
  queries_reset_time <- as.POSIXct(as.numeric(response$headers$`X-RateLimit-Reset`)/1000, origin="1970-01-01")
  print(paste('Stat-Xplore_API has',num_queries_remaining,
              'queries remaining, reset due at',queries_reset_time))
  
  # Extract the text
  response_text <- httr::content(response, as = "text", encoding = "utf-8")
  
  # If the server returned an error raise it with the response text
  if (response$status_code != 200) {
    stop(stringr::str_glue(
      "The server responded with the error message: {response_text}"))
  }
  
  # Process the JSON, and return
  jsonlite::fromJSON(response_text, simplifyVector = FALSE)
}

#' Extract the results of a query from the response json
#'
#' extract_results processes the results of the query and extracts the
#' data in a format suitable for analysis.
#'
#' @param json The results of the query as parsed json.
#' The original function had a custom parameter, this has been removed to 
#' simplify the code
#' @return A list of the results for the given cube.
extract_results <- function(json) {
  
  # Extract measure labels
  measures <- purrr::map_chr(json$measures, function(measure) measure$label)
  
  # Extract field labels
  fields <- purrr::map_chr(json$fields, function(field) field$label)
  
  # Extract labels for items
  items <- purrr::map(json$fields, function(field) {
    unlist(lapply(field$items, function(item) item$labels))
  })
  names(items) <- fields
  
  # Extract uris for items
  uris <- purrr::map(json$fields, function(field) {
    unlist(lapply(field$items, function(item) item$uris))
  })
  names(uris) <- fields
  
  
  # Extract dataframes for measures
  dfs <- purrr::imap(measures, function(measure, i) {
    
    df <- extract_items_df(items)
    values <- unlist(json$cubes[[i]][[1]])
    
    num_rows <- nrow(df)
    num_values <- length(values)
    
    if (num_rows != num_values) {
      stop(stringr::str_c(
        "Could not process query results. ",
        stringr::str_glue("There are {num_rows} item combinations "),
        stringr::str_glue("but {num_values} values. ") ,
        "Have you provided the correct metadata for custom aggregate ",
        "variables? See: https://github.com/olihawkins/statxplorer",
        "#custom-aggregate-variables"))
    }
    
    df[[measure]] <- values
    df
  })
  names(dfs) <- measures
  
  # Return the results
  list(
    measures = measures,
    fields = fields,
    items = items,
    uris = uris,
    dfs = dfs)
}


extract_items_df <- function(items) {
  
  # Create a dataframe of the combinations in order
  do.call(tidyr::expand_grid, items)
}

StatXplore_date_conversion <- function(df){
  new <- df
  
  if('Month' %in% names(df)){
    
   # new <- new %>%
   #  separate(Month, into = c('month_numeric', 'month_text'), sep = 6, remove=FALSE) %>%
   #  #Turn YYYYmm format into YYYYmmdd format by setting dd to the first of each month
   #  mutate(month_numeric = paste(month_numeric,'01', sep = '')) %>%
   #  mutate(date = as.Date(month_numeric, format='%Y%m%d'))
    
    try_formats = c('%Y%m (%b-%y) %d', '%B %Y %d') 
    #These are possible formats the date is in, including day at end which must be added in
    solved <- FALSE
    for(format in try_formats){
      temp <- new %>%
        mutate(Month_day = paste(Month, '01')) %>% #Add in the day
        mutate(date = as.Date(Month_day, format = format))
      
      if(!all(is.na(temp$date))) {
        solved <- TRUE
        new <- temp
        break
      }
    } 
    if(!(solved)){
      print('Could not understand Month format')
      warning('Month format not understood so date conversion unsuccesful')
    }
   
   
   } else if ('Quarter' %in% names(df)){
     
     new <- new %>%
       mutate(Quarter_day = paste0('01-',Quarter), .keep='all') %>%
       mutate(date = as.Date(Quarter_day, format = '%d-%b-%y', .keep='all'))
     
   } else {
      print('No month or quarter in the dataframe') #Printing as well as warning helps with debugging
      warning("No 'Month' or 'Quarter' column in dataframe, therefore date conversion unsuccesful")
    
   
    }
  new
}
