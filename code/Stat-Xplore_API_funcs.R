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
      timeout = 60)},
    error = function(c) {
      stop(paste("Error message:",c,"Could not connect to Stat-Xplore: the server may be down"))
    })
  
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


query_path <- 'code/sample_query.json'
api_key_path <- 'code/Stat-Xplore_API_key.txt'
a <- get_statxplore_api_results(query_path, api_key_path)