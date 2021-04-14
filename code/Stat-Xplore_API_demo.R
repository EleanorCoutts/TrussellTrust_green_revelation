source('code/Stat-Xplore_API_funcs.R')

query_path <- 'code/sample_query.json'
api_key_path <- 'code/Stat-Xplore_API_key.txt'
res <- get_statxplore_api_results(query_path, api_key_path)

housing_benfit_claimants <- res$dfs$`Housing Benefit Claimants`