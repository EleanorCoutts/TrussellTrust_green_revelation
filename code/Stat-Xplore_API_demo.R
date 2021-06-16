source('code/Stat-Xplore_API_funcs.R')

list_queries <- c('code/Housing_benefit.json', 
                  'code/Carers_allowance_entitlement.json',
                  'code/Carers_allowance_payment.json')

#query_path <- list_queries[1]
api_key_path <- 'code/Stat-Xplore_API_key.txt'


res <- get_statxplore_api_results(list_queries[1], api_key_path)
housing_benefit_claimants <- res$dfs$`Housing Benefit Claimants`
housing_benefit_mean_weekly_award <- res$dfs$`Mean of Weekly Award Amount`

res <- get_statxplore_api_results(list_queries[2], api_key_path)
carers_allownce_entitlement <- res$dfs$`CA (Entitled) - 2011 Geographies`

res <- get_statxplore_api_results(list_queries[3], api_key_path)
carers_allowance_payment <- res$dfs$`CA (In Payment) - 2011 Geographies`