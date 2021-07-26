library(tidyverse)
library(readxl)

### Specify paths ###
IMD_LA_Eng_path <- "./data/IMD_data/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx"
IMD_small_pop_path <- "./data/IMD_data/File_6_-_IoD2019_Population_Denominators.xlsx"
IMD_small_scores_path <- "./data/IMD_data/File_5_-_IoD2019_Scores.xlsx"
IMD_small_Eng_path <- "./data/IMD_data/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"

#IMD_small_Scot_path <- "./data/IMD_data/SIMD+2020v2+-+ranks.xlsx"


### England analysis ###
IMD_small_Eng <- read_excel(IMD_small_Eng_path, sheet = "IMD2019")
IMD_small_pop <- read_excel(IMD_small_pop_path, sheet = 'Population Denominators')
IMD_small_scores <- read_excel(IMD_small_scores_path, sheet = "IoD2019 Scores")
IMD_LA_Eng <- read_excel(IMD_LA_Eng_path, sheet = "IMD")

#Save LAD code to LAD name conversion
LAD_code_name <- IMD_small_Eng %>%
  mutate(LAD_code = `Local Authority District code (2019)`,
         LAD = `Local Authority District name (2019)`) %>%
  select(LAD, LAD_code) %>%
  unique()

IMD_small_Eng_aggregated <- IMD_small_Eng %>%
  left_join(IMD_small_pop) %>%
  left_join(IMD_small_scores) %>% 
  #All data sources have same column names for comparable variables
  rename(LAD_code = `Local Authority District code (2019)`,
         LSOA_code = `LSOA code (2011)`,
         Total_pop = `Total population: mid 2015 (excluding prisoners)`,
         IMD_score = `Index of Multiple Deprivation (IMD) Score`,
         IMD_decile = `Index of Multiple Deprivation (IMD) Decile`)%>%
  group_by(LAD_code) %>%
  summarise(IMD_Eng_mean_score = weighted.mean(x = IMD_score, w = Total_pop),
            IMD_Eng_lowest10_frac = sum(IMD_decile == 1)/n()) %>%
  mutate(date = as.Date('2019-01-01')) %>%
  #Add name back in
  left_join(LAD_code_name)
    
IMD_data <- list(IMD_Eng_lowest10_frac = select(IMD_small_Eng_aggregated, -IMD_Eng_mean_score),
                 IMD_Eng_mean_score = select(IMD_small_Eng_aggregated, -IMD_Eng_lowest10_frac))
IMD_metadata <- list(
  IMD_Eng_lowest10_frac = list(
    description = 'Fraction of small areas ranked in bottom 10% in index of multiple deprivation (England)',
    source = 'Locally saved data',
    unit = ''),
  IMD_Eng_mean_score = list(
    description = 'Average index of multiple deprivation score (England)',
    source = 'Locally saved data',
    unit = ''
  ))

# comparison <- full_join(IMD_small_Eng_aggregated, IMD_LA_Eng, 
#                         by = c("LAD_code" = "Local Authority District code (2019)")) %>%
#   mutate(mean_score_diff_frac = (`IMD - Average score` - mean_score)/`IMD - Average score`,
#          lowest_10_diff_frac = ((`IMD - Proportion of LSOAs in most deprived 10% nationally` - lowest10_frac)/`IMD - Proportion of LSOAs in most deprived 10% nationally`))



#print(all.equal(comparison$`IMD - Average score`, comparison$mean_score, tolerance = 0.001))

### Scotland analysis ###
#IMD_small_Scot <- read_excel(IMD_small_Scot_path, sheet = 'SIMD 2020v2 ranks')