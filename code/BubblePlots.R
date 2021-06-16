library(ggplot2)
library(dplyr)

##################################################################################

#Plot bubble plots

##################################################################################

#' Plot a bubble plot
#' 
#' Size of bubble is number of food vouchers, data must have column sum(vouchers).
#' Data must also have columns 'year' and 'Country'.
#' 
#' @param geography Column name that gives the area name (i.e. it specifies the area type)
#' @param x_axis Name of column that will go on the horizontal axis
#' @param y_axis Name of column that will go on the vertical axis
#' @param year_val Filter so that the data is only this year. If NULL, not filtered.
#' @param country_val Filter to only have data from this country. If NULL, not filtered.
Plot_bubble <- function(data, geography, x_axis, y_axis, year_val = NULL, quarter_val = NULL,
                        country_val = NULL){
  data_alt <- data %>%
    mutate(area_name = .data[[geography]]) %>%
    mutate(TotalVouchers = sum_vouchers) %>%
    mutate(x = .data[[x_axis]]) %>%
    mutate(y = .data[[y_axis]])
    #As the column name for the geography varies (e.g. LAD, region...), rename it
  
  if(!is.null(year_val)){
    data_alt <- data_alt %>% filter(year == year_val)
  }
  if(!is.null(quarter_val)){
    data_alt <- data_alt %>% filter(quarter == quarter_val)
  }
  if(!is.null(country_val)){
    data_alt <- data_alt %>% filter(country == country_val)
  }
  
  data_alt
  
  plot <- ggplot(data = data_alt, aes(x=x, y=y, size = TotalVouchers, color = country)) +
    geom_point(alpha = 0.5) +
    scale_size(range=c(.1,8),name="TotalVouchers") +
    xlab(x_axis) +
    ylab(y_axis)
    
  plot
}


# test <- Plot_bubble(LAD_panelALL, geography = 'LAD', 
#                     x_axis = 'Households on Universal Credit', y_axis = 'EstUnemploymentRate',
#                     year_val = 2020, quarter_val = 'Q4', country_val = NULL)
# test
