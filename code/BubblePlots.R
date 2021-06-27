library(ggplot2)
library(dplyr)
library(plotly)

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
Plot_bubble <- function(data, geography, x_axis, y_axis,
                        country_val = NULL){
  data_alt <- data %>%
    mutate(TotalVouchers = sum_vouchers) %>%
    mutate(text = LAD)
  
  plot <- ggplot(data = data_alt, aes_string(x=x_axis, y=y_axis, text="text",
                                             size = "TotalVouchers", color = "country")) +
    geom_point(alpha = 0.5) +
    scale_size(range=c(.1,8),name="TotalVouchers") +
    theme(legend.position="bottom")
    
  plot
}

Plot_bubble_interactive <- function(data, x_axis, y_axis,
                                    x_axis_name=NULL, y_axis_name=NULL, 
                                    short_x_name = NULL, short_y_name = NULL,
                                    pre_x_unit = '', pre_y_unit = '',
                                    post_x_unit = '', post_y_unit = '', 
                                    limits=NULL){
  
  #country to colour mapping
  colours = c('England' = 'red', 'Wales' = 'green', 'Scotland' = 'Blue')
  #Neither StatXplore data nor Nomis data have Northern Ireland, therefore exclude from bubble plots
  
  if(is.null(x_axis_name)){x_axis_name = x_axis}
  if(is.null(y_axis_name)){y_axis_name = y_axis}
  if(is.null(short_x_name)){short_x_name = x_axis_name}
  if(is.null(short_y_name)){short_y_name = y_axis_name}
  
  data <- data %>%
    mutate(x = .data[[x_axis]], y = .data[[y_axis]]) %>%
    filter(country != "Northern Ireland")
  
  fig <- plot_ly(data, x = ~x, y = ~y, color = ~country, colors = colours, 
                 size = ~sum_vouchers, #text = ~LAD, 
                 type = 'scatter', mode = 'markers', 
                 sizes = c(8,40),fill = ~'', opacity = 0.6,
                 marker = list(symbol = 'circle', sizemode = 'diameter'),
                 text = ~paste0('Country: ',country,' <br>LAD: ',LAD,
                               '<br>',short_x_name, ': ', pre_x_unit,x, post_x_unit,
                               '<br>',short_y_name,': ', pre_y_unit, y, post_y_unit,
                               '<br>Foodparcel Vouchers: ',sum_vouchers),
                 hoverinfo = 'text'
                ) %>%
    layout(xaxis = list(title = x_axis_name),
           yaxis = list(title = y_axis_name)) #%>%
    
    if('selected_bool' %in% names(data)){
    
      fig <- fig %>%
        
        add_trace(data = filter(data, selected_bool == TRUE), 
                x = ~x, y = ~y,  
                size = ~sum_vouchers, #text = ~LAD, 
                type = 'scatter', mode = 'markers', 
                sizes = c(8,40),fill = ~'', opacity = 1, showlegend = FALSE,
                name = 'highlight',
                marker = list(symbol = 'circle', sizemode = 'diameter',
                              color = rgb(0,0,0,0),
                              line = list(width = 3, color = rgb(0,0,0,1)))
                )
    }
  
  
  #If given, set the limits
  if(!(is.null(limits))){
    
    fig <- fig %>%
      layout(xaxis = list(range = limits$x), 
             yaxis = list(range = limits$y))
  }
  
  fig
  
}

# test_date = as.Date('2020-01-01')
# test <- create_panel_dataset(LAD_panel, explanatory_vars,
#                        explanatory_metadata, test_date)$panel_dataset
# 
# selected_bool <- rep(FALSE, nrow(test))
# selected_bool[c(1,2,3)] <- TRUE
# test$selected_bool <- selected_bool
#  
# p <- Plot_bubble_interactive(test,
#                         x_axis = "unemployment", y_axis = "workLimitingDisabled")
# 
# p

