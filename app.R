
library(shiny)
library(tidyverse)
library(lubridate)
library(shinyWidgets)

#Set up the list of dates that can be selected in the app
current <- ymd('2015-01-01') #This is the first date for the slider
new <- current
date_list <- lubridate::ymd() #empty list of dates
while(new < Sys.Date()) {
  date_list <- c(date_list, new)
  current <- new
  new <- current + months(3)
}
date_list <- format(date_list,'%d %b %Y')

source("code/ImportData.r")
source("code/CreatePanelDatasetLAD.r")
source("code/BubblePlots.R")


#Note this will read in all the data from API's, which might never be used

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Trussell Trust Data Analysis"),
  
  tabsetPanel(
    tabPanel('Instructions'),
    
    tabPanel('Main',
      sidebarLayout(
        sidebarPanel(
          selectInput("x_select", "choose value to go on x axis", choices = names(explanatory_vars)), 
          selectInput("y_select", "choose value to go on y axis", choices = names(explanatory_vars)),
          sliderTextInput('date_select', 'Slide to choose date', choices = date_list,
                      selected = date_list[length(date_list)]),
          actionButton('left_date_button','<'),
          actionButton('right_date_button','>')
                  
    ),
    
    mainPanel(
      plotlyOutput("bubble_plot"),
      textOutput('x_axis_description'),
      textOutput('y_axis_description')
      )
    )
  ),
    tabPanel('LAD selection',
             checkboxGroupInput('LAD_selector', 'Select Local Authority Districts', choices = LAD_list)
             )
  ),
  
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  panel_dataset_return <- reactive({
    input$date_select
    input$LAD_selector
    
    
    date_selected <- as.Date(input$date_select, format = '%d %b %Y')
    
    func_return <- create_panel_dataset(LAD_panel, explanatory_vars, 
                                       explanatory_metadata, date_selected)
    
    func_return$panel_dataset <- func_return$panel_dataset %>%
      mutate(selected_bool = LAD %in% input$LAD_selector)
    
    func_return
    
  })
  
  #We want plot limits to remain constant as we scroll through time - they
  #depend only on the variables chosen. So make a reactive variable
  
  plot_limits <- reactive({
    
    x_data <- explanatory_vars[[input$x_select]][input$x_select]
    xlims <- c(0.9*min(x_data), 1.1*max(x_data))
    
    y_data <- explanatory_vars[[input$y_select]][input$y_select]
    ylims <- c(0.9*min(y_data), 1.1*max(y_data)) 
    
    list(x = xlims, y = ylims)
      
  })
  
  output$bubble_plot <- renderPlotly({
    
    panel_dataset_return()

    panel_dataset <- panel_dataset_return()$panel_dataset
    metadata <- panel_dataset_return()$metadata
    
    Plot_bubble_interactive(panel_dataset, x_axis = input$x_select, 
                            y_axis = input$y_select, limits = plot_limits())
    
  })
  
  #The action buttons with arrows on move the date slider by one unit left/right
  observeEvent(input$left_date_button, {
    index = which(date_list == input$date_select)
    if(index > 1){
      new_date <- date_list[[index-1]]
    } else {
      new_date <- date_list[[index]]
    }
    
    updateSliderTextInput(session, 'date_select', selected = new_date)
  })
  
  observeEvent(input$right_date_button, {
    index = which(date_list == input$date_select)
    if(index < length(date_list)){
      new_date <- date_list[[index+1]]
    } else {
      new_date <- date_list[[index]]
    }
    
    updateSliderTextInput(session, 'date_select', selected = new_date)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
