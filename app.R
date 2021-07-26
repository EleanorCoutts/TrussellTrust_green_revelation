
library(shiny)
library(tidyverse)
library(lubridate)
library(shinyWidgets)

#Set up the list of dates that can be selected in the app
current <- ymd('2015-01-01') #This is the first date for the slider
new <- current
date_list_ymd <- lubridate::ymd() #empty list of dates
while(new < Sys.Date()) {
  date_list_ymd <- c(date_list_ymd, new)
  current <- new
  new <- current + months(3)
}



date_list <- format(date_list_ymd,'%d %b %Y')

date_list_ymd <- as.character(date_list_ymd)

source("code/ImportData.r")
source("./code/read_IMD.r")
source("code/CreatePanelDatasetLAD.r")
source("code/BubblePlots.R")


#Note this will read in all the data from API's, which might never be used

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Trussell Trust Data Analysis"),
  
  tabsetPanel(
    tabPanel('Instructions',
             includeMarkdown("./code/App_instructions.md")),
    
    tabPanel('Main',
      sidebarLayout(
        sidebarPanel(
          selectInput("x_select", "choose value to go on x axis", choices = explanatory_metadata_description), 
          checkboxInput("x_log", "Logarithmic scale on x axis?", value = FALSE),
          
          selectInput("y_select", "choose value to go on y axis", choices = explanatory_metadata_description),
          checkboxInput("y_log", "Logarithmic scale on y axis?", value = FALSE),
          
          sliderTextInput('date_select', 'Slide to choose date', choices = date_list,
                      selected = date_list[length(date_list)]),
          actionButton('left_date_button','<'),
          actionButton('right_date_button','>'),
          
          fluidRow(
            downloadButton('download_data', label = 'Download underlying data')
            )
          
                  
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
  
  snapshot_dataset_return <- reactive({
    input$date_select
    input$LAD_selector
    
    
    date_selected <- as.Date(input$date_select, format = '%d %b %Y')
    
    func_return <- create_snapshot_dataset(LAD_panel, explanatory_vars, 
                                       explanatory_metadata, date_selected)
    
    func_return$dataset <- func_return$dataset %>%
      mutate(selected_bool = LAD %in% input$LAD_selector)
    
    func_return
    
  })
  
  panel_dataset_return <- reactive(
    create_panel_dataset_time(LAD_panel, explanatory_vars, 
                              explanatory_metadata, date_list_ymd)
  )
  
  
  
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
    

    snapshot_dataset <- snapshot_dataset_return()$dataset
    metadata <- snapshot_dataset_return()$metadata
    
    annotation <- paste0('Foodbank data from: ', metadata$Foodbank_data$chosen_date, '.\n',
                         metadata[[input$x_select]]$description, ' data from: ', metadata[[input$x_select]]$chosen_date, '.\n',
                         metadata[[input$y_select]]$description, ' data from: ', metadata[[input$y_select]]$chosen_date)
    
    
    x_axis_lab <- metadata[[input$x_select]]$description
    if(nchar(metadata[[input$x_select]]$unit)>0){
      x_axis_lab <- paste0(x_axis_lab, " (", metadata[[input$x_select]]$unit, ")")
    }
    
    y_axis_lab <- metadata[[input$y_select]]$description
    if(nchar(metadata[[input$y_select]]$unit)>0){
      y_axis_lab <- paste0(y_axis_lab, " (", metadata[[input$y_select]]$unit, ")")
    }    
    
    Plot_bubble_interactive(snapshot_dataset, x_axis = input$x_select, 
                            y_axis = input$y_select, limits = plot_limits(),
                            x_axis_name = x_axis_lab,
                            y_axis_name = y_axis_lab,
                            x_log = input$x_log,
                            y_log = input$y_log,
                            annotation = annotation)
    
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
  
  output$download_data <- downloadHandler(filename = 'Bubble_plots_data_download.xlsx',
                                          content = function(file){
                                          write_panel_data_to_file(file, 
                                            isolate(panel_dataset_return()$data), 
                                            isolate(panel_dataset_return()$metadata))
                                          })
}

# Run the application 
shinyApp(ui = ui, server = server)
