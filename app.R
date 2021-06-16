
library(shiny)
print(getwd())
source("code/ImportData.r")
source("code/CreatePanelDatasetLAD.r")
source("code/BubblePlots.R")


#Note this will read in all the data from API's, which might never be used

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Trussell Trust Data Analysis"),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x_select", "choose value to go on x axis", choices = names(explanatory_vars)), 
      selectInput("y_select", "choose value to go on y axis", choices = names(explanatory_vars))
    ),
    
    mainPanel(
      plotOutput("bubble_plot")
    )
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$bubble_plot <- renderPlot({Plot_bubble(LAD_panelALL, geography = 'LAD', 
                                                x_axis = input$x_select, y_axis = input$y_select,
                                                year_val = 2020, quarter_val = 'Q4', country_val = NULL)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
