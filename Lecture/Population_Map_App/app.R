#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(spData)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Population Map"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         # sliderInput("bins",
         #             "Number of bins:",
         #             min = 1,
         #             max = 50,
         #             value = 30),
         # 
         sliderInput("year",
                     "Year:",
                     min = 1950,
                     max = 2030,
                     value = 2020,
                     step = 5,
                     sep = ""),
         
         numericInput("pop",
                      "Minimum Population (Millions):",
                      min = 0,
                      max = 40,
                      value = 1,
                      step = .5)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         # plotOutput("distPlot"),
         leafletOutput("map"),
         dataTableOutput("table")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
   
   output$map <- renderLeaflet({ 
           
           filter(urban_agglomerations, 
                  year == input$year,
                  population_millions > input$pop) %>% 
                   leaflet() %>%
                   addTiles() %>% 
                   addMarkers()
           
           })
   
   output$table = renderDataTable({
           filter(urban_agglomerations, 
                  year == input$year,
                  population_millions > input$pop)
           
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

