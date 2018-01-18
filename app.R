library(shinydashboard)
library(quantmod)
library(ggplot2)

source('VaR.R')

ui <- dashboardPage(
  
  
  dashboardHeader(title = "Portfolio Analysis"),
  dashboardSidebar(
    sidebarMenu(id="tabs",
                menuItem("Stocks", tabName="stocks", icon=icon("line-chart"), selected=TRUE),
                menuItem("Value at Risk", tabName = "VaR", icon=icon("heartbeat"))
    ),
    
    hr(),
    conditionalPanel("input.tabs==stocks",
                     fluidRow(
                       column(1),
                       column(10,
                              checkboxGroupInput("variable", "Stocks of BVM:",
                                                 c("Amazon.com" = "AMZN.MX",
                                                   "Coca-Cola FEMSA" = "KOFL.MX",
                                                   "Grupo Bimbo" = "BIMBOA.MX")),
                              selectizeInput('VaR', 'Select a type of VaR', choices = c("Historical simulation",
                                                                                        "Delta Normal",
                                                                                        "Monte Carlo Simulation",
                                                                                        "EWMA Delta Normal",
                                                                                        "EWMA MonteCarlo")),
                              hr(),
                              dateRangeInput("dates", 
                                             "Date range",
                                             start = as.character(Sys.Date()-365), 
                                             end = as.character(Sys.Date()))
                        
                              )
                          )
                    )
    
  ),
  
  dashboardBody(
             
     tabItems(
              tabItem(tabName = "stocks",
                      fluidRow(
                        title = "Controls",
                        sliderInput("slider", "Number of observations:", 1, 100, 50)
                               )
                        
                      ),
              tabItem(tabName = "VaR",
                            
                        fluidRow(
                          plotOutput("plot", height = 250),
                          verbatimTextOutput("text1")
                                )
                               
                        )
          )
            
      )

)


server <- function(input, output) {
  
  output$text1<-renderText(typeof(input$variable))
  
  output$plot <- renderPlot({
    validate(
      need(input$variable, 'Please choose at least one stock'),
      need(input$VaR != '', 'Please choose a state.')
    )
    getPrices(input$variable,input$dates[1],input$dates[2],input$VaR)
  })
  
  
  
}
 
  
# Run the app
shinyApp(ui, server)

