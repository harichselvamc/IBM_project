# library(shiny)
# library(dplyr)
# library(ggplot2)

# # Load the dataset
# dataset <- read.csv("cleaneddataset.csv")

# ui <- fluidPage(
#   titlePanel("Inflation Analysis"),
  
#   fluidRow(
#     column(width = 12, align = "center",
#            selectInput("country", "Select Country:", choices = unique(dataset$country))
#     )
#   ),
  
#   fluidRow(
#     column(width = 6,
#            h4("Correlation between Inflation and Interest Rate:"),
#            textOutput("correlation_output1"),
#            plotOutput("scatter_plot1")
#     ),
#     column(width = 6,
#            h4("Correlation between Inflation and Unemployment Rate:"),
#            textOutput("correlation_output2"),
#            plotOutput("scatter_plot2")
#     )
#   )
# )

# server <- function(input, output) {
  
#   observe({
#     country_data <- dataset %>% filter(country == input$country)
    
#     # Inflation vs. Interest Rate
#     complete_data1 <- na.omit(data.frame(interest_rate = country_data$Real.interest.rate...., 
#                                          inflation = country_data$`Inflation..consumer.prices..annual...`))
#     correlation1 <- cor(complete_data1$interest_rate, complete_data1$inflation)
    
#     output$correlation_output1 <- renderText({
#       paste("Correlation:", round(correlation1, 2))
#     })
    
#     output$scatter_plot1 <- renderPlot({
#       scatter_plot1 <- ggplot(complete_data1, aes(x = inflation, y = interest_rate)) +
#         geom_point() + 
#         labs(title = paste("Inflation vs Interest Rate -", input$country), x = "Inflation(%)", y = "Interest Rate")
#       print(scatter_plot1)
#     })
    
#     # Inflation vs. Unemployment Rate
#     unemployment_rate <- country_data$Unemployment..total....of.total.labor.force...national.estimate.
#     complete_data2 <- na.omit(data.frame(unemployment_rate, 
#                                          inflation = country_data$`Inflation..consumer.prices..annual...`))
#     correlation2 <- cor(complete_data2$unemployment_rate, complete_data2$inflation)
    
#     output$correlation_output2 <- renderText({
#       paste("Correlation:", round(correlation2, 2))
#     })
    
#     output$scatter_plot2 <- renderPlot({
#       scatter_plot2 <- ggplot(complete_data2, aes(x = inflation, y = unemployment_rate)) +
#         geom_point() + 
#         labs(title = paste("Inflation vs Unemployment Rate -", input$country), x = "Inflation(%)", y = "Unemployment Rate")
#       print(scatter_plot2)
#     })
#   })
# }

# shinyApp(ui = ui, server = server)

library(shiny)
library(dplyr)
library(ggplot2)

# Load the dataset
dataset <- read.csv("cleaneddataset.csv")

ui <- fluidPage(
  titlePanel("Inflation Analysis"),
  
  fluidRow(
    column(width = 12, align = "center",
           selectInput("country", "Select Country:", choices = unique(dataset$country))
    )
  ),
  
  fluidRow(
    column(width = 6,
           h4("Correlation between Inflation and Interest Rate:"),
           textOutput("correlation_output1"),
           plotOutput("scatter_plot1")
    ),
    column(width = 6,
           h4("Correlation between Inflation and Unemployment Rate:"),
           textOutput("correlation_output2"),
           plotOutput("scatter_plot2")
    )
  ),
  
  fluidRow(
    column(width = 12,
           h4("Simple Linear Regression:"),
           textOutput("coefficients"),
           textOutput("coefficients_interpretation")
    )
  )
)

server <- function(input, output) {
  
  observe({
    country_data <- dataset %>% filter(country == input$country)
    
    # Inflation vs. Interest Rate
    complete_data1 <- na.omit(data.frame(interest_rate = country_data$Real.interest.rate...., 
                                         inflation = country_data$`Inflation..consumer.prices..annual...`))
    correlation1 <- cor(complete_data1$interest_rate, complete_data1$inflation)
    
    output$correlation_output1 <- renderText({
      paste("Correlation:", round(correlation1, 2))
    })
    
    output$scatter_plot1 <- renderPlot({
      scatter_plot1 <- ggplot(complete_data1, aes(x = inflation, y = interest_rate)) +
        geom_point() + 
        labs(title = paste("Inflation vs Interest Rate -", input$country), x = "Inflation(%)", y = "Interest Rate")
      print(scatter_plot1)
    })
    
    # Inflation vs. Unemployment Rate
    unemployment_rate <- country_data$Unemployment..total....of.total.labor.force...national.estimate.
    complete_data2 <- na.omit(data.frame(unemployment_rate, 
                                         inflation = country_data$`Inflation..consumer.prices..annual...`))
    correlation2 <- cor(complete_data2$unemployment_rate, complete_data2$inflation)
    
    output$correlation_output2 <- renderText({
      paste("Correlation:", round(correlation2, 2))
    })
    
    output$scatter_plot2 <- renderPlot({
      scatter_plot2 <- ggplot(complete_data2, aes(x = inflation, y = unemployment_rate)) +
        geom_point() + 
        labs(title = paste("Inflation vs Unemployment Rate -", input$country), x = "Inflation(%)", y = "Unemployment Rate")
      print(scatter_plot2)
    })
    
    # Simple Linear Regression
    simple_model <- lm(Real.interest.rate.... ~ Deposit.interest.rate...., data = country_data)
    
    output$coefficients <- renderText({
      paste("When deposit interest rate is zero, the real interest rate is:", round(coef(simple_model)[1], 2), "%")
    })
    
    # Coefficients interpretation
    output$coefficients_interpretation <- renderText({
      paste("If the Deposit interest rate goes up by one unit (let's say 1%), the Real interest rate tends to go down by",
            round(coef(simple_model)[2], 2), "%.")
    })
    
    
  })
}

shinyApp(ui = ui, server = server)

