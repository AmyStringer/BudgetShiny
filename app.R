#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Budget Constructer"),
    theme = shinythemes::shinytheme(theme = "journal"),
        fluidRow(
            column(width = 4, 
                radioButtons("timeFrame", "Select One", 
                             choices = c("Fortnightly", 
                                         "Monthly", 
                                         "Yearly"))
            ),
            column(width = 4, 
                radioButtons("SavType", "Savings = ($ or %)", 
                             choices = c("$", "%"))     
            )
            
        ),
        fluidRow(
            
            column(width = 4,
                h3("Add Income"),
                textInput("incomeName", "Ref. Name"), 
                numericInput("incomeVal", "Amount", value = ""), 
                actionButton("addInc", "Add")
            ), 
            column(width = 4,
                h3("Add Expenses"),
                textInput("expenseName", "Ref.Name"),
                numericInput("expenseVal", "Amount", ""),
                actionButton("addExp", "Add")
            ), 
            column(width = 4, 
                h3("Add Savings"), 
                textInput("savName", "Ref.Name"),
                numericInput("savVal", "Amount", ""),
                actionButton("addSav", "Add")
                )
            
        ),
    
    tabsetPanel(
        tabPanel("Income",
                 
                 dataTableOutput("Income")
                 
                 ), 
        tabPanel("Expenses",
                 
                 dataTableOutput("Expenses")
                 
                 ), 
        tabPanel("Summary",
                 
                 dataTableOutput("Summary")
                 
                 )
    )
    )



# Define server logic required to draw a histogram
server <- function(input, output) {

    IncomeTab <- reactiveValues(data = NULL)
    
    observeEvent(input$addInc, {
        temp <- data_frame(`Ref. Name` = input$incomeName, 
                           Amount = input$incomeVal)
        
        IncomeTab$data <- bind_rows(temp, IncomeTab$data)
    })
    
    output$Income <- renderDataTable({
        IncomeTab$data
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
