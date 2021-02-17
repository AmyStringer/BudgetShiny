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

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Budget Constructer"),
    theme = shinythemes::shinytheme(theme = "slate"),
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
                actionButton("AddInc", "Add")
            ), 
            column(width = 4,
                h3("Add Expenses"),
                textInput("expenseName", "Ref.Name"),
                numericInput("expenseVal", "Amount", ""),
                actionButton("AddExp", "Add")
            ), 
            column(width = 4, 
                h3("Add Savings"), 
                textInput("savName", "Ref.Name"),
                numericInput("savVal", "Amount", ""),
                actionButton("AddSav", "Add")
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
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
