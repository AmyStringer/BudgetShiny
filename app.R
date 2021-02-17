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

# Define UI for application that makes a budget and projects income 
ui <- fluidPage(
    # Application title
    theme = shinythemes::shinytheme(theme = "journal"),
    navbarPage(title = "Budget Helper",
        tabPanel("Welcome"
                 
                 
                 
                 ), 
        tabPanel("Budget",
            fluidRow(
                column(width = 4, 
                    radioButtons("timeFrame", "Select One", 
                                 choices = c("Fortnightly" = 26, 
                                             "Monthly" = 12))
                ),
                column(width = 4, 
                    radioButtons("SavType", "Savings = ($ or %)", 
                                 choices = c("$", "% (1-100)"))     
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
            tabPanel("Budget",
                     
                     column(width = 4,
                        
                         h5("To remove rows from the income table, select them, and then hit the remove button below.") ,   
                         actionButton("remInc", "Remove"),
                         dataTableOutput("Income")
                     ),
                     column(width = 4,
                         
                         h5("To remove rows from the expense table, select them, and then hit the remove button below.") ,   
                         actionButton("remExp", "Remove"),
                         dataTableOutput("Expenses") 
                     ),
                     column(width = 4,
                     
                         h5("To remove a rows from the savings table, select them, and then hit the remove button below.") ,  
                         actionButton("remSav", "Remove"), 
                         dataTableOutput("Savings")      
                     
                     )
                     
                     ), 

            tabPanel("Visualisation", 
                     
                     plotOutput("project")
                     
                     )
        )
        ), 
        tabPanel("Project Income" 
                 
                 
                 )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    #### income tab server #### 
    IncomeTab <- reactiveValues(data = NULL)
    
    observeEvent(input$addInc, {
        temp <- data_frame(`Ref. Name` = input$incomeName, 
                           Amount = input$incomeVal)
        
        IncomeTab$data <- bind_rows(temp, IncomeTab$data) 
    })
    
    observeEvent(input$remInc, {
        
        IncomeTab$data <- IncomeTab$data[-input$Income_rows_selected, ] 
    })
    
    output$Income <- renderDataTable({
        temp <- data.frame(Total = "Total", Amount = sum(IncomeTab$data$Amount))
        tot <- bind_rows(IncomeTab$data, temp)
        datatable(tot, selection = "multiple")
    })
    
    #### expenses tab server #### 
    ExpenseTab <- reactiveValues(data = NULL)
    
    observeEvent(input$addExp, {
        temp <- data_frame(`Ref. Name` = input$expenseName, 
                           Amount = input$expenseVal)
        
        ExpenseTab$data <- bind_rows(temp, ExpenseTab$data) 
    })
    
    observeEvent(input$remExp, {
        
        ExpenseTab$data <- ExpenseTab$data[-input$Expenses_rows_selected, ] 
    })
    
    output$Expenses <- renderDataTable({
        temp <- data.frame(Total = "Total", Amount = sum(ExpenseTab$data$Amount))
        tot <- bind_rows(ExpenseTab$data, temp)
        datatable(tot, selection = "multiple")
    })
    
    #### Savings Tab ####
    SavingsTab <- reactiveValues(data = NULL)
    
    observeEvent(input$addSav, {
        
        if (input$SavType == "$"){
            temp <- data_frame(`Ref. Name` = input$savName, 
                                Amount = input$savVal)
        } else if (input$SavType == "% (1-100)"){
            sumInc <- sum(IncomeTab$data$Amount)
            scale <- input$savVal/100
            totSav <- sumInc * scale
            
            temp <- data_frame(`Ref. Name` = input$savName, 
                               Amount = totSav)
        }
        
        SavingsTab$data <- bind_rows(temp, SavingsTab$data) 
    })
    
    observeEvent(input$remSav, {
        
        SavingsTab$data <- SavingsTab$data[-input$Savings_rows_selected, ] 
    })
    
    output$Savings <- renderDataTable({
        temp <- data.frame(Total = "Total", Amount = sum(SavingsTab$data$Amount))
        tot <- bind_rows(SavingsTab$data, temp)
        datatable(tot, selection = "multiple")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
