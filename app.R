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
            tags$hr(),
        
        tabsetPanel(
            tabPanel("Budget",
                     textOutput("summary"),
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
        tabPanel("Project Income", 
                 
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("timeframe", "Pay Cycle", 
                                      choices = c("Weekly" = 52, 
                                                  "Fortnightly" = 26, 
                                                  "Monthly" = 12)),
                         numericInput("hours", "Hours per Pay Cycle", value = 0),
                         numericInput("rate", "Rate", value = 0),
                         numericInput("save", "Savings Per Pay Cycle", value = 0)
                     ),
                     mainPanel(
                         dataTableOutput("CumSum"),
                         plotOutput("ProjIncome")
                     )
                 )
                 
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
    
    output$summary <- renderText({
        
        totInc <- sum(IncomeTab$data$Amount)
        totExp <- sum(ExpenseTab$data$Amount)
        totSav <- sum(SavingsTab$data$Amount)
        
        rem <- totInc - totExp - totSav
        
        paste0("In your budget for this period, you have earned, $", 
               totInc, 
               ". Your planned expenses come to a total of, $", 
               totExp, 
               " and you plan to put away, $", 
               totSav, 
               ". This leaves you with $", rem, " for miscellaneous expenses. ")
    })
    
    #### Projections tab #### 
    
    output$CumSum <- renderDataTable({

        month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        dat <- data_frame(Month = month, 
                          Pay = round((input$hours * input$rate * as.numeric(input$timeframe))/12, 2), 
                          Savings = round((input$save * as.numeric(input$timeframe))/12,2)) %>% 
            mutate(CumPay = round(cumsum(Pay),2), 
                   CumSave = round(cumsum(Savings),2)) %>%
            select(c("Month", "CumPay", "CumSave")) %>%
            pivot_longer(cols = c(CumPay, CumSave), names_to = "Amount") %>% 
            pivot_wider(names_from = Month, values_from = value)
        
        datatable(dat)
        
    })
    
    output$ProjIncome <- renderPlot({
        month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        dat <- data_frame(Month = month, 
                          Pay = round((input$hours * input$rate * as.numeric(input$timeframe))/12, 2), 
                          Savings = round((input$save * as.numeric(input$timeframe))/12,2)) %>% 
            mutate(CumPay = round(cumsum(Pay),2), 
                   CumSave = round(cumsum(Savings),2))
        
        ggplot(data = dat, aes(x = factor(Month, levels =  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))) +
            geom_point(aes(y = CumPay, color = "Cumulative Pay") ) + 
            geom_point(aes(y = CumSave, color = "Cumulative Savings") ) +
            labs(x = "Month", 
                   y = "Dollars") + 
            scale_color_manual(values = c("lightskyblue", "salmon")) + 
            theme_bw()
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
