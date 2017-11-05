library(shiny)
library(ggplot2)  # for the diamonds dataset

expense <- read.csv("Expenses.csv")
expense$Amount <- abs(expense$Amount......)
expense$Amount...... <- NULL
names(expense)
head(expense)

ui <- fluidPage(
  titlePanel("Expenses"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset == "Table"',
        checkboxGroupInput("show_vars", "Columns in expense to show:",
                           names(expense), selected = names(expense)),
        br(),
        sliderInput("min","Minimum Amount to Display",
                    value = 20,
                    min = 0,
                    max = 3000),
        sliderInput("max","Maximum Amount to Display",
                    value =1000,
                    min = 0,
                    max = 3000)
      ),
      conditionalPanel(
        'input.dataset == "Plot"',
        radioButtons("sum_vars", "Variable to be Summarized:",
                           names(expense), selected = names(expense)[1])
        )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Table", DT::dataTableOutput("mytable1")),
        tabPanel("Plot", plotOutput("plot"))
      )
    )
  )
)

server <- function(input, output) {
 
  #Filter dataset for display by min and max choices 
  expense2 <- reactive({
    validate(
      need(input$min <= input$max, 'minimum value cannot be greater than maximum value!')
    )
    subset(expense, expense$Amount >= input$min & expense$Amount <= input$max)
    })
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(expense2()[, input$show_vars, drop = FALSE])
  })
  
  #Select the variable to be displayed in histogram
  plotvar <- reactive({
    expense[[input$sum_vars]]
  })
  
  output$plot <- renderPlot(
    if(is.numeric(plotvar())) hist(plotvar(), main = "Histogram", )
    else plot(plotvar())
  )
  
}

shinyApp(ui, server)