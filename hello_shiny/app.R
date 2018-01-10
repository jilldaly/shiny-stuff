library(shiny)
library(ggplot2)

ui <- fluidPage(  
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
  
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"')
      
      ),

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "alpha1HistPlot"),
      plotOutput(outputId = "beta2HistPlot"),
      uiOutput(outputId="selectXAxis"),
      plotOutput(outputId = "scatterPlot")
    )
  )
)

server <- function(input, output) {
  
  readfile <- reactive({
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
  })
  

  output$alpha1HistPlot <- renderPlot({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    
    hist(readfile()[,1], 
         col = "#75AADB", border = "white",
         xlab = "Alpha1",  main = "Alpha1")
  })
  
  output$beta2HistPlot <- renderPlot({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    
    hist(readfile()[,2], col = "#75AADB", border = "white",
         xlab = "Beta2", main = "Beta2")
  })
  
  output$selectXAxis <- renderUI({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    
    selectInput("xvar", "X Axes:",
                choices=colnames( readfile() )
    )
  })
  
  
  # Generate a plot of the 
  output$scatterPlot <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    
    df <- readfile()

    x_axis <- input$selectXAxis
    y_axis <- 3 - as.numeric(input$selectXAxis)
    
  cat(x_axis, y_axis)
    
    p <- ggplot(df, aes(x=x_axis, y=y_axis)) +
      geom_point(shape=1) +    # Use hollow circles
      geom_smooth() + # Add a loess smoothed fit curve with confidence region
      ggtitle("Alpha1 v Beta2") +
      theme(plot.title = element_text(size=20, hjust=0.5))
    print(p)
  })  
}

shinyApp(ui = ui, server = server)
