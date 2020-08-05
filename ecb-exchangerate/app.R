###############################################################################
#                           INSTALL / IMPORT PACKAGES                         #
###############################################################################
for (package in c("shiny", "plotly", "shinythemes", "dplyr", "magrittr", "pdftools", "stringr", "DT", "lubridate", "quantmod")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# This is for deploying to shiny.io (not allowed install packages)
# link to deployed app: https://jilldaly.shinyapps.io/euroexchange/
#library("shiny")
#library("plotly")
#library("shinythemes")
#library("dplyr")
#library("magrittr")
#library("pdftools")
#library("stringr")
#library("DT")
#library("lubridate")
#library("quantmod")

###############################################################################
#             CREATE ONCE, SHARE ACROSS ALL USER SESSIONS                     #
###############################################################################

# Download the files and create a dataframe
csvURL <- "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip"
download.file(csvURL, dest="dataset.zip", mode="wb") 
file <- unzip ("dataset.zip", exdir = "./")

# A read-only data set that will load once, when Shiny starts, and will be
# available to each user session
currency_data <- read.csv("eurofxref-hist.csv",  # name of file
                          header = TRUE,             # column names
                          sep = ",",                 # comma separated rows
                          na.string = "N/A",          # could be "." for SAS files
                          skip = 0,                  # number of rows to skip at the top of the file.  
                          strip.white = TRUE,        # strip out extra white space in strings.
                          fill = TRUE,               # fill in rows that have unequal numbers of columns
                          comment.char = "#",        # character used for comments that should not be read in
                          stringsAsFactors = FALSE   # Another control for deciding whether characters should be converted to factor
)

# Convert the string N/As to actual NAs 
currency_data[currency_data == "N/A"] <- NA

# Deliberately not using the subset function due to warning in api doc 
# (https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/subset)
# Using dplyr filter to to only keep specified currencies, and omit NAs.
currency_data <- currency_data %>%
      select(Date, USD, CNY, JPY, INR, GBP, NOK, SEK, CAD, DKK, RUB) %>%
      na.omit() 

# Download Reference for the Currencies
referencePDFUrl = "http://www.ecb.europa.eu/stats/shared/pdf/eurofxref.pdf"
pdfFile <- "euroreference.pdf"
referencePDFFile <- download.file(referencePDFUrl, dest=pdfFile, mode="wb") 
pdf_table <- pdf_text(pdfFile) %>% 
  str_split("\n") %>% 
  .[[1]] %>%
  str_subset("([A-Z]{3})")

# Convert the references to a lookp table
currency_lookup <- pdf_table %>%
  str_split_fixed("\\s{3,}", n = 3) %>% 
  as_data_frame() %>% 
  filter(V1 %in% colnames(currency_data)) %>%
  rename(Code = V1, Description = V2, Rate = V3) %>%
  mutate(Display = paste(Code, Description, sep = ' - '))

# Add the Display as the label for the Currency Input Selectors
choices <- currency_lookup$Code
names(choices) <- currency_lookup$Display

# We don't need the Display column any more
currency_lookup <- currency_lookup %>%
  select (-c(Display)) 


###############################################################################
#                   UI :: RUN ONCE, HTML CACHED FOR ALL SESSION               #
###############################################################################
ui <- fluidPage(
              # Set theme
              theme = shinytheme("flatly"),
  
              titlePanel("Euro Foreign Exchange Rates"),
              hr(),
              
              fluidRow(
                column(12,
                       # Place the performance stats front and center
                       htmlOutput(outputId = "yearperformance"),
                       tags$a(href = "https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html", "Source: European Central Bank", target = "_blank"),
                       br(),
                       br()
                ),
                column(12,
                       fluidRow(
                         br(),
                         column(5,
                              # Select currency to plot
                              selectInput(inputId = "selectCurrency", label = strong("Currency"),
                                          choices = choices, 
                                          selected = "USD",
                                          multiple = FALSE)
                          ),
                          column(7,
                                # Select date range to change the plot as needed
                                dateRangeInput(inputId = "selectDate", label = strong("Date range"), 
                                               start = min(currency_data$Date), end = max(currency_data$Date),
                                               min = min(currency_data$Date), max = max(currency_data$Date))                       
                          )
                  )
              ),
              column(12,
                     # Allow the user to reset the date range (for usability)
                     actionLink(inputId = "resetDates", label = "Reset Date Range"),
                     tags$style(type='text/css', "#button { align: right; height- 10px; width- 100%;}"),
                     br() 
              ),
              column(12,
                     br(), 
                     tabsetPanel(
                       tabPanel("Chart", plotlyOutput(outputId = "timeseries", width = "100%", height = "600px")), 
                       tabPanel("Statistics", htmlOutput(outputId = "stats")), 
                       tabPanel("Data", DT::dataTableOutput(outputId = "datatable")), 
                       tabPanel("Summary", DT::dataTableOutput(outputId = "lookuptable")) 
                     )
              )
        )
)
###############################################################################
#                         SERVER :: RUN PER-SESSION                           #
###############################################################################
server <- function(input, output, session) {

        # not needed, but leaving this here as an example of how to debug at a low level
        cat(file=stderr(), "In Server function, demonstrating a debug line", "\n")
  

        # Reset the Date Range to the original value  
        observeEvent(input$resetDates, {
              updateDateRangeInput(session, "selectDate",
                    start = min(currency_data$Date), end = max(currency_data$Date)
              )        
        })  
        
        
        # Display the Performance Stats
        output$yearperformance <- renderUI({
          
              # extract the vector for the selected currency
              CURR <- currency_data[, which(colnames(currency_data)==input$selectCurrency)] %>%
                        as.numeric()
              
              year_data <- currency_data %>%
                    select(Date, input$selectCurrency)  %>%      
                    filter(Date > ymd(Sys.Date()) - years(1)) 
              
              yr_performance <- tail(year_data, n=1)
              yr_performance[[2]] <- as.numeric(yr_performance[[2]])
              yr_diff <- (CURR[[1]] - yr_performance[[2]])
              yr_perf <- paste(round(yr_diff / CURR[[1]],digits=2)*100,"%",sep="")
              
              yr_str <- sprintf("<h6>Year Performance (%s): <span style=\"color:#003399\">EURO = %s %.4f </span> <small><span style=\"color:%s\">%s%.4f (%s%s)</span></small></h6>", 
                      format(as.Date(yr_performance[[1]]), "%B %d %Y"), 
                      input$selectCurrency, 
                      yr_performance[[2]],
                      ifelse(yr_diff > 0, "green", "red"),
                      ifelse(yr_diff > 0, "+", ""),
                      yr_diff, 
                      ifelse(yr_diff > 0, "+", ""),
                      yr_perf,
                      sep = '<br/>')
          
              day_diff <- CURR[[1]] - CURR[[2]] 
              latest <- sprintf("<h6>Latest Performance (%s): <span style=\"color:#003399\">EUR 1 = %s %.4f </span><small><span style=\"color:%s\">%s%.4f (%s%s)</span></small></h6>", 
                                format(Sys.Date(), "%B %d %Y"), 
                                input$selectCurrency, 
                                CURR[[1]],
                                ifelse(day_diff > 0, "green", "red"),
                                ifelse(day_diff > 0, "+", ""),
                                day_diff,
                                ifelse(day_diff > 0, "+", ""),
                                paste(round(day_diff/CURR[[1]],digits=6)*100,"%",sep=""))
              
              # Display the text as html
              HTML(paste("", latest, yr_str, "", sep=""))
        })
      
    
        # Create chart 
        output$timeseries <- renderPlotly({
    
              # extract the vector for the selected currency 
              CURR <- currency_data[,which(colnames(currency_data)==input$selectCurrency)]
          
              # filter by date
              plot_data <- currency_data %>%
                            select(Date, input$selectCurrency)  %>%
                            #filter(CURR != "N/A") %>%
                            filter(Date >= as.Date(input$selectDate[1]) & Date <= as.Date(input$selectDate[2])) 
            
              # extract the vector for the selected currency 
              Y_CURR <- plot_data[,which(colnames(plot_data)==input$selectCurrency)]

              # construct the chart
              p <- plot_ly(plot_data, x = ~plot_data$Date) %>% 
                add_lines(y = ~Y_CURR, name = input$selectCurrency) %>%
                config(displayModeBar = F) %>%
                layout(title = paste("EUR v", input$selectCurrency),
                      showlegend = F, hovermode = 'compare',
                      margin = list(t = 100),
                      yaxis = list(title = "Rate", nticks = 15),
                      xaxis = list(
                        title = '', # hide axis title as it interferes with the text and it's apaprent that its a date
                        tickangle = 45,
                        nticks = 15,
                        rangeslider = list(type = "date")))
                
              # call the plotly function  
              p
        })    
      
        
        output$stats <- renderUI({
        
              # extract the vector for the selected currency
              CURR <- currency_data[, which(colnames(currency_data)==input$selectCurrency)] %>%
                        as.numeric()
              
              year_data <- currency_data %>%
                    select(Date, input$selectCurrency)  %>%      
                    filter(Date > ymd(Sys.Date()) - years(1))
              
              yr_performance <- tail(year_data, n=1)
              
              yr_str <- sprintf("<h3>Changes from %s to %s</h3>", 
                                format(as.Date(yr_performance[[1]]), "%B %d %Y"),
                                format(Sys.Date(), "%B %d %Y"))

              min_rate <- year_data[which(year_data[[2]] == min(year_data[[2]])), ]
              max_rate <- year_data[which(year_data[[2]] == max(year_data[[2]])), ]
              
              min_str <- paste("<li>Minimum Rate (",format(as.Date(min_rate[[1]]), "%B %d %Y"),") of ", min_rate[[2]], "</li>", sep = "")    
              max_str <- paste("<li>Maxmum Rate (",format(as.Date(max_rate[[1]]), "%B %d %Y"),") of ", max_rate[[2]], "</li>", sep = "")    
              avg_str <- paste("<li>Average Rate of ",round(mean(year_data[[2]]), digits = 4), "</li>", sep = "")  
              
              # Display the message/text using html
              HTML(paste("", yr_str, "<ul>", min_str, max_str, avg_str, "</ul>", sep = '<br/>'))
    
      })
      
        
      # Display the Currency Lookup Data  
      output$lookuptable <- DT::renderDataTable(
            DT::datatable(currency_lookup, 
                          options = list(paging = FALSE, searching = FALSE))  %>% 
              formatStyle(colnames(currency_lookup),  color = 'black', fontWeight = 'bold')
      )
     
      # Display the source Data
      output$datatable <- DT::renderDataTable(
            DT::datatable(currency_data, 
                    extensions = 'Scroller', 
                    options = list(
                      deferRender = TRUE,
                      scrollY = 200,
                      scroller = TRUE,
                      color = 'black', 
                      fontWeight = 'bold')
            )
      )
   
}

shinyApp(ui = ui, server = server)
