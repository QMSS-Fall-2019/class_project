library(shiny)
library(dplyr)
library(plyr)
library(data.table)
library(ggplot2)
library(shinyWidgets)

# CFPB <- read.csv(file.choose())
setwd("C:/Users/louis/Google Drive/QMSS Courses/Practicum/Practicum")
CFPB <- read.csv("Consumer_Complaints.csv")

# convert date from mm/dd/yyyy into yyyy/mm/dd
library(lubridate)
CFPB$Date.received <- mdy(CFPB$Date.received)

View(CFPB$Date.received)

ui <- fluidPage(    
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # Give the page a title
  titlePanel("Urgency of Complaints over Time"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      pickerInput(inputId="product", label="Product Category:", 
                  choices=sort(as.character(unique(CFPB$Product))), options = list(`actions-box` = TRUE),
                  multiple = TRUE, selected = "Mortgage"),
      
      pickerInput(inputId="company", label="Company:", 
                  choices=sort(as.character(unique(CFPB$Company))),  options = list(`actions-box` = TRUE),
                  multiple = TRUE, selected = "FLAGSTAR BANK, FSB"),
      
      dateRangeInput('dateRange2',
                     label = "Choose a start and end date:",
                     start = "2011-12-01", end = Sys.Date(),
                     min = "2001-01-20", max = Sys.Date() + 2,
                     separator = " - ", format = "dd/mm/yyyy",
                     startview = 'year', language = 'en', weekstart = 7
      ),
      
      hr(),
      
      helpText("Data from CFPB.")
    ), 
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("productCountPlot"),
    ),
    position = "right", # arranges layout of mainPanel and sideBar with sideBar to the right
  )
)

server <- shinyServer(function(input, output) {
  chosenDateRange <- reactive({
    dates_seq <- as.data.frame(seq.Date(as.Date(input$dateRange2[1]), as.Date(input$dateRange2[2]), by="1 day"))
    #colnames(dates_seq) <- "Date.received"
  })
  
  # Generate a sequence of all the possible dates in the FDIC dataframe and name the column in the dataframe
  #Date.received <- as.data.frame(seq.Date(as.Date(min(FDIC$Date.received, na.rm=TRUE)), as.Date(max(FDIC$Date.received, na.rm=TRUE)), by="1 day"))
  #colnames(Date.received) <- "Date.received"
  
  datasetInput <- reactive({
    # Dynamically filter for the company and product input by the user, then select for dates,
    # and generate a count of the occurrences for each date
    df <- CFPB %>%
      filter(Company == input$company, Product == input$product) %>%
      select("Date.received")%>%
      count(c("Date.received"))
  })
  
  # Fill in the spot we created for a plot
  output$productCountPlot <- renderPlot({
    # Prep the two data frames before merging frequencies from the count table to the 
    # sequence of all possible dates
    datasetInputDf <- datasetInput()
    setDT(datasetInputDf)
    
    Date.received <- as.data.frame(chosenDateRange())
    colnames(Date.received) <- "Date.received"
    setDT(Date.received)
    
    # Merge the frequencies into the sequence of possible dates, generating NA's for dates 
    # with no complaints. The frequencies and dates will be used to generate the dynamic graph
    # viewed by the user.
    dates_counts <- datasetInputDf[Date.received, on = c('Date.received')]
    
    # Render a basic plot
    ggplot(data=dates_counts, aes(x=Date.received, y=freq)) +
      geom_bar(stat="identity", color="#37A6BF") +
      theme_minimal() +
      xlab("Date of Complaint") + 
      ylab("Urgency Level") +
      scale_y_continuous(labels=function(label) sprintf('%15.2f', label))
    # scale_x_continuous(labels=function(label) sprintf('%15.2f', label))
    
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
