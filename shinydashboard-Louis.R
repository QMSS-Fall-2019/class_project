library(shiny)
library(dplyr)
library(plyr)
library(data.table)
library(ggplot2)

FDIC <- read.csv(file.choose())

# Begin with changing the types back from factors to make generating the bar plot easier.
FDIC$Product <- as.character(FDIC$Product)
FDIC$Company <- as.character(FDIC$Company)
FDIC$Date.received <- as.Date(FDIC$Date.received) 

# We also need to shift the Date.received column to work better with in the range interface
# in the Shiny UI (i.e. shifting form 0001 to 2001). The first value added shifts the dates up from 0000 to 
# 1970 as the reference year and the second value shifts them to 2000 as the reference.
FDIC$Date.received <- FDIC$Date.received + abs(as.numeric(as.Date("0000/01/01"))) + abs(as.numeric(as.Date("2000/01/01")))

ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Product Category"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput(inputId="product", label="Product Category:", 
                  choices=unique(FDIC$Product)),
      
      selectInput(inputId="company", label="Company:", 
                  choices=unique(FDIC$Company)),
      
      dateRangeInput('dateRange2',
                     label = "Choose a start and end date:",
                     start = Sys.Date() - 3, end = Sys.Date() + 3,
                     min = "2001-01-20", max = Sys.Date() + 5,
                     separator = " - ", format = "yyyy/mm/dd",
                     startview = 'year', language = 'en', weekstart = 7
      ),
      
      hr(),
      
      helpText("Data from FDIC.")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("productCountPlot"),
    )
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
    df <- FDIC %>%
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
      geom_bar(stat="identity", color="blue") +
      theme_minimal()
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
