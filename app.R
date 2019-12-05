library(ggplot2)
library(shiny)
library(readr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(textdata)
library(stats)
library(nnet)
library(xts)
library(tsbox)
library(zoo)
library(lubridate)
library(scales)

final_data_1_ <- read_csv("final_data.csv")

ui <- fluidPage(
  
  titlePanel("Geography Team"),
 
              selectInput(inputId = "company",
                          label = "Plot 1 - Select a Company",
                          choices = c("WELLS FARGO & COMPANY", 
                                      "BANK OF AMERICA, NATIONAL ASSOCIATION",
                                      "JPMORGAN CHASE & CO.", 
                                      "CITIBANK, N.A.")),
              
              selectInput(inputId = "product",
                          label = "Plot 1 - Select a Product",
                          choices = c("Credit Card",
                                      "Mortgage",
                                      "Account")),
               
              selectInput(inputId = "city",
                          label = "Sentiment Analysis - Select a City",
                          choices = c("Atlanta",
                                      "Houston",
                                      "Chicago")),
              
              selectInput(inputId = "pop_type",
                          label = "Predictive Modelling - Select",
                          choices = c("Urban",
                                      "Rural")),
              
              selectInput(inputId = "product1",
                          label = "Timeseries - Select a Product",
                          choices = c("Bank account or service",
                                      "Checking of savings account",
                                      "Consumer Loan")),
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
    
             tabPanel("Plot 1", plotOutput(outputId = "bar")),
             
             tabPanel("Sentiment Analysis", plotOutput(outputId = "bar1")),
   
             tabPanel("Predictive Modelling", verbatimTextOutput(outputId = "pred_mod")),
             
             tabPanel("Timeseries", plotOutput(outputId = "timeseries"))
             
    )
    
            )
)


server <- function(input, output) {
  
  output$bar <- renderPlot({
    
    #Mortgage, Credit Card, Account
    
    wells <- filter(final_data_1_, Company == "WELLS FARGO & COMPANY")
    wells_mortgage <- filter(final_data_1_, Company == "WELLS FARGO & COMPANY", prod_group == "Mortgage")
    wells_credit_card <- filter(final_data_1_, Company == "WELLS FARGO & COMPANY", prod_group == "Credit Card")
    wells_account <- filter(final_data_1_, Company == "WELLS FARGO & COMPANY", prod_group == "Account")
    
    boa <- filter(final_data_1_, Company == "BANK OF AMERICA, NATIONAL ASSOCIATION")
    boa_mortgage <- filter(final_data_1_, Company == "BANK OF AMERICA, NATIONAL ASSOCIATION", prod_group == "Mortgage")
    boa_credit_card <- filter(final_data_1_, Company == "BANK OF AMERICA, NATIONAL ASSOCIATION", prod_group == "Credit Card")
    boa_account <- filter(final_data_1_, Company == "BANK OF AMERICA, NATIONAL ASSOCIATION", prod_group == "Account")
    
    citi <- filter(final_data_1_, Company == "CITIBANK, N.A.")
    citi_mortgage <- filter(final_data_1_, Company == "CITIBANK, N.A.", prod_group == "Mortgage")
    citi_credit_card <- filter(final_data_1_, Company == "CITIBANK, N.A.", prod_group == "Credit Card")
    citi_account <- filter(final_data_1_, Company == "CITIBANK, N.A.", prod_group == "Account")
    
    jpm <- filter(final_data_1_, Company == "JPMORGAN CHASE & CO.")
    jpm_mortgage <- filter(final_data_1_, Company == "JPMORGAN CHASE & CO.", prod_group == "Mortgage")
    jpm_credit_card <- filter(final_data_1_, Company == "JPMORGAN CHASE & CO.", prod_group == "Credit Card")
    jpm_account <- filter(final_data_1_, Company == "JPMORGAN CHASE & CO.", prod_group == "Account")

    if (input$company == "WELLS FARGO & COMPANY" & input$product == "Mortgage") {
    
    ggplot(wells_mortgage) +
       geom_histogram(mapping = aes(x = wells_mortgage$State), stat = "count") +
      xlab("State") +
      ylab("Complaints with Narratives") 
  
    } else if (input$company == "WELLS FARGO & COMPANY" & input$product == "Credit Card") {
      
      ggplot(wells_credit_card) +
        geom_histogram(mapping = aes(x = wells_credit_card$State), stat = "count") +
        xlab("State") +
        ylab("Complaints with Narratives")
      
    } else if (input$company == "WELLS FARGO & COMPANY" & input$product == "Account") {
      
      ggplot(wells_account) +
        geom_histogram(mapping = aes(x = wells_account$State), stat = "count") +
        xlab("State") +
        ylab("Complaints with Narratives")
    
    } else if (input$company == "BANK OF AMERICA, NATIONAL ASSOCIATION" & input$product == "Mortgage") {
      
      ggplot(boa_mortgage) +
        geom_histogram(mapping = aes(x = boa_mortgage$State), stat = "count") +
        xlab("State") +
        ylab("Complaints with Narratives") 
      
    } else if (input$company == "BANK OF AMERICA, NATIONAL ASSOCIATION" & input$product == "Credit Card") {
      
      ggplot(boa_credit_card) +
        geom_histogram(mapping = aes(x = boa_credit_card$State), stat = "count") +
        xlab("State") +
        ylab("Complaints with Narratives")
      
    } else if (input$company == "BANK OF AMERICA, NATIONAL ASSOCIATION" & input$product == "Account") {
      
      ggplot(boa_account) +
        geom_histogram(mapping = aes(x = boa_account$State), stat = "count") +
        xlab("State") +
        ylab("Complaints with Narratives")
      
    } else if (input$company == "JPMORGAN CHASE & CO." & input$product == "Mortgage") {
      
      ggplot(jpm_mortgage) +
        geom_histogram(mapping = aes(x = jpm_mortgage$State), stat = "count") +
        xlab("State") +
        ylab("Complaints with Narratives") 
      
    } else if (input$company == "JPMORGAN CHASE & CO." & input$product == "Credit Card") {
      
      ggplot(jpm_credit_card) +
        geom_histogram(mapping = aes(x = jpm_credit_card$State), stat = "count") +
        xlab("State") +
        ylab("Complaints with Narratives")
      
    } else if (input$company == "JPMORGAN CHASE & CO." & input$product == "Account") {
      
      ggplot(jpm_account) +
        geom_histogram(mapping = aes(x = jpm_account$State), stat = "count") +
        xlab("State") +
        ylab("Complaints with Narratives")
      
    } else if (input$company == "CITIBANK, N.A." & input$product == "Mortgage") {
      
      ggplot(citi_mortgage) +
        geom_histogram(mapping = aes(x = citi_mortgage$State), stat = "count") +
        xlab("State") +
        ylab("Complaints with Narratives") 
      
    } else if (input$company == "CITIBANK, N.A." & input$product == "Credit Card") {
      
      ggplot(citi_credit_card) +
        geom_histogram(mapping = aes(x = citi_credit_card$State), stat = "count") +
        xlab("State") +
        ylab("Complaints with Narratives")
      
    } else if (input$company == "CITIBANK, N.A." & input$product == "Account") {
      
      ggplot(citi_account) +
        geom_histogram(mapping = aes(x = citi_account$State), stat = "count") +
        xlab("State") +
        ylab("Complaints with Narratives")
    }
})
  
  output$bar1 <- renderPlot({
  
    con_com_data <- readr::read_csv("complaints-2019-11-08_10_20.csv")
    
    ### cleaning zip codes
    
    Zip <- con_com_data$`ZIP code`
    
    Zip <- str_sub(Zip, 1, 3) # subsets zipcodes to first 3 digits 
    
    con_com_data$`ZIP code` <- as.numeric(Zip)
    
    con_com_data <- filter(con_com_data, !is.na(`ZIP code`))
    
    con_com_data <- filter(con_com_data, `ZIP code` > 99) 
    # removes all values less than 3 digits
    
    issue_zip <- data.frame(con_com_data$Issue, con_com_data$`ZIP code`)
    
    issue_zip <- issue_zip %>%
      rename(Issue = con_com_data.Issue) %>%
      rename(Zip = con_com_data..ZIP.code.)
    
    ### AFINN sentiments 
    
    issue_db <- data.frame(issue_desc = unique(con_com_data$Issue),
                           issue_tag = c(1:length(unique(con_com_data$Issue))),
                           stringsAsFactors = FALSE)
    
    afinn_neg <- get_sentiments("afinn") %>%
      filter(value < 0 )
    
    issue_afinn <- issue_db %>% #id tag for issues
      unnest_tokens(word, issue_desc) %>%
      inner_join(afinn_neg) %>%
      group_by(issue_tag) %>%
      summarise(value = sum(value, na.rm = TRUE))
    
    issues <- left_join(issue_db, issue_afinn) 
    
    ### NRC sentiments 
    
    nrc_anger <- get_sentiments("nrc") %>%
      filter(sentiment == "anger")
    
    issue_nrc <- issue_db %>% #id tag for issues
      unnest_tokens(word, issue_desc) %>%
      inner_join(nrc_anger) %>%
      group_by(issue_tag)
    
    issues_anger <- left_join(issue_db, issue_nrc) 
    
    anger_sentiment <- is.na(issues_anger$sentiment) == FALSE
    
    issues_anger$sentiment[anger_sentiment] <- as.numeric(-1) 
    # assigns all issues with "anger" sentiment with value of -1 
    
    null_sentiment <- is.na(issues_anger$sentiment) == TRUE
    
    issues_anger$sentiment[null_sentiment] <- as.numeric(0) 
    
    ### Joining with zip code data 
    
    sent_issues_zip <- issues %>%
      rename(Issue = issue_desc) %>%
      left_join(issue_zip)
    
    sent_issues_zip <- issues_anger %>%
      rename(Issue = issue_desc) %>%
      left_join(sent_issues_zip)
    
    null_value <- is.na(sent_issues_zip$value) == TRUE
    
    sent_issues_zip$value[null_value] <- as.numeric(0)
    
    sent_issues_zip$value <- as.numeric(sent_issues_zip$value)
    
    sent_issues_zip$sentiment <- as.numeric(sent_issues_zip$sentiment)
    
    sent_issues_zip$Urgency <- sent_issues_zip$sentiment + sent_issues_zip$value
    
    sent_issues_zip$Urgency <- abs(sent_issues_zip$Urgency)
    
    sent_issues_zip <- select(sent_issues_zip, -c(word, sentiment, value))
    
    atlanta <- filter(sent_issues_zip, Zip == 300)
    
    houston <- filter(sent_issues_zip, Zip == 770)
    
    chicago <- filter(sent_issues_zip, Zip == 606)
    
    
    if (input$city == "Atlanta") {
      
      atlanta %>%
        count(issue_tag, Urgency, sort = FALSE) %>%
        ungroup() %>%
        group_by(issue_tag) %>%
        ungroup() %>%
        ggplot(aes(issue_tag, n, fill = Urgency)) + 
        geom_col(show.legend = TRUE) +
        xlim(0, 100) + # only look at first 100 issues 
        labs(y = "Frequency", 
             x = "Issues in Atlanta based on Urgency") 
      
      
    } else if (input$city == "Houston") {
      
      houston %>%
        count(issue_tag, Urgency, sort = FALSE) %>%
        ungroup() %>%
        group_by(issue_tag) %>%
        ungroup() %>%
        ggplot(aes(issue_tag, n, fill = Urgency)) + 
        geom_col(show.legend = TRUE) +
        xlim(0, 100) + # only look at first 100 issues 
        labs(y = "Frequency", 
             x = "Issues in Houston based on Urgency") 
      
    } else if (input$city == "Chicago") {
      
      chicago %>%
        count(issue_tag, Urgency, sort = FALSE) %>%
        ungroup() %>%
        group_by(issue_tag) %>%
        ungroup() %>%
        ggplot(aes(issue_tag, n, fill = Urgency)) + 
        geom_col(show.legend = TRUE) +
        xlim(0, 100) + # only look at first 100 issues 
        labs(y = "Frequency", 
             x = "Issues in Chicago based on Urgency") 
    }
    
  })
  
  output$pred_mod <- renderText({
    
    complaints_clean <- read.csv("final_complaints.csv")
    
    
    
    set.seed(642597934)
    
    complaints <- complaints_clean
    
    complaints <- na.omit(complaints)
    
    complaints$product <- relevel(complaints$prod_group, ref = "Credit Card")
    
    test <- multinom(product ~ SESrank + pop_type + Urgency + homeowner, data = complaints)
    
    
    
    expanded = expand.grid(pop_type = c("Urban"),
                           SESrank = c("High"),
                           homeowner = c("Low"),
                           Urgency = 0)
    
    expanded1 = expand.grid(pop_type = c("Rural"),
                            SESrank = c("High"),
                            homeowner = c("Low"),
                            Urgency = 0
    )
    
    predicted = predict(test,expanded,type="probs")
    
    predicted1 = predict(test,expanded1,type="probs")
    
    
    if (input$pop_type == "Urban") {
      
      head(predicted)
      
    } else if (input$pop_type == "Rural") {
      
      head(predicted1)
    }
    
  })
  
  output$timeseries <- renderPlot({
    
    raw_data <- read_csv("ModifiedData.csv")
    
    dr <- as.data.frame(raw_data)
    
    df <- data.frame(stringsAsFactors=FALSE,
                     Date = dr$`Date sent to company`,
                     Product = dr$Product
    )
    
    df <- df %>% 
      mutate(
        Date = mdy(Date),
        Year = year(Date),
        Month = month(Date),
        Period = make_date(Year, Month, 1)
      ) %>% 
      group_by(Period, Product) %>% 
      summarise(
        incidents = n()
      ) 
    
    bank_account_or_service <- filter(df, Product == "Bank account or service")
    
    checking_or_savings_account <- filter(df, Product == "Checking or savings account")
    
    consumer_loan <- filter(df, Product == "Consumer Loan")
    
    plot1 <- ggplot() +
      geom_path(data = bank_account_or_service, mapping = aes(x = Period, y = incidents), colour = "red")
    
    plot2 <- ggplot() +
      geom_path(data = checking_or_savings_account, mapping = aes(x = Period, y = incidents), colour = "red")
    
    plot3 <- ggplot() +
      geom_path(data = consumer_loan, mapping = aes(x = Period, y = incidents), colour = "red")
    
    if (input$product1 == "Bank account or service") {
      
      plot1
      
    } else if (input$product1 == "Checking or savings account") {
      
      plot2
      
    } else if (input$product1 == "Consumer Loan") {
      
      plot3
      
    }
    
  })  
}


shinyApp(server = server, ui = ui)