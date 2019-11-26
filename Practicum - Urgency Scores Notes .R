### Sentiment Analysis/Urgency Scores 

library(tidyverse)
library(tidytext)

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

view(sent_issues_zip)

library(ggplot2)

atlanta <- filter(sent_issues_zip, Zip == 300)

atlanta %>%
  count(Issue, Urgency, sort = TRUE) %>%
  ungroup() %>%
  group_by(Urgency) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(Issue = reorder(Issue, n)) %>%
  ggplot(aes(Issue, n, fill = Urgency)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~Urgency, scales = "free_y") + 
  labs(y = "Frequency", 
       x = NULL) +
  coord_flip()

sent_issues_zip

### 

atlanta %>%
  count(issue_tag, Urgency, sort = TRUE) %>%
  ungroup() %>%
  group_by(Urgency) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(issue_tag = reorder(issue_tag, n)) %>%
  ggplot(aes(issue_tag, n, fill = Urgency)) + 
  geom_col(show.legend = TRUE) + 
  labs(y = "Frequency", 
       x = "Issues in Atlanta based on Urgency") 

