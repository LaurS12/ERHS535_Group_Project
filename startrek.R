# Get the Data
library(tidyverse)
library(tidytext)
library(textdata)
library(magrittr)
library(reshape2)
library(syuzhet)
library(RColorBrewer)
library(tm)
library(viridis)

startrek <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv")

#tidy the data
tibble(startrek)
startrek_tidy2 <- startrek %>% 
  select(char, line) %>%
  unnest_tokens(words, line) %>% #makes each word an observation
  rename("script_words" = words) %>% 
  group_by(char) %>% 
  mutate(char = recode(char, 'Geordi (V.O.)' = "Geordi",'Geordi (O.S.)' = "Geordi", 
                       'Computer (V.O.)' = "Computer", 'Computer Voice' = "Computer", 
                       'Computer Voice (V.O.)' = "Computer", 
                       'New Computer Voice' = "Computer",
                       'Riker (O.S.)' = "Riker",
                       'Picard (O.S.)' = "Picard", 'Picard (V.O.)' = "Picard", 'Young Picard' = "Picard",
                       'Jean-Luc' = "Picard")) 

select_char <- c("Picard", "Geordi", "Data", "Riker", "Computer")
startrek_tidy3 <- filter(startrek_tidy2, char %in% select_char)

##Load the lexicons

sent_afinn <- get_sentiments("afinn") #score

  

######Sentiment Analysis######

##bing
sentiment_bing <- get_sentiments("bing")  #pos/neg

bing_join <- sentiment_bing %>% 
  inner_join(startrek_tidy3, by = c("word" = "script_words")) %>% 
  group_by(sentiment, char) %>% 
  count()

bing_summary <- bing_join %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(difference = positive - negative) 

plot_bing <- bing_join %>% 
    ggplot(aes(char, sentiment, fill = sentiment, y = n)) +
    geom_bar(position = "stack", stat = "identity") +
    xlab("Star Trek Character") +
    ylab("Number of words") +
    labs(title = "Word Sentiments in Star Trek TNG:", 
         subtitle = "Character and Computer Interactions", 
         caption = "Data source: www.tidytuesday.com") +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 0.9),
      legend.title = element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank(),
      axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 0.5),
      ) 
  print(plot_bing)

###nrc (feelings)
omit_sent <- c("positive", "negative")
sent_nrc <- get_sentiments("nrc") %>% 
  filter(!sentiment %in% omit_sent)
  
nrc_join <- sent_nrc %>% 
  inner_join(startrek_tidy3, by = c("word" = "script_words")) %>% 
  group_by(sentiment, char) %>% 
  count() 

plot_nrc <- nrc_join %>% 
  ggplot(aes(char, sentiment, fill = sentiment, y = n)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Star Trek Character") +
  ylab("Number of words") +
  labs(title = "Word Sentiments in Star Trek TNG:", 
       subtitle = "Character and Computer Interactions", 
       caption = "Data source: www.tidytuesday.com") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0.9),
    legend.title = element_blank(),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 0.5),
  ) 
print(plot_nrc)



startrek_afinn <- sent_afinn %>% 
  inner_join(startrek_tidy2, by = c("word" = "script_words")) %>% 
  group_by(value, char) %>% 
  count() %>% 
  pivot_wider(names_from = value, values_from = n, values_fill = 0)


#Comparison cloud
library(reshape2)
comp_cloud <- startrek_tidy2 %>%
  group_by(char) %>%
  count(script_word) %>%
  acast(script_word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

