## Sentiment analysis R

library(tidyverse)
library(tidytext)
library(janeaustenr)
library(stringr)
library(tidytext)

sentiments

get_sentiments("bing")

# We will convert the text of our books into a tidy format using unnest_tokens() function



tidy_data <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


# Create positive sentiment object filter

positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

# Find positive sentiments in Emma
tidy_data %>%
  filter(book == "Emma") %>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)

# From our above result, we observe many positive words like "good", "happy", 
# "love" etc. In the next step, we will use spread() function to segregate our
# data into separate columns of positive and negative sentiments. We will then 
# use the mutate() function to calculate the total sentiment, that is, the
# difference between positive and negative sentiment.

bing <- get_sentiments("bing")

Emma_sentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book = "Emma" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Vizualise 

library(ggplot2)
ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words, 10)

counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")


library(reshape2)
install.packages("wordcloud")
library("wordcloud")

tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)
