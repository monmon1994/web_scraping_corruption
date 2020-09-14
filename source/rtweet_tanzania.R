## Tanzania 

# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
library(twitteR)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)

### Twitter

consumer_key <- "KVsraIh82C7cpohVlshVzWZnS"
consumer_secret <- "OLWaEvB7NoteYN7CC4o6K9w093oJyWXpTmXAkEC2YShjNp5RrS"
access_token <- "618337115-QOGiO1egjgQeo9pa4gWhenvb44PMdpAsExNK7W1o"
access_secret <- "genGeZo3THGoQooDHifCvfBXz2miMQEudhAzGQ221IvN1"

twitter_token <- create_token(
    app = "monmonapp",
    consumer_key = consumer_key,
    consumer_secret = consumer_secret,
    access_token = access_token,
    access_secret = access_secret)

#post_tweet("Posting my first ever tweet fron R! Hoorah! #RTweet")

## searching twitter

tan <- search_tweets("#tanzania", since = "2020-01-01", type = "mixed")
tan2 <- search_tweets("#tanzaniaelections", since = "2020-01-01", type = "mixed")
tan3 <- search_tweets("#tanzaniaelections2020", since = "2020-01-01", type = "mixed")
tan4 <- search_tweets("#ChamaChaMapinduzi", since = "2020-01-01", type = "mixed")
tan5 <- search_tweets("#NCCR–Mageuzi", since = "2020-01-01", type = "mixed")
tan6 <- search_tweets("#kazinabata", since = "2020-01-01", type = "mixed")

### searching specific words in tweets since Jan 2018-present

tanz_ccm <- search_tweets(q = "Magufuli", type = "mixed", lang = "en", retryonratelimit = T, include_rts = F)
tanz_ccm2 <- search_tweets("CCM", since = "2018-01-01", type = "mixed", lang = "en", retryonratelimit = T, include_rts = F)

tanz_opp <- search_tweets("Tundu Lissu", since = "2018-01-01", type = "mixed",lang = "en", retryonratelimit = T, include_rts = F)
tanz_opp2 <- search_tweets("Zitto Kabwe", since = "2018-01-01", type = "mixed", lang = "en", retryonratelimit = T, include_rts = F)
tanz_opp3 <- search_tweets("#CHAMEMA", since = "2018-01-01", type = "mixed",lang = "en", include_rts = F)
tanz_opp4 <- search_tweets("ACT-Wazalendo", since = "2018-01-01", type = "mixed",lang = "en", include_rts = F)

########
tanz_opp4 %>%
    count(location, sort = TRUE) %>%
    mutate(location = reorder(location, n)) %>%
    top_n(20) %>%
    ggplot(aes(x = location, y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = "Count",
         y = "Location",
         title = "Where Twitter users are from - unique locations ")

######## creating the tidy text

tidy_words1 <- tanz_ccm %>%
    select(text) %>%
    mutate(linenumber = row_number(),
           index = 1 + linenumber %/% 100) %>%
    unnest_tokens(word, text,
                  strip_punct = TRUE,
                  strip_numeric = TRUE) %>%
    anti_join(stop_words)

tidy_words1 %>% 
    count(word, sort = T)

# remove some words that aren't useful

tidy_words1[tidy_words1$word == c("https", "t.co"), ]

tidy_words1_n <- tidy_words1[!(tidy_words1$word %in% c("rt", "ya", "https", "t.co", "na", "wa", "za", "kwa")), ]

tidy_words1_n %>% 
    count(word, sort = TRUE) 

## sentiment 
