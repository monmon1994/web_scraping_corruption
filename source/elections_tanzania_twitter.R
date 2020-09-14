## Elections in Tanzania Twitter for Craig's project

# It would be an interesting study to compare the political leanings in 
# the English tweets vs. the Swahili ones, though -- to see they are similar or differ.

library(twitteR)
library(tidytext)
library(tidyverse)
library(rtweet)
library(translateR)
library(dplyr)

consumer_key <- "KVsraIh82C7cpohVlshVzWZnS"
consumer_secret <- "OLWaEvB7NoteYN7CC4o6K9w093oJyWXpTmXAkEC2YShjNp5RrS"
access_token <- "618337115-QOGiO1egjgQeo9pa4gWhenvb44PMdpAsExNK7W1o"
access_secret <- "genGeZo3THGoQooDHifCvfBXz2miMQEudhAzGQ221IvN1"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

## searching twitter

tan <- searchTwitter("#tanzania", since = "2020-01-01")
tan2 <- searchTwitter("#tanzaniaelections", since = "2020-01-01")
tan3 <- searchTwitter("#tanzaniaelections2020", since = "2020-01-01")
tan4 <- searchTwitter("#ChamaChaMapinduzi", since = "2020-01-01")
tan5 <- searchTwitter("#NCCR–Mageuzi", since = "2020-01-01")
tan6 <- searchTwitter("#kazinabata", since = "2020-01-01")



magufuli <- userTimeline('@MagufuliJP',n=100) #to find the tweets of any specific user 

homeTimeline (n=15) #tweets from own timeline
mentions (n=15) #tweets where you have been tagged

## looking at the words in df

fn_twitter_df <- twListToDF(c(tan, tan2, tan3, tan4, tan5, tan6)) # Convert to data frame

tweet_words <- fn_twitter_df %>% 
    select(id, text) %>% 
    unnest_tokens(word,text)
### cleaning the data frame of tweets

tidy_words <- fn_twitter_df %>%
    select(text) %>%
    mutate(linenumber = row_number(),
           index = 1 + linenumber %/% 100) %>%
    unnest_tokens(word, text,
                  strip_punct = TRUE,
                  strip_numeric = TRUE) %>%
    anti_join(stop_words)
# remove some words that aren't useful

tidy_words[tidy_words$word == c("rt", "ya", "https", "t.co", "na", "wa", "za", "kwa"), ]

tidy_words_df <- tidy_words[!(tidy_words$word %in% c("rt", "ya", "https", "t.co", "na", "wa", "za", "kwa")), ]

tidy_words_df %>% 
    count(word, sort = TRUE) 

## translation 
my.client.id.2 <- "AIzaSyDZnczHNmVO6fW02onwWoYKmT8A9yefj28"

data(enron)

google.df.out <- translate(tidy_words_df, content.field = 'word', google.api.key = my.client.id.2,
          source.lang = "sw", target.lang = "en") # not working yet


## count of tweets

tidy_words_df %>% 
    count(word,sort=T) %>% 
    slice(1:50) %>% 
    ggplot(aes(x = reorder(word, n, function(n) n), y = n)) + 
    geom_bar(stat = "identity") + 
    theme_minimal() + 
    labs(y = "number of times word is tweeted", x = "", title = "Tweets about Tanzania since Jan 2020",
         subtitle = "Hastags included: #tanzania, #tanzaniaelections, #tanzaniaelections2020, #ChamaChaMapinduzi, 
#NCCR–Mageuzi, #kazinabata") +
    coord_flip()

ggsave("tanzania_tweets_words.png", dpi = 600)
## google translate count of tweets

google.df.out %>% 
    count(word, sort = T) %>% 
    slice(1:50) %>% 
    ggplot(aes(x = reorder(word, n, function(n) n), y = n)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    coord_flip()




## sentiment

positive <- get_sentiments("bing") %>%
    filter(sentiment == "positive")

tidy_words_df %>%
    semi_join(positive) %>%
    count(word, sort = TRUE)
