## Elections in Tanzania Twitter 

library(twitteR)
library(tidytext)
library(tidyverse)
library(rtweet)

consumer_key <- "KVsraIh82C7cpohVlshVzWZnS"
consumer_secret <- "OLWaEvB7NoteYN7CC4o6K9w093oJyWXpTmXAkEC2YShjNp5RrS"
access_token <- "618337115-QOGiO1egjgQeo9pa4gWhenvb44PMdpAsExNK7W1o"
access_secret <- "genGeZo3THGoQooDHifCvfBXz2miMQEudhAzGQ221IvN1"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

## searching twitter

tan <- searchTwitter("#tanzania",n=1000,lang="en")

magufuli <- userTimeline('@MagufuliJP',n=100) #to find the tweets of any specific user 

homeTimeline (n=15) #tweets from own timeline
mentions (n=15) #tweets where you have been tagged



fn_twitter_df <- twListToDF(fn_twitter) # Convert to data frame

tweet_words <- fn_twitter_df %>% 
    select(id, text) %>% 
    unnest_tokens(word,text)

tweet_words %>% 
    count(word,sort=T) %>% 
    slice(1:50) %>% 
    ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    xlab("")

