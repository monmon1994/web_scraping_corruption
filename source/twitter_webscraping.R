install.packages("twitteR")
install.packages("tidytext")

library(twitteR)
library(tidytext)
library(tidyverse)
library(rtweet)

consumer_key <- "cYnvHtnzKpERDiuPa9L72SbhG"
consumer_secret <- "GlnvOs88BBXxtmugK7sUTtxn4wr699BXel0cEUouZ44SldLF8a"
access_token <- "618337115-78dbFl9nuJCrrj52Pm9mDF3xoE7kmodTbx7ns4Ff"
access_secret <- "scHf1PIrwlITz6SLLtFGnaAmSIlcelUl6rN9PNQ0YZHBy"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

## searching twitter

fn_twitter <- searchTwitter("#corruption",n=1000,lang="en")

fn_twitter_df <- twListToDF(fn_twitter) # Convert to data frame

tweet_words <- fn_twitter_df %>% 
    select(id, text) %>% 
    unnest_tokens(word,text)

tweet_words %>% 
    count(word,sort=T) %>% 
    slice(1:20) %>% 
    ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    xlab("")
