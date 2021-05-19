#' trump_tweets.R
#'
#' We try and learn something about Trump tweets
#'
#'

# --- Library --- #
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(tidytext)
library(textstem)
library(vader)
library(lubridate)
library(stm)
library(tokenizers)
library(ggplot2)


# --- load the data --- # 
tweets <- #YOUR_CODE

# add calendar week to data
tweets <- 
    YOUR_CODE

# --- Clean the main junk out of text --- #
tweets <-
    tweets %>%
    mutate(
        # remove links
        text = str_remove_all(text, "https\\S*"),
        text = str_remove_all(text, "http\\S*"),
        text = str_remove_all(text, "t.co*"),
        # remove mentions
        text = str_remove_all(text, "@\\S*"),
        # remove annoying html stuff
        text = str_remove_all(text, "amp"),
        text = str_remove_all(text, "&S*"),
        text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
        text = str_replace_all(text, "<a(.*?)>", " "),
        text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
        text = str_replace_all(text, "&#[:digit:]+;", " "),
        text = str_remove_all(text, "<[^>]*>"),
        # remove numbers
        text = str_remove_all(text, "[:digit:]"),
        # remove excess whitespace
        text = str_squish(text),
        text = str_trim(text),
        # remove RT for retweets -- keeping retweets in the data
        text = str_remove_all(text, "RT")
    ) %>%
    filter(count_words(text) > 1) %>%
    rownames_to_column("id") %>%
    select(-text_id)

# --- Sentiment analysis --- #
vader_sents <- 
    #YOUR_CODE

vader_sents2 <- 
    #YOUR_CODE

tweet_sent <-
    #YOUR_CODE

weekly_sent <-
    #YOUR CODE

# plot it?
weekly_sent %>%
    ggplot()#YOUR CODE

# Can we now go for a topic model?

# some data processing steps ...
tidy_trump <- 
    tweets %>%
    select(id, text) %>%
    unnest_tokens(word, text) %>%
    mutate(word = lemmatize_words(word))

nums <- 
    tidy_trump %>% 
    filter(str_detect(word, "^[0-9]")) %>% 
    select(word) %>% 
    unique()

tidy_trump <- 
    tidy_trump %>% 
    anti_join(stop_words) %>%
    anti_join(nums, by = "word")

# set up what my vocab list will be
word_counts <- 
    #YOUR_CODE

# only keep words from my vocab list
tidy_trump <- 
    #YOUR CODE

# for each tweet how often is each word occuring?

doc_word_counts <- 
    #YOUR CODE
    
# cast this to a matrix
trump_dtm <- 
    #YOUR CODE

# model!
trump_lda <-
    #YOUR CODE

# what are these topics about?
    #YOUR CODE

# give each tweet its most probably topic..

tweets_gamma <- 
    tidy(trump_lda, 
         matrix = "gamma",
         document_names = rownames(trump_dtm)
    ) %>%
    rename(id = document) %>%
    group_by(id) %>%
    slice_max(gamma) %>%
    select(-gamma)

#YOUR CODE

# now lets plot it ...
topic_trends <- 
    trump_topics %>%
    group_by(cal_week, topic) %>%
    count() %>%
    ungroup()

topic_trends %>%
    #YOUR CODE

