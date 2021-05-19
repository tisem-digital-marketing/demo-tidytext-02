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

# --- Download data --- # 

data_id <- "1HkirbijbXkWKaT1JyjKWGa6DdwcmxR8s"

out_file <- "data/tweets.zip"

drive_download(
    as_id(data_id), 
    path = out_file, 
    overwrite = TRUE)

unzip(out_file,
      exdir = "data")
file.remove(out_file)


# --- load the data --- # 
tweets <- read_csv('data/trump_early_2020_tweets.csv')

# add calendar week to data
tweets <- 
    tweets %>%
    mutate(cal_week = week(date_est))

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
    vader_df(tweets$text)

vader_sents2 <- 
    vader_sents %>%
    rowid_to_column("id") %>%
    filter(word_scores != 'ERROR') %>%
    mutate(vader_class = case_when(
        compound < -0.05 ~ "negative",
        compound > 0.05 ~ "positive",
        TRUE ~ "neutral"
    )
    ) %>% 
    select(id, vader_class) %>%
    mutate(id = as.character(id))

tweet_sent <-
    tweets %>%
    inner_join(vader_sents2, by = "id")

weekly_sent <-
    tweet_sent %>%
    group_by(cal_week, vader_class, business_hours) %>%
    count() %>%
    ungroup() %>%
    pivot_wider(names_from = vader_class, values_from = n) %>%
    mutate(pos_neg_ratio = positive / negative)

# plot it?
weekly_sent %>%
    ggplot(aes(x = cal_week, 
               y = pos_neg_ratio,
               color = business_hours)
           ) +
    geom_line() +
    theme_bw()

# sweet!

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
    tidy_trump %>%
    group_by(word) %>%
    count(sort = TRUE) %>%
    filter(n > 5)

# only keep words from my vocab list
tidy_trump <- 
    tidy_trump %>%
    filter(word %in% word_counts$word)

# for each tweet how often is each word occuring?

doc_word_counts <- 
    tidy_trump %>%
    count(id, word) %>%
    ungroup()

# cast this to a matrix
trump_dtm <- 
    doc_word_counts %>%
    cast_sparse(id, word, n)

# model!
trump_lda <-
    stm(trump_dtm,
        # why 10 topics, why not, its an example
        K = 10,
        # seed fixes the random number draw
        # so we should all get the same results
        seed = 123456789)

# what are these topics about?
labelTopics(trump_lda)

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

trump_topics <- 
    tweets %>%
    inner_join(tweets_gamma, by = "id")

# now lets plot it ...
topic_trends <- 
    trump_topics %>%
    group_by(cal_week, topic) %>%
    count() %>%
    ungroup()

topic_trends %>%
    filter(topic %in% c(1, 2, 9, 10)) %>%
    ggplot(aes(
        x = cal_week, 
        y = n,
        color = as.factor(topic)
    )
           ) +
    geom_line() + 
    theme_bw()

# each topic over time?  

topic_trends %>%
    #filter(topic %in% c(1, 2, 9, 10)) %>%
    ggplot(aes(
        x = cal_week, 
        y = n
    )
    ) +
    geom_line() + 
    facet_wrap(~topic) +
    theme_bw()

# if we have time, which we dont ... 

topic_trends <- 
    topic_trends %>%
    mutate(topic_name = case_when(
        topic == 1 ~ "topic 1", # ie. name it something meaningful,
        topic == 2 ~ "topic 2",
        topic == 3 ~ "topic 3",
        topic == 4 ~ "topic 4",
        topic == 5 ~ "topic 5",
        topic == 6 ~ "topic 6",
        topic == 7 ~ "topic 7",
        topic == 8 ~ "topic 8",
        topic == 9 ~ "topic 9",
        TRUE ~ "topic 10"
            )
    )

topic_trends %>%
    #filter(topic %in% c(1, 2, 9, 10)) %>%
    ggplot(aes(
        x = cal_week, 
        y = n
    )
    ) +
    geom_line() + 
    facet_wrap(~topic_name) +
    theme_bw()
