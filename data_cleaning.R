#' data_cleaning.R
#'
#' Clean up and select a date range of tweets
#' from donald trump for analysis
#'
#'

# --- Library --- #
library(readr)
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(tidytext)
library(textstem)
library(vader)
library(lubridate)
library(stm)


# --- load the data --- # 
tweets <- read_csv('data/tweets_01-08-2021.csv')

# let's focus on tweets around when corona started
# why? i needed to make the code run faster somehow
# for teaching purposes

start_date <- as.Date("2020-01-01")
end_date   <- as.Date("2020-04-30")
bus_day_start <- hms::as_hms("09:00:00")
bus_day_end   <- hms::as_hms("20:00:00")

tweets_covid <-
    tweets %>%
    mutate(date = force_tz(date, "UTC"),
           date_est = with_tz(date, "America/New_York"),
           date_posted = floor_date(date_est, unit = 'day'),
           hour_posted = floor_date(date_est, unit = 'hour'),
           time = hms::as_hms(hour_posted)
           ) %>%
    filter(between(as.Date(date_posted),
                   start_date,
                   end_date)
           ) %>%
    mutate(business_hours = 
               case_when(
                time < bus_day_start | time > bus_day_end ~ "non-business hours",
                TRUE ~ "business hours"
               )
           ) %>%
    rownames_to_column("text_id")

trump_tweets <- 
    tweets_covid %>%
    select(text_id, text, date_est, business_hours)

write_csv(trump_tweets, 'data/trump_early_2020_tweets.csv')
