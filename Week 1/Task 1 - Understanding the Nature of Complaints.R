# Loading libraries
library(tidyverse)
library(wordcloud)
library(ggwordcloud)
library(tidytext)

# Loading data from github
airline.review <-
read.csv("https://raw.githubusercontent.com/Erhun-Joel/British-airways-forage-job-simulation/refs/heads/main/Data/review_data.csv?token=GHSAT0AAAAAAC6S6LR6A3MTPT5K4NUBWFH6Z7W2EZQ") %>%
  as_tibble()
glimpse(airline.review)

# Lets take an arbirtuary line using rating. Any rating below 7 is unsatisfacotry
airline.review <- airline.review %>%
  mutate(sentiment = if_else(rating < 7, "negative", "positive"))
airline.review

# Check porportion of satisfactory performance
airline.review %>%
  count(sentiment)

# Take sample of positive review headers
(airline.review %>%
  filter(
    sentiment == "positive",
    !str_detect(header, "British Airways customer review")
  ) %>%
  pull(header))[sample(1:500, size = 20)]

# Take sample of negative review headers
(airline.review %>%
  filter(
    sentiment == "negative",
    !str_detect(header, "British Airways customer review")
  ) %>%
  pull(header))[sample(1:2000, size = 20)]

# Lets go straight the the written text and plot word clouds

# Start with positive reviews
positive.reviews <-
airline.review %>%
  filter(sentiment == "positive") %>%
  select(review_text) %>%
  unnest_tokens(word, review_text) %>%
  filter(
    !(str_detect(word, "\\d")),
    !(word %in% stop_words$word)
  ) %>%
  count(word, sort = TRUE)

wordcloud(words = positive.reviews$word, freq = positive.reviews$n, min.freq = 100)

# Immediately, we see a problem, there is no context. We may be left guessing what the words mean. Lets use an ngram of 3 instead.

# Setting up vector containing important words in positive reviews
positive.vector <-
positive.reviews %>%
  top_n(n = 800, wt = n) %>%
  pull(word)
head(positive.vector)

# Creating count of positive phrases
positive.review.ngram <-
airline.review %>%
  filter(sentiment == "positive") %>%
  select(review_text) %>%
  unnest_tokens(word, review_text, token = "ngrams", n = 2) %>%
  filter(
    str_detect(word, paste0(positive.vector, collapse = "|"))
  ) %>%
    count(word, sort = TRUE)
positive.review.ngram

# Checking the distribution of frequencies to see how to plot word cloud
positive.review.ngram %>%
  filter(n < 200) %>%
  ggplot(aes(n)) +
  geom_histogram() +
  labs(y = "Occurances", x = "Frequency count") +
  theme_minimal()

# From the graph, the individual frequencies are quite sparse. Lets use a minimum frequency of 20
positive.review.ngram %>%
  filter(n > 50) %>%
  ggplot(aes(label = word, size = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 10) +
  theme_minimal()

# We see phrases such as Food, Efficiency, friendly, clean, comfortable, leg room, professional, on-time and son on

# Lets now do the same for negative reviews

# Get negative vector
negative.vector <-
(airline.review %>%
  filter(sentiment == "negative") %>%
  select(review_text) %>%
  unnest_tokens(word, review_text) %>%
  filter(
    !word %in% stop_words$word,
    !str_detect(word, "\\d")
  ) %>%
  count(word, sort = TRUE))[1:4000,] %>%
  pull(word)
negative.vector

# Get count of 2-worded negative phrases
negative.review.ngram <-
airline.review %>%
  filter(sentiment == "negative") %>%
  select(review_text) %>%
  unnest_tokens(word, review_text, token = "ngrams", n = 2) %>%
  filter(
    str_detect(word, paste0(negative.vector, collapse = "|"))
  ) %>%
  count(word, sort = TRUE)

# Plot a wordcloud for negative phrases
wordcloud(words = negative.review.ngram$word, freq = negative.review.ngram$n, min.freq = 70)

# Hmm. The words seem similar to the positive graph except for phrases like inflight entertainment.
# We have to find words exclusive of both sections
reviews.tfidf <-
airline.review %>%
  select(sentiment, review_text) %>%
  unnest_tokens(words, review_text) %>%
  filter(
    !words %in% stop_words$word,
    !str_detect(words, "\\d")
  ) %>%
  count(sentiment, words, sort = TRUE) %>%
  bind_tf_idf(term = words, document = sentiment, n = n)
reviews.tfidf

reviews.tfidf %>%
  group_by(sentiment) %>%
  top_n(n = 20, wt = tf_idf) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = NULL, x = "tf-idf") +
  theme_minimal()

# Key words, Negative:
# Expenses
# Reservations
# Reimbused
# Hotels
# Refundable
# Telephone

# Key words positive:
# Spotless
# Warmly
# Tender
# Respectful
# Ambience

# Lets see if we can add context by using making ngram = 2
reviews.ngrams.tfidf <-
airline.review %>%
  select(sentiment, review_text) %>%
  unnest_tokens(words, review_text, token = "ngrams", n = 2) %>%
  filter(
    str_detect(words, paste0(unique(c(positive.vector, negative.vector)), collapse = "|")),
    !str_detect(words, "\\d")
  ) %>%
  count(sentiment, words, sort = TRUE) %>%
  bind_tf_idf(term = words, document = sentiment, n = n)
reviews.ngrams.tfidf

reviews.ngrams.tfidf %>%
  group_by(sentiment) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(x = "tf-idf", y = NULL) +
  theme_minimal()
# Negative: Pricing, customer service
# Positive: Good schedule, no queues, customer service

# Lets see if we can add even more context by using making ngram = 3
reviews.ngrams.tfidf.again <-
airline.review %>%
  select(sentiment, review_text) %>%
  unnest_tokens(words, review_text, token = "ngrams", n = 3) %>%
  filter(
    str_detect(words, paste0(unique(c(positive.vector, negative.vector)), collapse = "|")),
    !str_detect(words, "\\d")
  ) %>%
  count(sentiment, words, sort = TRUE) %>%
  bind_tf_idf(term = words, document = sentiment, n = n)
reviews.ngrams.tfidf.again

reviews.ngrams.tfidf.again %>%
  group_by(sentiment) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(x = "tf-idf", y = NULL) +
  theme_minimal()
# Negatives: customer service, website
# Positives: customer service, clean environment

# Lets do this one more time with ngrams = 4 removing the numeric condition
reviews.ngrams.tfidf.again.again <-
airline.review %>%
  select(sentiment, review_text) %>%
  unnest_tokens(words, review_text, token = "ngrams", n = 4) %>%
  filter(
    str_detect(words, paste0(unique(c(positive.vector, negative.vector)), collapse = "|"))
  ) %>%
  count(sentiment, words, sort = TRUE) %>%
  bind_tf_idf(term = words, document = sentiment, n = n)
reviews.ngrams.tfidf.again.again

reviews.ngrams.tfidf.again.again %>%
  group_by(sentiment) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(x = "tf-idf", y = NULL) +
  theme_minimal()

# Insights:
# Negatives: price of tickets, cancelled flights, business class
# Positives: comfortable seats, professional staff, smooth flight, good food

# Two intersting topics: business class and website

# Lets check out business class reviews

# business class sentiment proportion
airline.review %>%
  select(sentiment, review_text) %>%
  filter(str_detect(review_text, regex("business class", ignore_case = TRUE))) %>%
  count(sentiment)

# Why?
airline.review %>%
  filter(str_detect(review_text, regex("business class", ignore_case = TRUE))) %>%
  select(sentiment, review_text) %>%
  unnest_tokens(words, review_text, token = "ngrams", n = 2) %>%
  filter(
    str_detect(words, paste0(unique(c(positive.vector, negative.vector)), collapse = "|")),
    !str_detect(words, "\\d")
  ) %>%
  count(sentiment, words, sort = TRUE) %>%
  bind_tf_idf(term = words,  document = sentiment, n = n) %>%
  filter(tf_idf != 0) %>%
  group_by(sentiment) %>%
  slice_max(order_by = tf_idf, n = 25, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(
    y = NULL,
    x = "tf_idf"
  ) +
  theme_minimal()

# Specific problems with the nature of business class is due to: Response period, price of tickets and inavailability of refunding pipeline
# Good reviews in business class hovered around: Early arrival and club suites

# Website sentiment proportion
airline.review %>%
  filter(str_detect(review_text, regex("website", ignore_case = TRUE))) %>%
  count(sentiment)
# Sentiments with website mentioned are disproportionately negative.

# Why?
(airline.review %>%
  filter(str_detect(review_text, regex("website", ignore_case = TRUE))))[c("sentiment", "review_text")] %>%
  unnest_tokens(words, review_text, token = "ngrams", n = 2) %>%
  filter(
    str_detect(words, paste0(unique(c(positive.vector, negative.vector)), collapse = "|")),
    !str_detect(words, "\\d")
  ) %>%
  count(sentiment, words, sort = TRUE) %>%
  bind_tf_idf(words, sentiment, n) %>%
  filter(tf_idf != 0) %>%
  group_by(sentiment) %>%
  slice_max(order_by = tf_idf, n = 30, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "tf-idf",
    y = NULL
  )
# Negative sentiments include: refunding problems, phone calling, seating
# Positive words includes: informative announcements.

# Conclusion:
# The refunding pipeline requires work
# Business class needs to justify its price tag
# Customer service may need a little improvement
