# Loading required libraries
library(tidyverse)
library(tidytext)
library(SnowballC)
library(stm)

# Loading dataset
airline.review <-
read.csv("https://raw.githubusercontent.com/Erhun-Joel/British-airways-forage-job-simulation/refs/heads/main/Data/review_data.csv?token=GHSAT0AAAAAAC6S6LR6WNALXXYSU2WGDILAZ7W3SVA") %>%
  as_tibble()
airline.review

# Creating sparse dataset
airline.sparse <-
airline.review %>%
  mutate(sentiment = if_else(rating <= 7, "negative", "positive")) %>%
  select(sentiment, review_text) %>%
  unnest_tokens(word, review_text) %>%
  filter(!(word %in% stop_words$word)) %>%
  mutate(word = wordStem(word)) %>%
  count(sentiment, word, sort = TRUE) %>%
  cast_sparse(sentiment, word, n)
airline.sparse

# Fiting the stm model (starting with 3 topics)
topic.model <- stm(airline.sparse, K = 3)

topic.model %>%
  tidy(matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 30) %>%
  ungroup() %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free_y", nrow = 1) +
  theme_minimal() +
  scale_y_reordered() +
  labs(
    y = NULL, x = expression("beta")
  )
# This shows almost no seperation between contributing words in a topic

# Lets try higher K values
bulk.topic.models <-
function(sparse_data, k){

  # Create objects to store models
  models <- list()

  # for-loop initialization
  for(i in 1:length(k)){

    # Train model
    sparse.model <- stm(sparse_data, K = k[i])

    # Append model to list
    models <- c(models, list(sparse.model))

  }

  # return list of models
  return(models)

}

topic.models <- bulk.topic.models(sparse_data = airline.sparse, k = 3:8)
topic.models

# Create function to process beta results
beta.results <-
function(x, values){

  # Get words with maximum beta values
  tidy(x, matrix = "beta") %>%
    group_by(topic) %>%
    slice_max(order_by = beta, n = values, with_ties = FALSE) %>%
    ungroup()

}

# Create function to automatically plot beta results
beta.plot <-
function(x){

  # Creating plot
  x %>%
    ggplot(aes(beta, reorder_within(term, beta, topic), fill = as.factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    labs(
      x = expression("beta"),
      y = NULL
    ) +
    scale_y_reordered()+
    theme_minimal()

}

# First topic model
(topic.models %>%
  map(beta.results, values = 20))[[1]] %>%
  beta.plot()

# Second topic model
(topic.models %>%
  map(beta.results, values = 20))[[2]] %>%
  beta.plot()

# Third topic model
(topic.models %>%
  map(beta.results, values = 20))[[3]] %>%
  beta.plot()

# Fourth topic model
(topic.models %>%
  map(beta.results, values = 20))[[4]] %>%
  beta.plot()

# Fifth topic model
(topic.models %>%
  map(beta.results, values = 20))[[5]] %>%
  beta.plot()

# Further inspection shows that the beta's of each topics rarely differ in order. This is not satisfactory.
# Further analysis needed to get more insights