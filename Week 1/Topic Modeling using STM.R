# Loading required libraries
library(tidyverse)
library(tidytext)
library(stm)

# Loading dataset
airline.review <-
read.csv("https://raw.githubusercontent.com/Erhun-Joel/British-airways-forage-job-simulation/refs/heads/main/Data/review_data.csv?token=GHSAT0AAAAAAC6S6LR6A3MTPT5K4NUBWFH6Z7W2EZQ") %>%
  as_tibble()
airline.review

