# Loading neccessary libraries
library(tidyverse)
library(rvest)

# Create function to automatically get British Airways reviews
get.sentiment.data <- function(page_size = 3000){
  
  # Declare link
  link = paste0(
    "https://www.airlinequality.com/airline-reviews/british-airways/page/",
    1,
    "/?sortby=post_date%3ADesc&pagesize=",
    page_size
    )

  # Get out html content
  result = read_html(link)

  x = c(x, html_text(html_nodes(result, "span[itemprop='ratingValue']"))[-1])

  # Make output tibble
  output = tibble(
    date = html_text(html_nodes(result, "time[itemprop='datePublished']")),
    review_type = "airline-reviews",
    header = html_text(html_nodes(result, ".text_header")),
    rating = html_text(html_nodes(result, "span[itemprop='ratingValue']"))[-1],
    review_text = html_text(html_nodes(result, ".text_content"))
    )
  
  # Organise output to acceptable format
  output <- output %>%
      separate("review_text", into = c("status", "review_text"), sep = "\\|", fill = "left") %>%
      mutate(
          header = str_remove_all(header, "\\\""),
          rating = as.numeric(rating),
          review_text = str_trim(review_text),
          status = case_when(
            is.na(status) ~ "unknown",
            str_detect(status, "Not Verified") ~ "not Verified",
            str_detect(status, "Unverified") ~ "not Verified",
            str_detect(status, "Trip Verified") | str_detect(status, "Verified Review") ~ "verified",
            TRUE ~ status
          ),
          date = paste0(
            str_extract(date, "^[0-9]+"),
            "/",
            str_trim(str_extract(date, " [a-zA-Z]+ ")),
            "/",
            str_extract(date, "[0-9]+$")
          ),
          date = as.Date(date, format = "%d/%B/%Y")
      )

  # Return output
  return(output)

}

# Run function and check result
airway.sentiment <- get.sentiment.data(page_size = 3350)
airway.sentiment

# Check out date of last result
arrange(airway.sentiment, date)[1,1]

# Save data into local file


