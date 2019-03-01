library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(syuzhet)
library(tidytext)
library(ggplot2)
library(forcats)
library(dplyr)
library(rvest)
library(stringr)

webpage <- read_html("https://www.imdb.com/list/ls021105452/")
results <- webpage %>%
  html_nodes(".mode-detail")

records <- vector("list", length = length(results))
for (i in seq_along(results)) {
  rank <- results[i] %>% 
    html_nodes(".text-primary") %>% 
    html_text(trim = TRUE)
  title <- results[i] %>%
    html_nodes(".lister-item-header") %>%
    html_text(trim = TRUE) %>%
    str_sub(13)
  genre <- results[i] %>%
    html_nodes(".genre") %>%
    html_text(trim = TRUE)
  #rating <- results[i] %>%
   # html_nodes(".ipl-rating-star__rating") %>%
    #html_text(trim = TRUE)
  records[[i]] <- data_frame(rank=rank,title=title,genre=genre)
}
df <- bind_rows(records)
df
