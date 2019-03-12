#install.packages('stringi')
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
library(stringi)
library(vcd)

webpage <- read_html("https://www.imdb.com/list/ls021105452/")
results <- webpage %>%
  html_nodes(".mode-detail")

records <- vector("list", length = length(results))
for (i in seq_along(results)) {
  rank <- results[i] %>% 
    html_nodes(".text-primary") %>% 
    html_text(trim = TRUE) %>%
    str_remove(pattern = "\\.")
  title <- results[i] %>%
    html_nodes(".lister-item-header") %>%
    html_text(trim = TRUE) %>%
    str_remove(pattern = "[0-9].\n    \n    ") %>%
    str_remove(pattern = "\n   ") %>%
    str_remove(pattern = " \\(2018\\)") %>%
    str_remove(pattern = " \\(2017\\)")
  runtime <- results[i] %>%
    html_nodes(".runtime") %>%
    html_text(trim = TRUE) %>%
    str_remove(pattern = " min")
  genre <- results[i] %>%
    html_nodes(".genre") %>%
    html_text(trim = TRUE) %>%
    str_split(", ")
  rating <- results[i] %>%
    html_nodes(".ratings-metascore") %>%
    html_text(trim = TRUE) %>%
    str_remove(pattern = "        \n        Metascore")
  records[[i]] <- data_frame(rank=rank,title=title,runtime=runtime,genre=genre,rating=rating)
}
df <- bind_rows(records)
for (i in 10:83){
  df$title[[i]] <- str_sub(df$title[[i]],2)
}
df
time <- hist(as.numeric(df$runtime),breaks=seq(50,160,5))
plot(time,main=paste("Histogram of Runtimes"),xlab="Minutes")

qplot(as.numeric(runtime), as.numeric(rank), data=df, 
      main="Rating by Runtime",
      xlab="Runtime", ylab="Rating")

qplot(as.numeric(rating), as.numeric(rank), data=df, 
      main="Rank by Rating",
      xlab="Rating", ylab="Rank")
