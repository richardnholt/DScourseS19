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
requestURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = "mHjDjv6Ay4jDqt9gmDITYafrS"
consumerSecret = "CzqpN1cZM8M5BQkhJnwSnG0eKawPNpFTEMQe23vYvjoQlM2utL"

accessToken = "1608536688-EInrfedkwlHy4s607TYk5nSYZbjGahk4013YeVh"
accessSecret = "Q5vLQYwhX3cG8LKJsCdJUAyZgIMKITT3dN3OfDAhCdkTS"

setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessSecret)

tweets <- searchTwitter('Gallogly', 
                        geocode='35.2225685120,-97.4394760132,20mi',  
                        n=500, retryOnRateLimit=1)

tweets.df <- twListToDF(tweets) 
View(tweets.df)

head(tweets.df$text)

# Removing hashtag , urls and other special charactersR

tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("@.*","",tweets.df2)
tweets.df2 <- gsub("RT","",tweets.df2)

data <- data.frame(tweets = as.character(tweets.df2), 
                   stringsAsFactors = FALSE)

data <- data %>% 
  unnest_tokens(word, tweets)

sentiment <- get_sentiments("nrc")

data <- inner_join(data, sentiment, by = "word")

ggplot(data = data, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()
