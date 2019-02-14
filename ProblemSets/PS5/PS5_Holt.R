library(rvest)
library(stringr)
library(dplyr)

webpage	<- read_html("https://www.history.com/schedule")
results	<- webpage %>%
  html_nodes(".collapsed")

records <- vector("list", length = length(results))
for (i in seq_along(results)) {
  time <- results[i] %>% 
    html_nodes(".time") %>% 
    html_text(trim = TRUE) %>%
    str_c(', February 12') %>%
    str_sub(7)
  show <- results[i] %>%
    html_nodes(".show-name") %>%
    html_text(trim = TRUE)%>%
    str_c(" --")
  episode <- results[i] %>%
    html_nodes(".episode-title") %>%
    html_text(trim = TRUE)%>%
    str_c(" --")
  records[[i]] <- data_frame(show=show, episode=episode, time=time)
}
df <- bind_rows(records)
df
