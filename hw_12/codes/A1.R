# a: best rated movie
best_rate <- rating %>% filter(!is.na(Rating)) %>% 
  group_by(MovieID) %>% summarise(Rate = mean(Rating), watch = n()) %>% 
  filter(watch > 100) %>% arrange(desc(Rate)) %>% 
  left_join(movie, by = c("MovieID")) %>% 
  slice(1:10)

knitr::kable(best_rate)

# b: most reviewed movie
most_rev <- tag %>% filter(!is.na(MovieID)) %>%
  group_by(MovieID) %>% 
  summarise(Review = n()) %>% 
  arrange(desc(Review)) %>% 
  left_join(movie, by = c("MovieID")) %>% 
  slice(1:10)

knitr::kable(most_rev)

# c: most hated movie
worst_rate <- rating %>% filter(!is.na(Rating)) %>% 
  group_by(MovieID) %>% summarise(Rate = mean(Rating), watch = n()) %>% 
  filter(watch > 500) %>% arrange(Rate) %>% 
  left_join(movie, by = c("MovieID")) %>% 
  slice(1:10)

knitr::kable(worst_rate)

# d: yearly built movies
yearly <- movie %>% mutate(year = tail(str_split(Title, " ")[[1]], n=1)) %>% 
  group_by(year) %>% summarise(count = n())
