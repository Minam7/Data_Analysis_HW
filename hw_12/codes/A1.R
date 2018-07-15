# a: best rating movie

best_rate <- rating %>% filter(!is.na(Rating)) %>% 
  group_by(MovieID) %>% summarise(Rate = mean(Rating), watch = n()) %>% 
  filter(watch > 100) %>% arrange(desc(Rate)) %>% 
  left_join(movie, by = c("MovieID")) %>% 
  slice(1:10)

knitr::kable(best_rate)
