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
movie <- movie %>% mutate(year = str_extract(Title, '\\([[:digit:]]{4}\\)'))
yearly <- movie %>% group_by(year) %>% summarise(count = n()) %>% filter(!is.na(year))

yearly  %>% mutate(year = str_extract(year, '[[:digit:]]{4}')) %>% 
  hchart(type = "line", hcaes(x = year, y = count)) %>% 
  hc_yAxis(title = list(text = "Count")) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_title(text = "Number of Movie Produced Yearly", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_sandsignika())

# e: favourite genre
