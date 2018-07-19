# a: best rated movie
best_rate <- rating %>% filter(!is.na(Rating)) %>% 
  group_by(MovieID) %>% summarise(Rate = mean(Rating), watch = n()) %>% 
  filter(watch > 1000) %>% arrange(desc(Rate)) %>% 
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

# extract genre for each movie
movie_genre <- movie
movie_genre$Action = 0
movie_genre$Adventure = 0
movie_genre$Animation = 0
movie_genre$Children = 0
movie_genre$Comedy = 0
movie_genre$Crime = 0
movie_genre$Documentary = 0
movie_genre$Drama = 0
movie_genre$Fantasy = 0
movie_genre$`Film-Noir` = 0
movie_genre$Horror = 0
movie_genre$Musical = 0
movie_genre$Mystery = 0
movie_genre$Romance = 0
movie_genre$`Sci-Fi` = 0
movie_genre$Thriller = 0
movie_genre$War = 0
movie_genre$Western = 0

movie_genre <- movie_genre %>% 
  mutate(Action = ifelse(str_detect(Genres, 'Action'),1,0),
         Adventure = ifelse(str_detect(Genres, 'Adventure'),1,0),
         Animation = ifelse(str_detect(Genres, 'Animation'),1,0),
         Children = ifelse(str_detect(Genres, 'Children'),1,0),
         Comedy = ifelse(str_detect(Genres, 'Comedy'),1,0),
         Crime = ifelse(str_detect(Genres, 'Crime'),1,0),
         Documentary = ifelse(str_detect(Genres, 'Documentary'),1,0),
         Drama = ifelse(str_detect(Genres, 'Drama'),1,0),
         Fantasy = ifelse(str_detect(Genres, 'Fantasy'),1,0),
         `Film-Noir` = ifelse(str_detect(Genres, 'Film-Noir'),1,0),
         Horror = ifelse(str_detect(Genres, 'Horror'),1,0),
         Musical = ifelse(str_detect(Genres, 'Musical'),1,0),
         Mystery = ifelse(str_detect(Genres, 'Mystery'),1,0),
         Romance = ifelse(str_detect(Genres, 'Romance'),1,0),
         `Sci-Fi` = ifelse(str_detect(Genres, 'Sci-Fi'),1,0),
         Thriller = ifelse(str_detect(Genres, 'Thriller'),1,0),
         War = ifelse(str_detect(Genres, 'War'),1,0),
         Western = ifelse(str_detect(Genres, 'Western'),1,0))
movie_genre <- na.omit(movie_genre)

movie_genre <- rating %>% filter(!is.na(Rating)) %>% 
  group_by(MovieID) %>% summarise(Rate = mean(Rating), watch = n()) %>% 
  left_join(movie_genre, by = c("MovieID"))

yearly_genre <- movie_genre %>% filter(watch > 100 & Rate > 3) %>% 
  group_by(year) %>% summarise_at(c(7:23), sum)
yearly_genre <- na.omit(yearly_genre)

yearly_genre$chosen <- colnames(yearly_genre %>% 
                                  select(-year))[max.col(yearly_genre %>% 
                                                           select(-year),ties.method="first")]
knitr::kable(yearly_genre %>% select(year, chosen))
