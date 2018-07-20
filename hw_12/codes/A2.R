library(reshape2)

# a: count of genre movies
count_genre <- movie_genre  %>% summarise_at(c(7:24), sum)
count_genre = melt(count_genre)
colnames(count_genre) = c('genre', 'count')

count_genre %>% arrange(desc(count)) %>% 
  hchart(type = "column", hcaes(x = genre, y = count, color = count)) %>% 
  hc_yAxis(title = list(text = "Count")) %>% 
  hc_xAxis(title = list(text = "Genre")) %>% 
  hc_title(text = "Number of Movie Produced in Each Genre", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_538())


# b: genres correlation plot
library(corrplot)
corrplot(cor(movie_genre  %>% select(c(7:24))), method = "color", tl.cex = 0.5/par("cex"), cl.cex = 0.5/par("cex"))

# c: average rating for genres
library(ggplot2)

rate_genre <- movie_genre %>% 
  mutate(Action = Rate*Action,
         Adventure = Rate*Adventure,
         Animation = Rate*Animation,
         Children = Rate*Children,
         Comedy = Rate*Comedy,
         Crime = Rate*Crime,
         Documentary = Rate*Documentary,
         Drama = Rate*Drama ,
         Fantasy = Rate*Fantasy,
         `Film-Noir` = Rate*`Film-Noir`,
         Horror = Rate*Horror,
         Musical = Rate*Musical,
         Mystery = Rate*Mystery,
         Romance = Rate*Romance,
         `Sci-Fi` = Rate*`Sci-Fi`,
         Thriller = Rate*Thriller,
         War = Rate*War,
         Western = Rate*Western)

rate_sum_genre <- rate_genre %>% summarise_at(c(7:24), sum)
count_genre <- movie_genre  %>% summarise_at(c(7:24), sum)

rate_mean_genre <- rate_sum_genre/count_genre
mrate_mean_genre = melt(rate_mean_genre)
colnames(mrate_mean_genre) = c('genre', 'mean')

p = ggplot(data = mrate_mean_genre, mapping = aes(x = genre, y = mean, fill = mean)) + 
  ggtitle("Genre Average Rating") + geom_bar(stat="identity") + 
  scale_fill_gradient(low="gold", high="darkgreen") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

knitr::kable(mrate_mean_genre)

# d: cinema golden age
movie_rate <- movie_genre %>% select(-c(7:24)) %>% 
  mutate(year = as.numeric(str_extract(year, '[[:digit:]]{4}'))) %>% 
  mutate(decade = year - (year %% 10)) %>% 
  group_by(decade) %>% summarise(watch_mean = mean(watch), watch = sum(watch), rate_mean = mean(Rate)) %>% 
  arrange(desc(rate_mean))

golden_age <- movie_rate %>% slice(1)
cat("Cinema Golden Age is", golden_age[1,1,1], "with rating mean of", golden_age[1,4,1])

movie_rate  %>% arrange(decade) %>% 
  hchart(type = "line", hcaes(x = decade, y = rate_mean)) %>% 
  hc_yAxis(title = list(text = "Average Rating")) %>% 
  hc_xAxis(title = list(text = "Decade")) %>% 
  hc_title(text = "Rating Average in Decades", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_flat())


