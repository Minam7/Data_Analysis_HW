library(reshape2)

# a: count of genre movies
count_genre <- movie_genre  %>% summarise_at(c(7:24), sum)
count_genre = melt(count_genre)
colnames(count_genre) = c('genre', 'count')

count_genre %>% 
  hchart(type = "column", hcaes(x = genre, y = count, color = count)) %>% 
  hc_yAxis(title = list(text = "Count")) %>% 
  hc_xAxis(title = list(text = "Genre")) %>% 
  hc_title(text = "Number of Movie Produced in Each Genre", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_538())


# b: genres correlation plot
library(corrplot)
corrplot(cor(movie_genre  %>% select(c(7:24))), method = "color", tl.cex = 0.5/par("cex"), cl.cex = 0.5/par("cex"))

# c: average rating for genres
