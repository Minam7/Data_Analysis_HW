library(wordcloud2)

word_fig <- all_books %>% slice(1:200) %>% select(word, freq = count)

wordcloud2(word_fig , figPath = "images/face.png" ,size = .5, color = "black")