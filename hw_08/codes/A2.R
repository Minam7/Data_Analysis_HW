library(wordcloud2)

word_fig <- all_books %>% slice(1:200) %>% select(word, freq = count)

wordcloud2(word_fig , figPath = "images/dickens1_1.png", size = 0.17, color = "black")
