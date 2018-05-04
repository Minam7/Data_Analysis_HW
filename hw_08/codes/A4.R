tidy_data = list()
for( i in 1:nrow(dickens_list)){
  book = books_texts[[i]]
  wbook = book %>% mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("chapter [\\divxlc]+", 
                                                 ignore_case = TRUE)))) %>%
    unnest_tokens(word, text) %>% 
    mutate(book = dickens_list[i,2,1])
  tidy_data[[i]] = wbook
}
tidy_books = bind_rows(tidy_data)
tidy_books  <- tidy_books %>% anti_join(stop_words)

bing_word_counts <- tidy_books %>%
  group_by(book) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

for( i in 1:nrow(dickens_list)){
  plot_name = paste("Sentiment in ", dickens_list[i,2,1], sep=" ")
  p <- bing_word_counts %>%
    filter(book == dickens_list[i,2,1]) %>% 
    group_by(sentiment) %>%
    top_n(20) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_grid(~sentiment, scales="free_y") +
    labs(y = plot_name,
         x = NULL) +
    coord_flip()
  print(p)
}

