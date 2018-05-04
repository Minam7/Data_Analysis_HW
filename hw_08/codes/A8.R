tidy_data = list()
for( i in 1:nrow(dickens_list)){
  book = books_texts[[i]]
  wbook = book %>% mutate(chapter = cumsum(str_detect(text, regex("chapter [\\divxlc]+", ignore_case = TRUE)))) %>%
    filter(text != "") %>% 
    mutate(book = dickens_list[i,2,1])
  
  bi = wbook %>% unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
    separate(bigram, c("first_word", "second_word"), sep = " ") %>%
    filter(!first_word %in% stop_words$word) %>%
    filter(!second_word %in% stop_words$word) %>% 
    group_by(chapter) %>% 
    count(first_word, second_word, sort = TRUE) %>% 
    mutate(collocation = paste(first_word, second_word, sep=" "))
  
  uni = wbook %>% unnest_tokens(unigram, text, token = "ngrams", n = 1) %>% 
    filter(!unigram %in% stop_words$word) %>%
    group_by(chapter) %>% 
    count(unigram , sort = TRUE)
    
  
  tidy_data[[i]] = list(bi, uni)
}
