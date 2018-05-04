uni_data = list()
bi_data = list()
for( i in 1:nrow(dickens_list)){
  book = books_texts[[i]]
  wbook = book %>% mutate(chapter = cumsum(str_detect(text, regex("chapter [\\divxlc]+", ignore_case = TRUE)))) %>%
    filter(text != "") %>% 
    mutate(book = dickens_list[i,2,1]) %>% 
    group_by(chapter)
  
  bi = wbook %>% unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% ungroup() %>% 
    separate(bigram, c("first_word", "second_word"), sep = " ") %>%
    filter(!first_word %in% stop_words$word) %>%
    filter(!second_word %in% stop_words$word) %>% 
    group_by(chapter) %>% 
    count(book, first_word, second_word, sort = TRUE) %>% 
    mutate(collocation = paste(first_word, second_word, sep=" "), 
           chapter_words = sum(n)) %>% 
    mutate(index = row_number(), tf = n/chapter_words) %>% 
    filter(index < 500, index > 10)
  bi_data[[i]] = bi
  
  uni = wbook %>% unnest_tokens(unigram, text, token = "ngrams", n = 1) %>% ungroup() %>% 
    filter(!unigram %in% stop_words$word) %>%
    group_by(chapter) %>% 
    count(book, unigram , sort = TRUE) %>% 
    mutate(chapter_words = sum(n), index = row_number(),
           tf = n/chapter_words) %>% 
    filter(index < 500, index > 10)
  uni_data[[i]] = uni
}

unigram_data_all = bind_rows(uni_data) %>% as.data.frame(stringsAsFactors = F)
unigram_data_all <- unigram_data_all %>% ungroup() %>% 
  group_by(book, chapter)

co = lm(log10(tf) ~ log10(index), data = unigram_data_all)
p = unigram_data_all %>% 
  ggplot(aes(index, tf, color = book)) + 
  geom_abline(intercept = co$coefficients[1], slope = co$coefficients[2], color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 1) + 
  ylab("term frequency") +
  ggtitle("1-gram distribution for Charles Dickens") +
  scale_x_log10() +
  scale_y_log10()
p

bigram_data_all = bind_rows(bi_data) %>% as.data.frame(stringsAsFactors = F)
bigram_data_all <- bigram_data_all %>% ungroup() %>% 
  group_by(book, chapter)

co = lm(log10(tf) ~ log10(index), data = bigram_data_all)
p = bigram_data_all %>% 
  ggplot(aes(index, tf, color = book)) + 
  geom_abline(intercept = co$coefficients[1], slope = co$coefficients[2], color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 1) + 
  ylab("term frequency") +
  ggtitle("2-gram distribution for Charles Dickens") +
  scale_x_log10() +
  scale_y_log10()
p