tidy_data = list()
for( i in 1:nrow(dickens_list)){
  book = books_texts[[i]]
  wbook = book %>% mutate(linenumber = row_number(),
                          chapter = cumsum(str_detect(text, regex("chapter [\\divxlc]+", 
                                                                  ignore_case = TRUE)))) %>%
    filter(text != "") %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
    mutate(book = dickens_list[i,2,1])
  tidy_data[[i]] = wbook
}

tidy_colocation_books = bind_rows(tidy_data)

tidy_colocation_separated <- tidy_colocation_books %>%
  separate(bigram, c("first_word", "second_word"), sep = " ")

untidy_colocation_count <- tidy_colocation_separated %>% 
  select(-gutenberg_id, -linenumber) %>% 
  count(first_word, second_word, sort = TRUE) %>% 
  mutate(collocation = paste(first_word, second_word, sep=" "))

top_30_collocation <- untidy_colocation_count %>% slice(1:30) %>% 
  rename(count = n)

top_30_collocation %>% 
  hchart(type = "column",hcaes(x = collocation, y = count, color = count)) %>% 
  hc_title(text = "Most repeated collocations with stopwords", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_sandsignika())

# removing stop words
tidy_colocation_filtered <- tidy_colocation_separated %>%
  filter(!first_word %in% stop_words$word) %>%
  filter(!second_word %in% stop_words$word)

tidy_colocation_count <- tidy_colocation_filtered %>% 
  count(first_word, second_word, sort = TRUE)

tidy_colocation_count <- tidy_colocation_count[-c(1, 2), ]

tidy_colocation_count <- tidy_colocation_count %>%
  mutate(collocation = paste(first_word, second_word, sep=" "))

top_30_collocation <- tidy_colocation_count %>% slice(1:30) %>% 
  rename(count = n)

top_30_collocation %>% 
  hchart(type = "column",hcaes(x = collocation, y = count, color = count)) %>% 
  hc_title(text = "Most repeated collocations without stopwords", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_sandsignika())
