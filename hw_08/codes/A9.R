woolf_list <- gutenberg_works(author == "Austen, Jane")
woolf_list <- woolf_list[-c(6, 9, 10), ]

woolf_texts = list()
# downloading all books
for( i in 1:nrow(woolf_list)){
  book = gutenberg_download(woolf_list[i, 1, 1])
  woolf_texts[[i]] = book
}
saveRDS(woolf_texts, file="data/woolf.rds")

woolf_texts = readRDS(file="data/woolf.rds")

woolf_uni_data = list()
woolf_bi_data = list()
for( i in 1:nrow(woolf_list)){
  book = woolf_texts[[i]]
  wbook = book %>% mutate(chapter = cumsum(str_detect(text, regex("chapter [\\divxlc]+", ignore_case = TRUE)))) %>%
    filter(text != "") %>% 
    group_by(chapter)
  
  bi = wbook %>% unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("first_word", "second_word"), sep = " ") %>% 
    count(first_word, second_word, sort = TRUE) %>% 
    mutate(collocation = paste(first_word, second_word, sep=" "),
           chapter_words = sum(n)) %>% 
    mutate(index = row_number(), tf = n/chapter_words) %>% 
    mutate(book = woolf_list[i,2,1])
  woolf_bi_data[[i]] = bi
    
  uni = wbook %>% unnest_tokens(unigram, text, token = "ngrams", n = 1) %>% 
    count(unigram , sort = TRUE) %>% 
    mutate(chapter_words = sum(n), index = row_number(),
           tf = n/chapter_words) %>% 
    mutate(book = woolf_list[i,2,1])
  woolf_uni_data[[i]] = uni
}

woolf_unigram_data_all = bind_rows(woolf_uni_data) %>% as.data.frame(stringsAsFactors = F)
woolf_unigram_data_all <- woolf_unigram_data_all %>% ungroup() %>% 
  group_by(book, chapter)

co = lm(log10(tf) ~ log10(index), data = woolf_unigram_data_all)
p = woolf_unigram_data_all %>% 
  ggplot(aes(index, tf, color = book)) + 
  geom_abline(intercept = co$coefficients[1], slope = co$coefficients[2], color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 1) + 
  ylab("term frequency") +
  ggtitle("1-gram distribution for Jane Austen") +
  scale_x_log10() +
  scale_y_log10()
p

woolf_bigram_data_all = bind_rows(woolf_bi_data) %>% as.data.frame(stringsAsFactors = F)
woolf_bigram_data_all <- woolf_bigram_data_all %>% ungroup() %>% 
  group_by(book, chapter)

co = lm(log10(tf) ~ log10(index), data = woolf_bigram_data_all)
p = woolf_bigram_data_all %>% 
  ggplot(aes(index, tf, color = book)) + 
  geom_abline(intercept = co$coefficients[1], slope = co$coefficients[2], color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 1) + 
  ylab("term frequency") +
  ggtitle("2-gram distribution for Jane Austen") +
  scale_x_log10() +
  scale_y_log10()
p

kruskal.test(book ~ tf, woolf_unigram_data_all)

kruskal.test(book ~ tf, woolf_bigram_data_all)


wilcox.test(unigram_data_all$tf, woolf_unigram_data_all$tf,alternative = "two.sided", exact = FALSE, correct = FALSE)

wilcox.test(bigram_data_all$tf, woolf_bigram_data_all$tf,alternative = "two.sided", exact = FALSE, correct = FALSE)
