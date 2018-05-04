hugo_list <- gutenberg_works(author == "Hugo, Victor")
hugo_list <- hugo_list[c(9:13), ] 

miserable_texts = list()
# downloading all books
for( i in 1:nrow(hugo_list)){
  book = gutenberg_download(hugo_list[i, 1, 1])
  miserable_texts[[i]] = book
}
saveRDS(miserable_texts, file="data/miserable.rds")

tidy_data = list()
for( i in 1:nrow(hugo_list)){
  book = miserable_texts[[i]]
  wbook = book %>% mutate(linenumber = row_number(),
                          chapter = cumsum(str_detect(text, regex("chapter [\\divxlc]+", 
                                                                  ignore_case = TRUE)))) %>%
    unnest_tokens(word, text) %>% 
    mutate(book = hugo_list[i,2,1])
  tidy_data[[i]] = wbook
}
miserable_books = bind_rows(tidy_data)
miserabley_books <- miserable_books %>% anti_join(stop_words)

n <- 200
nr <- nrow(miserabley_books)
miserabley_books_part <- split(miserabley_books, rep(1:ceiling(nr/n), each=n, length.out=nr))

books = miserabley_books_part[[1]]
mis_bing_word_counts <- books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
pos = c(mis_bing_word_counts %>% filter(sentiment == "positive") %>% count())
neg = c(mis_bing_word_counts %>% filter(sentiment == "negative") %>% count())
mis_atmo = data.frame(pos , neg, stringsAsFactors=FALSE)
colnames(mis_atmo) = c("pos", "neg")

for( i in 2:200){
  books = miserabley_books_part[[i]]
  mis_bing_word_counts <- books %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE)
  pos = c(mis_bing_word_counts %>% filter(sentiment == "positive") %>% count())
  neg = c(mis_bing_word_counts %>% filter(sentiment == "negative") %>% count())
  mis_atmo1 = data.frame(pos , neg, stringsAsFactors=FALSE)
  colnames(mis_atmo1) = c("pos", "neg")
  mis_atmo = bind_rows(mis_atmo, mis_atmo1)
}

mis_atmo <- mis_atmo %>% mutate(part = row_number())

hc <- highchart() %>% 
  hc_xAxis(categories = mis_atmo$part) %>% 
  hc_add_series(name = "Postive", data = mis_atmo$pos) %>% 
  hc_add_series(name = "Negative", data = mis_atmo$neg) %>% 
  hc_title(text = "Emotional Atmosphere of Les Misérables", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_elementary())

hc
