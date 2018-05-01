library(highcharter)

dickens_list <- gutenberg_works(author == "Dickens, Charles")
dickens_list <- dickens_list[-c(51, 59, 65, 67, 68, 69, 70, 71, 72), ]
dickens_list <- dickens_list %>% select(gutenberg_id, title)

books_texts = list()

# downloading all books
for( i in 1:nrow(dickens_list)){
  book = gutenberg_download(dickens_list[i, 1, 1])
  books_texts[[i]] = book
}

saveRDS(books_texts, file="data/booktext.rds")

# using books
all_books = list()
for( i in 1:nrow(dickens_list)){
  book = books_texts[[i]]
  wbook = book %>% mutate(text = str_to_lower(text)) %>% 
    select(text) %>% 
    str_replace_all("\"","") %>% 
    str_replace_all("[[:punct:]]","") %>% str_split(pattern = "\\s") %>% 
    unlist() %>% as.data.frame(stringsAsFactors = F)
    if(i == 1){
      all_books = wbook
    } else {
      all_books = bind_rows(all_books, wbook)
    }
}  
  

all_books <- all_books %>% table() %>% 
  as.data.frame(stringsAsFactors = F)
  
colnames(all_books) = c("word","count")

all_books <- all_books %>%
    filter(!word %in% stop_words$word) %>% 
    filter(str_length(word)>1) %>% 
    filter(!str_detect(word,"\\d")) %>%
    arrange(desc(count))

top_words <- all_books %>% slice(1:20)

top_words %>% 
  hchart(type = "column",hcaes(x = word, y = count, color = count)) %>% 
  hc_title(text = "Most repeated words", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_sandsignika())

