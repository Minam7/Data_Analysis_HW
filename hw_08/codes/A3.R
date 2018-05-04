library(highcharter)

character_list = list()
for( i in 1:nrow(dickens_list)){
  book = books_texts[[i]]
  wbook = book %>% 
    str_replace_all("\"","") %>% 
    str_replace_all("[[:punct:]]","") %>% 
    str_split(pattern = "\\s") %>% 
    unlist() %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = F)
  
  colnames(wbook) = c("word","count")
  wbook = wbook %>%
    filter(!str_to_lower(word) %in% stop_words$word) %>% 
    filter(str_length(word)>1) %>% 
    filter(!str_detect(word,"\\d")) %>%
    arrange(desc(count)) %>% 
    mutate(proper = !word %in% str_to_lower(word)) %>% 
    mutate(Book = dickens_list[i,2,1]) %>% 
    filter(proper == TRUE)
  character_list[[i]] = wbook
}

characters = bind_rows(character_list)
    
top_characters <- characters %>% 
  group_by(Book) %>% 
  mutate(percent = round(100*count/sum(count))) %>% 
  arrange(desc(percent)) %>% 
  mutate(rank = row_number() %>% as.integer()) %>% 
  filter(rank < 6) %>%
  rename(name = word)

top_characters %>% 
  hchart("column", hcaes(x = Book, y = percent, group = name)) %>% 
  hc_add_theme(hc_theme_google())
