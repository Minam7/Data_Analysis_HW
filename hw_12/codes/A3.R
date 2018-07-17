library(wordcloud2)
library(tidytext)

title_data <- movie %>% select(text = Title) %>% mutate(text = str_to_lower(text)) %>% 
  str_replace_all("\"","") %>% 
  str_replace("[[:punct:]]", "") %>% 
  str_split(pattern = "\\s") %>% 
  unlist() %>% as.data.frame(stringsAsFactors = F) %>% 
  filter( . != "")
  

title_data <- title_data %>% table() %>% 
  as.data.frame(stringsAsFactors = F)

colnames(title_data) = c("word","count")

title_data <- title_data %>%
  filter(!word %in% stop_words$word) %>% 
  filter(str_length(word)>1) %>% 
  filter(!str_detect(word,"\\d")) %>%
  filter(!str_detect(word,"\\(")) %>%
  filter(!str_detect(word,"[[:punct:]]")) %>% 
  arrange(desc(count)) %>% 
  filter(count > 4) %>% 
  select(word, freq = count)

wordcloud2(title_data , figPath = "images/movie-projector.png" , size = 0.9, color = "black")

