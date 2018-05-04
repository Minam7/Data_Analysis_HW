she_or_he <- untidy_colocation_count %>% filter(first_word == "she" | first_word == "he")

she_or_he <- she_or_he %>% group_by(first_word) %>% arrange(desc(n)) %>% top_n(20, wt = n)

# with stopwords
she_or_he %>% ungroup() %>% filter(first_word == "she") %>% rename(count = n) %>% 
  hchart(type = "column",hcaes(x = second_word, y = count, color = count)) %>% 
  hc_title(text = "Most repeated female verbs with stopwords", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_flat())

she_or_he %>% ungroup() %>% filter(first_word == "he") %>% rename(count = n) %>% 
  hchart(type = "column",hcaes(x = second_word, y = count, color = count)) %>% 
  hc_title(text = "Most repeated male verbs with stopwords", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_darkunica())

# without stopwords
she_or_he <- untidy_colocation_count %>% filter(first_word == "she" | first_word == "he") %>% 
  filter(!second_word %in% stop_words$word) %>% 
  group_by(first_word) %>% arrange(desc(n)) %>% top_n(20, wt = n)

she_or_he %>% ungroup() %>% filter(first_word == "she") %>% rename(count = n) %>% 
  hchart(type = "column",hcaes(x = second_word, y = count, color = count)) %>% 
  hc_title(text = "Most repeated female verbs without stopwords", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_flat())

she_or_he %>% ungroup() %>% filter(first_word == "he") %>% rename(count = n) %>% 
  hchart(type = "column",hcaes(x = second_word, y = count, color = count)) %>% 
  hc_title(text = "Most repeated male verbs without stopwords", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_darkunica())
