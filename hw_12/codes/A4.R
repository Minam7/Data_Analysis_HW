library(arules)

transact <- rating %>% filter(Rating > 2.5) %>% select(UserID, MovieID)
user_item_matrix <- as(split(transact$MovieID, transact[,"UserID"]), "transactions")
user_item_matrix

parameters = list(
  supp = 0.001,
  conf = 0.7,
  maxlen = 2
)
association_rules = apriori(user_item_matrix,parameter = parameters)
summary(association_rules)

association_rules = subset(association_rules, lift >= 3.785)
summary(association_rules)

movie_recs <- association_rules %>% as(., "data.frame") %>% 
  mutate(lhs = as.numeric(str_extract(rules, "[[:digit:]]+")),
         rhs = as.numeric(str_extract(str_extract(rules, " \\{[[:digit:]]+"), "[[:digit:]]+"))) %>% 
  mutate(MovieID = lhs) %>% select(-lhs) %>% 
  inner_join(movie %>% select(MovieID, Title), by = c("MovieID")) %>% 
  mutate(lhs = MovieID, lhs_title = Title) %>% select(-MovieID, -Title) %>% 
  mutate(MovieID = rhs) %>% select(-rhs) %>% 
  inner_join(movie %>% select(MovieID, Title), by = c("MovieID")) %>% 
  mutate(rhs = MovieID, rhs_title = Title) %>% select(-MovieID, -Title) %>% 
  filter(lhs_title == "Castle in the Sky (Tenkû no shiro Rapyuta) (1986)" | lhs_title == "Cast Away (2000)" | 
           lhs_title == "No Country for Old Men (2007)" | lhs_title == "Memento (2000)" |
           rhs_title == "Castle in the Sky (Tenkû no shiro Rapyuta) (1986)" | rhs_title == "Cast Away (2000)" | 
           rhs_title == "No Country for Old Men (2007)" | rhs_title == "Memento (2000)") %>% 
  group_by(rhs) %>% 
  arrange(desc(lift)) %>% 
  slice(1) %>% 
  group_by(lhs) %>% 
  arrange(desc(lift)) %>% 
  slice(1) %>% 
  ungroup()

movie_r <- bind_rows(movie_recs %>% filter(rhs_title == "Cast Away (2000)" | rhs_title == "No Country for Old Men (2007)" | rhs_title == "Memento (2000)") %>% 
  select(Movie = rhs_title, Recommended = lhs_title), 
  movie_recs %>% filter(lhs_title == "Castle in the Sky (Tenkû no shiro Rapyuta) (1986)") %>% 
  select(Movie = lhs_title, Recommended = rhs_title))

knitr::kable(movie_r)
