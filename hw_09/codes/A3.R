max_flow <- sp500 %>%
  group_by(Date) %>% 
  summarise(flow = sum(Volume)) %>% 
  arrange(desc(flow)) %>% slice(1:4)

knitr::kable(max_flow)