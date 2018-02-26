res_table <- res_table %>% group_by(Season, team) %>% 
  mutate(score = ifelse(GD > 0, 3, ifelse(GD < 0, 0, 1)),
         game_nu = dense_rank(Date))

all_data <- res_table %>% inner_join(a_rank, by = c("Season", "game_nu", "opponent" = "team"))

interval <- all_data %>% filter(Season < 2011 & Season > 2000) %>% 
  filter(team == "Valencia CF" | team == "Real Madrid" | team == "Atletico Madrid" | team == "FC Barcelona") %>%
  ungroup %>% group_by(Season) %>% 
  filter(GD < 1) %>%
  filter(rank > round(max(rank)/2)) %>% 
  filter(opponent != "Valencia CF" & opponent != "Real Madrid" & opponent != "Atletico Madrid" & opponent != "FC Barcelona") %>% 
  ungroup() %>% group_by(team, opponent) %>% 
  summarise(lost = n()) %>% 
  ungroup() %>% group_by(team) %>% 
  filter(lost == max(lost))

interval %>% 
  hchart(type = "scatter", hcaes(x = opponent, y = lost, group = team)) %>% 
  hc_yAxis(title = list(text = "losts")) %>% 
  hc_xAxis(type = 'category', title = list(categories = interval$opponent, text = "opponent")) %>% 
  hc_title(text = "Best Four Teams Black Cats in 2001-2010", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_darkunica())

p1 = ggplot(data = interval, mapping = aes(x = opponent, y = lost, color = team)) + ggtitle("Best Four Teams Black Cats in 2001-2010") + geom_point(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1
