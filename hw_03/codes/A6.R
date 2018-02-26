longest_run <- res_table %>%
  ungroup() %>% group_by(team) %>% 
  arrange(Season, game_nu) %>%
  mutate(run = sequence(rle(as.character(score))$lengths)) %>% 
  group_by(score) %>% 
  filter(run == max(run)) %>% 
  arrange(-run) %>% 
  ungroup()

knitr::kable(longest_run %>% 
               filter(score == 3) %>% 
               select(Season = Season, Team = team, Strike = run))

knitr::kable(longest_run %>% 
               filter(score == 1) %>% 
               select(Season = Season, Team = team, Strike = run))

knitr::kable(longest_run %>% 
               filter(score == 0) %>% 
               select(Season = Season, Team = team, Strike = run))
