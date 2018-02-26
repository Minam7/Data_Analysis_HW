# earliest championship
first <- a_rank %>% group_by(Season, game_nu) %>% filter(rank == 1)
second <- a_rank %>% group_by(Season, game_nu) %>% filter(rank == 2)
first_sec <- first %>% inner_join(second, by = c("Season", "game_nu")) %>% 
  mutate(diff = score_cum.x - score_cum.y) %>% 
  select(Season = Season, game_nu = game_nu, team = team.x, diff = diff) %>% 
  ungroup() %>% group_by(Season) %>% 
  mutate(remaining_game = max(game_nu) - game_nu, is_win = ifelse(remaining_game*3 < diff, TRUE, FALSE)) %>% 
  filter(is_win == TRUE) %>% ungroup() %>% 
  filter(remaining_game == max(remaining_game)) %>% 
  select(Season = Season, Team = team, Game_Number = game_nu, Remaining_Game = remaining_game, Score_Difference = diff) %>% 
  arrange(-Score_Difference)
  
knitr::kable(first_sec)


# most powerful championship in history
second <- full_season %>% filter(rank == 2)
first <- full_season %>% filter(rank == 1)
first_sec <- first %>% inner_join(second, by = "Season") %>% mutate(diff = score_cum.x - score_cum.y) %>% select(Season = Season, team = team.x, diff = diff)
powerful_champ <- first_sec %>% ungroup %>% filter(diff == max(diff))
cat('most powerful championship is:', powerful_champ[2]$team, " in ", powerful_champ[1]$Season)
