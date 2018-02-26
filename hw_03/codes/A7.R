# earliest downfall
last <- a_rank %>% group_by(Season, game_nu) %>% filter(rank == max(rank))
one_before_last <- a_rank %>% group_by(Season, game_nu) %>% filter(rank == max(rank) - 1)
two_before_last <- a_rank %>% group_by(Season, game_nu) %>% filter(rank == max(rank) - 2)
three_before_last <- a_rank %>% group_by(Season, game_nu) %>% filter(rank == max(rank) - 3)

last_fourth <- last %>% inner_join(three_before_last, by = c("Season", "game_nu")) %>% 
  mutate(diff = score_cum.y - score_cum.x) %>% 
  select(Season = Season, game_nu = game_nu, team = team.x, diff = diff) %>% 
  group_by(Season) %>% 
  mutate(remaining_game = max(game_nu) - game_nu, is_fall = ifelse(remaining_game*3 < diff, TRUE, FALSE)) %>% 
  filter(is_fall) %>% ungroup() %>% 
  filter(remaining_game == max(remaining_game)) %>% 
  select(Season = Season, Team = team, Game_Number = game_nu, Remaining_Game = remaining_game, Score_Difference = diff) %>% 
  arrange(-Score_Difference)

one_before_last_fourth <- one_before_last %>% inner_join(three_before_last, by = c("Season", "game_nu")) %>% 
  mutate(diff = score_cum.y - score_cum.x) %>% 
  select(Season = Season, game_nu = game_nu, team = team.x, diff = diff) %>% 
  group_by(Season) %>% 
  mutate(remaining_game = max(game_nu) - game_nu, is_fall = ifelse(remaining_game*3 < diff, TRUE, FALSE)) %>% 
  filter(is_fall) %>% ungroup() %>% 
  filter(remaining_game == max(remaining_game)) %>% 
  select(Season = Season, Team = team, Game_Number = game_nu, Remaining_Game = remaining_game, Score_Difference = diff) %>% 
  arrange(-Score_Difference)

falldown <- rbind(last_fourth, one_before_last_fourth)

two_before_last_fourth <- two_before_last %>% inner_join(three_before_last, by = c("Season", "game_nu")) %>% 
  mutate(diff = score_cum.y - score_cum.x) %>% 
  select(Season = Season, game_nu = game_nu, team = team.x, diff = diff) %>% 
  group_by(Season) %>% 
  mutate(remaining_game = max(game_nu) - game_nu, is_fall = ifelse(remaining_game*3 < diff, TRUE, FALSE)) %>% 
  filter(is_fall) %>% ungroup() %>% 
  filter(remaining_game == max(remaining_game)) %>% 
  select(Season = Season, Team = team, Game_Number = game_nu, Remaining_Game = remaining_game, Score_Difference = diff) %>% 
  arrange(-Score_Difference)

falldown <- rbind(falldown, two_before_last_fourth)

falldown <- falldown %>% filter(Remaining_Game == max(Remaining_Game))

knitr::kable(falldown)
