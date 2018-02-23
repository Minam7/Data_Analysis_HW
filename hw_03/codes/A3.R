# full league results
res_table <- rbind(
  laLiga %>% mutate(team = home,
                    opponent = visitor,
                    GF = hgoal,
                    GA = vgoal,
                    GD = GF-GA) ,
  laLiga %>% mutate(team = visitor,
                    opponent = home, 
                    GF = vgoal,
                    GA = hgoal,
                    GD = GF-GA)
  ) %>% select(Date, Season, team, opponent, GF, GA, GD)

all_table <- res_table %>% group_by(Season, team) %>% 
  mutate(score = ifelse(GD > 0, 3, ifelse(GD < 0, 0, 1)),
         game_nu = dense_rank(Date),
         time = Date) %>% 
  arrange(Season, team, game_nu) %>% 
  mutate(score_cum = cumsum(score),
         GF_cum = cumsum(GF),
         GA_cum = cumsum(GA),
         GD_cum = cumsum(GD)
         ) %>% 
  select(Season, team, game_nu, time, score_cum, GF_cum, GA_cum, GD_cum)

all_table <- all_table %>% ungroup() %>% group_by(Season, game_nu) %>% 
  mutate(time = min(time)) %>% ungroup() %>% group_by(Season, team)

# full season results
full_season <- all_table %>% 
  filter(game_nu == max(game_nu)) %>% 
  arrange(Season, desc(score_cum), desc(GD_cum)) %>% 
  group_by(Season) %>%
  mutate(rank = row_number() %>% as.integer()) 

# champions in full season
full_champions <- full_season %>% filter(rank == 1)

  
# half season results
half_season <- all_table %>% 
  filter(game_nu == floor(max(game_nu)/2)) %>% 
  arrange(Season,desc(score_cum), desc(GD_cum)) %>% 
  group_by(Season) %>% 
  mutate(rank = row_number() %>% as.integer()) 

half_champions <- half_season %>% filter(rank == 1)

tot <- all_table %>% ungroup() %>% select(Season) %>% distinct() %>% nrow()

half_full <- inner_join(half_champions, full_champions, by = "Season")

half_full <- half_full %>% mutate(same = ifelse(team.x == team.y, 1, 0))

yes <- half_full %>% ungroup() %>%  summarise(sum(same))

res <- yes*100/tot

cat("only ", res[1]$sum, "percent of half season champions are full season champions.")

