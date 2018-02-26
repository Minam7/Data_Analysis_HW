###### 10 weakest teams of all time
weak <- full_season %>%
  filter(rank == max(rank)) %>% 
  mutate(score_rate = round(10*score_cum/(max(game_nu)*3))) %>% 
  ungroup %>% filter(score_rate == min(score_rate))

weak_plot = ggplot(data = weak, mapping = aes(y = Season, x = team, color = score_cum/10)) + geom_point(stat="identity") + scale_color_gradient(low="red", high="blue") + ggtitle("Weakest teams of all time") + ylab("season") + guides(color=guide_legend(title="score rate")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
weak_plot


###### 1990 rank 13 bad luck in 1991 - 2016
bad_luck <- full_season %>% 
  filter(Season == 1990) %>%
  filter(rank == 13)


what <- full_season %>% 
  filter(Season > 1990) %>% 
  filter(team == bad_luck$team) %>% 
  ungroup() %>% group_by(team) %>% 
  summarise(rank = round(mean(rank)))

destiny <- what

bad_luck <- full_season %>% 
  filter(Season == 1991) %>%
  filter(rank == 13)

what <- full_season %>% 
  filter(Season > 1991) %>% 
  filter(team == bad_luck$team) %>% 
  ungroup() %>% group_by(team) %>% 
  summarise(rank = round(mean(rank)))

destiny <- rbind(destiny, what)

bad_luck <- full_season %>% 
  filter(Season == 1992) %>%
  filter(rank == 13)

what <- full_season %>% 
  filter(Season > 1992) %>% 
  filter(team == bad_luck$team) %>% 
  ungroup() %>% group_by(team) %>% 
  summarise(rank = round(mean(rank)))

destiny <- rbind(destiny, what)

bad_luck <- full_season %>% 
  filter(Season == 1993) %>%
  filter(rank == 13)

what <- full_season %>% 
  filter(Season > 1993) %>% 
  filter(team == bad_luck$team) %>% 
  ungroup() %>% group_by(team) %>% 
  summarise(rank = round(mean(rank)))

destiny <- rbind(destiny, what)

bad_luck <- full_season %>% 
  filter(Season == 1994) %>%
  filter(rank == 13)

what <- full_season %>% 
  filter(Season > 1994) %>% 
  filter(team == bad_luck$team) %>% 
  ungroup() %>% group_by(team) %>% 
  summarise(rank = round(mean(rank)))

destiny <- rbind(destiny, what)

bad_luck <- full_season %>% 
  filter(Season == 1995) %>%
  filter(rank == 13)

what <- full_season %>% 
  filter(Season > 1995) %>% 
  filter(team == bad_luck$team) %>% 
  ungroup() %>% group_by(team) %>% 
  summarise(rank = round(mean(rank)))

destiny <- rbind(destiny, what)

bad_luck <- full_season %>% 
  filter(Season == 1996) %>%
  filter(rank == 13)

what <- full_season %>% 
  filter(Season > 1996) %>% 
  filter(team == bad_luck$team) %>% 
  ungroup() %>% group_by(team) %>% 
  summarise(rank = round(mean(rank)))

destiny <- rbind(destiny, what)

bad_luck <- full_season %>% 
  filter(Season == 1997) %>%
  filter(rank == 13)

what <- full_season %>% 
  filter(Season > 1997) %>% 
  filter(team == bad_luck$team) %>% 
  ungroup() %>% group_by(team) %>% 
  summarise(rank = round(mean(rank)))

destiny <- rbind(destiny, what)

bad_luck <- full_season %>% 
  filter(Season == 1998) %>%
  filter(rank == 13)

what <- full_season %>% 
  filter(Season > 1998) %>% 
  filter(team == bad_luck$team) %>% 
  ungroup() %>% group_by(team) %>% 
  summarise(rank = round(mean(rank)))

destiny <- rbind(destiny, what)

bad_luck <- full_season %>% 
  filter(Season == 1999) %>%
  filter(rank == 13)

what <- full_season %>% 
  filter(Season > 1999) %>% 
  filter(team == bad_luck$team) %>% 
  ungroup() %>% group_by(team) %>% 
  summarise(rank = round(mean(rank)))

destiny <- rbind(destiny, what)

destiny <- destiny %>% group_by(team) %>% 
  summarise(rank = round(mean(rank)))

thirteen_plot = ggplot(data = destiny, mapping = aes(y = rank, x = team, fill = rank)) + geom_bar(stat="identity") + scale_fill_gradient(low="brown1", high="brown4") + ggtitle("Influence of ranking 13 in team future!") + ylab("average rank") + guides(color=guide_legend(title="average rank")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
thirteen_plot

###### derby full results in years
derby <- res_table %>% 
  group_by(Season) %>% 
  filter((team == "Real Madrid" & opponent == "Atletico Madrid") |
           (team == "Atletico Madrid" & opponent == "Real Madrid") |
           (team == "Real Madrid" & opponent == "FC Barcelona") |
           (team == "FC Barcelona" & opponent == "Real Madrid") |
           (team == "FC Barcelona" & opponent == "Espanyol Barcelona") |
           (team == "Espanyol Barcelona" & opponent == "FC Barcelona") |
           (team == "Valencia CF" & opponent == "Levante UD") |
           (team == "Levante UD" & opponent == "Valencia CF"))


