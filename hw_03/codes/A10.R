###### 10 weakest teams of all time
weak <- full_season %>%
  filter(rank == max(rank)) %>% 
  mutate(score_rate = round(10*score_cum/(max(game_nu)*3))) %>% 
  ungroup %>% filter(score_rate == min(score_rate))

weak %>% 
  hchart(type = "column", hcaes(x = team, y = score_cum/10, group = Season)) %>% 
  hc_yAxis(title = list(text = "losts")) %>% 
  hc_xAxis(type = 'category', title = list(categories = weak$team, text = "opponent")) %>% 
  hc_title(text = "Weakest teams of all time", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_darkunica())

weak_plot = ggplot(data = weak, mapping = aes(y = score_cum/10, x = team, fill = Season)) + geom_bar(stat="identity") + scale_fill_gradient(low="turquoise4", high="palegreen") + ggtitle("Weakest teams of all time") + ylab("score rate") + guides(color=guide_legend(title="season"), fill=guide_legend(title="season")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
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

destiny %>% 
  hchart(type = "column",hcaes(x = team, y = rank)) %>% 
  hc_title(text = "Influence of ranking 13 in team future!", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_economist())

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
           (team == "Levante UD" & opponent == "Valencia CF")) %>% 
  mutate(allG = GA + GF) %>% 
  mutate(coler = ifelse((team == "Real Madrid" & opponent == "Atletico Madrid") |
                          (team == "Atletico Madrid" & opponent == "Real Madrid"),
                        "Madrid derby", 
                        ifelse((team == "Real Madrid" & opponent == "FC Barcelona") |
                                 (team == "FC Barcelona" & opponent == "Real Madrid"),
                               "El Clásico", ifelse((team == "FC Barcelona" & opponent == "Espanyol Barcelona") |
                                                      (team == "Espanyol Barcelona" & opponent == "FC Barcelona"), 
                                                    "Barcelona derby", ifelse((team == "Valencia CF" & opponent == "Levante UD") |
                                                                                (team == "Levante UD" & opponent == "Valencia CF")
                                                                              , "Valencia derby", NA))))) %>% 
  ungroup() %>% group_by(Season) %>% filter(allG == max(allG))


derby %>% 
  hchart(type = "column", hcaes(x = Season, y = allG, group = coler)) %>% 
  hc_yAxis(title = list(text = "average goal")) %>% 
  hc_xAxis(title = list(text = "season")) %>% 
  hc_title(text = "Derby goals in years", style = list(fontWeight = "bold"))

p = ggplot(data = derby, aes(y = allG, x = Season, color = coler, fill = coler)) + geom_bar(stat="identity", position = "dodge") + ggtitle("Derby goals in years") + xlab("season") + ylab("average goal") + guides(fill=guide_legend(title="derby"), color=guide_legend(title="derby"))
p
