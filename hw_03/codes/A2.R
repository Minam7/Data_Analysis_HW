# average goal per game
atable <- atable %>% mutate(goaltotgame = (goalsA + goalsF)/GP)
boring_teams <- atable %>% 
  group_by(team) %>% summarise(goaltotavg = mean(goaltotgame)) %>% 
  arrange(goaltotavg) %>% slice(1:10)

highchart() %>% 
  hc_add_series(data = boring_teams, type = "line", hcaes(x = team, y = goaltotavg), name = "Boring teams", showInLegend = FALSE) %>% 
  hc_yAxis(title = list(text = "boring rate")) %>% 
  hc_xAxis(type = 'category', title = list(categories = boring_teams$team, text = "team")) %>% 
  hc_title(text = "Most boring teams", style = list(fontWeight = "bold")) %>%
  hc_add_theme(hc_theme_flat())

# average goal per game
laLiga <- laLiga %>% mutate(totgoal = hgoal + vgoal)
boring_seasons <- laLiga %>% 
  group_by(Season) %>% 
  summarise(avggoal = mean(totgoal)) %>% 
  arrange(avggoal) %>% 
  slice(1:10)
  
highchart() %>% 
  hc_add_series(data = boring_seasons, type = "line", hcaes(x = reorder(Season, avggoal), y = avggoal), name = "Boring seasons", showInLegend = FALSE, color = "lightseagreen") %>% 
  hc_yAxis(title = list(text = "boring rate")) %>% 
  hc_xAxis(type = 'category', title = list(categories = boring_seasons$Season, text = "season")) %>% 
  hc_title(text = "Most boring teams", style = list(fontWeight = "bold")) %>%
  hc_add_theme(hc_theme_flat())
