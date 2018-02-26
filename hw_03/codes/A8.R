a_rank <- all_table %>% 
  group_by(Season, game_nu) %>% 
  arrange(desc(score_cum), desc(GD_cum)) %>% 
  mutate(rank = row_number() %>% as.integer())

rank_1998 <- a_rank %>% ungroup() %>% filter(Season == 1998) %>% arrange(time)

# team <- c("FCB", "RMA", "MLL", "VCF", "CEL", "RCD", "ESP", "ATH", "ZAR", "RSO", "BET", "VLD", "ATM", "ROV", "RAC", "ALV", "EXT", "VIL", "TEN", "SAL")
# rank <- c(1:20)
# team_names <- data.frame(team, rank)

rank_1998  %>% 
  hchart(type = "line", hcaes(x = datetime_to_timestamp(time), y = rank, group = team)) %>% 
  hc_yAxis(title = list(text = "Position"), reversed = TRUE, max = 20, tickInterval = 1, min = 1,
           plotLines = list(list(color = "#FF0000", width = 2, value = 10, dashStyle = 'shortdash'))
           ) %>% 
  hc_xAxis(type = "datetime", title = list(text = "date"), dateTimeLabelFormats = list(day = '%d of %b')) %>% 
  hc_title(text = "Team rankings in 1998/1999", style = list(fontWeight = "bold"))


ranking_1998 <- ggplot(data=rank_1998, aes(x= time, y=rank, colour=team)) + geom_line() + scale_y_reverse(limit=c(20,0)) + ggtitle("Team rankings in 1998/1999")
ranking_1998
