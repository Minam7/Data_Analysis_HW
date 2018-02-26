league_2012 <- laLiga %>% filter(Season == 2012) %>% 
  select(home, visitor, FT, hgoal, vgoal) %>% 
  mutate(GD=hgoal-vgoal, result = ifelse(GD > 0, "H", ifelse(GD < 0, "A", "D")))

team_pos <- full_season %>% ungroup() %>%
  filter(Season == 2012) %>% select(team) %>% 
  arrange(team) %>% mutate(place = row_number())

league_2012$home <- factor(league_2012$home, levels=rev(team_pos$team))
league_2012$visitor <- factor(league_2012$visitor, levels=team_pos$team)

league_2012$title <- "Spain laLiga 2012/2013 - RESULTS MATRIX"

matrix <- ggplot(league_2012, aes(visitor, home, fill = factor(result))) + 
  geom_tile(colour="steelblue4", size=1.5, stat="identity", height=1, width=1) + 
  geom_text(data=league_2012, aes(visitor, home, label = FT), color="black", size=rel(2)) +
  scale_x_discrete(expand = c(0, 0), position = "top") +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = c("firebrick1", "steelblue1", "chartreuse3"))+
  xlab("") + 
  ylab("") +
  facet_grid(. ~ title) +
  theme(
    strip.background = element_rect(fill="darkblue"),
    strip.text = element_text(size=15, colour="white"),
    strip.placement = "outside",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill=NA,color="steelblue4", size=0.5, linetype="solid"),
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    axis.text = element_text(color="white", size=rel(1)),
    panel.background = element_rect(fill="darkblue"),
    plot.background = element_rect(fill="steelblue4"),
    legend.position = "none",
    axis.text.x  = element_text(angle=75, vjust=0.5, hjust=0)        
  ) 
matrix
