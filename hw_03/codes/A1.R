# making data and league table
library(devtools)
library(dplyr)
library(highcharter)
library(engsoccerdata)
library(ggplot2)

fdb = as.tbl(spain)

# creating all league table
laLiga <- fdb %>% filter(tier == 1)
laLiga$Date <- as.Date(laLiga$Date, format="%Y-%m-%d") 
#rbind two copies of the orignal df, simply reversing home/away team for each match
rbind(
  laLiga %>%
    select(Season, team = home, opp = visitor, GF = hgoal, GA = vgoal),
  laLiga %>%
    select(Season, team = visitor, opp = home, GF = vgoal, GA = hgoal)
) %>% mutate(GD = GF-GA) %>% 
  group_by(team,Season) %>% 
  summarize(GP = n(),
            goalsF = sum(GF),
            goalsA = sum(GA),
            goaldif = sum(GD),
            W = sum(GD > 0),
            D = sum(GD == 0),
            L = sum(GD < 0)
  ) %>% 
  mutate(score = W*3 + D) %>%
  arrange(Season, desc(score), desc(goaldif)) %>% 
  group_by(Season) %>% 
  mutate(rank = row_number() %>% as.integer()) -> atable

##################################################

champion_table <- atable %>% 
  group_by(Season) %>% filter(rank == 1)
champs <- champion_table %>% 
  group_by(team) %>% summarise(championships = n())

p1 = ggplot(data = champs, mapping = aes(x = team, y = championships, fill = championships)) + ggtitle("Number of championships based on team") + geom_bar(stat="identity") + scale_fill_gradient(low="gold", high="darkgreen") + ylab("championship") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1

champs %>% 
  hchart(type = "column",hcaes(x = team, y = championships, color = championships)) %>% 
  hc_title(text = "Number of championships based on team", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_sandsignika())
