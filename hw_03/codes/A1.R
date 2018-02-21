# making data and league table
library(devtools)
library(dplyr)
library(highcharter)
library(engsoccerdata)
library(ggplot2)

fdb = as.tbl(spain)
View(fdb)

# creating all league table
laLiga <- fdb %>% filter(tier == 1)
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
            W = sum(GD>0),
            D = sum(GD==0),
            L = sum(GD<0)
  ) %>% 
  mutate(score = W*3 + D) %>%
  arrange(Season,desc(score)) %>% 
  group_by(Season) %>% 
  mutate(rank = rank(-score) %>% as.integer()) -> atable
View(atable)

##################################################

champion_table <- atable %>% 
  group_by(Season) %>% slice(which.max(score))
champs <- champion_table %>% 
  group_by(team) %>% summarise(cup = n())

p1 = ggplot(data = champs, mapping = aes(x = team, y = cup, fill = cup)) + ggtitle("number of championships based on team") + geom_bar(stat="identity") + scale_fill_gradient(low="gold", high="darkgreen") + ylab("championship") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1
