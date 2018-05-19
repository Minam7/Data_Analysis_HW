sp500 <- sp500 %>% mutate(profit = 100*((Close - Open)/Open))

non_profit_range <- sp500 %>% filter(profit < 0) %>% group_by(Year, Month, Day) %>% summarise(damage_rate = n()/505)

thirteen <- non_profit_range %>% filter(Day == 13)
non_thirteen <- non_profit_range %>% filter(Day != 13)

t.test(thirteen$damage_rate, non_thirteen$damage_rate, alt = "two.sided")
