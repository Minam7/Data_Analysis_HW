health <- wdi_data %>% filter(`Indicator Code`== 'SH.XPD.CHEX.PC.CD') %>% 
  select(country = `Country Name`, matches('\\d{4}')) %>% 
  melt(id.vars=c('country')) %>% 
  mutate(value = value/100, type = 'health') %>% 
  filter(!is.na(value))

life_expectancy <- life_expectancy %>% filter(!is.na(value)) %>% mutate(type = 'life expectancy')

health_expect <- health %>% rbind(life_expectancy)

p = ggplot(data = health_expect, mapping = aes(x = variable, y = value, fill = type, color = type)) + geom_boxplot() +
  xlab("Year") + ylab("Life Expectancy or Health Cost") + ggtitle("Average Life Expectancy and Health Cost Worldwide") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
p
