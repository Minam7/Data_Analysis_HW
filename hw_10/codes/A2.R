library(reshape2)

life_expectancy <- wdi_data %>% filter(`Indicator Code`== 'SP.DYN.LE00.IN') %>% 
  select(country = `Country Name`, matches('\\d{4}')) %>% 
  melt(id.vars=c('country'))

rwd_life_expectancy <- life_expectancy %>% filter(country == 'Rwanda')

p = ggplot(data = life_expectancy, mapping = aes(x = variable, y = value)) + geom_boxplot() +
  geom_line(data = rwd_life_expectancy, mapping = aes(x = variable, y = value, group = 1, color = 'Rwanda')) + 
  xlab("Year") + ylab("Life Expectancy") + ggtitle("Average Life Expectancy In Worldwide") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
p
  
rwd_death <- wdi_data %>% filter(`Indicator Code`== 'SP.DYN.CDRT.IN') %>% 
  select(country = `Country Name`, matches('\\d{4}')) %>% filter(country == 'Rwanda') %>% 
  melt(id.vars=c('country')) %>% summarise(tot_death = 1000*sum(value,na.rm = TRUE))

cat("Total Death in Rwanda is", rwd_death[1,], "people.")
