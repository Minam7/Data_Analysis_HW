# a
max_life_exp <- life_expectancy %>% group_by(country) %>% 
  summarise(exp = mean(value)) %>% arrange(desc(exp)) %>% slice(1:10)

knitr::kable(max_life_exp)

# b
edm <-wdi_data %>% filter(`Indicator Code`== 'NY.GDP.PCAP.CD') %>% 
  select(country = `Country Name`, matches('\\d{4}')) %>% 
  melt(id.vars=c('country')) %>% 
  filter(!is.na(value))

q = ggplot(edm ,aes(x = variable, y = value)) + geom_boxplot() +
    xlab("Year") + ggtitle("School enrollment, primary (gross), gender parity index (GPI)n") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_line(data = subset(edm,country == 'Iran, Islamic Rep.'),aes(x = variable, y = value, group = 1, color = 'Iran, Islamic Rep.'))
q

# c
hcm <- wdi_data %>% filter(`Indicator Code`== 'SH.DTH.IMRT') %>% 
  select(country = `Country Name`, matches('\\d{4}')) %>% 
  melt(id.vars=c('country')) %>% 
  filter(!is.na(value)) %>% 
  group_by(country) %>% 
  summarise(death = sum(value)) %>% arrange(desc(death)) %>% slice(1:10)

knitr::kable(hcm)