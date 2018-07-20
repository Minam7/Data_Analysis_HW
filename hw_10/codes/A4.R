iran_buy_cap <- wdi_data %>% filter(`Indicator Code`== 'NY.GDP.PCAP.PP.CD') %>% 
  select(country = `Country Name`, matches('\\d{4}')) %>% 
  filter(country == 'Iran, Islamic Rep.') %>% 
  melt(id.vars=c('country')) %>% 
  filter(!is.na(value))

iran_buy_cap %>% arrange(variable) %>% 
  hchart(type = "line", hcaes(x = variable, y = value)) %>% 
  hc_yAxis(title = list(text = "Purchasing Capability")) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_title(text = "Purchasing Capability in Iran", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_economist())
