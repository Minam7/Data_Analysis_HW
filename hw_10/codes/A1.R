income <- wdi_data %>% filter(`Indicator Code`== 'NY.ADJ.NNTY.CD') %>% 
  select(country = `Country Name`, income = `2016`)

poverty <- wdi_data %>%  filter(`Indicator Code`== 'NY.GDP.PCAP.PP.CD') %>% 
  select(country = `Country Name`, poverty = `2016`)

poverty_line <- wdi_data %>% filter(`Indicator Code`== 'SI.POV.NAHC') %>% 
  select(country = `Country Name`, poverty_line = `2014`)

population <- wdi_data %>% filter(`Indicator Code`== 'SP.POP.TOTL') %>% 
  select(country = `Country Name`, population = `2016`)

life_expectancy <- wdi_data %>% filter(`Indicator Code`== 'SP.DYN.LE00.IN') %>% 
  select(country = `Country Name`, life_expectancy = `2016`)

poverty <- poverty %>%  inner_join(income, by = c("country")) %>% 
  inner_join(population, by = c("country")) %>% 
  inner_join(poverty_line, by = c("country")) %>% 
  inner_join(life_expectancy, by = c("country")) %>% 
  arrange(poverty) %>% 
  mutate(daily_income = income/(365*population)) %>% 
  slice(1:10)
  
knitr::kable(poverty)

poverty %>% arrange(daily_income) %>% 
  hchart(type = "column", hcaes(x = country, y = daily_income)) %>% 
  hc_yAxis(title = list(text = "Daily Income")) %>% 
  hc_xAxis(title = list(text = "Country")) %>% 
  hc_title(text = "Daily Income in Poor Countries", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_ffx())
