sp500 <- sp500 %>% inner_join(sector, by = c("stock_name" = "Symbol"))

# Annual
stock_yearly <- full_join(
  sp500 %>% group_by(stock_name, Year) %>% arrange(Month, Day) %>% slice(1) %>% summarise(Start = Close, Sector = Sector),
  sp500 %>% group_by(stock_name, Year) %>% arrange(-Month, -Day) %>% slice(1) %>% summarise(End = Close, Sector = Sector),
  by = c("stock_name", "Year", "Sector")
) %>% select(Year, stock_name, Start, End, Sector)

stock_yearly <- stock_yearly %>% mutate(profit = 100*((End - Start)/Start)) %>% ungroup()

knitr::kable(stock_yearly %>% arrange(desc(profit)) %>% slice(1))

knitr::kable(stock_yearly %>% group_by(Sector) %>% arrange(desc(profit)) %>% slice(1))

stock_yearly %>% group_by(Sector) %>% arrange(desc(profit)) %>% slice(1) %>% 
  hchart(type = "column", hcaes(x = stock_name, y = profit, group = Sector)) %>% 
  hc_yAxis(title = list(text = "Profit")) %>% 
  hc_xAxis(type = 'category', title = list(categories = stock_yearly$stock_name, text = "Stock Name")) %>% 
  hc_title(text = "Top Annual Profitable Stocks", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_flat())

# Every Two Year
# TODO

# Every Five Year
# TODO
