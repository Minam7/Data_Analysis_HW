sp500 <- sp500 %>% inner_join(sector, by = c("stock_name" = "Symbol"))

stock_yearly <- full_join(
  sp500 %>% group_by(stock_name, Year) %>% arrange(Month, Day) %>% slice(1) %>% summarise(Start = Close, Sector = Sector),
  sp500 %>% group_by(stock_name, Year) %>% arrange(-Month, -Day) %>% slice(1) %>% summarise(End = Close, Sector = Sector),
  by = c("stock_name", "Year", "Sector")
)

