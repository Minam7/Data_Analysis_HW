iran_sig_eq <- read_delim("data/disaster.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(lat = LATITUDE,long = LONGITUDE, magnit = INTENSITY,name = COUNTRY,year = YEAR) %>% 
  dplyr::select(lat, long, magnit, name, year) %>% filter(magnit >= 7 & name == 'IRAN') %>% 
  mutate(diff = year - lag(year))

cdf_iran <- ecdf(iran_sig_eq$diff)
summary(cdf_iran)

B = 1 - cdf_iran(1)
AB = cdf_iran(6)
p = (AB)/(B)

cat("Probability of earthquake in 5 years is", p)
