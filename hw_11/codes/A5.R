library(rworldmap)

disaster_sum <- read_delim("data/disaster.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(lat = LATITUDE,long = LONGITUDE, magnit = INTENSITY,country = COUNTRY,year = YEAR, death=TOTAL_DEATHS) %>% 
  dplyr::select(lat, long, magnit, country, year, death)

disaster_sum = na.omit(disaster_sum)
disaster_sum <- disaster_sum %>% group_by(country) %>% summarise(fatality_sum = sum(death), fatality_mean = mean(death))

heatMap <- joinCountryData2Map(disaster_sum, joinCode = "ISO3",
                              nameJoinColumn = "country")
