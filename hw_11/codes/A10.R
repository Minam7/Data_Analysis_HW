# a continents difference in earthquake magnitude
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='high')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  

  indices = over(pointsSP, countriesSP)
  indices$REGION   # returns the continent (7 continent model)
}

disaster_ten <- disaster_death %>% filter(!is.na(long) & !is.na(lat))
s <- disaster_ten %>% select(long, lat)
s$continent <- as.character(coords2continent(s))
s <- s %>% select(continent)

disaster_ten <- bind_cols(disaster_ten, s)

disaster_ten_sun <- disaster_ten %>% group_by(continent) %>% filter(!is.na(continent) & !is.na(death) & !is.na(magnit)) %>% 
  summarise(count = n(), fatality = sum(death), mean_fatality = mean(death), mean_mag = mean(magnit))

disaster_ten_sun[3,1] = "Oceania"
disaster_ten_sun[6,1] = "South America"

hchart(disaster_ten_sun, "treemap", hcaes(x = continent, value = mean_mag, color = count)) %>% 
  hc_title(text = "Difference in Signifact Earthquakes Occurance in Continents", style = list(fontWeight = "bold"))


# b continents fatality
hcmap("custom/world-continents", data = disaster_ten_sun, value = "fatality",
      joinBy = c("name", "continent"), name = "Continents Total Fatality",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2)) %>% 
  hc_title(text = "Continents Total Fatality", style = list(fontWeight = "bold"))

hcmap("custom/world-continents", data = disaster_ten_sun, value = "mean_fatality",
      joinBy = c("name", "continent"), name = "Continents Mean Fatality",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2)) %>% 
  hc_title(text = "Continents Fatality Average", style = list(fontWeight = "bold"))

# c Iran earthquakes
earth_q_sum <- earth_q %>% as.data.frame(stringsAsFactors = F)

earth_q_sum$Province = as.character(earth_q_sum$Province)
earth_q_sum$Province[earth_q_sum$Province == "semnan"] = as.character("Semnan")
  
earth_q_sum <- earth_q_sum %>% group_by(Province) %>% filter(!is.na(Magnitude)) %>% summarise(mean_mag = mean(Magnitude)) %>% 
  arrange(Province)

earth_q_sum <- earth_q_sum[-c(1,4,5,15,27,34,35), ]
earth_q_sum[4,1] = as.character("Chahar Mahall and Bakhtiari")
earth_q_sum[12,1] = as.character("Esfahan")
earth_q_sum[9,1] = as.character("Hamadan")
earth_q_sum[13,1] = as.character("Kerman")
earth_q_sum[15,1] = as.character("Razavi Khorasan")
earth_q_sum[17,1] = as.character("Kohgiluyeh and Buyer Ahmad")
earth_q_sum[26,1] = as.character("Sistan and Baluchestan")

s <- c(NA, 0)
names(s)<-c("Province","mean_mag")
earth_q_sum <- rbind(earth_q_sum, s)

hcmap("countries/ir/ir-all", data = earth_q_sum, value = "mean_mag",
      joinBy = c("name", "Province"), name = "Iran Earthquakes",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1) %>% 
  hc_title(text = "Iran Earthquake Magnitude Average", style = list(fontWeight = "bold"))
