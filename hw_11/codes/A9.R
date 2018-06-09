library(sp)

coords2country = function(points)
{  
  countriesSP <- getMap(resolution='high')
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  indices$ISO3 # returns the ISO3 code 
}

s <- worldwide %>% select(longitude, latitude)
s$country <- coords2country(s)
s <- s %>% select(country)

worldwide <- bind_cols(worldwide, s)

worldwide <- worldwide %>% filter(!is.na(country)) %>% 
  mutate(year = as.numeric(format(time, format = "%Y")))

worldwide_sum <- worldwide %>% group_by(year, country) %>% 
  summarise(count = n(), mean_mag = mean(mag), mean_depth = mean(depth))

hchart(worldwide_sum %>% filter(year == 2015), "treemap", hcaes(x = country, value = count, color = mean_mag)) %>% 
  hc_title(text = "2015 Earthquakes", style = list(fontWeight = "bold"))

hchart(worldwide_sum %>% filter(year == 2016), "treemap", hcaes(x = country, value = count, color = mean_mag)) %>% 
  hc_title(text = "2016 Earthquakes", style = list(fontWeight = "bold"))

hchart(worldwide_sum %>% filter(year == 2017), "treemap", hcaes(x = country, value = count, color = mean_mag)) %>% 
  hc_title(text = "2017 Earthquakes", style = list(fontWeight = "bold"))

hchart(worldwide_sum %>% filter(year == 2018), "treemap", hcaes(x = country, value = count, color = mean_mag)) %>% 
  hc_title(text = "2018 Earthquakes", style = list(fontWeight = "bold"))
