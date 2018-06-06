library(gganimate)

disaster = read_delim("data/disaster.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(lat = LATITUDE,long = LONGITUDE, magnit = INTENSITY,name = COUNTRY,year = YEAR) %>% 
  dplyr::select(lat, long, magnit, name, year)

nadis <- na.omit(disaster) %>% arrange(-magnit)
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders

p <- ggplot() + xlab("longitute") + ylab("latitude") + mapWorld + ggtitle("Earthquake Intensity")
p <- p + geom_point(aes(x = nadis$long, y = nadis$lat, size = nadis$magnit, frame = nadis$magnit), color="indianred1") + guides(size=guide_legend(title="Intensity"))
gganimate(p)


animation<-gganimate(p, "images/eq.gif")