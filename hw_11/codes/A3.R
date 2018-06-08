iran_eq <- read_rds("data/iran_earthquake.rds")%>% arrange(-Long)
iran_eq = iran_eq[-c(1),]

p <- ggplot(iran_eq, aes(x=Long, y=Lat)) +
  geom_point() + stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  xlab("Longitude") + ylab("Latitude") + 
  guides(fill=guide_legend(title="Density")) + 
  scale_fill_distiller(palette=4, direction=-1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  ggtitle("Iran Earthquakes Density Plot")
p
