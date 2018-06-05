library(plotly)


p <- plot_ly(data = earth_q, x = ~Latitude, y = ~Longitude, z = ~Depth, size = ~Magnitude,
             marker = list(color = ~Magnitude, symbol = 'circle', sizemode = 'diameter', colorscale = c('#FFE1A1', '#683531'), showscale = TRUE), sizes = c(0.5, 7.8),
             text = ~paste('Province:', Province, '<br>City:', City, '<br>Magnitude:', Magnitude)) %>%
  add_markers() %>%
  layout(scene = list(title = 'Iran Earthquakes between Sep. to Nov. 2015',
                      xaxis = list(title = 'Latitude', range = c(23, 40)),
                      yaxis = list(title = 'Longitude', range = c(40, 71)),
                      zaxis = list(title = 'Depth', range = c(0, 162))))
p
