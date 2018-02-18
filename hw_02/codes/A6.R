library(readr)
library(dplyr)
library(ggplot2)
mobile = read_csv("data/mobile_data.csv")

mobile$desit <- with(mobile, (mobile$weight)/(mobile$dim_length*mobile$dim_breadth*mobile$dim_thickness))
mobile_nafree <- subset(mobile, !is.na(desit))

mobile_goosh = mobile_nafree %>% filter(dim_thickness > 20) %>% filter(display_size < 3) %>% filter(desit > 0.0008)
mobile_goosh = mobile_goosh %>% mutate(gooshkubi = (dim_thickness*desit)/display_size)
gooshkub_top10 = head(mobile_goosh[order(mobile_goosh$gooshkubi, decreasing = TRUE), ], 10)

p = ggplot(data = gooshkub_top10, aes(y = gooshkubi, x = device, fill = gooshkubi)) + geom_bar(stat="identity") + scale_fill_gradient(low="steelblue", high="midnightblue") + ggtitle("top 20 gooshkubs") + xlab("devices") + ylab("gooshkubi") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
