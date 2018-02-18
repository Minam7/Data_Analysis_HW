library(readr)
library(dplyr)
library(ggplot2)
mobile = read_csv("data/mobile_data.csv")

htc_dev = mobile %>% filter(company=="HTC")
htc_dev = subset(htc_dev, !is.na(price))

htc_dev = htc_dev %>% group_by(year) %>% filter(price == max(price)) %>% distinct(year, .keep_all = TRUE)
View(htc_dev)

flagship = ggplot(data = htc_dev, mapping = aes(x = year, y = price)) + geom_point(stat="identity") + ggtitle("HTC flagships") + geom_text(mapping = aes(label = device), angle = -45, hjust = 1.1, size = 2.5)
flagship
