library(readr)
library(dplyr)
library(ggplot2)
mobile = read_csv("data/mobile_data.csv")

battery_weigth = ggplot(data = mobile, mapping = aes(x = battery_mah, y = weight, color = weight)) + geom_point(stat = "identity") + xlab("battery capacity") + ylab("weight") + scale_color_gradient(low="brown1", high="brown4")
battery_weigth

bat_nafree = subset(mobile, !is.na(battery_mah))
mobile_nafree = subset(bat_nafree, !is.na(weight))

cat("correlation between battery capacity and weight: ", cor(mobile_nafree$battery_mah, mobile_nafree$weight))
