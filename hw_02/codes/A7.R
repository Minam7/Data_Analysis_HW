library(readr)
library(dplyr)
library(ggplot2)
mobile = read_csv("data/mobile_data.csv")

mobile$desit <- with(mobile, (mobile$weight)/(mobile$dim_length*mobile$dim_breadth*mobile$dim_thickness))
mobile_nafree <- subset(mobile, !is.na(desit))

mobile_nafree$float <- ifelse(mobile_nafree$desit < 0.0010, TRUE, FALSE)

p_float = ggplot(data = mobile_nafree, mapping = aes(x = float, fill=..count..)) + geom_histogram(stat="count") + ggtitle('floating devices on water') + ylab("number of devices") + xlab("floating")
p_float
