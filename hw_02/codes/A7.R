library(readr)
library(dplyr)
library(ggplot2)
mobile = read_csv("data/mobile_data.csv")

mobile$desit <- with(mobile, (mobile$weight)/(mobile$dim_length*mobile$dim_breadth*mobile$dim_thickness))
mobile_nafree <- subset(mobile, !is.na(desit))

mobile_nafree$float <- ifelse(mobile_nafree$desit < 0.001, TRUE, FALSE)
View(mobile_nafree[which(mobile_nafree$float == TRUE), ])

p_float = ggplot(data = mobile_nafree, mapping = aes(x = desit, fill = float)) + geom_histogram(position="identity", binwidth = 0.00003) + xlab("density") + ylab("count") + ggtitle("floating devices vs sinking devices in water") 
p_float
