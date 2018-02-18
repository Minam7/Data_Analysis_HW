library(readr)
library(dplyr)
library(ggplot2)
mobile = read_csv("data/mobile_data.csv")

# corelation of fields to price
mobile <- mobile[which(mobile$company != "QMobile"),]
mobile <- mobile[which(mobile$price < 3000),]
mobile <- subset(mobile, !is.na(price))
mobile <- subset(mobile, !is.na(battery_mah))
mobile <- subset(mobile, !is.na(weight))
mobile <- subset(mobile, !is.na(dim_thickness))
mobile <- subset(mobile, !is.na(display_size))
mobile <- subset(mobile, !is.na(cam_px))
mobile <- subset(mobile, !is.na(ram))

mobile$ppi <- with(mobile, round(sqrt((mobile$px_row^2)+(mobile$px_col^2))/(mobile$display_size)))
mobile <- subset(mobile, !is.na(ppi))

mobile$desit <- with(mobile, (mobile$weight)/(mobile$dim_length*mobile$dim_breadth*mobile$dim_thickness))
mobile <- subset(mobile, !is.na(desit))

features <- c('battery_mah','weight','dim_thickness', 'display_size', 'cam_px', 'ram', 'ppi', 'density')
corelation <- c(cor(mobile$price, mobile$battery_mah), cor(mobile$price, mobile$weight), cor(mobile$price, mobile$dim_thickness), cor(mobile$price, mobile$display_size), cor(mobile$price, mobile$cam_px), cor(mobile$price, mobile$ram), cor(mobile$price, mobile$ppi), cor(mobile$price, mobile$desit))
my <- data.frame(features, corelation)
View(my)
p1 = ggplot(data = my, mapping = aes(x = features, y = corelation, fill = corelation)) + ggtitle("corelation of different features with price") + geom_bar(stat="identity") + scale_fill_gradient(low="gold", high="darkgreen") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1


# average price in years
mobile_price <- mobile %>% group_by(year) %>% summarise(price_average = mean(price , na.rm = TRUE))
p2 = ggplot(data = mobile_price, mapping = aes(x = year, y = price_average, fill = price_average)) + ggtitle("changes of prices in years") + geom_bar(stat="identity") + scale_fill_gradient(low="midnightblue", high="darkred") + ylab("average price") + guides(fill=guide_legend(title="average price")) 
p2

