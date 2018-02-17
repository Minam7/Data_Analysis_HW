library(readr)
library(dplyr)
library(ggplot2)
mobile = read_csv("data/mobile_data.csv")

mobile$ppi <- with(mobile, round(sqrt((mobile$px_row^2)+(mobile$px_col^2))/(mobile$display_size)))
mobile_nafree <- subset(mobile, !is.na(ppi))
p = ggplot(data = mobile_nafree, mapping = aes(x = ppi, fill=..count..)) + geom_histogram(binwidth = 12) + scale_fill_gradient(low="blue", high="red") + ggtitle("number of cellphones based on pixels per inch(ppi)")
p

device_ppi = mobile_nafree %>% group_by(year) %>% summarise(average= mean(ppi, na.rm = TRUE))
p_ppi = ggplot(data = device_ppi, mapping = aes(y = average, x = year, fill = average)) + geom_bar(stat="identity") + scale_fill_gradient(low="red", high="blue") + ggtitle("cellphone ppi per year") + ylab("average ppi") + guides(fill=guide_legend(title="average ppi")) 
p_ppi

cat("highest ppi cellphone name: ", (mobile_nafree$device[which.max(mobile_nafree$ppi)]), 'ppi:', (mobile_nafree$ppi[which.max(mobile_nafree$ppi)]))
