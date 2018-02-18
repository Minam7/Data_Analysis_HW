library(readr)
library(dplyr)
library(ggplot2)
mobile = read_csv("data/mobile_data.csv")
most_devices = mobile %>% group_by(company) %>% summarise(count = n())

top_vendor = head(most_devices[order(most_devices$count, decreasing=TRUE), ], 20)

p = ggplot(data = top_vendor, aes(y = count, x = company, fill = count)) + geom_bar(stat="identity") + scale_fill_gradient(low="red", high="green") + coord_flip() + ggtitle("top 20 vendors") + xlab("number of manufactured devices")
p
cat("most cellphone vendor: ", (most_devices$company[which.max(most_devices$count)]))
